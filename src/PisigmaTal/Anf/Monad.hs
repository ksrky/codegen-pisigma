{-# LANGUAGE TemplateHaskell #-}

module PisigmaTal.Anf.Monad where

import Control.Lens.Combinators
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import Data.Map.Strict          qualified as M
import PisigmaTal.Id

data AnfContext = AnfContext
    { _anfIdCount    :: M.Map Id (IORef Int)
    , _anfFunArities :: M.Map String Int
    }

instance Semigroup AnfContext where
    AnfContext a1 b1 <> AnfContext a2 b2 = AnfContext (a1 <> a2) (b1 <> b2)

makeLenses ''AnfContext

extendIdCount :: Id -> AnfM a -> AnfM a
extendIdCount x m = do
    ref <- liftIO $ newIORef 0
    locally anfIdCount (M.insert x ref) m

readIdCount :: Id -> AnfM Int
readIdCount x = do
    m <- view anfIdCount
    case M.lookup x m of
        Just ref -> liftIO $ readIORef ref
        Nothing  -> fail "readIdCount: unknown id"

whenUnusedId :: Id -> AnfM a -> AnfM a
whenUnusedId x m = do
    n <- readIdCount x
    if n == 0 then m else fail "whenUnusedId: id is used"

incrIdCount :: Id -> AnfM ()
incrIdCount x = do
    m <- view anfIdCount
    case M.lookup x m of
        Just ref -> liftIO $ modifyIORef' ref (+ 1)
        Nothing  -> fail "incrIdCount: unknown id"

decrIdCount :: Id -> AnfM ()
decrIdCount x = do
    m <- view anfIdCount
    case M.lookup x m of
        Just ref -> liftIO $ modifyIORef' ref (\n -> n - 1)
        Nothing  -> fail "decrIdCount: unknown id"

type AnfM = ReaderT AnfContext IO

runAnfM :: AnfM a -> IO a
runAnfM m = runReaderT m (AnfContext M.empty M.empty)

type AnfSetupM m = WriterT AnfContext m
