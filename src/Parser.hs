module Parser (pTy, pExp, parseProg) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Void
import Raw
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer     qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/-" "-/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

charL :: Char -> Parser Char
charL = lexeme . char

stringL :: Text -> Parser Text
stringL = lexeme . string

parens :: Parser a -> Parser a
parens = lexeme . between (char '(') (char ')')

pName :: Parser String
pName = do
    x <- lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "Name"
    when (x `elem` ["let", "in", "Int"]) empty
    return x

pLit :: Parser Lit
pLit = LInt <$> lexeme L.decimal <?> "Lit"

pTInt :: Parser Ty
pTInt = TInt <$ stringL "Int" <?> "TInt"

pTy1 :: Parser Ty
pTy1 = lexeme (pTInt <|> parens pTy) <?> "Ty1"

pTy :: Parser Ty
pTy = makeExprParser pTy1 [[InfixR (TFun <$ stringL "->")]] <?> "Ty"

pELit :: Parser Exp
pELit = ELit <$> pLit <?> "ELit"

pEVar :: Parser Exp
pEVar = EVar <$> pName <?> "EVar"

pEApp :: Parser Exp
pEApp = do
    e1 <- pExp1
    es <- some pExp1
    return (foldl EApp e1 es) <?> "<EApp>"

pELam :: Parser Exp
pELam = ELam <$> (charL '\\' *> parens pName) <* charL ':' <*> pTy1 <* stringL "->" <*> pExp <?> "ELam"

pELet :: Parser Exp
pELet = ELet <$> (stringL "let" *> pName) <* charL '=' <*> pExp <* stringL "in" <*> pExp <?> "ELet"

pExp1 :: Parser Exp
pExp1 =
    pELit
    <|> pEVar
    <|> parens pExp <?> "Exp1"

pExp2 :: Parser Exp
pExp2 = makeExprParser (try pEApp <|> pExp1) table <?> "Exp2"
  where
    table =
        [ [InfixL (EBinOp . Text.unpack <$> stringL "*")]
        , [InfixL (EBinOp . Text.unpack <$> stringL "+")]
        ]

pExp :: Parser Exp
pExp = pELam <|> pELet <|> pExp2 <?> "Exp"

pProg :: Parser Prog
pProg = pExp <* eof <?> "<Prog>"

parseProg :: MonadFail m => String -> Text -> m Prog
parseProg fname input = case runParser pProg fname input of
    Left err   -> fail $ errorBundlePretty err
    Right prog -> return prog
