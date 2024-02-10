module Parser (pTy, pExp, parseProg) where

import Data.Text                  hiding (empty)
import Data.Void
import Id
import Lambda
import Lambda.Prim
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

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

ifPrec :: Int -> Int -> Parser a -> Parser a
ifPrec p1 p2 p = if p1 < p2 then p else empty

pName :: Parser String
pName = (:) <$> letterChar <*> many alphaNumChar <?> "<Id>"

pId :: Parser Id
pId = fromString <$> lexeme pName <?> "<Id>"

pLit :: Parser Lit
pLit = LInt <$> lexeme L.decimal <?> "<Lit>"

pTInt :: Parser Ty
pTInt = TInt <$ stringL "Int" <?> "<TInt>"

pTFun :: Parser Ty
pTFun = TFun <$> pTy 2 <* stringL "->" <*> pTy 1 <?> "<TFun>"

pTy :: Int -> Parser Ty
pTy p = try (ifPrec p 1 pTFun) <|> pTInt <|> parens (pTy 0) <?> "<Ty>"

pVar :: Parser Var
pVar = (,) <$> pId <* charL ':' <*> pTy 0 <?> "<Var>"

pELit :: Parser Exp
pELit = ELit <$> pLit <?> "<ELit>"

pEVar :: Parser Exp
pEVar = EVar <$> parens pVar <?> "<EVar>"

pEApp :: Parser Exp
pEApp = EApp <$> pExp 9 <*> pExp 8 <?> "<EApp>"

pELam :: Parser Exp
pELam = ELam <$> (charL '\\' *> parens pVar) <* stringL "->" <*> pExp 0 <?> "<ELam>"

pELet :: Parser Exp
pELet = ELet <$> (stringL "let" *> pVar) <* charL '=' <*> pExp 0 <* stringL "in" <*> pExp 0 <?> "<ELet>"

pEExpTy :: Parser Exp
pEExpTy = parens (EExpTy <$> lexeme (pExp 0 <* char ':') <*> pTy 0) <?> "<EExpTy>"

pBinMul :: Parser Exp
pBinMul = EApp <$> (EApp (EVar varTimes) <$> pExp 7) <* charL '*' <*> pExp 6 <?> "<BinTimes>"

pBinAdd :: Parser Exp
pBinAdd = EApp <$> (EApp (EVar varPlus) <$> pExp 6) <* charL '+' <*> pExp 5 <?> "<BinAdd>"

pAtom :: Parser Exp
pAtom =
    pELit
    <|> try pEVar
    <|> try pEExpTy
    <|> try (parens (pExp 0)) <?> "<Atom>"

pExp :: Int -> Parser Exp
pExp p = lexeme (
    try (ifPrec p 9 pEApp)
    <|> try (ifPrec p 7 pBinMul)
    <|> try (ifPrec p 6 pBinAdd)
    <|> ifPrec p 1 pELet
    <|> pAtom
    <|> ifPrec p 2 pELam)
    <?> "<Exp>"

pProg :: Parser Prog
pProg = pExp 0 <* eof <?> "<Prog>"

parseProg :: MonadFail m => String -> Text -> m Prog
parseProg fname input = case runParser pProg fname input of
    Left err   -> fail $ errorBundlePretty err
    Right prog -> return prog


