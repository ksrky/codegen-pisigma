module Parser (pTy, pExp, parseProg) where

import Data.Text                  hiding (empty)
import Data.Void
import Id
import Lambda
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
pId = Id <$> lexeme pName <*> pure newUniq <?> "<Id>"

pLit :: Parser Lit
pLit = LInt <$> lexeme L.decimal <?> "<Lit>"

pTInt :: Parser Ty
pTInt = TInt <$ stringL "Int" <?> "<TInt>"

pTFun :: Parser Ty
pTFun = TFun <$> pTy 1 <* stringL "->" <*> pTy 0 <?> "<TFun>"

pTy :: Int -> Parser Ty
pTy p = try (ifPrec p 1 pTFun) <|> pTInt <|> parens (pTy 0) <?> "<Ty>"

pVar :: Parser Var
pVar = (,) <$> pId <* charL ':' <*> pTy 0 <?> "<Var>"

pELit :: Parser Exp
pELit = ELit <$> pLit <?> "<ELit>"

pEVar :: Parser Exp
pEVar = EVar <$> parens pVar <?> "<EVar>"

pEApp :: Parser Exp
pEApp = EApp <$> pExp 1 <*> pExp 2 <?> "<EApp>"

pELam :: Parser Exp
pELam = ELam <$> (charL '\\' *> parens pVar) <* stringL "->" <*> pExp 0 <?> "<ELam>"

pELet :: Parser Exp
pELet = ELet <$> (stringL "let" *> pVar) <* charL '=' <*> pExp 0 <* stringL "in" <*> pExp 0 <?> "<ELet>"

pExpTy :: Parser Exp
pExpTy = parens (EExpTy <$> lexeme (pExp 0 <* char ':') <*> pTy 0) <?> "<EExpTy>"

pExp :: Int -> Parser Exp
pExp p = lexeme (
    try (ifPrec p 1 pEApp)
    <|> try pELet
    <|> pELit
    <|> try pEVar
    <|> pELam
    <|> try pExpTy
    <|> parens (pExp 0))
    <?> "<Exp>"

pProg :: Parser Prog
pProg = pExp 0 <* eof <?> "<Prog>"

parseProg :: MonadFail m => String -> Text -> m Prog
parseProg name input = case runParser pProg name input of
    Left err   -> fail $ errorBundlePretty err
    Right prog -> return prog


