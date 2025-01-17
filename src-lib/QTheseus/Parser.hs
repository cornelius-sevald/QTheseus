{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module QTheseus.Parser (parseProg, parseProgFromFile) where

import Data.Char (isLower, isUpper)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import QTheseus.Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as P

lexer :: P.GenTokenParser Text a Identity
lexer =
  let def =
        P.LanguageDef
          { P.commentStart = "(*",
            P.commentEnd = "*)",
            P.commentLine = "",
            P.nestedComments = True,
            P.identStart = letter <|> char '_',
            P.identLetter = alphaNum <|> char '_',
            P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
            P.opLetter = P.opStart def,
            P.reservedNames = ["data", "iso", "eval", "inL", "inR"],
            P.reservedOpNames = ["+", "*", ",", ":", "<->", "()"],
            P.caseSensitive = True
          }
   in P.makeTokenParser def

parseProgFromFile :: FilePath -> IO (Either ParseError Prog)
parseProgFromFile fname = do
  input <- TIO.readFile fname
  pure $ parseProg fname input

parseProg :: SourceName -> Text -> Either ParseError Prog
parseProg = runParser progParser ()

progParser :: Parser Prog
progParser = do
  whiteSpace
  prog <- many defParser
  eof
  pure prog

defParser :: Parser Def
defParser = pData <|> pIso <|> pEval
  where
    pData = do
      reserved "type"
      tn <- TName <$> identifier
      void $ symbol "="
      args <- sepBy1 pConstructor (symbol "|")
      pure $ DataTyp tn args
    pIso =
      do
        reserved "iso"
        fname <- pFName
        void $ symbol ":"
        ityp <- iTypParser
        clauses <- many1 (symbol "|" >> clauseParser)
        pure $ Iso fname ityp clauses
        <?> "isomorphism"
    pEval =
      do
        reserved "eval"
        fname <- pFName
        val <- valParser
        pure $ Eval fname val
        <?> "evaluation"
    pConstructor = do
      cnstr <- CName <$> identifier
      typ <- option One typParser
      pure (cnstr, typ)

typParser :: Parser Typ
typParser = buildExpressionParser typTable simpleTypParser
  where
    simpleTypParser =
      (symbol "0" $> Zero)
        <|> (symbol "1" $> One)
        <|> (symbol "'" >> TypVar . VName <$> identifier)
        <|> (TypDef . TName <$> identifier)
        <|> parens typParser
    typTable =
      [ [Infix (reservedOp "*" $> Times) AssocLeft],
        [Infix (reservedOp "+" $> Plus) AssocLeft]
      ]

valParser :: Parser PVal
valParser = buildExpressionParser valTable simpleValParser
  where
    simpleValParser =
      reservedOp "()" $> Unit
        <|> try
          ( do
              fname <- pFName
              val <- simpleValParser
              pure $ App fname val
          )
        <|> try
          ( do
              vname <- pVName
              pure $ Var vname
          )
        <|> try
          ( do
              cname <- pCName
              val <- option Unit simpleValParser
              pure $ Constr cname val
          )
        <|> parens valParser
    valTable =
      [ [Prefix (reserved "inL" $> SumL), Prefix (reserved "inR" $> SumR)],
        [Infix (reservedOp "," $> Prod) AssocLeft]
      ]

iTypParser :: Parser ITyp
iTypParser =
  do
    t1 <- typParser
    reservedOp "<->"
    t2 <- typParser
    pure $ ITyp t1 t2
    <?> "type isomorphism"

clauseParser :: Parser Clause
clauseParser =
  do
    x1 <- valParser
    reservedOp "<->"
    x2 <- valParser
    pure $ Clause x1 x2
    <?> "clause"

pVName :: Parser VName
pVName = VName <$> lowercaseParse "variable name"

pFName :: Parser FName
pFName = FName <$> lowercaseParse "function name"

pCName :: Parser CName
pCName = CName <$> uppercaseParse "constructor name"

uppercaseParse :: String -> Parsec Text () Text
uppercaseParse msg =
  try
    ( do
        name <- identifier
        if startsWithUpper name
          then pure name
          else fail msg
    )

lowercaseParse :: String -> Parsec Text () Text
lowercaseParse msg =
  try
    ( do
        name <- identifier
        if startsWithLower name
          then pure name
          else fail msg
    )

identifier :: Parser Text
identifier = T.pack <$> P.identifier lexer

reserved, reservedOp :: String -> Parser ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

symbol :: String -> Parser Text
symbol = (T.pack <$>) . P.symbol lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

startsWithUpper :: Text -> Bool
startsWithUpper t =
  case T.uncons t of
    Just (c, _) -> isUpper c
    Nothing -> False

startsWithLower :: Text -> Bool
startsWithLower t =
  case T.uncons t of
    Just (c, _) -> isLower c
    Nothing -> False
