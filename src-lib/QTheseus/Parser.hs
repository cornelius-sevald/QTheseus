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
import QTheseus.Core
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
            P.reservedNames = ["type", "iso", "eval", "inL", "inR"],
            P.reservedOpNames = ["+", "*", ",", ":", "<->", "()"],
            P.caseSensitive = True
          }
   in P.makeTokenParser def

parseProgFromFile :: FilePath -> IO (Either ParseError UncheckedProg)
parseProgFromFile fname = do
  input <- TIO.readFile fname
  pure $ parseProg fname input

parseProg :: SourceName -> Text -> Either ParseError UncheckedProg
parseProg = runParser progParser ()

progParser :: Parser UncheckedProg
progParser = do
  whiteSpace
  prog <- many defParser
  eof
  pure prog

defParser :: Parser (Def Name)
defParser = pData <|> pIso <|> pEval
  where
    pData = do
      reserved "type"
      tn <- pTName
      void $ symbol "="
      args <- sepBy1 pConstructor (symbol "|")
      pure $ DataTyp tn args
    pIso =
      do
        reserved "iso"
        name <- pFName
        void $ symbol ":"
        ityp <- iTypParser
        clauses <- many1 (symbol "|" >> clauseParser)
        pure $ Iso name ityp clauses
        <?> "isomorphism"
    pEval =
      do
        reserved "eval"
        name <- pFName
        val <- valParser
        pure $ Eval name val
        <?> "evaluation"
    pConstructor = do
      cnstr <- pCName
      typ <- option One typParser
      pure $ Constructor cnstr typ

typParser :: Parser (Typ Name)
typParser = buildExpressionParser typTable simpleTypParser
  where
    simpleTypParser =
      (symbol "0" $> Zero)
        <|> (symbol "1" $> One)
        <|> (symbol "'" >> TypVar <$> identifier)
        <|> (TypDef <$> pTName)
        <|> parens typParser
    typTable =
      [ [Infix (reservedOp "*" $> Times) AssocLeft],
        [Infix (reservedOp "+" $> Plus) AssocLeft]
      ]

valParser :: Parser (PVal Name)
valParser = buildExpressionParser valTable simpleValParser
  where
    simpleValParser =
      reservedOp "()" $> Unit
        <|> try (App <$> pFName <*> simpleValParser)
        <|> try (Var <$> pVName)
        <|> try (Constr <$> pCName <*> option Unit simpleValParser)
        <|> parens valParser
    valTable =
      [ [Prefix (reserved "inL" $> SumL), Prefix (reserved "inR" $> SumR)],
        [Infix (reservedOp "," $> Prod) AssocLeft]
      ]

iTypParser :: Parser (ITyp Name)
iTypParser =
  do
    t1 <- typParser
    reservedOp "<->"
    t2 <- typParser
    pure $ ITyp t1 t2
    <?> "type isomorphism"

clauseParser :: Parser (Clause Name)
clauseParser =
  do
    x1 <- valParser
    reservedOp "<->"
    x2 <- valParser
    pure $ Clause x1 x2
    <?> "clause"

uppercaseParse :: String -> Parsec Text () Name
uppercaseParse msg =
  try
    ( do
        name <- identifier
        if startsWithUpper $ nameToText name
          then pure name
          else fail msg
    )

lowercaseParse :: String -> Parsec Text () Name
lowercaseParse msg =
  try
    ( do
        name <- identifier
        if startsWithLower $ nameToText name
          then pure name
          else fail msg
    )

-- | Convenience functions for parsing
-- variable, function, type, and constructor names.
--
-- Note that here "VName" is short for variable name,
-- and does not refer to the `VName` type.... yeah.
pVName, pFName, pTName, pCName :: Parser Name
pVName = lowercaseParse "variable name"
pFName = lowercaseParse "function name"
pTName = uppercaseParse "type name"
pCName = uppercaseParse "constructor name"

identifier :: Parser Name
identifier = nameFromString <$> P.identifier lexer

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
