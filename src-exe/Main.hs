{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Options.Applicative
import QTheseus.Parser (parseProg)
import QTheseus.Pretty (prettyProgText)
import System.Exit (die)
import System.IO

data Input
  = FileInput FilePath
  | StdInput
  deriving (Eq)

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file"
      )

stdInput :: Parser Input
stdInput =
  flag'
    StdInput
    ( long "stdin"
        <> help "Read from stdin"
    )

input :: Parser Input
input = fileInput <|> stdInput

run :: Input -> IO ()
run inpt = do
  let source = case inpt of
        FileInput f -> f
        StdInput -> "<stdin>"
  handle <- case inpt of
    FileInput f -> openFile f ReadMode
    StdInput -> pure stdin
  contents <- TIO.hGetContents handle
  case parseProg source contents of
    Left err -> die $ show err
    Right prog -> do
      TIO.putStrLn "Successfully parsed program:"
      TIO.putStr $ prettyProgText prog

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (input <**> helper)
        ( fullDesc
            <> progDesc "QTheseus interpreter."
        )
