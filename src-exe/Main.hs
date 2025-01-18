{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Options.Applicative
import QTheseus.Coverage (checkCoverage, reportErrors)
import QTheseus.Parser (parseProg)
import QTheseus.Pretty
import System.Exit (die, exitFailure, exitSuccess)
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
  TIO.putStrLn "Parsing program..."
  case parseProg source contents of
    Left err -> die $ show err
    Right prog -> do
      TIO.putStrLn "Successfully parsed program:"
      TIO.putStrLn "============================"
      TIO.putStr $ renderText $ prettyProg prog
      TIO.putStrLn "============================"
      TIO.putStrLn "Checking coverage of program..."
      case checkCoverage prog of
        [] -> do
          TIO.putStrLn "No coverage errors :)"
          exitSuccess
        errs -> do
          TIO.putStrLn "Encountered coverage errors:"
          TIO.putStrLn $ renderText $ reportErrors errs
          exitFailure

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (input <**> helper)
        ( fullDesc
            <> progDesc "QTheseus interpreter."
        )
