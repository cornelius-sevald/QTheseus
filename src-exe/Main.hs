{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Prettyprinter (Pretty (pretty))
import QTheseus.Coverage (checkCoverage, reportCoverageErrors)
import QTheseus.Parser (parseProg)
import QTheseus.Pretty
import QTheseus.Semantics (eval, reportEvalError)
import QTheseus.Syntax (Def (..))
import QTheseus.TypeChecker (checkDupDefs, makeNamesUnique)
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
    Right uncheckedProg -> do
      TIO.putStrLn "Successfully parsed program:"
      TIO.putStrLn "============================"
      TIO.putStr $ renderText $ prettyProg uncheckedProg
      TIO.putStrLn "============================"
      TIO.putStrLn "Checking coverage of program..."
      case checkCoverage uncheckedProg of
        errs@(_ : _) -> do
          TIO.putStrLn "Encountered coverage errors:"
          TIO.putStrLn $ renderText $ reportCoverageErrors errs
          exitFailure
        [] -> do
          TIO.putStrLn "No coverage errors :)"
          TIO.putStrLn "Checking for duplicate definitions..."
          case checkDupDefs uncheckedProg of
            Just errs -> do
              TIO.putStrLn "Duplicate definitions definitions:"
              TIO.putStrLn $ T.pack $ show errs
            Nothing -> do
              TIO.putStrLn "No duplicate definitions :)"
              TIO.putStrLn "Making names globally unique..."
              let (prog, _) = makeNamesUnique mempty uncheckedProg
              TIO.putStrLn "Processed program:"
              TIO.putStrLn "============================"
              TIO.putStr $ renderText $ prettyProg prog
              TIO.putStrLn "============================"
              case eval prog of
                Left err -> do
                  TIO.putStrLn "Encountered evaluation error:"
                  TIO.putStrLn $ renderText $ reportEvalError err
                  exitFailure
                Right results -> do
                  let evals = [Eval iso val | Eval iso val <- uncheckedProg]
                  forM_ (zip results evals) $ \(res, ev) -> do
                    TIO.putStrLn $ renderText $ pretty ev
                    TIO.putStrLn $ T.pack $ "=> " <> show res
                  exitSuccess

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (input <**> helper)
        ( fullDesc
            <> progDesc "QTheseus interpreter."
        )
