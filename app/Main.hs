{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Underload.Evaluator (EvalError, newEvalState, runProgram)
import Underload.Code (Code)
import Underload.Code.Parser (parseCode)

import Data.Function ((&))
import Data.String.Utils (rstrip)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  sourceFilename <- getFilename
  sourceFile <- rstrip <$> readFile sourceFilename
  parsedCode <- case parseCode sourceFilename sourceFile of
                  Left parseError -> do
                    putStrLn $ "Parse error: " ++ show parseError
                    exitFailure
                  Right parsedCode -> pure parsedCode
  runFile parsedCode >>= \case
    Left evalError -> do
         putStrLn $ "Eval error: " ++ show evalError
         exitFailure
    Right () -> pure ()

runFile :: Code -> IO (Either EvalError ())
runFile code = runProgram & runExceptT & flip evalStateT initialState
    where initialState = newEvalState code

getFilename :: IO FilePath
getFilename = getArgs >>= \case
                [x] -> pure x
                _ -> do
                  putStrLn "Expected usage: ./underload-hs <filename>"
                  exitFailure
