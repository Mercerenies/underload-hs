{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Underload.Evaluator (EvalState(), EvalError(), newEvalState, runTerm, runProgram) where

import Underload.Code (Code, Term(..), enclose, popLeft)
import Underload.Atom (Atom, atomChar)

import Control.Monad (void)
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

data EvalState = EvalState {
      evalStack :: [Code],
      evalContinuation :: Code
    } deriving (Show)

data EvalError = PopFromEmptyStack
               | UnknownInstruction Atom

instance Show EvalError where
    showsPrec _ PopFromEmptyStack = showString "Pop from empty stack"
    showsPrec _ (UnknownInstruction a) = showString "Unknown instruction: " . shows a

newEvalState :: Code -> EvalState
newEvalState cont = EvalState { evalStack = [], evalContinuation = cont }

doPush :: MonadState EvalState m => Code -> m ()
doPush value = modify (\state -> state { evalStack = value : evalStack state })

doPop :: (MonadState EvalState m, MonadError EvalError m) => m Code
doPop = do
  stack <- gets evalStack
  case stack of
    [] -> throwError PopFromEmptyStack
    (top:stack') -> do
                modify (\state -> state { evalStack = stack' })
                return top

cmdSwap :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdSwap = do
  x <- doPop
  y <- doPop
  doPush x
  doPush y

cmdDup :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdDup = do
  value <- doPop
  doPush value
  doPush value

cmdDiscard :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdDiscard = void doPop

cmdCat :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdCat = do
  x <- doPop
  y <- doPop
  doPush (x <> y)

cmdEnclose :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdEnclose = do
  x <- doPop
  doPush (enclose x)

cmdEval :: (MonadState EvalState m, MonadError EvalError m) => m ()
cmdEval = do
  code <- doPop
  modify $ \state -> state { evalContinuation = code <> evalContinuation state }

cmdOutput :: (MonadState EvalState m, MonadError EvalError m, MonadIO m) => m ()
cmdOutput = do
  value <- doPop
  liftIO $ putStr (show value)

runTerm :: (MonadState EvalState m, MonadError EvalError m, MonadIO m) => Term -> m ()
runTerm (Quoted code) = doPush code
runTerm (AtomTerm atom) = case atomChar atom of
                            '~' -> cmdSwap
                            ':' -> cmdDup
                            '!' -> cmdDiscard
                            '*' -> cmdCat
                            'a' -> cmdEnclose
                            '^' -> cmdEval
                            'S' -> cmdOutput
                            _ -> throwError $ UnknownInstruction atom

runProgram :: (MonadState EvalState m, MonadError EvalError m, MonadIO m) => m ()
runProgram =
    popInstruction >>= \case
      Nothing -> pure ()
      Just t -> runTerm t >> runProgram
 where popInstruction = do
               cont <- gets evalContinuation
               case popLeft cont of
                 Nothing -> pure Nothing
                 Just (t, code') -> do
                   modify $ \state -> state { evalContinuation = code' }
                   pure $ Just t
