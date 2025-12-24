{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Underload.Code (Term(..), Code(..), singleton, enclose, popLeft) where

import Underload.Atom (Atom)

import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq

data Term = AtomTerm Atom | Quoted Code
            deriving (Eq)

newtype Code = Code (Seq Term)
    deriving (Eq, Semigroup, Monoid)

instance Show Term where
    showsPrec _ (AtomTerm atom) = shows atom
    showsPrec _ (Quoted code) = ("(" ++) . shows code . (")" ++)

instance Show Code where
    showsPrec _ (Code ts) = foldr (.) id $ fmap shows ts

singleton :: Term -> Code
singleton = Code . Seq.singleton

enclose :: Code -> Code
enclose = singleton . Quoted

popLeft :: Code -> Maybe (Term, Code)
popLeft (Code code) = case Seq.viewl code of
                        EmptyL -> Nothing
                        t :< code' -> Just (t, Code code')
