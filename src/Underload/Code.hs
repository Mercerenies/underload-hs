{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Underload.Code (Term(..), Code(..)) where

import Underload.Atom (Atom)

import Data.Sequence (Seq)

data Term = AtomTerm Atom | Quoted Code
            deriving (Eq)

newtype Code = Code (Seq Term)
    deriving (Eq, Semigroup, Monoid)

instance Show Term where
    showsPrec _ (AtomTerm atom) = shows atom
    showsPrec _ (Quoted code) = ("(" ++) . shows code . (")" ++)

instance Show Code where
    showsPrec _ (Code ts) = foldMap shows ts
