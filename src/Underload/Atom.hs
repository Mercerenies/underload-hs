{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Underload.Atom (Atom(), atomChar, tryAtom, atom) where

import Data.Maybe (maybeToList)

-- A character other than a literal parenthesis.
newtype Atom = Atom Char
    deriving newtype (Show, Eq)

atomChar :: Atom -> Char
atomChar (Atom ch) = ch

tryAtom :: Char -> Maybe Atom
tryAtom ch = if ch == '(' || ch == ')' then Nothing else Just (Atom ch)

atom :: Char -> Atom
atom ch = case tryAtom ch of
            Just x -> x
            Nothing -> error "Underload.Atom: atom: invalid character"

instance Read Atom where
    readsPrec _ [] = []
    readsPrec _ (s:ss) = maybeToList $ fmap (\atom -> (atom, ss)) (tryAtom s)
