{-# LANGUAGE FlexibleContexts #-}

module Underload.Util (satisfyMaybe) where

import Text.Parsec

import Data.Maybe (fromJust, isJust)

satisfyMaybe :: Stream s m Char => (Char -> Maybe a) -> ParsecT s u m a
satisfyMaybe f = (fromJust . f) <$> satisfy (isJust . f)
