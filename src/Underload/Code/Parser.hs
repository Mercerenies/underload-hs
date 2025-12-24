
module Underload.Code.Parser (code, parseCode) where

import Underload.Code (Term(..), Code(..))
import Underload.Atom (Atom, tryAtom)
import Underload.Util (satisfyMaybe)

import Text.Parsec
import qualified Data.Sequence as Seq

type Parser = Parsec String ()

atom :: Parser Atom
atom = satisfyMaybe tryAtom

quoted :: Parser Term
quoted = Quoted <$> between (char '(') (char ')') code

term :: Parser Term
term = quoted <|> AtomTerm <$> atom

code :: Parser Code
code = Code . Seq.fromList <$> many term

parseCode :: SourceName -> String -> Either ParseError Code
parseCode = parse code
