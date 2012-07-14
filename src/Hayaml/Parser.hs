module Hayaml.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec

yamlSequence = keyValuePair `endBy` eol

keyValuePair = liftM2 (,) key value
key          = many nonDelimeterChar
value        = char ':' >> many nonDelimeterChar

nonDelimeterChar = noneOf ":\n"
eol = char '\n'

parseYml :: String -> Either ParseError [(String, String)]
parseYml input = parse yamlSequence "unexpected" input

