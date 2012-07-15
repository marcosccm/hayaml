module Hayaml.Parser where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec

import Hayaml.Model.YamlNode

yamlSequence = YObject <$> parsedKeyValue `sepBy` eol
    where parsedKeyValue = (,) <$> key <*> value

key = many nonDelimeterChar

value = YString <$> parsedValue
    where parsedValue = char ':' >> many nonDelimeterChar

nonDelimeterChar = noneOf ":\n"
eol = char '\n'

parseYml :: String -> Either ParseError YamlNode
parseYml input = parse yamlSequence "unexpected" input

