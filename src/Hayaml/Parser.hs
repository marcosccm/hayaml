module Hayaml.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec

import Hayaml.Model.YamlNode


yamlSequence = keyValuePair `endBy` eol

keyValuePair = YObject `fmap` ((:[]) `fmap` parsedKeyValue) 
    where parsedKeyValue = liftM2 (,) key value

key = many nonDelimeterChar

value = YString `fmap` parsedValue
    where parsedValue = char ':' >> many nonDelimeterChar

nonDelimeterChar = noneOf ":\n"
eol = char '\n'

parseYml :: String -> Either ParseError [YamlNode]
parseYml input = parse yamlSequence "unexpected" input

