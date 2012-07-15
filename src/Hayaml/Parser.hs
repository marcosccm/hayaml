module Hayaml.Parser where

import Numeric
import Control.Monad
import Control.Applicative ((<$>), (<*>), (<$), empty)
import Text.ParserCombinators.Parsec

import Hayaml.Model.YamlNode

yamlSequence = YObject <$> parsedKeyValue `sepBy` eol
    where parsedKeyValue = (,) <$> key <*> yValue

key = many nonDelimeterChar

yValue = char ':' >> value
    where value = YNumber <$> parseNumber
              <|> YString <$> parseString

parseNumber = do stream <- getInput
                 case readSigned readFloat stream of 
                  [(n, stream')] -> n <$ setInput stream' 
                  _              -> empty 

parseString = many nonDelimeterChar

nonDelimeterChar = noneOf ":\n"
eol = char '\n'

parseYml :: String -> Either ParseError YamlNode
parseYml input = parse yamlSequence "unexpected" input

