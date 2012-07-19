module Hayaml.Parser where

import Numeric
import Control.Monad
import Control.Applicative ((<$>), (<*>), (<$), (*>), empty)
import Text.ParserCombinators.Parsec

import Hayaml.Model.YamlNode

yamlSequence = YObject <$> parsedKeyValue `sepEndBy` eol
    where parsedKeyValue = (,) <$> key <*> yValue

key = spaces *> many (noneOf ":")

yValue = spaces *> char ':' *> spaces *> value
    where value = YNumber <$> parseNumber
              <|> YString <$> parseString

parseNumber = do stream <- getInput
                 case readSigned readFloat stream of 
                  [(n, stream')] -> n <$ setInput stream' 
                  _              -> empty 

parseString = between strDelimiter strDelimiter str
    where strDelimiter = oneOf "'\""
          str          = many strChar
          strChar      = satisfy (`notElem` "\"\'\\")

eol = char '\n'

parseYml :: String -> Either ParseError YamlNode
parseYml input = parse yamlSequence "unexpected" input

