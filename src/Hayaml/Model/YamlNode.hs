module Hayaml.Model.YamlNode where

type Key = String
data YamlNode = Blank 
              | YString String
              | YNumber Float
              | YObject [(Key, YamlNode)]
              | YArray  [YamlNode]
                deriving (Eq, Ord, Show)

