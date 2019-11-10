module Data.Jasty.AST where

import           Data.Scientific

data JValue
  = JObject [Member]
  | JArray [JValue]
  | JString String
  | JNumber Scientific
  | JTrue
  | JFalse
  | JNull
  deriving (Eq, Read, Show)

type Member = (String, JValue)
