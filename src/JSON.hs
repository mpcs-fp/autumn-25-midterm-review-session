module JSON where

data JSON
  = JNull
  | JBoolean Bool
  | JInt Int
  | JFloat Double
  | JString String
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Show, Eq)

data LookupStep
  = O String
  | A Int
  deriving Show
