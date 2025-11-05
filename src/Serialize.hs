module Serialize where

import Data.List (intercalate)
import JSON (JSON(..))

serialize :: JSON -> String
serialize JNull = "null"
serialize (JBoolean b) = if b then "true" else "false"
serialize (JInt n) = show n
serialize (JFloat d) = show d
serialize (JString s) = show s
serialize (JArray arr) = "[" ++ intercalate "," (map serialize arr) ++ "]"
serialize (JObject []) = ""
serialize (JObject ((s, json) : rest)) =
    "{" ++ show s ++ ":" ++ serialize json ++ "}" ++ serialize (JObject rest)

-- Test cases:
-- λ> putStrLn (serialize $ JInt 12)
-- 12
-- λ> putStrLn (serialize $ JBoolean True)
-- true
-- λ> putStrLn (serialize $ JFloat 4.58329)
-- 4.58329
-- λ> putStrLn (serialize $ JString "happy happy joy joy")
-- "happy happy joy joy"
-- λ> serialize $ JString "happy happy joy joy"
-- "\"happy happy joy joy\""
-- λ> putStrLn (serialize $ JArray [JInt 1, JInt 2, JInt 3])
-- [1,2,3]
-- λ> putStrLn (serialize $ JArray [JBoolean False, JBoolean True])
-- [false,true]
-- λ> example1 = JObject [ ("key", JString "value") ]
-- λ> putStrLn . serialize $ example1
-- {"key":"value"}
-- λ> example2 = JObject [ ("dudebro", JArray [JInt 4, JFloat 8.75, JNull]) ]
-- λ> putStrLn . serialize $ example2
-- {"dudebro":[4,8.75,null]}
-- λ> example3 = JObject [ ("", JArray []) ]
-- λ> putStrLn . serialize $ example3
-- {"":[]}
