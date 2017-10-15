-- A JSON library written in Haskell, adapted from an example
-- from "Real World Haskell".

-- Note that the module name must match the name of the file.
module JSON (
  JValue(..), --The (..) means that we are exporting the type and all of its constructors
  toString,
  isNull,
  jsonRightTriangles)
where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord, Show)


--The type annotation indicates that the function takes a
toString :: JValue      -> String
toString (JString s)    = "\"" ++ s ++ "\""
toString (JNumber n)    = show n
toString (JBool True)   = "true"
toString (JBool False)  = "false"
toString JNull          = "null"
toString (JArray lst)   = "[" ++ list2str lst ++ "]"
-- Comment out the next line, recompile, and call toString with a JObject.  Note the error.
toString (JObject o) =
  "{\n" ++ obj2str o ++ "\n}" -- added by sidmishraw for JObject strings

-- added by sidmishraw for JObject strings
obj2str :: [(String, JValue)] -> String
obj2str [] = ""
obj2str (x:[]) = ("\"" ++ fst x ++ "\" : " ++ toString (snd x))
obj2str (x:xs) = obj2str (x:[]) ++ ",\n" ++ obj2str xs 

list2str :: [JValue] -> String
list2str [] = ""
list2str (x:[]) = (toString x)
list2str (x:xs) = (toString x) ++ ",\n " ++ (list2str xs)

isNull JNull  = True
-- The following 5 lines are replaced with the _ version
--isNull (JString s) = False
--isNull (JNumber n) = False
--isNull (JBool b)   = False
--isNull (JObject o) = False
--isNull (JArray a)  = False
isNull _      = False

jsonRightTriangles = JArray [ JObject [("a",JNumber a), ("b",JNumber b), ("c",JNumber c)] |
  a<-[1..10],
  b<-[1..10],
  c<-[1..10],
  a^2 + b^2 == c^2]


