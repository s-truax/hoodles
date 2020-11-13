import Data.Map.Lazy (Map)

data JSON =
  JSONStr String                     |
  JSONBool Bool                      |
  Object (Map String JSON) |
  Array [JSON]
  deriving (Show)

-- I was originally returning a (Maybe JSON, Maybe String),
-- but then got inspired to do this by a stackoverflow post talking about
-- Haskell constants. It's like we don't even really need constants.
-- This feels more idomatic to Haskell, and feels a lot better than
-- writing (Nothing, Nothing) for parse errors.

data ParseResult = ParsedJson JSON String | ParseError

-- I just discovered I need getter functions for the above datatype.

parsedJson :: ParseResult -> Maybe JSON
parsedJson ParseError             = Nothing
parsedJson (ParsedJson json src)  = json

parsedSrc :: ParseResult -> Maybe JSON
parsedSrc ParseError        = Nothing
parsedSrc (ParsedJson json src) = json

-- I've just realized we need to return a tuple with a string
-- because after we parse the next "element", we need to know what's
-- left of the stream.

-- For next time: JSON defines 7 values, 4 of which are compound
-- and 3 of which are primitive. I think we need functions to read
-- all of the compound values, at least. Then a function to read the
-- primitive values `null`, `true` and `false`.

readObject :: String -> Maybe (JSON, String)
readObject [] = Nothing
readObject (x:xs)
    | x == '}'  = Just (EmptyJSON, xs)
    | otherwise = Nothing

-- assume input is a token stream.

parseJson :: String -> Maybe JSON
parseJson "" = Just EmptyJSON
parseJson (x:xs)
    | x == '{'   = readObject xs
    | x == '['   = readArray xs
    | x == '"'   = readString xs
    | otherwise  = readBool xs

readValue :: String -> ParseResult
readValue "" = ParseError
readValue (x:xs)
    | x == '{'                = readObject xs
    | x == '['                = readArray xs
    | x == '"'                = readString xs
    | elem x ['t', 'f', 'n']  = readAtom xs
    | elem x ('-':['0'..'9']) = readNum xs
    | otherwise               = ParseError

readArray :: String -> ParseResult
readArray "" = ParseError
readArray (x:xs)
    | x == ']'  = Array []
    | otherwise = maybe ParseError (ParsedJson firstVal) restVals
    where firstVal  = parsedJson $ readVal xs
          src1      = parsedSrc  $ readVal xs
          restParse = readArray $ tail src1
          restVals  = parsedJson $ restParse
          restSrc   = parsedSrc $ restParse

