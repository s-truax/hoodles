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

parsedVal :: ParseResult -> Maybe JSON
parsedVal ParseError             = Nothing
parsedVal (ParsedJson val src)  = Just val

parsedSrc :: ParseResult -> Maybe String
parsedSrc ParseError            = Nothing
parsedSrc (ParsedJson val src)  = Just src

maybeErr = maybe ParseError

-- I've just realized we need to return a tuple with a string
-- because after we parse the next "element", we need to know what's
-- left of the stream.

-- For next time: JSON defines 7 values, 4 of which are compound
-- and 3 of which are primitive. I think we need functions to read
-- all of the compound values, at least. Then a function to read the
-- primitive values `null`, `true` and `false`.

-- assume input is a token stream.

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
    | x == ']'  = ParsedJson (Array []) xs
    | otherwise = ParsedJson result restSrc
    where firstVal  = parsedVal $ readVal xs
          src1      = parsedVal $ readVal xs
          restParse = maybeErr (readArray . tail) src1
          restVals  = maybeErr parsedJson restParse
          restSrc   = maybeErr parsedSrc restParse
          result    = maybeErr (ParsedJson . (:) firstVal) restVals

readObject :: String -> ParseResult
readObject _ = ParseError

readNum :: String -> ParseResult
readNum _ = ParseError

readAtom :: String -> ParseResult
readAtom _ = ParseError

readString :: String -> ParseResult
readString _ = ParseError

-- The maybe types are killing me. Everything has a maybe condition.
-- I think the better way is really just to do it with a
-- (Maybe JSON, Maybe String) deal.
-- So I'm kind of seeing the deal here. I think I need to define all of
-- my datatypes and then write intuitive interfaces for them in the form
-- of functions. Ok. Go back to the drawing board and write out how you would
-- want to interact with these data.

{-
I honestly don't even know what a good way to represent a JSON would be.
Why would I even want to read a JSON into Haskell? Well, JSON is an
interchange / serialization format, so really it's just a human-readable
way of serializing data that is also easy (lol) to parse. I'm in a Python
mindset: you can parse a JSON directly into a Python dictionary, which is
very natural. An easier task might be writing some data out to a JSON from
Haskell. But I guess it's also reasonable to have a standard Haskell JSON
datatype that could be passed between functions. What should that be?
-}
