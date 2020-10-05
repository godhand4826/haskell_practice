-- module JSON (
--     JSON(..), jsonParse, jsonString
-- ) where

import Data.Maybe
import Data.Functor
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

main :: IO ()
main = do
  let s = "{\"a\":1,\"b\":[true,{\"$\\t$\":0.1},[],\"FOO\",\"F\\\\F\",+3,-10, -5.3e3, -4.4E2, 1E23, false, [[null, [],1]]]}"
  case parse jsonParse "input" s of
    Left err -> print err
    Right j -> print j >> putStrLn (jsonString j)

data JSON = JSObject [(String, JSON)]
    | JSArray [JSON]
    | JSString String
    | JSNumber Double
    | JSBool Bool
    | JSNull deriving (Show, Eq)

jsonString :: JSON -> String
jsonString (JSObject kvs) = "{"++ intercalate ", " (map f kvs) ++"}"
    where f (s, j) =  "\"" ++ s ++ "\":" ++ jsonString j
jsonString (JSArray es) = "[" ++ intercalate ", " (map jsonString es) ++"]"
jsonString (JSNumber n) = show n
jsonString (JSString s) = "\"" ++ s ++ "\""
jsonString (JSBool b) = if b then "true" else "false"
jsonString JSNull = "null"

jsonParse :: Parser JSON
jsonParse = element

value :: Parser JSON
value = choice [ object, array, string', number, true, false, null' ]

object :: Parser JSON
object = between (char '{') (char '}') members

members :: Parser JSON
members = do
    spaces
    m <- optionMaybe member
    if isNothing m then
        return $ JSObject []
    else do
        next <- optionMaybe comma
        if isNothing next then
            return $ JSObject [fromJust m]
        else do
            JSObject ms <- members
            return $ JSObject (fromJust m:ms)

member :: Parser (String, JSON)
member = do
    ws
    JSString s <- string'
    ws
    char ':'
    e <- element
    return (s, e)

array :: Parser JSON
array = between (char '[') (char ']') elements

elements :: Parser JSON
elements = do
    spaces
    e <- optionMaybe element
    if isNothing e then
        return $ JSArray []
    else do
        next <- optionMaybe comma
        if isNothing next then
            return $ JSArray [fromJust e]
        else do
            JSArray es <- elements
            return $ JSArray (fromJust e:es)

element :: Parser JSON
element = between ws ws value

string' :: Parser JSON
string' = between (char '"') (char '"') characters

characters :: Parser JSON
characters = JSString <$> many character

character :: Parser Char
character = escape <|> unescaped

unescaped :: Parser Char
unescaped = noneOf "\"\\"

escape :: Parser Char
escape = do
    char '\\'
    choice [
        char '"',
        char '\\',
        char '/',
        '\b' <$ char 'b',
        '\f' <$ char 'f',
        '\n' <$ char 'n',
        '\r' <$ char 'r',
        '\t' <$ char 't',
        hex
        ]

hex :: Parser Char
hex = do
    char 'u'
    code <- sequence $ [1..4] $> alphaNum
    let s = "\'\\x" ++ code ++ "\'"
    return $ read s

number :: Parser JSON
number = fmap (JSNumber . read . ignorePlusSign) . many1 . oneOf $ ['0'..'9'] ++ "+-.Ee"
            where ignorePlusSign s = case s of
                                    ('+':xs) -> xs
                                    _ -> s

true :: Parser JSON
true  = JSBool True <$ string "true"

false :: Parser JSON
false = JSBool False <$ string "false"

null' :: Parser JSON
null' = JSNull <$ string "null"

comma :: Parser Char
comma =  between ws ws (char ',')

ws :: Parser String
ws = many $ oneOf " \n\r\t"
