{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Free (parseJSON,parseJSON') where

{-
 -
-}


import Text.Parsec
    ( char,
      digit,
      hexDigit,
      satisfy,
      spaces,
      string,
      between,
      choice,
      optional,
      sepBy,
      (<|>),
      many1,
      runParser,
      ParsecT )
import Text.Parsec.String ()
import Data.String (IsString(..))
import Control.Monad (when,void)
import Control.Applicative (empty)
import Data.Functor.Identity (Identity)
import Data.Foldable (traverse_)

type Parser = ParsecT String () Identity ()


-------------------------------
-- Main combinators
-------------------------------

char' :: Char -> Parser
char' = void . char

digit' :: Parser 
digit' = void digit

whitespace :: Parser
whitespace = spaces 

digits :: Parser
digits = void $ many1 digit

digit19 :: Parser 
digit19 = digit >>= \d -> when (d == '0') empty

number :: Parser
number  
  = ("0" <|> optional "-" *> digit19)
  *> optional digits
  *> optional ("." >> digit')
  *> optional digits
  *> optional (("E" <|> "e") *> optional ("-" <|> "+") *> digits)

jstring :: Parser 
jstring = between (char' '"') (char' '"') $ optional . void . many1 $ special <|> nonSpecial
  where 
  nonSpecial :: Parser 
  nonSpecial = void $ satisfy (liftA2 (&&) (/= '"') (/= '\\'))
  special :: Parser 
  special = char' '\\' *> choice 
    [   "\""
    , "\\"
    , "/"
    , "b"
    , "f"
    , "n"
    , "r"
    , "t"
    , "u" *> traverse_ (const $ void hexDigit) [(1 :: Int)..4]
    ]

-------------------------------
-- Orphans
-------------------------------

instance (a ~ ()) => IsString (ParsecT String () Identity a) where
  fromString str 
    = void $ string str


-------------------------------
-- Parser
-------------------------------


object :: Parser 
object = between "{" "}" $ 
  whitespace *> void ((whitespace *> jstring *> whitespace *> ":" *> value) `sepBy` ",")

array :: Parser 
array = between "[" "]" $ 
  whitespace *> void (value `sepBy` "," )

value :: Parser 
value = whitespace *> choice 
  [ number 
  , object 
  , array 
  , "true"
  , "false"
  , "null"
  , jstring
  ] *> whitespace

json :: Parser
json = object <|> array 

parseJSON :: FilePath -> IO String 
parseJSON fp = either (\e -> "error in " <> show e) (const "OK!") . runParser json () fp <$> readFile fp

parseJSON' :: FilePath -> IO () 
parseJSON' fp = putStrLn ("Parsing: " <> fp) >> (parseJSON fp >>= putStrLn) <* putStrLn "\n"

test :: IO ()
test = traverse_ parseJSON' $ (mappend "./jsons/") <$> 
  [ "numbers.json"
  , "t1.json"
  , "t2.json"
  , "t3.json"
  , "t4.json"

  ]
