{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Ift where

import Text.Parsec 
import Data.String
import Data.Functor.Identity
import Control.Monad
import Data.Either 
import Data.Foldable

connector :: Int -> Bool 
connector = (/=) 0

bConnector :: Bool -> Int 
bConnector True  = 1
bConnector False = 0


type Parser a = ParsecT String () Identity a 


-------------------------------
-- Orphans
-------------------------------

instance (a ~ String) => IsString (ParsecT String () Identity a) where
  fromString str 
    = string str <* spaces


-------------------------------
-- Theoretical answers
-------------------------------
-- 1.a y 1.b

-- p -> parseIf 
-- parseIf -> if p then p aux {parseIf.val = if p1.val /= 0 then p2.val else maybe 0 id aux.val}
-- parseIf -> parsePlus {parseIf.val = parsePlus.val}
-- parsePlus -> parseTerminal parsePlusTail {parsePlus.val = parseTerminal.val + parsePlusTail.val}
-- aux -> else p {aux.val = p1.val}
-- aux -> lambda {aux.val = 0}
-- parseTerminal -> n {parseTerminal.val = n.val}
-- parsePlusTail -> + parsePlus {parsePlusTail.val = parsePlus.val}
-- parsePlusTail -> lambda {parsePlusTail.val = 0}

-- 1.c 
-- pseudocodigo que corre como un plus.
parseIf = (f <$> ("if" *> p) <*> ("then" *> p) <*> aux) <|> parsePlus
  where f a b c = if (connector a) then b else c 

parsePlus = f <$> parseTerminal <*> parsePlusTail 
  where f = (+)

aux = ("else" *> p) <|> pure 0

parseTerminal :: Parser Int
parseTerminal = f <$> many "-" <*> many1 digit <* spaces
  where f m s = read $ concat m <> s

parsePlusTail = ("+" *> parsePlus) <|> pure 0

p = parseIf

parseIFT :: String -> Either ParseError Int
parseIFT = runParser p () "" 


-------------------------------
-- Test cases
-------------------------------


parseCases = 
  [ "20"
  , "if 1 then 1 else 0"
  , "if 0 then 1 else 0"
  , "3 + 2"
  , "if -1 + 1 then 1 else 0"
  , "if -1 + 3 then 1 else 0"
  , "if -1 + 1 then 1 + 1 else 2 + 2 "
  , "if -1 + 3 then 1 + 1 else 2 + 2"
  , "if 1 then 1"
  , "if 0 then 1"
  , "if 1 then if 1 then 1 else 2 else 3"
  , "if 1 then if 0 then 1 else 2 else 3"
  , "if 0 then if 1 then 1 else 2 else 3"
  , "if 1 then if 1 then 1 else 2"
  , "if 1 then if 0 then 1 else 2"
  , "if 0 then if 1 then 1 else 2"
  ]

expectedResults :: [Int]
expectedResults =
  [ 20
  , 1
  , 0
  , 5
  , 0
  , 1
  , 4
  , 1 + 1
  , 1
  , 0
  , 1
  , 2 
  , 3
  , 1
  , 2
  , 0
  ]

parseTests :: IO ()
parseTests = traverse_ ((either print print) . parseIFT) $ parseCases

valTests :: [Int]
valTests 
  = let rs = zipWith (==) (fmap Right expectedResults) (fmap parseIFT parseCases)
    in [i | (False,i) <- zip rs [1..]]
