{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IFT where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (words)
import GHC.Stack (HasCallStack)
import Data.Foldable 
import Control.Lens
import Data.Char (isUpper,isLower)

{-
Initial Grammar

```
IFT = ({n,+,if,then,else},{E},P,E)

E -> if E then E else E
  | if E then E 
  | E + E
  | n
```
-}


-------------------------
-- 2.a
-- ----------------------


{-
 Extended Grammar with initial non recursive symbol `S`-

```
S -> E
E -> if E then E else E
  | if E then E 
  | E + E
  | n
```
 -
-}

data Tokens = If | Then | Else | (:+:) | N deriving Eq 

instance Show Tokens where 
  show If    = "if"
  show Then  = "then"
  show Else  = "else"
  show (:+:) =  "+"
  show N     = "n"

type RuleNumber = Int
type DotPos     = Int
type LookAhead  = String
type Token      = String
type Item = (RuleNumber,DotPos,LookAhead)
type ItemSet = Set Item 


grammar :: [String]
grammar = 
  [ "S -> E"
  , "E -> if E then E else E"
  , "E -> if E then E"
  , "E -> E + E"
  , "E -> n $"
  ]

terminal :: String -> Bool
terminal x' = x' `elem` ["if","then","else","+","n", "$", "epsilon"]

nonTerminal :: String -> Bool
nonTerminal = not . terminal 


tokenize :: String -> [Token]
tokenize = words 

tokenizedGrammar :: [[Token]]
tokenizedGrammar = tokenize <$> grammar

classifiedGrammar :: [(Token,[Token])]
classifiedGrammar = classify <$> tokenizedGrammar

-- E -> XXXX ~ (E,XXXX)
classify :: HasCallStack => [Token] -> (Token,[Token])
classify (x:_:xs) = (x,xs)
classify xs = error $ "Impossible case for classify in production: " <> concat xs

-- items :: HasCallStack => ItemSet 
-- items = Set.fromList $ do 
--   (ruleNumber,(_,xs)) <- fmap (classify . tokenize) <$> [(0 :: Int)..] `zip` grammar
--   dotPos <- fst <$> [(0 :: Int)..] `zip` xs
--   -- we dont care about duplicates
--   [(ruleNumber,dotPos),(ruleNumber,dotPos + 1)]

productionsStartingBy :: Token -> [[Token]]
productionsStartingBy s = classifiedGrammar ^.. folded . filtered ((==) s . fst) . _2

first :: Token -> Set Token
first = first' Set.empty 
  where 
  first' acc x | terminal x = Set.insert x acc 
  first' acc x = let 
    ss      = head <$> productionsStartingBy x
    terms    = ss ^.. folded . filtered terminal 
    nonterms = ss ^.. folded . filtered nonTerminal 
    acc'     = Set.fromList terms `Set.union` acc
    in case acc == acc' of 
      True -> acc 
      False -> Set.unions $ first' acc' <$> nonterms

efirst :: [Token] -> Set Token 
efirst = flip foldl' (Set.singleton "epsilon") $ \acc s -> case "epsilon" `Set.member` acc  of
  True  -> Set.filter (/= "epsilon") acc `Set.union` first s
  False -> acc

closure :: ItemSet -> ItemSet 
closure i = undefined 
  where 
  matchingItem :: Item -> [([Token],Token,[Token])]
  matchingItem (ruleNumber,dotPos,_) = let 
    x0 = classifiedGrammar ^? ix ruleNumber >>= (f dotPos . snd)
    x1  = maybe [] pure x0  
        & toListOf (folded . filtered (\s -> s ^. _2 & all isUpper))
    in x1

  matchingProduction :: Token -> [[Token]]
  matchingProduction s = classifiedGrammar ^.. folded . filtered ((==) s . fst) . _2 

  matchingTerminal :: [Token] -> String -> [Token]
  matchingTerminal beta a = Set.toList $ efirst (beta <> pure a)

  
  f :: Int -> [String] -> Maybe ([String],String,[String])
  f n s = drop n s  ^? ix 0 >>= \b -> pure (take n s,b,drop (n+1) s) 
  
  newI = undefined 
