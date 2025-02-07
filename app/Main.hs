module Main (main) where

import Free (parseJSON')
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Control.Monad ((<=<))
import System.Path.Glob (glob)

main :: IO ()
main = getArgs >>= traverse_ (traverse_ parseJSON' <=< glob) 
