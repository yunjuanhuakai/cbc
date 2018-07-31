module Main where

import Project

import qualified Data.Map.Strict as Map

import Text.PrettyPrint.GenericPretty

main :: IO ()
main = do
  r <- runMain (cbMain "/home/makai/Project/haskell/cbc/cb-project")
  pp $ fst r
  print $ snd r