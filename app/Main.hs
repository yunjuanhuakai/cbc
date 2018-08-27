module Main where

import Project

import qualified Data.Map.Strict as Map

import Text.PrettyPrint.GenericPretty

main :: IO ()
main = do
  r <- runMain (cbMain "/home/makai/Project/haskell/cbc/cb-project")
  -- mapM_ (mapM_ (mapM_ print)) $ fst r
  print $ snd r