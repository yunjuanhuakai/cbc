module Main where

import Project

import qualified Data.Map.Strict as Map

import Text.PrettyPrint.GenericPretty

main :: IO ()
main = do
  r <- runMain (cbMain "C:\\Users\\makai\\learn\\haskell\\cbc\\cb-project")
  pp r