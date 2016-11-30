{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Text (unpack, pack)
import Data.Text (Text, isInfixOf, unpack)
import Problem1
import System.Environment
import System.IO

main = do
  x <- getArgs
  hhh (x !! 0) (read . (flip (!!) 1) $ x) (pack $ x !! 2) >>= putStrLn . concat . map display . concat . maybeToList

display :: (Googler, Text) -> String
display ((Googler a t u), text) = unpack $ t <> "\n" <> u <> "\n" <> a <> "\n" <> text

