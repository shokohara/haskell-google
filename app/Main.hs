{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Text (Text, unpack, intercalate, pack)
import Problem1
import System.Console.ANSI
import System.Environment

main :: IO ()
main = do
  x <- getArgs
  v <- hhh (x !! 0) (read . (flip (!!) 1) $ x) (pack $ x !! 2)
--  putStrLn "testn\ntest\ntest\n"
--  putStrLn . display2 . concat . maybeToList $ v
  mapM_ display (zip [0..] . concat . maybeToList $ v)

display :: (Int, (Googler, [Text])) -> IO ()
display (n, ((Googler a t u), ts)) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
  putStr . flip (++) " " . show $ n
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  putStrLn . unpack $ t
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
  putStrLn . unpack $ u
  setSGR []
  putStrLn . unpack $ intercalate "\n" ts
  putStrLn ""

instance Show Googler where
  show (Googler _ title url) = unpack $ title <> "\n" <> url <> "\n"


