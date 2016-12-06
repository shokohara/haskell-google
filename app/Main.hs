{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Maybe (maybeToList)
import Data.Text (Text, unpack, intercalate, pack)
import Problem1
import System.Console.ANSI
import System.Environment
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  when (args `isPresent` (command "search")) $ do
    keyword <- args `getArgOrExit` argument "keyword"
    count <- args `getArgOrExit` longOption "count"
    scount <- args `getArgOrExit` longOption "ccount"
    word <- args `getArgOrExit` argument "word"
    v <- hhh keyword (read count) (read scount) (pack word)
    mapM_ display (zip [0..] . concat . maybeToList $ v)

display :: (Int, (Googler, [Text])) -> IO ()
display (n, ((Googler _ t u), ts)) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
  putStr . flip (++) " " . show $ n
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
  putStrLn . unpack $ t
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
  putStrLn . unpack $ u
  setSGR []
  putStrLn . unpack $ intercalate "\n" ts
  putStrLn ""

