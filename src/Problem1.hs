{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Problem1 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString (empty)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Data.String.Here
import Data.Text (Text, isInfixOf, unpack, lines)
import GHC.Generics
import Prelude hiding (lines)
import System.Process.ByteString
import Test.WebDriver
import Data.Tuple.HT

data Googler = Googler { abstract, title, url :: Text } deriving (Generic, Eq)

instance FromJSON Googler

hhh :: String -> Int -> Text -> IO (Maybe [(Googler, [Text])])
hhh k c r = runMaybeT $ do
  googlers <- MaybeT $ decodeToGooglers <$> search k c
  texts <- lift $ (mapM crawl googlers >>= return)
  googlerText <- return $ zip googlers texts
  return . filter (not . null . snd) . map (mapSnd (filter (isInfixOf r) . lines)) $ googlerText

search :: String -> Int -> IO ByteString
search k c = do
  (_, stdout, _) <- readProcessWithExitCode "googler" (words [i|--noua --json --count ${c} --exact ${k}|]) empty
  return . BL.fromStrict $ stdout

decodeToGooglers :: ByteString -> Maybe [Googler]
decodeToGooglers x = decode x :: Maybe [Googler]

crawl :: Googler -> IO Text
crawl v = runSession defaultConfig . finallyClose $ do
  openPage . unpack . url $ v
  x <- findElem . ByXPath $ "//body/."
  getText x

check::[Char]->[Char]->Bool
check l s = check' l s True where
  check' _ [] h          = True
  check' [] _ h          = False
  check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)

--check :: Eq a => [a] -> [a] -> Bool
--check l s = check' l s True where
--    check' _ [] h          = True
--    check' [] _ h          = False
--    check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)
--data X = MyText Text | HighlightedText Text
--
--highlight :: Text -> Text -> [X]
--highlight src word =
