{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Problem1 where

import Data.Aeson
import Data.ByteString (empty)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Data.String.Here
import Data.Text (Text, isInfixOf, unpack)
import GHC.Generics
import Prelude
import System.Process.ByteString
import Test.WebDriver
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

data Googler = Googler { abstract, title, url :: Text } deriving (Show, Generic, Eq)

instance FromJSON Googler

g :: String -> Int -> IO ByteString
g k c = do
  (_, stdout, _) <- readProcessWithExitCode "googler" (words [i|--noua --json --count ${c} --exact ${k}|]) empty
  return . BL.fromStrict $ stdout

f :: String -> Int -> IO (Maybe [Googler])
f k c = dc <$> g k c

j :: [Googler] -> IO [Text]
j = mapM toSrc

k :: String -> Int -> MaybeT IO [(Googler, Text)]
k url c = do
  googlers <- MaybeT $ f url c
  x <- lift $ (j googlers >>= return)
  return $ zip googlers x

l :: String -> Int -> MaybeT IO [(Googler, Text)]
l url c = do
  googlers <- MaybeT $ f url c
  x <- lift $ (j googlers >>= return)
  gt <- return $ zip googlers x
  return $ filter (isInfixOf "Haskell" . snd) $ gt

h :: String -> Int -> IO (Maybe [(Googler, Text)])
h k c = runMaybeT $ (l k c)

dc :: ByteString -> Maybe [Googler]
dc x = decode x :: Maybe [Googler]

toSrc :: Googler -> IO Text
toSrc v = runSession defaultConfig $ do
  openPage . unpack . url $ v
  x <- findElem . ByXPath $ "//body/."
  getText x

