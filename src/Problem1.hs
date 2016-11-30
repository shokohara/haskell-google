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
import Data.Monoid
import Data.String.Here
import Data.Text (Text, isInfixOf, unpack)
import GHC.Generics
import Prelude
import System.Process.ByteString
import Test.WebDriver

data Googler = Googler { abstract, title, url :: Text } deriving (Generic, Eq)

instance FromJSON Googler

hhh :: String -> Int -> Text -> IO (Maybe [(Googler, Text)])
hhh k c r = runMaybeT $ do
  googlers <- MaybeT $ decodeToGooglers <$> search k c
  texts <- lift $ (mapM crawl googlers >>= return)
  googlerText <- return $ zip googlers texts
  return $ filter (isInfixOf r . snd) $ googlerText

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

