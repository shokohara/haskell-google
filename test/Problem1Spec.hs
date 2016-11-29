{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Problem1Spec (spec) where

import Prelude hiding (lines)
import Problem1
import Test.HUnit
import Test.Hspec
--import Test.Hspec.Contrib.HUnit
import Data.String.Here
import Data.Maybe
import Data.Text (lines)
import qualified Data.Text.IO as T
import Test.WebDriver
import Debug.Trace
import Data.FileEmbed

spec :: Spec
spec = do
  describe "premise" $
    it "words" $
      words [i|--noua --json|] `shouldBe` ["--noua", "--json"]
  describe "" $ do
    it "h" $
      length . fromJust <$> h "Haskell" 4 `shouldReturn` 4
    it "d" $
      isJust <$> f "test" 1 `shouldReturn` True
    it "f" $
      length . fromJust <$> f "test" 1 `shouldReturn` 1

