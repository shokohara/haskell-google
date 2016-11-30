{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Problem1Spec (spec) where

import Prelude hiding (lines)
import Problem1
import Test.Hspec
import Test.HUnit
import Data.FileEmbed
import Data.Maybe
import Data.String.Here
import Data.Text (lines)
import qualified Data.Text.IO as T
import Debug.Trace
import Test.WebDriver

spec :: Spec
spec = do
  describe "premise" $
    it "words" $
      words [i|--noua --json|] `shouldBe` ["--noua", "--json"]
  describe "" $ do
    it "h" $
      length . fromJust <$> hhh "Haskell" 4 "haskell" `shouldReturn` 3
--    it "d" $
--      isJust <$> f "test" 1 `shouldReturn` True
--    it "f" $
--      length . fromJust <$> f "test" 1 `shouldReturn` 1

