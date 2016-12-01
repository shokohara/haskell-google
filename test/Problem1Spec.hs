{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Problem1Spec (spec) where

import Prelude hiding (lines)
import Test.Hspec
import Data.Maybe
import Data.List as L
import Data.String.Here

spec :: Spec
spec = do
  describe "premise" $ do
    it "words" $
      words [i|--noua --json|] `shouldBe` ["--noua", "--json"]
    it "intercalate" $
      L.intercalate "\n" ["", ""] `shouldBe` "\n"
--  describe "" $ do
--    it "h" $
--      length . fromJust <$> hhh "Haskell" 1 "haskell" `shouldReturn` 1
--    it "d" $
--      isJust <$> f "test" 1 `shouldReturn` True
--    it "f" $
--      length . fromJust <$> f "test" 1 `shouldReturn` 1

