{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Problem1Spec (spec) where

import Prelude hiding (lines)
import Test.Hspec
import Data.List as L
import Data.String
import Data.String.Here

x :: Bool
x = Prelude.elem ('a' :: Char) ("abc" :: [Char])

spec :: Spec
spec = do
  describe "premise" $ do
    it "words" $
      words [i|--noua --json|] `shouldBe` ["--noua", "--json"]
    it "intercalate" $
      L.intercalate "\n" ["", ""] `shouldBe` "\n"
    it "highlight" $
      (Prelude.elem ("ab" :: [Char]) ("abc" :: String)) `shouldBe` True
      --["a", "bc"]
--  describe "" $ do
--    it "h" $
--      length . fromJust <$> hhh "Haskell" 1 "haskell" `shouldReturn` 1
--    it "d" $
--      isJust <$> f "test" 1 `shouldReturn` True
--    it "f" $
--      length . fromJust <$> f "test" 1 `shouldReturn` 1

--check::[Char]->[Char]->Bool
--check l s = check' l s True where
--  check' _ [] h          = True
--  check' [] _ h          = False
--  check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)
--
