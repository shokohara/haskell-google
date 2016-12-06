{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Problem1Spec (spec) where

import Prelude hiding (lines)
import Test.Hspec
import Data.List as L
import Data.String
import Data.String.Here
import Problem1
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "decodeToGoogler" $
    it "" $
      (isJust . decodeToGoogler) [i|{"abstract":"","title":"","url":""}|] `shouldBe` True

