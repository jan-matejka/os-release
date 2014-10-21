module OsReleaseSpec (spec) where

import           Test.Hspec
import           System.OsRelease

spec :: Spec
spec = do
    describe "detect" $ do
        it "parses" $ do
            detect' ["NAME=openSUSE"]      `shouldBe` Just OpenSUSE
            detect' []                     `shouldBe` Nothing
            detect' ["NAME=open"]          `shouldBe` Nothing
            detect' ["NAME=\"openSUSE\""]  `shouldBe` Nothing
