module OsReleaseSpec (spec) where

import           Test.Hspec

import Prelude hiding (EQ, LT, GT)
import System.OsRelease
import System.Environment.XDG.BaseDir
import Data.Maybe

type OsId = String

spec :: Spec
spec = do
    describe "nmap" $ do
        it "works" $ do
            cg <- getUserConfigFile "os-release" "acceptance.cf"
            f  <- readFile cg
            let osid = fromJust . readOsId $ lines f

            x <- detect
            show x `shouldBe` osid

readOsId :: [String] -> Maybe OsId
readOsId (x:_) = Just x
readOsId _ = Nothing
