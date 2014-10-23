module OsReleaseSpec (spec) where

import           Test.Hspec

import Prelude hiding (EQ, LT, GT)
import System.OsRelease
import System.Environment.XDG.BaseDir
import Data.Maybe
import System.FilePath.Posix

type OsId = String

spec :: Spec
spec = do
    describe "detect" $ do
        it "detects" $ do
            cg <- getUserConfigFile "os-release" ("acceptance" </> "detect-os")
            f  <- readFile cg
            let osid = fromJust . readOsId $ lines f

            x <- detect
            show x `shouldBe` osid

readOsId :: [String] -> Maybe OsId
readOsId (x:_) = Just x
readOsId _ = Nothing
