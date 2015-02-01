module OsReleaseSpec (spec) where

import           Test.Hspec
import           System.OsRelease
import           Data.Map.Lazy

spec :: Spec
spec = do
    describe "parseOs" $ do
        it "parses simple value"                   $ fooBar "foo=bar"
        it "parses simple value with trailing NL"  $ fooBar "foo=bar\n"
        it "parses single quoted value"            $ fooBar "foo='bar'"
        it "parses double quoted value"            $ fooBar "foo=\"bar\""
        it "parses multiple values" $ parseCase
            "foo=bar\nqux=quux"
            [("foo", "bar"), ("qux", "quux")]

        it "breaks on misquoting 1" $ errCase "foo=\"bar'"
        it "breaks on misquoting 2" $ errCase "foo='bar\""

    where
        fooBar x = parseCase x [("foo", "bar")]

        parseCase x y =
            case parseOs x of
                Left  e -> expectationFailure $ show e
                Right z -> z `shouldBe` (fromList y)

        errCase x =
            case parseOs x of
                Left  _ -> "1" `shouldBe` "1"
                Right z -> expectationFailure (show z)
