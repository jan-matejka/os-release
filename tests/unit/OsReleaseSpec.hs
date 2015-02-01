{-# LANGUAGE OverloadedStrings #-}
module OsReleaseSpec (spec) where

import           Test.Hspec
import           System.OsRelease
import           Data.Map.Lazy
import           Data.Monoid
import           OsReleaseSpec.Reals

spec :: Spec
spec = do
    describe "parseOs" $ do
        it "parses simple value"                   $ fooBar "foo=bar"
        it "parses simple value with trailing NL"  $ fooBar "foo=bar\n"
        it "parses single quoted value"            $ fooBar "foo='bar'"
        it "parses double quoted value"            $ fooBar "foo=\"bar\""

        it "parses _ var" $ parseCase "f_x=''" [("f_x", "")]

        it "parses quoted space" $ parseCase "f='a b'" [("f", "a b")]
        it "parses quoted -"     $ parseCase "f='a-b'" [("f", "a-b")]

        it "parses special \\" $ specialFooBar "foo='ba\\\\r'" "\\"
        it "parses special `"  $ specialFooBar "foo='ba\\`r'" "`"
        it "parses special '"  $ specialFooBar "foo='ba\\'r'" "'"
        it "parses special $"  $ specialFooBar "foo='ba\\$r'" "$"
        it "parses special \"" $ specialFooBar "foo=\"ba\\\"r\"" "\""

        it "parses multiple values" $ parseCase
            "foo=bar\nqux=quux"
            [("foo", "bar"), ("qux", "quux")]

        it "parses empty val noquotes" $ parseCase "foo=" [("foo", "")]
        it "parses empty val quote \"" $ parseCase "foo=\"\"" [("foo", "")]
        it "parses empty val quote '"  $ parseCase "foo=''" [("foo", "")]

        it "breaks on misquoting 1" $ errCase "foo=\"bar'"
        it "breaks on misquoting 2" $ errCase "foo='bar\""
        it "breaks on unquoted $"   $ errCase "foo='ba$r'"
        it "breaks on unquoted `"   $ errCase "foo='ba`r'"
        it "breaks on unquoted \""  $ errCase "foo=\"ba\"r\""
        it "breaks on unquoted '"   $ errCase "foo='ba'r'"
        it "breaks on unquoted \\"  $ errCase "foo='ba\\r'"
        it "breaks on unquoted val with space"  $ errCase "foo=ba r"
        it "breaks on unquoted val with ;"      $ errCase "foo=ba;r"
        it "breaks on unquoted val with \\"     $ errCase "foo=ba\\r"

        it "parses Gentoo os-release"           $ realCase Gentoo
        it "parses OpenSUSE Factory os-release" $ realCase OpenSUSEFactory

    where
        fooBar x = parseCase x [("foo", "bar")]

        specialFooBar :: String -> String -> Expectation
        specialFooBar x y = parseCase x [("foo", OsReleaseValue $ "ba" <> y <> "r")]

        realCase x = parseCase (input x) (output x)

        parseCase x y =
            case parseOs x of
                Left  e -> expectationFailure $ show e
                Right z -> z `shouldBe` (fromList y)

        errCase x =
            case parseOs x of
                Left  _ -> ("1"::String) `shouldBe` "1"
                Right z -> expectationFailure (show z)
