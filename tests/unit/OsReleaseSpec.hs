{-# LANGUAGE OverloadedStrings #-}
module OsReleaseSpec (spec) where

import           Test.Hspec
import           System.OsRelease
import           Data.Map.Lazy
import           Data.Monoid
import           OsReleaseSpec.Reals
import           System.IO.Temp
import           System.IO

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

    describe "readOs'" $ do
        it "reads 1st" $ do
            withSystemTempFile "1st.txt" $ \f1 hf1 -> do
                withSystemTempFile "2nd.txt" $ \f2 hf2 -> do
                    hPutStr hf1 "1"
                    hPutStr hf2 "2"
                    mapM_ hClose [hf1, hf2]

                    xs <- readOs' [f1, f2]
                    case xs of
                        Right x -> x `shouldBe` "1"
                        Left e -> expectationFailure $ show e

        it "reads 2nd" $ do
            withSystemTempFile "a-file.txt" $ \f h -> do
                hPutStr h "1"
                hClose h

                xs <- readOs' ["/oh-well-let's-hope-no-one-makes-this-file", f]
                case xs of
                    Right x -> x `shouldBe` "1"
                    Left e -> expectationFailure $ show e

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
