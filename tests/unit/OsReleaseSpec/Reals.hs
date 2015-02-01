module OsReleaseSpec.Reals
    ( Reals (..)
    , output
    , input
    )
where

import System.OsRelease
import Data.Monoid

data TestVal = Var String
             | Val String
             | QVal String

toInput :: TestVal -> String
toInput (Var  x) = x
toInput (Val  x) = x
toInput (QVal x) = "\"" <> x <> "\""

val :: TestVal -> String
val (Var  _) = error "Not a val"
val (Val  x) = x
val (QVal x) = x

data Reals = Gentoo

testVals :: Reals -> [(TestVal, TestVal)]
testVals Gentoo =
    [ (Var "NAME"           , Val "Gentoo")
    , (Var "ID"             , Val "gentoo")
    , (Var "PRETTY_NAME"    , QVal "Gentoo/Linux")
    , (Var "ANSI_COLOR"     , QVal "1;32")
    , (Var "HOME_URL"       , QVal "http://www.gentoo.org/")
    , (Var "SUPPORT_URL"    , QVal "http://www.gentoo.org/main/en/support.xml")
    , (Var "BUG_REPORT_URL" , QVal "https://bugs.gentoo.org/")
    ]

input :: Reals -> String
input x = unlines [toInput x' <> "=" <> toInput y | (x',y) <- testVals x]

output :: Reals -> [(OsReleaseKey, OsReleaseValue)]
output x = [ (OsReleaseKey x', OsReleaseValue $ val y) | (Var x', y) <- testVals x]
