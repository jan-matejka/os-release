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

data Reals = Gentoo | OpenSUSEFactory | Fedora20 | Arch

testVals :: Reals -> [(TestVal, TestVal)]
testVals Gentoo =
    [ (Var "NAME"           , Val  "Gentoo")
    , (Var "ID"             , Val  "gentoo")
    , (Var "PRETTY_NAME"    , QVal "Gentoo/Linux")
    , (Var "ANSI_COLOR"     , QVal "1;32")
    , (Var "HOME_URL"       , QVal "http://www.gentoo.org/")
    , (Var "SUPPORT_URL"    , QVal "http://www.gentoo.org/main/en/support.xml")
    , (Var "BUG_REPORT_URL" , QVal "https://bugs.gentoo.org/")
    ]

testVals OpenSUSEFactory =
    [ (Var "NAME"           , Val  "openSUSE")
    , (Var "VERSION"        , QVal "20150115 (Tumbleweed)")
    , (Var "VERSION_ID"     , QVal "20150115")
    , (Var "PRETTY_NAME"    , QVal "openSUSE 20150115 (Tumbleweed) (x86_64)")
    , (Var "ID"             , Val  "opensuse")
    , (Var "ANSI_COLOR"     , QVal "0;32")
    , (Var "CPE_NAME"       , QVal "cpe:/o:opensuse:opensuse:20150115")
    , (Var "BUG_REPORT_URL" , QVal "https://bugs.opensuse.org")
    , (Var "HOME_URL"       , QVal "https://opensuse.org/")
    , (Var "ID_LIKE"        , QVal "suse")
    ]

testVals Fedora20 =
    [ (Var "NAME"                           , Val  "Fedora")
    , (Var "VERSION"                        , QVal "20 (Heisenbug)")
    , (Var "ID"                             , Val  "fedora")
    , (Var "VERSION_ID"                     , Val  "20")
    , (Var "PRETTY_NAME"                    , QVal "Fedora 20 (Heisenbug)")
    , (Var "ANSI_COLOR"                     , QVal "0;34")
    , (Var "CPE_NAME"                       , QVal "cpe:/o:fedoraproject:fedora:20")
    , (Var "HOME_URL"                       , QVal "https://fedoraproject.org/")
    , (Var "BUG_REPORT_URL"                 , QVal "https://bugzilla.redhat.com/")
    , (Var "REDHAT_BUGZILLA_PRODUCT"        , QVal "Fedora")
    , (Var "REDHAT_BUGZILLA_PRODUCT_VERSION", Val  "20")
    , (Var "REDHAT_SUPPORT_PRODUCT"         , QVal "Fedora")
    , (Var "REDHAT_SUPPORT_PRODUCT_VERSION" , Val  "20")
    ]

testVals Arch =
    [ (Var "NAME"           , QVal "Arch Linux")
    , (Var "ID"             , Val  "arch")
    , (Var "PRETTY_NAME"    , QVal "Arch Linux")
    , (Var "ANSI_COLOR"     , QVal "0;36")
    , (Var "HOME_URL"       , QVal "https://www.archlinux.org/")
    , (Var "SUPPORT_URL"    , QVal "https://bbs.archlinux.org/")
    , (Var "BUG_REPORT_URL" , QVal "https://bugs.archlinux.org/")
    ]

input :: Reals -> String
input x = unlines [toInput x' <> "=" <> toInput y | (x',y) <- testVals x]

output :: Reals -> [(OsReleaseKey, OsReleaseValue)]
output x = [ (OsReleaseKey x', OsReleaseValue $ val y) | (Var x', y) <- testVals x]
