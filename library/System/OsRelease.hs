{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module System.OsRelease
    ( parseOs
    , readOs
    , OsReleaseValue (..)
    , OsReleaseKey (..)
    , OsReleaseLine
    , OsRelease
    )
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import Data.Map.Lazy hiding (foldl)
import Data.Functor.Identity
import Data.String
import Data.Monoid
import Control.Applicative ((<$>))
import Control.Monad

type OsReleaseLine  = (OsReleaseKey, OsReleaseValue)
newtype OsReleaseKey   = OsReleaseKey String
    deriving (Ord, Eq, IsString, Show)

newtype OsReleaseValue = OsReleaseValue String
    deriving (IsString, Eq, Show)

type OsRelease      = Map OsReleaseKey OsReleaseValue

class Parsable a where
    parser :: ParsecT String () Identity a

instance Parsable OsReleaseKey where
    parser = OsReleaseKey <$> many1 alphaNum

instance Parsable OsReleaseValue where
    parser = OsReleaseValue <$> (qVal <|> nqVal)
      where
        qVal :: Parser String
        qVal  = do
            quote <- oneOf "'\""
            value <- manyTill (qValInside quote) (char quote)
            noJunk
            return (foldl mappend "" value)

        noJunk = try . lookAhead $ eof <|> (void newline)

        qValInside :: Char -> ParsecT String () Identity String
        qValInside quote = (qSpecial quote) <|> (:[]) <$> (noneOf $ specials quote)

        qSpecial :: Char -> ParsecT String () Identity String
        qSpecial quote = (\x -> [x!!1]) <$>
            foldl1 (<|>) [try (string $ "\\" <> [x]) | x <- specials quote]

        specials :: Char -> [Char]
        specials quote =
            [ quote
            , '\\'
            , '$'
            , '`'
            ]

        nqVal :: Parser String
        nqVal = do
            x <- many alphaNum
            noJunk
            return x

instance Parsable OsReleaseLine where
    parser = do
        var <- parser :: ParsecT String () Identity OsReleaseKey
        _ <- char '='
        val <- parser :: ParsecT String () Identity OsReleaseValue
        return (var, val)

instance Parsable OsRelease where
    parser = fromList <$> (sepEndBy (parser :: ParsecT String () Identity OsReleaseLine) newline)


readOs :: IO (Either ParseError OsRelease)
readOs = do
    xs <- readFile "/etc/os-release"
    return $ parseOs xs

parseOs :: String -> Either ParseError OsRelease
parseOs xs = parse (parser :: ParsecT String () Identity OsRelease) "os-release" xs
