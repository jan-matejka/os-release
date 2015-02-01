{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module System.OsRelease
    ( parseOs
    , readOs
    )
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Data.Map.Lazy
import Data.Functor.Identity
import Data.String
import Control.Applicative ((<$>))

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
            value <- nqVal
            _ <- char quote
            return value

        nqVal :: Parser String
        nqVal = many1 alphaNum

instance Parsable OsReleaseLine where
    parser = do
        var <- parser :: ParsecT String () Identity OsReleaseKey
        _ <- char '='
        val <- parser :: ParsecT String () Identity OsReleaseValue
        return (var, val)

instance Parsable OsRelease where
    parser = fromList <$> (sepEndBy (parser :: ParsecT String () Identity OsReleaseLine) $ char '\n')


readOs :: IO (Either ParseError OsRelease)
readOs = do
    xs <- readFile "/etc/os-release"
    return $ parseOs xs

parseOs :: String -> Either ParseError OsRelease
parseOs xs = parse (parser :: ParsecT String () Identity OsRelease) "os-release" xs
