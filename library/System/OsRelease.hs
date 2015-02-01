{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module System.OsRelease
    ( parseOs
    , readOs
    , readOs'
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
import Data.Either
import Control.Applicative ((<$>))
import Control.Monad
import qualified Control.Exception as E

type OsReleaseLine  = (OsReleaseKey, OsReleaseValue)
newtype OsReleaseKey   = OsReleaseKey String
    deriving (Ord, Eq, IsString, Show)

newtype OsReleaseValue = OsReleaseValue String
    deriving (IsString, Eq, Show)

type OsRelease      = Map OsReleaseKey OsReleaseValue

class Parsable a where
    parser :: ParsecT String () Identity a

instance Parsable OsReleaseKey where
    parser = OsReleaseKey <$> many1 (alphaNum <|> char '_')

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


readOs :: IO (Either OsReleaseError OsRelease)
readOs = do
    xs <- readOs' ["/etc/os-release", "/usr/lib/os-release"]
    case xs of
        Left  e -> return $ Left e
        Right x -> return . h $ parseOs x
  where
    h (Left  e) = Left $ OsReleaseParseError e
    h (Right x) = Right x

data OsReleaseError = OsReleaseError String
                    | OsReleaseParseError ParseError
    deriving (Show)

readOs' :: [FilePath] -> IO (Either OsReleaseError String)
readOs' fs = do
    xs <- mapM (E.try . readFile) fs :: IO [Either E.IOException String]
    return . h $ rights xs
  where
    h [] = Left . OsReleaseError $ "Neither of " <> unwords fs <> " could be read"
    h (x:_) = Right x

parseOs :: String -> Either ParseError OsRelease
parseOs xs = parse (parser :: ParsecT String () Identity OsRelease) "os-release" xs
