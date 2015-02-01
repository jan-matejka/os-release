module System.OsRelease
    ( parseOs
    , readOs
    )
where

import Text.ParserCombinators.Parsec
import Data.Map.Lazy
import Control.Applicative ((<$>))

type OsReleaseRaw = Map String String

osReleaseParser :: Parser [(String, String)]
osReleaseParser = sepEndBy line eol
  where
    line :: Parser (String, String)
    line  = do
        var' <- var
        _ <- char '='
        val' <- val
        return (var', val')

    var :: Parser String
    var   = many1 alphaNum

    val :: Parser String
    val   = qVal <|> nqVal

    qVal :: Parser String
    qVal  = do
        quote <- oneOf "'\""
        value <- nqVal
        _ <- char quote
        return value

    nqVal :: Parser String
    nqVal = many1 alphaNum

    eol :: Parser Char
    eol   = char '\n'

readOs :: IO (Either ParseError OsReleaseRaw)
readOs = do
    xs <- readFile "/etc/os-release"
    return $ parseOs xs

parseOs :: String -> Either ParseError OsReleaseRaw
parseOs xs = fromList <$> parse osReleaseParser "os-release" xs
