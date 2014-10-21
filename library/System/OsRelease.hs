module System.OsRelease
    ( detect
    , OsRelease (..)
    , detect'
    )
where

data OsRelease = OpenSUSE
    deriving (Eq, Show)

detect :: IO (Maybe OsRelease)
detect = do
    osr <- readFile "/etc/os-release"
    return . detect' $ lines osr

detect' :: [String] -> Maybe OsRelease
detect' [] = Nothing
detect' ("NAME=openSUSE":_) = Just OpenSUSE
detect' (_:xs) = detect' xs
