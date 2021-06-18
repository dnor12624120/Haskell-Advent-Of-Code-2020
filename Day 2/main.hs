import System.IO  
import Control.Monad

data Password = Password {
    minP  :: Int,
    maxP  :: Int,
    charP :: Char,
    passP :: String
} deriving (Eq, Show)

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

-- a password is valid if text contains [min, max] occurences of char
isValid1 :: Password -> Bool
isValid1 pass = let min = minP pass
                    max = maxP pass
                    char = charP pass
                    text = passP pass
                in between min max (count char text)
                where between x y z= (z >= x) && (z <= y)

-- a password is valid if only one of min or max contains char
isValid2 :: Password -> Bool
isValid2 pass = let min = minP pass
                    max = maxP pass
                    char = charP pass
                    text = passP pass
                in (text !! (min - 1) == char) /= (text !! (max - 1) == char)
                

-- ranges use the format n1-n2, we need to get n1 and n2
splitRange :: String -> (String, String)
splitRange str = (takeWhile (/= '-') str, tail $ dropWhile (/= '-') str)

-- input is of the form 
stringToPassword :: [String] -> Password
stringToPassword input = let rangePart = input !! 0
                             charPart  = input !! 1
                             textPart  = input !! 2
                             minNum = read $ fst $ splitRange rangePart
                             maxNum = read $ snd $ splitRange rangePart
                             char = head charPart
                         in Password minNum maxNum char textPart

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    list <- hGetContents handle
    print $ count True (map (isValid2 . stringToPassword) $ map words (lines list))
    hClose handle