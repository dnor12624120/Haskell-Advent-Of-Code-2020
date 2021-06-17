import System.IO  
import Control.Monad

findProduct :: [Int] -> Int
findProduct list = head [x * y | x <- list, y <- list, x + y == 2020]

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    list <- hGetContents handle
    let parsedList = map (read :: String -> Int) (words list) in
        print $ findProduct parsedList 
    hClose handle