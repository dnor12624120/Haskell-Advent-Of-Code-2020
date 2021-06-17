import System.IO  
import Control.Monad

-- find 2 elements of a list that sum up to 2020
-- since the requirements only ask for one pair, we assume head is safe to use here
findProductOf2 :: [Int] -> Int
findProductOf2 list = head [x * y | x <- list, y <- list, x + y == 2020]

-- find 3 elements of a list that sum up to 2020
-- the solution is getting repetitive. is there a way to generalize over  
-- the amount of variables we're pulling from the list?
findProductOf3 :: [Int] -> Int
findProductOf3 list = head [x * y * z | x <- list, y <- list, z <- list, x + y + z == 2020]



main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    list <- hGetContents handle
    let parsedList = map (read :: String -> Int) (words list) in
        print $ findProductOf3 parsedList 
    hClose handle