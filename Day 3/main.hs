import System.IO  
import Control.Monad

countTrees :: [String] -> Int -> Int -> Int -> Int
countTrees [] _ _ _ = 0
countTrees (currentLine:rest) offset width slope = 
    case currentLine !! (offset `mod` width) of
        '.' -> countTrees rest (offset + slope) width slope
        otherwise -> 1 + countTrees rest (offset + slope) width slope

countTreesBy2 :: [String] -> Int -> Int -> Int -> Int
countTreesBy2 (currentLine:_:rest) offset width slope = 
    case currentLine !! (offset `mod` width) of
        '.' -> countTreesBy2 rest (offset + slope) width slope
        otherwise -> 1 + countTreesBy2 rest (offset + slope) width slope
countTreesBy2 (currentLine:[]) offset width slope = 
    case currentLine !! (offset `mod` width) of
        '.' -> 0
        otherwise -> 1
countTreesBy2 _ _ _ _ = 0

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    list <- hGetContents handle
    print $ countTreesBy2 (lines list) 0 (length $ head $ lines list) 1
    hClose handle