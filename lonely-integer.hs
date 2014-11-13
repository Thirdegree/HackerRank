{-# LANGUAGE NoMonomorphismRestriction #-}


import Data.List (sortBy, sort, group)

find :: [Int] -> [Int] 
find = head . (sortBy (\x y -> (length x) `compare` (length y))) . group . sort

parseOut :: [Int] -> String
parseOut = unlines . map show

parseIn :: [String] -> [Int]
parseIn xs = map (\x -> read x) xs

main :: IO()
main = getLine >> getLine >>= putStrLn . parseOut . find . parseIn . words