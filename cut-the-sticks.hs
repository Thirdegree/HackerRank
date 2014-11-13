--Read input from STDIN. Print output to STDOUT
import Control.Monad
import Data.List

parseIn :: [String] -> [Int]
parseIn xs = map (\x -> read x :: Int) xs

stickCut :: (Num a, Ord a) => [a] -> [Int]
stickCut xs = go $ sort xs
    where
        go [] = []
        go ns = length ns : go (filterStick $ mapStick ns)

        filterStick = filter (/=0)
        mapStick ys = map (subtract (head ys)) ys

parseOut :: [Int] -> String
parseOut = unlines . map show

main :: IO()
main = getLine >> getLine >>= putStrLn . parseOut . stickCut . parseIn . words
