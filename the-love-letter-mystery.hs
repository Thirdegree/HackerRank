import Data.Char
import Control.Monad

split :: [a] -> ([a],[a])
split a = (take ((length a)`div`2) a, reverse $ drop ((length a)`div`2) a)

pairs :: ([a],[b]) -> [(a,b)]
pairs (a,b) = zipWith (\x y -> (x, y)) a b

changeCount :: (Char, Char) -> Int
changeCount (a,b) = abs (ord a - ord b)

totalChanges :: [(Char, Char)] -> Int
totalChanges a = sum $ map changeCount a

main :: IO()
main = do
    times <- readLn :: IO(Int)
    cases <- replicateM times getLine
    let out = map (totalChanges . pairs . split) cases
    putStrLn . unlines $ map show out
