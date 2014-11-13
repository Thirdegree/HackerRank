import Data.List
import Control.Monad

findDel x = (length x) - go x
    where
        go x = length $ map head (group x)
    
main = do
    times <- readLn :: IO(Int)
    cases <- replicateM times getLine
    let 
        dels = map findDel cases
    putStrLn . unlines $ map show dels
