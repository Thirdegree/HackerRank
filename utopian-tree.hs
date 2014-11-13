import Control.Monad

utopean x = go 1 x 0
    where 
        go a 0 c = a
        go a b c = case c of 
                            1 -> go (a+1) (b-1) 0
                            0 -> go (a*2) (b-1) 1
        
main = do
    stimes <- getLine
    let times = read stimes
    scases <- replicateM times getLine
    let 
        cases = map read scases
        processed = map utopean cases
        processedTwo = map show processed
    putStrLn $ unlines processedTwo
    
