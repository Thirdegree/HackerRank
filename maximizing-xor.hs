import Data.Bits

main = do
    inp1 <- getLine
    inp2 <- getLine
    let 
        l = read inp1::Int
        r = read inp2::Int
        m = maximum [x`xor`y | x<-[l..r], y<-[l..r], x>=y]
    putStrLn $ show m




