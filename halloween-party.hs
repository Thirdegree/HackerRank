import Control.Monad

cuts :: Integral t => t -> (t,t)
cuts a = case a`mod`2 of 
                    0 -> (a`div`2, a`div`2)
                    1 -> (a`div`2, a`div`2 + 1)

peices :: Num a => (a,a) -> a
peices (a,b) = a*b

format :: (Integral b, Show b) => [b] -> [String]
format a = map (show . peices . cuts) a

main :: IO()
main = (readLn :: IO(Int)) >>= (\x -> replicateM x getLine) >>= putStrLn . unlines . format . (map read)
