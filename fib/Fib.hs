import Control.Monad (forM_)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: [Integer]
fib' = 0 : 1 : zipWith (+) fib' (tail fib')

main :: IO ()
main = forM_ (take 100 fib') (putStrLn . show)
