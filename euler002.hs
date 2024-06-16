-- A fibonacci implementation that is slow due to branching
fibonacciBasic :: Int -> Int
fibonacciBasic n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacciBasic (n-2) + fibonacciBasic (n-1)

-- A faster fibonacci implementation 
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fib' (n-2) 0 1
    where
    fib' 0 x y = x+y 
    fib' i x y = fib' (i-1) y (x+y)

-- For a function f that maps [0..] to t1, returns the first i such that f i > n
findFirst :: (Ord t1, Num t2) => t1 -> (t2 -> t1) -> t2
findFirst n f = findFirst' n f 0
    where 
    findFirst' n f i = if f i > n then i else findFirst' n f (i + 1)

nmax :: Int = findFirst 4000000 fibonacci 

euler002 :: Int
euler002 = sum [fibonacci x | x <- [1..nmax], even (fibonacci x)]

main :: IO()
main = print euler002