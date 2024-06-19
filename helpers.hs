module Helpers where

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0] 

findLargestPrime :: Integral t => t -> t
findLargestPrime n = findLargestPrime' n istart 
    where 
    findLargestPrime' n i = if n `mod` i == 0 && isPrime i then i else findLargestPrime' n (i-1)
    istart = round (sqrt (fromIntegral n))

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1,n]

primes :: Integral a => a -> [a]
primes n = [x | x <- [2..n], isPrime x]

primeFactors :: Integral a => a -> [a]
primeFactors n = [p | p <- primes n, n `mod` p == 0]
    where upperBound = n `div` 2

primeDecomposition :: Integral a => a -> [a]
primeDecomposition n = primeDecomposition' n (primeFactors n) []
    where
    primeDecomposition' n [] ps = []
    primeDecomposition' n (f:fs) ps = if n `mod` f == 0 then f : primeDecomposition' (n `div` f) (f:fs) (f:ps) else primeDecomposition' n fs ps
