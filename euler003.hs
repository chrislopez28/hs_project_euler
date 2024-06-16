import Helpers (isPrime, primeFactors)

findLargestPrime :: Integral t => t -> t
findLargestPrime n = findLargestPrime' n istart 
    where 
    findLargestPrime' n i = if n `mod` i == 0 && isPrime i then i else findLargestPrime' n (i-1)
    istart = round (sqrt (fromIntegral n))

example :: Integer = findLargestPrime 13195
euler003 :: Integer = findLargestPrime 600851475143

main :: IO()
main = print euler003