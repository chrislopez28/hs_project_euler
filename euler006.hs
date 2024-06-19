euler006 :: Integer
euler006 = sum [1..100]^2 - sum [x^2 | x <- [1..100]]

main :: IO()
main = print euler006