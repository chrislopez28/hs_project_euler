import Data.List (sortBy)

products :: [Int]
products = sortBy (flip compare) [x * y | x <- [100..999], y <- [100..999]]

isPalindrome :: String -> Bool 
isPalindrome cs = cs == reverse cs

findPalindrome :: [Int] -> Int
findPalindrome [] = 0
findPalindrome (y:ys) = if isPalindrome (show y) then y else findPalindrome ys

euler004 :: Int = findPalindrome products

main :: IO()
main = print euler004