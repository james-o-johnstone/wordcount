module Lib
    ( halveEvens
    , safeString
    , holes
    , longestText
    , adjacents
    , commas
    , addPolynomials
    , sumNumbers
    ) where

import Data.Char
import Data.List
import Data.Function (on)

-- If a number is even, half it. 
-- If a number is odd, remove it.
halveEvens :: [Integer] -> [Integer]
halveEvens = map half . filter isEven
    where
        half x = x `div` 2
        isEven x
            | x `mod` 2 == 0 = True
            | otherwise = False

-- Replace every character that is a control
-- character, or that is not an ASCII character
-- with an underscore
safeString :: String -> String
safeString = map replace
    where 
        replace char
            | isControl char = '_'
            | not (isAscii char) = '_'
            | otherwise = char 

-- Given a list, return a list of lists where
-- each element contains the first list with
-- an element removed. The elements should be 
-- removed in order.
-- E.g. [1,2,3] = [[2,3],[1,3],[1,2]]
-- holes :: Eq a => [a] -> [[a]]
holes :: [a] -> [[a]]
holes list = map (\x->removeElement (zip list [0..]) x) (zip list [0..])
    where
        removeElement list x = map fst (filter(\y -> snd y /= snd x) list)

-- Given non-empty list, find the entry for which
-- shows the longest text. If multiple
-- entries are the same, prefer the last entry
longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` comparison)
    where
        comparison x = length(show x)

-- pair each element with the next one in the list
adjacents :: [a] -> [(a,a)]
adjacents list = zip list (tail list)

-- add commas between strings
commas :: [String] -> String
commas [] = ""
commas list = concatMap addComma (init list) ++ last list
    where
        addComma x = x ++ ", "

-- given coefficients to polynomial eqns as 
-- lists of the same length, output the coeffs
-- for the sum of these equations
-- e.g. [[0,1],[1,1]] = [0+1],[1+1] = [1,2]
addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1 (zipWith (+))

-- Output the sum of all natural numbers
-- in the given string
-- e.g. a1bc222d3f44 = 1 + 222 + 3 + 44 = 270
sumNumbers :: String -> Integer
sumNumbers inputStr = 
    sum $ map toChar (filter (all isDigit) (groupBy hasDigits inputStr))
    where
        hasDigits x y = isDigit x && isDigit y
        toChar x = read x :: Integer
