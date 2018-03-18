module WordCount 
    ( wordCount
    ) where

import Data.List

import Lib

wordCount :: String -> String
wordCount inputStr = 
    showNLines ++ showNEmptyLines ++ showNWords ++ showNUniqueWords
    ++ showNWordsFollowed ++ showLongestLine
    where
        showNLines = 
            "Number of lines: " ++ showLength inputLines ++ "\n"
        showNEmptyLines = 
            "Number of empty lines: " 
            ++ showLength (filter null inputLines) ++ "\n"
        showNWords = 
            "Number of words: " ++ showLength inputWords ++ "\n"
        showNUniqueWords = 
            "Number of unique words: " ++ showLength (nub inputWords) ++ "\n"
        showNWordsFollowed = 
            "Number of words followed by themselves: "
            ++ showLength (filter (uncurry (==)) (adjacents inputWords))
            ++ "\n"
        showLongestLine = 
            "Length of the longest line: " 
            ++ showLength (longestText inputLines) ++ "\n"
        inputLines = lines inputStr
        inputWords = words inputStr
        showLength x = show $ length x