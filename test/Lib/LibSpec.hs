module Lib.LibSpec where

import SpecHelper
    
spec :: Spec
spec =
    describe "lib tests" $ do
        describe "halveEvens" $ do
            context "with []" $
                it "should be []" $
                    halveEvens [] `shouldBe` []

            context "with [1,2,3,4,5]" $
               it "should be [1,2]" $
                    halveEvens [1,2,3,4,5] `shouldBe` [1,2]
    
            context "with [6,6,6,3,3,3,2,2,2]" $
                it "should be [3,3,3,1,1,1]" $
                    halveEvens [6,6,6,3,3,3,2,2,2] `shouldBe` [3,3,3,1,1,1]
                            
        describe "safeString" $ do
            context "with []" $
                it "should be []" $
                    safeString [] `shouldBe` []

            context "with Hello World!" $
                it "should be Hello World!" $
                    safeString "Hello World!" `shouldBe` "Hello World!"
    
            context "with That’s your line:\n" $
                it "should be That_s your line:_" $
                    safeString "That’s your line:\n" 
                    `shouldBe` "That_s your line:_"

            context "with 🙋.o(“Me Me Me”)" $
                it "should be _.o(_Me Me Me_)" $
                    safeString "🙋.o(“Me Me Me”)" `shouldBe` "_.o(_Me Me Me_)"

        describe "holes" $ do
            context "with []" $
                it "should be []" $
                    holes "" `shouldBe` []
            
            context "with 'Hello'" $
                it "should be [ello, Hllo, Helo, Helo, Hell]" $
                    holes "Hello" `shouldBe` 
                    ["ello", "Hllo", "Helo", "Helo", "Hell"]

        describe "longestText" $ do
            context "with [True, False]" $
                it "should be [False]" $
                    longestText [True,False] `shouldBe` False
                        
            context "with [2,3,16,32]" $
                it "should be 32" $
                    longestText [2,3,16,32] `shouldBe` 32

            context "with (words 'Hello World')" $
                it "should be 'World'" $
                    longestText (words "Hello World") `shouldBe` "World"

            context "with (words 'Olá mundo')" $
                it "should be 'Olá'" $
                    longestText (words "Olá mundo") `shouldBe` "Olá"
        
        describe "adjacents" $ do
            context "with ''" $
                it "should be []" $
                    adjacents "" `shouldBe` []
            
            context "with [True]" $
                it "should be []" $
                    adjacents [True] `shouldBe` []

            context "with 'Hello'" $
                it "should be [('H','e'),('e','l'),('l','l'),('l','o')]" $
                    adjacents "Hello"
                    `shouldBe` [('H','e'),('e','l'),('l','l'),('l','o')]
 
        describe "commas" $ do
            context "with []" $
                it "should be ''" $
                    commas [] `shouldBe` ""

            context "with ['Hello']" $
                it "should be 'Hello'" $
                   commas ["Hello"] `shouldBe` "Hello"

            context "with ['Hello','World']" $
                it "should be 'Hello, World'" $
                    commas ["Hello","World"] `shouldBe` "Hello, World"

            context "with ['Hello','','World']" $
                it "should be 'Hello, , World'" $
                    commas ["Hello","","World"] `shouldBe` "Hello, , World"

            context "with ['Hello','new','World']" $
                it "should be 'Hello, new, World'" $
                    commas ["Hello","new","World"] `shouldBe` "Hello, new, World"

        describe "addPolynomials" $ do
            context "with [[]]" $
                it "should be []" $
                    addPolynomials [[]] `shouldBe` []

            context "with [[0, 1], [1, 1]]" $
                it "should be [1,2]" $
                    addPolynomials [[0, 1], [1, 1]] `shouldBe` [1,2]
 
            context "with [[0, 1, 5], [7, 0, 0], [-2, -1, 5]]" $
                it "should be [5, 0, 10]" $
                    addPolynomials
                        [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] `shouldBe` 
                        [5,0,10]

        describe "sumNumbers" $ do
            context "with ''" $
                it "should be 0" $
                    sumNumbers "" `shouldBe` 0

            context "with 'Hello world!'" $
                it "should be 0" $
                    sumNumbers "Hello world!" `shouldBe` 0                    
            
            context "with 'a1bc222d3f44'" $
                it "should be 270" $
                    sumNumbers "a1bc222d3f44" `shouldBe` 270

            context "with 'words0are1234separated12by3integers45678'" $
                it "should be 46927" $
                    sumNumbers 
                    "words0are1234separated12by3integers45678" `shouldBe`
                    46927

            context "with '000a'" $
                it "should be 0" $
                    sumNumbers "000a" `shouldBe` 0

            context "with '0.00a'" $
                it "should be 0" $
                    sumNumbers "0.00a" `shouldBe` 0     

main :: IO()
main = hspec spec