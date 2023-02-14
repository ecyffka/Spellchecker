module WordModelTest(wordModelTests) where

import Test.HUnit
import qualified Data.Map as Map
import WordModel

-- Test suite for WordModel
testCorpus = ["we","we","we","we","all","all","live","in","in","in","in","in","a","a","a","a","a","a","a","a","yellow","yellow","submarine"]
testDict = Map.fromList [("we", 4), ("all", 2), ("live", 1), ("in", 5), ("a", 8), ("yellow", 2), ("submarine", 1)]

-- Manual word model for tests
wm :: WordModel.WordModel
wm = WordModel testDict 23 
-- Trained word model to test the 'train' function
wm2 :: WordModel.WordModel
wm2 = train (EmptyModel ()) testCorpus

-- known
testKnownTrue = TestCase (assertEqual "for (known wm 'all')" True (known wm "all"))
testKnownFalse = TestCase (assertEqual "for (known wm 'blue')" False (known wm "blue"))
testKnownEmpty = TestCase (assertEqual "for (known (EmptyModel ()) 'all')" False (known (EmptyModel ()) "all"))
-- wordProb
testWordProb1 = TestCase (assertEqual "for (wordProb wm 'yellow')" 0.1 (wordProb wm "yellow"))
testWordProb2 = TestCase (assertEqual "for (wordProb wm 'in')" 0.2 (wordProb wm "in"))
testWordUnseen = TestCase (assertEqual "for (wordProb wm 'blue')" (1 / 30) (wordProb wm "blue"))
testWordEmpty = TestCase (assertEqual "for (wordProb (EmptyModel ()) 'yellow')" 0 (wordProb (EmptyModel ()) "yellow"))
-- train
testTrain1 = TestCase (assertEqual "for (wordProb wm2 'a')" (wordProb wm "a") (wordProb wm2 "a"))
testTrainVocabSize = TestCase (assertEqual "for wm2 numTokens" 23 n) where (WordModel map n) = wm2
-- incrementCount
testIncrementCount1 = TestCase (assertEqual "for (wordProb wm 'live') after increment" (3 / 31) (wordProb wm3 "live")) where wm3 = incrementCount wm "live"
testIncrementCount2 = TestCase (assertEqual "for numTokens after increment" 24 n) where (WordModel map n) = incrementCount wm "in"

wordModelTests = TestList[
    TestLabel "test1" testKnownTrue, 
    TestLabel "test2" testKnownFalse,
    TestLabel "test3" testKnownEmpty,
    TestLabel "test4" testWordProb1,
    TestLabel "test5" testWordProb2,
    TestLabel "test6" testWordUnseen,
    TestLabel "test7" testWordEmpty,
    TestLabel "test8" testTrain1,
    TestLabel "test9" testTrainVocabSize,
    TestLabel "test10" testIncrementCount1,
    TestLabel "test11" testIncrementCount2
    ]