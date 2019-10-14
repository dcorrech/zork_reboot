module NaturalLanguageParserTests where

import NaturalLanguageParser
import NaturalLanguageLexer
import TestUtils
import Test.HUnit

verbsInTokenListTest1 = TestCase (assertEqual "for (verbsInTokenList []),"
                                      []
                                      (verbsInTokenList []))

verbsInTokenListTest2 = TestCase (assertEqual "for (verbsInTokenList [dummyTokenNoun]),"
                                      []
                                      (verbsInTokenList [dummyTokenNoun]))

verbsInTokenListTest3 = TestCase (assertEqual "for (verbsInTokenList [dummyTokenVerb]),"
                                      [dummyTokenVerb]
                                      (verbsInTokenList [dummyTokenVerb]))

verbsInTokenListTest4 = TestCase (assertEqual "for (verbsInTokenList dummyListOfTokens),"
                                      verbsInDummyList
                                      (verbsInTokenList dummyListOfTokens))

nounsInTokenListTest1 = TestCase (assertEqual "for (nounsInTokenList []),"
                                      []
                                      (nounsInTokenList []))

nounsInTokenListTest2 = TestCase (assertEqual "for (nounsInTokenList [dummyTokenNoun]),"
                                      [dummyTokenNoun]
                                      (nounsInTokenList [dummyTokenNoun]))

nounsInTokenListTest3 = TestCase (assertEqual "for (nounsInTokenList [dummyTokenVerb]),"
                                      []
                                      (nounsInTokenList [dummyTokenVerb]))

nounsInTokenListTest4 = TestCase (assertEqual "for (nounsInTokenList dummyListOfTokens),"
                                      nounsInDummyList
                                      (nounsInTokenList dummyListOfTokens))

generateSentencesTest1 = TestCase (assertEqual "for (generateSentences []),"
                                      []
                                      (generateSentences []))

generateSentencesTest2 = TestCase (assertEqual "for (generateSentences [verbsInDummyList]),"
                                      [(Word dummyTokenVerb)]
                                      (generateSentences [[dummyTokenVerb]]))

generateSentencesTest3 = TestCase (assertEqual "for (generateSentences [verbsInDummyList]),"
                                      [Word (TokenVerb "verb1" ["verb1","synonym"]),
                                       Word (TokenVerb "verb2" ["verb2","synonym"]),
                                       Word (TokenVerb "verb3" ["verb3","synonym"])]
                                      (generateSentences [verbsInDummyList]))

generateSentencesTest4 = TestCase (assertEqual "for (generateSentences [[dummyTokenVerb]. [dummyTokenNoun]]),"
                                      [SimpleSentence (TokenVerb "verb1" ["verb1","synonym"]) (TokenNoun "noun1" ["noun1","synonym"])]
                                      (generateSentences [[dummyTokenVerb], [dummyTokenNoun]]))

-- verb/noun in wrong order
generateSentencesTest5 = TestCase (assertEqual "for (generateSentences [[dummyTokenVerb]. [dummyTokenNoun]]),"
                                      allPossibleSimpleSentencesFromVerbNounDummyLists
                                      (generateSentences [verbsInDummyList, nounsInDummyList]))

parserTest1 = TestCase (assertEqual "for (parseSentence [])),"
                                      []
                                      (parseSentence []))

parserTest2 = TestCase (assertEqual "for (parseSentence [dummyTokenVerbMatch]),"
                                      [dummyWord]
                                      (parseSentence [dummyTokenVerbMatch]))

parserTest3 = TestCase (assertEqual "for (parseSentence [dummyTokenNounMatch]),"
                                      []
                                      (parseSentence [dummyTokenNounMatch]))

parserTest4 = TestCase (assertEqual "for (parseSentence [dummyTokenVerbMatch, dummyTokenVerbMatch]),"
                                      []
                                      (parseSentence [dummyTokenVerbMatch, dummyTokenVerbMatch]))

parserTest5 = TestCase (assertEqual "for (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch]),"
                                      []
                                      (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch]))

parserTest6 = TestCase (assertEqual "for (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch, dummyTokenNounMatch]),"
                                      []
                                      (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch, dummyTokenNounMatch]))

parserTest7 = TestCase (assertEqual "for (parseSentence [dummyTokenVerbMatch, dummyTokenVerbMatch, dummyTokenNounMatch]),"
                                      []
                                      (parseSentence [dummyTokenVerbMatch, dummyTokenVerbMatch, dummyTokenNounMatch]))

parserTest8 = TestCase (assertEqual "for (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch, dummyTokenNounMatch]),"
                                      [dummySimpleSentence]
                                      (parseSentence [dummyTokenVerbMatch, dummyTokenNounMatch]))

parserTest9 = TestCase (assertEqual "for (parseSentence [dummyTokenNounMatch, dummyTokenVerbMatch, dummyTokenNounMatch]),"
                                      []
                                      (parseSentence [dummyTokenVerbMatch, dummyTokenNounMatch, dummyTokenVerbMatch, dummyTokenNounMatch]))

naturalLanguageParserTests = TestList [TestLabel "verbsInTokenListTest1" verbsInTokenListTest1,
                       TestLabel "verbsInTokenListTest2" verbsInTokenListTest2,
                       TestLabel "verbsInTokenListTest3" verbsInTokenListTest3,
                       TestLabel "verbsInTokenListTest4" verbsInTokenListTest4,
                       TestLabel "nounsInTokenListTest1" nounsInTokenListTest1,
                       TestLabel "nounsInTokenListTest2" nounsInTokenListTest2,
                       TestLabel "nounsInTokenListTest3" nounsInTokenListTest3,
                       TestLabel "nounsInTokenListTest4" nounsInTokenListTest4,
                       TestLabel "generateSentencesTest1" generateSentencesTest1,
                       TestLabel "generateSentencesTest2" generateSentencesTest2,
                       TestLabel "generateSentencesTest3" generateSentencesTest3,
                       TestLabel "generateSentencesTest4" generateSentencesTest4,
                       TestLabel "generateSentencesTest5" generateSentencesTest5,
                       TestLabel "parserTest1" parserTest1,
                       TestLabel "parserTest2" parserTest2,
                       TestLabel "parserTest3" parserTest3,
                       TestLabel "parserTest4" parserTest4,
                       TestLabel "parserTest5" parserTest5,
                       TestLabel "parserTest6" parserTest6,
                       TestLabel "parserTest7" parserTest7,
                       TestLabel "parserTest8" parserTest8,
                       TestLabel "parserTest9" parserTest9]