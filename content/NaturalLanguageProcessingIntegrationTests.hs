module NaturalLanguageProcessingIntegrationTests where

import NaturalLanguageParser
import NaturalLanguageLexer
import TestUtils
import Test.HUnit

------- Tests ---------
-- NOTE: Must download the Test.HUnit package to run.

integrationTest1 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match"])))

integrationTest2 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "no-match"])))

integrationTest3 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\"])),"
                                      [dummyWord]
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1"])))

integrationTest4 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\", \"verb1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "verb1"])))

integrationTest5 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["noun1"])))

integrationTest6 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"noun1\", \"verb1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["noun1", "verb1"])))

integrationTest7 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\", \"noun1\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "noun1"])))

integrationTest8 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\", \"noun1\", \"verb1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "noun1", "verb1"])))

integrationTest9 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\", \"noun1\", \"verb1\", \"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "noun1", "verb1", "noun1"])))

integrationTest10 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"verb1\"])),"
                                      [dummyWord]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "verb1"])))

integrationTest11 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\"])),"
                                      [dummyWord]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "no-match", "verb1"])))

integrationTest12 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"no-match\"])),"
                                      [dummyWord]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "verb1", "no-match"])))

integrationTest13 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"noun1\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "verb1", "noun1"])))

integrationTest14 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"noun1\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "verb1", "noun1"])))

integrationTest15 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"verb1\", \"noun1\", \"no-match\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "noun1", "no-match"])))

integrationTest16 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"no-match\", \"noun1\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "verb1", "no-match", "noun1"])))

integrationTest17 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\", \"no-match\", \"no-match\", \"noun1\"])),"
                                      [dummySimpleSentence]
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "no-match", "verb1", "no-match", "no-match", "no-match", "noun1"])))

integrationTest18 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\", \"no-match\", \"no-match\", \"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["verb1", "no-match", "verb1", "no-match", "no-match", "no-match", "noun1"])))

integrationTest19 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\", \"no-match\", \"no-match\", \"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "noun1", "verb1", "no-match", "no-match", "no-match", "noun1"])))

integrationTest20 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\", \"no-match\", \"no-match\", \"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "no-match", "verb1", "no-match", "no-match", "noun1", "noun1"])))

integrationTest21 = TestCase (assertEqual "for (parseSentence (lexInput dummyPossibleTokens [\"no-match\", \"no-match\", \"verb1\", \"no-match\", \"no-match\", \"noun1\"])),"
                                      []
                                      (parseSentence (lexInput dummyPossibleTokens ["no-match", "no-match", "verb1", "noun1", "no-match", "verb1", "noun1"])))

naturalLanguageProcessingIntegrationTests = TestList [TestLabel "integrationTest1" integrationTest1,
                                                      TestLabel "integrationTest2" integrationTest2,
                                                      TestLabel "integrationTest3" integrationTest3,
                                                      TestLabel "integrationTest4" integrationTest4,
                                                      TestLabel "integrationTest5" integrationTest5,
                                                      TestLabel "integrationTest6" integrationTest6,
                                                      TestLabel "integrationTest7" integrationTest7,
                                                      TestLabel "integrationTest8" integrationTest8,
                                                      TestLabel "integrationTest9" integrationTest9,
                                                      TestLabel "integrationTest10" integrationTest10,
                                                      TestLabel "integrationTest11" integrationTest11,
                                                      TestLabel "integrationTest12" integrationTest12,
                                                      TestLabel "integrationTest13" integrationTest13,
                                                      TestLabel "integrationTest14" integrationTest14,
                                                      TestLabel "integrationTest15" integrationTest15,
                                                      TestLabel "integrationTest16" integrationTest16,
                                                      TestLabel "integrationTest17" integrationTest17,
                                                      TestLabel "integrationTest18" integrationTest18,
                                                      TestLabel "integrationTest19" integrationTest19,
                                                      TestLabel "integrationTest20" integrationTest20,
                                                      TestLabel "integrationTest21" integrationTest21]