module NaturalLanguageLexerTests where

import NaturalLanguageLexer
import TestUtils
import Test.HUnit

------- Tests ---------

--TODO: Swap order of test cases. (i.e. the first should be the expected, second should be the method result)
tokenizeTest1 = TestCase (assertEqual "for (tokenize \"\" dummyTokenVerb),"
                                      (tokenize "" dummyTokenVerb)
                                      Nothing)
tokenizeTest2 = TestCase (assertEqual "for (tokenize \"\" dummyTokenNoun),"
                                      (tokenize "" dummyTokenNoun)
                                      Nothing)
tokenizeTest3 = TestCase (assertEqual "for (tokenize \"non-match\" dummyTokenVerb),"
                                      (tokenize "non-match" dummyTokenVerb)
                                      Nothing)
tokenizeTest4 = TestCase (assertEqual "for (tokenize \"non-match\" dummyTokenNoun),"
                                      (tokenize "non-match" dummyTokenNoun)
                                      Nothing)
tokenizeTest5 = TestCase (assertEqual "for (tokenize \"verb1\" dummyTokenVerb),"
                                      (tokenize "verb1" dummyTokenVerb)
                                      (Just (TokenMatch "verb1" [dummyTokenVerb])))
tokenizeTest6 = TestCase (assertEqual "for (tokenize \"noun1\" dummyTokenNoun),"
                                      (tokenize "noun1" dummyTokenNoun)
                                      (Just (TokenMatch "noun1" [dummyTokenNoun])))

-- Test if case-insensitive
tokenizeTest7 = TestCase (assertEqual "for (tokenize \"vERb1\" dummyTokenVerb),"
                                      (tokenize "vERb1" dummyTokenVerb)
                                      (Just (TokenMatch "verb1" [dummyTokenVerb])))
tokenizeTest8 = TestCase (assertEqual "for (tokenize \"nOuN1\" dummyTokenNoun),"
                                      (tokenize "nOuN1" dummyTokenNoun)
                                      (Just (TokenMatch "noun1" [dummyTokenNoun])))

joinTest1 = TestCase (assertEqual "for (join Nothing Nothing),"
                                  (join Nothing Nothing)
                                  Nothing)
joinTest2 = TestCase (assertEqual "for (join (Just dummyTokenNounMatch) Nothing),"
                                  (join (Just dummyTokenNounMatch) Nothing)
                                  (Just dummyTokenNounMatch))
joinTest3 = TestCase (assertEqual "for (join Nothing (Just dummyTokenNounMatch)),"
                                  (join Nothing (Just dummyTokenNounMatch))
                                  (Just dummyTokenNounMatch))
joinTest4 = TestCase (assertEqual "for (join (Just equivalentTokenNounMatch) (Just equivalentTokenVerbMatch)),"
                                  (join (Just equivalentTokenNounMatch) (Just equivalentTokenVerbMatch))
                                  (Just equivalentTokenJoinedMatch))

lexerTest1 = TestCase (assertEqual "for (lexInput [] []),"
                                   (lexInput [] [])
                                   [])
lexerTest2 = TestCase (assertEqual "for (lexInput dummyPossibleTokens []),"
                                   (lexInput dummyPossibleTokens [])
                                   [])
lexerTest3 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\"] []),"
                                   (lexInput dummyPossibleTokens ["no-match"])
                                   [])
lexerTest4 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\", \"no-match\"],"
                                   (lexInput dummyPossibleTokens ["no-match", "no-match"])
                                   [])
lexerTest5 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"equal1\"]),"
                                   (lexInput dummyPossibleTokens ["equal1"])
                                   [TokenMatch "equal1" [TokenNoun "equal1" ["equal1", "synonym"],
                                                         TokenVerb "equal1" ["equal1", "synonym"]]])
lexerTest6 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"noun1\", \"no-match\"]),"
                                   (lexInput dummyPossibleTokens ["noun1", "no-match"])
                                   [TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]])
lexerTest7 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\", \"verb1\"]),"
                                   (lexInput dummyPossibleTokens ["no-match", "verb1"])
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]]])
lexerTest8 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"verb1\", \"noun1\"]),"
                                   (lexInput dummyPossibleTokens ["verb1", "noun1"])
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]],
                                    TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]])
lexerTest9 = TestCase (assertEqual "for  (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"no-match\", \"noun1\"]),"
                                   (lexInput dummyPossibleTokens ["no-match", "verb1", "no-match", "noun1"])
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]],
                                    TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]])


naturalLanguageLexerTests = TestList [TestLabel "tokenizeTest1" tokenizeTest1,
                       TestLabel "tokenizeTest2" tokenizeTest2,
                       TestLabel "tokenizeTest3" tokenizeTest3,
                       TestLabel "tokenizeTest4" tokenizeTest4,
                       TestLabel "tokenizeTest5" tokenizeTest5,
                       TestLabel "tokenizeTest6" tokenizeTest6,
                       TestLabel "tokenizeTest7" tokenizeTest7,
                       TestLabel "tokenizeTest8" tokenizeTest8,
                       TestLabel "joinTest1" joinTest1,
                       TestLabel "joinTest2" joinTest2,
                       TestLabel "joinTest3" joinTest3,
                       TestLabel "joinTest4" joinTest4,
                       TestLabel "lexerTest1" lexerTest1,
                       TestLabel "lexerTest2" lexerTest2,
                       TestLabel "lexerTest3" lexerTest3,
                       TestLabel "lexerTest4" lexerTest4,
                       TestLabel "lexerTest5" lexerTest5,
                       TestLabel "lexerTest6" lexerTest6,
                       TestLabel "lexerTest7" lexerTest7,
                       TestLabel "lexerTest8" lexerTest8,
                       TestLabel "lexerTest9" lexerTest9]