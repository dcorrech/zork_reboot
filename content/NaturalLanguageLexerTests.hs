module NaturalLanguageLexerTests where

import NaturalLanguageLexer
import TestUtils
import Test.HUnit

------- Tests ---------
-- NOTE: Must download the Test.HUnit package to run.

tokenizeTest1 = TestCase (assertEqual "for (tokenize \"\" dummyTokenVerb),"
                                      Nothing
                                      (tokenize "" dummyTokenVerb))
tokenizeTest2 = TestCase (assertEqual "for (tokenize \"\" dummyTokenNoun),"
                                      Nothing
                                      (tokenize "" dummyTokenNoun))
tokenizeTest3 = TestCase (assertEqual "for (tokenize \"non-match\" dummyTokenVerb),"
                                      Nothing
                                      (tokenize "non-match" dummyTokenVerb))
tokenizeTest4 = TestCase (assertEqual "for (tokenize \"non-match\" dummyTokenNoun),"
                                      Nothing
                                      (tokenize "non-match" dummyTokenNoun))
tokenizeTest5 = TestCase (assertEqual "for (tokenize \"verb1\" dummyTokenVerb),"
                                      (Just (TokenMatch "verb1" [dummyTokenVerb]))
                                      (tokenize "verb1" dummyTokenVerb))
tokenizeTest6 = TestCase (assertEqual "for (tokenize \"noun1\" dummyTokenNoun),"
                                      (Just (TokenMatch "noun1" [dummyTokenNoun]))
                                      (tokenize "noun1" dummyTokenNoun))

-- Test if case-insensitive
tokenizeTest7 = TestCase (assertEqual "for (tokenize \"vERb1\" dummyTokenVerb),"
                                      (Just (TokenMatch "verb1" [dummyTokenVerb]))
                                      (tokenize "vERb1" dummyTokenVerb))
tokenizeTest8 = TestCase (assertEqual "for (tokenize \"nOuN1\" dummyTokenNoun),"
                                      (Just (TokenMatch "noun1" [dummyTokenNoun]))
                                      (tokenize "nOuN1" dummyTokenNoun))

joinTest1 = TestCase (assertEqual "for (join Nothing Nothing),"
                                  Nothing
                                  (join Nothing Nothing))
joinTest2 = TestCase (assertEqual "for (join (Just dummyTokenNounMatch) Nothing),"
                                  (Just dummyTokenNounMatch)
                                  (join (Just dummyTokenNounMatch) Nothing))
joinTest3 = TestCase (assertEqual "for (join Nothing (Just dummyTokenNounMatch)),"
                                  (Just dummyTokenNounMatch)
                                  (join Nothing (Just dummyTokenNounMatch)))
joinTest4 = TestCase (assertEqual "for (join (Just equivalentTokenNounMatch) (Just equivalentTokenVerbMatch)),"
                                  (Just equivalentTokenJoinedMatch)
                                  (join (Just equivalentTokenNounMatch) (Just equivalentTokenVerbMatch)))

lexerTest1 = TestCase (assertEqual "for (lexInput [] []),"
                                   []
                                   (lexInput [] []))
lexerTest2 = TestCase (assertEqual "for (lexInput dummyPossibleTokens []),"
                                   []
                                   (lexInput dummyPossibleTokens []))
lexerTest3 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\"] []),"
                                   []
                                   (lexInput dummyPossibleTokens ["no-match"]))
lexerTest4 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\", \"no-match\"],"
                                   []
                                   (lexInput dummyPossibleTokens ["no-match", "no-match"]))
lexerTest5 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"equal1\"]),"
                                   [TokenMatch "equal1" [TokenNoun "equal1" ["equal1", "synonym"],
                                                         TokenVerb "equal1" ["equal1", "synonym"]]]
                                   (lexInput dummyPossibleTokens ["equal1"]))
lexerTest6 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"noun1\", \"no-match\"]),"
                                   [TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]]
                                   (lexInput dummyPossibleTokens ["noun1", "no-match"]))
lexerTest7 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"no-match\", \"verb1\"]),"
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]]]
                                   (lexInput dummyPossibleTokens ["no-match", "verb1"]))
lexerTest8 = TestCase (assertEqual "for (lexInput dummyPossibleTokens [\"verb1\", \"noun1\"]),"
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]],
                                    TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]]
                                   (lexInput dummyPossibleTokens ["verb1", "noun1"]))
lexerTest9 = TestCase (assertEqual "for  (lexInput dummyPossibleTokens [\"no-match\", \"verb1\", \"no-match\", \"noun1\"]),"
                                   [TokenMatch "verb1" [TokenVerb "verb1" ["verb1", "synonym"]],
                                    TokenMatch "noun1" [TokenNoun "noun1" ["noun1", "synonym"]]]
                                   (lexInput dummyPossibleTokens ["no-match", "verb1", "no-match", "noun1"]))


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