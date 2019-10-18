module TestUtils where

import NaturalLanguageLexer
import NaturalLanguageParser

-- Build dummy token verb.
dummyTokenVerb :: Token
dummyTokenVerb = (TokenVerb "verb1" ["verb1", "synonym"])

-- Build another dummy token verb.
secondDummyTokenVerb :: Token
secondDummyTokenVerb = (TokenVerb "verb2" ["verb2", "synonym"])

-- Build dummy token noun.
dummyTokenNoun :: Token
dummyTokenNoun = (TokenNoun "noun1" ["noun1", "synonym"])

-- Build dummy token verb equal to equalTokenNoun.
equalTokenVerb :: Token
equalTokenVerb = (TokenVerb "equal1" ["equal1", "synonym"])

-- Build dummy token noun equal to equalTokenVerb.
equalTokenNoun :: Token
equalTokenNoun = (TokenNoun "equal1" ["equal1", "synonym"])

-- Build dummy token verb match.
dummyTokenVerbMatch :: TokenMatch
dummyTokenVerbMatch = (TokenMatch "verb1" [dummyTokenVerb])

-- Build dummy token noun match.
dummyTokenNounMatch :: TokenMatch
dummyTokenNounMatch = (TokenMatch "noun1" [dummyTokenNoun])

-- Build dummy token verb match equal to equivalentTokenNounMatch.
equivalentTokenVerbMatch :: TokenMatch
equivalentTokenVerbMatch = (TokenMatch "equal1" [equalTokenNoun])

-- Build dummy token noun match equal to equivalentTokenVerbMatch.
equivalentTokenNounMatch :: TokenMatch
equivalentTokenNounMatch = (TokenMatch "equal1" [equalTokenVerb])

-- Output of joining equivalentTokenNounMatch and equivalentTokenVerbMatch
equivalentTokenJoinedMatch :: TokenMatch
equivalentTokenJoinedMatch = (TokenMatch "equal1" [equalTokenVerb, equalTokenNoun])

-- List of dummy tokens that can be matched two.
dummyPossibleTokens :: [Token]
dummyPossibleTokens = [dummyTokenVerb, dummyTokenNoun, secondDummyTokenVerb, equalTokenVerb, equalTokenNoun]

-- Another list of dummy tokens.
dummyListOfTokens :: [Token]
dummyListOfTokens = [dummyTokenVerb,
                     dummyTokenNoun,
                     (TokenVerb "verb2" ["verb2", "synonym"]),
                     (TokenVerb "verb3" ["verb3", "synonym"]),
                     (TokenNoun "noun2" ["noun2", "synonym"])]

-- Verbs in dummyListOfTokens
verbsInDummyList :: [Token]
verbsInDummyList = [dummyTokenVerb,
                    (TokenVerb "verb2" ["verb2", "synonym"]),
                    (TokenVerb "verb3" ["verb3", "synonym"])]

-- Nouns in dummyListOfTokens
nounsInDummyList :: [Token]
nounsInDummyList = [dummyTokenNoun,
                    (TokenNoun "noun2" ["noun2", "synonym"])]

-- All possible sentences from verbsInDummyList and nounsInDummyList for the possible matchable tokens dummyPossibleTokens.
allPossibleSimpleSentencesFromVerbNounDummyLists :: [Sentence]
allPossibleSimpleSentencesFromVerbNounDummyLists = [SimpleSentence (TokenVerb "verb1" ["verb1","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb1" ["verb1","synonym"]) (TokenNoun "noun2" ["noun2","synonym"]),
                                                    SimpleSentence (TokenVerb "verb2" ["verb2","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb2" ["verb2","synonym"]) (TokenNoun "noun2" ["noun2","synonym"]),
                                                    SimpleSentence (TokenVerb "verb3" ["verb3","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb3" ["verb3","synonym"]) (TokenNoun "noun2" ["noun2","synonym"])]

-- a dummy word
dummyWord :: Sentence
dummyWord = (Word dummyTokenVerb)

-- a dummy verb-noun sentence
dummySimpleSentence :: Sentence
dummySimpleSentence = (SimpleSentence dummyTokenVerb dummyTokenNoun)