module TestUtils where

import NaturalLanguageLexer
import NaturalLanguageParser

dummyTokenVerb :: Token
dummyTokenVerb = (TokenVerb "verb1" ["verb1", "synonym"])

secondDummyTokenVerb :: Token
secondDummyTokenVerb = (TokenVerb "verb2" ["verb2", "synonym"])

dummyTokenNoun :: Token
dummyTokenNoun = (TokenNoun "noun1" ["noun1", "synonym"])

equalTokenVerb :: Token
equalTokenVerb = (TokenVerb "equal1" ["equal1", "synonym"])

equalTokenNoun :: Token
equalTokenNoun = (TokenNoun "equal1" ["equal1", "synonym"])

dummyTokenVerbMatch :: TokenMatch
dummyTokenVerbMatch = (TokenMatch "verb1" [dummyTokenVerb])

dummyTokenNounMatch :: TokenMatch
dummyTokenNounMatch = (TokenMatch "noun1" [dummyTokenNoun])

equivalentTokenVerbMatch :: TokenMatch
equivalentTokenVerbMatch = (TokenMatch "equal1" [equalTokenNoun])

equivalentTokenNounMatch :: TokenMatch
equivalentTokenNounMatch = (TokenMatch "equal1" [equalTokenVerb])

equivalentTokenJoinedMatch :: TokenMatch
equivalentTokenJoinedMatch = (TokenMatch "equal1" [equalTokenVerb, equalTokenNoun])

dummyPossibleTokens :: [Token]
dummyPossibleTokens = [dummyTokenVerb, dummyTokenNoun, secondDummyTokenVerb, equalTokenVerb, equalTokenNoun]

dummyListOfTokens :: [Token]
dummyListOfTokens = [dummyTokenVerb,
                     dummyTokenNoun,
                     (TokenVerb "verb2" ["verb2", "synonym"]),
                     (TokenVerb "verb3" ["verb3", "synonym"]),
                     (TokenNoun "noun2" ["noun2", "synonym"])]

verbsInDummyList :: [Token]
verbsInDummyList = [dummyTokenVerb,
                    (TokenVerb "verb2" ["verb2", "synonym"]),
                    (TokenVerb "verb3" ["verb3", "synonym"])]

nounsInDummyList :: [Token]
nounsInDummyList = [dummyTokenNoun,
                    (TokenNoun "noun2" ["noun2", "synonym"])]

allPossibleSimpleSentencesFromVerbNounDummyLists :: [Sentence]
allPossibleSimpleSentencesFromVerbNounDummyLists = [SimpleSentence (TokenVerb "verb1" ["verb1","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb1" ["verb1","synonym"]) (TokenNoun "noun2" ["noun2","synonym"]),
                                                    SimpleSentence (TokenVerb "verb2" ["verb2","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb2" ["verb2","synonym"]) (TokenNoun "noun2" ["noun2","synonym"]),
                                                    SimpleSentence (TokenVerb "verb3" ["verb3","synonym"]) (TokenNoun "noun1" ["noun1","synonym"]),
                                                    SimpleSentence (TokenVerb "verb3" ["verb3","synonym"]) (TokenNoun "noun2" ["noun2","synonym"])]

