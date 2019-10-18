-- Adapted from Laurence Emms "What The Functional" Website on Haskell programming.
-- See https://whatthefunctional.wordpress.com/2018/03/10/making-a-text-adventure-in-haskell-part-2/
-- and https://github.com/WhatTheFunctional/HaskellAdventure/blob/master/NaturalLanguageParser.hs

module NaturalLanguageParser (Sentence(..),
                              parseSentence,
                              generateSentences,
                              nounsInTokenList,
                              verbsInTokenList,
                              buildSentence) where

import NaturalLanguageLexer

------- Algebraic Data Types ---------

data Sentence = InvalidSentence |
                Word Token |
                SimpleSentence Token Token deriving (Show, Eq)

------- Functions ---------

-- Given a list of TokenMatches from the lexer, construct all sentences possible with the TokenMatch list.
parseSentence :: [TokenMatch] -> [Sentence]
parseSentence [(TokenMatch _ matches)]
    = generateSentences [(verbsInTokenList matches)]
parseSentence [(TokenMatch _ matches1), (TokenMatch _ matches2)]
    = generateSentences [(verbsInTokenList matches1),
                         (nounsInTokenList matches2)]
parseSentence _ = []

-- Helper function to generate all possible sentences possible with a list of a list of Tokens
--    For single verb phrases, simple map all verbs to a Word value.
--    For verb-noun phrases, uses list comprehensions to generate all verb-noun phrases possible from a list of verbs and nouns.
generateSentences :: [[Token]] -> [Sentence]
generateSentences [tokenVerbs]      = map (\verb -> Word verb) tokenVerbs
generateSentences [verbs, nouns]    = [(SimpleSentence verb noun) | verb <- verbs, noun <- nouns]
generateSentences _                 = []

-- Returns all of the TokenVerbs in a list of Tokens
verbsInTokenList :: [Token] -> [Token]
verbsInTokenList []                                   = []
verbsInTokenList ((TokenVerb word synonyms) : rest)   = (TokenVerb word synonyms) : verbsInTokenList rest
verbsInTokenList (_ : rest)                           = verbsInTokenList rest

-- Returns all of the TokenNouns in a list of Tokens
nounsInTokenList :: [Token] -> [Token]
nounsInTokenList []                                   = []
nounsInTokenList ((TokenNoun word synonyms) : rest)   = (TokenNoun word synonyms) : nounsInTokenList rest
nounsInTokenList (_ : rest)                           = nounsInTokenList rest

-- Searches a list of Tokens for a TokenVerb with a given string identifier.
--    Returns Nothing if no match found.
--    Returns Maybe Token if match found.
findVerb :: [Token] -> String -> Maybe Token
findVerb [] string = Nothing
findVerb (verbToken:rest) string
    | ((getTokenIdentifier verbToken) == string)    = (Just verbToken)
    | otherwise                                     = findVerb rest string

-- Searches a list of Tokens for a TokenNoun with a given string identifier.
--    Returns Nothing if no match found.
--    Returns Maybe Token if match found.
findNoun :: [Token] -> String -> Maybe Token
findNoun [] string = Nothing
findNoun (nounToken:rest) string
    | ((getTokenIdentifier nounToken) == string)    = (Just nounToken)
    | otherwise                                     = findVerb rest string

-- Converts a Maybe Token to a list of Tokens
convertMaybeToList :: (Maybe Token) -> [Token]
convertMaybeToList Nothing                    = []
convertMaybeToList (Just token)               = [token]

-- Helper to build sentences from a given list of tokens.
buildSentenceHelper :: [Token] -> Sentence
buildSentenceHelper []                        = InvalidSentence
buildSentenceHelper [verbToken]               = Word verbToken
buildSentenceHelper [verbToken, nounToken]    = SimpleSentence verbToken nounToken
buildSentenceHelper _                         = InvalidSentence

-- Builds sentences from a list of all verb tokens, all noun tokens, and a list of strings
--    Used in Scene.hs to help build Actions. 
buildSentence :: [Token] -> [Token] -> [String] -> Sentence
buildSentence _ _ []                          = InvalidSentence
buildSentence verbsList _ [verb]              = buildSentenceHelper (convertMaybeToList (findVerb verbsList verb))
buildSentence verbsList nounList [verb, noun] = buildSentenceHelper ((convertMaybeToList (findVerb verbsList verb)) ++
                                                                    (convertMaybeToList (findNoun nounList noun)))
buildSentence _ _ _                           = InvalidSentence



