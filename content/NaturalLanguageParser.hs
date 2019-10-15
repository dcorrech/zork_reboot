-- Adapted from Laurence Emms "What The Functional" Website on Haskell programming.
-- See https://whatthefunctional.wordpress.com/2018/03/10/making-a-text-adventure-in-haskell-part-2/
-- and https://github.com/WhatTheFunctional/HaskellAdventure/blob/master/NaturalLanguageParser.hs

module NaturalLanguageParser (Sentence(..),
                              parseSentence,
                              generateSentences,
                              nounsInTokenList,
                              verbsInTokenList) where

import NaturalLanguageLexer

------- Algebraic Data Types ---------

data Sentence = InvalidSentence |
                Word Token |
                SimpleSentence Token Token deriving (Show, Eq)

------- Functions ---------

parseSentence :: [TokenMatch] -> [Sentence]
parseSentence [(TokenMatch _ matches)]
    = generateSentences [(verbsInTokenList matches)]
parseSentence [(TokenMatch _ matches1), (TokenMatch _ matches2)]
    = generateSentences [(verbsInTokenList matches1),
                         (nounsInTokenList matches2)]
parseSentence _ = [] --TODO: Invalid sentence?


generateSentences :: [[Token]] -> [Sentence]
generateSentences [tokenVerbs] = map (\verb -> Word verb) tokenVerbs
generateSentences [verbs, nouns] = [(SimpleSentence verb noun) | verb <- verbs, noun <- nouns]
generateSentences _ = [] -- TODO: null sentence?

-- TODO: write a general function to cover both of these cases.
verbsInTokenList :: [Token] -> [Token]
verbsInTokenList []                                   = []
verbsInTokenList ((TokenVerb word synonyms) : rest)   = (TokenVerb word synonyms) : verbsInTokenList rest
verbsInTokenList (_ : rest)                           = verbsInTokenList rest

nounsInTokenList :: [Token] -> [Token]
nounsInTokenList []                                   = []
nounsInTokenList ((TokenNoun word synonyms) : rest)   = (TokenNoun word synonyms) : nounsInTokenList rest
nounsInTokenList (_ : rest)                           = nounsInTokenList rest

-- Require tests --

findVerb :: [Token] -> String -> Maybe Token
findVerb [] string = Nothing
findVerb (verbToken:rest) string
    | ((getTokenIdentifier verbToken) == string)    = (Just verbToken)
    | otherwise                                     = findVerb rest string

findNoun :: [Token] -> String -> Maybe Token
findNoun [] string = Nothing
findNoun (nounToken:rest) string
    | ((getTokenIdentifier nounToken) == string)    = (Just nounToken)
    | otherwise                                     = findVerb rest string

convertMaybeToList :: (Maybe Token) -> [Token]
convertMaybeToList Nothing                    = []
convertMaybeToList (Just token)               = [token]

buildSentenceHelper :: [Token] -> Sentence
buildSentenceHelper []                        = InvalidSentence
buildSentenceHelper [verbToken]               = Word verbToken
buildSentenceHelper [verbToken, nounToken]    = SimpleSentence verbToken nounToken
buildSentenceHelper _                         = InvalidSentence

buildSentence :: [Token] -> [Token] -> [String] -> Sentence
buildSentence _ _ []                          = InvalidSentence
buildSentence verbsList _ [verb]              = buildSentenceHelper (convertMaybeToList (findVerb verbsList verb))
buildSentence verbsList nounList [verb, noun] = buildSentenceHelper ((convertMaybeToList (findVerb verbsList verb)) ++
                                                                    (convertMaybeToList (findNoun nounList noun)))
buildSentence _ _ _                           = InvalidSentence



