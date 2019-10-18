-- Adapted from Laurence Emms "What The Functional" Website on Haskell Programming.
-- See https://whatthefunctional.wordpress.com/2018/03/10/making-a-text-adventure-in-haskell-part-1/
-- and https://github.com/WhatTheFunctional/HaskellAdventure/blob/master/NaturalLanguageLexer.hs

module NaturalLanguageLexer (Token(..),
                             TokenMatch(..),
                             join,
                             lexInput,
                             tokenize,
                             getTokenIdentifier) where

import qualified Data.Char


------- Algebraic Data Types ---------

-- Represents the possible tokens that can be in a sentence.
--    This lexer is able to identify verbs and nouns specified in Scenes.hs
--    A TokenVerb has a String identifier and a list of synonyms.
--    A TokenNoun has a String identifier and a list of synonyms.
data Token = TokenVerb String [String] |
             TokenNoun String [String] deriving (Show, Eq)

-- Represents a string that has matched to a token.
--    Because a string may match to multiple tokens, a TokenMatch has a list of tokens.
data TokenMatch = TokenMatch String [Token] deriving (Show, Eq)



------- Functions ---------

-- Checks whether a given string matches a given token.
--    Returns Nothing if no match found.
--    Returns Maybe TokenMatch if match found.
tokenize :: String -> Token -> Maybe TokenMatch
tokenize "" _ = Nothing -- empty word matches to nothing
tokenize commandWord (TokenVerb word synonyms)
    | (elem lowerCaseWord synonyms) = Just (TokenMatch word [(TokenVerb word synonyms)])
    | otherwise                     = Nothing
        where lowerCaseWord = map Data.Char.toLower commandWord
tokenize commandWord (TokenNoun word synonyms)
    | (elem lowerCaseWord synonyms) = Just (TokenMatch word [(TokenNoun word synonyms)])
    | otherwise                     = Nothing
        where lowerCaseWord = map Data.Char.toLower commandWord

-- Combines two TokenMatches into one. Used when a given string matches to two different tokens.
--    Returns Nothing when the two TokenMatches don't match or when given Nothing as one of the inputs.
--    Returns TokenMatch with combined list of Tokens when the two TokenMatches match.
join :: Maybe TokenMatch -> Maybe TokenMatch -> Maybe TokenMatch
join Nothing Nothing    = Nothing
join (Just a) Nothing   = Just a
join Nothing (Just b)   = Just b
join (Just (TokenMatch wordA tokensA)) (Just (TokenMatch wordB tokensB))
    | wordA == wordB    =(Just (TokenMatch wordA (tokensA ++ tokensB))) -- both matches involve the same word
    | otherwise         = Nothing                                       -- matches are of different words. join should not be called like this.

-- Given a list of possible tokens and a sentence, returns a list of TokenMatches for all words in the sentence.
--    Uses foldl to attempt to tokenize each string with all possible tokens.
lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput _ [] = []
lexInput possibleTokens (word:words) = lexTokens possibleTokens words [(foldl (\acc token -> (tokenize word token) `join` acc) Nothing possibleTokens, words)]

-- Mutually-recursive helper function for lexInput.
--    Builds the list of TokenMatches to be returned by lexInput
--    If lexInput found no TokenMatches, call lexInput on the remaining words.
--    If lexInput found TokenMatches and the first TokenMatch in the passed list is Nothing, then recursively call lexTokens
--        on the rest of TokenMatches
--    If lexInput found TokenMatches and the first TokenMatch in the passed list is a Just TokenMatch, then append token
--        to to-be-returned list lexTokens and call lexInput
lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens words [] = lexInput potentialTokens words
lexTokens potentialTokens words ((Nothing, _) : tokens) = lexTokens potentialTokens words tokens
lexTokens potentialTokens words ((Just token, tokenWords) : tokens) = token : lexInput potentialTokens words

-- Returns the string identifier of a token.
getTokenIdentifier :: Token -> String
getTokenIdentifier (TokenVerb string _) = string
getTokenIdentifier (TokenNoun string _) = string