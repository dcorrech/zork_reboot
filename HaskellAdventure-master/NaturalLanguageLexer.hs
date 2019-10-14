module NaturalLanguageLexer (Token(..),
                             TokenMatch(..),
                             join,
                             lexInput,
                             tokenize) where

import qualified Data.Char


------- Algebraic Data Types ---------

data Token = TokenVerb String [String] |
             TokenNoun String [String] deriving (Show, Eq)

data TokenMatch = TokenMatch String [Token] deriving (Show, Eq)



------- Functions ---------

-- Match a single word to the correct token
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

-- Joins TokenMatches. Used when a given commandWord matches to both a TokenVerb and a TokenNoun.
join :: Maybe TokenMatch -> Maybe TokenMatch -> Maybe TokenMatch
join Nothing Nothing    = Nothing
join (Just a) Nothing   = Just a
join Nothing (Just b)   = Just b
join (Just (TokenMatch wordA tokensA)) (Just (TokenMatch wordB tokensB))
    | wordA == wordB    =(Just (TokenMatch wordA (tokensA ++ tokensB))) -- both matches involve the same word
    | otherwise         = Nothing                                       -- matches are of different words. join should not be called like this.

lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput _ [] = []
lexInput possibleTokens (word:words) = lexTokens possibleTokens words [(foldl (\acc token -> (tokenize word token) `join` acc) Nothing possibleTokens, words)]

lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens words [] = lexInput potentialTokens words
lexTokens potentialTokens words ((Nothing, _) : tokens) = lexTokens potentialTokens words tokens
lexTokens potentialTokens words ((Just token, tokenWords) : tokens) = token : lexInput potentialTokens words












--TODO: Add to tokenize to reduce redundancy.
--matchToken :: Keyword -> Token -> Maybe TokenMatch
--matchToken word token
--    | (elem lowerCaseWord synonyms) = Just (TokenMatch word [token])
--    | otherwise                     = Nothing
--        where lowerCaseWord = map Data.Char.toLower word