module NaturalLanguageParser where

import NaturalLanguageLexer

------- Algebraic Data Types ---------

data Sentence = InvalidSentence |
                Word Token |
                SimpleSentence Token Token deriving (Show, Eq)

--parseSentence :: [TokenMatch] -> [Sentence]
--parseSentence [(TokenMatch _ tokens1), (TokenMatch _ tokens2)]
--    = makeSentence []

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