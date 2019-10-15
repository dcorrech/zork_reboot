-- Game.hs
-- Copyright Damasia Maria Correch & Jeffrey Miiller 2019
-- Graph traversal mechanics and playing the Zork Reboot game.

module Game where

import System.IO
import System.Exit
import Scenes

import NaturalLanguageLexer
import NaturalLanguageParser

--Remove
import TestUtils

import Data.List.Split

-- ALLTODOs for Game:
-- TODO: Remove ^? characters that show up when entering delete.
-- TODO: Fix following bug: putting in "inspect" matches to a simple sentence.


-- Takes a given SceneMap, starts and ends the game. Adapted from play function by David Poole (2019) given in Assignment 3.
play :: SceneMap -> IO SceneMap
play map = 
    do
        putStrLn "Start your adventure?"
        ans <- getLine
        if (ans `elem` ["y", "yes", "ye", "yeah", "sure", "oui"])
            then do
                putStrLn("Movement: type N to move north, E for east, S for south, W for west.")
                putStrLn("You wake up in an unfamiliar place, dazed and disoriented.")
                newmap <- readScene map "not read"
                return newmap
            else do
                putStrLn ("Okay, bye!")
                exitSuccess

-- Takes given SceneMap, prints description and advances user through the map. Adapted from askabout function by David Poole (2019) given in Assignment 3.
readScene :: SceneMap -> String -> IO SceneMap
readScene (Scene i description actions n e s w conditionals) flag =
    do
        if (flag == "not read")
            then do 
                putStrLn(description ++ " What do you do?")
            else putStrLn("What do you do?")
        line <- getLine
        let sentences = parseLine line
        print sentences
        if (fixdel(line) `elem` ["N","n","north","North"])
            then do
                newScene <- readScene n "not read"
                return newScene
            else if (fixdel(line) `elem` ["E","e","east","East"])
                then do
                    newScene <- readScene e "not read"
                    return newScene
                else if (fixdel(line) `elem` ["S","s","south","South"])
                    then do
                        newScene <- readScene s "not read"
                        return newScene
                    else if (fixdel(line) `elem` ["W","w","west","West"])
                        then do
                            newScene <- readScene w "not read"
                            return newScene
                        else if (fixdel(line) `elem` actions) -- THIS WILL BE CHANGED TO SOMETHING WITH THE PARSER, SO THAT I CAN GRAB THE INDEX OF THE CONDITIONAL SCENE BASED ON THE INPUT AND AVAILABLE ACTIONS
                            then do
                                let index = getListIndex line actions
                                conditionalScene <- readScene (conditionals!!index) "not read"
                                return conditionalScene
                            -- else if (fixdel(line) `elem` masterVerbs) 
                            --     then do
                            --     putStrLn ("You can't do that here.")
                            --     currentScene <- readScene (Scene i description actions n e s w conditionals) "read"
                            --     return currentScene 
                                else if (fixdel(line) == "quit" || (fixdel(line) == "exit"))
                                    then do
                                        putStrLn("You have quit the game. Goodbye!")
                                        exitSuccess
                                    else do
                                        putStrLn ("COMMAND NOT RECOGNIZED.")
                                        currentScene <- readScene (Scene i description actions n e s w conditionals) "read"
                                        return currentScene          
                                        
readScene (Scene1 i description actions n e s w) flag =
    do
        if (flag == "not read")
            then do 
                putStrLn(description ++ " What do you do?")
            else putStrLn("What do you do?")
        line <- getLine
        let sentences = parseLine line
        print sentences
        if (fixdel(line) `elem` ["N","n","north","North"])
            then do
                newScene <- readScene n "not read"
                return newScene
            else if (fixdel(line) `elem` ["E","e","east","East"])
                then do
                    newScene <- readScene e "not read"
                    return newScene
                else if (fixdel(line) `elem` ["S","s","south","South"])
                    then do
                        newScene <- readScene s "not read"
                        return newScene
                    else if (fixdel(line) `elem` ["W","w","west","West"])
                        then do
                            newScene <- readScene w "not read"
                            return newScene
                        else if ((inActionList sentences actions) /= EmptyAction)
                            then do
                                conditionalScene <- readScene (getSceneMap (inActionList sentences actions)) "read"
                                return conditionalScene
                            -- else if (sentences `elem` allTokens) 
                            --     then do
                            --     putStrLn ("You can't do that here.")
                            --     currentScene <- readScene (Scene i description actions n e s w conditionals) "read"
                            --     return currentScene 
                                else if (fixdel(line) == "quit" || (fixdel(line) == "exit"))
                                    then do
                                        putStrLn("You have quit the game. Goodbye!")
                                        exitSuccess
                                    else do
                                        putStrLn ("COMMAND NOT RECOGNIZED.")
                                        currentScene <- readScene (Scene1 i description actions n e s w) "read"
                                        return currentScene          

readScene (SceneError errormsg parent) _ =
    do
        putStrLn(errormsg ++ " What do you do instead?")
        parentScene <- readScene parent "read"
        return parentScene
        

readScene (EmptyScene parent) _ = 
    do
        putStrLn("There is no path this way. What do you do instead?")
        parentScene <- readScene parent "read"
        return parentScene

readScene (InspectedScene description parent) _ = 
    do
        putStrLn(description)
        parentScene <- readScene parent "read"
        return parentScene

readScene DeathScene _ = -- EVENTUALLY INCLUDE POINT VALUE HERE
    do
        putStrLn("YOU HAVE DIED. Play again?")
        line <- getLine
        if (line `elem` ["y", "yes", "ye", "yeah", "sure", "oui"])
            then do
                play zorkMapStart
            else do
                putStrLn("Okay, bye!")
                exitSuccess

getSceneMap :: Action -> SceneMap
getSceneMap (Action sentences scenemap) = scenemap
getScenemap EmptyAction = NullScene

inActionList :: [Sentence] -> [Action] -> Action 
inActionList [] [] = EmptyAction
inActionList [] lst = EmptyAction
inActionList lst [] = EmptyAction
inActionList sentences (action:rest)
    |inAction sentences action = action
    |otherwise = inActionList sentences rest

inAction :: [Sentence] -> Action -> Bool 
inAction [] _ = False
inAction sentences (Action asentences scenemap)
    |[x | x <- sentences, y <- asentences, x == y] == [] = False
    |otherwise = True

parseLine :: [Char] -> [Sentence]
parseLine [] = []
parseLine line = parseSentence tokenMatches
    where processedLine = Data.List.Split.splitOneOf "' ', '\t'" line
          tokenMatches = lexInput allTokens processedLine

getListIndex e lst = indexHelper e lst 0

indexHelper e (h:t) acc
    | e == h = acc
    | otherwise = indexHelper e t (acc+1)

-- Returns str without instances of \DEL or characters directly preceding \DEL
fixdel str = delhelper str []

-- Uses accumulator str to remove elements of string that are followed by \DEL
delhelper [] str = str
delhelper (h1:t1) str
   | (h1 == '\DEL') = delhelper t1 (removelast str)
   | otherwise = delhelper t1 (str ++ [h1])

-- Removes last element of a list
removelast (h:t)
   | t == [] = t
   | otherwise = h:(removelast t)


-- Starts game, based on go function by David Poole (2019) for Assignment 3
go :: IO SceneMap
go = play zorkMapStart