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
-- TODO: Remove ^? characters that show up when entering delete. -- Dama
-- TODO: "verb" commands that print list of available commands


lineDelimiters :: [Char]
lineDelimiters = [' ', '\t']

-- Takes a given SceneMap, starts and ends the game.

-- Starts game, based on go function by David Poole (2019) for Assignment 3
go :: IO ()
go = do
      printIntroduction
      putStrLn "Start your adventure? (y/n)"
      ans <- getLine
      if (ans `elem` ["y", "yes", "ye", "yeah", "sure", "oui"])
         then do
               printGameInformation
               printGameIntro
               play zorkMapStart "not read" []
         else do
               separatePrompts
               putStrLn ("Okay, bye!")
               separatePrompts
               exitSuccess

-- Play takes the current scene, a flag indicating whether the room has been already entered, and the current inventory.
play :: SceneMap -> String -> [Item] -> IO ()
play map flag inventory=
    do
      separatePrompts
      readScene map flag inventory

-- Takes given SceneMap, prints description and advances user through the map.
readScene :: SceneMap -> String -> [Item] -> IO ()
readScene (Scene description actions n e s w) flag inventory =
    do
        printSceneIntro description flag

        line <- getLine
        let sentences = parseLineToSentence (fixdel line)
            matchedAction = (findMatchingAction sentences actions)

        actOnInput line sentences (Scene description actions n e s w) matchedAction inventory

readScene (SceneError errormsg parent) _ inventory =
    do
        putStrLn(errormsg ++ " What do you do instead?")
        play parent "read" inventory

readScene (InspectedScene description parent) _ inventory =
    do
        putStrLn(description)
        play parent "read" inventory

readScene DeathScene _ inventory =
    do
        putStrLn("YOU HAVE DIED.")
        let points =  getInventoryValue inventory
        putStrLn("You scored: " ++ (show points) ++ " points.")
        putStrLn("Play again?")
        restartGame

readScene (ExitScene string) _ inventory =
    do
        putStrLn("CONGRATULATIONS, YOU HAVE ESCAPED.")
        let points =  getInventoryValue inventory
        putStrLn("You scored: " ++ (show points) ++ " points.")
        putStrLn("Play again?")
        restartGame

actOnInput :: String -> [Sentence] -> SceneMap -> Action -> [Item] -> IO ()
actOnInput line sentences (Scene description actions n e s w) action inventory
    | (fixdel(line) == "quit" || (fixdel(line) == "exit"))                      = do
                                                                                    separatePrompts
                                                                                    putStrLn("You have quit the game. Goodbye!")
                                                                                    separatePrompts
                                                                                    exitSuccess

    | (fixdel(line) == "help")                                                  = do
                                                                                    printGameInformation
                                                                                    play (Scene description actions n e s w) "read" inventory
    | (fixdel(line) == "inventory")                                             = do
                                                                                    printInventory inventory
                                                                                    play (Scene description actions n e s w) "read" inventory
    | sentencesMatch sentences [(buildSentenceWrapper ["go", "north"]),(buildSentenceWrapper ["north"])]         = play n "not read" inventory
    | sentencesMatch sentences [(buildSentenceWrapper ["go", "east"]),(buildSentenceWrapper ["east"])]           = play e "not read" inventory
    | sentencesMatch sentences [(buildSentenceWrapper ["go", "south"]),(buildSentenceWrapper ["south"])]         = play s "not read" inventory
    | sentencesMatch sentences [(buildSentenceWrapper ["go", "west"]),(buildSentenceWrapper ["west"])]           = play w "not read" inventory
    | (action /= EmptyAction)                                                   = performAction action inventory
    | otherwise                                                                 = do
                                                                                    separatePrompts
                                                                                    putStrLn ("COMMAND NOT RECOGNIZED.")
                                                                                    play (Scene description actions n e s w) "read" inventory
actOnInput _ _ _ _ _ = return ()

performAction :: Action -> [Item] -> IO()
performAction EmptyAction _                                         = return ()
performAction (Action sentences nextScene) inventory                = play nextScene "read" inventory
performAction (InventoryChange sentences item nextScene) inventory  = play nextScene "read" (item : inventory)

findMatchingAction :: [Sentence] -> [Action] -> Action
findMatchingAction [] [] = EmptyAction
findMatchingAction [] lst = EmptyAction
findMatchingAction lst [] = EmptyAction
findMatchingAction sentences (action:rest)
    | actionMatchesWithSentences sentences action = action
    | otherwise = findMatchingAction sentences rest

actionMatchesWithSentences :: [Sentence] -> Action -> Bool
actionMatchesWithSentences [] _ = False
actionMatchesWithSentences sentences (Action asentences _)
    | [x | x <- sentences, y <- asentences, x == y] == [] = False
    | otherwise = True
actionMatchesWithSentences sentences (InventoryChange isentences _ _)
    | [x | x <- sentences, y <- isentences, x == y] == [] = False
    | otherwise = True

sentencesMatch :: [Sentence] -> [Sentence] -> Bool
sentencesMatch sentences1 sentences2 = [x | x <- sentences1, y <- sentences2, x == y] /= []

parseLineToSentence :: [Char] -> [Sentence]
parseLineToSentence [] = []
parseLineToSentence line = parseSentence tokenMatches
    where processedLine = Data.List.Split.splitOneOf lineDelimiters line
          tokenMatches = lexInput allTokens processedLine

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

getInventoryValue :: [Item] -> Int
getInventoryValue inventory = foldr (\x y -> getPointsFromItem(x) + y) 0 inventory

getPointsFromItem :: Item -> Int
getPointsFromItem (Treasure id val description) = val

getSceneMapFromAction :: Action -> SceneMap
getScenemapFromAction EmptyAction = NullScene
getSceneMapFromAction (Action _ scenemap) = scenemap
getSceneMapFromAction (InventoryChange _ _ scenemap) = scenemap

printIntroduction :: IO ()
printIntroduction = do
                      putStrLn "Welcome to Dama and Jeff's CPSC 312 Project: ZORK REBOOT"
                      putStrLn "Prepare yourself for a mindbending dive into a dark room, plus another dark room, maybe a dark hallway, some walls and floors, maybe a hammer, and some very terrifying Cthulhu references. "
                      putStrLn "Text doesn't get more exciting than this."
                      putStrLn ""
                      putStrLn ""
                      putStrLn "***YOU HAVE BEEN WARNED***"
                      separatePrompts

printGameInformation :: IO()
printGameInformation = do
                        putStrLn ""
                        putStrLn "***INFO***"
                        putStrLn "Movement: Type 'look' to describe your surroundings. Type 'inventory' to check your current inventory. Type 'help' to repeat this message."
                        putStrLn ""

printGameIntro :: IO()
printGameIntro = putStrLn "You wake up in an unfamiliar place, dazed and disoriented. There is a dusty leather bag to the side. Getting to your feet, you thread your arms through its straps and tighten it to your back."

printSceneIntro :: String -> String -> IO ()
printSceneIntro description flag
    | (flag == "not read")      = putStrLn(description ++ " What do you do?")
    | otherwise                 = putStrLn("What do you do?")

printInventory :: [Item] -> IO ()
printInventory [] = do
                      separatePrompts
                      putStrLn "Your inventory is empty."
printInventory inventory = do
                              separatePrompts
                              putStrLn "Your inventory contains:"
                              printItems inventory

printItems :: [Item] -> IO ()
printItems [] = return ()
printItems (item:rest) = do
                          print item
                          printItems rest

separatePrompts :: IO()
separatePrompts = do putStrLn ""
                     putStrLn "--------------------"
                     putStrLn ""

restartGame :: IO ()
restartGame = do
                 line <- getLine
                 if (line `elem` ["y", "yes", "ye", "yeah", "sure", "oui"])
                 then do
                       play zorkMapStart "not read" []
                 else do
                       putStrLn("Okay, bye!")
                       exitSuccess