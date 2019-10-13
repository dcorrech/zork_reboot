-- Scenes.hs
-- Copyright Damasia Maria Correch & Jeffrey Miiller 2019
-- Starting point: a graph wih scenes, basically the map for players to explore
import System.IO

-- Scene includes String (description of scene), Integer is key/index of current scene, [Component] items available to interact with in scene, another [Component] for actions available in scene, and 4 SceneMap for the adjacent scenes (N/E/S/W)
data SceneMap = Scene Integer String [SceneComponent] [SceneComponent] SceneMap SceneMap SceneMap SceneMap
                | EmptyScene SceneMap
                | SceneError String SceneMap

instance Show SceneMap where
    show (Scene i description items actions n e s w) = show description
    show (EmptyScene parent) = show parent
    show (SceneError msg parent) = show msg

zorkMapStart = Scene 0 "You wake up in a dusty, dimly lit room. The paint on the wall is chipping away, and the carpet is frayed. To the South of the room, you see a worn wooden door. To the West, there is a boarded-up window."
                    [Component "carpet", Component "wall", Component "boards", Component "window", Component "door", Component "doorknob"] [Component "inspect", Component "open", Component "turn", Component "break", Component "look"] sceneNorth sceneEast sceneSouth sceneWest

sceneNorth = Scene 1 "The North wall of the room is barren, and the paint looks old." [Component "wall", Component "paint"] [Component "inspect", Component "look", Component "touch"] (EmptyScene sceneNorth) sceneEast zorkMapStart sceneWest
sceneEast = Scene 2 "The East wall of the room has scratches on it, chipping away the paint more than the other walls." [Component "wall", Component "paint"] [Component "inpect", Component "look", Component "touch"] sceneNorth (EmptyScene sceneEast) sceneSouth zorkMapStart
sceneSouth = Scene 3 "There is a wooden door in front of you. It looks heavy and old." [Component "door", Component "doorknob", Component "wall"] [Component "inspect", Component "open", Component "turn", Component "kick"] zorkMapStart sceneEast (SceneError "You can't walk through a closed door." sceneSouth) sceneWest
sceneWest = Scene 4 "There is a boarded up window in front of you. There are scratches on the boards, and the dim light in the room is filtering in from the cracks between the boards here." [Component "window", Component "board"] [Component "inspect", Component "look", Component "take", Component "pull"] sceneNorth zorkMapStart sceneSouth (EmptyScene sceneWest)

-- SceneComponents allow players to interact with scene; Components are objects present in the scene, while Components are the verbs used to interact with Components
data SceneComponent = Component String
                deriving (Show, Eq)

-- Takes a given SceneMap, starts and ends the game. Based on play function by David Poole (2019) given in Assignment 3.
play :: SceneMap -> IO SceneMap
play map = 
    do
        putStrLn "Start your adventure?"
        ans <- getLine
        if (ans `elem` ["y", "yes", "ye", "yeah", "sure", "oui"])
            then do
                putStrLn("Movement: type N to move north, E for east, S for south, W for west.")
                newmap <- readScene map
                return newmap
            else do
                putStrLn ("Game over.")
                return map

-- Takes given SceneMap, prints description and advances user through the map. Based on askabout fucntion by David Poole (2019) given in Assignment 3.
readScene :: SceneMap -> IO SceneMap
readScene (Scene i description items actions n e s w) =
    do
        putStrLn(description ++ " What do you do?")
        line <- getLine
        if (line `elem` ["N","n","north","North"])
            then do
                newScene <- readScene n
                return newScene
            else if (line `elem` ["E","e","east","East"])
                then do
                    newScene <- readScene e
                    return newScene
                else if (line `elem` ["S","s","south","South"])
                    then do
                        newScene <- readScene s
                        return newScene
                    else if (line `elem` ["W","w","west","West"])
                        then do
                            newScene <- readScene w
                            return newScene
                        else do
                            putStrLn ("Command not recognized.")
                            currentScene <- readScene (Scene i description items actions n e s w)
                            return currentScene                    

readScene (SceneError errormsg parent) =
    do
        putStrLn(errormsg ++ " What do you do instead?")
        readScene parent

readScene (EmptyScene parent) = 
    do
        putStrLn("There is no path this way. What do you do instead?")
        parentScene <- readScene parent
        return parentScene

go :: IO SceneMap
go = play zorkMapStart