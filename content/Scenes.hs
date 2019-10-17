-- Scenes.hs
-- Copyright Damasia Maria Correch & Jeffrey Miiller 2019
-- Starting point: a graph wih scenes, basically the map for players to explore

module Scenes where

import NaturalLanguageLexer
import NaturalLanguageParser

-- Scene includes String (description of scene), [Action] for actions available in scene and 4 SceneMap for the adjacent scenes (N/E/S/W)
data SceneMap =   Scene String [Action] SceneMap SceneMap SceneMap SceneMap
                | InspectedScene String SceneMap
                | SceneError String SceneMap
                | DeathScene
                | NullScene
                | ExitScene String
            deriving (Eq)

instance Show SceneMap where
    show (Scene description actions n e s w) = show description
    show (InspectedScene description parent) = show description
    show (SceneError msg parent) = show msg
    show DeathScene = ""
    show NullScene = ""

data Action = EmptyAction
            | Action [Sentence] SceneMap
            | InventoryChange [Sentence] Item SceneMap
        deriving (Eq, Show)

-- An item has an identifier, a points value, and a description.
data Item = Treasure String Int String deriving (Eq)

instance Show Item where
    show (Treasure id points description) = show (id ++ ": " ++ description)

-- AREA 1 SCENES

zorkMapStart = Scene "You are in a dusty, dimly lit room. The paint on the wall is chipping away, and a dirty carpet covers the ground. There is a small statue at your feet. To the South of the room, you see a worn wooden door. To the West, there is a boarded-up window."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "You are in a dusty, dimly lit room. The paint on the wall is chipping away, and a dirty carpet covers the ground. There is a small statue at your feet. To the South of the room, you see a worn wooden door. To the West, there is a boarded-up window."  zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "The carpet is stained brown and red, but seems firmly secured to the ground." zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "statue"])]
              (InspectedScene "The statue is of a odd figure that appears to be eating human beings. Is the figure an octopus? Maybe a reptile? A human being? All three?" zorkMapStart)),
     (InventoryChange [(buildSentenceWrapper ["take", "statue"])]
              (Treasure "Statue" 1000 "A statue of some octopus, reptile, and human hybrid.")
              zorkMapStart),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "From here, you see nothing but dirt on the walls." zorkMapStart))]
    sceneNorth sceneEast sceneSouth sceneWest

sceneNorth = Scene "The North wall of the room is barren, and the paint looks old. There is a old, worn painting on the wall."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "The North wall of the room is barren, and the paint looks old. There is an old, worn painting on the wall."  sceneNorth)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "There is a bit of paint peeling off the walls, revealing multiple layers of paint." sceneNorth)),
     (Action [(buildSentenceWrapper ["inspect", "paint"])]
              (InspectedScene "Some layers of paint reveal rusty red splotches that smell like blood." sceneNorth)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneNorth)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "The carpet is stained brown and red, but seems firmly secured to the ground." sceneNorth)),
     (Action [(buildSentenceWrapper ["touch", "wall"])]
              (InspectedScene "The wall is hard and rough." sceneNorth)),
     (Action [(buildSentenceWrapper ["touch", "paint"])]
              (InspectedScene "The paint is dry and flaky. You can't peel too much, but you see some paint layers interspersed with red splotches." sceneNorth)),
     (Action [(buildSentenceWrapper ["touch", "floor"]),
              (buildSentenceWrapper ["touch", "carpet"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneNorth)),
     (Action [(buildSentenceWrapper ["inspect", "painting"])]
              (InspectedScene "The painting is of a strange, awe-inspiring animal that seems to be part-man, part-lizard, and part-octopus." sceneNorth)),
     (InventoryChange [(buildSentenceWrapper ["move", "painting"])]
              (Treasure "Stone Tablet" 1000 "The stone tablet is inscribed in characters you've never seen before. Despite your lack of understanding, your overcome with fear and terror looking at it. You are forced to put it away, lest it drive you mad.")
              (InspectedScene "You gently push the painting aside. To your surprise, there is a hole in the wall containing a stone tablet. You carefully pick up the tablet and slip it into your bag."  sceneNorth)),
     (Action [(buildSentenceWrapper ["peel", "paint"])]
              (InspectedScene "The paint is dry and flaky. Peeling it reveals more layers interspersed with red splotches." sceneNorth))]
    (SceneError "There is no path this way." sceneNorth) sceneEast zorkMapStart sceneWest

sceneEast = Scene "The East wall of the room has scratches on it, chipping away the paint more than the other walls."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "The East wall of the room has scratches on it, chipping away the paint more than the other walls."  sceneEast)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "You can see that the wall has various layers of paint, some peeling away faster than others." sceneEast)),
     (Action [(buildSentenceWrapper ["inspect", "paint"])]
              (InspectedScene "The paint here looks easy to peel away." sceneEast)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneEast)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "The carpet is stained brown and red, but seems firmly secured to the ground." sceneEast)),
     (Action [(buildSentenceWrapper ["touch", "wall"])]
              (InspectedScene "The wall is hard and rough." sceneEast)),
     (Action [(buildSentenceWrapper ["touch", "paint"])]
              (InspectedScene "The paint is dry and flaky, you easily peel it off, revealing writing scratched onto the wall. It reads 'THEY HUNGER'. " sceneEast)),
     (Action [(buildSentenceWrapper ["touch", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneEast)),
     (Action [(buildSentenceWrapper ["touch", "carpet"])]
              (InspectedScene "The carpet is rough and wet in some places, but seems firmly secured to the ground." sceneEast))]
    sceneNorth (SceneError "There is no path this way." sceneEast) sceneSouth zorkMapStart

sceneSouth = Scene "There is a wooden door in front of you. It looks heavy and old."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "There is a wooden door in front of you. It looks heavy and old." sceneSouth)),
     (Action [(buildSentenceWrapper ["inspect", "door"])]
              (InspectedScene "The door is closed, and the wood is unscathed, though clearly old. The doorknob looks meticulously pollished." sceneSouth)),
     (Action [(buildSentenceWrapper ["inspect", "doorknob"])]
              (InspectedScene "The doorknob is shining from a recent pollish." sceneSouth)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "There is nothing special about the wall here. It's just dirty." sceneSouth)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "There is nothing special about the floor here. It's just dirty carpet." sceneSouth)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "There is nothing special about the carpet here. It's just dirty." sceneSouth)),
     (Action [(buildSentenceWrapper ["open", "door"]),
              (buildSentenceWrapper ["turn", "doorknob"])]
              (InspectedScene "You turn the doorknob, opening the door." goThroughDoorScene)),
     (Action [(buildSentenceWrapper ["kick", "door"])]
              (InspectedScene "You kick at the door to no avail. It's heavy and closed." sceneSouth)),
     (Action [(buildSentenceWrapper ["push", "door"])]
              (InspectedScene "The door does no budge under your weight." sceneSouth))]
     zorkMapStart sceneEast (SceneError "You can't walk through a closed door." sceneSouth) sceneWest

goThroughDoorScene = Scene "You are in the open door."
    [(Action [(buildSentenceWrapper ["look"])]
             (InspectedScene "You are in the open door."  goThroughDoorScene))]
    sceneSouth (SceneError "There is no path this way." goThroughDoorScene) firstDoorScene (SceneError "There is no path this way." goThroughDoorScene)

sceneWest = Scene "There is a boarded up window in front of you. There are scratches on the boards, and the dim light in the room is filtering in from the cracks between the boards here."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "There is a boarded up window in front of you. There are scratches on the boards, and the dim light in the room is filtering in from the cracks between the boards here."  sceneWest)),
     (Action [(buildSentenceWrapper ["inspect", "window"]),
              (buildSentenceWrapper ["inspect", "glass"])]
              (InspectedScene "The window is boarded up, but through the cracks between the boards you see that it's late afternoon, and wherever you are seems to be surrounded by dense forest." sceneWest)),
     (Action [(buildSentenceWrapper ["inspect", "boards"]),
              (buildSentenceWrapper ["inspect", "board"])]
             (InspectedScene "The boards look firmly nailed to the window, and have bloody scratches on them." sceneWest)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneWest)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
             (InspectedScene "The carpet is stained brown and red, but seems firmly secured to the ground." sceneWest)),
     (Action [(buildSentenceWrapper ["pull", "boards"]),
              (buildSentenceWrapper ["pull", "board"])]
              (SceneError "The boards are firmly nailed to the window. You cannot pull them off with your bare hands." sceneWest))]
    sceneNorth zorkMapStart sceneSouth (SceneError "There is no path this way." sceneWest)

firstDoorScene = Scene "A long hallway stretches ahead of you, spreading out to the south. To the north is a door."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "A long hallway stretches ahead of you, spreading out to the south. To the north is a door." firstDoorScene)),
     (Action [(buildSentenceWrapper ["inspect", "door"])]
              (InspectedScene "The door looks the same from this side as it did on the other side. It is remarkably clean, despite its clear old age." firstDoorScene)),
     (Action [(buildSentenceWrapper ["inspect", "doorknob"])]
              (InspectedScene "The doorknob is just as shiny on this side, still meticulously polished." firstDoorScene)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "The walls here are bare and wooden, with no paint, but there are bloody scratches leading deeper into the hall." firstDoorScene)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
             (InspectedScene "The same carpet extends into the hallway in front of you. It is frayed and stained." firstDoorScene)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "The carpet here is dirty and frayed." firstDoorScene)),
     (Action [(buildSentenceWrapper ["peel", "carpet"])]
              (SceneError "The carpet here is slightly looser, but you still cannot peel it." firstDoorScene)),
     (Action [(buildSentenceWrapper ["close", "door"])]
              (InspectedScene "The door does not close when you use a regular amount of strength" firstDoorScene)),
     (Action [(buildSentenceWrapper ["slam", "door"]),
              (buildSentenceWrapper ["force", "door"])]
              (InspectedScene "You slam the door shut. The sound of it reverberates down the hallway." firstDoorSceneDeath)),
     (Action [(buildSentenceWrapper ["turn", "doorknob"])]
              (InspectedScene "The doorknob turns easily, but the door is still open." firstDoorScene)),
     (Action [(buildSentenceWrapper ["kick", "door"])]
              (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" firstDoorScene))]
     goThroughDoorScene (SceneError "There is no path this way." firstDoorScene) hallSouth (SceneError "There is no path this way." firstDoorScene)

firstDoorSceneDeath = InspectedScene "All too quickly, you feel an icy cold sensation rising from your feet up to your throat as you see dark tentacles materializing from the shadows, shooting up the hallway and enveloping you. You barely have a second to think before you feel an undeniable madness stirring in your mind as the tentacles shoot into your mouth, and then there is just all-possessing cold and darkness." DeathScene

-- AREA 2 SCENES
hallSouth = Scene "As you walk down the hall, you see an ominous blue light coming in from the west where a room opens up, and a dimmer, yellow light from the east, where a hall stretches onward."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "As you walk down the hall, you see an ominous blue light coming in from the west where a room opens up, and a dimmer, yellow light from the east, where a hall stretches onward." hallSouth)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "There is nothing special about this wall." hallSouth)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "There is nothing special about the floor here." hallSouth)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "There is nothing special about the carpet." hallSouth)),
     (Action [(buildSentenceWrapper ["peel", "carpet"])]
              (InspectedScene "You can't peel the carpet here." hallSouth))]
    firstDoorScene hallEast (SceneError "There is no path this way." hallSouth) roomWest

roomWest = Scene "You walk into the blue-lit room, and you hear a squelching sound behind you as a wall of writhing tentacles blocks your way back. You feel a horrible icy sensation coming from the tentacles, and you hear the gurgling of an incomprehensible, guttural language coming from them. You feel your mind might give into the foreign whispers if you get too close."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "You walk into the blue-lit room, and you hear a squelching sound behind you as a wall of writhing tentacles blocks your way back. You feel a horrible icy sensation coming from the tentacles, and you hear the gurgling of an incomprehensible, guttural language coming from them. You feel your mind might give into the foreign whispers if you get too close." roomWest)),
     (Action [(buildSentenceWrapper ["inspect", "wall"]),
              (buildSentenceWrapper ["inspect", "words"])]
              (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." roomWestDeath)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"]),
              (buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is still covered with carpet, which is particularly damp here." roomWest)),
     (Action [(buildSentenceWrapper ["inspect", "tentacles"]),
              (buildSentenceWrapper ["touch", "tentacles"])]
              roomWestDeath),
     (Action [(buildSentenceWrapper ["touch", "wall"])]
              (InspectedScene "The wall is rough with scratches. They are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." roomWestDeath)),
     (Action [(buildSentenceWrapper ["touch", "floor"]),
              (buildSentenceWrapper ["touch", "carpet"])]
              (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." roomWest)),
     (Action [(buildSentenceWrapper ["peel", "carpet"])]
              (InspectedScene "You peel away the carpet, revealing a block of text in a language you don't understand. Still, you can't seem to look away, and the words start shifting as you feel a pressure building in your head. The words shift into a shape: an arrow pointing south, into a staircase you now see." roomWest))]
    (SceneError "There is no path this way." roomWest) roomWestDeath stairsSouth centerRoomWest

roomWestDeath = InspectedScene "The tentacles reach out and grab you before you can make another move, wrapping around your extremities and pulling you into the icy cold wall of movement. There are two eyes looking into yours, and you hear the strange gurgling language you heard coming from the tentacles before as your mind descends into madness. Then there is darkness." DeathScene

centerRoomWest = Scene "The icy sensation from the tentacles eases up as you step away from them, and you can calmly take in your surroundings now. To the south, a stairway goes down into the darkness. The carpet is frayed and stained, and the walls are covered in scratches; some look like words. Above you, a faint blue glow illuminates the room in a similar shade as the tentacles."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "The icy sensation from the tentacles eases up as you step away from them, and you can calmly take in your surroundings now. To the south, a stairway goes down into the darkness. The carpet is frayed and stained, and the walls are covered in scratches; some look like words. Above you, a faint blue glow illuminates the room in a similar shade as the tentacles." centerRoomWest)),
     (Action [(buildSentenceWrapper ["inspect", "wall"]),
              (buildSentenceWrapper ["inspect", "words"])]
              (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." centerRoomWest)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"]),
              (buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is still covered with carpet, which is particularly damp here." roomWest)),
     (Action [(buildSentenceWrapper ["inspect", "tentacles"])]
              (InspectedScene "From this distance, you can see the tentacles writhing a safe distance away, reaching forward but not far enough to touch you. The tentacles seem almost spectral from here, and there is something very unnerving about looking at them for too long." centerRoomWest)),
     (Action [(buildSentenceWrapper ["touch", "floor"]),
              (buildSentenceWrapper ["touch", "carpet"])]
              (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." roomWest)),
     (Action [(buildSentenceWrapper ["peel", "carpet"])]
              (InspectedScene "You peel away the carpet, revealing a block of text in a language you don't understand. Still, you can't seem to look away, and the words start shifting as you feel a pressure building in your head. The words shift into a shape: an arrow pointing south, into a staircase you now see." roomWest))]
    (SceneError "There is no path this way." roomWest) roomWest stairsSouth (SceneError "There is no path this way." roomWest)


stairsSouth = Scene "This staircase descends into darkness, where you hear faint rustling. To the east, you see an opening with a faint light, but further south the light doesn't reach, and you can't see what's ahead. A rope descends from the ceiling."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "This staircase descends into darkness, where you hear faint rustling. To the east, you see an opening with a faint light, but further south the light doesn't reach, and you can't see what's ahead. A rope descends from the ceiling." stairsSouth)),
     (Action [(buildSentenceWrapper ["inspect"]),
              (buildSentenceWrapper ["inspect", "darkness"]),
              (buildSentenceWrapper ["inspect", "staircase"])]
              (InspectedScene "There is a rope from the ceiling. The staircase seems safe to walk on, although you have no idea what is at the bottom." stairsSouth)),
     (Action [(buildSentenceWrapper ["touch", "wall"])]
              (InspectedScene "The wall is cool, and feels like stone, a big change from the scratched up wooden walls from before." stairsSouth)),
     (Action [(buildSentenceWrapper ["inspect", "rope"])]
              (InspectedScene "It looks like a normal rope. Tugging on it reveals that there is something heavy on the other end. You cannot see what that thing is." stairsSouth)),
     (InventoryChange [(buildSentenceWrapper ["pull","rope"])]
            (Treasure "Metal Bell" 1000 "A head-sized metal bell that is attached to a rope. It has a odd sequence of engravings around its perimeter.")
            (InspectedScene "You tentatively place your hands around the rope, giving it a quick tug. Feeling more assured, you begin to pull the rope towards yourself. You can tell that there is something hard and heavy at the end of the rope. Quickly, the rope becomes loose as the object falls. You quickly dash out of the way. You see that there is an engraved bell on the end. You put it in your bag." stairsSouth)),
     (Action [(buildSentenceWrapper ["touch", "floor"])]
              (InspectedScene "The floor of the staircase is wet, and moves further downwards." stairsSouth))]
    roomWest roomEast boulderHall (SceneError "There is no path this way." stairsSouth)

-- AREA 3 SCENES

hallEast = Scene "The faint yellow light from the east gets brighter as you walk further down this hall. To the south is a closed door, where the yellow light filters in through the bottom, and further east is a dead end."
    [(Action [(buildSentenceWrapper ["look"])] 
            (InspectedScene "The faint yellow light from the east gets brighter as you walk further down this hall. To the south is a closed door, where the yellow light filters in through the bottom, and further east is a dead end." hallEast)),
        (Action [(buildSentenceWrapper ["inspect","wall"])] 
            (InspectedScene "There is nothing special about this wall." hallEast)),
        (Action [(buildSentenceWrapper ["inspect","floor"]), (buildSentenceWrapper ["inspect", "carpet"])] 
            (InspectedScene "There is nothing special about the carpeting on the floor." hallEast)),
        (Action [(buildSentenceWrapper ["inspect","door"])] 
            (InspectedScene "There is nothing special about the door." hallEast)),
        (Action [(buildSentenceWrapper ["inspect","doorknob"])] 
            (InspectedScene "There is nothing special about the doorknob." hallEast)),
        (Action [(buildSentenceWrapper ["peel","carpet"])] 
            (SceneError "You cannot peel the carpet." hallEast)),
        (Action [(buildSentenceWrapper ["open","door"]),(buildSentenceWrapper ["turn","doorknob"])]
            (InspectedScene "You open the door." roomEast)),
        (Action [(buildSentenceWrapper ["kick","door"])] 
            (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" hallEast))]
    (SceneError "There is no path this way." hallEast) (SceneError "There is no path this way." hallEast) (SceneError "You can't walk through a closed door." hallEast) hallSouth 

roomEast = Scene "This room has the same yellow light that was spreading into the hall. You see a table in the center of the room, and a staircase going down to the west." -- ADD ITEMS HERE LATER
    [(Action [(buildSentenceWrapper ["look"])] 
            (InspectedScene "This room has the same yellow light that was spreading into the hall. You see a table in the center of the room, and a staircase going down to the west." roomEast)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "There is nothing special about this wall." roomEast)),
        (Action [(buildSentenceWrapper ["inspect","floor"]), (buildSentenceWrapper ["inspect","carpet"])]
            (InspectedScene "There is nothing special about the carpeting on the floor here." roomEast)),
        (Action [(buildSentenceWrapper ["inspect","door"])] 
            (InspectedScene "There is nothing special about the door." roomEast)),
        (Action [(buildSentenceWrapper ["inspect","doorknob"])] 
            (InspectedScene "There is nothing special about the doorknob." roomEast)),
        (Action [(buildSentenceWrapper ["peel","carpet"])] 
            (SceneError "You cannot peel the carpet." roomEast)),
        (Action [(buildSentenceWrapper ["inspect","table"])]
            (InspectedScene "The table is metal, and bolted to the ground. On it is a blood-stained gem." centerRoomEast)),
        (InventoryChange [(buildSentenceWrapper ["take","gem"])]
            (Treasure "Blood-stained Gem" 1000 "The gem is a rich green hue, but is unlike any stone you've ever seen. It sparkles with a magnificent intensity and seems to be of cosmic-origin. It is shaped into a sharp point, almost like a stake. On its point is a smattering of blood.")
            centerRoomEast),
        (Action [(buildSentenceWrapper ["close", "door"])]
            (InspectedScene "The door doesn't close." roomEast)),
        (Action [(buildSentenceWrapper ["slam","door"]), (buildSentenceWrapper ["force","door"])] 
            (InspectedScene "You slam the door shut. The sound of it reverberates through the room." roomEastDeath)),
        (Action [(buildSentenceWrapper ["kick","door"])]
            (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" roomEast)),
        (Action [(buildSentenceWrapper ["turn","doorknob"])]
            (InspectedScene "The doorknob turns freely." roomEast))]
    hallEast (SceneError "There is no path this way." roomEast) centerRoomEast stairsSouth

centerRoomEast = Scene "At the center of this room, you see a table. A path back to the hall opens up to the north, and there is a staircase going down to the west."
    [(Action [(buildSentenceWrapper ["look"])] 
            (InspectedScene "At the center of this room, you see a table. A path back to the hall opens up to the north, and there is a staircase going down to the west." centerRoomEast)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "There is nothing special about this wall." centerRoomEast)),
        (Action [(buildSentenceWrapper ["inspect","floor"]), (buildSentenceWrapper ["inspect","carpet"])]
            (InspectedScene "There is nothing special about the carpeting on the floor here." centerRoomEast)),
        (Action [(buildSentenceWrapper ["inspect","door"])] 
            (InspectedScene "There is nothing special about the door." centerRoomEast)),
        (Action [(buildSentenceWrapper ["inspect","doorknob"])] 
            (InspectedScene "There is nothing special about the doorknob." centerRoomEast)),
        (Action [(buildSentenceWrapper ["peel","carpet"])] 
            (SceneError "You cannot peel the carpet." centerRoomEast)),
        (Action [(buildSentenceWrapper ["inspect","table"])]
            (InspectedScene "The table is metal, and bolted to the ground. On it is a blood-stained gem." centerRoomEast)),
        (InventoryChange [(buildSentenceWrapper ["take","gem"])]
            (Treasure "Blood-stained Gem" 1000 "The gem is a rich green hue, but is unlike any stone you've ever seen. It sparkles with a magnificent intensity and seems to be of cosmic-origin. It is shaped into a sharp point, almost like a stake. On its point is a smattering of blood.")
            centerRoomEast),
        (Action [(buildSentenceWrapper ["inspect","gem"])]
            (InspectedScene "The gem is hefty, and has blood stains on its head." centerRoomEast)),
        (Action [(buildSentenceWrapper ["close", "door"])]
            (InspectedScene "The door doesn't close." roomEast)),
        (Action [(buildSentenceWrapper ["slam","door"]), (buildSentenceWrapper ["force","door"])] 
            (InspectedScene "You slam the door shut. The sound of it reverberates through the room." roomEastDeath)),
        (Action [(buildSentenceWrapper ["kick","door"])]
            (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" centerRoomEast)),
        (Action [(buildSentenceWrapper ["turn","doorknob"])]
            (InspectedScene "The doorknob turns freely." centerRoomEast))]
    hallEast (SceneError "There is no path this way." centerRoomEast) (SceneError "There is no path this way." centerRoomEast) stairsSouth

roomEastDeath = InspectedScene "All too quickly, you feel an icy cold sensation rising from your feet up to your throat as you see dark tentacles materializing from the shadows, shooting up the hallway and enveloping you. You barely have a second to think before you feel an undeniable madness stirring in your mind as the tentacles shoot into your mouth, and then there is just all-possessing cold and darkness." DeathScene

-- AREA 4
boulderHall = Scene "The bottom of the staircase flattens out, and you appear to have hit a dead end. There is a pile of boulders blocking the way forward, and the faintest light makes it through the cracks between the large rocks, providing enough light to see the formation, and the fact that this staircase is all made of stone, like a cavern."
    [(Action [(buildSentenceWrapper ["look"])] 
            (InspectedScene "The bottom of the staircase flattens out, and you appear to have hit a dead end. There is a pile of boulders blocking the way forward, and the faintest light makes it through the cracks between the large rocks, providing enough light to see the formation, and the fact that this staircase is all made of stone, like a cavern." boulderHall)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "The wall is made of solid stone, with a greenish vein running through it, getting thicker the further from the boulders it gets. " boulderHall)),
        (Action [(buildSentenceWrapper ["inspect","floor"])]
            (InspectedScene "The floor is made of solid stone, and the greenish vein that runs through it, up the stairs, looks wet." boulderHall)),
        (Action [(buildSentenceWrapper ["inspect","boulder"])]
            (InspectedScene "The boulders are blocking the way forward. They look like the same kind of stone that the rest of this passage is made of, but there are no greenish veins running through them. They are stacked up to the ceiling, but thin beams of light filter through the cracks between them." boulderHall)),
        (Action [(buildSentenceWrapper ["touch","wall"])]
            (InspectedScene "The wall is cool and dry, but the greenish being that runs through it is wet and slimy." boulderHall)),
        (Action [(buildSentenceWrapper ["touch","floor"])]
            (InspectedScene "The stone of the floor is dry, but the greenish vein that runs through it is wet and feels slimy." boulderHall)),
        (Action [(buildSentenceWrapper ["touch","boulder"])]
            (InspectedScene "The boulders feel heavy and solid, and you feel your hand tingling as you touch them." boulderHall)),
        (Action [(buildSentenceWrapper ["move","boulder"])]
            (InspectedScene "Despite how sturdy they look and feel, when you try moving the boulders, you do so effortlessly. They disintegrate at your touch, revealing a well-let passage leading into a room to the south." boulderLessHall))]
    stairsSouth (SceneError "There is no path this way." boulderHall) (SceneError "There is no path this way." boulderHall) (SceneError "There is no path this way." boulderHall)

boulderLessHall = Scene "The hall stretches further south, where a warm yellow light filters in from a room."
    [(Action [(buildSentenceWrapper ["look"])]
            (InspectedScene "The hall stretches further south, where a warm yellow light filters in from a room." boulderLessHall)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "The wall is made of solid stone, with a greenish vein running through it, getting thicker the further from the boulders it gets. " boulderLessHall)),
        (Action [(buildSentenceWrapper ["inspect","floor"])]
            (InspectedScene "The floor is made of solid stone, and the greenish vein that runs through it, up the stairs, looks wet." boulderLessHall)),
        (Action [(buildSentenceWrapper ["inspect","boulders"]), (buildSentenceWrapper ["touch","boulders"])]
            (InspectedScene "The boulders are gone, the way forward is cleared now." boulderLessHall)),
        (Action [(buildSentenceWrapper ["touch","wall"])]
            (InspectedScene "The wall is cool and dry, but the greenish being that runs through it is wet and slimy." boulderLessHall)),
        (Action [(buildSentenceWrapper ["touch","floor"])]
            (InspectedScene "The stone of the floor is dry, but the greenish vein that runs through it is wet and feels slimy." boulderLessHall))]
    stairsSouth (SceneError "There is no path this way." boulderHall) roomSouth (SceneError "There is no path this way." boulderHall)

roomSouth = Scene "This small room is lit by the glowing ceiling, which pulsates with a warm yellow light. To the east is a window, while to the west a painting hangs on the wall. The south wall is barren."
    [(Action [(buildSentenceWrapper ["look"])]
            (InspectedScene "This small room is lit by the glowing ceiling, which pulsates with a warm yellow light. To the east is a boarded up window, while to the west a painting hangs on the wall. The south wall is barren." roomSouth)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "The west wall has an abstract painting of coiling shapes, it is very colorful. There is nothing special about the south wall." roomSouth)),
        (Action [(buildSentenceWrapper ["inspect","floor"])]
            (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." roomSouth)),
        (Action [(buildSentenceWrapper ["inspect","painting"])]
            (InspectedScene "Upon closer inspection, the coils in the painting seem to be multiple octopi and squids tangled together. The paint used and the material are not anything you recognize." roomSouth)),
        (Action [(buildSentenceWrapper ["inspect","window"])]
            (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." roomSouth)),
        (Action [(buildSentenceWrapper ["inspect","boards"])]
            (InspectedScene "The boards have no cracks between them, you cannot see the other side or the glass of the window." roomSouth)),
        (Action [(buildSentenceWrapper ["touch","wall"])]
            (InspectedScene "The walls are the same stone as the staircase area." roomSouth)),
        (Action [(buildSentenceWrapper ["touch","floor"])]
            (InspectedScene "The floor is the same stone as in the staircase, cool to the touch and dry except for the greenish veins running up north starting where the boulders were before they disappeared, which are wet and slimy." roomSouth)),
        (Action [(buildSentenceWrapper ["touch","painting"])]
            (InspectedScene "You touch the painting on the west wall, feeling the unfamiliar material. It feels slimy, like the greenish veins in the floor and walls, and you feel an icy cold sensation working its way up the arm you touched it with." roomSouth)),
        (Action [(buildSentenceWrapper ["touch","window"]),(buildSentenceWrapper ["touch","boards"])]
            (InspectedScene "You walk to the east and touch the boards on the window. They feel neatly but loosely nailed together." windowScene)),
        (Action [(buildSentenceWrapper ["move","painting"])]
            (InspectedScene "You easily move the painting, revealing a hole in the wall where two glowing blue eyes stare at you." wallSceneDeath))]
    boulderLessHall windowScene (SceneError "There is no path this way." roomSouth) (SceneError "There is no path this way." roomSouth)

windowScene = Scene "As you approach the window, you hear rustling on the other side. The window is boarded up, and you can't see what's on the other side." -- WILL ADD ACTIONS WITH ITEMS ONCE THOSE ARE SET UP --> POSSIBILITY OF TAKING OUT THE BOARDS TO REACH THE WINDOW WITH TOOLS, THEN BREAKING THE WINDOW TO ESCAPE
    [(Action [(buildSentenceWrapper ["look"])]
            (InspectedScene "As you approach the window, you hear rustling on the other side. The window is boarded up, and you can't see what's on the other side." windowScene)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "There is nothing special about the wall. There is a window on it." windowScene)),
        (Action [(buildSentenceWrapper ["inspect","floor"])]
            (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." windowScene)),
        (Action [(buildSentenceWrapper ["inspect","window"]),(buildSentenceWrapper ["inspect","board"])]
            (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." windowScene)),
        (Action [(buildSentenceWrapper ["touch","wall"])]
            (InspectedScene "The wall is dry stone." windowScene)),
        (Action [(buildSentenceWrapper ["touch","floor"])]
            (InspectedScene "The floor in this area is dry stone." windowScene)),
        (Action [(buildSentenceWrapper ["touch","window"]),(buildSentenceWrapper ["touch","board"])]
            (InspectedScene "The boards on the window feel loosely nailed together." windowScene)),
        (Action [(buildSentenceWrapper ["pull","board"])]
            (InspectedScene "You pull the boards out." openWindowScene))]
    boulderLessHall (SceneError "You can't go through the boarded up window." windowScene) (SceneError "There is no path this way." windowScene) roomSouth

openWindowScene = Scene "With the boards gone, you see an open window that leads into a small path through some woods. There is a knife on the windowsill. You see a couple squirrels rustling around, rushing down the path."
    [(Action [(buildSentenceWrapper ["look"])]
            (InspectedScene "With the boards gone, you see an open window that leads into a small path through some woods. There is a knife on the windowsill. You see a couple squirrels rustling around, rushing down the path." openWindowScene)),
        (Action [(buildSentenceWrapper ["inspect","wall"])]
            (InspectedScene "There is nothing special about the wall. There is a window on it." openWindowScene)),
        (Action [(buildSentenceWrapper ["inspect", "knife"])]
              (InspectedScene "The knife is covered in fresh blood and, if your eyes don't deceive you, human flesh." openWindowScene)),
        (InventoryChange [(buildSentenceWrapper ["take", "knife"])]
              (Treasure "Ritual Knife" 1000 "A knife that appears to be used in rituals. It is freshly used.")
              openWindowScene),
        (Action [(buildSentenceWrapper ["inspect","floor"])]
            (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." openWindowScene)),
        (Action [(buildSentenceWrapper ["inspect","window"]),(buildSentenceWrapper ["inspect","board"])]
            (InspectedScene "The boards are gone, and the window is open." openWindowScene)),
        (Action [(buildSentenceWrapper ["climb","window"])]
            (InspectedScene "You climb out the window and feel the cool air of the outdoors." exitScene))]
    boulderLessHall exitScene (SceneError "There is no path this way." windowScene) roomSouth

wallSceneDeath = InspectedScene "The blue eyes' gaze burns into the back of your skull, and you don't remember who you are anymore. You willingly step into the hole in the wall, feeling the icy cold sensation that was running up your arm now consume your entire body, and you embrace it. The eyes glint for a moment, and you hear distant screaming, then you are enveloped in darkness." DeathScene

exitScene = ExitScene "Once outside, you turn and see that the window you just exited through slams shut. The structure you have just left seems to be an amalgamation of an old Victorian house and a cave, melded into each other, and the structure starts pulsating as the window closes. With you gone, the nightmarish structure recedes into the ground, leaving you standing in the middle of an empty clearing. You follow the path through the woods to safety, and the further you get from the clearing, the less you remember about what you've just experienced."

allTokens :: [Token]
allTokens = allVerbTokens ++ allNounTokens

allVerbTokens :: [Token]
allVerbTokens = [(TokenVerb "look" ["look"]),
                 (TokenVerb "inspect" ["inspect", "see", "view", "observe", "search", "examine"]),
                 (TokenVerb "touch" ["touch", "feel", "rub"]),
                 (TokenVerb "peel" ["peel", "scratch", "rip"]),
                 (TokenVerb "open" ["open"]),
                 (TokenVerb "close" ["close"]),
                 (TokenVerb "force" ["force","slam"]),
                 (TokenVerb "take" ["take", "grab", "pick"]),
                 (TokenVerb "turn" ["turn", "twist"]),
                 (TokenVerb "kick" ["kick", "bodyslam", "hit"]),
                 (TokenVerb "pull" ["pull"]),
                 (TokenVerb "push" ["push"]),
                 (TokenVerb "move" ["move", "slide"]),
                 (TokenVerb "go" ["go", "walk", "run", "skip", "slide", "moonwalk", "hop", "crabwalk"]),
                 (TokenVerb "climb" ["climb","exit","leave"]),
                 (TokenVerb "north" ["north", "North", "n", "N"]),
                 (TokenVerb "west" ["west", "West", "w", "W"]),
                 (TokenVerb "south" ["south", "South", "s", "S"]),
                 (TokenVerb "east" ["east", "East", "e", "E"])]

allNounTokens :: [Token]
allNounTokens = [(TokenNoun "floor" ["floor", "ground"]),
                 (TokenNoun "carpet" ["carpet", "carpeting"]),
                 (TokenNoun "wall" ["wall", "walls"]),
                 (TokenNoun "paint" ["paint"]),
                 (TokenNoun "door" ["door", "entrance"]),
                 (TokenNoun "window" ["window","glass"]),
                 (TokenNoun "board" ["board", "boards", "plank", "planks", "wood"]),
                 (TokenNoun "boulder" ["boulder", "boulders", "rock", "rocks", "stone", "stones"]),
                 (TokenNoun "painting" ["painting", "art", "frame"]),
                 (TokenNoun "table" ["table", "desk"]),
                 (TokenNoun "gem" ["gem", "jewel", "diamond","gemstone","crystal"]),
                 (TokenNoun "doorknob" ["doorknob", "knob", "handle"]),
                 (TokenNoun "tentacles" ["tentacles", "tentacle", "appendage"]),
                 (TokenNoun "vein" ["vein", "veins", "slime"]),
                 (TokenNoun "words" ["words", "word", "writing", "writings", "script", "scripts", "handwriting"]),
                 (TokenNoun "darkness" ["darkness"]),
                 (TokenNoun "rope" ["rope", "string"]),
                 (TokenNoun "knife" ["knife"]),
                 (TokenNoun "staircase" ["staircase", "stairs"]),
                 (TokenNoun "painting" ["painting", "artwork", "artpiece"]),
                 (TokenNoun "statue" ["statue"]),
                 (TokenNoun "north" ["north", "North", "n", "N"]),
                 (TokenNoun "west" ["west", "West", "w", "W"]),
                 (TokenNoun "south" ["south", "South", "s", "S"]),
                 (TokenNoun "east" ["east", "East", "e", "E"]),
                 (TokenNoun "scratches" ["scratches", "scratchings"])]

-- Adapted from Laurence Emms "What The Functional" Website on Haskell programming.
-- See https://whatthefunctional.wordpress.com/2018/03/10/making-a-text-adventure-in-haskell-part-2/
-- and https://github.com/WhatTheFunctional/HaskellAdventure/blob/master/NaturalLanguageParser.hs
buildSentenceWrapper :: [String] -> Sentence
buildSentenceWrapper strings = buildSentence allVerbTokens allNounTokens strings
