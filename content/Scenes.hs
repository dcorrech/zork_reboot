-- Scenes.hs
-- Copyright Damasia Maria Correch & Jeffrey Miiller 2019
-- Starting point: a graph wih scenes, basically the map for players to explore

module Scenes where

import NaturalLanguageLexer
import NaturalLanguageParser

-- Scene includes String (description of scene), Integer is key/index of current scene, [String] for actions available in scene ** WILL CHANGE TO [SceneComponent when parser is good to go], and 4 SceneMap for the adjacent scenes (N/E/S/W), ** WILL ADD [String] is list of flags for the room
data SceneMap = Scene Integer String [String] SceneMap SceneMap SceneMap SceneMap [SceneMap]
                | Scene1 Integer String [Action] SceneMap SceneMap SceneMap SceneMap
                | EmptyScene SceneMap
                | InspectedScene String SceneMap
                | DeathScene
                | SceneError String SceneMap
                | NullScene
            deriving (Eq)

instance Show SceneMap where
    show (Scene i description actions n e s w cond) = show description
    show (EmptyScene parent) = show parent
    show (InspectedScene description parent) = show description
    show (SceneError msg parent) = show msg
    show DeathScene = ""
    show NullScene = ""

data Action = Action [Sentence] SceneMap
            |EmptyAction
        deriving (Eq, Show)
testAction = Action [(SimpleSentence (TokenVerb "look" ["look", "inspect", "see", "view", "observe", "search", "examine"]) (TokenNoun "floor" ["floor", "ground", "carpet"]))] (InspectedScene "The floor is covered in a carpet with brown and reddish stains." zorkMapStart)

-- -- SceneComponents allow players to interact with scene; Integer is conditionalIndex Strings are objects present in the scene, while Strings are the verbs used to interact with Strings
-- data SceneComponent = Component Integer String
--                 deriving (Show, Eq)

-- AREA 1 SCENES
zorkMapStart = Scene1 0 "You are in a dusty, dimly lit room. The paint on the wall is chipping away, and a dirty carpet covers the ground. To the South of the room, you see a worn wooden door. To the West, there is a boarded-up window." 
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "You are in a dusty, dimly lit room. The paint on the wall is chipping away, and a dirty carpet covers the ground. To the South of the room, you see a worn wooden door. To the West, there is a boarded-up window."  zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "carpet"])]
              (InspectedScene "The carpet is stained brown and red, but seems firmly secured to the ground." zorkMapStart)),
     (Action [(buildSentenceWrapper ["inspect", "wall"])]
              (InspectedScene "From here, you see nothing but dirt on the walls." zorkMapStart))]
    sceneNorth sceneEast sceneSouth sceneWest
sceneNorth = Scene1 1 "The North wall of the room is barren, and the paint looks old."
    [(Action [(buildSentenceWrapper ["look"])]
              (InspectedScene "The North wall of the room is barren, and the paint looks old."  sceneNorth)),
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
     (Action [(buildSentenceWrapper ["touch", "floor"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneNorth)),
     (Action [(buildSentenceWrapper ["touch", "carpet"])]
              (InspectedScene "The floor is covered in a carpet with brown and reddish stains." sceneNorth)),
     (Action [(buildSentenceWrapper ["peel", "paint"])]
              (InspectedScene "The paint is dry and flaky. Peeling it reveals more layers interspersed with red splotches." sceneNorth))]
    (EmptyScene sceneNorth) sceneEast zorkMapStart sceneWest

sceneEast = Scene1 2 "The East wall of the room has scratches on it, chipping away the paint more than the other walls."
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
    sceneNorth (EmptyScene sceneEast) sceneSouth zorkMapStart

sceneSouth = Scene1 3 "There is a wooden door in front of you. It looks heavy and old."
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
              (InspectedScene "You turn the doorknob, opening the door." firstDoorScene)),
     (Action [(buildSentenceWrapper ["kick", "door"])]
              (InspectedScene "You kick at the door to no avail. It's heavy and closed." sceneSouth)),
     (Action [(buildSentenceWrapper ["puah", "door"])]
              (InspectedScene "The door does no budge under your weight." sceneSouth))]
     zorkMapStart sceneEast (SceneError "You can't walk through a closed door." sceneSouth) sceneWest

sceneWest = Scene1 4 "There is a boarded up window in front of you. There are scratches on the boards, and the dim light in the room is filtering in from the cracks between the boards here."
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
    sceneNorth zorkMapStart sceneSouth (EmptyScene sceneWest)

firstDoorScene = Scene 5 "A long hallway stretches ahead of you, spreading out to the south. To the north is a door." 
    ["look", "inspect door", "inspect doorknob", "inspect wall", "inspect floor", "inspect carpet", "peel carpet", "close door", "slam door", "force door", "turn doorknob", "kick door"] -- GONNA NEED TO ADD SOME KIND OF FLAG FOR WHETHER THE DOOR IS OPEN OR NOT
    sceneSouth (EmptyScene firstDoorScene) hallSouth (EmptyScene firstDoorScene) 
    [firstDoorScene,
        (InspectedScene "The door looks the same from this side as it did on the other side. It is remarkably clean, despite its clear old age." firstDoorScene),
        (InspectedScene "The doorknob is just as shiny on this side, still meticulously polished." firstDoorScene),
        (InspectedScene "The walls here are bare and wooden, with no paint, but there are bloody scratches leading deeper into the hall." firstDoorScene),
        (InspectedScene "The same carpet extends into the hallway in front of you. It is frayed and stained." firstDoorScene),
        (InspectedScene "The carpet here is dirty and frayed." firstDoorScene),
        (SceneError "The carpet here is slightly looser, but you still cannot peel it." firstDoorScene),
        (InspectedScene "The door does not close when you use a regular amount of strength" firstDoorScene),
        (InspectedScene "You slam the door shut. The sound of it reverberates down the hallway." firstDoorSceneDeath),
        (InspectedScene "You slam the door shut. The sound of it reverberates down the hallway." firstDoorSceneDeath),
        (InspectedScene "The doorknob turns easily, but the door is still open." firstDoorScene),
        (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" firstDoorScene)]
firstDoorSceneDeath = InspectedScene "All too quickly, you feel an icy cold sensation rising from your feet up to your throat as you see dark tentacles materializing from the shadows, shooting up the hallway and enveloping you. You barely have a second to think before you feel an undeniable madness stirring in your mind as the tentacles shoot into your mouth, and then there is just all-possessing cold and darkness." DeathScene

-- AREA 2 SCENES
hallSouth = Scene 6 "As you walk down the hall, you see an ominous blue light coming in from the west where a room opens up, and a dimmer, yellow light from the east, where a hall stretches onward."
    ["look", "inspect wall", "inspect floor", "inspect carpet", "peel carpet"]
    firstDoorScene hallEast (EmptyScene hallSouth) roomWest
    [hallSouth,
        (InspectedScene "There is nothing special about this wall." hallSouth),
        (InspectedScene "There is nothing special about the floor here." hallSouth),
        (InspectedScene "There is nothing special about the carpet." hallSouth),
        (InspectedScene "You can't peel the carpet here." hallSouth)]
roomWest = Scene 7 "You walk into the blue-lit room, and you hear a squelching sound behind you as a wall of writhing tentacles blocks your way back. You feel a horrible icy sensation coming from the tentacles, and you hear the gurgling of an incomprehensible, guttural language coming from them. You feel your mind might give into the foreign whispers if you get too close." 
    ["look", "inspect wall", "inspect words", "inspect floor", "inspect carpet", "inspect tentacles", "touch tentacles", "touch wall", "touch floor", "touch carpet", "peel carpet"]
    (EmptyScene roomWest) roomWestDeath stairsSouth centerRoomWest
    [roomWest,
        (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." roomWestDeath),
        (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." roomWestDeath),
        (InspectedScene "The floor is still covered with carpet, which is particularly damp here." roomWest),
        (InspectedScene "The carpet is very damp in this room, and you see it giving way in some places." roomWest),
        roomWestDeath,
        roomWestDeath,
        (InspectedScene "The wall is rough with scratches. They are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." roomWestDeath),
        (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." roomWest),
        (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." roomWest),
        (InspectedScene "You peel away the carpet, revealing a block of text in a language you don't understand. Still, you can't seem to look away, and the words start shifting as you feel a pressure building in your head. The words shift into a shape: an arrow pointing south, into a staircase you now see." roomWest)]
roomWestDeath = InspectedScene "The tentacles reach out and grab you before you can make another move, wrapping around your extremities and pulling you into the icy cold wall of movement. There are two eyes looking into yours, and you hear the strange gurgling language you heard coming from the tentacles before as your mind descends into madness. Then there is darkness." DeathScene
centerRoomWest = Scene 8 "The icy sensation from the tentacles eases up as you step away from them, and you can calmly take in your surroundings now. To the south, a stairway goes down into the darkness. The carpet is frayed and stained, and the walls are covered in scratches; some look like words. Above you, a faint blue glow illuminates the room in a similar shade as the tentacles."
    ["look", "inspect wall", "inspect words", "inspect floor", "inspect carpet", "inspect tentacles", "touch floor", "touch carpet", "peel carpet"]
    (EmptyScene roomWest) roomWest stairsSouth (EmptyScene roomWest) 
    [centerRoomWest,
        (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." centerRoomWest),
        (InspectedScene "The scratches on the wall are mostly unintelligible, but you make out a few disparate words: 'MADNESS', 'COLD', 'GET OUT', 'WHY'." centerRoomWest),
        (InspectedScene "The floor is still covered with carpet, which is particularly damp here." centerRoomWest),
        (InspectedScene "The carpet is very damp in this room, and you see it giving way in some places." centerRoomWest),
        (InspectedScene "From this distance, you can see the tentacles writhing a safe distance away, reaching forward but not far enough to touch you. The tentacles seem almost spectral from here, and there is something very unnerving about looking at them for too long." centerRoomWest),
        (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." centerRoomWest),
        (InspectedScene "You feel the damp carpet, it seems much looser than in the previous rooms you've seen." centerRoomWest),
        (InspectedScene "You peel away the carpet, revealing a block of text in a language you don't understand. Still, you can't seem to look away, and the words start shifting as you feel a pressure building in your head. The words shift into a shape: an arrow pointing south, into a staircase you now see." centerRoomWest)]
stairsSouth = Scene 9 "This staircase descends into darkness, where you hear faint rustling. To the east, you see an opening with a faint light, but further south the light doesn't reach, and you can't see what's ahead."
    ["look", "inspect", "touch wall", "touch floor"]
    roomWest roomEast boulderHall (EmptyScene stairsSouth)
    [stairsSouth,
        (InspectedScene "It is too dark to see anything beyond the faint light to the east." stairsSouth),
        (InspectedScene "The wall is cool, and feels like stone, a big change from the scratched up wooden walls from before." stairsSouth),
        (InspectedScene "The floor of the staircase is wet, and moves further downwards." stairsSouth)]

-- AREA 3 SCENES ** WILL ADD EXTRA ACTION OPTIONS TO AREA 5 ONCE WE GET ITEMS IN. 
hallEast = Scene1 10 "The faint yellow light from the east gets brighter as you walk further down this hall. To the south is a closed door, where the yellow light filters in through the bottom, and further east is a dead end." 
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
    (EmptyScene hallEast) (EmptyScene hallEast) (SceneError "You can't walk through a closed door." hallEast) hallSouth 
roomEast = Scene1 16 "This room has the same yellow light that was spreading into the hall. You see a table in the center of the room, and a staircase going down to the west." -- ADD ITEMS HERE LATER 
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
            (InspectedScene "The table is metal, and bolted to the ground. On it is a blood-stained hammer." centerRoomEast)),
        (Action [(buildSentenceWrapper ["close", "door"])]
            (InspectedScene "The door doesn't close." roomEast)),
        (Action [(buildSentenceWrapper ["slam","door"]), (buildSentenceWrapper ["force","door"])] 
            (InspectedScene "You slam the door shut. The sound of it reverberates through the room." roomEastDeath)),
        (Action [(buildSentenceWrapper ["kick","door"])]
            (InspectedScene "The door is very heavy, you feel a sharp pain on your foot, but can still move. What was the point of that?" roomEast)),
        (Action [(buildSentenceWrapper ["turn","doorknob"])]
            (InspectedScene "The doorknob turns freely." roomEast))]
    hallEast (EmptyScene roomEast) centerRoomEast stairsSouth
centerRoomEast = Scene1 18 "At the center of this room, you see a table. A path back to the hall opens up to the north, and there is a staircase going down to the west."
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
    hallEast (EmptyScene centerRoomEast) (EmptyScene centerRoomEast) stairsSouth
roomEastDeath = InspectedScene "All too quickly, you feel an icy cold sensation rising from your feet up to your throat as you see dark tentacles materializing from the shadows, shooting up the hallway and enveloping you. You barely have a second to think before you feel an undeniable madness stirring in your mind as the tentacles shoot into your mouth, and then there is just all-possessing cold and darkness." DeathScene

-- AREA 4
boulderHall = Scene 11 "The bottom of the staircase flattens out, and you appear to have hit a dead end. There is a pile of boulders blocking the way forward, and the faintest light makes it through the cracks between the large rocks, providing enough light to see the formation, and the fact that this staircase is all made of stone, like a cavern." 
    ["look", "inspect wall", "inspect floor", "inspect boulders", "touch wall", "touch floor", "touch boulders", "move boulders"]
    stairsSouth (EmptyScene boulderHall) (EmptyScene boulderHall) (EmptyScene boulderHall)
    [boulderHall,
        (InspectedScene "The wall is made of solid stone, with a greenish vein running through it, getting thicker the further from the boulders it gets. " boulderHall),
        (InspectedScene "The floor is made of solid stone, and the greenish vein that runs through it, up the stairs, looks wet." boulderHall),
        (InspectedScene "The boulders are blocking the way forward. They look like the same kind of stone that the rest of this passage is made of, but there are no greenish veins running through them. They are stacked up to the ceiling, but thin beams of light filter through the cracks between them." boulderHall),
        (InspectedScene "The wall is cool and dry, but the greenish being that runs through it is wet and slimy." boulderHall),
        (InspectedScene "The stone of the floor is dry, but the greenish vein that runs through it is wet and feels slimy." boulderHall),
        (InspectedScene "The boulders feel heavy and solid, and you feel your hand tingling as your touch them." boulderHall),
        (InspectedScene "Despite how study they look and feel, when you try moving the boulders, you do so effortlessly. They disintegrate at your touch, revealing a well-let passage leading into a room to the south." boulderLessHall)]
boulderLessHall = Scene 12 ""
    ["look", "inspect wall", "inspect floor", "inspect boulders", "touch wall", "touch floor", "touch boulders"]
    stairsSouth (EmptyScene boulderHall) southestRoom (EmptyScene boulderHall)
    [boulderHall,
        (InspectedScene "The wall is made of solid stone, with a greenish vein running through it, getting thicker the further from the boulders it gets. " boulderLessHall),
        (InspectedScene "The floor is made of solid stone, and the greenish vein that runs through it, up the stairs, looks wet." boulderLessHall),
        (InspectedScene "The boulders are gone, the way forward is cleared now." boulderLessHall),
        (InspectedScene "The wall is cool and dry, but the greenish being that runs through it is wet and slimy." boulderLessHall),
        (InspectedScene "The stone of the floor is dry, but the greenish vein that runs through it is wet and feels slimy." boulderLessHall),
        (InspectedScene "The boulders are gone, the way forward is cleared now." boulderLessHall)]
southestRoom = Scene 13 "This small room is lit by the glowing ceiling, which pulsates with a warm yellow light. To the east is a boarded up window, while to the west a painting hangs on the wall. The south wall is barren."
    ["look", "inspect wall", "inspect floor", "inspect painting", "inspect window", "inspect boards", "inspect board", "touch wall", "touch floor", "touch painting", "touch window", "touch boards", "touch board", "move painting"]
    boulderLessHall windowScene (EmptyScene southestRoom) wallScene
    [southestRoom,
        (InspectedScene "The west wall has an abstract painting of coiling shapes, it is very colorful. There is nothing special about the south wall." southestRoom),
        (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." southestRoom),
        (InspectedScene "Upon closer inspection, the coils in the painting seem to be multiple octopi and squids tangled together. The paint used and the material are not anything you recognize." southestRoom),
        (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." southestRoom),
        (InspectedScene "The boards have no cracks between them, you cannot see the other side or the glass of the window." southestRoom),
        (InspectedScene "The boards have no cracks between them, you cannot see the other side or the glass of the window." southestRoom),
        (InspectedScene "The walls are the same stone as the previous staircase area." southestRoom),
        (InspectedScene "The floor is the same stone as in the staircase, cool to the touch and dry except for the greenish veins running up north starting where the boulders were before they disappeared, which are wet and slimy." southestRoom),
        (InspectedScene "You walk to the west wall and touch the painting, feeling the unfamiliar material. It feels slimy, like the greenish veins in the floor and walls, and you feel an icy cold sensation working its way up the arm you touched it with." wallScene),
        (InspectedScene "You walk to the east and touch the boards on the window. They feel firmly and neatly nailed together." windowScene),
        (InspectedScene "You walk to the east and touch the boards on the window. They feel firmly and neatly nailed together." windowScene),
        (InspectedScene "You walk to the east and touch the boards on the window. They feel firmly and neatly nailed together." windowScene),
        (InspectedScene "You easily move the painting, revealing a hole in the wall where two glowing blue eyes stare at you." wallSceneDeath)]
wallScene = Scene 14 "The west wall has an abstract painting of coiling shapes, it is very colorful." 
    ["look", "inspect wall", "inspect floor", "inspect painting", "touch wall", "touch floor", "touch painting", "move painting"]
    boulderLessHall southestRoom (EmptyScene wallScene) (EmptyScene wallScene)
    [wallScene,
        (InspectedScene "There is nothing special about the wall other than the painting." wallScene),
        (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." wallScene),
        (InspectedScene "Upon closer inspection, the coils in the painting seem to be multiple octopi and squids tangled together. The paint used and the material are not anything you recognize." wallScene),
        (InspectedScene "The walls are the same stone as the previous staircase area." southestRoom),
        (InspectedScene "The floor is the same stone as in the staircase, cool to the touch and dry except for the greenish veins running up north starting where the boulders were before they disappeared, which are wet and slimy." southestRoom),
        (InspectedScene "You walk to the west wall and touch the painting, feeling the unfamiliar material. It feels slimy, like the greenish veins in the floor and walls, and you feel an icy cold sensation working its way up the arm you touched it with." wallScene),
        (InspectedScene "You easily move the painting, revealing a hole in the wall where two glowing blue eyes stare at you." wallSceneDeath)]
wallSceneDeath = InspectedScene "The blue eyes' gaze burns into the back of your skull, and you don't remember who you are anymore. You willingly step into the hole in the wall, feeling the icy cold sensation that was running up your arm now consume your entire body, and you embrace it. The eyes glint for a moment, and you hear distant screaming, then you are enveloped in darkness." DeathScene
windowScene = Scene 15 "As you approach the window, you hear rustling on the other side. The window is boarded up, and you can't see what's on the other side." -- WILL ADD ACTIONS WITH ITEMS ONCE THOSE ARE SET UP --> POSSIBILITY OF TAKING OUT THE BOARDS TO REACH THE WINDOW WITH TOOLS, THEN BREAKING THE WINDOW TO ESCAPE
    ["look", "inspect wall", "inspect floor", "inspect window", "inspect boards", "inspect board", "touch wall", "touch floor", "touch window", "touch boards", "touch board", "pull boards", "pull board"]
    boulderLessHall (EmptyScene windowScene) (EmptyScene windowScene) southestRoom
    [windowScene,
        (InspectedScene "There is nothing special about the wall. There is a window on it." windowScene),
        (InspectedScene "The floor is the same stone as in the staircase, with greenish veins running up north starting where the boulders were before they disappeared." windowScene),
        (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." windowScene),
        (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." windowScene),
        (InspectedScene "The window is boarded up with no cracks between the boards, so you cannot see the other side." windowScene),
        (InspectedScene "The wall is stone and dry." windowScene),
        (InspectedScene "The floor in this area is stone and dry." windowScene),
        (InspectedScene "The boards on the window feel firmly nailed together." windowScene),
        (InspectedScene "The boards on the window feel firmly nailed together." windowScene),
        (InspectedScene "The boards on the window feel firmly nailed together." windowScene),
        (InspectedScene "You cannot pull the boards out with your bare hands." windowScene),
        (InspectedScene "You cannot pull the boards out with your bare hands." windowScene)]

allTokens :: [Token]
allTokens = allVerbTokens ++ allNounTokens

allVerbTokens :: [Token]
allVerbTokens = [(TokenVerb "look" ["look"]),
                 (TokenVerb "inspect" ["inspect", "see", "view", "observe", "search", "examine"]),
                 (TokenVerb "touch" ["touch", "feel", "rub"]),
                 (TokenVerb "peel" ["peel", "scratch", "rip"]),
                 (TokenVerb "open" ["open"]),
                 (TokenVerb "close" ["close", "slam"]),
                 (TokenVerb "force" ["force"]),
                 (TokenVerb "take" ["take", "grab"]),
                 (TokenVerb "turn" ["turn", "twist"]),
                 (TokenVerb "kick" ["kick", "bodyslam", "hit"]),
                 (TokenVerb "pull" ["pull"]),
                 (TokenVerb "push" ["push"]),
                 (TokenVerb "move" ["move", "slide"])]

allNounTokens :: [Token]
allNounTokens = [(TokenNoun "floor" ["floor", "ground", "carpet", "carpeting"]),
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
               (TokenNoun "scratches" ["scratches", "scratchings"])]

-- Adapted from Laurence Emms "What The Functional" Website on Haskell programming.
-- See https://whatthefunctional.wordpress.com/2018/03/10/making-a-text-adventure-in-haskell-part-2/
-- and https://github.com/WhatTheFunctional/HaskellAdventure/blob/master/NaturalLanguageParser.hs
buildSentenceWrapper :: [String] -> Sentence
buildSentenceWrapper strings = buildSentence allVerbTokens allNounTokens strings
