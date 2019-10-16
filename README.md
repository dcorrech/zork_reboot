# zork_reboot

The aim of our project is to develop an in-depth, immersive, text-based interactive fiction game. The game will consist of a world within which players can move, explore the surroundings, interact with objects in the world through issuing short, text-based commands. As an example, players may be able to issue commands to move in particular directions, inspect particular objects more closely, pick up  objects, or read particular things. In order to progress in the game, players will need to move around the map and try to find a way out of the location they are stuck in. Players will find themselves thrust into the midst of a mysterious location with a unusual, interesting history and must explore the world to learn more about it. In particular, we hope to model our game after other text-based interactive fiction games such as Zork.

In order to implement this game, we will need to accomplish several tasks. Firstly, we will need to devise a sufficiently intelligent parser that is capable of interpreting a variety of possible text-based commands that, furthermore, is also reasonably flexible (i.e. there are multiple ways to achieve the same thing) to provide players with greater ease of use. Secondly, we will need to devise a way to represent the world itself and any objects, interactive or otherwise, within each location of the world. Building a traversable graph or tree structure would likely be sufficient to complete this task. Finally, we must write the story and world within which the game will take place.

The basic functionality of the natural language parser is adapted WhatTheFunctional's HaskelAdventure repo, while game's basic functionality is original code of an interactive map that players can walk around and explore. We aim to add the following extended functionality:

- The ability to hold an inventory of objects and treasures picked up in the map.
- The treasures add up to a total score upon completing the game, the more points, the better you did!
- An escape route as well as multiple death scenes that terminate the game depending on what the player does in each scene.

# How to run:
Start GHCi, load the Game module, and then call command "go".
