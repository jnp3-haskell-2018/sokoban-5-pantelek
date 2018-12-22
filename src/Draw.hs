module Draw where

import Graphs
import Types
import Levels
import MazeOps

addCharAtInitialPos :: DrawFun -> Char -> DrawFun
addCharAtInitialPos d c = (\x y -> if x == 0 && y == 0 then c else d x y)

blank, wall, ground, storage, box, boxt, player, playert :: Picture
wall    = (\d -> addCharAtInitialPos d '#')
player  = (\d -> addCharAtInitialPos d '@')
playert = (\d -> addCharAtInitialPos d '+')
box     = (\d -> addCharAtInitialPos d '$')
boxt    = (\d -> addCharAtInitialPos d '*')
storage = (\d -> addCharAtInitialPos d '.')
ground  = (\d -> addCharAtInitialPos d ' ')
blank   = id 

(&) = (.)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile BoxT    = boxt
drawTile Blank   = blank

translated :: Integer -> Integer -> Picture -> Picture
translated x y p = (\d -> \x' y' -> p (\x'' y'' -> d (x'' + x) (y'' - y)) (x' - x) (y' + y))

pictureOfMaze :: Level -> Picture
pictureOfMaze level@(Level pos maze) =
  foldr (\c pic -> pic & (translated (coordX c) (coordY c) (drawTile(maze c)))) blank l
  where l = findAllReachable (mazeNeighbours maze) [pos] []
