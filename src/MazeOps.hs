module MazeOps where

import Types
import Graphs

isWalkable :: Tile -> Bool
isWalkable Ground  = True
isWalkable Storage = True
isWalkable _       = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1)  y   
adjacentCoord L (C x y) = C (x-1)  y   
adjacentCoord U (C x y) = C x     (y+1)
adjacentCoord D (C x y) = C x     (y-1)

findBoxes :: Level -> [Coord]
findBoxes (Level pos maze) = filter (\c -> maze c == Box ) reachableCoords
  where reachableCoords = findAllReachable (mazeNeighbours maze) [pos] []

removeBoxes :: Level -> Level
removeBoxes (Level pos maze) = Level pos (f  . maze)
  where f = (\t -> if t == Box then Ground else t)

addBoxes :: [Coord] -> Level -> Level
addBoxes cords (Level pos maze) = (Level pos f)
  where f c = if (elem c cords) 
                then if (maze c == Storage) then BoxT else Box 
                else maze c

mazeNeighbours :: Maze -> Coord -> [Coord]
mazeNeighbours maze c@(C x y)
  | maze c == Blank = []
  | maze c == Wall  = filter (\c' -> maze c' == Wall) closeCoords
  | otherwise        = closeCoords
    where closeCoords = [C x (y-1), C x (y+1), C (x-1) y, C (x+1) y]

isClosed :: Level -> Bool
isClosed (Level initial maze) = 
  isWalkable (maze initial)
  && isGraphClosed initial neighbours (\x -> maze x /= Blank)
  where
    neighbours = (mazeNeighbours maze)

isSane :: Level -> Bool
isSane (Level pos maze) =
  boxesCount <= storagesCount
  where
    boxesCount    = length(filter (\x -> maze x == Box) l)
    storagesCount = length(filter (\x -> maze x == Storage) l)
    l = findAllReachable (mazeNeighbours maze) [pos] []