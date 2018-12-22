module Types where

data Tile = Wall | Ground | Storage | Box | Blank | BoxT deriving Eq

data Direction = R | U | L | D deriving Eq

data Coord = C {
    coordX :: Integer,
    coordY :: Integer
} deriving Eq

type Maze = Coord -> Tile

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

data Level = Level Coord Maze

data Event = KeyPress String

type Screen = String

data Interaction world = Interaction
    world
    (Event -> world -> world)
    (world -> Screen)
    
data State = S {    
    stPos   :: Coord,
    stDir   :: Direction,
    stBoxes :: [Coord],
    stLevel :: Int,
    stMoves :: Int
}
instance Eq State where
    S c d b n _ == S c' d' b' n' _ = c == c' && d == d' && b == b' && n == n'

data WithUndo state = WithUndo state [state] 

data SSState state = StartScreen | Running state deriving Eq
