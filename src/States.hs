module States where

import Types
import MazeOps
import Levels
import Draw

createInitialState :: Int -> State
createInitialState n =
  S pos U (findBoxes level) n 0
    where level@(Level pos _) = levels!!n

isWinning :: State -> Bool
isWinning (S _ _ b n _) = all (\x -> maze x == Storage) b
    where Level _ maze = levels!!n

endScreen :: Int -> [Char]
endScreen x = "You won in " ++ show x ++ "moves!\n\
               \Press 1 for next level\n\
               \Press esc to restart"

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s@(S _ _ _ n _)
    | key == "1"  = createInitialState ((n + 1) `mod` (length levels))
    | isWinning s = s
    | key == "d"  = tryToGo R s
    | key == "w"  = tryToGo U s 
    | key == "a"  = tryToGo L s
    | key == "s"  = tryToGo D s
handleEvent _ p   = p

tryToGo :: Direction -> State -> State
tryToGo d (S c _ b n count) = 
    if elem adjC b
    then if isWalkable (maze adjCC) && notElem adjCC b 
            then S adjC d (adjCC : filter (/= adjC) b) n (count + 1)
            else S c d b n count
    else if isWalkable (maze adjC) 
            then S adjC d b n (count + 1)
            else S c d b n count
    where
        (Level _ maze) = removeBoxes (levels!!n)
        adjC  = adjacentCoord d c
        adjCC = adjacentCoord d adjC

drawState :: State -> Screen 
drawState state@(S c _ b n count) =
    [if y == 41 then '\n' else d y x| x <- [-10..13], y <-[-40..41]] ++ message
    where 
    message = if isWinning state then endScreen count else []
    level@(Level _ maze) = (levels!!n)
    playerPicture = translated (coordX c) (coordY c) (if maze c == Storage then playert else player)
    mazePicture   = pictureOfMaze (addBoxes b (removeBoxes (level)))
    d = (playerPicture & mazePicture) (\x y -> ' ')