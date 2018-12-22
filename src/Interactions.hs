module Interactions where

import Types
import Levels
import MazeOps
import Draw

resettable :: Interaction s -> Interaction s
resettable (Interaction state0 handle drawS) = 
  Interaction state0 handle' drawS
  where handle' (KeyPress key) _ | key == "\ESC" = state0
        handle' e s = handle e s

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle drawS) = Interaction state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = drawS s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 handle drawS)
  = Interaction state0'  handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = drawS s

startScreen :: Screen
startScreen = " GoodMazes  BadMazes\n" ++ 
              [if y == 11 then '\n' else d y x | x <- [-5..5], y <-[-10..11]] 
                where
                d = (translated 5 0 (pictureOfBools(map check badLevels))
                    & translated (-5) 0 (pictureOfBools(map check levels))) (\x y -> ' ')
                check maze = isSane maze && isClosed maze

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral (div k 2)) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                      (-fromIntegral (i `div` k))
                      (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  (\d -> addCharAtInitialPos d 'o')
        pictureOfBool False = (\d -> addCharAtInitialPos d 'x')