module Main where

import Draw
import Types    
import Interactions
import States
import System.IO

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state step draw) = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    putStr "\ESCc"
    putStrLn (draw state)
    go state
    where
        go state' = do
          input <- getChar
          let state'' = step (KeyPress [input]) state'
          putStr "\ESCc"
          putStrLn(draw state'')
--          putStrLn $ "You pressed: " ++ show input
          go state''

main :: IO ()
main = runInteraction (withUndo (withStartScreen (resettable (Interaction (createInitialState 1) handleEvent drawState))))


