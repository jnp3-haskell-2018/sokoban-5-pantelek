module Levels where

import Types

levels :: [Level]
levels = [level1, level2, level3, level4]
badLevels :: [Level]
badLevels = [badLevel1, badLevel2, badLevel3, badLevel4, badLevel5, badLevel6, badLevel7]

level1, level2, level3, level4 :: Level
level1 = Level (C 0 1) level
    where level (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x ==  2 && y <= 0        = Wall
            | x ==  3 && y <= 0        = Storage
            | x >= -2 && y == 0        = Box
            | otherwise                = Ground
            
level2 = Level (C 0 1) level
    where level (C x y)
            | abs x > 3  || abs y > 2  = Blank
            | abs x == 3 || abs y == 2 = Wall
            | x == 2     || y == 1     = Storage
            | abs x == 2 || abs y == 1 = Ground
            | abs x == 1               = Wall
            | otherwise                = Box

level3 = Level (C 1 (-1)) level
    where level (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x == -1 && y > -3        = Wall
            | x >= 2 && y <= 0         = Wall
            | x == 0 && y == 3         = Storage
            | x == 1 && y == -3        = Storage
            | x == 1 && y == -2        = Box
            | x == -2 && y == 1        = Box
            | otherwise                = Ground
    
level4 = Level (C (-1) 0) level
    where level (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x == 2 && abs y == 1     = Wall
            | x == 3 && abs y <= 1     = Storage
            | x == 1 && abs y <= 1     = Box
            | otherwise                = Ground

badLevel1, badLevel2, badLevel3, badLevel4, badLevel5, badLevel6, badLevel7 :: Level
badLevel1 = Level (C 0 (-2)) level
    where level (C x y)
            | abs x > 3  || abs y > 3  = Blank
            | abs x == 3 || abs y == 3 = Wall
            | abs x == 2 && y == 0     = Box
            | abs x == 2 || abs y == 2 = Ground
            | x == 0 && y >= 0         = Storage
            | otherwise                = Wall


badLevel2 = Level (C 0 (-2)) level
    where level (C x y)
            | abs x > 3  || abs y > 4  = Blank
            | x == 0     && y == 4     = Box
            | abs x == 3 || abs y == 4 = Wall
            | abs x == 2 && y <= -2    = Storage
            | abs x == 1 && y == -1    = Storage 
            | abs x <= 1 && y <= -2    = Box
            | otherwise                = Ground

badLevel3 = Level (C 0 0) level
    where level (C x y)
            | abs x > 4  || abs y > 1  = Blank
            | abs x == 4 || abs y == 1 = Wall
            | abs x == 2 &&     y == 0 = Box
            |     x == 3 &&     y == 0 = Storage 
            | otherwise                = Ground
            
badLevel4 = Level (C 0 0) level
    where level (C x y)
            |     x == 4 &&     y == 1      = Ground
            |     x == 4 && abs y < 4       = Wall 
            | abs x > 3  || abs y > 3       = Blank
            |     x == 3 && y < 3 && y >= 0 = Ground
            | abs x == 3  || abs y == 3     = Wall
            | abs x == 2 &&  abs y == 1 && x * y > 0  = Box
            |     x < 0 &&       y == -2    = Storage 
            | otherwise                     = Ground
    
badLevel5 = Level (C (-3) 2) level 
    where level (C x y)
            | abs x > 4  || abs y > 4        = Blank
            | abs x == 4 || abs y == 4       = Wall
            | x >= 3                         = Wall
            | x == 2 && y >= 0               = Wall
            | x == 1 && y >= 1               = Wall
            | x <= -1 && y == 1              = Wall
            | x == -3 && y == 0              = Wall
            | (x == -1 || x == 0) && y == -1 = Wall
            | x <= 0 && y == -3              = Wall
            | x == 1 && y == -1              = Storage
            | x == -2 && y == 2              = Box
            | otherwise                      = Ground
    
badLevel6 = Level (C 1 (-1)) level
    where level (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x ==  2 && y <= 0        = Wall
            | x ==  3 && y == 0        = Wall
            | x ==  3 && y <= -1       = Storage
            | x >= -2 && y == 0        = Box
            | otherwise                = Ground
    
badLevel7 = Level (C 1 (-1)) level
    where level (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | x ==  2 && y <= 0        = Wall
            | x ==  3 && y <= 0        = Storage
            | x >= -2 && y == 0        = Box
            | otherwise                = Ground