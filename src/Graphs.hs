module Graphs where

findAllReachable :: Eq a => (a -> [a]) -> [a] -> [a] -> [a]
findAllReachable _ [] _ = []
findAllReachable neighbours (h:t) visited =
     (h:unvisitedNeighbours) ++ (findAllReachable neighbours nextQueue visited') 
  where
    nextQueue = t           ++ unvisitedNeighbours
    visited'  = (h:visited) ++ unvisitedNeighbours
    unvisitedNeighbours = filter (\x -> not (elem x (h:visited))) (neighbours h)
    
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  all isOk (findAllReachable neighbours [initial] [])

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable target initial neighbours =
  elem target (findAllReachable neighbours [initial] [])

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
  foldr f True vs
  where f v result = result && (reachable v initial neighbours)