{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
import Data.Vector as V
import Data.Vector.Mutable as MV
import Data.Vector.Unboxed.Mutable as UMV
import Data.List as L
import Data.Int
import Data.IORef

data Route = Route {dest:: !Int32, cost:: !Int32}
             deriving Show
           
type Node = [Route]

accumRoutes :: V.Vector Node -> String -> V.Vector Node
accumRoutes nodes ln =
    let nums = words ln
        (node, neighbour, cost) = (Prelude.read (nums !! 0), Prelude.read (nums !! 1), Prelude.read (nums !!2))
    in
      V.modify (\v -> do
                    let r = Route{dest=neighbour, cost=cost}
                    old <- MV.read v node
                    MV.write v node (r:old)) nodes
    
readPlaces :: [String] -> (V.Vector Node, Int32)
readPlaces (x:xs) =
    let numNodes = Prelude.read x
        nodes = V.generate numNodes (\ _ -> [])
    in
      (L.foldl' accumRoutes nodes xs, fromIntegral numNodes)
          
getLongestPath :: V.Vector Node -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPath !nodes !nodeID !visited = do
  ({-# SCC "writevisited" #-} UMV.unsafeWrite visited (fromIntegral nodeID) True)
  max <- {-# SCC "newioref" #-} newIORef 0
  Prelude.mapM_  (\ Route{dest, cost} -> do
             isVisited <- {-# SCC "readvisited" #-} UMV.unsafeRead visited (fromIntegral dest)
             case isVisited of
               True -> return ()
               False -> do
                   dist <- {-# SCC "getNwPath" #-} fmap (+ cost) $ getLongestPath nodes dest visited
                   maxVal <- {-# SCC "readmaxagain" #-} readIORef max
                   ( {-# SCC "changedist" #-} if dist > maxVal then writeIORef max dist else return ()))
         (nodes ! (fromIntegral nodeID))
  ({-# SCC "writeunvisited" #-}  UMV.write visited (fromIntegral nodeID) False)
  ({-# SCC "returnmax" #-} readIORef max)

main :: IO()
main = do   
  content <- readFile "agraph"
  let (nodes, numNodes) = readPlaces $ lines content
  visited <- UMV.replicate (fromIntegral numNodes) False
  getLongestPath nodes 0 visited >>= print
