{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad as CM
import Data.Vector as V
import Data.Vector.Mutable as MV
import Data.Vector.Generic as GV
import Data.Vector.Unboxed.Mutable as UMV
import Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Deriving as DU
import Data.List as L
import Data.Int
import Data.IORef
import Data.Time.Clock.POSIX
import Text.Printf

data Route = Route {dest:: !Int32, cost:: !Int32}
             deriving Show

DU.derivingUnbox "Route"
      [t| Route -> (Int32, Int32)|]
      [| \Route{dest,cost} -> (dest,cost)|]
      [| \(dest,cost) -> Route{dest,cost}|]
                      
type Node = [Route]

type Node2 = UV.Vector Route

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
         (nodes V.! (fromIntegral nodeID))
  ({-# SCC "writeunvisited" #-}  UMV.write visited (fromIntegral nodeID) False)
  ({-# SCC "returnmax" #-} readIORef max)

getLongestPathImperative :: V.Vector Node -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPathImperative !nodes !nodeID !visited = do
  UMV.write visited (fromIntegral nodeID) True
  max <- newIORef 0
  Prelude.mapM_  (\ Route{dest, cost} -> do
             isVisited <- UMV.read visited (fromIntegral dest)
             case isVisited of
               True -> return ()
               False -> do
                   dist <- fmap (+ cost) $ getLongestPath nodes dest visited
                   maxVal <- readIORef max
                   if dist > maxVal then writeIORef max dist else return ())
         (nodes V.! (fromIntegral nodeID))
  UMV.write visited (fromIntegral nodeID) False
  readIORef max
  
getLongestPath2 :: V.Vector Node2 -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPath2 !nodes !nodeID !visited = do
  UMV.unsafeWrite visited (fromIntegral nodeID) True
  max <- loop (fromIntegral (UV.length (nodes V.! (fromIntegral nodeID)) - 1)) (0::Int32)
  UMV.write visited (fromIntegral nodeID) False
  return max
    where
      loop :: Int32 -> Int32 -> IO (Int32)
      loop !i !maxDist = if i < 0 then return maxDist
                         else do
                             let route = (nodes V.! (fromIntegral nodeID)) UV.! (fromIntegral i)
                             isVisited <- UMV.unsafeRead visited (fromIntegral (dest route))
                             case isVisited of
                               True -> loop (i-1) maxDist
                               False -> do
                                 dist <- fmap (+ (cost route)) $ getLongestPath2 nodes (dest route) visited
                                 let newMax = if dist > maxDist then dist else maxDist
                                 loop (i-1) newMax

getLongestPath3 :: V.Vector Node -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPath3 !nodes !nodeID !visited = do
  UMV.unsafeWrite visited (fromIntegral nodeID) True
  max <- loop (nodes V.! (fromIntegral nodeID)) (0::Int32)
  UMV.write visited (fromIntegral nodeID) False
  return max
    where
      loop :: [Route] -> Int32 -> IO (Int32)
      loop [] !maxDist = return maxDist
      loop !(Route{dest,cost}:neighbours) !maxDist = do
          isVisited <- UMV.unsafeRead visited (fromIntegral dest)
          newMax <- case isVisited of
                         True -> return maxDist
                         False -> do
                           dist <- fmap (+ cost) $ getLongestPath3 nodes dest visited
                           return $ if dist > maxDist then dist else maxDist
          loop neighbours newMax

getLongestPath4 :: V.Vector Node2 -> Int32 -> UMV.IOVector Bool -> IO (Int32)
getLongestPath4 !nodes !nodeID !visited = do
  UMV.write visited (fromIntegral nodeID) True
  max <- GV.foldM' acc (0::Int32) (nodes V.! (fromIntegral nodeID))
  UMV.write visited (fromIntegral nodeID) False
  return max
    where
      acc :: Int32 -> Route -> IO (Int32)
      acc maxDist Route{dest,cost}  = do
          isVisited <- UMV.read visited (fromIntegral dest)
          case isVisited of
            True -> return maxDist
            False -> do
              dist <- fmap (+ cost) $ getLongestPath4 nodes dest visited
              return $ if dist > maxDist then dist else maxDist

getLongestPath5 :: V.Vector Node2 -> Int32 -> UV.Vector Bool -> (Int32, UV.Vector Bool)
getLongestPath5 !nodes !nodeID !visited = 
  let visited' :: UV.Vector Bool = GV.modify (\v -> UMV.write v (fromIntegral nodeID) True) visited
      (max, visited'') :: (Int32, UV.Vector Bool) = GV.foldl' acc (0::Int32, visited') (nodes V.! (fromIntegral nodeID))
  in (max, GV.modify (\v -> UMV.write v (fromIntegral nodeID) False) visited'')
    where
      acc :: (Int32, UV.Vector Bool) -> Route -> (Int32, UV.Vector Bool)
      acc (maxDist, lVisited) Route{dest,cost}  = 
          case lVisited UV.! (fromIntegral dest) of
            True -> (maxDist, lVisited)
            False ->
                let (dist, lVisited') = getLongestPath5 nodes dest lVisited
                    totDist = cost + dist
                in ((if totDist > maxDist then totDist else maxDist), lVisited')

              
lPathFun :: V.Vector Node -> Int32 -> UMV.IOVector Bool -> IO (Int32)
lPathFun !nodes !nodeID !visited = do
  UMV.write visited (fromIntegral nodeID) True
  max <- CM.foldM acc (0::Int32) (nodes V.! (fromIntegral nodeID))
  UMV.write visited (fromIntegral nodeID) False
  return max
    where
      acc :: Int32 -> Route -> IO (Int32)
      acc maxDist Route{dest,cost}  = do
          isVisited <- UMV.read visited (fromIntegral dest)
          case isVisited of
            True -> return maxDist
            False -> do
              dist <- fmap (+ cost) $ lPathFun nodes dest visited
              return $ if dist > maxDist then dist else maxDist
              
main :: IO()
main = do   
  content <- readFile "agraph"
  let (nodes, numNodes) = readPlaces $ lines content
  visited <- UMV.replicate (fromIntegral numNodes) False             
  let !nodes2 = V.map UV.fromList nodes
  --lPathFun nodes 0 visited >>= print
  !start <- getPOSIXTime
  !len <- getLongestPath4 nodes2 0 visited
  !end <- getPOSIXTime
  printf "%d LANGUAGE Haskell %d\n" len (round $ 1000 * (end - start)::Int)
  --let (cost, _) = getLongestPath5 nodes2 0 (UV.replicate (fromIntegral numNodes) False)
  --print cost
  --getLongestPath nodes 0 visited >>= print
