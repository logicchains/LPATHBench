module LongestPath

open System
open System.IO

type Route = 
    struct 
        val Dest : int
        val Cost : int
        new (dest : int, cost : int) = { Dest = dest; Cost = cost}
    end
          
type Node = Route array

let readPlaces () =    
  let lines = 
    use stream = File.OpenRead(@"agraph")
    use reader = new StreamReader(stream)
    (reader.ReadToEnd()).Split ([|"\n"|], System.StringSplitOptions.None)
  let numNodes = lines.[0] |> int
  let nodes = Array.create numNodes [||]
  lines.[1..]
        |> Array.map(fun l -> l.Split ([|" "|], System.StringSplitOptions.None)) 
        |> Array.filter (fun l -> l.Length > 2) 
        |> Array.fold(fun (acc : Node[]) v ->
            let node, neighbour, cost = (int v.[0], int v.[1], int v.[2])
            acc.[node] <- Array.append [| Route(neighbour, cost) |] acc.[node]
            acc) nodes

let rec getLongestPath (nodes : Node array) nodeId (visited : bool array) =
  visited.[nodeId] <- true;
  let mutable maxcost = 0
  for neighbour in nodes.[nodeId] do     
    if not visited.[neighbour.Dest] then
      let dist = neighbour.Cost + getLongestPath nodes neighbour.Dest visited
      if dist > maxcost then maxcost <- dist
  visited.[nodeId] <- false;
  maxcost

[<EntryPoint>]
let main argv = 
  let nodes = readPlaces()
  let visited = Array.zeroCreate nodes.Length
  let sw = System.Diagnostics.Stopwatch.StartNew()
  let len = getLongestPath nodes 0 visited
  sw.Stop()
  let duration = sw.ElapsedMilliseconds
  printf "%d LANGUAGE FSharp %d\n" len duration
  0
