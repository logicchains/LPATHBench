module lpath

open System
open System.IO

type route = {dest: int; cost: int}

type node = route list

let readPlaces () =
  use stream = File.OpenRead(@"agraph")
  use reader = new StreamReader(stream)
  let lines = (reader.ReadToEnd()).Split ([|"\n"|], System.StringSplitOptions.None)
  let numNodes = int <| Seq.head lines
  let nodes = Array.init numNodes (fun a -> [])
  let rec loop i =
    let nums = lines.[i].Split ([|" "|], System.StringSplitOptions.None)
    let len = Array.length nums 
    if len > 2 then
      let node, neighbour, cost = (int nums.[0], int nums.[1], int nums.[2])
      nodes.[node] <- ({dest= neighbour; cost=cost} :: nodes.[node]);
      loop (i + 1)
    else ();
  loop 1;
  nodes

let rec getLongestPath (nodes: node array) nodeID (visited: bool array) =
  visited.[nodeID] <- true;
  let max = ref 0
  List.iter (fun neighbour -> if (not visited.[neighbour.dest])
                                  then (
                                        let dist = neighbour.cost + getLongestPath nodes neighbour.dest visited;
                                        if dist > !max then max := dist;)
                                  else ();)
      nodes.[nodeID]
  visited.[nodeID] <- false;
  !max

let () =
  let nodes = readPlaces()
  let visited = Array.init (Array.length nodes) (fun x -> false)
  Console.WriteLine(getLongestPath nodes 0 visited);
