open List
open Unix
open Printf
       
type route = {dest: int; cost: int}

type node = route list

type node2 = route array
		  
let readPlaces () =
  let f = open_in "agraph" in
  let n = int_of_string (input_line f) in
  let nodes = Array.init n (fun a -> []) in
  let rec loop () =
    let nums = Str.split (Str.regexp "[ \t]+") @@ input_line f in
    let len = length nums in
    if len = 3 then 
      let (node, neighbour, cost) = (int_of_string (nth nums 0), int_of_string (nth nums 1), int_of_string (nth nums 2)) in
      nodes.(node) <- ({dest= neighbour; cost=cost} :: nodes.(node));
      loop ()
    else ();
  in try
    loop();
    (nodes, n)
  with e ->
    (nodes, n)
      
let rec getLongestPath nodes nodeID visited =
  visited.(nodeID) <- true;
  let max = ref 0 in
  iter (fun neighbour -> if (not (visited.(neighbour.dest)))
			 then (
			   let dist = neighbour.cost + getLongestPath nodes neighbour.dest visited in
			   if dist > !max then max := dist;)
			 else ();)
       nodes.(nodeID);
  visited.(nodeID) <- false;
  !max

let rec getLongestPath2 (nodes: node2 array) nodeID visited =
  visited.(nodeID) <- true;
  let rec loop nodes nodeID visited i maxDist =
    if i < 0 then maxDist
    else
      let neighbour = nodes.(nodeID).(i) in
      if (not visited.(neighbour.dest))
      then
	let dist = neighbour.cost + getLongestPath2 nodes neighbour.dest visited in
	let newMax = if dist > maxDist then dist else maxDist in
        loop nodes nodeID visited (i-1) newMax
      else
	loop nodes nodeID visited (i-1) maxDist in
  let (max: int) =
    loop nodes nodeID visited (Array.length nodes.(nodeID) - 1) 0
  in
  visited.(nodeID) <- false;
  max;;
   

let () =
  let (nodes, numNodes) = readPlaces() in
  let visited = Array.init numNodes (fun x -> false) in
  let fstNodes = Array.map (fun n1 -> Array.of_list n1) nodes in
  let start = Unix.gettimeofday() in
  let len = getLongestPath2 fstNodes 0 visited in
  printf "%d LANGUAGE Ocaml %d\n" len (int_of_float @@ 1000. *. (Unix.gettimeofday() -. start))
  (*  print_int @@ getLongestPath nodes 0 visited;*)
