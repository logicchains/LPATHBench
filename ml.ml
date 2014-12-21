open List
open Unix
open Printf
       
type route = {dest: int; cost: int}

type node = route list

type node2 = int array
		  

let node2_of_route_list node =
  let a = Array.make (2 * (List.length node)) 0 in
  let rec fill i = function
    | [] -> a
    | hd::tl ->
        Array.set a i hd.dest;
        Array.set a (i+1) hd.cost;
        fill (i+2) tl
  in
    fill 0 node

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
      let neighbours = nodes.(nodeID) in
      let dest = neighbours.(i) in
      let cost = neighbours.(i+1) in
      if (not visited.(dest))
      then
	let dist = cost + getLongestPath2 nodes dest visited in
	let newMax = if dist > maxDist then dist else maxDist in
        loop nodes nodeID visited (i-2) newMax
      else
	loop nodes nodeID visited  (i-2) maxDist in
  let (max: int) =
    loop nodes nodeID visited (Array.length nodes.(nodeID) - 2) 0
  in
  visited.(nodeID) <- false;
  max;;
   

let () =
  let (nodes, numNodes) = readPlaces() in
  let visited = Array.init numNodes (fun x -> false) in
  let fstNodes = Array.map node2_of_route_list nodes in
  let start = Unix.gettimeofday() in
  let len = getLongestPath2 fstNodes 0 visited in
  printf "%d LANGUAGE Ocaml %d\n" len (int_of_float @@ 1000. *. (Unix.gettimeofday() -. start))
  (*  print_int @@ getLongestPath nodes 0 visited;*)
