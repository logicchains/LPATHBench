open List

type route = {dest: int; cost: int}

type node = route list

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

let () =
  let (nodes, numNodes) = readPlaces() in
  let visited = Array.init numNodes (fun x -> false) in
  print_int @@ getLongestPath nodes 0 visited;
