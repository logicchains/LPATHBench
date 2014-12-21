type route = {
  dest: int;
  cost: int;
}

let build_array node =
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
  let next_int () = Scanf.fscanf f " %d" (fun n -> n) in
  let n = next_int () in
  let nodes = Array.make n [] in
  begin
    try while true do
      let node = next_int () in
      let dest = next_int () in
      let cost = next_int () in
      nodes.(node) <- ({dest; cost} :: nodes.(node));
    done with _ -> ()
  end;
  let nodes = Array.map build_array nodes in
  (nodes, n)

let rec getLongestPath nodes nodeID visited =
  visited.(nodeID) <- true;
  let rec loop nodes nodeID visited i maxDist =
    if i < 0 then maxDist
    else
      let neighbour = nodes.(nodeID) in
      let dest = neighbour.(i) in
      let cost = neighbour.(i+1) in
      if visited.(dest)
      then loop nodes nodeID visited (i-2) maxDist
      else
        let dist = cost + getLongestPath nodes dest visited in
	let newMax = if dist > maxDist then dist else maxDist in
        loop nodes nodeID visited (i-2) newMax
  in
  let (max: int) =
    loop nodes nodeID visited (Array.length nodes.(nodeID) - 2) 0
  in
  visited.(nodeID) <- false;
  max;;

let () =
  let (nodes, numNodes) = readPlaces() in
  let visited = Array.init numNodes (fun x -> false) in
  let start = Unix.gettimeofday() in
  let len = getLongestPath nodes 0 visited in
  Printf.printf "%d LANGUAGE Ocaml %d\n"
    len (int_of_float @@ 1000. *. (Unix.gettimeofday() -. start))
