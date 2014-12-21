type route = {
  dest: int;
  cost: int;
}

let readPlaces () =
  let f = open_in "agraph" in
  let next_int () = Scanf.fscanf f " %d" (fun n -> n) in
  let n = next_int () in
  let nodes = Array.init n (fun a -> []) in
  begin
    try while true do
      let node = next_int () in
      let dest = next_int () in
      let cost = next_int () in
      nodes.(node) <- ({dest; cost} :: nodes.(node));
    done with _ -> ()
  end;
  (Array.map Array.of_list nodes, n)

let rec getLongestPath nodes nodeID visited =
  visited.(nodeID) <- true;
  let rec loop nodes nodeID visited i maxDist =
    if i < 0 then maxDist
    else
      let {dest; cost} = nodes.(nodeID).(i) in
      if visited.(dest)
      then loop nodes nodeID visited (i-1) maxDist
      else
        let dist = cost + getLongestPath nodes dest visited in
	let newMax = if dist > maxDist then dist else maxDist in
        loop nodes nodeID visited (i-1) newMax
  in
  let (max: int) =
    loop nodes nodeID visited (Array.length nodes.(nodeID) - 1) 0
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
