extern crate time;

use std::io::File;
use std::io::BufferedReader;
use time::precise_time_ns;

#[deriving(Clone, Show)]
struct Route{
  dest: i32,
  cost: i32
}

#[deriving(Clone, Show)]
struct Node {
  neighbours: Vec<Route>
}

fn read_places(num_nodes: &mut uint) ->Vec<Node>{
  let path = Path::new("agraph");
  let mut file = BufferedReader::new(File::open(&path));
  let lines: Vec<String> = file.lines().map(|x| x.unwrap()).collect();
  *num_nodes = from_str(lines[0].as_slice().trim()).expect("Error, first line of file should describe the amount of nodes");
  let mut nodes: Vec<Node> = Vec::from_fn(*num_nodes, |_| Node{neighbours: vec![]});
  for ln in lines.slice(1,lines.len()).iter(){
    let nums: Vec<&str> = ln.as_slice().split(' ').collect();
    if nums.len() < 3{
      break;
    }
    let node: uint = from_str(nums[0]).expect("Error: node id was not a uint");
    let neighbour: i32 = from_str(nums[1]).expect("Error: neighbour id was not an int");
    let cost: i32 = from_str(nums[2]).expect("Error: route cost was not an int");
    let mut newnode = nodes[node].clone();
    newnode.neighbours.push(Route{dest: neighbour, cost: cost});
    let mut newnodes = Vec::with_capacity(*num_nodes);
    for i in range(0u, *num_nodes){
      newnodes.push(if i == node{ newnode.clone()} else {nodes[i].clone()});
    }
    nodes = newnodes;
  }
  nodes
}

fn get_longest_path(nodes: &Vec<Node>, nodeID: i32, visited: *const bool) -> i32{
  let uintID = nodeID.to_uint().expect("Error, something had a negative node id");
  unsafe{  
    let newAddr: uint = (visited as uint) + (uintID as uint);
    let newAddrP: *mut bool = newAddr as *mut bool;
    *newAddrP = true;
  }
  let mut dist: i32;
  let mut max: i32 = 0;
  for neighbour in nodes[uintID].neighbours.iter(){
    let udest = neighbour.dest.to_uint().expect("Almost done..");
    let isVisited: bool;
    unsafe{
      let newAddr: uint = udest + (visited as uint);
      let newAddrP: *mut bool = newAddr as *mut bool;
      isVisited = *newAddrP
    }
    if !isVisited{
      dist = neighbour.cost + get_longest_path(nodes, neighbour.dest, visited);
      if dist > max{
	max = dist;
      }
    }    
  }
  unsafe{  
    let newAddr: uint = (visited as uint) + (uintID as uint);
    let newAddrP: *mut bool = newAddr as *mut bool;
    *newAddrP = false;
  }
  max
}

fn main(){
   let mut num_nodes = 0;
   let nodes = read_places(&mut num_nodes);
   let mut visited: Vec<bool> = Vec::from_fn(num_nodes, |_| false);
   let startTime = precise_time_ns();
   let path = get_longest_path(&nodes, 0, &visited[0] as *const bool);
   let duration = (precise_time_ns() - startTime) / 1000000;
   println!("{} Language Rust {}", path, duration);
}