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

fn get_longest_path(nodes: &Vec<Node>, node_id: uint, visited: &mut [bool]) -> i32 {
    visited[node_id] = true;
    let mut max: i32 = 0;
    for neighbour in nodes[node_id].neighbours.iter() {
        let is_visited = visited[neighbour.dest as uint];
        if !is_visited {
            let dist = neighbour.cost + get_longest_path(nodes, neighbour.dest as uint, visited);
            if dist > max{
                max = dist;
            }
        }
    }
    visited[node_id] = false;
    max
}

fn main(){
   let mut num_nodes = 0;
   let nodes = read_places(&mut num_nodes);
   let mut visited: Vec<bool> = Vec::from_fn(num_nodes, |_| false);
   let startTime = precise_time_ns();
   let path = get_longest_path(&nodes, 0, visited.as_mut_slice());
   let duration = (precise_time_ns() - startTime) / 1000000;
   println!("{} LANGUAGE Rust {}", path, duration);
}
