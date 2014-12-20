extern crate time;

use std::io::{BufferedReader, File};
use time::precise_time_ns;

struct Route {
    dest: i32,
    cost: i32,
}

struct Node {
    neighbours: Vec<Route>,
}

fn read_places() -> Vec<Node> {
    let path = Path::new("agraph");
    let mut file = BufferedReader::new(File::open(&path));
    let mut lines = file.lines().map(|x| x.unwrap());

    let numnodes: uint = match lines.next() {
        Some(num) => from_str(num.as_slice().trim()).unwrap(),
        _         => panic!("Error, first line of file should describe the amount of nodes")
    };

    let mut nodes = Vec::from_fn(numnodes, |_| Node { neighbours: Vec::with_capacity(numnodes) });

    for line in lines {
        let nums: Vec<&str> = line.split(' ').collect();
        let node     : uint = from_str(nums[0]       ).expect("Error: node id was not a uint");
        let neighbour: i32  = from_str(nums[1]       ).expect("Error: neighbour id was not an int");
        let cost     : i32  = from_str(nums[2].trim()).expect("Error: route cost was not an int");
        nodes[node].neighbours.push(Route {dest: neighbour, cost: cost});
    }

    return nodes;
}

fn get_longest_path(nodes: &Vec<Node>, node_id: i32, visited: &mut Vec<bool>) -> i32 {
    unsafe {*visited.unsafe_mut(node_id as uint) = true;}
    let mut max = 0i32;

    for neighbour in nodes[node_id as uint].neighbours.iter() {
        if ! unsafe{*visited.unsafe_get(neighbour.dest as uint)} {
            let dist = neighbour.cost + get_longest_path(nodes, neighbour.dest, visited);

            if dist > max {
                max = dist;
            }
        }
    }

    unsafe {*visited.unsafe_mut(node_id as uint) = false;}
    return max;
}

fn main() {
    let nodes = read_places();
    let mut visited = Vec::from_elem(nodes.len(), false);
    let startTime = precise_time_ns();
    let path = get_longest_path(&nodes, 0, &mut visited);
    let duration = (precise_time_ns() - startTime) / 1000000;
    println!("{} LANGUAGE Rust {}", path, duration);
}

