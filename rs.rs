#![feature(slicing_syntax)]
#![allow(deprecated)]

extern crate time;
extern crate test;

use time::{precise_time_ns};
use std::{cmp};

use tree::{Node};

mod tree {
    use std::io::{BufferedReader, File};
    use std::{mem};
    use std::cell::{Cell};

    pub struct Node {
        routes: Box<[(*const Node, i32)]>,
        pub visited: Cell<bool>,
    }

    impl Node {
        pub fn routes(&self) -> &[(&Node, i32)] {
            unsafe { mem::transmute(self.routes[]) }
        }
    }

    pub struct Tree {
        nodes: Box<[Node]>,
    }

    impl Tree {
        pub fn nodes(&self) -> &[Node] {
            self.nodes[]
        }
    }

    pub fn read_places() -> Tree {
        let path = Path::new("agraph");
        let mut file = BufferedReader::new(File::open(&path).unwrap());

        let numnodes = match file.read_line() {
            Ok(num) => num[].trim().parse().unwrap(),
            _ => panic!("Error, first line of file should describe the amount of nodes")
        };

        let mut nodes = Vec::from_fn(numnodes, |_| Node {
            routes: vec!().into_boxed_slice(),
            visited: Cell::new(false),
        }).into_boxed_slice();

        let mut vec_nodes = Vec::from_fn(numnodes, |_| vec!());

        while let Ok(line) = file.read_line() {
            let nums: Vec<&str> = line.trim().split(' ').collect();
            let src  = nums[0].parse().expect("Error: node id was not a uint");
            let dest = nums[1].parse().expect("Error: neighbour id was not an int");
            let cost = nums[2].parse().expect("Error: route cost was not an int");
            vec_nodes[src].push((&nodes[dest] as *const Node, cost));
        }

        for (node, vec_node) in nodes.iter_mut().zip(vec_nodes.into_iter()) {
            node.routes = vec_node.into_boxed_slice();
        }

        Tree { nodes: nodes }
    }
}

fn get_longest_path(nodes: &[Node], cur: &Node) -> i32 {
    let mut max = 0;

    cur.visited.set(true);

    for &(neighbor, cost) in cur.routes().iter() {
        if !neighbor.visited.get() {
            let dist = cost + get_longest_path(nodes, neighbor);
            max = cmp::max(max, dist);
        }
    }

    cur.visited.set(false);

    max
}

fn main() {
    let tree = tree::read_places();
    let start_time = precise_time_ns();
    let path = get_longest_path(tree.nodes(), &tree.nodes()[0]);
    let duration = (precise_time_ns() - start_time) / 1000000;
    println!("{} LANGUAGE Rust {}", path, duration);
}
