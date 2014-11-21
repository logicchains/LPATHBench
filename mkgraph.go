package main

import (
	"fmt"
	"flag"
	"math/rand"
	"math"
	"time"
)

type dist struct{
	to int32
	cost int32
}

type place struct{
	x int32
	y int32
	neighbours []dist
}

func calcDist(p1, p2 *place)int32{
	return int32(math.Sqrt(float64((p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y))))
}

func makeRoutes(ps []place, r *rand.Rand){
	for i := range ps{
		p := &ps[i]
		others := r.Perm(len(ps))
		percentageToConnectTo := r.Float32()
		next := i + 1
		if next == len(ps){
			next = 0
		}
		p.neighbours = append(p.neighbours, dist{to: int32(next), cost: calcDist(p, &ps[next])})
		for _, o := range others{
			var newNeighbour dist			
			if r.Float32() < percentageToConnectTo && o != next && o != i{
				newNeighbour = dist{to: int32(o), cost: calcDist(p, &ps[o])}
			}else{
				newNeighbour = dist{-1,-1}
			}
			p.neighbours = append(p.neighbours, newNeighbour)				
		}
	}
}

func main(){
	nPlaces := flag.Int("places", 10, "The number of place nodes in the graph")
	worldsize := flag.Int("worldsize", 100, "The size of the world")
	flag.Parse()
	places := make([]place,0,*nPlaces)
	for i := 0; i < *nPlaces; i++ {
		places = append(places, place{x: rand.Int31n(int32(*worldsize)), y: rand.Int31n(int32(*worldsize))})
	}
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	makeRoutes(places, r)
	fmt.Println(*nPlaces)
	for i, p := range places{
		for _, n := range p.neighbours{
			if n.cost >= 0{
				fmt.Printf("%d %d %d \n",i, n.to, n.cost)
			}
		}
	}
}
