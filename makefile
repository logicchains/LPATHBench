NUM_NODES = 10
WORLD_SIZE = 1000

cpp: cpp.cpp
	g++ cpp.cpp -std=c++11 -Wall -O3 -o cpp

racket: rkt.rkt
	raco exe rkt.rkt

csharp: cs.cs
	mcs cs.exe

java: jv.java
	javac jv.java

haskell: hs.hs
	ghc hs.hs -O3

haskellprof: hs.hs
	ghc hs.hs -O3 -prof -fprof-auto -caf-all -fforce-recomp -rtsopts

ocaml: ml.ml
	ocamlfind ocamlopt -linkpkg -package str -noassert -unsafe -fno-PIC -nodynlink -inline 100 -o ml ml.ml

rust: rs.rs
	rustc rs.rs --opt-level=3

go: go.go
	go build go.go

dmd: d.d
	dmd d.d -ofdmdd -O -release -inline

graphbuilder: mkgraph.go
	go build mkgraph.go

graph: graphbuilder
	./mkgraph -places=$(NUM_NODES) -worldsize=$(WORLD_SIZE) > agraph
