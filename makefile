NUM_NODES = 10
WORLD_SIZE = 1000

runall:	
	./fs.exe
	./cpp
	./rkt
	mono ./cs.exe
	java jv
	./hs
	./ml
	./lisp
	./rs
	./go
	./d
	./nim
	luajit lj.lua

buildall: fsharp cpp racket csharp java haskell ocaml lisp rust go d

fsharp: fs.fs
	fsharpc fs.fs

cpp: cpp.cpp
	g++ cpp.cpp -std=c++11 -Wall -O3 -o cpp

racket: rkt.rkt
	raco exe rkt.rkt

csharp: cs.cs
	mcs cs.cs

java: jv.java
	javac jv.java

haskell: hs.hs
	ghc hs.hs -O3

haskellprof: hs.hs
	ghc hs.hs -O3 -prof -fprof-auto -caf-all -fforce-recomp -rtsopts

ocaml: ml.ml
	ocamlfind ocamlopt -linkpkg -package str,unix -noassert -unsafe -fno-PIC -nodynlink -inline 100 -o ml ml.ml

lisp: lisp.lisp
	sbcl --load lisp.lisp --non-interactive

rust: rs.rs
	rustc rs.rs --opt-level=3

go: go.go
	go build go.go

d: d.d
	ldc2 d.d -ofd -O3 -release -inline
	
nim: nim.nim
	nim c -d:release nim.nim

graphbuilder: mkgraph.go
	go build mkgraph.go

graph: graphbuilder
	./mkgraph -places=$(NUM_NODES) -worldsize=$(WORLD_SIZE) > agraph
