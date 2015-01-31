USING: arrays grouping.extras io.encodings.utf8 io.files kernel kernel.private
locals math math.integers.private math.parser math.private sequences
sequences.private slots.private splitting tools.time ;
IN: lpath

: parse-graph ( str -- data )
    utf8 file-lines rest [ " " split harvest [ string>number ] map ] map ;

: reshape-graph ( data -- seq )
    [ first ] group-by [ second [ rest ] { } map-as ] { } map-as ;

: read-graph ( str -- G )
    parse-graph reshape-graph ;

: initial-visited ( G -- array )
    length 0 <array> ;

! Helps Factor generate unsafe assembly.
: fast-add ( fix1 fix2 -- fix3 )
    { fixnum fixnum } declare fixnum+fast ; inline

: fast-array-nth ( arr fix -- el )
    { array fixnum } declare swap nth-unsafe ; inline

: fast-first2 ( arr -- el1 el2 )
    { array } declare first2-unsafe ; inline

DEFER: (longest-path)

:: ((longest-path)) ( G visited neighbours len i running-max -- running-max' )
    len i eq? [ running-max ] [
        neighbours i fast-array-nth
        fast-first2 dupd visited rot fast-array-nth 1 eq?
        [ 2drop running-max ] [
            swap G visited rot (longest-path) fast-add running-max fixnum-max
        ] if
        [ G visited neighbours len i 1 fast-add ] dip ((longest-path))
    ] if ; inline recursive

: (longest-path) ( G visited id -- value )
    { array array fixnum } declare
    2dup swap 1 -rot set-array-nth
    3dup pick array-nth dup array-length 0 0 ((longest-path))
    [ swap 0 -rot set-array-nth drop ] dip ;

: longest-path ( G visited -- path-length )
    0 (longest-path) ;

: run-test ( path -- longest nanos )
    read-graph dup initial-visited [ longest-path ] benchmark ;
