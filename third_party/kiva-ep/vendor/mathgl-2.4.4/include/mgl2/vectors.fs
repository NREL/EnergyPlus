\ Integer vectors library                     Thu Feb 21 12:46:01 MST 2008
\ Copyright (C) 2008, Sergey Plis
\
\ This program is free software; you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation; either version 2 of the License, or
\ (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

\needs float import float

\ vector variables better have names ending with "(" for readability
Module vectors
also float

\ fetches
| create fetch_operations ' c@ , ' w@ , ' @ ,  0 , ' 2@ , ' f@ ,

\ stores
| create store_operations ' c! , ' w! , ' ! ,  0 , ' 2! , ' f! ,
| : type-idx ( cell_size -- idx ) 4 >> ; macro
| : f-op ( cell-size -- cfa ) type-idx cells fetch_operations + @ ;
| : s-op ( cell-size -- cfa ) type-idx cells store_operations + @ ;

: ^)! ( *vector -- addr ) [ 3 cells ] literal - @ ;
: ^)@ ( *vector -- addr ) [ 4 cells ] literal - @ ;

\ number of elements
: )size ( *vector -- size ) [ 1 cells ] literal - @ ;

\ set number of elements - useful for temporal size adjustments in
\ datastructures such as heaps
: )size! ( sz *vector -- ) [ 1 cells ] literal - ! ;

\ size of an element in bytes
: )type ( *vector -- size ) [ 2 cells ] literal - @ ;

: )free ( *vector -- ) [ 4 cells ] literal - free throw ;

\ header | fetch_cfa | store_cfa | el_size | #els |
\ cell-size in bits
\ unnamed vector
: _vector ( n cell-size -- addr )
    2dup * [ 4 cells ] literal + allocate throw
    dup >r   over f-op swap !
    r@ cell+ over s-op swap !
    r@ [ 2 cells ] literal + ! \ cell size store
    r@ [ 3 cells ] literal + ! \ #els store
    r> [ 4 cells ] literal + ;

\ named vector
: vector ( n cell-size -- )
    create
    2dup * [ 4 cells ] literal + allocate throw dup ,
    dup >r   over f-op swap !
    r@ cell+ over s-op swap !
    r@ [ 2 cells ] literal + ! \ cell size store
    r@ [ 3 cells ] literal + ! \ #els store
    r> dup
    \ erasing the content
    [ 2 cells ] literal + @ over [ 3 cells ] literal + @ *
    swap [ 4 cells ] literal + swap erase
  does> @ [ 4 cells ] literal + ;

\ vector of pointers
: vector*  ( # -- *vector ) cell 8 * _vector ;

| : ?idx-in-range ( *vector idx -- 1/0 ) dup rot )size < swap 0>= and ;
| : check-range ( *vector idx -- *vector idx | fail )
    2dup ?idx-in-range not abort" Index is out of range! " ;

\ addr of ith element of the vector
: *)  ( *vector i -- addr ) over )type 3 >> * + ;
: )@ ( *vector index -- )
    [IFDEF] отладка
	check-range
    [THEN]
    over dup ^)@ >r )type 3 >> * + r> execute ;
: )! ( value *vector index -- )
    [IFDEF] отладка
	check-range
    [THEN]
    over dup ^)! >r )type 3 >> * + r> execute ;
\ : test! cell * + ! ;
| create print-funcs ' . , ' . , ' . , 0 , ' d. , ' f. ,
: )print ( *v -- cfa ) )type type-idx cells print-funcs + @ execute ;
: )map ( *v xt -- ) swap dup )size 0 do 2dup  i )@ swap execute loop 2drop ;
: map ( *v -- ) ( word-to-map ) ' swap dup )size 0 do 2dup  i )@ swap execute loop 2drop ;
: )initperm ( v( -- )
    dup )size 0 do
	dup
	i swap over )!
    loop drop ;
: ). ( *vector -- ) dup )size 0 do dup i )@ over )print loop drop ;
\ does arbitrary vector contain this element ?
: )in? ( *v value -- 1/0 )
    swap dup )size 0 do
	2dup i )@ = if 2drop True unloop exit then
    loop 2drop False ;
: )find ( *v value -- i True/False )
    swap dup )size 0 do
	2dup i )@ = if 2drop i True unloop exit then
    loop 2drop False ;
: vector->stack ( *v -- n1 n2 .. n# # )
    dup )size 0 do dup i )@ swap loop )size ;
\ initialized cell vector
\ preserve order
: ivector* ( n1 n2 .. n# # -- *vector )
    dup vector* swap 1- 0 swap do
	swap over i )!
    -1 +loop ;
\ reversed order
: irvector* ( n1 n2 .. n# # -- *vector )
    dup vector* swap 0 do
	swap over i )!
    loop ;
\ does not take care of duplicate elements
| : overlap ( v1( v2( -- n1 .. n2 # / 0 ) depth 2- >r
    dup )size 0 do
	2dup i )@ )in? if
	    dup i )@ -rot
	then
    loop 2drop depth r> - ;
| : notoverlap ( v1( v2( -- n1 .. n2 # )
    depth 2- >r
    dup )size 0 do
	2dup i )@ )in? not if
	    dup i )@ -rot
	then
    loop 2drop depth r> - ;
: )union ( *v1( *v2( -- *v3( )
    over >r
    notoverlap
    r> swap >r vector->stack r> +
    dup 0= abort" empty union!"
    ivector* ;
: )intersection ( *v1( *v2( -- *v3(/0 )
    overlap dup 0<> if ivector* then ;
\ elementwise comparison of two vectors
: )= ( *v1( *v2( -- 1/0 ) dup )size >r over )size r>
    <> if 2drop 0 exit then
    dup )size 0 do
	2dup i )@ swap i )@ <> if
	    2drop unloop 0 exit
	then
    loop 2drop -1 ;
: subset? ( *v( *s( -- 1/0 )
    2dup )intersection dup 0= if -rot 2drop exit then
    dup >r )= swap drop r> )free ;
: )clone ( *v -- *cv )
    vector->stack ivector* ;
: )erase ( *v -- ) dup )size over )type 3 >> * erase ;
: _last ( *v -- *v idx-of-last-element ) dup )size 1- ;

clear
previous
Module;