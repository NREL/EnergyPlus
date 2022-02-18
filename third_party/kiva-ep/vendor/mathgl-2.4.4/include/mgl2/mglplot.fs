\ Mathgl library wrapper                      Thu Feb 21 12:33:02 MST 2008
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

\needs gsl     include gsl.fs
\needs mathgl  include mathgl.fs
\needs vectors include vectors.fs
\needs $!      include string.fs

Module mglplot

also minos also vectors also gsl also mathgl also float

\ ------------------------------------------------------------------------
\                               structures
\ ------------------------------------------------------------------------
struct{
    cell transp?
    cell transp_type
    cell alpha?
    cell light
    cell box
    cell grid
    double ambient
    cell axis?
    cell axis_str
    cell rotation[
    cell aspect[
    cell colorbar
    double linewidth
    double fontsize
    cell labelx$
    cell labely$
    cell labelz$
    cell textrotate
} mgl-params

struct{
    cell next      \ next plot in the list of current figure
    cell prev
    cell xrange    \
    cell yrange    \
    cell zrange    \
    cell hmdtlist  \ vector of all data objects used in the plot
    cell params    \ vector of parameters to the plotting function
    cell ops       \ vector of xt's of operations to place params
                   \ list on the stack so the plotting function can
                   \ be executed
    cell plotfunc  \ the plotting function
} a_plot

struct{
    cell next      \ next figure in the figure structure
    cell #
    cell active?   \ True/False
    cell figure    \ plot object
} a_figure

: dispose-hmdtlist ( v( -- )
    dup )size 0 do
	dup i )@
	mgl_delete_data
    loop drop ;

: dispose-a-plot ( *a_plot -- )
    dup a_plot hmdtlist @ dispose-hmdtlist
    dup a_plot params @ )free
    dup a_plot ops @ )free
    dup a_plot xrange @ ]free
    dup a_plot yrange @ ]free
    dup a_plot zrange @ ]free
    free throw ;

: dispose-all-plots ( *a_plot -- )
    dup begin while
	    dup a_plot next @
	    swap dispose-a-plot
	    dup
    repeat drop ;

\ simple aux words
| : ]@s ( x[ -- f: x[0] .. x[n] )
    dup ]size 0 do dup i ]@ loop drop ;
| : ]!s ( x[ f: x[0] .. x[n] -- )
    dup ]size 0 do dup i ]! loop drop ;
| create axis-param 0 c, 0 c, 0 c, 0 c, 0 c,
| create mglstrbuff $FF allot
: default-rotation ( -- r[ ) 3 :] dup 0e 0e 0e ]!s ;
: clear-axis-param
    axis-param dup 'x swap c! 1+ dup 'y swap c! 1+ 'z swap c! ;

: NAN ( -- f: NAN) $7FC00000 sp@ sf@ drop ;
\ ------------------------------------------------------------------------
\                                 GUI
\ ------------------------------------------------------------------------

component class mathplot
  private:
    method fill-pixmap
    method mgl_settings_reset
    cell var mhold
  public:
    early open
    early dialog
    early open-app

    method generate-plot
    method addplot
    method clear-pixmap
    method #plots
    method clf
    method mgl-rotation!
    method mgl-colorbar

    method mgl-hold
    method mgl-holdoff
    method mgl-holdon
    method hold@
    method hold!
    method mgl-grid
    method fontsize

    method mgl-xlabel
    method mgl-ylabel    
    method mgl-zlabel
    
    canvas ptr mCanvas
    ( [varstart] )
    \ memory-pixmap ptr pxmap \ the pixmap on the canvas
    cell var pxmap
    cell var graph         \ the graph to display
    cell var plotlist      \ list of plots to display
    cell var parameters    \ parameter structure
    ( [varend] )
  how:
    : params   DF[ 0 ]DF s" figure" ;
    : hold@ mhold @ ;
    : hold! mhold ! ;    
    : mgl-hold mhold @ not mhold ! ;
    : mgl-holdoff mhold off ;
    : mgl-holdon mhold on ;
    
    : open     self DF[ 0 ]DF s" bigforth Plot" open-component ;
    : dialog   new  DF[ 0 ]DF s" bigforth Plot" open-dialog ;
    : open-app new  DF[ 0 ]DF s" bigforth Plot" open-application ;
    : clear-pixmap pxmap @ if pxmap @ >o dispose o> 0 pxmap ! then ;
    : #plots ( -- )
	    0
	    plotlist @ begin dup while
		    swap 1+ swap
		    a_plot next @
		    repeat
	    drop ;

    : assign
	&600 &400  mgl_create_graph_zb graph !
	sizeof mgl-params allocate throw parameters !
	parameters @ sizeof mgl-params erase
	mgl_settings_reset
	
	\ 0 0
	parameters @ mgl-params labelx$ dup @ if $off else drop then
	\ 0 0
	parameters @ mgl-params labely$ dup @ if $off else drop then
	\ 0 0
	parameters @ mgl-params labelz$ dup @ if $off else drop then
	
	0 pxmap !
	0 plotlist !
	mhold off ;

    : fontsize ( f: size -- ) parameters @ mgl-params fontsize sf! ;
    
    : mgl-set-params ( -- )
	graph @
	dup parameters @ mgl-params rotation[ @ ]@s mgl_rotate
	dup parameters @ mgl-params ambient     sf@ mgl_set_ambbr
	dup parameters @ mgl-params fontsize    sf@ mgl_set_font_size
	dup parameters @ mgl-params linewidth   sf@ mgl_set_base_line_width
	dup parameters @ mgl-params aspect[   @ ]@s mgl_aspect
	dup parameters @ mgl-params transp?       @ mgl_set_transp
	dup parameters @ mgl-params transp_type   @ mgl_set_transp_type
	dup parameters @ mgl-params alpha?        @ mgl_set_alpha
	dup parameters @ mgl-params light         @ mgl_set_light
	dup parameters @ mgl-params textrotate    @ mgl_set_rotated_text
	dup parameters @ mgl-params box           @ mgl_box
	    parameters @ mgl-params grid @ if
	    dup 0" xyz" 0 mgl_axis_grid
	then
	drop ;
    : mgl-post-params ( -- )
	graph @ 1 mgl_set_rotated_text 
	parameters @ mgl-params labelx$ @ if
	    parameters @ mgl-params labelx$ $@ mglstrbuff 0place
	    parameters @ mgl-params fontsize sf@ 0e0	    
	    graph @ 'x mglstrbuff 0 mgl_label_ext
	then 
	parameters @ mgl-params labely$ @ if
	    parameters @ mgl-params labely$ $@ mglstrbuff 0place
	    parameters @ mgl-params fontsize sf@ 0e0	    
	    graph @ 'y mglstrbuff 0 mgl_label_ext	    
	then
	parameters @ mgl-params labelz$ @ drop 0 if
	    parameters @ mgl-params labelz$ $@ mglstrbuff 0place
	    parameters @ mgl-params fontsize sf@ 0e0	    
	    graph @ 'z mglstrbuff 0 mgl_label_ext	    	    
	then
	parameters @ mgl-params colorbar @ drop 0 if
	    dup 0 0 mgl_colorbar
	then ;
    : mgl_settings_reset
	1 parameters @ mgl-params transp? !
	1 parameters @ mgl-params transp_type !
	0 parameters @ mgl-params alpha? !
	0 parameters @ mgl-params box !
	0 parameters @ mgl-params colorbar !
	0 parameters @ mgl-params light !
	1 parameters @ mgl-params axis? !
	axis-param parameters @ mgl-params axis_str !
	clear-axis-param
	parameters @ mgl-params labelx$ dup @ if $off else drop then
	parameters @ mgl-params labely$ dup @ if $off else drop then
	parameters @ mgl-params labelz$ dup @ if $off else drop then
	\ parameters @ mgl-params textrotate off
	!0.9 parameters @ mgl-params ambient sf!
	!2.5 parameters @ mgl-params fontsize sf!
	!1.2 parameters @ mgl-params linewidth sf!
	parameters @ mgl-params rotation[ @ dup if ]free else drop then
	default-rotation parameters @ mgl-params rotation[ !
	parameters @ mgl-params aspect[ @ dup if ]free else drop then
	3 :] parameters @ mgl-params aspect[ !
    ;

    : mgl_settings_free
	parameters @ mgl-params labelx$ dup @ if $off else drop then
	parameters @ mgl-params labely$ dup @ if $off else drop then
	parameters @ mgl-params labelz$ dup @ if $off else drop then
	parameters @ mgl-params rotation[ @ dup if ]free else drop then
	parameters @ mgl-params aspect[ @ dup if ]free else drop then
    ;
    
    : addplot ( *a_plot -- )
	mhold @ if
	    >r plotlist @ r@ a_plot next ! r>
	    plotlist @ if
		dup plotlist @ a_plot prev !
	    then
	else
	    plotlist @ dispose-all-plots    
	then
	plotlist !
	generate-plot
	mcanvas draw ;
    : display_a_plot ( *a_plot -- )
	>r graph @
	r@ a_plot ops @
	r@ a_plot params @
	dup )size 0 do
	    i -rot
	    2>r
	    2r@ rot dup >r *) swap r> )@ F execute
	    2r>
	loop 2drop
	r> a_plot plotfunc @
	F execute ;
    : xmin ( -- f: xmin )
	1e20 plotlist @ begin dup while
		dup a_plot xrange @ ]min fmin
		a_plot next @
	repeat
	drop ;
    : xmax ( -- f: xmax )
	-1e20 plotlist @ begin dup while
		dup a_plot xrange @ ]max fmax
		a_plot next @
	repeat
	drop ;
    : ymin ( -- f: ymin )
	1e20 plotlist @ begin dup while
		dup a_plot yrange @ ]min fmin
		a_plot next @
	repeat
	drop ;
    : ymax ( -- f: ymax )
	-1e20 plotlist @ begin dup while
		dup a_plot yrange @ ]max fmax
		a_plot next @
	repeat
	drop ;

    : zmin ( -- f: zmin )
	1e20 plotlist @ begin dup while
		dup a_plot zrange @ ]min fmin
		a_plot next @
	repeat
	drop ;
    : zmax ( -- f: ymax )
	-1e20 plotlist @ begin dup while
		dup a_plot zrange @ ]max fmax
		a_plot next @
	repeat
	drop ;

    : display_plots ( -- )
	0 plotlist @ begin dup while
		nip dup a_plot next @
	repeat
	drop
	begin dup while
		dup display_a_plot
		a_plot prev @
	repeat drop ;
   
    : create-figure ( -- )
	graph @ mgl_identity
	graph @ mgl_clf
	graph @ mCanvas with w @ h @ endwith mgl_set_size
	
\  	parameters @ mgl-params rotation[ @ ]null? if
\  	    graph @
\  	    [ 1e 3e fsqrt f/ 1e fswap f- 2e f/ 0.7e f* ] fliteral
\  	    fdup
\  	    fdup fdup 1e fswap f- fswap 1e fswap f-
\  	    mgl_set_zoom
\  	else
	    graph @ 0e 0e 0e 0e mgl_set_zoom
\	    parameters @ mgl-params fontsize sf@	    
\ 	    1.2e f* parameters @ mgl-params fontsize sf!
\	then
	graph @ xmin ymin zmin xmax ymax zmax mgl_set_axis_3d
\	graph @ xmin ymin zmin mgl_set_origin
	graph @ NAN NAN NAN mgl_set_origin
	mgl-set-params
	mgl-post-params
	display_plots
    ;
    : fill-pixmap
	    clear-pixmap
	    graph @ mgl_get_rgb
	    mCanvas with w @ h @ endwith * 4 * dup allocate throw
	    dup >r
	    swap 4 / 3 *  move r@
	    mCanvas with w @ h @ endwith memory-pixmap new pxmap !
	    r> free throw ;
    : generate-plot ( -- )
	create-figure
	fill-pixmap ;

    : freeplots plotlist @ dispose-all-plots 0 plotlist ! ;

    : clf freeplots clear-pixmap draw ;

    : mgl-rotation! ( f: x y z )
	fswap frot parameters @ mgl-params rotation[ @ ]!s
	generate-plot
	mcanvas draw ;
    : mgl-rotation@ ( -- f: x y z )
	parameters @ mgl-params rotation[ @ ]@s ;
    : mgl-colorbar ( -- )
	parameters @ mgl-params colorbar @
	not
	parameters @ mgl-params colorbar !
	generate-plot mcanvas draw ;
    : mgl-grid parameters @ mgl-params grid dup @ not swap !
	generate-plot mcanvas draw ;
    : mgl-xlabel ( addr u -- )
	parameters @ mgl-params labelx$ dup @
	if dup $off $! else $! then
	generate-plot mcanvas draw ;
    : mgl-ylabel ( addr u -- )
	parameters @ mgl-params labely$ dup @
	if dup $off $! else $! then
	generate-plot mcanvas draw ;	
    : mgl-zlabel ( addr u -- )
	parameters @ mgl-params labelz$ dup @
	if dup $off $! else $! then
	generate-plot mcanvas draw ;	
        
    : dispose
	clear-pixmap
	plotlist @ dispose-all-plots
	graph @ mgl_delete_graph
	mgl_settings_free
	parameters @ sizeof mgl-params erase
	parameters @ free throw
	super dispose ;
    : widget
       CV[
	outer with pxmap @ endwith 0<> if
	    outer with pxmap @ endwith icon
	then
	]CV
	^^ CK[ 2drop 2drop ]CK
	$258 $1 *hfil $190 $1 *vfil canvas new  ^^bind mCanvas
	$10 $1 *hfill *hglue new
	^^ S[ s" not done" mCanvas text ]S X"   Save  " button new
	^^ S[ close ]S X"   Close  " button new
	&3 habox new vfixbox panel
	&2 vabox new ;
class;

sizeof a_figure allocate throw constant current-figure

current-figure sizeof a_figure erase

actor class clear-pointer
  how:
    : dispose
	current-figure a_figure active? off
	current-figure sizeof a_figure erase
	super dispose ;
class;

: init-plot   
    current-figure a_figure active? @ not if
	screen self mathplot new
	current-figure a_figure figure !
	current-figure a_figure active? on
	clear-pointer new
	current-figure a_figure figure @
	mathplot with >callback open endwith
    then ;

: s>range[] ( fmin fmax -- :] ) 2 :] dup 1 ]! dup 0 ]! ;

: []plot ( x[ str0 xt -- *a_plot )
    >r >r dup >r
    mgl_create_data dup rot mgl_data_set_vector
    sizeof a_plot allocate throw
    dup r@ dup ]min ]max s>range[] swap a_plot yrange !
    dup r> ]size  0e s>f s>range[] swap a_plot xrange !
    dup            0e 0e s>range[] swap a_plot zrange !
    dup 0 swap a_plot next !
    dup 0 swap a_plot prev !
    over 1 ivector* over a_plot hmdtlist !
    over r> 2 ivector* over a_plot params !
    ['] @ ['] @ 2 ivector* over a_plot ops !
    nip r> over a_plot plotfunc ! ;
: addplot ( *a_plot -- )
    init-plot
    current-figure a_figure figure @
    mathplot with addplot endwith ;
: ]plot ( *gsl_vector 0"" -- ) ['] mgl_plot []plot addplot ;
: ]stem ( *gsl_vector 0"" -- ) ['] mgl_stem []plot addplot ;
: ]bars ( *gsl_vector 0"" -- ) ['] mgl_bars []plot addplot ;
: ]step ( *gsl_vector 0"" -- ) ['] mgl_step []plot addplot ;
: ]area ( *gsl_vector 0"" -- ) ['] mgl_area []plot addplot ;

: [[]]plot ( x[[ str0 xt -- *a_plot )
    >r >r dup >r
    mgl_create_data dup rot mgl_data_set_matrix
    sizeof a_plot allocate throw
    dup r@ dup ]]min ]]max s>range[] swap a_plot zrange !
    dup r@ ]]size1  0e s>f s>range[] swap a_plot xrange !
    dup r> ]]size2  0e s>f s>range[] swap a_plot yrange !
    dup 0 swap a_plot next !
    dup 0 swap a_plot prev !
    over 1 ivector* over a_plot hmdtlist !
    over r> 2 ivector* over a_plot params !
    ['] @ ['] @ 2 ivector* over a_plot ops !
    nip r> over a_plot plotfunc ! ;
: ]]surf ( *gsl_matrix 0"" -- ) ['] mgl_surf [[]]plot addplot ;
: ]]tile ( *gsl_matrix 0"" -- ) ['] mgl_tile [[]]plot addplot ;
: ]]belt ( *gsl_matrix 0"" -- ) ['] mgl_belt [[]]plot addplot ;
: ]]fall ( *gsl_matrix 0"" -- ) ['] mgl_fall [[]]plot addplot ;
: ]]mesh ( *gsl_matrix 0"" -- ) ['] mgl_mesh [[]]plot addplot ;
: ]]msurf ( *gsl_matrix 0"" -- )    
    init-plot
    2dup ]]surf
    current-figure a_figure figure @
    mathplot with hold@ mgl-holdon endwith
    >r
    ]]mesh
    r>
    current-figure a_figure figure @
    mathplot with hold! endwith ;

: [[]]plotf ( f:v x[[ str0 xt -- *a_plot )
    >r >r dup >r
    mgl_create_data dup rot mgl_data_set_matrix
    sizeof a_plot allocate throw
    dup r@ dup ]]min ]]max s>range[] swap a_plot zrange !
    dup r@ ]]size1  0e s>f s>range[] swap a_plot xrange !
    dup r> ]]size2  0e s>f s>range[] swap a_plot yrange !
    dup 0 swap a_plot next !
    dup 0 swap a_plot prev !
    over 1 ivector* over a_plot hmdtlist !
    over r> f>fs 3 ivector* over a_plot params !
    ['] @ ['] @ ['] sf@ 3 ivector* over a_plot ops !
    nip r> over a_plot plotfunc ! ;
: ]]boxs ( f:v *gsl_matrix 0"" -- ) ['] mgl_boxs [[]]plotf addplot ;

: clf current-figure a_figure active? @ if
	current-figure a_figure figure @ >o mathplot clf o>
    then ;

: fontsize! ( f:size -- ) current-figure a_figure active? @ if
	current-figure a_figure figure @ >o mathplot fontsize o>
    then ;

: rotation ( F: x y z -- )
    current-figure a_figure active? @ if
	current-figure a_figure figure @ >o mathplot mgl-rotation! o>
    else
	fdrop fdrop fdrop
    then ;
: colorbar current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-colorbar o>
    then ;


: mglhold current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-hold o>
    then ;

: mglholdon current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-holdon o>
    then ;
: mglholdoff current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-holdoff o>
    then ;

: mglgrid current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-grid o>
    then ;

: xlabel ( addr u -- ) current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-xlabel o>
    then ;
: ylabel ( addr u -- ) current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-ylabel o>
    then ;
: zlabel ( addr u -- ) current-figure a_figure active? @ if
	current-figure a_figure figure @
	>o mathplot mgl-zlabel o>
    then ;


clear
previous previous previous previous previous
Module;

\\\
also mglplot also minos also gsl
100 fvector x[ x[ ]randomize
x[ 0 ]plot
