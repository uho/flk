\  FLK X-Forms access
\
\  Copyright (C) 1998 Lars Krueger 
\
\  This file is part of FLK.
\
\  FLK is free software; you can redistribute it and/or
\  modify it under the terms of the GNU General Public License
\  as published by the Free Software Foundation; either version 2
\  of the License, or (at your option) any later version.
\
\  This program is distributed in the hope that it will be useful,
\  but WITHOUT ANY WARRANTY; without even the implied warranty of
\  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\  GNU General Public License for more details.
\
\  You should have received a copy of the GNU General Public License
\  along with this program; if not, write to the Free Software
\  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

\ $Id: flkXforms.fs,v 1.7 1998/09/13 21:28:56 root Exp $
\ $Log: flkXforms.fs,v $
\ Revision 1.7  1998/09/13 21:28:56  root
\ fixed cf stackk bugs
\
\ Revision 1.6  1998/08/30 10:50:59  root
\ new optimizer algorithm
\
\ Revision 1.5  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.4  1998/07/03 20:57:50  root
\ level 2 optimizers added
\
\ Revision 1.3  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.2  1998/06/03 07:55:16  root
\ added functions
\
\ Revision 1.1  1998/05/23 17:55:17  root
\ Initial revision
\

S" libX11.so.6" lib X11
S" libXt.so.6" lib Xt
S" libm.so.5" lib math
S" libforms.so.0.88" lib xforms

int fct: fl_initialize 5 ints 
int fct: fl_do_forms void 
int fct: fl_check_forms void 
int fct: fl_do_only_forms void 
int fct: fl_check_only_forms void 
int fct: fl_bgn_form 3 ints 
void fct: fl_end_form void 
int fct: fl_show_form 4 ints 
void fct: fl_hide_form int 
int fct: fl_add_clock 6 ints 
void fct: fl_freeze_form int 
void fct: fl_unfreeze_form int 
void fct: fl_freeze_all_forms void 
void fct: fl_unfreeze_all_forms void 
void fct: fl_free_form int 
int fct: fl_add_menu 6 ints 
int fct: fl_set_menu_entries 2 ints 
void fct: fl_set_menu 2 ints 
void fct: fl_set_object_gravity 3 ints 
void fct: fl_show_alert 4 ints 
int fct: fl_use_fselector int 
int fct: fl_show_fselector 4 ints 
int fct: fl_add_chart 6 ints 
void fct: fl_add_chart_value 2 ints double int 
void fct: fl_replace_chart_value 2 ints double 2 ints 
int fct: fl_set_form_atclose 3 ints 
int fct: fl_set_atclose 2 ints 
int fct: fl_add_browser 6 ints 
void fct: fl_clear_browser int 
void fct: fl_add_browser_line 2 ints 
void fct: fl_addto_browser 2 ints 
void fct: fl_addto_browser_chars 2 ints 
int fct: fl_load_browser 2 ints 
void fct: fl_set_browser_topline 2 ints 
void fct: fl_select_browser_line 2 ints 
void fct: fl_deselect_browser_line 2 ints 
void fct: fl_deselect_browser int 
int fct: fl_isselected_browser_line 2 ints 
void fct: fl_set_browser_fontsize 2 ints 
void fct: fl_set_browser_fontstyle 2 ints 
int fct: fl_set_object_callback 3 ints 
int fct: fl_get_browser int 
int fct: fl_bgn_group void 
int fct: fl_end_group void 
int fct: fl_add_button 6 ints 
void fct: fl_hide_object int 
void fct: fl_show_object int 
int fct: fl_add_box 6 ints
void fct: fl_set_object_lalign 2 ints
int fct: fl_get_input int 
void fct: fl_set_input 2 ints
int fct: fl_add_text 6 ints
int fct: fl_add_input 6 ints
void fct: fl_set_object_boxtype 2 ints
void fct: fl_set_object_label 2 ints

 0 CONSTANT  FL_NO_BOX
 1 CONSTANT  FL_UP_BOX
 2 CONSTANT  FL_DOWN_BOX
 3 CONSTANT  FL_BORDER_BOX
 4 CONSTANT  FL_SHADOW_BOX
 5 CONSTANT  FL_FRAME_BOX
 6 CONSTANT  FL_ROUNDED_BOX
 7 CONSTANT  FL_EMBOSSED_BOX
 8 CONSTANT  FL_FLAT_BOX
 9 CONSTANT  FL_RFLAT_BOX
10 CONSTANT  FL_RSHADOW_BOX
11 CONSTANT  FL_OVAL_BOX
12 CONSTANT  FL_OSHADOW_BOX

  0 CONSTANT FL_PLACE_FREE
  1 CONSTANT FL_PLACE_MOUSE
  2 CONSTANT FL_PLACE_CENTER
  4 CONSTANT FL_PLACE_POSITION
  8 CONSTANT FL_PLACE_SIZE
 16 CONSTANT FL_PLACE_GEOMETRY
 32 CONSTANT FL_PLACE_ASPECT
 64 CONSTANT FL_PLACE_FULLSCREEN
128 CONSTANT FL_PLACE_HOTSPOT
256 CONSTANT FL_PLACE_ICONIC
1 14 LSHIFT CONSTANT FL_FREE_SIZE
1 15 LSHIFT CONSTANT FL_FIX_SIZE

 -1 CONSTANT FL_TRANSIENT
  0 CONSTANT FL_NOBORDER
  1 CONSTANT FL_FULLBORDER

0 CONSTANT FL_ANALOG_CLOCK
1 CONSTANT FL_DIGITAL_CLOCK

0 CONSTANT FL_TOUCH_MENU
1 CONSTANT FL_PUSH_MENU
2 CONSTANT FL_PULLDOWN_MENU

0  CONSTANT ForgetGravity
1  CONSTANT NorthWestGravity
2  CONSTANT NorthGravity		
3  CONSTANT NorthEastGravity	
4  CONSTANT WestGravity		
5  CONSTANT CenterGravity		
6  CONSTANT EastGravity		
7  CONSTANT SouthWestGravity	
8  CONSTANT SouthGravity		
9  CONSTANT SouthEastGravity	
10 CONSTANT StaticGravity		

0 CONSTANT FL_BAR_CHART
1 CONSTANT FL_HORBAR_CHART
2 CONSTANT FL_LINE_CHART
3 CONSTANT FL_FILL_CHART
4 CONSTANT FL_SPIKE_CHART
5 CONSTANT FL_PIE_CHART
6 CONSTANT FL_SPECIALPIE_CHART

0 CONSTANT FL_BLACK
1 CONSTANT FL_RED
2 CONSTANT FL_GREEN
3 CONSTANT FL_YELLO
4 CONSTANT FL_BLUE
5 CONSTANT FL_MAGENTA
6 CONSTANT FL_CYAN
7 CONSTANT FL_WHITE
8 CONSTANT FL_TOMATO
9 CONSTANT FL_INDIANRED
10 CONSTANT FL_SLATEBLUE
11 CONSTANT FL_COL1
12 CONSTANT FL_RIGHT_BCOL
13 CONSTANT FL_BOTTOM_BCOL
14 CONSTANT FL_TOP_BCOL
15 CONSTANT FL_LEFT_BCOL
16 CONSTANT FL_MCOL
17 CONSTANT FL_INACTIVE
18 CONSTANT FL_PALEGREEN
19 CONSTANT FL_DARKGOLD
20 CONSTANT FL_ORCHID
21 CONSTANT FL_DARKCYAN
22 CONSTANT FL_DARKTOMATO
23 CONSTANT FL_WHEAT
24 CONSTANT FL_DARKORANGE
25 CONSTANT FL_DEEPPINK
26 CONSTANT FL_CHARTREUSE
27 CONSTANT FL_DARKVIOLET
28 CONSTANT FL_SPRINGGREEN
29 CONSTANT FL_DODGERBLUE
    
1 CONSTANT FL_ON 
1 CONSTANT FL_OK 
1 CONSTANT FL_VALID 
1 CONSTANT FL_PREEMPT 
2 CONSTANT FL_AUTO 
FL_AUTO CONSTANT FL_WHEN_NEEDED 
0 CONSTANT FL_OFF 
0 CONSTANT FL_CANCEL 
0 CONSTANT FL_INVALID 
0 CONSTANT FL_NONE 
-1 CONSTANT FL_IGNORE 
-2 CONSTANT FL_CLOSE 

0 CONSTANT FL_NORMAL_BROWSER
1 CONSTANT FL_SELECT_BROWSER
2 CONSTANT FL_HOLD_BROWSER
3 CONSTANT FL_MULTI_BROWSER

0 CONSTANT FL_NORMAL_BUTTON
1 CONSTANT FL_PUSH_BUTTON
2 CONSTANT FL_RADIO_BUTTON
3 CONSTANT FL_HIDDEN_BUTTON
4 CONSTANT FL_TOUCH_BUTTON
5 CONSTANT FL_INOUT_BUTTON
6 CONSTANT FL_RETURN_BUTTON
7 CONSTANT FL_HIDDEN_RET_BUTTON
8 CONSTANT FL_MENU_BUTTON

 0 CONSTANT FL_NORMAL_STYLE
 1 CONSTANT FL_BOLD_STYLE
 2 CONSTANT FL_ITALIC_STYLE
 3 CONSTANT FL_BOLDITALIC_STYLE
 4 CONSTANT FL_FIXED_STYLE
 5 CONSTANT FL_FIXEDBOLD_STYLE
 6 CONSTANT FL_FIXEDITALIC_STYLE
 7 CONSTANT FL_FIXEDBOLDITALIC_STYLE
 8 CONSTANT FL_TIMES_STYLE
 9 CONSTANT FL_TIMESBOLD_STYLE
10 CONSTANT FL_TIMESITALIC_STYLE
11 CONSTANT FL_TIMESBOLDITALIC_STYLE
    
1  9 LSHIFT CONSTANT FL_SHADOW_STYLE
1 10 LSHIFT CONSTANT FL_ENGRAVED_STYLE
1 11 LSHIFT CONSTANT FL_EMBOSSED_STYLE

0 CONSTANT FL_NORMAL_TEXT

0 CONSTANT FL_ALIGN_CENTER
1 CONSTANT FL_ALIGN_TOP
2 CONSTANT FL_ALIGN_BOTTOM
4 CONSTANT FL_ALIGN_LEFT
8 CONSTANT FL_ALIGN_RIGHT 
FL_ALIGN_TOP FL_ALIGN_LEFT OR CONSTANT FL_ALIGN_TOP_LEFT
FL_ALIGN_TOP FL_ALIGN_RIGHT OR CONSTANT FL_ALIGN_TOP_RIGHT
FL_ALIGN_BOTTOM FL_ALIGN_LEFT OR CONSTANT FL_ALIGN_BOTTOM_LEFT
FL_ALIGN_BOTTOM FL_ALIGN_RIGHT OR CONSTANT FL_ALIGN_BOTTOM_RIGHT
1 13 LSHIFT CONSTANT FL_ALIGN_INSIDE
1 14 LSHIFT CONSTANT FL_ALIGN_VERT
FL_ALIGN_TOP_LEFT CONSTANT FL_ALIGN_LEFT_TOP
FL_ALIGN_TOP_RIGHT CONSTANT FL_ALIGN_RIGHT_TOP
FL_ALIGN_BOTTOM_LEFT CONSTANT FL_ALIGN_LEFT_BOTTOM
FL_ALIGN_BOTTOM_RIGHT CONSTANT FL_ALIGN_RIGHT_BOTTOM

0 CONSTANT FL_NORMAL_INPUT
1 CONSTANT FL_FLOAT_INPUT
2 CONSTANT FL_INT_INPUT
3 CONSTANT FL_DATE_INPUT
4 CONSTANT FL_MULTILINE_INPUT
5 CONSTANT FL_HIDDEN_INPUT
6 CONSTANT FL_SECRET_INPUT

\ ------------------------------ High level access -----------------------------
: fl_run 			( -- )
  BEGIN 
    fl_do_forms
  WHILE
  REPEAT ;
  
0 VALUE last-object

int callback: form-closer int int 		( form userdata -- ok? )
  DROP fl_hide_form
  FL_IGNORE
;callback

\ Define a form.
: form 					( back widht height -<name>- )
  :
  ROT POSTPONE LITERAL
  SWAP POSTPONE LITERAL
  POSTPONE LITERAL
  POSTPONE fl_bgn_form			\ form 
;

: end-form 				( -- )
  POSTPONE DUP
  ['] form-closer >CFA @ 
  POSTPONE RLITERAL
  42 POSTPONE LITERAL
  POSTPONE fl_set_form_atclose
  POSTPONE DROP
  POSTPONE fl_end_form
  POSTPONE ;
; IMMEDIATE 

: digital-clock 			( x y w h -- )
  FL_DIGITAL_CLOCK -TWIST S0" " 
  fl_add_clock TO last-object
;

: analog-clock 				( x y w h -- )
  FL_ANALOG_CLOCK -TWIST S0" " 
  fl_add_clock TO last-object
;

: gravity 				( nw se -- )
  last-object -ROT fl_set_object_gravity ;

: alignment 				( align -- )
  last-object SWAP fl_set_object_lalign ;

: boxtype 				( bt -- )
  last-object SWAP fl_set_object_boxtype ;

\ Allocate a cell counted string for the given string and append a zero to it.
\ Both the address of the asciiz string (for the C function) and the address
\ of the cell counted string is returned.
: >asciiz 				( addr len -- asciiz str )
  DUP 1+ $ALLOCATE 			\ addr len str
  DUP -TURN $copy
  DUP $COUNT + 				\ str last
  0 SWAP C!
  1 OVER +!  
  DUP CELL+ SWAP ;

\ Access the nth parameter in a C-callback.
: C-param 				( n -- )
  compile-only
  [ ALSO ASSEMBLER ]
  (opt-flush)
  regalloc-reset
  req-free
  esp free0 mov,
  CELLS CELL+ [free0] free0 mov,
  0 free>tos
  [ PREVIOUS ]
; IMMEDIATE

\ Start the compilation of a headerless, unnamed word and return it's address.
\ The parameter tells the word how many parameters have to be copied to the
\ stack. To maintain a correct stack handling ebp is saved here to the return
\ stack.
: noname-callback 			( n -- addr )
  CHERE SWAP 
  [ ALSO ASSEMBLER ]
  0 TO #tos-cache
  0 TO offs-ebp 
  (opt-flush)
  regalloc-reset
  ebp push,
  HA-SAVE-F-EBP	#[] ebp mov,
  ebx push,
  ecx push,
  edx push,
  esi push,
  edi push,
  0 ?DO
    I 7 + POSTPONE C-param
  LOOP
  [ PREVIOUS ] ;

\ End a callback. ebp is restored.
: end-callback 				( -- )
  [ ALSO ASSEMBLER ]
  (opt-flush)
  regalloc-reset
  (end-word)
  edi pop,
  esi pop,
  edx pop,
  ecx pop,
  ebx pop,
  ebp pop,
  ret,
  [ PREVIOUS ] ;

\ Store a asciiz string and return starting point.
: asciiz,				( str -- addr )
  HERE SWAP $COUNT DUP 1+ ALLOT 	\ addr s #s
  TUCK FLOCK SWAP CMOVE 		\ addr #s 
  OVER + 0 SWAP C! ;

\ Display the form using the string as the title.
: show-form 				( form addr len -- )
  FL_PLACE_FREE FL_FULLBORDER 2SWAP 	\ form place border addr len
  >asciiz 				\ f p b asciiz str
  -TWIST fl_show_form DROP  		\ str
  FREE THROW ;

\ Begin a menu definition. 
: menu( 				( -<name>- menu )
  skip-space BL PARSE $ALLOCOPY 	\ str
  []( SWAP []+= ;

\ Begin a sub-menu.
: sub-menu 				( menu addr len -- menu )
  S" /" 2SWAP $allo-cat 		\ menu str
  DUP asciiz, SWAP FREE THROW 		\ menu addr
  []+= 0 []+= 0 []+= 0 []+=
;

\ Define a simple menu entry calling name. name's stack effect must be 
\ ( index -- index ) where index is the (one-based) index into the menu.
: item 					( menu addr len -<name>- menu )
  $allocopy DUP asciiz, SWAP FREE THROW \ menu text
  []+= TRUE STATE exchange SWAP 	\ state menu 
  1 noname-callback 			\ state menu addr
  []+= 0 []+= 0 []+= 			\ state menu
  [ ALSO ASSEMBLER ]
  regalloc-flush
  ' >CFA @ ## call,
  end-callback
  [ PREVIOUS ] 
  SWAP STATE !
;

\ Define an evaluated menu entry. The stack effect of the evaluated code must
\ be ( index -- index ) where index is the (one-based) index into the menu.
: item" 				( menu addr len -<text">- menu )
  $allocopy DUP asciiz, SWAP FREE THROW \ menu text
  []+= TRUE STATE exchange SWAP 	\ state menu 
  1 noname-callback 			\ state menu addr
  []+= 0 []+= 0 []+= 			\ state menu
  POSTPONE S"
  POSTPONE EVALUATE
  end-callback
  SWAP STATE !
;

\ Define a compiled menu entry. The stack effect must be ( index -- index )
\ where index is the (one-based) index into the menu. The rest of the line is
\ compiled.
: item: 				( menu addr len -<text>- menu )
  $allocopy DUP asciiz, SWAP FREE THROW \ menu text
  []+= TRUE STATE exchange SWAP 	\ state menu 
  1 noname-callback 			\ state menu addr
  []+= 0 []+= 0 []+= 			\ state menu
  INTERPRET
  end-callback
  SWAP STATE !
;

\ End a submenu.
: end-sub 				( menu -- menu )
  4 0 do 
    0 []+=
  loop ;

\ End a menu definition.
: )menu 				( menu -- )
  end-sub
  DUP 0 []@ 				\ menu str
  DUP $COUNT $CREATE FREE THROW 	\ menu
  DUP []# 1 BEGIN			\ menu cnt ind
    2DUP > 
  WHILE 				\ menu cnt ind
    PLUCK OVER []@ DUP 
    IF r, ELSE , THEN 1+		\ menu cnt ind
    PLUCK OVER []@ DUP 
    IF r, ELSE , THEN 1+		\ menu cnt ind
    PLUCK OVER []@ DUP 
    IF r, ELSE , THEN 1+		\ menu cnt ind
    PLUCK OVER []@ , 1+
  REPEAT 2DROP )[] ;

\ Create a menu and add it to the current form.
: menu 					( x y w h addr menu -- )
  >R FL_PUSH_MENU -ROTARE 		\ type x y w h label
  fl_add_menu 				\ obj
  DUP TO last-object
  R> 					\ obj menu
  fl_set_menu_entries DROP
;

\ Display an alert panel with three lines of text and the first line bold.
: alert-panel 				( s1 s2 s3 -- )
  1 fl_show_alert ;

\ Pick a filename and return it and a success flag.
: gui-filename 				( -- addr len TRUE / FALSE )
  0 fl_use_fselector DROP
  S0" Pick a file to be INCLUDED."
  S0" *" S0" " S0" "
  fl_show_fselector 			\ addr
  DUP IF
    strlen TRUE
  THEN ;

: <group fl_bgn_group DROP ;
: group> fl_end_group DROP ;

: obj-callback 				( -<name>- )
  POSTPONE last-object
  POSTPONE callback
  0 POSTPONE LITERAL
  POSTPONE fl_set_object_callback
  POSTPONE DROP
; IMMEDIATE 

: regular-object 			( type -<name constructor>- )
  CREATE , ' r, DOES>
  DUP @ 				\ x y w h label addr type
  SWAP >R -ROTARE R>			\ type x y w h label addr
  CELL+ @ EXECUTE 
  TO last-object ;
  
FL_SPECIALPIE_CHART 	regular-object special-pie-chart	fl_add_chart
FL_PIE_CHART 		regular-object pie-chart 		fl_add_chart
FL_BAR_CHART 		regular-object bar-chart 		fl_add_chart
FL_HORBAR_CHART 	regular-object horbar-chart		fl_add_chart
FL_LINE_CHART 		regular-object line-chart		fl_add_chart
FL_FILL_CHART 		regular-object fill-chart		fl_add_chart
FL_SPIKE_CHART 		regular-object spike-chart		fl_add_chart
FL_NORMAL_BROWSER 	regular-object normal-browser 		fl_add_browser
FL_HOLD_BROWSER 	regular-object hold-browser 		fl_add_browser

FL_NORMAL_BUTTON  	regular-object normal-button 		fl_add_button
FL_PUSH_BUTTON 		regular-object push-button 		fl_add_button
FL_RADIO_BUTTON 	regular-object radio-button 		fl_add_button
FL_HIDDEN_BUTTON 	regular-object hidden-button 		fl_add_button
FL_TOUCH_BUTTON 	regular-object touch-button 		fl_add_button
FL_INOUT_BUTTON  	regular-object inout-button 		fl_add_button
FL_RETURN_BUTTON 	regular-object return-button 		fl_add_button
FL_HIDDEN_RET_BUTTON 	regular-object hidden-return-button 	fl_add_button
FL_MENU_BUTTON 		regular-object menu-button 		fl_add_button

FL_UP_BOX 		regular-object up-box 			fl_add_box
FL_DOWN_BOX 		regular-object down-box 		fl_add_box
FL_BORDER_BOX 		regular-object border-box 		fl_add_box
FL_SHADOW_BOX 		regular-object shadow-box 		fl_add_box
FL_FRAME_BOX 		regular-object frame-box 		fl_add_box
FL_ROUNDED_BOX 		regular-object rounded-box 		fl_add_box
FL_EMBOSSED_BOX 	regular-object embossed-box 		fl_add_box
FL_FLAT_BOX 		regular-object flat-box 		fl_add_box
FL_RFLAT_BOX 		regular-object rflat-box 		fl_add_box
FL_RSHADOW_BOX 		regular-object rshadow-box 		fl_add_box
FL_OVAL_BOX 		regular-object oval-box 		fl_add_box
FL_OSHADOW_BOX 		regular-object oshadow-box		fl_add_box

FL_NORMAL_TEXT 		regular-object normal-text 		fl_add_text

FL_NORMAL_INPUT		regular-object normal-input 		fl_add_input
FL_FLOAT_INPUT		regular-object float-input 		fl_add_input
FL_INT_INPUT		regular-object int-input 		fl_add_input
FL_DATE_INPUT		regular-object date-input 		fl_add_input
FL_MULTILINE_INPUT	regular-object multiline-input 		fl_add_input
FL_HIDDEN_INPUT		regular-object hidden-input 		fl_add_input
FL_SECRET_INPUT		regular-object secret-input 		fl_add_input

