\  FLK X-Windows control panel
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

\ $Id: flkcontrol.fs,v 1.7 1998/07/06 18:01:13 root Exp $
\ $Log: flkcontrol.fs,v $
\ Revision 1.7  1998/07/06 18:01:13  root
\ word browser extended
\
\ Revision 1.6  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.5  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.4  1998/06/03 07:55:16  root
\ added file browser
\
\ Revision 1.3  1998/06/01 18:05:39  root
\ added error message if xforms setup fails
\
\ Revision 1.2  1998/05/24 15:41:26  root
\ check for error on fl_initialize
\
\ Revision 1.1  1998/05/23 17:55:17  root
\ Initial revision
\

: gui-include 				( -- )
  gui-filename IF 
    ['] INCLUDED CATCH
    IF
      S0" Error loading file."
      S0" "
      S0" " alert-panel
    THEN
  THEN ;

0 VALUE gui-wl-browser
0 VALUE gui-word-browser
0 VALUE gui-hash-chart
0 VALUE gui-file-view
0 VALUE word-panel
0 VALUE gui-current-file
0 VALUE gui-current-wl-xts
0 VALUE gui-current-cfa-text

void callback: view-button-pressed int int 		( -- )
  gui-hash-chart fl_hide_object
  gui-file-view fl_show_object
;callback

void callback: stats-button-pressed int int  		( -- )
  gui-hash-chart fl_show_object
  gui-file-view fl_hide_object
;callback

: fill-file-view 				( fn -- )
  CELL+ $COUNT >asciiz SWAP 			\ str asciiz
  gui-file-view OVER fl_load_browser 		\ str asciiz ok?
  0= IF
    gui-file-view S0" Can't open file" fl_add_browser_line
    gui-file-view SWAP fl_add_browser_line
  ELSE DROP
  THEN
  )[]
;

: gui-set-word-info 				( xt -- )
  DUP >OCFA @ 					\ xt ocfa
  BASE @ >R HEX <# 
  DUP IMAGE-BASE <> IF 				\ xt ocfa
    0 # # # # # # # # S"  OCFA: 0x" "HOLD 	\ xt 0.
    2DROP
  ELSE  
    DROP
  THEN 						\ xt 
  >CFA @ DUP IMAGE-BASE <> IF 			\ cfa
    0 # # # # # # # # S" CFA: 0x" "HOLD
    2DROP
  ELSE 
    DROP
  THEN 						\ 
  0. #> 					\ addr len
  >asciiz 					\ asciiz str
  gui-current-cfa-text ROT 			\ str obj asciiz
  fl_set_object_label )[]
  R> BASE ! ;
  
void callback: word-selected-cb int int  	( browser 0 -- )
  DROP fl_get_browser 1-			\ line
  gui-current-wl-xts SWAP []@ 			\ xt
  DUP >FN @ DUP gui-current-file <> IF 		\ xt fn
    DUP TO gui-current-file
    word-panel fl_freeze_form
    gui-file-view fl_clear_browser 		\ xt fn
    fill-file-view 				\ xt
    word-panel fl_unfreeze_form
  ELSE DROP
  THEN 
  DUP >DL @ gui-file-view OVER 
  fl_set_browser_topline 			\ xt line
  gui-file-view SWAP
  fl_select_browser_line
  gui-set-word-info
;callback

CREATE wl-count #BUCKETS CELLS ALLOT
: fill-word-browser 			( wid -- )
  wl-count #BUCKETS CELLS ERASE
  wl-count 				\ wid cnt
  gui-current-wl-xts 0 []-truncate 
  #BUCKETS 0 DO
    OVER BEGIN 				\ thread cnt xt
      @
      IMAGE-BASE OVER <>
    WHILE 				\ thread cnt xt
      gui-current-wl-xts OVER []+= TO gui-current-wl-xts
      DUP >NAME COUNT >asciiz 		\ thread cnt xt asciiz str
      gui-word-browser ROT  		\ thread cnt xt str brw asciiz
      fl_add_browser_line )[] 		\ thread cnt xt 
      1 PLUCK +!
    REPEAT DROP
    CELL+ SWAP CELL+ SWAP
  LOOP
  2DROP
;

void callback: wl-browser-cb int int 	( browser 0 -- )
  DROP DUP fl_get_browser 1-		\ brw ind
  voc-link SWAP 0 ?DO 			\ brw wid
    #BUCKETS CELLS + @
  LOOP 					\ brw wid
  word-panel fl_freeze_form
  gui-word-browser fl_clear_browser
  fill-word-browser DROP
  wl-count #BUCKETS 0 DO 		\ cnt
    DUP @ DUP 0 D>F 			\ cnt / f: val
    0 <# #S #> >asciiz SWAP  		\ cnt str asciiz
    gui-hash-chart I 1+ ROT  		\ cnt str obj ind txt /f: VALUE
    FL_BLACK fl_replace_chart_value
    )[] CELL+
  LOOP DROP
  word-panel fl_unfreeze_form
;callback

: chart-rect 255 5 165 220 ;

FL_FLAT_BOX 430 300 form word-panel-form
  5 5 120 220 S0" word lists" hold-browser
  last-object TO gui-wl-browser
  obj-callback wl-browser-cb
  NorthWestGravity SouthWestGravity gravity
  
  130 5 120 220 S0" words" hold-browser
  last-object TO gui-word-browser
  obj-callback word-selected-cb
  NorthWestGravity SouthWestGravity gravity

  5 250 250 30 S0" " normal-text
  last-object TO gui-current-cfa-text
  FL_SHADOW_BOX boxtype
  SouthWestGravity SouthEastGravity gravity
  
  chart-rect S0" hash stats" horbar-chart
  last-object TO gui-hash-chart
  NorthWestGravity SouthEastGravity gravity
  
  chart-rect S0" file view" normal-browser
  last-object TO gui-file-view
  NorthWestGravity SouthEastGravity gravity
  last-object FL_FIXED_STYLE fl_set_browser_fontstyle 

  <group
    270 250 50 30 S0" stats" radio-button
    obj-callback stats-button-pressed
    SouthEastGravity SouthEastGravity gravity
    370 250 50 30 S0" view" radio-button
    obj-callback view-button-pressed
    SouthEastGravity SouthEastGravity gravity
  group>
end-form

0 VALUE faultyword-addr-input
0 VALUE faultyword-base-input
0 VALUE faulty-word-form
0 VALUE faulty-OutputText

void callback: find_faulty_word_cb int int 	( obj data -- )
  BASE @ >R HEX
  2DROP 0. faultyword-addr-input fl_get_input 	\ 0 0 addr
  strlen >NUMBER 2DROP D>S 			\ fault-at
  0. faultyword-base-input fl_get_input strlen
  >NUMBER 2DROP D>S 				\ fault-at base
  2DUP U< IF
    2DROP
    S" Wrong fault-at value. (Lower than base.)"
    >asciiz SWAP 				\ str asciiz
    faulty-OutputText SWAP fl_set_object_label	\ str
    )[]
  ELSE
    (faulty-word) 				\ xt cfa?
    OVER 0= IF 
      2DROP 
      S" Error in address. "
      S" No word found." 				\ s1 #s1 s2 #s2
    ELSE
      SWAP >NAME COUNT 				\ cfa? s1 #s1
      ROT IF
	S"  (execution semantics)"
      ELSE
	S"  (optimization semantics)"
      THEN 					\ s1 #s1 s2 #s2
    THEN
    $allo-cat 					\ str
    DUP $COUNT 					\ str addr len
    >asciiz SWAP 				\ str1 str2 asciiz
    faulty-OutputText SWAP fl_set_object_label	\ str1 str2
    )[] )[]
  THEN
  R> BASE !
;callback

FL_FLAT_BOX 270 300 form FaultyWordForm
  120 140 80 40 S0" Find word." normal-button 
    obj-callback find_faulty_word_cb
  50 230 200 30 S0" " normal-text 
    last-object TO faulty-OutputText
    FL_SHADOW_BOX boxtype
    FL_ALIGN_LEFT FL_ALIGN_INSIDE OR alignment
  80 20 170 30 S0" Fault at (hex)" normal-input 
    FL_ALIGN_LEFT alignment
    last-object TO faultyword-addr-input
  80 60 170 30 S0" base (hex)" normal-input 
    FL_ALIGN_LEFT alignment
    last-object TO faultyword-base-input
  10 230 40 30 S0" Word:" normal-text 
    FL_ALIGN_LEFT FL_ALIGN_INSIDE OR alignment
  145 90 30 40 S0" @2->" normal-text 
    FL_ALIGN_LEFT FL_ALIGN_INSIDE OR alignment
  145 190 30 40 S0" @2->" normal-text 
    FL_ALIGN_LEFT FL_ALIGN_INSIDE OR alignment
end-form

: gui-find-faulty-word 			( -- )
  faulty-word-form 0= IF
    FaultyWordForm TO faulty-word-form
  THEN
  faulty-word-form S" Find word from address" show-form
;

: fill-wl-browser 			( -- )
  voc-link BEGIN 			\ wid
    #BUCKETS CELLS + DUP @ SWAP 
    CELL+ COUNT 			\ next addr len
    >asciiz 				\ next asciiz str
    SWAP gui-wl-browser SWAP
    fl_add_browser_line 		\ next str
    )[]
    DUP IMAGE-BASE =
  UNTIL DROP ;

: gui-words 				( -- )
  word-panel 0= IF
    word-panel-form TO word-panel
    []( TO gui-current-wl-xts
    gui-hash-chart fl_hide_object
    gui-file-view fl_hide_object
    fill-wl-browser
    #BUCKETS 0 DO
      gui-hash-chart 0E0 		\ str asciiz obj / f: 0.0
      S0" " FL_BLACK fl_add_chart_value
    LOOP
  THEN
  word-panel S" Word list browser" show-form
;

menu( main-menu
  S" Include file" item gui-include
  S" Browse words" item gui-words
  S" Find fault"   item gui-find-faulty-word
  S" Exit" item BYE
)menu

0 VALUE code-chart
0 VALUE data-chart
FL_FLAT_BOX 200 150 form mainform
  5 5 190 40 S0" Main Menu" main-menu menu 
    NorthWestGravity NorthEastGravity gravity
  5 50 90 75 S0" code space" pie-chart
    NorthWestGravity SouthGravity gravity
    last-object TO code-chart
    code-chart CUNUSED 0 D>F S0" free" FL_GREEN fl_add_chart_value
    code-chart CHERE IMAGE-BASE - 0 D>F S0" used" FL_RED fl_add_chart_value
  105 50 90 75 S0" data space" pie-chart
    NorthGravity SouthEastGravity gravity
    last-object TO data-chart
    data-chart UNUSED 0 D>F S0" free" FL_GREEN fl_add_chart_value
    data-chart HERE IMAGE-BASE - HA-CODESIZE @ - 0 D>F S0" used" FL_RED fl_add_chart_value
end-form

HERE RVALUE lasthere
CHERE RVALUE lastchere
: (allot-hook) 
  here lasthere 100 + > IF
    data-chart 1 UNUSED 0 D>F S0" free" FL_GREEN fl_replace_chart_value
    data-chart 2 HERE IMAGE-BASE - HA-CODESIZE @ - 0 D>F S0" used" FL_RED
    fl_replace_chart_value 
    HERE TO lasthere 
  THEN ;

: (callot-hook) 
  CHERE lastchere 1000 + > IF
    code-chart 1 CUNUSED 0 D>F S0" free" FL_GREEN fl_replace_chart_value
    code-chart 2 CHERE IMAGE-BASE - 0 D>F S0" used" FL_RED
    fl_replace_chart_value 
    CHERE TO lastchere
  THEN ;

0 VALUE main-form
: gui-controller main-form S" X-FLK control panel" show-form ;

: start-gui-control 
  (setup-system)
  #? ARGC ARGV S0" XFlk" 0 0 
  fl_initialize 0= IF 
    ." Error setting up XForms." CR BYE 
  THEN
  0 TO word-panel
  mainform TO main-form
  gui-controller 
  0 TO SOURCE-ID
  FALSE STATE !
  0 TO error-fn
  ['] load-cmdline CATCH DUP TO (lcmd-ex) QUIT-CATCHER
  (lcmd-ex) IF ANY-KEY BYE THEN
  error-fn ?DUP IF FREE DROP 0 TO error-fn THEN
  QUIT ;

: (bkgrnd-form-checker)  			( -- )
  fl_check_forms DROP ;

' (bkgrnd-form-checker) TO backgrounder 
' (callot-hook) TO CALLOT-HOOK
' (allot-hook) TO ALLOT-HOOK

S" xflk" 2DUP copy-executable
TURNKEY start-gui-control
BYE
