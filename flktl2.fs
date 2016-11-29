\ FLK level2 optimizers (target)
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

\ $Id$
\ $Log$

\ This is executed in the host.

: t-COUNT 				( addr -- addr+1 len )
  DUP 1+ SWAP t-C@ ;

: t-COMPARE 				( taddr len addr len -- flag )
  2SWAP SWAP TARGET + SWAP 2SWAP
  COMPARE ;

: t-SEARCH-WORDLIST 			( addr len wid -- {xt} flag )
  -ROT 					\ wid ca u
  2DUP (calcVocHash) 			\ wid ca u hash
  [ISEM] CELLS [PREVIOUS] 		\ wid ca u offs
  TURN + 				\ ca u &head
  BEGIN 				\ ca u &head
    t-@ 				\ ca u xt
    DUP 				\ ca u xt xt
    IMAGE-BASE <>
  WHILE 				\ ca u xt
    DUP t->name t-COUNT 			\ ca u xt na nl
    ROT >R 				\ ca u na nl / r: xt
    2OVER 				\ ca u na nl ca u / r: xt
    t-COMPARE 0= IF 			\ ca u / r: xt
      R> 				\ ca u xt
      DUP t->flags t-C@ 		\ ca u xt flags
      DUP HF-VISIBLE AND IF 		\ ca u xt flags
        2SWAP 2DROP 			\ xt flags
	HF-IMMEDIATE AND 
	IF 1 ELSE -1 THEN
	EXIT
      ELSE 				\ ca u xt flags
        DROP >R
      THEN
    THEN 				\ ca u
    R> 					\ ca u xt
  REPEAT 				\ ca u image-base
  DROP 2DROP 0 ;


: t-SEARCH-WORDLISTS 			( addr len -- xt true / false )
  2DUP HA-DEF-WL t-SEARCH-WORDLIST 	\ addr len {xt} flag
  IF 
    NIP NIP TRUE 			\ xt true
  ELSE
    HA-ASS-WL t-SEARCH-WORDLIST 	\ {xt} flag
  THEN ;

\ Structure of a node:
\ Offset  	Meaning
\ 0 		next brother 
\ 1 cell 	xt to optimizer away
\ 2 cells 	xt of optimizer
\ 3 cells 	next node (downward)

\ Run down the chain stored in node-var and return the node containing xt and
\ true or false if not found.
: (opt-find-brother) 		 	( xt node-var -- node true / false )
  BEGIN 
    t-@ DUP 				\ xt node do?
  WHILE 				\ xt node
    2DUP 1 [ISEM] CELLS [PREVIOUS] +
    t-@ = 				\ xt node found?
    IF 					\ xt node
      NIP TRUE EXIT
    THEN 				\ xt node
  REPEAT
  2DROP FALSE ; 

\ Make a node.
: (opt-make-node) 			( lastnode xt -- node )
  td-here CODE-SIZE + 			\ ln xt' node
  ROT DUP 				\ xt' node ln ln 
  t-@ td-r,  				\ xt' node ln
  OVER SWAP t-! 			\ xt' node 
  SWAP td-r, 0 td-r, 0 td-r, 		\ node
; 

S" opt2tree" ' COMMENTS >BODY @  	\ addr len wid
SEARCH-WORDLIST 			\ xt flg
0= [IF] 
  S" opt2tree not declared." error-exit
[THEN]
>BODY @ 				\ t-xt
t->DFA t-@ CONSTANT t-opt2tree

ISEM-DEF
\ A pretty 0.
: opt( 				( -- 0 )
  0 ;

\ Placeholder for literal. Use only within opt( )opt:.
: ''# 				( ... n -- .. 0 n+1 )
  0 SWAP 1+ ;

\ Find the word in the target dictionary and append the target xt to the list.
: '' 				( ... n -<name>- ... xt n+1 )
  BL PARSE 			\ ... n addr len
  t-SEARCH-WORDLISTS 		\ ... n {xt} found?
  0= ABORT" Unknown word to optimize away." 
  SWAP 1+ ;

\ Define an optimizer.
: )opt: 				( ... n -- )
  tc-here 				\ ... n xt
  (begin-word) 
  TRUE TO (t-compiling)
  ALSO CSEM ALSO DEFINITIONS OSEM
  (t-interpret)
  (end-word) ret,
  PREVIOUS PREVIOUS DEFINITIONS
  SWAP 					\ ... xt n
  OVER 0= ABORT" no words to optimize."
  t-opt2tree 				\ ... xt n lastnode
  BEGIN 				\ ... xt n lastnode
    OVER 2 +  				\ ... xt n lastnode n+2
    ROLL 				\ ... xt n lastnode xt'
    2DUP SWAP 				\ ... xt n ln xt' xt' ln
    (opt-find-brother) 			\ ... xt n ln xt' ((node true) /false)
    IF 					\ ... xt n lastnode xt' node
      NIP NIP 				\ ... xt n node
    ELSE 				\ ... xt n ln xt'
      (opt-make-node) 			\ ... xt n node
    THEN 				\ ... xt n node
    3 [ISEM] CELLS [PREVIOUS] + 	\ ... xt n lastnode
    SWAP 1- SWAP OVER 0= 		\ ... xt n-1 lastnode fini?
  UNTIL 				\ ... xt 0 lastnode
  NIP 1 [ISEM] CELLS [PREVIOUS] - 	\ xt opt-addr
  DUP t-@ 				\ xt opt-addr old-xt
  ABORT" Trying to define two optimizers for the same sequence."
  t-! ;

: (t-opt2tree) 				( ind node-var -- )
  BEGIN 				\ ind v
    t-@ DUP 				\ ind node cont?
  WHILE 				\ ind node
    OVER SPACES
    DUP 1 [ISEM] CELLS [PREVIOUS] + t-@ \ ind node xt
    DUP 0= IF  				\ ind node xt
      ." -- number -- " 
      DROP
    ELSE 				\ ind node xt
      DUP 
      t->name 
      t-COUNT 
      SWAP TARGET + SWAP TYPE
      SPACE . 
    THEN  				\ ind node
    DUP 2 [ISEM] CELLS [PREVIOUS] + t-@ \ ind node opt?
    IF ."  ***" THEN CR
    2DUP 3 [ISEM] CELLS [PREVIOUS] + 	\ ind node ind son-var
    SWAP 2 + SWAP RECURSE 		\ ind node
  REPEAT 2DROP
;

: t-.opt2tree  				( -- )
  CR 
  ." ##################### tree of optimizers ########################" CR
  0 t-opt2tree [ISEM] (t-opt2tree) [PREVIOUS]
  ." #################################################################" CR
;

OSEM-DEF
: ;opt 					( -- )
  FALSE TO (t-compiling) ; IMMEDIATE
ISEM-DEF
ISEM DEFINITIONS SEAL

