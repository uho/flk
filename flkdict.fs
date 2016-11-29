\  FLK basic dictionary management
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

\ $Id: flkdict.fs,v 1.18 1998/08/30 10:50:59 root Exp $
\ $Log: flkdict.fs,v $
\ Revision 1.18  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.17  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.16  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.15  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.14  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.13  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.12  1998/05/21 19:24:49  root
\ XForms support
\
\ Revision 1.11  1998/05/09 21:47:05  root
\ relocation-drop fixed (corrects a bug in "r, does>" )
\
\ Revision 1.10  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.9  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.8  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.7  1998/04/29 18:20:30  root
\ better hash function
\
\ Revision 1.6  1998/04/24 16:47:39  root
\ float support corrected
\
\ Revision 1.5  1998/04/15 18:15:30  root
\ align changed for float support
\
\ Revision 1.4  1998/04/11 11:55:44  root
\ FORGET/MARKER support added
\
\ Revision 1.3  1998/04/10 14:42:50  root
\ bugs corrected
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ fixed SEARCH-WORDLIST (visibility check added)
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

#BUCKETS CONSTANT #BUCKETS

\ Calculate the dictionary hash function from the given string. The return
\ hash is limited to 0..#BUCKETS-1.
: (calc-hash) 				( ca u -- hash )
( OK )
  OVER C@ 2* 				\ ca u hash
  OVER 1 > IF 				\ ca u hash
    ROT CHAR+ C@ + DUP 2* + 		\ u hash
  ELSE
    ROT DROP 				\ u hash
  THEN
  +
  #BUCKETS MOD ;

[META] HF-IMMEDIATE [PREVIOUS] CONSTANT HF-IMMEDIATE
[META] HF-OIMMEDIATE [PREVIOUS] CONSTANT HF-OIMMEDIATE
[META] HF-VISIBLE [PREVIOUS] CONSTANT HF-VISIBLE
[META] HF-CREATED [PREVIOUS] CONSTANT HF-CREATED

\ Are we case insensitive?
VARIABLE CAPS

\ Case sensitive/insensitive compare depending on the value in CAPS.
: (swl-COMPARE)
( OK )
  CAPS @ IF
    CAPS-COMPARE
  ELSE
    COMPARE
  THEN ;

\ Print the name of the wordlist wid.
: .VOC 					( wid -- )
( OK )
  DUP #BUCKETS CELLS + CELL+ COUNT 	\ wid addr len
  DUP 0= IF 				\ wid add len
    2DROP ." unnamed(" .addr ." ) "
  ELSE
    TYPE SPACE DROP
  THEN ;

\ Which wordlists are known?
: VOCS 					( -- )
  ." Known wordlists: "  CR
  voc-link BEGIN 			\ wid
    DUP .VOC #BUCKETS CELLS + @
    DUP IMAGE-BASE =
  UNTIL DROP CR
;

\ See standard.
: DEPTH SP-BASE SP@ - 2 RSHIFT 1- ;
( OK )

\ See standard.
: .S 					( -- )
( OK )
  BASE @ >R 
  DEPTH 				\ depth
  DECIMAL
  DUP ." <" 1 .R ." > " 		\ depth
  8 MIN DUP 0 ?DO 			\ ind
    DUP PICK 
    . 1-
  LOOP DROP CR 
  R> BASE ! ;

\ See standard.
: SEARCH-WORDLIST 			( c-addr u wid -- 0 | xt 1 | xt -1 )
( OK )
  -ROT 					\ wid ca u
  2DUP (calc-hash) 			\ wid ca u hash
  CELLS  				\ wid ca u offs
  TURN + 				\ ca u &head
  BEGIN 				\ ca u &head
    @ 					\ ca u xt
    DUP 				\ ca u xt xt
    IMAGE-BASE <>
  WHILE 				\ ca u xt
    DUP >NAME COUNT 			\ ca u xt na nl
    ROT >R 				\ ca u na nl / r: xt
    2OVER 				\ ca u na nl ca u / r: xt
    (swl-COMPARE) 0= IF 		\ ca u / r: xt
      R> 				\ ca u xt
      DUP >FLAGS C@ 			\ ca u xt flags
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

\ Storage area for wid's.
#IN-ORDER CONSTANT #IN-ORDER
CREATE ((order-field)) #IN-ORDER CELLS ALLOT
VARIABLE ((order-cnt))

CREATE ((unique-order)) #IN-ORDER CELLS ALLOT
VARIABLE ((unique-cnt))

\ Put the given wordlist-id into ((unique-order)) if it is not in it yet.
: (unique-set-order) 			( wid -- )
  ((unique-cnt)) @ 0 ?DO
    I CELLS ((unique-order)) + @ 	\ wid wid-u
    OVER = IF 				\ wid
      DROP UNLOOP EXIT
    THEN
  LOOP
  ((unique-order)) ((unique-cnt)) @ CELLS + !
  1 ((unique-cnt)) +!
;

\ See standard.
: SET-ORDER 				( widn ... wid1 n -- )
( OK )
  DUP #IN-ORDER  			\ ... n n max
  < INVERT IF -49 THROW THEN 		\ .. n
  DUP 0= IF -50 THROW THEN 		\ .. n
  DUP ((order-cnt)) ! 
  0 ((unique-cnt)) !
  0 DO 					\ ... wid
    DUP (unique-set-order)
    I CELLS 
    ((order-field)) + !
  LOOP ;

\ See standard.
: GET-ORDER 				( -- widn ... wid1 n )
( OK )
  ((order-cnt)) @ 			\ n
  0 ?DO
    ((order-cnt)) @ 1- I - CELLS 
    ((order-field)) + @
  LOOP
  ((order-cnt)) @ ;

\ Which wordlist is searched first?
: TOP-VOC 				( -- wid )
( OK )
  ((order-field)) @ ; 

VARIABLE (current)

\ See standard.
: SET-CURRENT 				( wid -- )
( OK )
  (current) ! ;

\ See standard.
: GET-CURRENT 				( -- wid )
( OK )
  (current) @ ;

\ See standard.
: ORDER 				( -- )
( OK )
  ." Search order: "
  GET-ORDER 0 DO .VOC LOOP CR
  ." current: "
  GET-CURRENT .VOC CR ;

\ Same function as FIND, just more useful interface ( and faster too ).
: SEARCH-WORDLISTS  			( c-addr u -- 0 / xt 1 / xt -1 )
( OK )
  ((unique-cnt)) @ 0 DO 		\ c u 
    2DUP 				\ c u c u
    I CELLS ((unique-order)) + @  	\ c u c u wid
    SEARCH-WORDLIST 			\ c u ( 0 / xt imm ? )
    ?DUP IF 				\ c u xt imm?
      2SWAP 2DROP 			\ xt imm?
      UNLOOP EXIT 
    THEN 				\ c u
  LOOP
  2DROP 0 ;

\ See standard.
: FIND 			( c-addr -- c-addr 0  |  xt 1  |  xt -1 )
( OK )
  DUP 					\ co
  COUNT 				\ co c u
  SEARCH-WORDLISTS 			\ co ( 0 / xt flag )
  DUP IF
    ROT DROP
  THEN ;

' NOOP RVALUE ALLOT-HOOK
' NOOP RVALUE CALLOT-HOOK

\ See standard.
: ALLOT 				( n -- )
( OK )
  DUP 0> IF
    DUP UNUSED < INVERT IF
      -8 THROW
    THEN
  THEN
  +TO HERE ALLOT-HOOK EXECUTE ;

\ Store one item into the relocation table.
: relocation! 				( x -- )
( OK )
  RELOCATION-TABLE@ SWAP IMAGE-BASE - []+= RELOCATION-TABLE! ;

\ remove all entries higher than addr but only in code area
: relocation-drop 			( addr -- )
  IMAGE-BASE - 				\ rel-addr
  RELOCATION-TABLE@ 			\ ra rt
  DUP []# 				\ ra rt #rt
  BEGIN
    1- 2DUP 				\ ra rt ind rt ind
    []@ 				\ ra rt ind cont
    FLOCK OVER 				\ ra rt ind cont ra cont
    <
  WHILE 				\ ra rt ind cont
    HA-CODESIZE @ < IF 			\ ra rt ind
      ( cont is in code area )
      2DUP []-delete
    THEN
  REPEAT 				\ ra rt ind cont
  2DROP 2DROP ;

\ See standard.
: , 					( x -- )
( OK )
  HERE 1 CELLS ALLOT ! ;

\ See standard.
: C, 					( char -- )
( OK )
  HERE 1 CHARS ALLOT C! ;

\ Same as , but relocate the value.
: r, 					( x -- )
( OK )
  HERE relocation! , ;

\ Same as ALLOT but in code area.
: CALLOT 				( n -- )
( OK )
  DUP 0> IF
    DUP CUNUSED < INVERT IF
      -8 THROW
    THEN
  THEN
  +TO CHERE CALLOT-HOOK EXECUTE ;

\ Same as , but in code area.
: c-, 					( x -- )
( OK )
  CHERE 1 CELLS CALLOT ! ;

\ Same as C, but in code area.
: c-C, 					( char -- )
( OK )
  CHERE 1 CHARS CALLOT C! ;

\ Same as r, but in code area.
: c-r, 					( x -- )
( OK )
  CHERE relocation! c-, ;

\ The assembler uses different names.
: asm-r,   c-r, ;

\ The assembler uses different names.
: asm-,    c-,  ;

\ The assembler uses different names.
: asm-c,   c-C, ;

\ The assembler uses different names.
: asm-here CHERE ;

\ The assembler uses different names.
: asm-c!   C! ;

\ The assembler uses different names.
: asm-!    ! ;

\ Align primitive. Used as an short-cut for the various ALIGNs (FLOATING POINT
\ wordset).
: (align) 				( new-here -- ) 
  DUP HERE-LIMIT >= IF -8 THROW THEN
  TO HERE ;

\ See standard.
: ALIGN  				( -- )
( OK )
  HERE ALIGNED (align) ;

\ See standard.
: WORDLIST 				( -- wid )
( OK )
  HERE
  #BUCKETS 0 DO
     IMAGE-BASE r,
  LOOP 
  voc-link r,
  DUP TO voc-link
  0 C, ( name ) ;

\ See standard.
: ALSO  				( -- ) 
( OK )
  GET-ORDER OVER SWAP 1+ SET-ORDER ;
  
\ See standard.
: PREVIOUS  				( -- ) 
( OK )
  GET-ORDER 				\ wid... n
  DUP 1 > IF
    1- NIP
  THEN 
  SET-ORDER ;
  
\ See standard.
: ONLY 				( -- )
( OK )
  FORTH-WORDLIST 1 SET-ORDER ;

\ See standard.
: FORTH 				( -- )
( OK )
  GET-ORDER NIP FORTH-WORDLIST SWAP SET-ORDER ;

\ See standard.
: ASSEMBLER 				( -- )
( OK )
  GET-ORDER NIP ASSEMBLER-WORDLIST SWAP SET-ORDER ;

\ See standard.
: EDITOR 				( -- )
( OK )
  GET-ORDER NIP EDITOR-WORDLIST SWAP SET-ORDER ;

\ Put the environment wordlist on top of the search order. This is not allowed
\ in a standard program and is only nessesary whenever new environment query
\ items arrive.
: ENVIRONMENT 				( -- )
( OK )
  GET-ORDER NIP ENVIRONMENT-WORDLIST SWAP SET-ORDER ;

\ Drop n items and n too.
: nDROP 				( n*x n -- )
( OK )
  0 ?DO
    DROP
  LOOP ;

\ See standard.
: DEFINITIONS 				( -- )
( OK )
  TOP-VOC SET-CURRENT ;

