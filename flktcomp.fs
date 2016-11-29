\  FLK compiler words (target versions) 
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

\ $Id: flktcomp.fs,v 1.32 1998/09/21 11:25:20 root Exp $
\ $Log: flktcomp.fs,v $
\ Revision 1.32  1998/09/21 11:25:20  root
\ fixed ?DO
\
\ Revision 1.31  1998/09/17 13:12:23  root
\ fixed COMPILE, (was IMMEDIATE)
\
\ Revision 1.30  1998/09/13 21:28:56  root
\ fixed cf stackk bugs
\
\ Revision 1.29  1998/09/13 16:14:56  root
\ missing (delete-cs-item) added in ;
\
\ Revision 1.28  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.27  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.26  1998/07/18 10:49:59  root
\ bug corrected
\
\ Revision 1.25  1998/07/16 19:31:37  root
\ changed to conditional near jumps
\
\ Revision 1.24  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.23  1998/07/08 19:54:35  root
\ WORD bug removed
\
\ Revision 1.22  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.21  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.20  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.19  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.18  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.17  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.16  1998/05/21 19:24:49  root
\ XForms support
\
\ Revision 1.15  1998/05/17 08:27:09  root
\ script mode, ODOES>
\
\ Revision 1.14  1998/05/09 21:47:05  root
\ words re-arranged
\
\ Revision 1.13  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.12  1998/05/01 18:11:25  root
\ GNU license text added
\ s checked
\
\ Revision 1.11  1998/04/30 09:42:25  root
\ s added.
\
\ Revision 1.10  1998/04/29 18:20:30  root
\ CREATEd words' dfa aligned
\
\ Revision 1.9  1998/04/27 18:41:42  root
\ wrong defaults for flags corrected
\
\ Revision 1.8  1998/04/25 11:02:07  root
\ 2constant fixed (stayed hidden)
\
\ Revision 1.7  1998/04/24 16:47:39  root
\ DO LOOP corrected
\
\ Revision 1.6  1998/04/17 06:23:29  root
\ changed and checked all control flow primitives
\
\ Revision 1.5  1998/04/15 18:15:30  root
\ constant visible flag corrected
\
\ Revision 1.4  1998/04/11 11:55:44  root
\ FORGET/MARKER support added
\
\ Revision 1.3  1998/04/10 14:42:50  root
\ bugs corrected
\
\ Revision 1.2  1998/04/09 09:48:37  root
\ case added
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ Return the parse area as a string.
: (init-parse) 				( -- pa #pa )
  SOURCE >IN @ /STRING ;

\ Increase ind while there is a whitespace at this index.
: (skip-space) 				( addr len ind -- addr len ind' )
  BEGIN
    2DUP > 
    IF 					\ addr len ind
      ROT 2DUP +  			\ len ind addr caddr
      C@ whitespace? 			\ len ind addr spc?
      -TURN -ROT TURN                   \ addr len ind spc?
    ELSE
      FALSE
    THEN
  WHILE 				\ addr len ind
    1+ 
  REPEAT 				\ addr len ind
  ;

\ Advance >IN to first non-space.
: skip-space 				( -- )
  SOURCE >IN @ 				\ addr len ind
  (skip-space)
  >IN !
  2DROP ;

\ Same as -TRAILING, but on the left end of the string.
: bl-skip 				( addr len -- addr' len' )
  0 (skip-space) 			\ addr len ind
  /STRING ;

\ Return the substring ended by the character or the end of the string.
: (parse) 				( char pa #pa -- caddr u )
( OK )
  0 					\ char pa #pa ind
  BEGIN
    2DUP > 
    IF 					\ char pa #pa ind
      FLOCK 				\ char pa #pa ind char
      FLOCK PLUCK + C@  		\ char pa #pa ind char ch
      <>
    ELSE
      FALSE
    THEN
  WHILE 				\ char pa #pa ind
    1+
  REPEAT 				\ char pa #pa ind
  NIP 					\ char pa ind
  ROT DROP 				\ pa ind
  ; 

\ Return the substring ended by a whitespace or the end of the string.
: (bl-parse) 				( pa #pa -- caddr u )
( OK )
  0 					\ pa #pa ind
  BEGIN
    2DUP > 
    IF 					\ pa #pa ind
      PLUCK OVER + C@ whitespace? 	\ pa #pa ind ws?
      INVERT
    ELSE
      FALSE
    THEN
  WHILE 				\ pa #pa ind
    1+
  REPEAT 				\ pa #pa ind
  NIP 					\ pa ind
  ; 

\ See standard.
: PARSE  				( char "ccc<char>" -- c-addr u )
( OK )
  >IN @ TO (last-parse-from)
  BL OVER = IF
    DROP (init-parse) 
    (bl-parse) 				\ c-addr u
  ELSE
    (init-parse) (parse) 
  THEN 
  DUP 1+ >IN +! 
  DUP TO (last-parse-len) ; 

\ Advance to the first character unequal to char.
: skip-char 				( char -- )
  >R
  (init-parse) 				\ pa #pa
  BEGIN
    DUP 0<> 				\ pa #pa room?
  WHILE
    OVER C@ R@ <> IF
      R> DROP 2DROP EXIT
    THEN
    1 >IN +!
    1 /STRING
  REPEAT
  R> 2DROP DROP ;

\ Complain about empty word by throwing an exception.
: empty-def? 				( len -- len)
( OK )
  DUP 0= IF -16 THROW THEN ;

\ See standard.
: CHAR 					( "<spaces>name" -- char )
( OK )
  skip-space
  BL PARSE 				\ c u
  empty-def? DROP C@ ; 

\ Copy the string src,len to the byte counted string dst.
: place 				( src len dst -- )
( OK )
  SWAP 255 MIN SWAP
  2DUP 					\ src len dst len dst
  C!
  CHAR+ SWAP 				\ src dst+1 len
  MOVE ;

CREATE (word-pad) #WORD-PAD CHARS ALLOT
#WORD-PAD CONSTANT #WORD-PAD
\ See standard.
: WORD 					( char "<chars>ccc<char>" -- c-addr )
( OK )
    DUP BL = IF
      DROP skip-space
      (init-parse)
      (bl-parse)
    ELSE
      DUP skip-char
      (init-parse)
      (parse)
    THEN 				\ addr len
    DUP 1+ >IN +! 
    DUP TO (last-parse-len) 
    (word-pad) place
    (word-pad) ; 

0 VALUE (coding)
26 CONSTANT HEADERLEN
-1 VALUE redef-warning
0 RVALUE filename-list
0 VALUE cur-fn

\ Store a string into the code area.
: c-", 					( addr len -- )
( OK )
  255 MIN DUP c-C,
  0 ?DO  				\ addr
    DUP C@ c-C,
    CHAR+
  LOOP DROP ;

\ Generate a header from the given values.
: (buildHeader) 		( back cfa o-cfa flags n-addr n-cnt -- )
( OK )
   CHERE TO lastheader
   >R >R >R >R >R
   c-r, R> c-r, R> c-r, 
   HERE c-r,
   cur-fn c-r,
   (refill-line) c-,
   R> c-c, R> R> 		\ n-addr n-cnt
   c-", ;

\ Hash the string and link it into current dictionary. The first word in the
\ hash line is return by its xt.
: name>hash 				( s l -- back )
( OK )
  (calc-hash) CELLS 
  GET-CURRENT + 			\ hash
  DUP @ SWAP 				\ back hash
  CHERE SWAP ! 				\ back
;

\ Check if the word exists and warn if so. Create a header and fill it with
\ default entries.
: (namedHeader) 			( s l -- )
( OK )
  DUP 0= IF -16 THROW THEN
  redef-warning IF
    2DUP GET-CURRENT SEARCH-WORDLIST 	\ s l xt flag
    IF 					\ s l xt
      >NAME ." Warning: Redefining " 
      COUNT q-TYPE ."  with " 2DUP 
      q-TYPE CR
    THEN
  THEN
  TUCK 					\ l s l
  2DUP >R >R name>hash 			\ l back 
  SWAP HEADERLEN + CHERE + 		\ back cfa 
  IMAGE-BASE 				\ back cfa ocfa
  0  					\ back cfa o-cfa flags
  R> R> 				\ back cfa o-cfa flags s l 
  (buildHeader) ;

\ See standard.
: : 					( C: -<name>- colon-sys )
( OK )
  STATE @ (coding) OR 
  IF -29 THROW THEN
  skip-space
  BL PARSE 				\ na nl
  empty-def?
  CFT-colon (new-cs-item)
  CHERE (curr-cf-item) 3 CELLS + ! 		\ na nl 
\  2DUP ." Word: " q-TYPE .S CR
  (namedHeader)
  (begin-word)
  TRUE STATE !
; 

\ Complain if not compiling by throwing an exception.
: compile-only 				( -- )
( OK )
  STATE @ INVERT IF 
    -14 THROW 
  THEN ;

\ See standard.
: ; 					( C: colon-sys -- )
( OK )
  compile-only
\  ." ending word." .S CR
  (end-word)
  ret,
  CFT-colon (check-cs-item)
\  ." word ended." .S CR
  FALSE STATE ! 			\ 
  (curr-cf-item) 3 CELLS + @ >FLAGS 	\ ffa
  DUP C@ HF-VISIBLE OR 
  SWAP C!
  (delete-cs-item)
  ; IMMEDIATE 

\ See standard.
: COMPILE, 				( xt -- )
( OK )
  compile-only 				\ xt
  DUP >FLAGS C@ HF-OIMMEDIATE AND IF 	\ xt
    ( word's osem is immediate)
    DUP >OCFA @ 			\ xt osem
    DUP IMAGE-BASE = IF 		\ xt osem
      DROP
      ." Internal error: OIMMEDIATE word '"
      >NAME COUNT TYPE
       ." ' has no osem!" CR
       ABORT
    THEN 				\ xt osem
    NIP (EXECUTE)
  ELSE
    (opt-add-xt)
  THEN
;

\ See standard.
: IMMEDIATE 				( -- )
( OK )
  lastheader 				\ xt
  DUP 0= IF ILLEGAL-IMMEDIATE-EXCEPTION THROW THEN
  >FLAGS DUP C@ HF-IMMEDIATE OR
  SWAP C! ;

\ Return a dummy string for noname words.
: (unnamed-name) S" unnamed" ;
( OK )
    
\ See standard.
: :NONAME 				( -- xt ) ( C: -- colon-sys )
( OK )
  CHERE DUP 				\ xt hdr
  CFT-colon (new-cs-item)
  (curr-cf-item) CELL+ !
  0 					\ back 
  (unnamed-name) NIP 
  HEADERLEN + CHERE + 			\ back cfa
  IMAGE-BASE 0 (unnamed-name) 		\ back cfa o-cfa flags n-addr n-cnt
  (buildHeader)
  (begin-word)
  TRUE STATE ! ;

\ See standard.
: RECURSE 				( -- )
( OK )
  compile-only
  lastheader >CFA @ >R
  regalloc-reset
  (end-word) R>
  DWORD ## call,
  ; IMMEDIATE 

\ See standard.
: ' 					( -<name>- xt )
( OK )
  skip-space
  BL PARSE 
  empty-def?
  SEARCH-WORDLISTS 			\ xt flag
  0= IF -13 THROW THEN 			\ xt
; 

\ See standard.
: LITERAL 				( x -- )
( OK )
  compile-only
  (opt-add-const)
; IMMEDIATE 

\ A relocated literal. See LITERAL.
: RLITERAL 				( x -- )
( OK )
  compile-only
  (opt-add-r-const)
; IMMEDIATE 

\ See standard.
: 2LITERAL 				( x0 x1 -- )
( OK )
  compile-only
  >R
  (opt-add-const)
  R>
  (opt-add-const)
; IMMEDIATE 

\ See standard.
: SLITERAL 				( ca1 u -- )
( OK )
  compile-only
  2>R
  HERE (opt-add-r-const)
  2R> 					\ ca1 u
  TUCK 2>R (opt-add-const) 2R>
  0 ?DO
    DUP C@ C,
    CHAR+
  LOOP DROP
; IMMEDIATE 

\ See standard.
: [COMPILE] 				( -<name>- )
( OK )
  compile-only
  ' COMPILE,
; IMMEDIATE 

\ See standard.
: ['] 					( -<name>- )
( OK )
  compile-only
  ' RLITERAL
; IMMEDIATE 

CREATE S"-buffer #WORD-PAD CHARS ALLOT
\ See standard.
: S" 					( -ccc"-  addr len / )
( OK )
  34 PARSE 				\ addr len
  STATE @ IF
    SLITERAL 				\ 
  ELSE
    #WORD-PAD MIN TUCK 			\ len addr len
    S"-buffer SWAP CMOVE 		\ len
    S"-buffer SWAP 			\ addr len
  THEN
; IMMEDIATE 

\ See standard.
: C" 					( -ccc"- )
( OK )
  compile-only
  34 PARSE 255 MIN
  HERE (opt-add-r-const)
  DUP C,
  0 ?DO 
    DUP C@ C,
    CHAR+
  LOOP DROP
; IMMEDIATE 

\ See standard.
: ." 					( -ccc"- )
( OK )
  compile-only
  34 PARSE 				\ addr len
  SLITERAL 				\ 
  ['] TYPE COMPILE,
; IMMEDIATE 

\ See standard.
: POSTPONE 				( -<name>- )
( OK )
  compile-only 
  ' 					\ xt
  DUP >FLAGS C@ HF-IMMEDIATE AND 	\ xt imm?
  IF
    COMPILE,
  ELSE
    RLITERAL 
    ['] COMPILE, COMPILE,
  THEN
; IMMEDIATE 

VARIABLE abort"-len
VARIABLE abort"-addr

\ Work-around, because VARIABLEs can't be ticked.
: (abort"-len) abort"-len ;

\ Work-around, because VARIABLEs can't be ticked.
: (abort"-addr) abort"-addr ;

\ See standard.
: ABORT" 				( -ccc"- )
( OK )
  compile-only
  (opt-flush)
  34 PARSE 				\ addr len
  2>R
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  0 jz,
  save-allocator 			\ allocator
  save-labels 				\ allocator labels 
    2R> 				\ allocator labels addr len
    HERE -ROT TUCK 			\ allocator labels addr2 len addr len
    0 ?DO
      DUP C@ C,
      CHAR+
    LOOP DROP 				\ allocator labels addr2 len
    SWAP (opt-add-r-const) (opt-add-const)
    ['] (abort"-len)  COMPILE,
    ['] !             COMPILE,
    ['] (abort"-addr) COMPILE,
    ['] !             COMPILE,
    -2 LITERAL
    ['] THROW         COMPILE,
    (opt-flush)
    restore-labels
  0 $:
  restore-allocator
; IMMEDIATE 

\ See standard.
: >BODY 				( xt -- dfa )
  DUP >FLAGS C@ HF-CREATED AND 0= 
  IF -31 THROW THEN 			\ xt
  >DFA @ ;

\ See standard.
: TO 					( -<name>- )
( OK )
  ' >BODY 				\ dfa
  STATE @ IF
    RLITERAL
    ['] ! COMPILE,
  ELSE
    !
  THEN ; IMMEDIATE 

\ Add x to the value name.
: +TO 					( x -<name>- )
( OK )
  ' >BODY 				\ dfa
  STATE @ IF
    RLITERAL
    ['] +! COMPILE,
  ELSE
    +!
  THEN ; IMMEDIATE 

\ Increase the value x by one.
: TO++ 					( -<name>- )
( OK )
  compile-only
  1 LITERAL
  ' >BODY 				\ dfa
  RLITERAL
  ['] +! COMPILE,
; IMMEDIATE 

\ Decrease the value x by one.
: TO-- 					( -<name>- )
( OK )
  compile-only
  -1 LITERAL
  ' >BODY 				\ dfa
  RLITERAL
  ['] +! COMPILE,
 ; IMMEDIATE 
 
\ Set the visible flag in the last created header.
: make-last-visible 			( -- )
  lastheader >FLAGS DUP C@ HF-VISIBLE OR SWAP C! ;

\ Set the O-IMMEDIATE flag in the last created header.
: make-last-oimmediate 			( -- )
  lastheader >FLAGS DUP C@ HF-OIMMEDIATE OR SWAP C! ;

0 VALUE lastcreated-ret

\ Create the word with the given name.
: $CREATE 				( addr len -- )
  empty-def? 				\ addr len
  (namedHeader)
  make-last-visible
  make-last-oimmediate
  lastheader >FLAGS DUP C@ HF-CREATED OR SWAP C!
  ALIGN
  HERE lastheader >dfa !
  ( interpret. sem.)
  (begin-word)
  req-free
  HERE +relocate ## free0 mov,
  0 free>tos
  (opt-flush)
  regalloc-flush
  CHERE TO lastcreated-ret
  ret,
  ( optimizer sem.)
  CHERE 				\ optsem
  (begin-word)
  ( fake compilation )
  STATE @ TRUE STATE ! 			\ optsem state
  HERE RLITERAL
  (opt-flush)
  ['] (opt-add-r-const) COMPILE,
  (end-word)
  ret,
  STATE !
  lastheader >OCFA !  ; 

\ See standard.
: CREATE 				( -<name>- )
( OK )
  skip-space
  BL PARSE 
  $CREATE ; 

\ See standard.
: VARIABLE
( OK )
  CREATE 1 CELLS ALLOT ; 
 
\ See standard.
: 2VARIABLE
( OK )
  CREATE 2 CELLS ALLOT ; 
 
\ See standard.
: [CHAR]   				( -<name>- )
( OK )
  CHAR LITERAL ; IMMEDIATE 

\ Primitive for CONSTANT and RCONSTANT.
: (CONSTANT) 				( x relo? -<name>- )
  skip-space
  BL PARSE 				\ x relo? addr len
  empty-def?
  (namedHeader) 			\ x relo?
  make-last-visible
  make-last-oimmediate
  ( interp. sem. )
  (begin-word) 				\ x relo?
  2DUP 					\ x relo? x relo?
  req-free
  IF +relocate THEN 			\ x relo? x
  ## free0 mov, 			\ x relo?
  0 free>tos 
  (end-word)
  ret,
  ( opt. sem.)
  (begin-word)
  STATE @ TRUE STATE ! 			\ x relo? state
  CHERE  				\ x relo? state optsem
  2SWAP 				\ state optsem x relo?
  IF 
    RLITERAL 
    ['] (opt-add-r-const) COMPILE,
  ELSE 
    LITERAL 
    ['] (opt-add-const) COMPILE,
  THEN 					\ state optsem relo?
  (end-word)
  ret,
  SWAP
  STATE !
  lastheader >OCFA !
; 

\ See standard.
: CONSTANT 				( x -<name>- )
  FALSE (CONSTANT) ; 

\ Relocated constant.
: RCONSTANT TRUE (CONSTANT) ; 

\ See standard.
: 2CONSTANT 				( xl xh -<name>- )
( OK )
  skip-space
  BL PARSE 				\ xl xh addr len
  empty-def?
  (namedHeader) 			\ xl xh
  make-last-visible
  make-last-oimmediate
  ( interp. sem. )
  (begin-word)
  2DUP 
  req-free
  req-free
  ## free1 mov, 			\ free1=xh
  ## free0 mov, 			\ free0=xl
  0 free>tos 
  1 free>tos 
  (end-word)
  ret,
  ( opt. sem.)
  (begin-word)
  STATE @ TRUE STATE ! 			\ xl xh state
  CHERE  				\ xl xh state optsem
  2SWAP 2LITERAL 			\ state optsem
  ['] SWAP COMPILE,
  ['] (opt-add-const) COMPILE,
  ['] (opt-add-const) COMPILE,
  (end-word)
  ret,
  SWAP STATE !
  lastheader >OCFA ! ; 

\ Primitive for VALUE and RVALUE. 
: (value) 				( x rel? -<name>- )
  skip-space
  BL PARSE 				\ x rel? addr len
  empty-def?
  (namedHeader) 			\ x rel?
  lastheader >FLAGS DUP C@ HF-CREATED OR SWAP C!
  make-last-visible
  make-last-oimmediate
  ( interp. sem. )
  (begin-word)
  regalloc-reset
  req-free
  HERE #[] free0 mov,
  0 free>tos
  (end-word)
  ret,
  ( opt. sem.)
  (begin-word)
  STATE @ TRUE STATE ! 			\ x rel? state
  CHERE  				\ x rel? state optsem
  HERE RLITERAL
  ['] (opt-add-r-const) COMPILE,
  ['] @ RLITERAL
  ['] (opt-add-xt) COMPILE,
  (end-word)
  ret,
  SWAP STATE !
  lastheader >OCFA ! 			\ x rel?
  HERE lastheader >dfa !
  IF r, ELSE , THEN ; 

\ See standard.
: VALUE 				( x -<name>- )
( OK )
  FALSE (value) ; 

\ Relocated VALUE.
: RVALUE 				( x -<name>- )
( OK )
  TRUE (value) ; 

\ Factor of AHEAD and IF.
: (ahead) 				( xt -- )
  CFT-orig (new-cs-item)
  (curr-cf-item) CHAR+ allocator-state
  fwd-jmp (curr-cf-item) 3 CELLS + ! 
;

: (then) 				( -- )
  regalloc-reset
  CFT-orig (check-cs-item)
  FALSE (curr-cf-item) CHAR+ allocator-rebuild 	\ 
  (curr-cf-item) 3 CELLS + @ resolve-jmp 	\ 
  (delete-cs-item) ;

\ See standard.
c: AHEAD  				( C: -- orig )
  regalloc-reset
  ['] jmp, (ahead) ; 

\ See standard.
c: IF 					( flag -- ) ( C: -- orig ) 
( OK )
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  ['] n-jz, (ahead) ;

\ See standard.
c: THEN 					( -- ) ( C: orig -- )
( OK )
  (then) ;

\ See standard.
c: ELSE 					( -- ) ( C: orig1 -- orig2 ) 
( OK )
  regalloc-reset
  ['] jmp, (ahead) 
  1 (CS-ROLL)
  (then)
  ;

\ See standard.
c: BEGIN 				( -- ) ( C: -- dest )
( OK )
  regalloc-reset
  CFT-dest (new-cs-item)
  (curr-cf-item) CHAR+ allocator-state
  asm-here (curr-cf-item) 3 CELLS + !  ;

\ See standard.
c: UNTIL 				( flag -- )  ( C: dest -- )
( OK )
  CFT-dest (check-cs-item)
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  TRUE (curr-cf-item) CHAR+ allocator-rebuild 		
  (curr-cf-item) 3 CELLS + @ 		\ jmp-addr
  0 jnz,
  ## jmp,
  0 $:
  (delete-cs-item)
  ;

\ See standard.
c: AGAIN 				( -- ) ( C: dest -- )
( OK )
  CFT-dest (check-cs-item)
  regalloc-reset
  FALSE (curr-cf-item) CHAR+ 
  allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 		\ jmp-addr
  ## jmp, 
  (delete-cs-item) ; 

\ See standard.
c: WHILE 				( C: dest -- orig dest ) ( flag -- )
( OK )
  CFT-dest (check-cs-item)
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  ['] n-jz, (ahead) 			\ dest orig
  1 (CS-ROLL)
  ;

\ See standard.
c: REPEAT 				( C: orig dest -- ) ( -- )
( OK )
  CFT-dest (check-cs-item)
  regalloc-reset
  FALSE (curr-cf-item) CHAR+ 
  allocator-rebuild 			\ C: orig dest
  (curr-cf-item) 3 CELLS + @ 		\ jmp-addr / C: orig dest 
  (delete-cs-item) 			\ jmp-addr / C: orig
  ## jmp,
  CFT-orig (check-cs-item)
  (curr-cf-item) CHAR+ allocator-store
  (curr-cf-item) 3 CELLS + @
  (delete-cs-item) 			\ jmp-addr
  resolve-jmp ;

\ Data format for CFT-do
\ Offset 	meaning
\ 0 		type
\ 1 byte 	allocator state
\ 3 cells 	a1=addr. of inner code
\ 4 cells 	a3=fix-addr for ?DO
\ 5 cells 	last-leave

\ Set's up a new cf-stack item and copies the current allocator state to it.
: (prepare-do-cs-item) 			( -- )
  CFT-do (new-cs-item)
  do-state (curr-cf-item) CHAR+ 11 MOVE
  do-state allocator-state
  asm-here (curr-cf-item) 3 CELLS + !
  last-leave (curr-cf-item) 5 CELLS + ! 
  IMAGE-BASE TO last-leave 
;

\ See standard.
c: DO 					( lim start -- ) ( C: -- do-sys ) 
( OK )
  regalloc-reset
  req-any 				\ tos0=start=ind=eax
  req-any 				\ tos1=lim=ecx
  $$ 80000000 ## tos1 add,
  tos1 tos0 sub,
  tos1 push,
  tos0 push, 				\ r: lim ind
  2 reg-free
  (prepare-do-cs-item)
  0 (curr-cf-item) 4 CELLS + ! ;

\ See standard.
c: ?DO 					( lim start -- ) ( C: -- do-sys ) 
( OK )
  regalloc-reset
  req-any 				\ tos0=start=ind=eax
  req-any 				\ tos1=lim=ecx
  tos1 tos0 cmp,
  ['] n-je, fwd-jmp 			\ a3
  $$ 80000000 ## tos1 add,
  tos1 tos0 sub,
  tos1 push,
  tos0 push, 				\ a3 / r: cnt ind
  2 reg-free
  (prepare-do-cs-item)
  (curr-cf-item) 4 CELLS + ! ;

\ Resolve current leave chain.
: (resolve-leave) 			( -- )
  last-leave
  BEGIN 				\ leave-addr
    DUP IMAGE-BASE <> 			\ la cont?
  WHILE 				\ la
    DUP @ 				\ la last-la
    SWAP resolve-jmp
  REPEAT 				\ la
  DROP 
  (curr-cf-item) 5 CELLS + @
  TO last-leave 
  (curr-cf-item) CHAR+ do-state 11 MOVE
  ;

\ See standard.
c: LOOP 				( -- ) ( C: do-sys -- ) 
( OK )
  CFT-do (check-cs-item)
  regalloc-reset
  req-free
  free0 pop,
  free0 inc,
  free0 push,
  regalloc-reset
  TRUE do-state allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 		\ jmp-addr
  ## n-jno,
  (resolve-leave)
  8 ## esp add, 
  (curr-cf-item) 4 CELLS + @ ?DUP IF
    resolve-jmp
  THEN 
  (delete-cs-item) 
  ;

\ See standard.
c: +LOOP 				( inc -- ) ( C: do-sys -- )
( OK )
  CFT-do (check-cs-item)
  regalloc-reset
  req-any
  req-free
  free0 pop,
  tos0 free0 add,
  free0 push,
  1 reg-free
  TRUE do-state allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 		\ jmp-to
  ## n-jno,
  (resolve-leave)
  8 ## esp add, 
  (curr-cf-item) 4 CELLS + @ ?DUP IF
    resolve-jmp
  THEN 
  (delete-cs-item) ;

\ See standard.
c: LEAVE 				( -- ) ( R: do-sys -- ) ( C: -- )
( OK )
  regalloc-reset
  FALSE do-state allocator-rebuild 
  ['] jmp, fwd-jmp  			\ fix-addr
  last-leave 				\ fa last-fa
  OVER ! 				\ fa
  TO last-leave
  ;

\ See standard.
c: I 					( -- I )
( OK )
  regalloc-reset
  req-free
  0 [esp] free0 mov,
  4 [esp] free0 add,
  0 free>tos ;

\ See standard.
c: UNLOOP 				( -- )
( OK )
  regalloc-reset
  req-free
  8 ## esp add, ;

\ See standard.
c: J 					( -- J )
( OK )
  regalloc-reset
  req-free
  8 [esp] free0 mov,
  12 [esp] free0 add,
  0 free>tos ; 

\ Change the ex. sem of last created word by removing the optimizer code and
\ changing the ret to a jmp
: (does>) 				( jmp-addr -- )
( OK )
  lastheader >FLAGS C@ DUP HF-CREATED AND 0= IF -31 THROW THEN
  HF-OIMMEDIATE INVERT AND lastheader >FLAGS C!
  lastheader 				\ jmp-addr xt
  >ocfa IMAGE-BASE SWAP ! 		\ jmp-addr
  lastcreated-ret TO CHERE 		\ jmp-addr
  ## jmp, 
  lastcreated-ret relocation-drop ;

\ Remove the ex. sem. of the last created word and make it the opt. sem. The
\ ret is changed to a jmp.
: (odoes>) 				( jmp-addr -- )
  lastheader >FLAGS C@ DUP HF-CREATED AND 0= IF -31 THROW THEN
  HF-OIMMEDIATE INVERT AND lastheader >FLAGS C!
  lastheader 				\ jmp-addr xt
  DUP >CFA @ OVER >OCFA ! 		\ jmp-addr xt
  >CFA IMAGE-BASE SWAP !
  lastcreated-ret TO CHERE 		\ jmp-addr
  ## jmp, 
  lastcreated-ret relocation-drop ;

\ compile a call to (does>) and a ret, reset register allocator
: (o/DOES>) 				( C: do-sys1 odoes? -- do-sys2 )
( OK )
  0 RLITERAL 
  >R (opt-flush) R>
  CHERE 4 - 				\ odoes? fix-addr
  SWAP IF
    ['] (odoes>)
  ELSE
    ['] (does>) 
  THEN COMPILE,
  (end-word)
  ret,
  (begin-word) 			\ fix-addr
  regalloc-reset
  CHERE SWAP !
; 

\ See standard.
: DOES> FALSE (o/DOES>) ; IMMEDIATE

\ Support for writing optimizers. " CREATE .. ODOES> ..." is equivalent to 
\ " CREATE IMMEDIATE .. DOES> compile-only ...".
: ODOES> TRUE (o/DOES>) ; IMMEDIATE

\ See standard.
: CODE 					( "<spaces>name" -- )
( OK )
  (coding) STATE @ OR IF
    -29 THROW
  THEN
  skip-space BL PARSE 			\ addr len
  empty-def? (namedHeader)
  (begin-word)
  TRUE TO (coding) 
  ALSO ASSEMBLER
  reset-labels ;

\ End a CODE oder ;CODE sequence.
: ENDCODE 				( -- )
( OK )
  PREVIOUS 
  (coding) INVERT 
  IF -14 THROW THEN
  (end-word)
  ret,
  FALSE TO (coding) 
  lastheader >FLAGS DUP 
  C@ HF-VISIBLE OR SWAP C! ;

\ See standard.
: ;CODE 				( C: colon-sys -- )
( OK )
  0 RLITERAL CHERE 4 - 			\ fix-addr
  ['] (does>) COMPILE,
  (end-word)
  regalloc-init 			\ fix-addr
  CHERE SWAP !
  TRUE TO (coding)
; IMMEDIATE

\ See standard.
: CASE  				( -- C: case-sys )
  (opt-flush)
  CFT-case (new-cs-item)
  0 (curr-cf-item) 3 CELLS + !
; IMMEDIATE

\ See standard.
: OF  					( case-sys -- orig case-sys / x -- )
  (opt-flush)
  CFT-case (check-cs-item)
  1 (curr-cf-item) 3 CELLS + +!
  ['] OVER COMPILE, 
  ['] =    COMPILE,
  ['] IF   COMPILE,
  ['] DROP COMPILE,
  (opt-flush)
  1 (CS-ROLL)
; IMMEDIATE

\ See standard.
: ENDOF 				( orig1 case -- orig2 case )
  (opt-flush)
  CFT-case (check-cs-item)
  1 (CS-ROLL)
  ['] ELSE COMPILE,
  (opt-flush)
  1 (CS-ROLL)
; IMMEDIATE

\ See standard.
: ENDCASE  				( orig1..orign case-sys -- )
  (opt-flush)
  CFT-case (check-cs-item)
  (curr-cf-item) 3 CELLS + @
  (delete-cs-item) 			\ cnt
  ['] DROP COMPILE,
  0 ?DO
     ['] THEN COMPILE,
  LOOP
  (opt-flush)
; IMMEDIATE


\ Create a memory variable with nchars characters room + character count
: $VARIABLE 			( nchars -<name>- )
  CREATE 0 , CHARS ALLOT
;

