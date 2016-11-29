\  FLK level 2 compiler
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

\ $Id: flklevel2.fs,v 1.5 1998/08/30 10:50:59 root Exp $
\ $Log: flklevel2.fs,v $
\ Revision 1.5  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.4  1998/07/06 18:01:13  root
\ made literals before calls more efficient
\
\ Revision 1.3  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.2  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.1  1998/06/08 22:18:09  root
\ Initial revision
\


\ Factors for inlining various constants.
: ((inline-r-const)) 			( n -- )
  regalloc-reset
  req-free
  +relocate
  ## free0 mov,
  0 free>tos 
  ;

: ((inline-const)) 			( nr -- )
  regalloc-reset
  req-free
  ## free0 mov,
  0 free>tos 
  ;

\ xt's of unoptimized words
CREATE opt2xt OPT2MAXWORDS CELLS ALLOT
\ Is the cached literal relocated?
CREATE opt2rel OPT2MAXWORDS CELLS ALLOT
\ Values of cached literals
CREATE opt2val OPT2MAXWORDS CELLS ALLOT

\ Ring buffer structure
CREATE opt2rb OPT2MAXWORDS , 0 , 0 , RB-S-EMPTY , 

\ Return the stored value and the relocation flag.
: opt-getlit 				( ind -- x rel? )
  opt2rb RB-INDEX 			\ index
  CELLS DUP opt2val + @ SWAP
  opt2rel + @ ;

\ Change the stored value and its relocation flag.
: opt-setlit 				( x rel? ind -- )
  opt2rb RB-INDEX CELLS 		\ x rel? offs
  TUCK 					\ x offs rel? offs
  opt2rel + ! 				\ x offs
  opt2val + ! ; 

\ Move a cell from foffs to toffs relative to arr.
: (opt-move) 				( toffs foffs arr -- )
  TUCK + @ -ROT + ! ; 

\ Remove one item at from.
: (opt-remove) 				( from -- )
  DUP opt2rb RB-ENTRIES 1- 		\ from from entries
  SWAP - 				\ from cnt
  0 ?DO 				\ from
    DUP opt2rb RB-INDEX CELLS 		\ from t-offs
    OVER 1+ opt2rb RB-INDEX CELLS 	\ from t-o f-offs
    2DUP opt2xt (opt-move)
    2DUP opt2rel (opt-move)
    opt2val (opt-move) 			\ from
    1+
  LOOP DROP 
  opt2rb RB-SHORTEND
  ;

\ Remove the given number of items starting at from.
: opt-remove  				( from cnt -- )
  0 ?DO 
    DUP (opt-remove)
  LOOP DROP ;

\ Structure of a node:
\ Offset  	Meaning
\ 0 		next brother 
\ 1 cell 	xt to optimizer away
\ 2 cells 	xt of optimizer
\ 3 cells 	next node (downward)

\ Find in node and its brothers the one that contains xt and return it and/or
\ a failure flag.
: (opt-flush-find) 			( xt node -- nnode false / true )
  BEGIN 				\ xt node
    IMAGE-BASE OVER <>
  WHILE 				\ xt node
    DUP CELL+ @ 			\ xt node node-xt
    PLUCK = IF 				\ xt node 
      NIP FALSE EXIT
    THEN
    @ 					\ xt next-node
  REPEAT 2DROP TRUE ;

\ Handle the real compilation.
: ((opt-flush-1)) 			( oxt -- )
  IMAGE-BASE OVER <> IF ( optimizer ) 	\ oxt 
    (opt,)
  ELSE ( normal compilation ) 		\ oxt 
    DROP 				\ 
    0 opt2rb RB-INDEX CELLS 		\ offset
    opt2xt OVER + @ 			\ offset xt
    IMAGE-BASE OVER <> IF ( word ) 	\ offset xt
      NIP (compile,)
    ELSE ( constant ) 			\ offs xt
      DROP opt2val OVER + @ 		\ offs x
      opt2rel ROT + @ 			\ x rel?
      IF 
\       ." address " DUP . CR
        ((inline-r-const))
      ELSE
\       ." number " DUP . CR
        ((inline-const)) 
      THEN
    THEN
    opt2rb RB-DELETED
  THEN 
  ;

\ Flush one item from the cache. This word is the heart of the optimizer.
\ Algo: Traverse the brothers for the current xt. When found, remember the
\ optimizer xt, read the next xt and go to the son. Continue until no further
\ brothers can be found or no more xt's are in the cache. If there is a valid
\ optimizer xt execute it. If no optimizer could be found, compile the first
\ xt and remove it.
\ Due the requirement of using this file in both host and target the execution
\ of the optimizer is hidden in (opt,) 
: (opt-flush-1) 			( -- )
  IMAGE-BASE opt2tree @ 		\ oxt node
  opt2rb RB-ENTRIES 0 ?DO 
    I opt2rb RB-INDEX 			\ oxt node ind
    CELLS opt2xt + @ 			\ oxt node xt
    OVER (opt-flush-find)  		\ oxt node nnode flg
    IF ( not found ) 			\ oxt node
      DROP ((opt-flush-1)) UNLOOP EXIT
    THEN  				\ oxt node nnode
    NIP 				\ oxt node
    DUP 2 CELLS + @ 			\ oxt node noxt
    IMAGE-BASE OVER <> IF 		\ oxt node noxt
      ROT DROP 				\ node noxt
      SWAP
    ELSE 				\ oxt node noxt
      DROP
    THEN 				\ oxt node
    3 CELLS + @
  LOOP DROP 				\ oxt
  ((opt-flush-1)) 
  ;

\ Flush the whole cache.
: (opt-flush) 				( -- )
\ ." opt-flush 1 " .S CR
  BEGIN
    opt2rb RB-ENTRIES
  WHILE
    (opt-flush-1)
  REPEAT 
\ ." opt-flush 2 " .S CR
  ;

\ Add an item to the cache.
: (opt-add-item) 			( xt x rel? -- )
  opt2rb RB-STATE@ RB-S-FULL = IF 	\ xt x rel?
     >R >R >R
     (opt-flush-1)
     R> R> R>
  THEN
  opt2rb RB-HEAD-INDEX CELLS 		\ xt x rel? offs
  TUCK opt2rel + ! 			\ xt x offs
  TUCK opt2val + ! 			\ xt offs
  opt2xt + ! 
  opt2rb RB-INSERTED ;
 
\ Add a constant to the cache.
: (opt-add-const) 			( x -- )
  FALSE IMAGE-BASE -ROT (opt-add-item) ;

\ Add a relocated constant to the cache
: (opt-add-r-const) 			( x -- )
  TRUE IMAGE-BASE -ROT (opt-add-item) ;
 
\ Add an xt to the cache
: (opt-add-xt) 				( xt -- )
  FALSE 0 (opt-add-item) ;

\ Interal defintion. Makes start of colon definition easier.
: (begin-word) 
  opt2rb RB-RESET
  regalloc-init ;

\ Interal definition. Eases end of colon definition and calls.
: (end-word)
\  ." end-word 1 " .S CR
  (opt-flush)
  regalloc-flush 
\  ." end-word 2 " .S CR
  ;

