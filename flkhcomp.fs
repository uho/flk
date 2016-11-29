\  FLK compiler words (host versions) 
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

\ $Id: flkhcomp.fs,v 1.16 1998/09/21 11:25:20 root Exp $
\ $Log: flkhcomp.fs,v $
\ Revision 1.16  1998/09/21 11:25:20  root
\ fixed ?DO
\
\ Revision 1.15  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.14  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.13  1998/07/18 10:49:59  root
\ bug corrected
\
\ Revision 1.12  1998/07/16 19:31:37  root
\ changed to conditional near jumps
\
\ Revision 1.11  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.10  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.9  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.8  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.7  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.6  1998/04/24 16:47:39  root
\ DO LOOP corrected
\
\ Revision 1.5  1998/04/16 18:43:39  root
\ flow control improved
\
\ Revision 1.4  1998/04/16 14:09:25  root
\ IF ELSE THEN generates faster code
\
\ Revision 1.3  1998/04/15 18:15:30  root
\ reordered words
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ all words checked and OK
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

META-DEF
\ Factor of AHEAD and IF.
: (ahead) 				( xt -- )
  CFT-orig (new-cs-item)
  (curr-cf-item) CHAR+ 
  allocator-state
  fwd-jmp (curr-cf-item) 3 CELLS + ! 
;
OSEM-DEF

\ See standard.
: IF 					( flag -- ) ( C: -- orig ) 
( OK )
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  ['] n-jz, (ahead)
  ;

: THEN					( -- ) ( C: orig -- )
( OK )
  regalloc-reset
  CFT-orig (check-cs-item)
  FALSE (curr-cf-item) CHAR+ 
  allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 
  resolve-jmp
  (delete-cs-item) 
  ;

: ELSE 					( -- ) ( C: orig1 -- orig2 ) 
  regalloc-reset
  ['] jmp, (ahead) 
  1 (CS-ROLL)
  [OSEM] THEN [PREVIOUS]
  ; 

\ See standard.
: BEGIN 				( -- ) ( C: -- dest )
( OK )
  regalloc-reset
  CFT-dest (new-cs-item)
  (curr-cf-item) CHAR+ allocator-state
  asm-here (curr-cf-item) 3 CELLS + ! 
  ;

\ See standard.
: UNTIL 				( flag -- )  ( C: dest -- )
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
: WHILE 				( C: dest -- orig dest ) ( flag -- )
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
: REPEAT 				( C: orig dest -- ) ( -- )
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
  resolve-jmp 
  ;

\ Data format for CFT-do
\ Offset 	meaning
\ 0		type
\ 1 byte 	allocator state
\ 3 cells 	a1=addr. of inner code
\ 4 cells 	a3=fix-addr for ?DO
\ 5 cells 	last-leave

META-DEF
\ Set's up a new cf-stack item and copies the current allocator state to it.
: (prepare-do-cs-item) 			( -- )
  CFT-do (new-cs-item)
  (curr-cf-item) CHAR+ allocator-state
  asm-here (curr-cf-item) 3 CELLS + !
;
OSEM-DEF

\ See standard.
: DO 					( lim start -- ) ( C: -- do-sys ) 
  regalloc-reset
  req-any 				\ tos0=start=ind=eax
  req-any 				\ tos1=lim=ecx
  $$ 80000000 ## tos1 add,
  tos1 tos0 sub,
  tos1 push,
  tos0 push, 				\ r: lim ind
  2 reg-free
  (prepare-do-cs-item)
  0 (curr-cf-item) 4 CELLS + !
  ;

\ See standard.
: ?DO 					( lim start -- ) ( C: -- do-sys ) 
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
  (curr-cf-item) 4 CELLS + ! 
  ;

\ See standard.
: LOOP 				( -- ) ( C: do-sys -- ) 
( OK )
  CFT-do (check-cs-item)
  regalloc-reset
  req-free
  free0 pop,
  free0 inc,
  free0 push,
  regalloc-reset
  TRUE (curr-cf-item) CHAR+ allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 		\ jmp-addr
  ## n-jno,
  8 ## esp add, 
  (curr-cf-item) 4 CELLS + @ ?DUP IF
    resolve-jmp
  THEN 
  (delete-cs-item) 
  ;

: I					( -- I )
( OK )
  regalloc-reset
  req-free
  0 [esp] free0 mov,
  4 [esp] free0 add,
  0 free>tos ;

: UNLOOP 				( -- )
( OK )
  regalloc-reset
  8 ## esp add, ;

: J					( -- J )
( OK )
  regalloc-reset
  req-free
  8 [esp] free0 mov,
  12 [esp] free0 add,
  0 free>tos ;

CO-DEF
: S" 				( -<">- )
( OK )
  [CHAR] " PARSE			\ addr len
  2>R td-here CODE-SIZE + 
  (opt-add-r-const) 2R>
  TUCK 2>R (opt-add-const)
  2R> td-", ; 


[FORTH] S" TYPE" 
' CSEM >BODY @ SEARCH-WORDLIST 
0= [IF]
 S" Can't find TYPE." error-exit
[THEN]
CONSTANT (type-xt)
[PREVIOUS]

: ." 				( -<">- )
( OK )
  [COMMENTS] S" [PREVIOUS]
  (type-xt) (opt-add-xt) ; 

\ See standard.
: RECURSE 				( -- )
( OK )
  t-lastheader t->CFA t-@ >R
  regalloc-reset
  (end-word) R>
  DWORD ## call,
  ; 

OSEM-DEF

: CASE  				( -- C: case-sys )
  CFT-case (new-cs-item)
  0 (curr-cf-item) CELL+ !
; 

: OF  					( case-sys -- orig case-sys / x -- )
  CFT-case (check-cs-item)
  1 (curr-cf-item) CELL+ +!
  [OSEM] OVER = 
  IF DROP [PREVIOUS]
  1 (CS-ROLL)
; 

: ENDOF 				( orig1 case -- orig2 case )
  CFT-case (check-cs-item)
  1 (CS-ROLL)
  [OSEM] ELSE [PREVIOUS]
  1 (CS-ROLL)
;

: ENDCASE  				( orig1..orign case-sys -- )
  CFT-case (check-cs-item)
  (curr-cf-item) CELL+ @
  (delete-cs-item) 			\ cnt
  [OSEM] DROP [PREVIOUS]
  0 ?DO
    [OSEM] THEN [PREVIOUS]
  LOOP
;


