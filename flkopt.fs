\  FLK level 2 optimizer
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

\ $Id: flkopt.fs,v 1.8 1998/09/13 18:55:51 root Exp $
\ $Log: flkopt.fs,v $
\ Revision 1.8  1998/09/13 18:55:51  root
\ fixed optimizers for cf stack
\
\ Revision 1.7  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.6  1998/07/18 10:49:59  root
\ bug corrected
\
\ Revision 1.5  1998/07/16 19:31:37  root
\ changed to conditional near jumps
\
\ Revision 1.4  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.3  1998/07/06 18:01:13  root
\ new optimizers ( IF/WHILE)
\
\ Revision 1.2  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.1  1998/07/03 20:57:50  root
\ Initial revision
\

: (_sizing) 				( prim-xt -- )
  0 opt-getlit 				\ pxt x rel?
  SWAP ROT EXECUTE SWAP
  0 opt-setlit
  1 1 opt-remove ;

opt( ''# '' CELLS )opt: ['] CELLS (_sizing) ;opt
opt( ''# '' 1+    )opt: ['] 1+    (_sizing) ;opt
opt( ''# '' 1-    )opt: ['] 1-    (_sizing) ;opt
opt( ''# '' CHAR+ )opt: ['] 1+    (_sizing) ;opt
opt( ''# '' 2*    )opt: ['] 2*    (_sizing) ;opt
opt( ''# '' 2/    )opt: ['] 2/    (_sizing) ;opt

\ General arithmetic optimizer. Precalculates or produces faster code.
: (#arith) 				( xt -- )
  0 opt-getlit 				\ xt x rel?
  0 2 opt-remove
  regalloc-reset
  req-any
  ?+relocate
  ## tos0 EXECUTE ;
  
: (##arith) 				( xt -- )
   1 opt-getlit
   0 opt-getlit 			\ xt x1 rel1 x0 rel0
   ROT OR -ROT 				\ xt rel tos1 tos0 
   TURN EXECUTE SWAP 			\ x rel
   0 opt-setlit
   1 2 opt-remove ;
   
opt( ''# '' +   )opt: ['] add, (#arith) ;opt
opt( ''# '' -   )opt: ['] sub, (#arith) ;opt
opt( ''# '' OR  )opt: ['] or,  (#arith) ;opt
opt( ''# '' AND )opt: ['] and, (#arith) ;opt
opt( ''# '' XOR )opt: ['] xor, (#arith) ;opt

opt( ''# ''# '' +   )opt: ['] +   (##arith) ;opt
opt( ''# ''# '' -   )opt: ['] -   (##arith) ;opt
opt( ''# ''# '' OR  )opt: ['] OR  (##arith) ;opt
opt( ''# ''# '' AND )opt: ['] AND (##arith) ;opt
opt( ''# ''# '' XOR )opt: ['] XOR (##arith) ;opt

\ @ optimizer.
opt( ''# '' @ )opt:
   0 opt-getlit DROP 			\ addr
   0 2 opt-remove 
   regalloc-reset
   req-free
   #[] free0 mov,
   0 free>tos ;opt
 
\ ! optimizer.
opt( ''# '' ! )opt:
   0 opt-getlit DROP 			\ addr
   0 2 opt-remove 
   regalloc-reset
   req-any
   tos0 #[] mov,
   1 reg-free ;opt

\ +! optimizer.
opt( ''# '' +! )opt:
   0 opt-getlit DROP 			\ addr
   0 2 opt-remove 
   regalloc-reset
   req-any
   tos0 #[] add,
   1 reg-free ;opt

\ Optimizer of + @ sequence.
opt( ''# '' + '' @ )opt: 
    0 opt-getlit 			\ x rel?
    regalloc-reset
    req-any 				\ tos0=offs
    ?+relocate
    [tos0] tos0 mov, 
    0 3 opt-remove
    ;opt
    
opt( '' + '' @ )opt:     
    regalloc-reset
    req-any 				\ tos0=offs
    req-any 				\ tos1=addr
    0 [tos0+tos1] tos1 mov,
    1 reg-free 
    0 2 opt-remove
    ;opt

opt( '' 0= '' IF )opt:
  regalloc-reset
  req-any
  tos0 tos0 test,
  1 reg-free
  ['] n-jnz, (ahead)
  0 2 opt-remove
;opt

: (#-rel-IF) 				( jmp-xt n-free -- orig )
  0 opt-getlit 				\ xt nf x rel?
  regalloc-reset
  req-any
  ?+relocate ## tos0 cmp,
  reg-free
  (ahead)
  0 3 opt-remove 
;

: (rel-IF) 				( jmp-xt nfree -- orig )
  regalloc-reset
  req-any req-any
  tos0 tos1 cmp,
  1+ reg-free
  (ahead)
  0 2 opt-remove
;

opt( '' =  '' IF )opt: ['] n-jnz,  1 (rel-IF) ;opt
opt( '' <> '' IF )opt: ['] n-jz,   1 (rel-IF) ;opt
opt( '' <  '' IF )opt: ['] n-jnl,  1 (rel-IF) ;opt
opt( '' >  '' IF )opt: ['] n-jng,  1 (rel-IF) ;opt
opt( '' <= '' IF )opt: ['] n-jnle, 1 (rel-IF) ;opt
opt( '' >= '' IF )opt: ['] n-jnge, 1 (rel-IF) ;opt

opt( ''# '' =  '' IF )opt: ['] n-jnz,  1 (#-rel-IF) ;opt
opt( ''# '' <> '' IF )opt: ['] n-jz,   1 (#-rel-IF) ;opt
opt( ''# '' <  '' IF )opt: ['] n-jnl,  1 (#-rel-IF) ;opt
opt( ''# '' >  '' IF )opt: ['] n-jng,  1 (#-rel-IF) ;opt
opt( ''# '' <= '' IF )opt: ['] n-jnle, 1 (#-rel-IF) ;opt
opt( ''# '' >= '' IF )opt: ['] n-jnge, 1 (#-rel-IF) ;opt

opt( '' OVER '' =  '' IF )opt: ['] n-jnz,  0 (rel-IF) 0 1 opt-remove ;opt
opt( '' OVER '' <> '' IF )opt: ['] n-jz,   0 (rel-IF) 0 1 opt-remove ;opt
opt( '' OVER '' <  '' IF )opt: ['] n-jng,  0 (rel-IF) 0 1 opt-remove ;opt
opt( '' OVER '' >  '' IF )opt: ['] n-jnl,  0 (rel-IF) 0 1 opt-remove ;opt
opt( '' OVER '' <= '' IF )opt: ['] n-jnge, 0 (rel-IF) 0 1 opt-remove ;opt
opt( '' OVER '' >= '' IF )opt: ['] n-jnle, 0 (rel-IF) 0 1 opt-remove ;opt

opt( ''# '' OVER '' =  '' IF )opt: ['] n-jnz,  0 (#-rel-IF) 0 1 opt-remove ;opt
opt( ''# '' OVER '' <> '' IF )opt: ['] n-jz,   0 (#-rel-IF) 0 1 opt-remove ;opt
opt( ''# '' OVER '' <  '' IF )opt: ['] n-jng,  0 (#-rel-IF) 0 1 opt-remove ;opt
opt( ''# '' OVER '' >  '' IF )opt: ['] n-jnl,  0 (#-rel-IF) 0 1 opt-remove ;opt
opt( ''# '' OVER '' <= '' IF )opt: ['] n-jnge, 0 (#-rel-IF) 0 1 opt-remove ;opt
opt( ''# '' OVER '' >= '' IF )opt: ['] n-jnle, 0 (#-rel-IF) 0 1 opt-remove ;opt

opt( '' DUP '' >R )opt:
  regalloc-reset
  req-any
  tos0 push,
  0 2 opt-remove
;opt

opt( '' R> '' DROP )opt:
  regalloc-reset
  req-free
  free0 pop,
  0 2 opt-remove
;opt

opt( '' DROP '' R> )opt:
  regalloc-reset
  req-any
  tos0 pop,
  0 2 opt-remove
;opt

: (2DUP-rel-IF) 			( jmp-xt -- )
  regalloc-reset
  req-any
  req-any
  tos0 tos1 cmp, 			\ jxt
  (ahead)
  0 3 opt-remove
;

opt( '' 2DUP '' =  '' IF )opt: ['] n-jnz,  (2DUP-rel-IF) ;opt
opt( '' 2DUP '' <> '' IF )opt: ['] n-jz,   (2DUP-rel-IF) ;opt
opt( '' 2DUP '' <  '' IF )opt: ['] n-jnl,  (2DUP-rel-IF) ;opt
opt( '' 2DUP '' >  '' IF )opt: ['] n-jng,  (2DUP-rel-IF) ;opt
opt( '' 2DUP '' <= '' IF )opt: ['] n-jnle, (2DUP-rel-IF) ;opt
opt( '' 2DUP '' >= '' IF )opt: ['] n-jnge, (2DUP-rel-IF) ;opt

: (rel-WHILE) 				( dest jmp-xt nr-free -- orig dest)
  (rel-IF) 				\ dest orig
  1 (CS-ROLL) ;

opt( '' =  '' WHILE )opt: ['] n-jnz,  1 (rel-WHILE) ;opt
opt( '' <> '' WHILE )opt: ['] n-jz,   1 (rel-WHILE) ;opt
opt( '' <  '' WHILE )opt: ['] n-jnl,  1 (rel-WHILE) ;opt
opt( '' >  '' WHILE )opt: ['] n-jng,  1 (rel-WHILE) ;opt
opt( '' <= '' WHILE )opt: ['] n-jnle, 1 (rel-WHILE) ;opt
opt( '' >= '' WHILE )opt: ['] n-jnge, 1 (rel-WHILE) ;opt

opt( '' OVER '' =  '' WHILE )opt: ['] n-jnz,  0 (rel-WHILE) 0 1 opt-remove ;opt
opt( '' OVER '' <> '' WHILE )opt: ['] n-jz,   0 (rel-WHILE) 0 1 opt-remove ;opt
opt( '' OVER '' <  '' WHILE )opt: ['] n-jng,  0 (rel-WHILE) 0 1 opt-remove ;opt
opt( '' OVER '' >  '' WHILE )opt: ['] n-jnl,  0 (rel-WHILE) 0 1 opt-remove ;opt
opt( '' OVER '' <= '' WHILE )opt: ['] n-jnge, 0 (rel-WHILE) 0 1 opt-remove ;opt
opt( '' OVER '' >= '' WHILE )opt: ['] n-jnle, 0 (rel-WHILE) 0 1 opt-remove ;opt

: (2DUP-rel-WHILE) 			( dest jmp-xt nr-free -- orig dest )
  (2DUP-rel-IF) 1 (CS-ROLL) ;

opt( '' 2DUP '' =  '' WHILE )opt: ['] n-jnz,  (2DUP-rel-WHILE) ;opt
opt( '' 2DUP '' <> '' WHILE )opt: ['] n-jz,   (2DUP-rel-WHILE) ;opt
opt( '' 2DUP '' <  '' WHILE )opt: ['] n-jnl,  (2DUP-rel-WHILE) ;opt
opt( '' 2DUP '' >  '' WHILE )opt: ['] n-jng,  (2DUP-rel-WHILE) ;opt
opt( '' 2DUP '' <= '' WHILE )opt: ['] n-jnle, (2DUP-rel-WHILE) ;opt
opt( '' 2DUP '' >= '' WHILE )opt: ['] n-jnge, (2DUP-rel-WHILE) ;opt

opt( '' OVER '' @ )opt: 
  regalloc-reset
  req-any
  req-any
  req-free
  0 [tos1] free0 mov,
  0 free>tos
  0 2 opt-remove ;opt

opt( '' OVER '' ! )opt:
  regalloc-reset
  req-any 				\ tos0=x
  req-any 				\ tos1=addr
  tos0 0 [tos1] mov,
  1 reg-free
  0 2 opt-remove
;opt

opt( '' DUP '' 1- )opt:
  regalloc-reset
  req-any
  req-free
  -1 [tos0] free0 lea,
  0 free>tos
  0 2 opt-remove
;opt

: (#_log_IF) 				( log-xt -- )
  0 opt-getlit 				\ xt x rel?
  regalloc-reset
  req-any
  ?+relocate
  ## tos0 EXECUTE
  1 reg-free
  ['] n-jz, (ahead)
  0 3 opt-remove 
  ;

: (_log_IF) 				( log-xt -- )
  regalloc-reset 			\ xt
  req-any
  req-any
  tos0 tos1 EXECUTE
  2 reg-free
  ['] n-jz, (ahead)
  0 2 opt-remove
; 
 
opt( '' OR  '' IF )opt: ['] or,   (_log_IF) ;opt
opt( '' AND '' IF )opt: ['] test, (_log_IF) ;opt
opt( ''# '' OR  '' IF )opt: ['] or,   (#_log_IF) ;opt
opt( ''# '' AND '' IF )opt: ['] test, (#_log_IF) ;opt

opt( '' DUP '' CELL+ '' @ )opt:
  regalloc-reset
  req-any
  req-free
  4 [tos0] free0 mov,
  0 free>tos
  0 3 opt-remove
;opt

