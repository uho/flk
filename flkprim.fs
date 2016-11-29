\  FLK primitive optimizer
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

\ $Id: flkprim.fs,v 1.20 1998/08/30 10:50:59 root Exp $
\ $Log: flkprim.fs,v $
\ Revision 1.20  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.19  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.18  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.17  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.16  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.15  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.14  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.13  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.12  1998/05/09 21:47:05  root
\ primitives added
\
\ Revision 1.11  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.10  1998/05/02 14:27:58  root
\ compile only primitives
\
\ Revision 1.9  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.8  1998/04/29 18:20:30  root
\ ROTARE/-ROTARE
\
\ Revision 1.7  1998/04/27 18:41:42  root
\ exchange primitive added
\
\ Revision 1.6  1998/04/25 11:02:07  root
\ * fixed (crrect now and faster)
\
\ Revision 1.5  1998/04/24 16:47:39  root
\ bug fixes
\
\ Revision 1.4  1998/04/10 14:42:50  root
\ bugs corrected
\
\ Revision 1.3  1998/04/09 11:35:03  root
\ primitives added and checked, all OK
\
\ Revision 1.2  1998/04/09 09:18:11  root
\ primives checked, roll corrected
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ See standard.
p: CHARS ;
( OK )

\ See standard.
p: @                     ( addr -- n )
( OK )
  regalloc-reset
  #tos-cache #USEREGS = IF
    req-any 
    0 [tos0] tos0 mov, 
  ELSE
    req-any req-free
    0 [tos0] free0 mov,
    1 reg-free
    0 free>tos
  THEN 
  ;

\ See standard.
p: !                     ( n addr -- )
( OK )
  regalloc-reset
  req-any
  req-any
  tos1 0 [tos0] mov, 
  2 reg-free ;

\ Store FALSE at the given address.
p: OFF 			( addr -- )
( OK )
  regalloc-reset
  req-any
  DWORD
  0 ## 0 [tos0] mov,
  1 reg-free ;

\ Store TRUE at the given address.
p: ON 			( addr -- )
( OK )
  regalloc-reset
  req-any
  DWORD
  -1 ## 0 [tos0] mov,
  1 reg-free ;

\ See standard.
p: 1+                    ( n -- n+1 )
( OK )
  regalloc-reset
  req-any 
  tos0 inc, ;

\ See standard.
p: CHAR+  			( addr -- addr+char)
( OK )
  regalloc-reset
  req-any 
  tos0 inc, ; 

\ Decrease the given address by the size of a character (1 byte).
p: CHAR-  			( addr -- addr-char )
( OK )
  regalloc-reset
  req-any 
  tos0 dec, ; 

\ See standard.
p: 1-
( OK )
  regalloc-reset
  req-any 
  tos0 dec, ;

\ See standard.
p: 2*
( OK )
  regalloc-reset
  req-any 
  1 ## tos0 shl, ;

\ See standard.
p: 2/
( OK )
  regalloc-reset
  req-any 
  1 ## tos0 sar, ;

\ See standard.
p: AND
( OK )
  regalloc-reset
  req-any
  req-any
  0 1 tos-swap
  tos0 tos1 and, 
  1 reg-free ;

\ See standard.
p: OR
( OK )
  regalloc-reset
  req-any
  req-any
  0 1 tos-swap
  tos0 tos1 or, 
  1 reg-free ;

\ See standard.
p: XOR
( OK )
  regalloc-reset
  req-any
  req-any
  0 1 tos-swap
  tos0 tos1 xor, 
  1 reg-free ;

\ See standard.
p: * 					( t1 t0 -- t1*t0 )
( OK )
  regalloc-reset
  req-edx 			\ tos0
  req-eax 			\ tos1
  tos0 mul, 
  1 reg-free ;

\ See standard.
p: +
( OK )
  regalloc-reset
  req-any req-any
  0 1 tos-swap
  tos0 tos1 add, 
  1 reg-free ;

\ See standard.
p: -
( OK )
  regalloc-reset
  req-any req-any
  tos0 tos1 sub, 
  1 reg-free ;

\ See standard.
p: INVERT
( OK )
  regalloc-reset
  req-any 
  tos0 not, ;

\ See standard.
p: NEGATE
( OK )
  regalloc-reset
  req-any 
  tos0 neg, ;

\ See standard.
p: C@
( OK )
  regalloc-reset
  req-any
  a-d-free
  free0 free0 xor,
  0 [tos0] free0l mov,
  1 reg-free 
  0 free>tos ;

\ See standard.
p: C! ( c addr -- )
( OK )
  regalloc-reset
  req-any req-a-d
  tos1l 0 [tos0] mov, 
  2 reg-free ;

\ See standard.
p: +!
( OK )
  regalloc-reset
  req-any
  req-any
  tos1 0 [tos0] add, 
  2 reg-free ;

\ See standard.
p: 2!
( OK )
  regalloc-reset
  req-any
  req-any
  req-any
  tos1 0       [tos0] mov,
  tos2 1 CELLS [tos0] mov, 
  3 reg-free ;

\ See standard.
p: 2@
( OK )
  regalloc-reset
  req-any
  req-free
  req-free
  0       [tos0] free0 mov,
  1 CELLS [tos0] free1 mov,
  1 reg-free
  1 free>tos 
  0 free>tos ;

\ See standard.
p: 2OVER 		( n1 n2 n3 n4 --- n1 n2 n3 n4 n1 n2 )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  req-free req-free
  tos3 free0 mov,
  tos2 free1 mov,
  0 free>tos 
  1 free>tos ;

\ See standard.
p: 2SWAP 			( n1 n2 n3 n4 --- n3 n4 n1 2n )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  1 3 tos-swap 
  0 2 tos-swap ;

\ See standard.
p: 2DUP 		( n1 n2 -- n1 n2 n1 n2 )
( OK )
  regalloc-reset
  req-any req-any 
  req-free req-free
  tos1 free0 mov,
  tos0 free1 mov,
  0 free>tos
  1 free>tos ;

\ Duplicate the top 3 values on the stack.
p: 3DUP 			( t2 t1 t0 -- t2 t1 t0 f2 f1 f0 )
( OK )
  regalloc-reset
  req-any req-any req-any 
  req-free req-free req-free 
  tos2 free2 mov,
  tos1 free1 mov,
  tos0 free0 mov,
  2 free>tos
  1 free>tos
  0 free>tos ;

\ See standard.
p: DROP 			( n -- )
( OK )
  regalloc-reset
  req-any 
  1 reg-free ;

\ See standard.
p: D>S 				( d -- n )
( OK )
  regalloc-reset
  req-any 
  1 reg-free ;

\ See standard.
p: NIP 				( a b -- b )
( OK )
  regalloc-reset
  req-any req-any 
  0 1 tos-swap 1 reg-free ;

\ See standard.
p: 2DROP 			( n1 n2 -- )
( OK )
  regalloc-reset
  req-any req-any
  2 reg-free ;

\ See standard.
p: DUP 				( n -- n  n)
( OK )
  regalloc-reset
  req-any
  req-free
  tos0 free0 mov, 
  0 free>tos ;

\ See standard.
p: OVER 			( n1 n2 -- n1 n2 n1 )
( OK )
  regalloc-reset
  req-any
  req-any
  req-free
  tos1 free0 mov, 
  0 free>tos ;

\ See standard.
p: ROT 				( n1 n2 n3 --- n2 n3 n1 )
( OK )
  regalloc-reset
  req-any req-any req-any
  0 1 tos-swap 		\ t2 t0 t1 
  0 2 tos-swap ; 	\ t1 t0 t2 

\ Put the the top of stack value below the two next values. Inverse operation
\ to ROT.
p: -ROT 			( n1 n2 n3 --- n3 n1 n2 )
( OK )
  regalloc-reset
  req-any req-any 
  req-any 		\ t2 t1 t0
  0 1 tos-swap 		\ t2 t0 t1 
  1 2 tos-swap ; 	\ t0 t2 t1

\ Rotate the top four items upwards.
p: TURN 			( t3 t2 t1 t0 -- t2 t1 t0 t3 )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  0 1 tos-swap 		\ t3 t2 t0 t1
  0 3 tos-swap 		\ t1 t2 t0 t3
  2 3 tos-swap ; 	\ t2 t1 t0 t3

\ Rotate the top four items downwards.
p: -TURN 			( t3 t2 t1 t0 -- t0 t3 t2 t1 )
( OK )
  regalloc-reset
  req-any req-any 
  req-any req-any 	\ t3 t2 t1 t0
  0 1 tos-swap 		\ t3 t2 t0 t1
  1 2 tos-swap 		\ t3 t0 t2 t1
  2 3 tos-swap 		\ t0 t3 t2 t1
  ;

\ Rotate the top five items upwards.
p: TWIST 			( t4 t3 t2 t1 t0 -- t3 t2 t1 t0 t4 )
( OK )
  regalloc-reset
  req-any req-any 
  req-any req-any 
  req-any 			\ t4 t3 t2 t1 t0
  0 4 tos-swap 			\ t0 t3 t2 t1 t4
  4 3 tos-swap 			\ t3 t0 t2 t1 t4
  3 2 tos-swap 			\ t3 t2 t0 t1 t4
  2 1 tos-swap 			\ t3 t2 t1 t0 t4
;

\ Rotate the top five items downwards.
p: -TWIST 			( t4 t3 t2 t1 t0 -- t0 t4 t3 t2 t1 )
( OK )
  regalloc-reset
  req-any req-any 
  req-any req-any 
  req-any 			\ t4 t3 t2 t1 t0
  0 1 tos-swap 			\ t4 t3 t2 t0 t1
  1 2 tos-swap 			\ t4 t3 t0 t2 t1
  2 3 tos-swap 			\ t4 t0 t3 t2 t1
  3 4 tos-swap 			\ t0 t4 t3 t2 t1
;

\ Rotate the top six items upwards.
p: ROTARE 			( t5 t4 t3 t2 t1 t0 -- t4 t3 t2 t1 t0 t5 )
  regalloc-reset
  req-any req-any 
  req-any req-any 
  req-any req-any 		\ t5 t4 t3 t2 t1 t0 
  5 4 tos-swap 			\ t4 t5 t3 t2 t1 t0 
  4 3 tos-swap 			\ t4 t3 t5 t2 t1 t0 
  3 2 tos-swap 			\ t4 t3 t2 t5 t1 t0 
  2 1 tos-swap 			\ t4 t3 t2 t1 t5 t0 
  1 0 tos-swap 			\ t4 t3 t2 t1 t0 t5 
;

\ Rotate the top six items downwards.
p: -ROTARE 			( t5 t4 t3 t2 t1 t0 -- t0 t5 t4 t3 t2 t1 )
  regalloc-reset
  req-any req-any 
  req-any req-any 
  req-any req-any 		\ t5 t4 t3 t2 t1 t0 
  0 1 tos-swap 			\ t5 t4 t3 t2 t0 t1 
  1 2 tos-swap 			\ t5 t4 t3 t0 t2 t1 
  2 3 tos-swap 			\ t5 t4 t0 t3 t2 t1 
  3 4 tos-swap 			\ t5 t0 t4 t3 t2 t1 
  4 5 tos-swap 			\ t0 t5 t4 t3 t2 t1 
;

\ See standard.
p: TUCK 			( t1 t0 -- t0 t1 t0 )
( OK )
  regalloc-reset
  req-any req-any 
  req-free
  tos0 free0 mov,
  0 1 tos-swap 		\ t0 t1
  0 free>tos 		\ t0 t1 f0
;

\ Copy the third stack item on top.
p: PLUCK 		( t2 t1 t0 -- t2 t1 t0 t2 )
( OK )
  regalloc-reset
  req-any req-any req-any
  req-free
  tos2 free0 mov,
  0 free>tos ;

\ Copy the fourth stack item on top.
p: FLOCK 		( t3 t2 t1 t0 -- t3 t2 t1 t0 t3 )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  req-free
  tos3 free0 mov,
  0 free>tos ;

\ See standard.
c: 2>R 			
( OK )
  regalloc-reset
  req-any req-any
  tos1 push,
  tos0 push,
  2 reg-free
;

\ See standard.
c: 2R>
( OK )
  regalloc-reset
  req-free req-free
  free0 pop,
  free1 pop,
  1 free>tos
  0 free>tos
;

\ See standard.
c: 2R@
  regalloc-reset
  req-free req-free
  0 [esp] free0 mov,
  4 [esp] free1 mov,
  1 free>tos
  0 free>tos ;

\ See standard.
p: D0=
( OK )
  regalloc-reset
  req-any req-any
  tos0 tos1 or,
  tos1l setz,
  31 ## tos1 shl, 
  31 ## tos1 sar, 
  1 reg-free ;
  
\ See standard.
p: S>D
( OK )
  regalloc-reset
  req-eax 		\ t0=eax
  free-edx 		\ f0=edx
  cdq, 0 free>tos ;

\ See standard.
p: SWAP
( OK )
  regalloc-reset
  req-any
  req-any 
  0 1 tos-swap ;

\ See standard.
c: >R
( OK )
  regalloc-reset
  req-any
  tos0 push, 
  1 reg-free ;

\ See standard.
c: R>
( OK )
  regalloc-reset
  req-free
  free0 pop, 
  0 free>tos ;

\ See standard.
c: R@
  regalloc-reset
  req-free
  0 [esp] free0 mov,
  0 free>tos ;

\ See standard.
p: LSHIFT 				( x1 u -- x2 )
( OK )
  regalloc-reset
  req-ecx
  req-any
  tos0 tos1 shl, 
  1 reg-free ;

\ See standard.
p: RSHIFT 				( x1 u -- x2 )
( OK )
  regalloc-reset
  req-ecx 				\ tos0
  req-any 				\ tos1
  tos0 tos1 shr, 
  1 reg-free ;

\ See standard.
p: 0= 					( n -- f )
( OK )
  regalloc-reset
  req-a-d
  tos0 tos0 or,
  tos0l setz,
  31 ## tos0 shl, 
  31 ## tos0 sar, ;

\ See standard.
p: 0< 					( n -- f )
( OK )
  regalloc-reset
  req-a-d
  tos0 tos0 or,
  tos0l setl,
  31 ## tos0 shl, 
  31 ## tos0 sar, ;

\ See standard.
p: 0> 					( n -- f )
( OK )
  regalloc-reset
  req-a-d
  tos0 tos0 or,
  tos0l setg,
  31 ## tos0 shl, 
  31 ## tos0 sar, ;

\ See standard.
p: 0<> 					( n -- f )
( OK )
  regalloc-reset
  req-a-d
  tos0 tos0 or,
  tos0l setnz,
  31 ## tos0 shl, 
  31 ## tos0 sar, ;
 
\ See standard.
p: = 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l setne,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ See standard.
p: <> 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l sete,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ See standard.
p: < 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l setge,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ Perform a less or equal comparison. Equivalent to > INVERT
p: <= 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l setg,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ See standard.
p: > 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l setle,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ Perform a greater or equal comparison. Equivalent to < INVERT
p: >= 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-any
  a-d-free
  free0 free0 xor,
  tos0 tos1 cmp,
  free0l setl,
  free0 dec,
  2 reg-free 
  0 free>tos ;

\ See standard.
p: U< 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-a-d
  tos0 tos1 cmp,
  tos1l setb,
  31 ## tos1 shl,
  31 ## tos1 sar,
  1 reg-free ;

\ See standard.
p: U> 					( n1 n2 -- f )
( OK )
  regalloc-reset
  req-any
  req-a-d
  tos0 tos1 cmp,
  tos1l seta,
  31 ## tos1 shl,
  31 ## tos1 sar,
  1 reg-free ;

\ See standard.
p: 2ROT ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
( OK )
  regalloc-reset
  req-any req-any req-any
  req-any req-any req-any 		\ t5 t4 t3 t2 t1 t0
  0 4 tos-swap 				\ t5 t0 t3 t2 t1 t4
  1 5 tos-swap 				\ t1 t0 t3 t2 t5 t4
  2 4 tos-swap 				\ t1 t2 t3 t0 t5 t4
  3 5 tos-swap ; 			\ t3 t2 t1 t0 t5 t4

c: EXIT 					( -- )
( OK )
  regalloc-reset
  regalloc-flush
  ret, ; 

c: ?EXIT 					( flag -- )
( OK )
  regalloc-reset
  req-any
  tos0 tos0 or,
  0 jz,
  1 reg-free
  save-allocator
  regalloc-flush
  ret,
  0 $:
  restore-allocator ; 

\ See standard.
p: FILL 				( addr cnt char -- )
( OK )
  regalloc-reset
  req-eax 				\ eax=char
  req-ecx 				\ ecx=cnt
  req-edi 				\ edi=addr
  rep, stosb,
  3 reg-free ;

\ See standard.
p: D+ 					( d1 d2 -- d1+d2 )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  tos1 tos3 add,
  tos0 tos2 adc,
  2 reg-free ;

\ See standard.
p: D- 					( d1 d2 -- d1-d2 )
( OK )
  regalloc-reset
  req-any req-any req-any req-any
  tos1 tos3 sub,
  tos0 tos2 sbb,
  2 reg-free ;

\ See standard.
p: D2* 					( d1 -- d2 )
( OK )
  regalloc-reset
  req-any req-any
  clc,
  1 ## tos1 rcl,
  1 ## tos0 rcl, ;

\ See standard.
p: D2/ 					( d1l d1h -- d2l d2h )
( OK )
  regalloc-reset
  req-any req-any req-free
  tos0 free0 mov,
  1 ## free0 rcl,
  1 ## tos0 rcr,
  1 ## tos1 rcr, ;

\ See standard.
p: D0< 					( dl dh -- flag )
( OK )
  regalloc-reset
  req-any req-any
  tos0 tos1 mov,
  31 ## tos1 sar,
  1 reg-free ;

\ See standard.
p: DNEGATE 				( dl dh -- d1l d1h )
( OK )
  regalloc-reset
  req-any req-any
  tos0 neg,
  tos1 neg,
  0 ## tos0 sbb, ;

\ See standard.
p: CMOVE 				( a1 a2 cnt -- )
( OK )
  regalloc-reset
  req-ecx 				\ ecx=cnt
  req-edi 				\ edi=a2
  req-esi 				\ esi=a1
  rep, movsb,
  3 reg-free ;

\ See standard.
p: CMOVE> 				( a1 a2 cnt -- )
( OK )
  regalloc-reset
  req-ecx 				\ ecx=cnt
  req-edi 				\ edi=a2
  req-esi 				\ esi=a1
  ecx edi add,
  ecx esi add,
  edi dec,
  esi dec,
  std,
  rep, movsb,
  cld,
  3 reg-free ;

\ See standard.
p: M* 					( n1 n2 -- d )
( OK )
  regalloc-reset
  req-edx
  req-eax
  edx imul, ;

\ See standard.
p: UM* 					( u1 u2 -- ud )
( OK )
  regalloc-reset
  req-edx
  req-eax
  edx mul, ;

\ See standard.
p: UM/MOD 				( ud un -- ur uq )
( OK )
  regalloc-reset
  req-any 				\ un=tos0
  req-edx 				\ udh=edx=tos1=rem
  req-eax 				\ udl=eax=tos2=quot
  tos0 div,
  1 2 tos-swap
  1 reg-free ;

\ See standard.
p: SM/REM 				( d1l d1h n1 -- nrem nquot )
( OK )
  regalloc-reset
  req-ebx
  req-edx
  req-eax
  ebx idiv,
  1 reg-free
  0 1 tos-swap ;

c: SP@ 				( -- sp )
( OK )
  regalloc-reset
  req-free
  ebp free0 mov,
  offs-ebp ## free0 add,
  0 free>tos ;

c: SP! 				( sp -- )
( OK )
  regalloc-reset
  regalloc-flush
  req-any
  1 reg-free
  0 TO offs-ebp
  eax ebp mov, ;

\ Retrieve the current return stack pointer.
c: RP@ 				( -- rp )
( OK )
  regalloc-reset
  req-free
  esp free0 mov,
  0 free>tos ;

\ Set the return stack pointer. Attention: A wrong value does not lead to a
\ segmentation fault immediate, but at the next call or return.
c: RP! 				( rp -- )
( OK )
  regalloc-reset
  req-any
  tos0 esp mov,
  1 reg-free ;

\ See standard.
p: DU< 				( d1l d1h d2l d2h -- flag )
( OK )
  regalloc-reset
  req-any 			\ tos0=d2h
  req-any 			\ tos1=d2l
  req-any 			\ tos2=d1h
  req-any 			\ tos3=d1l
  tos1 tos3 sub,
  tos0 tos2 sbb,
  tos3 tos3 sbb,
  3 reg-free ;

\ See standard.
p: PICK 			( n -- tos+n )
( OK )
  regalloc-reset
  regalloc-flush
  req-any
  tos0 inc,
  2 ## tos0 shl,
  ebp tos0 add,
  offs-ebp [tos0] tos0 mov, ;

\ Return the base address of the data stack. This is the highest accessable
\ address plus one cell since the stack grows downwards.
p: SP-BASE 			( -- sp-base)
( OK )
  regalloc-reset
  req-free
  HA-INIT-DATASTACK #[] free0 mov,
  0 free>tos ;

\ Return the base address of the return stack. This is the highest accessable
\ address plus one cell since the stack grows downwards.
p: RP-BASE 			( -- rp-base)
( OK )
  regalloc-reset
  req-free
  HA-INIT-CALLSTACK #[] free0 mov,
  0 free>tos ;

\ See standard.
p: ROLL 			( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
( OK )
  regalloc-reset
  regalloc-flush
  req-any 			\ tos0=eax=u
  tos0 ecx mov,
  2 ## tos0 shl, 		\ tos0=4*u
  ebp edi mov,
  ebp esi mov,
  tos0 edi add,
  tos0 esi add,
  4 ## edi add,
  0 [edi] ebx mov,
  std,
  rep, movsd,
  cld,
  ebx 0 [edi] mov,
  1 reg-free ;

\ See standard.
p: FORTH-WORDLIST 			( -- wid )
( OK )
  regalloc-reset
  req-free
  +relocate
  HA-DEF-WL ## free0 mov,
  0 free>tos ;

\ Return the wordlist identifier for the environment wordlist.
p: ENVIRONMENT-WORDLIST 		( -- wid )
( OK )
  regalloc-reset
  req-free
  +relocate
  HA-ENV-WL ## free0 mov,
  0 free>tos ;

\ Return the wordlist identifier for the ASSEMBLER wordlist.
p: ASSEMBLER-WORDLIST 			( -- wid )
( OK )
  regalloc-reset
  req-free
  +relocate
  HA-ASS-WL ## free0 mov,
  0 free>tos ;

\ Return the wordlist identifier for the EDITOR wordlist.
p: EDITOR-WORDLIST 			( -- wid )
( OK )
  regalloc-reset
  req-free
  +relocate
  HA-EDT-WL ## free0 mov,
  0 free>tos ;

\ Return the last possible address in the data area.
p: HERE-LIMIT 				( -- here-limit )
( OK )
  regalloc-reset
  req-free
  HA-HERE-LIMIT #[] free0 mov,
  0 free>tos ;

\ Return the last possible address in the code area.
p: CHERE-LIMIT 				( -- here-limit )
( OK )
  regalloc-reset
  req-free
  HA-CHERE-LIMIT #[] free0 mov,
  0 free>tos ;

\ Return inital value of HERE.
p: HERE-INIT 				( -- here-limit )
( OK )
  regalloc-reset
  req-free
  HA-HERE-INIT #[] free0 mov,
  0 free>tos ;

\ Return inital value of CHERE.
p: CHERE-INIT 				( -- here-limit )
( OK )
  regalloc-reset
  req-free
  HA-CHERE-INIT #[] free0 mov,
  0 free>tos ;

\ See standard.
p: COUNT 				( c-addr1 -- c-addr2 u )
( OK )
  regalloc-reset
  req-any
  a-d-free
  free0 free0 xor,
  0 [tos0] free0l mov,
  tos0 inc,
  0 free>tos ;

\ Primitive for EXECUTE. Jumps to the given address.
p: (EXECUTE) 				( addr -- )
( OK )
  regalloc-reset
  req-ebx
  1 reg-free
  regalloc-flush
  ebx call, ;

\ See standard.
p: CELLS 				( x -- x*4 )
( OK )
  regalloc-reset
  req-any
  2 ## tos0 shl, ;

\ See standard.
p: CELL+ 				( x -- x+4)
( OK )
  regalloc-reset
  req-any
  4 ## tos0 add, ;

\ Decrease the given address by one cell (4 bytes).
p: CELL- 				( x -- x-4)
( OK )
  regalloc-reset
  req-any
  4 ## tos0 sub, ;

\ Get the relocation table.
p: RELOCATION-TABLE@ 			( -- reltab )
( OK )
  regalloc-reset
  req-free
  HA-RELTABLE #[] free0 mov,
  0 free>tos ;

\ Store the relocation table.
p: RELOCATION-TABLE! 			( reltab -- )
( OK )
  regalloc-reset
  req-any
  tos0 HA-RELTABLE #[] mov,
  1 reg-free ; 

\ See standard.
p: / 					( n1 n2 -- n3 )
( OK )
  regalloc-reset
  req-edx 				\ n2=tos0=edx
  req-eax 				\ n1=tos1=eax
  req-free
  eax free0 mov,
  31 ## free0 sar,
  edx free0 xchg,
  free0 idiv, 				\ eax=quot edx=rem
  1 reg-free ;
  
\ See standard.
p: MOD 					( n1 n2 -- n3 )
( OK )
  regalloc-reset
  req-edx 				\ n2=tos0=edx
  req-eax 				\ n1=tos1=eax
  req-free
  eax free0 mov,
  31 ## free0 sar,
  edx free0 xchg,
  free0 idiv, 				\ eax=quot edx=rem
  0 1 tos-swap
  1 reg-free ;
  
\ See standard.
p: M+ 					( dl dh n -- dl dh )
( OK )
   regalloc-reset
   req-any 				\ tos0=n
   req-any 				\ tos1=dh
   req-any 				\ tos2=dl
   tos0 tos2 add,
   0 ## tos1 adc,
   1 reg-free ;

\ Return TRUE if the two numbers differ in their sign.
p: SignsDiffer? 			( n1 n2 -- flag )
( OK )
   regalloc-reset
   req-any
   req-any
   tos0 tos1 xor,
   31 ## tos1 sar,
   1 reg-free ;
   
\ Exchange the number and the content of the address.
p: exchange 				( x1 addr -- x2 )
   regalloc-reset
   req-any 				\ tos0=addr
   req-any 				\ tos1=x1
   tos1 0 [tos0] xchg,
   1 reg-free ;

\ See standard.
p: ALIGNED 				( addr -- addr2 )
( OK )
   regalloc-reset
   req-any
   3 ## tos0 add,
   3 INVERT ## tos0 and, ;

\ See standard.
p: MAX 					( n1 n2 -- n3 )
( OK )
   regalloc-reset
   req-any req-any
   tos0 tos1 cmp,
   0 jg,
   tos0 tos1 mov,
0 $:
   1 reg-free ;

\ See standard.
p: MIN 					( n1 n2 -- n3 )
( OK )
   regalloc-reset
   req-any req-any
   tos0 tos1 cmp,
   0 jl,
   tos0 tos1 mov,
0 $:
   1 reg-free ;

\ See standard.
p: ABS 					( n1 -- n2 )
( OK )
   regalloc-reset
   req-any
   tos0 tos0 or,
   0 jns,
   tos0 neg,
0 $: ;

\ Same as COUNT, but for cell counted strings.
p: $COUNT 				( c-addr1 -- c-addr2 u )
  regalloc-reset
  req-any
  req-free
  0 [tos0] free0 mov,
  4 ## tos0 add,
  0 free>tos ;

\ Put a 0 onto the stack. Generates smaller code that the normal inline
\ constant.
\ p: 0 					( -- 0 )
\    regalloc-reset
\    req-free
\    free0 free0 xor,
\    0 free>tos ;

\ Advance to the cell containing the address of the interpretation semantics of
\ the word.
p: >CFA 				( xt -- cfa )
   regalloc-reset
   req-any
   4 ## tos0 add, ;

\ Advance to the cell containing the address of the optimization semantics of
\ the word.
p: >OCFA 				( xt -- ocfa )
   regalloc-reset
   req-any
   8 ## tos0 add, ;
  
\ Advance to the cell containing the address of the data field of the word.
p: >DFA 					( xt -- dfa )
   regalloc-reset
   req-any
   12 ## tos0 add, ;

\ Advance to the cell containing the address of the filename of the word.
p: >FN 						( xt -- filename )
   regalloc-reset
   req-any
   16 ## tos0 add, ;

\ Advance to the cell containing the line number of the word.
p: >DL 						( xt -- definition-line )
   regalloc-reset
   req-any
   20 ## tos0 add, ;

\ Advance to the byte containing the flags of the word.
p: >FLAGS 				( xt -- ffa )
   regalloc-reset
   req-any
   24 ## tos0 add, ;

\ Advance to the byte containing the byte counted string containing the name
\ of the word.
p: >NAME 				( xt -- nfa )
   regalloc-reset
   req-any
   25 ## tos0 add, ;

