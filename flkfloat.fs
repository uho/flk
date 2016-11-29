\  FLK floating point words.
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

\ float.f                                                A. McKewan
\                       Modified substantially by R. Smith
\                       Modified for use in flk by L. Krueger

CR .( Loading Floating Point...)

\ Floating point values are maintained on a separate stack.
\ Major changes by rls to convert to a software stack

\ I (LK) changed the CODE words to my own assembler.

: (s.)   				( n addr len w -- addr w )
  >R DROP
  SWAP R@ * +  
  R> ;

: s. 					( n addr len w -- )
  (s.) type ;

0 VALUE float?

CREATE allregs 110 ALLOT

CODE >fregs   				( addr -- )
  wait,
  0 [eax] frstor,
  4 ## ebp add,
  wait,
  0 [ebp] eax mov,
ENDCODE

CODE >fregs>   				( addr -- )
  wait,
  0 [eax] fnsave,
  wait,
  0 [eax] frstor,
  4 ## ebp add,
  wait,
  0 [ebp] eax mov,
ENDCODE

CODE fpcw>      			( -- n )
  eax 0 [ebp] mov,
  ebx ebx xor,
  ebx push,
  esp ecx mov,
  0 [ecx] fnstcw,
  4 ## ebp sub,
  wait,
  eax pop,
ENDCODE
                
CODE >fpcw      			( n -- )
  eax push,
  esp ecx mov,
  0 [ecx] fldcw,
  edx pop,
  4 ## ebp add,
  0 [ebp] eax mov,
ENDCODE

CODE fpsw>      			( -- n )
  eax 0 [ebp] mov,
  eax eax xor,
  fnstswax,
  4 ## ebp sub,
  wait,
ENDCODE

\ Fp stack is 8 bytes per entry. Valid values are 8 and 10. The 10 byte
\ numbers are one clock per fetch/store slower.
8 CONSTANT B/FLOAT			

\ Rename fetch and store depending on size.
ALSO ASSEMBLER
B/FLOAT 8 = [IF]
: fldf, fld64, ;
: fstpf, fstp64, ;
[ELSE]
: fldf, fld80, ;
: fstpf, fstp80, ;
[THEN]
PREVIOUS


CR B/FLOAT . .( byte floating point numbers) SPACE


B/FLOAT ALIGNED 1 CELLS / CONSTANT CELLS/FLOAT

256 CONSTANT fstack-elements		\ 256 floating point elements in stack

CREATE FSTACK fstack-elements B/FLOAT * ALLOT

: (fstkuflo)   				( -- )
( OK )
  -45 THROW ;

' (fstkuflo) >CFA @ RCONSTANT fstkuflo
( OK )

: FDEPTH        			( -- n )
  FSP B/FLOAT / ;

CODE fexam    				( fs: r -- r ; -- n )
( OK )
  eax 0 [ebp] mov,
  #? FSP #[] ecx mov,
  4 ## ebp sub,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fxam,
  wait,
  fnstswax,
  wait,
  $$ 4700 ## eax and,
  st0 fstp,
  ret,
0 $:
  $$ 4100 ## eax mov,
ENDCODE

CODE (f@)       			( F: -- r ; addr -- )
  0 [eax] fldf,
  4 ## ebp add,
  0 [ebp] eax mov,
  wait,
ENDCODE

\ push on simulated stack
CODE fpush      			( f: r -- )  ( fs: -- r )   
  #? FSP #[] ecx mov,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  wait,
ENDCODE

\ push on real stack
CODE fpop      				( f: -- r )   ( fs: r -- )      
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F@         			( addr -- )  ( FS: -- r )
( OK )
  0 [eax] fldf,
  #? FSP #[] ecx mov,
  FSTACK +relocate [ecx] fstpf,
  4 ## ebp add,
  B/FLOAT ## ecx add,
  0 [ebp] eax mov,
  ecx #? FSP #[] mov,
ENDCODE

CODE SF@         			( addr -- )  ( FS: -- r )
  0 [eax] fld32,
  #? FSP #[] ecx mov,
  FSTACK +relocate [ecx] fstpf,
  4 ## ebp add,
  B/FLOAT ## ecx add,
  0 [ebp] eax mov,
  ecx #? FSP #[] mov,
ENDCODE

CODE DF@         			( addr -- )  ( FS: -- r )
  0 [eax] fld64,
  #? FSP #[] ecx mov,
  FSTACK +relocate [ecx] fstpf,
  4 ## ebp add,
  B/FLOAT ## ecx add,
  0 [ebp] eax mov,
  ecx #? FSP #[] mov,
ENDCODE

CODE F!         			( addr -- )  ( fs: -- )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  ecx #? FSP #[] mov,
  0 [eax] fstpf,
  4 ## ebp add,
  0 [ebp] eax mov,
  wait,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE DF!         			( addr -- )  ( fs: -- )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  ecx #? FSP #[] mov,
  0 [eax] fstp64,
  4 ## ebp add,
  0 [ebp] eax mov,
  wait,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE SF!         			( addr -- )  ( fs: -- )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  ecx #? FSP #[] mov,
  0 [eax] fstp32,
  4 ## ebp add,
  0 [ebp] eax mov,
  wait,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE


: FVARIABLE     			(  -<name>- ) \ compile time
                			( -- a1 )     \ runtime
  CREATE B/FLOAT ALLOT ;

: F,            			( -- ) ( F: r -- )
( OK )
  HERE B/FLOAT ALLOT F! ;

: FCONSTANT     			( F: r -<name>- ) \ compile time
                             		( F: -- r )       \ runtime
( OK )
  CREATE F, DOES> F@ ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point stack operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE FDUP       			( fs: r -- r r )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
  wait,
ENDCODE

CODE FDROP      			( fs: r -- )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  ecx #? FSP #[] mov, 
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FSWAP      			( fs: r1 r2 -- r2 r1 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FOVER      			( fs: r1 r2 -- r1 r2 r3 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT 2* ## ecx add,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FROT       			( fs: r1 r2 r3 -- r2 r3 r1 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf, 		\ r3=[fsp-1]
  B/FLOAT ## ecx sub,
  1 js,
  FSTACK +relocate [ecx] fldf, 		\ r2=[fsp-2]
  B/FLOAT ## ecx sub, 			\ ecx=fsp-3
  2 js,
  FSTACK +relocate [ecx] fldf, 		\ r1=[fsp-3]
  B/FLOAT 2* ## ecx add,		\ ecx=fsp-1
  FSTACK +relocate [ecx] fstpf, 	\ [fsp-1]=r1
  B/FLOAT 2* ## ecx sub, 		\ ecx=fsp-3
  FSTACK +relocate [ecx] fstpf, 	\ [fsp-3]=r2
  B/FLOAT ## ecx add,			\ ecx=fsp-2
  FSTACK +relocate [ecx] fstpf, 	\ [fsp-2]=r3
  ret,
0 $:
1 $:
2 $:
  fstkuflo ## jmp, 
ENDCODE

: FPICK         			( n -- ) ( fs: -- r )
  B/FLOAT * FSTACK FSP + B/FLOAT - SWAP - F@ ;

CODE fpi        			( fs: -- r )
( OK )
  #? FSP #[] ecx mov,
  fldpi,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE f1.0        			( fs: -- r )
( OK )
  #? FSP #[] ecx mov,
  fld1,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE fl2t        			( fs: -- r )
( OK)
  #? FSP #[] ecx mov,
  fldl2t,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE fl2e        			( fs: -- r )
( OK)
  #? FSP #[] ecx mov,
  fldl2e,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE flog2        			( fs: -- r )
( OK)
  #? FSP #[] ecx mov,
  fldlg2,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE fln2        			( fs: -- r )
( OK)
  #? FSP #[] ecx mov,
  fldln2,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

CODE f0.0        			( fs: -- r )
( OK)
  #? FSP #[] ecx mov,
  fldz,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov, 
ENDCODE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Simple math operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE F+         			( fs: r1 r2 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fldf,
  st1 faddp,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F-         			( fs: r1 r2 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fldf,
  st1 fsubp,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F*         			( fs: r1 r2 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fldf,
  st1 fmulp,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F/         			( fs: r1 r2 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fldf,
  st1 fdivp,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FATAN2         			( fs: r1 r2 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fldf,
  fpatan,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FNEGATE    			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fchs,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F2/ 	   			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fld1,
  fchs,
  FSTACK +relocate [ecx] fldf,
  fscale,
  FSTACK +relocate [ecx] fstpf,
  st0 fstp,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE F2* 	   			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fld1,
  FSTACK +relocate [ecx] fldf,
  fscale,
  FSTACK +relocate [ecx] fstpf,
  st0 fstp,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FABS 	   			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fabs,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FSQRT 	   			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fsqrt,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FLN 	   			( fs: r1 -- r3 )
( OK)
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldln2,
  FSTACK +relocate [ecx] fldf,
  fyl2x,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FACOSH 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldln2,
  FSTACK +relocate [ecx] fldf,
  st0 fld,
  st0 fmul,
  fld1,
  st1 fsubp,
  fsqrt,
  st1 faddp,
  fyl2x,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FASINH 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldln2,
  FSTACK +relocate [ecx] fldf,
  st0 fld,
  st0 fmul,
  fld1,
  st1 faddp,
  fsqrt,
  st1 faddp,
  fyl2x,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Various floating point constants
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE w! 				( x addr -- )
  req-any 				\ tos0=addr
  req-any 				\ tos1=x
  WORD: tos1 0 [tos0] mov,
  2 reg-free
ENDCODE


f1.0 FDUP F+                       FCONSTANT f2.0
f2.0 FDUP F+ FDUP F+ ( 8 ) f2.0 F+ FCONSTANT F10.0
f2.0 FSQRT F1.0 F-                 FCONSTANT sq2m1
f2.0 FSQRT F2.0 F/ F1.0 F-         FCONSTANT sq2/2m1
f1.0 f2.0 F/                       FCONSTANT F0.5

B/FLOAT 10 = [IF]                           
  \ infinity
  f0.0 FCONSTANT finf
    ' finf >BODY  $$ 80000000 OVER CELL+ !
    $$ 7FFF SWAP 2 CELLS + w!

  \ 2^63
  f1.0 FCONSTANT a2**63
    ' a2**63 >BODY 0 OVER !  
    $$ 80000000 OVER CELL+ !
    $$ 403E00000 SWAP 2 CELLS + w!

  \ largest non-infinite number
  f0.0 FCONSTANT fbig
    ' fbig >BODY -1 OVER !
    -1 OVER CELL+ !
    $$ 7FFE SWAP 2 CELLS + w!

  \ smallest non-zero-number
  f0.0 FCONSTANT feps             
    1 ' feps >BODY !

  f0.0 FCONSTANT fsmall
    ' fsmall >BODY 0 OVER !  $$ 80000000 OVER CELL+ !
    1 SWAP 2 CELLS + w!

[ELSE]  ( 8 byte mode )         

   \ infinity
  f0.0 FCONSTANT finf                     
    ' finf >BODY 0 OVER !  
    $$ 7FF00000 SWAP CELL+ ! 

  \ 2^63
  f1.0 FCONSTANT a2**63
    ' a2**63 >BODY 0 OVER !  $$ 43E00000 SWAP CELL+ !

  f0.0 FCONSTANT afinf

  \ largest non-infinite number
  f0.0 FCONSTANT fbig
    ' fbig >BODY  -1 OVER !
    $$ 7FEFFFFF SWAP CELL+ !
	  
  \ smallest non-zero number
  f0.0 FCONSTANT feps
    1 ' feps >BODY !

  f1.0 FCONSTANT fsmall
    ' fsmall >BODY 0 OVER !  $$ 00100000 SWAP CELL+ !

[THEN]

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point literals. We store the 80-bit floating point literal
\ inline and push to the FP stack at runtime.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: FLITERAL      			( fs: r -- )
  HERE  
  POSTPONE RLITERAL 
  POSTPONE F@
  F, 
  ; IMMEDIATE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Integer to float convsersion
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE D>F        			( d -- )  ( F: -- r )
( OK )
  eax 4 [ebp] xchg,
  #? FSP #[] ecx mov,
  eax 0 [ebp] mov,
  0 [ebp] fild64,
  8 ## ebp add,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  0 [ebp] eax mov,
ENDCODE

CODE F>D        			( -- d )  ( fs: r -- ) 
( OK )
  eax 0 [ebp] mov,
  #? FSP #[] ecx mov,
  8 ## ebp sub,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  0 [ebp] fistp64,
  0 [ebp] eax mov,
  eax 4 [ebp] xchg,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE
  
: S>F S>D D>F ;
: F>S F>D DROP ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point comparison operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE fcomppx     ( -- flags )   ( fs: r1 r2 -- )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT 2* ## ecx sub,
  0 js,
  eax 0 [ebp] mov,
  B/FLOAT ## ecx add,
  4 ## ebp sub,
  FSTACK +relocate [ecx] fldf,
  B/FLOAT ## ecx sub,
  FSTACK +relocate [ecx] fldf,
  fcompp,
  wait,
  fnstswax,
  $$ 4100 ## eax and,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE ftstp      ( -- flags )  ( fs: r1 -- )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  ecx #? FSP #[] mov,
  eax 0 [ebp] mov,
  4 ## ebp sub,
  FSTACK +relocate [ecx] fldf,
  ftst,
  wait,
  fnstswax,
  st0 fstp,
  $$ 4100 ## eax and,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: F0=           			( -- f )  ( F: r -- )   
( OK )
  ftstp $$ 4000 = ;
: F0<           			( -- f )  ( F: r -- )   
( OK )
  ftstp $$ 0100 = ;
: F0>           			( -- f )  ( F: r -- )   
( OK )
  ftstp 0= ;

: F=            			( -- f )  ( F: r1 f2 -- )   
( OK )
  fcomppx $$ 4000 = ;
: F<            			( -- f )  ( F: r1 f2 -- )   
( OK )
  fcomppx $$ 0100 = ;
: F>            			( -- f )  ( F: r1 f2 -- )   
( OK )
  fcomppx 0= ;

: F<= F> INVERT ;
: F>= F< INVERT ;

: FMAX          ( f1 f2 -- f3 )
( OK )
  FOVER FOVER F<
  IF      
    FSWAP
  THEN FDROP ;

: FMIN          ( f1 f2 -- f3 )
( OK )
  FOVER FOVER F>
  IF      
    FSWAP
  THEN FDROP ;

CODE (FSIN) 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fsin,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE (FCOS) 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fcos,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE (FSINCOS) 	   			( fs: r1 -- r2 r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fsincos,
  st1 fxch,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FREM2PI 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fld1,
  fldpi,
  fscale,
  st1 fstp,
  FSTACK +relocate [ecx] fldf,
1 $:
  fprem1,
  fnstswax,
  sahf,
  1 jp,
  FSTACK +relocate [ecx] fstpf,
  st0 fstp,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: FSIN          			( f: r1 -- r2 )
( OK )
  FDUP FABS a2**63 F<
  IF 
    (FSIN)
  ELSE 
    FREM2PI (FSIN)
  THEN ;

: FCOS          			( f: r1 -- r2 )
( OK )
  FDUP FABS a2**63 F<
  IF  
    (FCOS)
  ELSE  
    FREM2PI (FCOS)
  THEN ;

: FSINCOS       			( f: r1 -- r2 )
( OK )
  FDUP FABS a2**63 F<
  IF    
    (FSINCOS)
  ELSE  
    FREM2PI (FSINCOS)
  THEN ;

: FTAN          			( f: r1 -- r2 ) 
( OK )
  FSINCOS F/ ;

CODE FLNP1 	   			( fs: r1 -- r2 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldln2,
  FSTACK +relocate [ecx] fldf,
  #? sq2m1 #[] fldf,
  st1 fcomp,
  fnstswax,
  sahf,
  1 jp,
  2 ja,
  #? sq2/2m1 #[] fldf,
  st1 fcomp,
  fnstswax,
  sahf,
  3 jb,
  fyl2xp1,
  FSTACK +relocate [ecx] fstpf,
  ret,
2 $:
3 $:
  fld1,
  st1 faddp,
  fabs,
  fyl2x,
  FSTACK +relocate [ecx] fstpf,
  ret,
1 $:
  fcompp,
  ret,  
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FLOG 	   			( fs: r1 -- r3 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldlg2,
  FSTACK +relocate [ecx] fldf,
  fyl2x,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FATAN  	   			( fs: r1 -- r3 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  fld1,
  fpatan,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FEXPM1 	   			( fs: r1 -- r3 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldl2e,
  FSTACK +relocate [ecx] fldf,
  fld1,
  st1 fcom,
  fnstswax,
  sahf,
  4 jbe,
  fchs,
  st1 fcomp,
  fnstswax,
  sahf,
  2 jnc,
  3 je,
  st1 fmulp,
  f2xm1,
  FSTACK +relocate [ecx] fstpf,
  ret,
4 $:
  st0 fstp,
2 $:
3 $:
  st0 fmulp,
  st0 fld,
  1 jp,
  frndint,
  st1 fsub,
  fld1,
  fscale,
  st1 fstp,
  st1 fxch,
  f2xm1,
  fld1,
  st1 faddp,
  st1 fmulp,
  fld1,
  st1 fsubp,
  FSTACK +relocate [ecx] fstpf,
  ret,
1 $:
  st0 fstp,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FEXP       			( fs: r1 -- r2 ) 
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldl2e,
  FSTACK +relocate [ecx] fldf,
  st1 fmulp,
  st0 fld,
  frndint,
  st1 fssub,
  fld1,
  fscale,
  st1 fstp,
  st1 fxch,
  f2xm1,
  fld1,
  st1 faddp,
  st1 fmulp,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: FCOSH         			( f: r1 -- r2 )
  FABS FEXP f1.0 FOVER F/ F+ F2/ ;

: FTANH         			( f: r1 -- r2 )
  FDUP FABS f1.0 F<
  IF  
    F2* FEXPM1 FDUP f2.0 F+ F/
  ELSE    
    F2* FDUP F0<
    IF  
      FEXPM1 FDUP f2.0 F+ F/
    ELSE  
      FNEGATE FEXP FDUP f1.0 FSWAP F-
      FSWAP f2.0 F+ F/
    THEN
  THEN ;

CODE FACOS       			( fs: r1 -- r2 ) 
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldl2e,
  FSTACK +relocate [ecx] fldf, 		\ x
  st0 fld, 				\ x x
  st0 fld, 				\ x x x
  st1 fmulp, 				\ x x^2
  fld1, 				\ x x^2 1
  st1 fsubrp, 				\ x 1-x^2
  fsqrt, 				\ x sqrt(1-x^2)
  fabs, 				
  st1 fxch,				\ sqrt(1-x^2) x
  fpatan, 				\ acos(x)
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FASIN       			( fs: r1 -- r2 )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fldl2e,
  FSTACK +relocate [ecx] fldf,
  st0 fld,
  st0 fld, 				
  st1 fmulp,
  fld1,
  st1 fsubrp,
  fabs,
  fsqrt,
  fpatan,
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: F+!           			( addr -- ) ( F: r -- )
  DUP F@ F+ F! ;

: (fsinh)       			( f: r1 -- r2 )
  FEXP F1.0 FOVER F/ F- F2/ ;

: FSINH         			( f: r1 -- r2 )
  FDUP F0<
  IF      
    FABS (FSINH) FNEGATE
  ELSE 
    (FSINH)
  THEN ;

VARIABLE cwtemp

CODE FLOOR       			( fs: r1 -- r2 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  cwtemp #[] fnstcw,
  cwtemp #[] ebx mov,
  ebx edx mov,
  $$ F3FF ## ebx and,
  $$ 0400 ## ebx or,
  ebx cwtemp #[] mov,
  wait,
  cwtemp #[] fldcw,
  wait,
  frndint,
  FSTACK +relocate [ecx] fstpf,
  edx cwtemp #[] mov,
  wait,
  cwtemp #[] fldcw,
  wait,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

CODE FROUND       			( fs: r1 -- r2 )
( OK )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  FSTACK +relocate [ecx] fldf,
  cwtemp #[] fnstcw,
  wait,
  cwtemp #[] ebx mov,
  ebx edx mov,
  $$ F3FF ## ebx and,
  ebx cwtemp #[] mov,
  wait,
  cwtemp #[] fldcw,
  wait,
  frndint,
  FSTACK +relocate [ecx] fstpf,
  edx cwtemp #[] mov,
  wait,
  cwtemp #[] fldcw,
  wait,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: FATANH        			( f: r1 -- r2 )
  FDUP f1.0 F+ FDUP F0=
  IF    
    FDROP FDROP finf FNEGATE EXIT
  THEN
  FSWAP f1.0 F- FDUP F0=
  IF    
    FDROP FDROP finf EXIT
  THEN
  F/ FLN F2/ ;

: F~            			( f: r1 r2 r3 -- )  ( -- flag )
  FDUP F0<
  IF      
    FABS FOVER FABS 3 FPICK FABS F+ F*	\ r1 r2 r3*(r1+r2)
    FROT FROT F- FABS FSWAP F<
  ELSE  
    FDUP F0=
    IF    
      FDROP FSTACK FSP + B/FLOAT - DUP B/FLOAT -
      SWAP B/FLOAT SWAP B/FLOAT COMPARE 0=
      FDROP FDROP
    ELSE  
      FROT FROT F- FABS F<
    THEN
  THEN ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ floating point defining words and array operators
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: FALIGN                		( -- ) ;
: FALIGNED              		( addr -- addr ) ;
: DFALIGN               		( -- ) ;
: DFALIGNED             		( addr -- addr ) ;
: SFALIGN               		( -- ) ;
: SFALIGNED             		( addr -- addr ) ;

: FLOAT+        			( addr1 -- addr2 )
  B/FLOAT + ;

: FLOATS        			( n1 -- n2 )
  B/FLOAT * ;

: DFLOAT+       			( addr1 -- addr2 )
  8 + ;

: DFLOATS       			( n1 -- n2 )
  8 * ;

: SFLOAT+       			( addr1 -- addr2 )
  4 + ;

: SFLOATS       			( n1 -- n2 )
  4 * ;

: float-array   			( n1 -<name>- ) \ compile time
                			( -- a1 )       \ runtime
  CREATE FLOATS ALLOT ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Floating point number conversion. We do not have the floating point
\ integrated with the interpreter, so we must prefix floating point
\ literals with f#. ( f# 1.23E6 )
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CODE f**+n     				( f: r1 -- r2 ; n -- )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  fld1,
  4 ## ebp add,
  eax ebx mov,
  0 [ebp] eax mov,
  ebx ebx or,
  4 jz,
  FSTACK +relocate [ecx] fldf,
  st1 fxch,
1 $:
  1 ## ebx shr,
  2 jnc,
  st1 fmul,
2 $:
  3 jz,
  st1 fxch,
  st0 fmul,
  st1 fxch,
  1 jmpn,
3 $:
  st1 fxch,
  st0 fstp,
4 $:
  FSTACK +relocate [ecx] fstpf,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

\ Floating number raised to integer power.
: F**N     				( F: r1 -- r2 ; n -- )  
  DUP 0<
  IF 
    ABS F**+N F1.0 FSWAP F/
  ELSE  
    F**+N
  THEN ;

: F**           			( F: r1 r2 -- r3 )
  FDUP FROUND FDUP F>S F-         	\ r1 r4
  FDUP F0= 0=
  IF      \ non-zero fractional part of exponent
    FOVER FABS FLN F* FEXP FSWAP F**N F*
  ELSE  
    FDROP F**N
  THEN ;

FVARIABLE ftemp
CREATE $ftemp 128 ALLOT
CREATE fbcd-buf 10 ALLOT

: FALOG         			( f: r1 -- r2 ) 
( OK )
  f10.0 FSWAP f** ;

: 10**N-0.5     			( f: -- r )  ( n -- )
  f10.0 F**N F0.5 F- ;

CODE f>bcd      			( f: r -- )  ( addr -- )
  #? FSP #[] ecx mov,
  B/FLOAT ## ecx sub,
  0 js,
  ecx #? FSP #[] mov,
  FSTACK +relocate [ecx] fldf,
  0 [eax] fbstp,
  4 ## ebp add,
  0 [ebp] eax mov,
  ret,
0 $:
  fstkuflo ## jmp,
ENDCODE

: bcd-char@     			( n -- char )
  DUP 1 AND SWAP 2/ fbcd-buf + C@ SWAP
  IF      
    4 RSHIFT
  ELSE  
    15 AND
  THEN
  [CHAR] 0 + ;

: bcd-char!     			( char n -- )
  SWAP 15 AND SWAP
  DUP 1 AND SWAP 2/ fbcd-buf + SWAP
  IF    
    DUP C@ 15 AND ROT 4 LSHIFT
  ELSE  
    DUP C@ $$ F0 AND ROT
  THEN
  OR SWAP C! ;

B/FLOAT 10 =
[IF]    4932 CONSTANT big-power
[ELSE]  308  CONSTANT big-power
[THEN]

: f*10**n       			( n -- )  ( f: f1 -- f2 )
  DUP big-power >
  IF    
     big-power f10.0 f**n F*
     big-power - f10.0 f**n F*
  ELSE  
    f10.0 f**n F*
  THEN ;

: UMIN 					( u1 u2 -- u1/u2 )
  2DUP U< IF
    DROP
  ELSE
    NIP 
  THEN ;

: rep-normal    			( addr u -- n true )
  1 MAX 2DUP [CHAR] 0 FILL 18 UMIN
  FABS FDUP DUP 10**n-0.5 F<
  IF    
    FDUP DUP DUP 1- 10**n-0.5 F<
    IF    
      FDUP FLOG FLOOR F>S 1+ DUP >R
      - f*10**n
    ELSE  
      >R
    THEN
  ELSE  
    FDUP FLOG FLOOR F>S 1+ DUP >R
    OVER - f10.0 f**n F/
  THEN
  FROUND FDUP f10.0 DUP f**n F<
  IF    
    FDUP f10.0 DUP 1- f**n F<
    IF    
      f10.0 F* R> 1- >R
    THEN
  ELSE  
    f10.0 F/ R> 1+ >R
  THEN
  fbcd-buf f>bcd DUP >R + 1- R> 0
  DO    
    I bcd-char@ OVER C! 1-
  LOOP
  DROP R> TRUE ;

: rep-denormal   			( addr u -- n true )
  rep-normal ;

\ February 6th, 1996 - 18:05 tjz added 'FDROP' to the following def to
\ correct for a floating point zero left on the stack during E. and F.S
: rep-zero   				( addr u -- 0 true )  ( f: r -- )
  [CHAR] 0 FILL 0 TRUE FDROP ;
        
: rep-spec   				( addr u cstr -- n false )  ( f: r -- )
  >R 2DUP BLANK R> COUNT          	\ addr1 u addr2 v
  ROT UMIN >R SWAP R> MOVE
  $$ 7FFFFFFF 0 FDROP ;

: REPRESENT   				( addr u -- n flag1 flag2 ) ( f: r -- )
  fexam 8 RSHIFT                        \ get type of operand
  DUP 2 AND 0<> >R                      \ save sign = flag1
  FABS
  $$ 45 AND                             \ clear C1
  CASE  
    $$ 00 OF C" Unsupported" rep-spec ENDOF
    $$ 01 OF C" NAN"         rep-spec ENDOF
    $$ 04 OF rep-normal               ENDOF
    $$ 05 OF C" Infinity"    rep-spec ENDOF
    $$ 40 OF rep-zero                 ENDOF
    $$ 41 OF C" Empty"       rep-spec ENDOF
    $$ 44 OF rep-denormal             ENDOF
             C" Unknown"     rep-spec 
 ENDCASE
 R> SWAP ;

: xsign         			( char -- char false | nflag true )
  CASE  
    [CHAR] + OF false true ENDOF
    [CHAR] - OF true  true ENDOF
                false over
  ENDCASE ;

: e-char        			( char -- true | char false )
  CASE  
    [CHAR] D OF true       ENDOF
    [CHAR] d OF true       ENDOF
    [CHAR] E OF true       ENDOF
    [CHAR] e OF true       ENDOF
                false over
  ENDCASE ;

: 10digit 				( char -- n flag? )
  DUP 					
  10 (char>digit) 			\ char n false / char true
  INVERT 				\ char n true / char false
  DUP IF ROT DROP THEN 
  ;

: digit0  DUP [CHAR] 0 = ;

VARIABLE expsign
VARIABLE intcnt
VARIABLE fracnt
VARIABLE expcnt
VARIABLE charcnt
VARIABLE zerochar
CREATE $fsignif 128 ALLOT

: next-char     			( addr1 -- addr2 char flag ) \ flag = 0 means valid char
  COUNT -1 charcnt +!  charcnt @ 0< ;

CODE sig>f
  fbcd-buf #[] fbld,
  #? FSP #[] ecx mov,
  FSTACK +relocate [ecx] fstpf,
  B/FLOAT ## ecx add,
  ecx #? FSP #[] mov,
ENDCODE


: >float-int    			( f: -- r )
  intcnt @ 18 MIN DUP 1- SWAP 0
  ?DO     
    $fsignif I + C@ OVER bcd-char! 1-
  LOOP
  DROP sig>f
  intcnt @ 18 >                           \ allow looong input
  IF   
    $fsignif 18 + C@ 5 >=
    IF    
      $fsignif 18 + C@ 5 >
      IF    
        f1.0 F+
      ELSE  
        $fsignif 17 + C@ 1 AND
	IF    
	  f1.0 F+
	ELSE  
	  intcnt @ 19
	  ?DO   
	    $fsignif I + C@
	    0<> IF   
	      f1.0 F+ LEAVE
	    THEN
	  LOOP
	THEN
      THEN
    THEN
    f10.0 intcnt @ 18 - f**n F*
  THEN ;  

: >float-int.frac  			( f: -- r )
  >float-int f10.0 fracnt @ NEGATE f**n F* ;

: init->float   			( -- )
  0 expsign !  0 intcnt !               \ initialize various
  0 fracnt !  0 expcnt !                \ counts and such
  0 zerochar !
  fbcd-buf 10 ERASE                     \ clear bcd buffer
  $fsignif 128 ERASE                    \ clear intermed. buf
  ;

0 VALUE DOUBLE?

-1 VALUE DP-LOCATION

\ a version of NUMBER? that detects a decimal point.
: NUMBER?       			( addr len -- d1 f1 )
  FALSE TO DOUBLE?                	\ initially not a double #
  -1 TO DP-LOCATION
  OVER C@ [CHAR] - =
  OVER 0> AND DUP >R 
  IF 1 /STRING THEN
  DUP 0= 
  IF R> DROP FALSE EXIT THEN
  0 0 2SWAP >NUMBER
  OVER C@ [CHAR] .  = 			\ next char is a '.'
  OVER 0> AND 				\ more chars to look at
  IF 
    DUP 1- TO DP-LOCATION 1 /STRING 
    >NUMBER DUP 0= IF
      TRUE TO DOUBLE?  			\ mark as a double number
    THEN
  THEN
  NIP 0= R> IF >R DNEGATE R> THEN ;

: >FLOAT        ( addr len -- f )  ( f: -- r | <nothing> )
( OK )
        DUP 0=
        IF      DROP DROP FALSE EXIT
        THEN
        128 umin charcnt !                      \ save character count
        init->float
        next-char drop                          \ get first character
        dup bl =
        IF      drop charcnt @ bl-skip charcnt !
                next-char
                IF      drop drop f0.0
                        true EXIT               \ special case of 0.0
                THEN
        THEN
        xsign
        IF      IF      8       ELSE    0  THEN
                19 bcd-char!
                next-char
                IF      drop drop false EXIT
                THEN
        THEN
        BEGIN   digit0                          \ check for leading 0's
        WHILE   drop next-char
                IF      drop drop f0.0 true EXIT
                THEN
                true zerochar !
        REPEAT
        10digit
        IF      
                BEGIN   $fsignif intcnt @ + c!
                        1 intcnt +!
                        next-char
                        IF      drop drop >float-int true EXIT
                        THEN
                        10digit 0=
                UNTIL
                false zerochar !
        THEN
        dup [CHAR] . =                           \ decimal point?
        IF      
	        drop next-char
                IF      drop drop >float-int true EXIT
                THEN
                intcnt @ 0=
                IF
                        BEGIN   digit0
                        WHILE   drop next-char
                                IF      drop drop f0.0 true EXIT
                                ELSE    1 fracnt +!
                                THEN
                                true zerochar !
                        REPEAT
                THEN
                10digit
                IF      false zerochar !        
                        BEGIN   $fsignif intcnt @ + c!
                                1 intcnt +!
                                1 fracnt +!
                                next-char
                                IF      drop drop >float-int.frac
                                        true EXIT
                                THEN
                                10digit 0=
                        UNTIL
                THEN
        THEN
        e-char
        IF                                      \ exponent indicator
\                zerochar @                      \ optimization, mantissa=0
\                IF      drop f0.0 true EXIT     \ then whole number is zero
\                THEN                            \ unfortunately skips validation
                intcnt @ 0=
                IF      ( SMuB 07-20-95 drop ) f1.0 19 bcd-char@
                        [CHAR] 0 <>  ( <-- SMuB 07-20-95 )
                        IF      fnegate
                        THEN
                ELSE    >float-int
                THEN
                next-char
                IF      drop drop fracnt @
                        IF      f10.0 fracnt @ negate f**n f*
                        THEN
                        zerochar @                      \ mantissa=0?
                        IF      fdrop f0.0              \ then make result 0.0
                        THEN
                        true EXIT
                THEN
                xsign
                IF      >r next-char
                        IF      R> DROP DROP DROP
                                fracnt @
                                IF      f10.0 fracnt @
                                        negate f**n f*
                                THEN
                                zerochar @              \ mantissa=0?
                                IF      fdrop f0.0      \ then make result 0.0
                                THEN
                                TRUE EXIT
                        THEN
                ELSE    0 >r
                THEN
        ELSE    >float-int
                xsign
                IF      >r next-char
                        IF    R> DROP DROP DROP FDROP FALSE EXIT
                              \ SMuB r>drop drop ( SMuB 07-20-95 --> ) drop
                              \ SMuB fracnt @
                              \ SMuB IF      f10.0 fracnt @
                              \ SMuB        negate f**n f*
                              \ SMuB THEN
                              \ SMuB true EXIT
                        THEN
                ELSE    drop drop false EXIT
                THEN
        THEN
        drop 1- charcnt @ 1+ number?
        double? 0= and                  \ October 1st, 1996 - 10:51 tjz & am            
                                        \ double exponent not allowed
        IF      d>s r>
                IF      negate
                THEN
                fracnt @ negate + f10.0 f**n f*
                zerochar @                              \ mantissa=0?
                IF      fdrop f0.0                      \ then make result 0.0
                THEN
                true
        ELSE    
                2DROP FALSE R> DROP EXIT
        THEN ;

: f#            			( F: -- r )
  skip-space BL PARSE >FLOAT
  0= ABORT" invalid floating point number"
  STATE @
  IF POSTPONE FLITERAL THEN ; IMMEDIATE


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Output conversion.
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

VARIABLE sigdigits      6 sigdigits !           \ default = 6

B/FLOAT 10 = [IF]  18  [ELSE]  16  [THEN]  
CONSTANT maxsig

: PRECISION     				( -- n )
  sigdigits @ ;

: SET-PRECISION 				( n -- )
  sigdigits ! ;

: F.            				( F: r -- )
  FDEPTH 0 <= IF -45 THROW THEN
  PRECISION 1 MAX SET-PRECISION
  fexam $$ 0200 and
  IF FABS ." -" THEN
  FDUP F0.5 F< IF  
    ." ." f1.0 F+ $ftemp
    PRECISION 1+ maxsig UMIN
    REPRESENT
    DROP DROP DROP
    $ftemp 1+ PRECISION maxsig 1- UMIN
    TYPE SPACE
  ELSE 
    $ftemp PRECISION REPRESENT 0= IF 
      DROP DROP $ftemp PRECISION TYPE SPACE
    ELSE 
      DROP DUP PRECISION < IF 
        DUP 0= IF  
	  DROP ." ." $ftemp PRECISION TYPE SPACE EXIT
        THEN
        $ftemp OVER TYPE ." ."
        $ftemp OVER + SWAP PRECISION
        SWAP - TYPE SPACE
      ELSE  
        DUP PRECISION = 
	IF $ftemp SWAP TYPE ." . " EXIT THEN
        $ftemp PRECISION TYPE
        PRECISION - 0
        DO 
	  [CHAR] 0 EMIT
        LOOP
        ." . "
     THEN
    THEN
  THEN ;

: FE.           				( F: r -- )     \ display fp # in engineering notation
  PRECISION 3 MAX SET-PRECISION
  fexam $$ 0200 AND 
  IF FABS ." -" THEN
  $ftemp PRECISION REPRESENT 0= IF   
    DROP DROP
    $ftemp PRECISION TYPE SPACE EXIT
  THEN
  DROP 1- 3 /MOD SWAP 1+ $ftemp OVER TYPE ." ."
  $ftemp OVER + SWAP PRECISION SWAP - TYPE
  ." E" 3 * BASE @ >R DECIMAL . R> BASE ! ;

: e.            				( f: r -- )
  PRECISION 1 MAX SET-PRECISION
  fexam $$ 0200 AND
  IF  FABS ." -" THEN
  $ftemp PRECISION REPRESENT 0=
  IF 
    DROP DROP $ftemp PRECISION TYPE SPACE EXIT 
  THEN
  DROP 1- $ftemp 1 TYPE ." ."
  $ftemp 1+ PRECISION 1- TYPE
  ." E" BASE @ >R DECIMAL . R> BASE ! ;

: g.            				( f: r -- )
  FDUP FABS f10.0 PRECISION 2/ NEGATE f**n F<
  IF  e. EXIT THEN
  FDUP FABS f10.0 PRECISION f**n F<
  IF f.  ELSE e. THEN ;

: FS.           				( f: r -- r )   \ display fp # in scientific notation
  e. ;

6 VALUE show-fp-depth

: f.s           				( -- )  \ display floating point stack
  FDEPTH IF  
    FDEPTH CR ." {" 1 .r ." } "
    show-fp-depth FDEPTH UMIN DUP 1- SWAP 0
    DO    
      DUP I - fpick g.  
    LOOP DROP
  ELSE   
    ." Empty fp stack "
  THEN ;

: .fdepth       ( -- )
   ." Items; " FDEPTH .
   ." Bytes: " FSP @ . ;

GET-CURRENT ENVIRONMENT-WORDLIST SET-CURRENT

: FLOATING ;

: FLOATING-EXT ;

fstack-elements CONSTANT FLOATING-STACK
fbig FCONSTANT MAX-FLOAT

SET-CURRENT

\ ******************************************************************
\ Debugging tools
\ ******************************************************************

: (tag)   ( n1 addr -- n2 )          \ Returns the 2 bit tag for ST(n1)
        DUP >R CELL+ @ $$ 3800 AND $$ 0800 / +
        7 AND
        R> 2 CELLS + @ SWAP 0
        ?DO     2/ 2/
        LOOP
        3 AND ;

: .ftag2   ( i -- )
        allregs (tag)
        s" valid  zero   specialempty  " 7 s. ;

: npu-depth   ( -- n )
\        FR1 >FREGS>
        0 0 7
        DO      I allregs (tag) 3 <>
                IF      DROP I 1+ LEAVE
                THEN
        -1 +LOOP ;

77 CONSTANT FPDISPSIZE
allregs 7 CELLS + CONSTANT npstk

: .tenbyte   ( addr -- )               \ Needs lots of work yet.
        10 + 10 0
        DO      1- DUP C@ S>D <# # # #> TYPE SPACE
        LOOP
        DROP ;

: h.npustack   ( -- )
        8 0
        DO      cr ." ST(" I 1 .r ." ) "
                npstk I B/FLOAT * + .tenbyte
                I .ftag2
        LOOP
        ;

: .fmask   ( n -- )
        0 5
        DO      SPACE DUP I RSHIFT 1 AND
                I s" PrecUfloOfloZeroDnrmInvl" 4 s.
                ." =" 1 .R
        -1 +LOOP
        DROP ;

: .fstatus   ( n -- )
        ."  Flags: "
        0 7
        DO      DUP I RSHIFT 1 AND
                i s" ESSFPEUEOEZEDEIE" 2 s. ." =" .
        -1 +LOOP
        DROP ;

: .ftag1   ( n -- )
        4 base !
        0 7
        DO      DUP I 2* RSHIFT 3 AND .
        -1 +LOOP
        DROP ;

: fdump ( -- )      \ Dump of the real Floating Point Unit
        cr cr ."      Dump of the Floating Point NPU "
        allregs >fregs>  cr
        base @ >r
        allregs @ dup
        ." Cntl:  " hex $$ 0FFFF and 4 .r
        dup 10 rshift 3 and
        ."  Rnd-" S" neardownup  chop" 4 s.
        dup 8 rshift 3 and
        ."  Size = " S" sngl????dbl ext " 4 s.
        cr ."  Msk: " .fmask
        cr allregs cell+ @
        ." Status: " $$ 0FFFF and dup 4 .r space
        ." CC = " dup 8 rshift dup $$ 40 and
        IF      7 and 8 or
        ELSE    7 and
        THEN
        binary s>d <# # # # # #> type hex
        ."  Busy = " dup 15 rshift .
        ."  Top = " dup 11 rshift 7 and .
        cr .fstatus
        cr ." Tags: "
        allregs 2 CELLS + @ $$ FFFF and .ftag1
        cr ." IP = "
        allregs 3 CELLS + @ 8 .r
        cr ." Last Opcode = "  hex
        allregs 4 CELLS + @ dup 24 rshift
        $$ D8 or $$ 0FF and 3 .r
        dup 16 rshift $$ 0FF and 3 .r
        cr ."  CS Selector: " $$ 0FFFF and 4 .r
        cr ." Data Pointer: "
        allregs 5 CELLS + @ 8 .r
        cr ." Operand Pointer: "
        allregs 6 CELLS + @ $$ 0FFFF and 4 .r
        decimal
        cr ." NPU stack depth = " npu-depth .
        ."   Simulated stack depth = " fdepth .
        hex h.npustack cr
        r> base ! ;

\ display n1 as a hex number right justified in a field of n2 characters
: H.R           			( n1 n2 -- )    
  BASE @ >R HEX >R
  0 <# #S #> R> OVER - SPACES TYPE
  R> BASE ! ;

\ display n1 as a HEX number of n2 digits
: H.N           			( n1 n2 -- )    
  BASE @ >R HEX >R
  0 <# R> 0 ?DO # LOOP #> TYPE
  R> BASE ! ;

\ two digit HEX number
: H.2           			( n1 -- ) 
  2 H.N ;  
\ four digit HEX number
: H.4           			( n1 -- ) 
  4 H.N ;               
\ eight digit HEX number
: H.8           			( n1 -- ) 
  8 H.N ;               

: .ftempx        			( -- )
        0 B/FLOAT 1-
        DO      ftemp I + C@ h.2 SPACE
        -1 +LOOP ;

: f.x   ( f: r1 r2 ... rn -- r1 r2 ... rn )     \ Hex dump of fstack
        fdepth
        IF      fdepth cr ." {" 1 .r ." } "
                show-fp-depth fdepth umin dup 1- swap 0
                DO      cr dup i - fpick fdup ftemp f!
                        .ftempx 2 spaces g.
                LOOP
                drop
        ELSE    ." Empty fp stack "
        THEN ;

: interpret-float 			( addr len -- )
( OK )
  2DUP
  >FLOAT				\ addr len valid? / f:r
  IF
    STATE @ IF POSTPONE FLITERAL THEN 
    2DROP
  ELSE 					\ addr len
    in-chain other-numbers
  THEN ;

' interpret-float TO other-numbers
( OK )

: FINIT  				( -- )
( OK )
  0 TO FSP 
  INLINE 
  fninit,
  END-INLINE 
  in-chain initializer ;

' FINIT TO initializer
    
.( OK) CR

