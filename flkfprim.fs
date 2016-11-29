\  FLK floating point primitives
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

\ $Id: flkfprim.fs,v 1.1 1998/04/24 16:47:39 root Exp root $
\ $Log: flkfprim.fs,v $
\ Revision 1.1  1998/04/24 16:47:39  root
\ Initial revision
\
\ Revision 1.1  1998/04/15 18:15:30  root
\ Initial revision
\

p: F! 					( f-addr -- ) ( F: r -- )
   regalloc-reset
   req-any
   0 [tos0] fstp64,
   1 reg-free ;
   
p: F* 					( F: r1 r2 -- r1*r2 )
   regalloc-reset
   st1 fmulp,
   ;
   
p: F+ 					( F: r1 r2 -- r1+r2 )
   regalloc-reset
   st1 faddp,
   ;
   
p: F- 					( F: st1 st0 -- st1-st0 )
   regalloc-reset
   st1 fsubp,
   ;
   
p: F/ 					( F: st1 st0 -- st1/st0 )
   regalloc-reset
   st1 fdivp, ;
   
p: F@ 					( f-addr -- ) ( F: -- r )
   regalloc-reset
   req-any
   0 [tos0] fld64,
   1 reg-free ;
   
p: FDROP 				( F: r -- )
   regalloc-reset
   st0 fstp ;
   
p: FDUP 				( F: r -- r r )
   regalloc-reset
   st0 fld, ;
   
p: FNEGATE 				( F: r -- -r )
   regalloc-reset
   fchs, ;
   
p: FOVER 				( F: r1 r2 -- r1 r2 r1 )
   regalloc-reset
   st1 fld, ;
   
p: FROT 				( F: r1 r2 r3 -- r2 r3 r1 )
   regalloc-reset
   st1 fxch,
   st2 fxch, ;
   
p: FSWAP 				( F: r1 r2 -- r2 r1 )
   regalloc-reset
   st1 fxch, ;
   
p: DF! 					( addr -- ) ( F: r -- )
   regalloc-reset
   req-any
   0 [tos0] fstp64,
   1 reg-free ;
   
p: DF@ 					( addr -- ) ( F: -- r )
   regalloc-reset
   req-any
   0 [tos0] fld64,
   1 reg-free ;
   
p: FABS 				( F: r1 -- r2 )
   regalloc-reset
   fabs, ;
   
p: FCOS 				( F: r1 -- r2 )
   regalloc-reset
   fcos, ;
   
p: FSIN 				( F: r1 -- r2 )
   regalloc-reset
   fsin, ;
   
p: FSINCOS 				( F: r1 -- r2 r3 )
   regalloc-reset
   fsincos, ;
   
p: FSQRT 				( F: r1 -- r2 )
   regalloc-reset
   fsqrt, ;
   
p: SF! 					( addr -- ) ( F: r -- )
   regalloc-reset
   req-any
   0 [tos0] fstp32,
   1 reg-free ;
   
p: SF@ 					( addr -- ) ( F: -- r )
   regalloc-reset
   req-any
   0 [tos0] fld32,
   1 reg-free ;

p: F0< 					( -- flag ) ( F: r -- )
   regalloc-reset
   free-eax
   ftst,
   fnstswax,
   st0 fstp,
   sahf,
   free0l setl,
   31 ## free0 shl, 
   31 ## free0 sar,
   0 free>tos ;

p: F0<= 				( -- flag ) ( F: r -- )
   regalloc-reset
   free-eax
   ftst,
   fnstswax,
   st0 fstp,
   sahf,
   free0l setle,
   31 ## free0 shl, 
   31 ## free0 sar,
   0 free>tos ;

p: F0= 					( -- flag ) ( F: r -- )
   regalloc-reset
   free-eax
   ftst,
   fnstswax,
   st0 fstp,
   sahf,
   free0l setz,
   31 ## free0 shl, 
   31 ## free0 sar,
   0 free>tos ;

p: F0>= 				( -- flag ) ( F: r -- )
   regalloc-reset
   free-eax
   ftst,
   fnstswax,
   st0 fstp,
   sahf,
   free0l setge,
   31 ## free0 shl, 
   31 ## free0 sar,
   0 free>tos ;

p: F0> 					( -- flag ) ( F: r -- )
   regalloc-reset
   free-eax
   ftst,
   fnstswax,
   st0 fstp,
   sahf,
   free0l setg,
   31 ## free0 shl, 
   31 ## free0 sar,
   0 free>tos ;

p: F< 					( -- flag ) ( F: r1 r2 -- )
   regalloc-reset
   free-eax
   fcompp,
   fnstswax,
   sahf,
   free0l setl,
   31 ## free0 shl, 
   31 ## free0 sar, ;
   
p: F<= 					( -- flag ) ( F: r1 r2 -- )
   regalloc-reset
   free-eax
   fcompp,
   fnstswax,
   sahf,
   free0l setle,
   31 ## free0 shl, 
   31 ## free0 sar, ;
   
p: F= 					( -- flag ) ( F: r1 r2 -- )
   regalloc-reset
   free-eax
   fcompp,
   fnstswax,
   sahf,
   free0l sete,
   31 ## free0 shl, 
   31 ## free0 sar, ;
   
p: F> 					( -- flag ) ( F: r1 r2 -- )
   regalloc-reset
   free-eax
   fcompp,
   fnstswax,
   sahf,
   free0l setg,
   31 ## free0 shl, 
   31 ## free0 sar, ;
   
p: F>= 					( -- flag ) ( F: r1 r2 -- )
   regalloc-reset
   free-eax
   fcompp,
   fnstswax,
   sahf,
   free0l setge,
   31 ## free0 shl, 
   31 ## free0 sar, ;
   
p: F** 					( F: r1 r2 -- r1^r2 )
   regalloc-reset
   fyl2x,
   fxtract,
   f2xm1,
   fscale, ;
   
p: FEXP 				( F: r1 -- r2 )
   regalloc-reset
   fldl2e,
   st1 fmulp,
   fxtract,
   f2xm1,
   fscale, ;

p: FEXPM1 				( F: r1 -- r2 )
   regalloc-reset
   fldl2e,
   st1 fmulp,
   fxtract,
   f2xm1,
   fscale, 
   fld1,
   st1 fsubp, ;
   
p: FLN 					( F: r1 -- r2 )
   fldln2,
   st1 fxch, 
   fyl2x, ;
   
p: FLNP1 				( F: r1 -- r2 )
   regalloc-reset
   fld1,
   fldl2e,
   st1 fdivp,
   fyl2x,
   fld1,
   st1 faddp, ;
   
p: FALOG 				( F: r1 -- r2 )
   regalloc-reset
   fldl2t,
   st1 fmulp,
   fxtract,
   f2xm1,
   fscale, ;

p: FLOG 				( F: r1 -- r2 )
   regalloc-reset
   fld1,
   fldl2t,
   st1 fdivp,
   fyl2x, ;
   
p: FLOAT+ 				( addr1 -- addr2 )
   regalloc-reset
   req-any
   8 ## tos0 add, ;
   
p: FLOATS 				( n -- n*8 ) 
   regalloc-reset
   req-any
   3 ## tos0 shl, ;

p: DFLOAT+ 				( addr1 -- addr2 )
   regalloc-reset
   req-any
   8 ## tos0 add, ;
   
p: DFLOATS 				( n -- n*8 )
   regalloc-reset
   req-any
   3 ## tos0 shl, ;

p: SFLOAT+ 				( addr1 -- addr2 )
   regalloc-reset
   req-any
   4 ## tos0 add, ;

p: SFLOATS 				( n -- n*4 )
   regalloc-reset
   req-any
   2 ## tos0 shl, ;

p: FLOOR 				( F: r1 -- r2 )
   regalloc-reset
   ebp push,
   $$ 77f ## push,
   esp ebp mov,
   0 [ebp] fldcw,
   frndint,
   ebp pop,
   $$ 37f ## push,
   esp ebp mov,
   0 [ebp] fldcw,
   ebp pop,
   ;
   
p: FROUND  				( F: r1 -- r2 )
   regalloc-reset
   frndint,
   ;

\ ---------------------------------------------------------------
   
p: F1.0 					( F: -- 1.0 )
   fld1, ;

p: F2/ 						( F: r -- r/2 )
   fld1,
   fchs,
   st1 fxch,
   fscale, ;
   
p: FACOS 					( -- )
   regalloc-reset
   
   ;
   
p: FASIN 					( -- )
   regalloc-reset
   
   ;
   
p: FATAN 					( -- )
   regalloc-reset
   
   ;
   
p: FATAN2 					( -- )
   regalloc-reset
   
   ;
   
p: FTAN 					( -- )
   regalloc-reset
   
   ;
   
