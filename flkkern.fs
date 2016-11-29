\  FLK kernel words (core word set)
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

\ $Id: flkkern.fs,v 1.15 1998/09/13 15:42:04 root Exp $
\ $Log: flkkern.fs,v $
\ Revision 1.15  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.14  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.13  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.12  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.11  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.10  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.9  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.8  1998/05/09 21:47:05  root
\ moved some words to flktools.fs
\
\ Revision 1.7  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.6  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.5  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.4  1998/04/24 20:23:34  root
\ cleaning up
\ char>digit case insensitiv
\
\ Revision 1.3  1998/04/24 16:47:39  root
\ bug fixes
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ FM/REM, */MOD etc. fixed
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ See standard.
VARIABLE >IN 

\ See standard.
VARIABLE BASE

\ See standard.
0  VALUE HERE

\ HERE for code area
0  VALUE CHERE 				

\ See standard.
VARIABLE STATE 

\ See standard.
VARIABLE #TIB 

\ See standard.
VARIABLE SPAN 

\ Floating point stack pointer.
0  VALUE FSP

\ See standard.
32	CONSTANT BL

\ See standard.
0	CONSTANT FALSE

\ See standard.
-1	CONSTANT TRUE

\ See standard.
1	CONSTANT R/O

\ See standard.
2	CONSTANT R/W

\ See standard.
4	CONSTANT BIN

\ See standard.
8	CONSTANT W/O

\ LEAVE handling
0 VALUE last-leave
\ Contains the allocator state of the current DO or ?DO.
CREATE do-state 11 ALLOT

\ See standard.
: ERASE 0 FILL ;
( OK )

\ See standard.
: DABS 
( OK )
  2DUP D0< IF DNEGATE THEN ;

\ See standard.
: ?DUP 
( OK )
  DUP IF DUP THEN ;

\ See standard.
: HEX 16 BASE ! ;
( OK )

\ See standard.
: DECIMAL 10 BASE ! ;
( OK )

\ See standard.
: MOVE 					( from to len -- )
( OK )
  -ROT 2DUP U< IF ROT CMOVE> ELSE ROT CMOVE THEN ;

\ See standard.
: -TRAILING 				( c-addr u1 -- c-addr u2 )
( OK )
  BEGIN
    DUP 0<>
  WHILE
    1- 2DUP CHARS + C@ 
    BL > IF 				\ c-addr u1-1
      1+ EXIT
    THEN
  REPEAT ;

\ See standard.
: BLANK BL FILL ;
( OK )

\ See standard.
: /STRING 				( a1 n1 n2 -- a2 n3 )
( OK )
  DUP 0> IF 				\ a1 n1 n2 
    2DUP > INVERT IF
      DROP DUP
    THEN
  THEN
  ROT 					\ n1 n2 a1
  OVER + 				\ n1 n2 a2
  -ROT 					\ a2 n1 n2
  - ;

\ See standard.  Taken from gforth without inspection.
: M*/ 					( d1 n2 u3 -- dqout )
( OK )
  >R S>D >R ABS -ROT
  S>D R> XOR R> SWAP >R >R DABS ROT TUCK UM* 2SWAP UM*
  SWAP >R 0 D+ R> -ROT R@ UM/MOD -ROT R> UM/MOD NIP SWAP
  R> IF DNEGATE THEN ;
 
\ See standard. Taken from FPC with little inspection.
: FM/MOD 				( d1 n1 -- n2 n3 )
( OK )
  DUP >R ABS 				\ Save n1 and make it +ve.
  ROT ROT DUP >R DABS 			\ Save d1 and make it +ve.
  ROT UM/MOD 				\ -- n2 n3 )
  R> R@ SignsDiffer? IF 		\ If the signs of d1 & n1 differ ...
    OVER IF 				\ if the remainder n2 <> 0 ...
      1+ 				\ increment the quotient n3.
      R@ ABS ROT - SWAP 		\ n2 = n2 - n1
    THEN 
    NEGATE 				\ n3 = -n3
  THEN
  R> 0< IF 				\ If n1 -ve ...
    SWAP NEGATE SWAP 			\ n2 = -n2
  THEN
;
					 
\ See standard.
: */MOD ( n1 n2 n3 -- n4 n5)  -ROT M* ROT SM/REM ;
( OK )

\ See standard.
: */  ( n1 n2 n3 -- n4 )   */MOD SWAP DROP ;
( OK )

\ See standard.
: /MOD ( n1 n2 -- n3 n4)  SWAP S>D ROT SM/REM ;
( OK )

\ See standard.
: WITHIN 				( x lo hi -- flag )
( OK )
  2DUP < INVERT IF SWAP THEN 		\ x lo hi
  ROT 					\ lo hi x
  TUCK  				\ lo x hi x
  >  					\ lo x f1
  -ROT 					\ f1 lo x
  > INVERT AND ;

\ Convert a lower case letter to upper case.
: >UPPER 				( char -- char2)
( OK )
  DUP 97 123 WITHIN IF
    32 XOR
  THEN ;

\ Case insensitive compare.
: CAPS-COMPARE 				( c-addr1 u1 c-addr2 u2 -- n )
( OK )
  ROT 2DUP MIN 				\ c1 c2 u2 u1 um
  -ROT 2>R 				\ c1 c2 um
  0 ?DO 				\ c1 c2
    2DUP C@ >UPPER SWAP C@ >UPPER 	\ c1 c2 ch2 ch1
    2DUP > IF 
       UNLOOP 2R> 2DROP 2DROP 
       2DROP -1 EXIT
    ELSE 				\ c1 c2 ch2 ch1
       < IF 
	 UNLOOP 2R> 2DROP 2DROP 1 EXIT
       THEN
    THEN 				\ c1 c2
    SWAP 1+ SWAP 1+
  LOOP 					\ c1 c2
  2DROP 2R> 				\ u2 u1
  2DUP > IF 				\ u2 u1
    -1
  ELSE
    2DUP < IF
      1
    ELSE
      0
    THEN
  THEN 					\ u2 u1 n
  NIP NIP ;

\ See standard.
: COMPARE 				( c-addr1 u1 c-addr2 u2 -- n )
( OK )
  ROT 2DUP MIN 				\ c1 c2 u2 u1 um
  -ROT 2>R 				\ c1 c2 um
  0 ?DO 				\ c1 c2
    2DUP C@ SWAP C@ 			\ c1 c2 ch2 ch1
    2DUP > IF 
       UNLOOP 2R> 2DROP 2DROP 
       2DROP -1 EXIT
    ELSE 				\ c1 c2 ch2 ch1
       < IF 
	 UNLOOP 2R> 2DROP 2DROP 1 EXIT
       THEN
    THEN 				\ c1 c2
    SWAP 1+ SWAP 1+
  LOOP 					\ c1 c2
  2DROP 2R> 				\ u2 u1
  2DUP > IF 				\ u2 u1
    -1
  ELSE
    2DUP < IF
      1
    ELSE
      0
    THEN
  THEN 					\ u2 u1 n
  NIP NIP ;

\ Multiply a double by a single returning a double. The unused third cell is
\ assumed to be zero.
: UMD* 					( ud1l ud1h u -- ud2 )
( OK )
  TUCK 					\ ud1l u ud1h u
  UM* 					\ ud1l u ud2hl ud2hh
  DROP 					\ ud1l u ud2hl
  -ROT 					\ ud2hl ud1l u
  UM* 					\ ud2hl ud2lh ud2ll
  ROT + 				\ ud2l ud2h
;

\ Convert a character using the given base and return a failure flag.
: (char>digit) 				( char base -- digit FALSE | TRUE )
( OK )
  DUP 10 > IF ( hex etc . ) 		\ char base
    OVER 48 58 WITHIN IF 		\ char base
      DROP 48 - FALSE
    ELSE 				\ char base
      OVER 65 55 			\ c b c 65 55
      FLOCK + WITHIN IF      		\ c b
	DROP 55 - FALSE
      ELSE 				\ char base
        OVER 97 87 FLOCK + WITHIN IF 	\ char base
	  DROP 87 - FALSE
	ELSE  				\ char base
  	  2DROP TRUE
	THEN
      THEN
    THEN
  ELSE ( decimal or less )		\ char base
    OVER 48 DUP 			\ c b c 48 48 
    FLOCK + WITHIN IF 			\ c b 
      DROP 48 - FALSE
    ELSE
      2DROP TRUE
    THEN
  THEN ;

\ Convert a character with the current base.
: char>digit BASE @ (char>digit) ;

\ See standard.
: >NUMBER 				( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
( OK )
  BEGIN 				\ ud1 c1 u1
    DUP 
  WHILE 				\ ud1 c1 u1
    OVER C@ 				\ ud1 c1 u1 char
    DUP 48 >= IF 
      char>digit ?EXIT 			\ ud1 c1 u1 digit
      -ROT 				\ ud1 digit c1 u1
      2>R >R 				\ ud1 
      BASE @ 	 			\ ud1 base
      UMD* R> M+
      2R> 1 /STRING
    ELSE
      DROP EXIT
    THEN
  REPEAT ;

\ See standard.
: CONVERT
( OK )
  0 0 2SWAP >NUMBER DROP ;

\ See standard.
: UNUSED 				( -- n )
( OK )
  HERE-LIMIT HERE - ;
  
\ Same as UNUSED but for code area.
: CUNUSED 				( -- n )
( OK )
  CHERE-LIMIT CHERE - ;

#PAD-LEN CONSTANT #PAD-LEN

\ See standard.
: PAD HERE-LIMIT #PAD-LEN - ;
( OK )

CREATE (SEARCH-RESET-TABLE) 256 CELLS ALLOT

\ Initialize the string search table from the given string.
: (setup-search-table) 			( caddr u -- )
( OK )
  (SEARCH-RESET-TABLE) 256 CELLS -1 FILL ( fill reset table with -1 )
  0 DO 					\ c2
    DUP C@ 				\ c2 char
    CELLS (SEARCH-RESET-TABLE) + 	\ c2 addr
    DUP @ -1 = IF 			\ c2 addr
      I SWAP ! 				\ c2
    ELSE 				\ c2 addr
      DROP
    THEN 				\ c2
    CHAR+
  LOOP DROP ;

0 VALUE (search-addr)
0 VALUE (search-len)

\ Boyer/Moore search comparator word
: (search-comp) 			( pa pl sa -- offs char FALSE / TRUE )
( OK )
  OVER 					\ pa pl sa pl
  1- + 					\ pa pl sae
  -ROT TUCK 				\ sae pl pa pl
  1- + SWAP 				\ sae pae pl
  BEGIN 				\ sae pae pl
    1-
    -ROT 2DUP C@ SWAP C@ 		\ pl sae pae pc sc
    TUCK <> IF 				\ pl sae pae sc 
      NIP NIP FALSE EXIT
    THEN 				\ pl sae pae sc
    DROP CHAR- SWAP CHAR- SWAP 		\ pl sae+1 pae+1
    ROT
    DUP 0=
  UNTIL 2DROP DROP TRUE 
  ;

\ See standard.
: SEARCH 				( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
( OK )
  PLUCK OVER OR 0= 			\ c1 u1 c2 u2 empty?
  IF 2DROP FALSE EXIT THEN
  2DUP (setup-search-table)
  2OVER TO (search-len) 
  TO (search-addr) 2SWAP 		\ c2 u2 c1 u1
  BEGIN 				\ c2 u2 c1 u1
    PLUCK OVER 				\ c2 u2 c1 u1 u2 u1
    <
  WHILE 				\ c2 u2 c1 u1 
    2OVER FLOCK 			\ c2 u2 c1 u1 c2 u2 c1 
    (search-comp) 			\ c2 u2 c1 u1 ( offs char FALSE / TRUE )
    IF 					\ c2 u2 c1 u1 
      2SWAP 2DROP TRUE EXIT
    THEN 				\ c2 u2 c1 u1 offs char
    CELLS (SEARCH-RESET-TABLE) + @ 
    -
    /STRING
  REPEAT
  2DROP 2DROP
  (search-addr) (search-len) FALSE ;

\ Transfer n items and n itself to the return stack. An inlined version must
\ not save the top of return stack.
: n>R 					( ... n -- ) ( r: -- ... n )
( OK )
  R> 					\ ... n ret
  OVER BEGIN 				\ ... n ret cnt
    DUP
  WHILE 				\ ... x n ret cnt
    TURN >R
    1-
  REPEAT 				\ n ret 0
  DROP SWAP >R >R
  ;

\ Transfer n items from the return to the data stack. An inlined version must
\ not save the top of return stack.
: nR> 					( -- ... n ) ( r: ... n -- )
( OK )
  R> 					\ ret
  R> DUP 				\ ret n cnt
  BEGIN
    DUP 
  WHILE 				\ ret n cnt
    R> -TURN 				\ x ret n cnt
    1-
  REPEAT 				\ ... ret n 0
  DROP SWAP >R ;

\ The xt of the last defined word
0 VALUE lastheader

