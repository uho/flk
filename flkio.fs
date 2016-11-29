\  FLK basic input/output routines
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

\ $Id: flkio.fs,v 1.10 1998/09/21 11:25:20 root Exp $
\ $Log: flkio.fs,v $
\ Revision 1.10  1998/09/21 11:25:20  root
\ fixed SPACES
\
\ Revision 1.9  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.8  1998/07/06 18:01:13  root
\ "HOLD added
\
\ Revision 1.7  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.6  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.5  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.4  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.3  1998/04/11 11:55:44  root
\ FORGET/MARKER support added
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ all words checked and OK
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

((pnolen)) CONSTANT ((pnolen))

\ The buffer of the pictured numeric output is of a fixed size and at a fixed
\ address.
VARIABLE ((pnoptr))
CREATE ((pnobuf)) ((pnolen)) CHARS ALLOT 

\ See standard.
: <# 					( -- )
( OK )
  ((pnolen)) ((pnoptr)) ! ;

\ See standard.
: #> 					( d -- c-addr n )
( OK )
  2DROP ((pnoptr)) @ 	 		\ pptr
  ((pnolen)) OVER - 			\ pptr n
  ((pnobuf)) ROT + 	 		\ n c-addr
  SWAP ;

\ See standard.
: HOLD 					( char -- )
( OK )
  ((pnoptr)) @ 0= IF -17 THROW THEN
  -1 ((pnoptr)) +!
  ((pnoptr)) @ ((pnobuf)) + C! ;

\ Store a complete string into the pictured numeric buffer, (Reverses the
\ string, give it in normal order.)
: "HOLD 				( addr len -- )
  TUCK + SWAP 				\ e-addr len
  0 ?DO
    CHAR-
    DUP C@ HOLD
  LOOP DROP ;

\ Divides a double by a single and returns a single remainder and a double
\ qoutient. Adapted from gforth to use the data stack instead of the return
\ stack.
: UD/MOD 				( ud1l ud1h u2 -- urem udql udqh )
( OK )
  -ROT 					\ u2 ud1l ud1h
  0  					\ u2 ud1l ud1h 0
  FLOCK 				\ u2 ud1l ud1h 0 u2
  UM/MOD  				\ u2 ud1l ud1hd-rem ud1hd-quot
  -ROT 					\ u2 ud1hd-qout ud1l ud1hd-rem
  TURN 					\ ud1hd-qout ud1l ud1hd-rem u2
  UM/MOD 				\ ud1hd-qout ud1-r-rem ud1-r-qout
  ROT 					\ ud1-r-rem ud1-r-qout ud1hd-qout
;

\ See standard.
: #  					( ud1 -- ud2 )
( OK )
  BASE @ UD/MOD 			\ rem dquot
  ROT 	 				\ dq rem
  9 OVER < IF
    7 +
  THEN
  48 ( [CHAR] 0 ) + HOLD 		\ dq
  ;

\ See standard.
: #S 					( ud -- 0 0 )
( OK )
  BEGIN 
    #  					\ ud
    2DUP D0=
  UNTIL ;

\ See standard.
: SIGN 					( n -- )
( OK )
  0< IF
    45 ( CHAR] - ) HOLD
  THEN ;

\ See standard.
: SPACE BL EMIT ;
( OK )

\ See standard.
: SPACES ( n -- ) 0 MAX 0 ?DO SPACE LOOP ;
( OK )

\ Converts a number to a string and returns the address.
: (U.) 					( u -- a l )
( OK ) 
  0 <# #S #> ;

\ See standard.
: U. 					( u -- )
( OK )
  (U.) TYPE SPACE ;

\ See standard.
: U.R 					( u l -- )
( OK )
  >R (U.) R> OVER - SPACES TYPE ;

\ Like spaces.
: ZEROS 				( n -- )
( OK )
  0 ?DO
    48 ( [CHAR] 0 ) EMIT
  LOOP ;

\ Fills the empty place with zeros.
: U.0 					( u l -- )
( OK )
  SWAP (U.) ROT OVER - ZEROS TYPE ;

\ Converts a number to a string and returns the address.
: (.) 					( n -- a l )
( OK )
  DUP ABS 0 <# #S ROT SIGN #> ;

\ See standard.
: . 					( n -- )
( OK )
  (.) TYPE SPACE ;

\ See standard.
: .R 					( n l -- )
( OK )
  SWAP (.) ROT OVER - SPACES TYPE ;

\ Converts a number to a string and returns the address.
: (UD.) 				( ud -- a l )
( OK )
  <# #S #> ;

\ Writes an unsigned double to the console.
: UD. 					( ud -- )
( OK )
  (UD.) TYPE SPACE ;

\ Writes an unsigned double to the console.
: UD.R 					( ud l -- )
( OK )
  >R (UD.) R> OVER - SPACES TYPE  ;

\ Converts a number to a string and returns the address.
: (D.) 					( d -- a l )
( OK )
  TUCK DABS <# #S ROT SIGN  #> ;

\ See standard.
: D. 					( d -- )
( OK )
  (D.) TYPE SPACE ;

\ See standard.
: D.R 					( d l -- )
( OK )
  >R (D.) R> OVER - SPACES TYPE ;

\ Returns TRUE if the given character is considered a space.
: whitespace? 				( char -- )
( OK )
  BL <=
;

\ Draws a horizontal line of w characters starting at x,y.
: CHAR-H-LINE+ 					( x y w -- )
( OK )
  -ROT AT-XY 					\ w
  ?DUP 0= ?EXIT
  1- ." +"
  ?DUP 0= ?EXIT 				\ w-1
  1 ?DO ." -" LOOP
  ." +" ;
  
\ Draws a vertival line of h characters starting at x,y.
: CHAR-V-LINE+ 					( x y h -- )
( OK )
  -ROT 2DUP AT-XY ROT 				\ x y h
  ?DUP 0= IF 2DROP EXIT THEN 
  1- 						\ x y h-1
  ." +"
  0 ?DO 1+ 2DUP AT-XY ." |" LOOP 		\ x y+h-1
  AT-XY ." +" ;

\ Draws a box of the given width and height with the top left corner at x,y.
: CHAR-FRAME 					( x y width height -- )
( OK )
  -TURN 					\ h x y w
  3DUP CHAR-H-LINE+ 				\ h x y w 
  2OVER 2OVER 					\ h x y w h x y w
  TURN ROT + 					\ h x y w x w y+h
  1- SWAP CHAR-H-LINE+ 				\ h x y w
  -ROT TURN 					\ w x y h
  3DUP CHAR-V-LINE+ 				\ w x y h
  2SWAP + 1-  					\ y h x+w-1
  -ROT CHAR-V-LINE+ ;

\ Type the time.
: .TIME 					( sec min hr -- )
( OK )
  2 .R ." :" 2 U.0 ." :" 2 U.0 ;

\ What's my name?
: MONTH>NAME 					( mnth -- addr len )
( OK )
  CASE 
    1 OF S" Jan." ENDOF
    2 OF S" Feb." ENDOF
    3 OF S" Mar." ENDOF
    4 OF S" Apr." ENDOF
    5 OF S" May " ENDOF
    6 OF S" Jun." ENDOF
    7 OF S" Jul." ENDOF
    8 OF S" Aug." ENDOF
    9 OF S" Sep." ENDOF
   10 OF S" Oct." ENDOF
   11 OF S" Nov." ENDOF
   12 OF S" Dec." ENDOF
   S" unknown" ROT
  ENDCASE ; 

\ Type the date.
: .DATE 					( day mn yr -- )
( OK )
   SWAP ROT 					\ yr mn day
   . MONTH>NAME TYPE SPACE . ;

\ Type time and date.
: .TIME&DATE 					( sec min hr day mn yr -- )
( OK )
   .DATE SPACE SPACE .TIME SPACE ;

\ What's the time NOW?
: .NOW TIME&DATE ." Today is the " .DATE ." . It's now " .TIME ." ." CR ;
( OK )

\ Write an address.
: .addr 0 HEX ." 0x" <# # # # # # # # # #> TYPE SPACE DECIMAL ;
( OK )

\ Type a flag.
: .FLAG IF ." true" ELSE ." false" THEN ;
( OK )

\ Quoted emit.
: q-EMIT ." ``" EMIT ." '' " ;
( OK )

\ Quoted type.
: q-TYPE ." ``" TYPE ." ''" ;
( OK )


