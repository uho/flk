\  FLK EKEY support
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

\ $Id: flkkey_unix.fs,v 1.9 1998/07/13 18:08:54 root Exp $
\ $Log: flkkey_unix.fs,v $
\ Revision 1.9  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.8  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.7  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.6  1998/05/23 17:52:02  root
\ background processing
\
\ Revision 1.5  1998/05/17 08:27:09  root
\ script mode, ODOES>
\
\ Revision 1.4  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.3  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.2  1998/04/29 18:20:30  root
\ TAB key added
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

   9 CONSTANT KEY_TAB
  13 CONSTANT KEY_RET
 127 CONSTANT KEY_BACKSPACE   	\ Backspace (unreliable) 

23 CONSTANT key-list-len
CREATE key-list 
-1 ,  79 , ( key_left )
-1 ,  83 , ( key_right )
-1 ,  87 , ( key_up )
-1 ,  61 , ( key_down )
-1 ,  76 , ( key_home )
-1 , 164 , ( key_end )
-1 ,  81 , ( key_npage )
-1 ,  82 , ( key_ppage )
-1 ,  59 , ( key_dc )
-1 ,  77 , ( key_ic )
-1 ,  65 , ( key_f0 )
-1 ,  66 , ( key_f1 )
-1 ,  68 , ( key_f2 )
-1 ,  69 , ( key_f3 )
-1 ,  70 , ( key_f4 )
-1 ,  71 , ( key_f5 )
-1 ,  72 , ( key_f6 )
-1 ,  73 , ( key_f7 )
-1 ,  74 , ( key_f8 )
-1 ,  75 , ( key_f9 )
-1 ,  67 , ( key_f10 )
-1 , 216 , ( key_f11 )
-1 , 217 , ( key_f12 )

CREATE (ekey-buf) 10 ALLOT
0 VALUE (ek-cache-cnt)
0 VALUE (ek-cache-ind)

\ Reset the flags for the keys.
: (ek-reset) 				( -- )
  key-list key-list-len 0 DO
    TRUE OVER !
    2 CELLS +
  LOOP DROP ;

\ Type an zero terminated string.
: .asciiz 				( addr -- )
  DUP IF
    BEGIN 
      DUP C@ DUP
    WHILE 
      . CHAR+
    REPEAT 2DROP 
  ELSE
    ." <nul>" DROP
  THEN  
  ;

\ List the codes of all keys.
: .(keys) 
  key-list key-list-len 0 DO
    CELL+ DUP @ CELLS TERM-STRING + @ \ addr str
    .asciiz CR
    CELL+
  LOOP DROP ;

0 RVALUE backgrounder

\ See standard.
: KEY 				( -- n )
  backgrounder IMAGE-BASE <> IF
    BEGIN
      backgrounder EXECUTE
      KEY?
    UNTIL
  THEN
  (KEY) ;

\ EKEY with background tasking
: ((EKEY)) 			( -- n )
  backgrounder IMAGE-BASE <> IF
    BEGIN
      backgrounder EXECUTE
      EKEY?
    UNTIL
  THEN
  (EKEY) ;

\ Checks key-list and return number of matches. The case that only one key
\ string matches but is not ended is expressed by n==2 .
: (ek-matches) 				( index key -- index { code 1 / n } )
  0 key-list key-list-len 0 DO 		\ index key n addr
    DUP @ IF 				\ index key n addr
      DUP CELL+ @ CELLS TERM-STRING + @ \ index key n addr str
      ?DUP IF 				\ index key n addr
	TWIST 2DUP + C@ 		\ key n addr str index char
	ROTARE TUCK = IF 		\ n addr str index key
	  TWIST 1+ 			\ addr str index key n
	  2SWAP TUCK 			\ addr key n index str index
	  1+ + C@ 0= IF ( end ) 	\ addr key n index
	    NIP NIP NIP I 256 + 
	    1 UNLOOP EXIT
	  ELSE 				\ addr key n index
	    ROT  			\ addr n index key
	    2SWAP SWAP 			\ index key addr n
	  THEN
	ELSE 				\ n addr str index key
	  ROT DROP 			\ n addr index key
	  ROT FALSE OVER ! 		\ n index key addr
	  TURN SWAP 			\ index key n addr
	THEN
      THEN
    THEN
    2 CELLS +
  LOOP 					\ index key n addr
  DROP NIP 				\ index n
  DUP 1 = IF 1+ THEN
;

\ See standard.
: EKEY 					( -- n )
  (ek-cache-ind) (ek-cache-cnt) < IF
    (ek-cache-ind) DUP (ekey-buf) + C@ 	\ ind n
    SWAP 1+ TO (ek-cache-ind)
  ELSE ( cache empty ) 			\
    (ek-reset)
    0 BEGIN 				\ index
      ((ekey)) 				\ index key
      2DUP SWAP (ekey-buf) + C! 	\ index key
      (ek-matches) 			\ index ( code 1 / 0 / n )
      DUP 0= IF 			\ index 0
        DROP
	1+ TO (ek-cache-cnt)
	1 TO (ek-cache-ind)
	(ekey-buf) C@ 			\ n
	TRUE
      ELSE 
        1 = IF 
	  NIP 0 TO (ek-cache-cnt)
  	  1 TO (ek-cache-ind) TRUE
        ELSE
	  1+ FALSE
	THEN
      THEN
    UNTIL
  THEN ;

256 CONSTANT KEY_LEFT
257 CONSTANT KEY_RIGHT
258 CONSTANT KEY_UP
259 CONSTANT KEY_DOWN
260 CONSTANT KEY_HOME
261 CONSTANT KEY_END
262 CONSTANT KEY_NPAGE
263 CONSTANT KEY_PPAGE
264 CONSTANT KEY_DC
265 CONSTANT KEY_IC
266 CONSTANT KEY_F0
267 CONSTANT KEY_F1
268 CONSTANT KEY_F2
269 CONSTANT KEY_F3
270 CONSTANT KEY_F4
271 CONSTANT KEY_F5
272 CONSTANT KEY_F6
273 CONSTANT KEY_F7
274 CONSTANT KEY_F8
275 CONSTANT KEY_F9
276 CONSTANT KEY_F10
277 CONSTANT KEY_F11
278 CONSTANT KEY_F12

