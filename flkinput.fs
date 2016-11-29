\  FLK basic input words
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

\ $Id: flkinput.fs,v 1.13 1998/08/30 10:50:59 root Exp $
\ $Log: flkinput.fs,v $
\ Revision 1.13  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.12  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.11  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.10  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.9  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.8  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.7  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.6  1998/04/29 18:20:30  root
\ alarm signal (audible/visible)
\
\ Revision 1.5  1998/04/27 18:41:42  root
\ history support added
\
\ Revision 1.4  1998/04/11 11:55:44  root
\ FORGET/MARKER support added
\
\ Revision 1.3  1998/04/10 14:42:50  root
\ bugs corrected
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ all words checked and OK
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ Loud enough?
0 VALUE audible-signal

\ Ring a bell of flash the screen.
: ALERT 			( -- )
  audible-signal IF BELL ELSE FLASH THEN ;

\ List of history lists.
0 VALUE histories

\ Start with an empty history.
: init-history []( TO histories ;

\ Retrieve or create the history list with the given id. All history lists
\ between the last and this one are set to 0.
: get-history 			( id -- hlist )
  histories DUP []# 		\ id hist cnt
  BEGIN
    PLUCK OVER  		\ id hist cnt id cnt
    >=
  WHILE 			\ id hist cnt
    1+ SWAP 			\ id cnt+1 hist
    0 []+= SWAP 		\ id hist cnt
  REPEAT DROP 			\ id hist
  TUCK SWAP 			\ hist hist id
  []@ 				\ hist hlist
  ?DUP 0= IF ( no hlist yet ) 	\ hist 
    []( 			\ hist hlist
  THEN 				\ hist hlist
  SWAP TO histories ;

\ Place the history list in the top level list.
: set-history 			( hlist id -- )
  histories SWAP ROT []! TO histories ;

\ Allocate a string and append it to the list.
: $[]+= 			( addr len hlist -- hlist )
  -ROT $ALLOCOPY []+= ;

-1 VALUE history-id

0 VALUE completer

0 VALUE (ac-maxlen)
0 VALUE (ac-buf)
0 VALUE (ac-ind)
0 VALUE (ac-len)
0 VALUE (ac-hist)
0 VALUE (ac-histind)

-1 CONSTANT BACK_CHAR

\ Set the cursor to position ind.
: (ac-setind) 				( ind -- )
  (ac-ind) 				\ new old
  2DUP - 				\ new old move
  DUP 0< IF ( backwards) 		\ new old move
    NEGATE 0 DO
      BACK_CHAR EMIT
    LOOP DROP
  ELSE ( fwd ) 				\ new old move
    (ac-buf) ROT + 			\ new move addr
    SWAP 0 ?DO 				\ new addr
      DUP C@ EMIT CHAR+
    LOOP DROP
  THEN
  TO (ac-ind) ;

\ How many characters right of the cursor?
: (ac-rest) (ac-len) (ac-ind) - ;

\ Which address the cursor is over?
: (ac-bufind) (ac-buf) (ac-ind) + ;

\ Insert one character under the cursor.
: (ac-insert) 				( char -- )
  (ac-bufind) DUP CHAR+ 		\ from to
  (ac-rest) 				\ from to cnt
  CMOVE>
  TO++ (ac-len)
  (ac-ind) CHARS (ac-buf) + C! ;

\ Delete one character under the cursor.
: (ac-delete) 				( -- )
  (ac-ind) (ac-len) < IF
    (ac-bufind) DUP CHAR+ SWAP 		\ from to
    (ac-rest) CMOVE
    TO-- (ac-len) 
  THEN ;

\ Move cnt bytes from from-ind to to-ind without overflowing the buffer.
: (ac-move) 				( to-ind from-ind cnt -- )
  PLUCK OVER + 				\ to-ind from-ind cnt end-ind
  (ac-maxlen) OVER 			\ to-ind from-ind cnt end-ind max end
  < IF 					\ to-ind from-ind cnt end-ind 
    NIP
    PLUCK 				\ to-ind from-ind end-ind to-ind
    (ac-maxlen) SWAP - SWAP 		\ to-ind from-ind cnt end-ind 
  THEN DROP
  MOVE
;

\ Replace the buffer contents from len characters before (ac-ind) with the
\ given string.
: (ac-replace) 				( len addr' len' -- )
  DUP FLOCK - 				\ len addr' len' growth
  DUP (ac-ind) + 			\ len addr' len' growth to-ind
  (ac-ind) 				\ len addr' len' growth to-ind from-ind
  (ac-len) (ac-ind) - (ac-move) 	\ len addr' len' growth 
  -TURN 				\ growth len addr' len'
  (ac-ind) TURN - (ac-buf) + 		\ growth addr' len' dst
  SWAP MOVE 				\ growth
  DUP +TO (ac-ind)
  +TO (ac-len) 
;

\ One step to the left...
: (ac-moveleft) 0 (ac-ind) 1- MAX (ac-setind) ;

\ One step to the right...
: (ac-advance) 				( -- )
  TO++ (ac-ind)  
  (ac-ind) (ac-len) MAX TO (ac-len) ;

\ Pick last item in history.
: (ac-prevhist) 			( -- )
  history-id -1 <> IF
    (ac-histind) DUP 0= IF
      DROP (ac-hist) []# 1- 0 MAX
    ELSE
      1-
    THEN
    TO (ac-histind) 
  THEN ;

\ Pick next item in history.
: (ac-nexthist) 			( -- )
  history-id -1 <> IF
    (ac-histind) 1+ (ac-hist) []# OVER =
    IF 
      DROP 0
    THEN
    TO (ac-histind) 
  THEN ;

\ Copy the current history item to the buffer.
: (ac-copyhist) 			( -- )
  history-id -1 <> IF
    (ac-hist) (ac-histind) 		\ hlist ind 
    []@ $COUNT 				\ addr cnt
    (ac-maxlen) MIN 			\ addr cnt
    DUP TO (ac-len)
    DUP TO (ac-ind)
    (ac-buf) SWAP MOVE
  THEN ;

\ Hide the displayed string and leave then cursor at the beginning of the
\ line.
: (ac-prep-redisp) 			( -- )
  (ac-len) (ac-ind) -  SPACES
  (ac-len) 0 ?DO BACK_CHAR EMIT LOOP
  (ac-ind) SPACES
  (ac-ind) 0 ?DO BACK_CHAR EMIT LOOP
;

\ Redisplay the buffer and put the cursor to the right position.
: (ac-redisplay) 			( -- )
  (ac-buf) (ac-len) TYPE
  (ac-len) (ac-ind) - 0 ?DO BACK_CHAR EMIT LOOP
;

\ Call the completer word if one is set.
: (ac-complete) 			( -- )
  completer IF
    (ac-prep-redisp)
    completer EXECUTE
    (ac-redisplay)
  THEN ;

\ Show the rest of the line beginning at the current position.
: (ac-disp-rest) 			( -- )
  (ac-buf) (ac-ind) +
  (ac-len) (ac-ind) - 
  TYPE SPACE ;

\ Move the cursor backwards from the end to the current position.
: (ac-end-to-ind) 			( -- )
  (ac-len) (ac-ind) - 1+ 0 
  ?DO BACK_CHAR EMIT LOOP ;

\ See standard.
: ACCEPT  				( c-addr +n1 -- +n2 )
( OK )
  TO (ac-maxlen)
  TO (ac-buf)
  0 TO (ac-len)
  0 TO (ac-ind)
  history-id DUP -1 <> IF 
    get-history S" " ROT $[]+= DUP TO (ac-hist)
    []# 1- TO (ac-histind)
  ELSE
    DROP
  THEN
  BEGIN
    FALSE EKEY 				\ end? key
    CASE 
      KEY_RET 		OF DROP TRUE ENDOF
      KEY_HOME 		OF 0 (ac-setind) ENDOF
      KEY_END 		OF (ac-len) (ac-setind) ENDOF
      KEY_LEFT 		OF (ac-moveleft) ENDOF
      KEY_RIGHT 	OF (ac-len) (ac-ind) 1+ MIN (ac-setind) ENDOF
      KEY_DC 		OF (ac-delete) (ac-disp-rest) (ac-end-to-ind) ENDOF
      KEY_BACKSPACE 	OF 
        (ac-moveleft) (ac-delete) (ac-disp-rest)
        (ac-end-to-ind) ENDOF
      KEY_UP 		OF 
        (ac-prep-redisp) (ac-prevhist) (ac-copyhist) (ac-redisplay) 
      ENDOF
      KEY_DOWN 		OF 
        (ac-prep-redisp) (ac-nexthist) (ac-copyhist) (ac-redisplay) 
      ENDOF
      KEY_TAB 		OF (ac-complete) ENDOF
      DUP 32 256 WITHIN 		\ end? key ascii?
      IF
	(ac-len) (ac-maxlen) < IF
	  DUP (ac-insert) 
	  (ac-disp-rest)
	  (ac-advance) 
	  (ac-end-to-ind)
	THEN
      THEN
    ENDCASE 				\ end?
  UNTIL  				\  
  (ac-disp-rest) (ac-len) 		\ len
  history-id -1 <> IF 
    (ac-buf)  OVER  $ALLOCOPY 	 	\ len str
    (ac-hist) DUP []# 1- 		\ len str hlist ind
    []& 				\ len str addr
    exchange 				\ len oldstr
    FREE THROW 
    (ac-hist) history-id set-history
  THEN ;

\ See standard.
0 VALUE SOURCE-ID

\ Terminal Input Buffer
CREATE TIB #TIB-LEN CHARS ALLOT
\ File Input Buffer
CREATE FIB #TIB-LEN CHARS ALLOT
#TIB-LEN CONSTANT #TIB-LEN
0 VALUE (fib-ind-h)
0 VALUE (fib-ind-l)
0 VALUE #FIB

0 VALUE (eval-addr)
0 VALUE (eval-len)

\ See standard.
: SOURCE 				( -- addr len ) 
( OK )
  SOURCE-ID CASE 
    -1 OF (eval-addr) (eval-len) ENDOF
     0 OF TIB #TIB @ ENDOF 
     FIB #FIB ROT
  ENDCASE ;

\ See standard.
: EXPECT 				( c-addr +n -- )
( OK )
  ACCEPT SPAN ! ;

\ find word before cursor in accept buffer
: completer-word 			( -- addr len )
  (ac-buf) (ac-ind) + 0 		\ addr len
  BEGIN
    OVER CHAR- C@ whitespace? 		\ addr len space?
    PLUCK (ac-buf) = OR 		\ addr len done?
    DUP INVERT IF
      -ROT -1 /STRING ROT
    THEN
  UNTIL ;

\ Starts s1,l1 with l2 characters at s2 ?
: starting? 				( s1 l1 s2 l2 -- flag )
  ROT 2DUP 				\ s1 s2 l2 l1 l2 l1
  > IF 					\ s1 s2 l2 l1 
   ( s1 is shorter than s2 -> can't start with it )
   2DROP 2DROP FALSE
  ELSE 					\ s1 s2 l2 l1
    DROP TUCK  				\ s1 l s2 l
    CAPS @ 
    IF CAPS-COMPARE ELSE COMPARE THEN
    0=
  THEN ;

\ Return the length of the longest common string of the strings at addr1 and
\ addr2 with the maximum length len1.
: lcs 					( addr1 addr2 len1 -- len2 )
  DUP -TURN 				\ len s1 s2 len
  0 ?DO 				\ len s1 s2 
    2DUP C@ SWAP C@ 			\ len s1 s2 c2 c1
    CAPS @ IF
      >UPPER SWAP >UPPER
    THEN 
    <> IF 				\ len s1 s2
      2DROP DROP I UNLOOP EXIT
    THEN 				\ len s1 s2
    CHAR+ SWAP CHAR+ SWAP
  LOOP 					\ len s1 s2
  2DROP ;

\ Find the len2 first characters that the names of xt1 and xt2 have in common,
\ but only up to len1 chars.
: ((lcs)) 				( xt1 len1 xt2 -- xt1 len2 )
  PLUCK 				\ xt1 len1 xt2 xt1 
  >NAME CHAR+ 				\ xt1 len1 xt2 s1
  SWAP >NAME CHAR+ 			\ xt1 len1 s1 s2 
  ROT 	 				\ xt1 s1 s2 len1
  lcs 					\ xt1 len2
;

\ Search through one hash line
: ((find-lcs)) 		( addr len xt len' cnt line -- addr len xt len' cnt)
  BEGIN
    @ DUP IMAGE-BASE <>
  WHILE 			\ addr len xt len' cnt xt'
    DUP >R >NAME COUNT 		\ addr len xt len' cnt wrd #wrd / r: xt'
    2>R 			\ addr len xt len' cnt / r: xt' wrd #wrd
    TWIST TWIST 		\ xt len' cnt addr len
    2R> 			\ xt len' cnt addr len wrd #wrd / r: xt'
    2OVER starting? 			\ xt len' cnt addr len starts?
    IF 					\ xt len' cnt addr len
      ( found a word )
      -TWIST -TWIST 			\ addr len xt len' cnt
      1+ -ROT 				\ addr len cnt xt len' / r: xt'
      OVER 0= IF
        ( first word found )
	2DROP R@ 			\ addr len cnt xt 
	DUP >NAME C@ 			\ addr len cnt xt len'
      ELSE 				\ addr len cnt xt len' / r: xt'
        R@ 				\ addr len cnt xt len' xt' / r: xt'
	PLUCK >NAME COUNT TYPE SPACE 	\ addr len cnt xt len' xt'
	((lcs)) 			\ addr len cnt xt len' / r: xt'
	NIP R@ SWAP
      THEN
      ROT TWIST TWIST
    THEN 				\ xt len' cnt addr len / r: xt'
    -TWIST -TWIST R> 			\ addr len xt len' cnt xt'
  REPEAT DROP
;

\ Search through one wordlist.
: (find-lcs) 	( addr len xt len' cnt wid -- addr len xt len' cnt )
  #BUCKETS 0 DO 			\ addr len xt len' cnt line
    DUP >R 				\ addr len xt len' cnt line
    ((find-lcs)) R> 			\ addr len xt len' cnt line
    CELL+
  LOOP DROP
;

\ Search through all wordlists in search-order and look for words starting
\ with the given string. The number of words found is returned and depending
\ on it the xt of this word or the longest common string of these words.
: find-lcs 				( addr len -- xt 1 / 0 / addr' len' n )
  0 0 0  				\ addr len xt len' cnt
  ((unique-cnt)) @ 			\ addr len xt len' cnt vocs
  0 ?DO 				\ addr len xt len' cnt
    I CELLS ((unique-order)) + @ 	\ addr len xt len' cnt wid
    (find-lcs)
  LOOP 					\ addr len xt len' cnt
  TWIST TWIST 2DROP 			\ xt len' cnt
  DUP CASE 				\ xt len' cnt cnt
    0 OF NIP NIP ENDOF
    1 OF NIP ENDOF
    TURN 				\ len' cnt cnt xt
    >NAME COUNT 			\ len' cnt cnt addr len
    OVER SWAP TYPE SPACE -TURN 		\ addr' len' cnt cnt
  ENDCASE
;

\ Find the longest common string of all words in search order beginning at the
\ first space left of the cursor.
: query-completer 			( -- )
  completer-word 			\ addr len
  TUCK 					\ len addr len
  DUP 0= IF 
    2DROP DROP
  ELSE
    find-lcs 				\ len ( xt 1 / 0 / addr' len' n )
    CASE
      0 OF ALERT DROP ENDOF
      1 OF >NAME COUNT 			\ len addr' len' 
           (ac-replace)
	   BL (ac-insert)
	   (ac-advance)
	ENDOF
      -TURN  				\ n len addr' len' 
      CR
      (ac-replace) 			\ n
      ALERT ALERT
    ENDCASE
  THEN ;

\ See standard.
: QUERY 				( -- )
( OK )
  0 TO SOURCE-ID
  history-id 				\ hid
  completer 				\ hid compl
  0 TO history-id
  ['] query-completer TO completer
  TIB #TIB-LEN ACCEPT
  #TIB !
  TO completer
  TO history-id
  0 >IN ! ;

0 VALUE (refill-line)
0 VALUE (error-line)

\ See standard.
: REFILL 				( -- flag )
( OK )
  SOURCE-ID CASE
    -1 OF FALSE ENDOF
     0 OF QUERY CR TRUE ENDOF
     DUP FILE-POSITION THROW 		\ fid fpl fph
     TO (fib-ind-h) TO (fib-ind-l)
     FIB #TIB-LEN 			\ fid buf len
     PLUCK READ-LINE THROW 		\ fid len !eof?
     SWAP TO #FIB 			\ fid !eof?
     SWAP
     TO++ (refill-line)
  ENDCASE 
  DUP IF
    0 >IN !
  THEN ;

0 VALUE (last-parse-from)
0 VALUE (last-parse-len)
0 VALUE (error-parse-from)
0 VALUE (error-parse-len)

\ See standard.
: SAVE-INPUT 					( -- ... n )
( OK )
  >IN @ 					\ >in
  SOURCE-ID CASE
    -1 OF (eval-addr) (eval-len) 4 ENDOF 	\ >in eval-addr eval-len 4
     0 OF 2 ENDOF 				\ >in 2
     (fib-ind-l) (fib-ind-h) (refill-line)  	\ >in sid fibl fibh line 
     TURN 					\ >in fibl fibh line sid
     (last-parse-len) (last-parse-from) 7 	\ >in fibl fibh line sid lpl lpf 7
     TURN
  ENDCASE
  SOURCE-ID SWAP 
  ;

\ See standard.
: RESTORE-INPUT 				( .. n -- flag )
( OK )
  DROP 						\ ... sid
  (refill-line) TO (error-line)
  (last-parse-from) TO (error-parse-from)
  (last-parse-len) TO (error-parse-len)
  DUP TO SOURCE-ID 				\ ... sid
  CASE
    -1 OF TO (eval-len) TO (eval-addr) 
          FALSE ENDOF 				\ >in flag
     0 OF FALSE ENDOF 				\ >in flag
     						\ >in fibl fibh line lpl lpf fid 
     SWAP TO (last-parse-from)
     SWAP TO (last-parse-len)
     SWAP TO (refill-line)
     -ROT 					\ >in fid fibl fibh
     TO (fib-ind-h) TO (fib-ind-l) 		\ >in fid
     (fib-ind-l) (fib-ind-h) PLUCK 		\ >in fid fibl fibh fid
     REPOSITION-FILE THROW 
     TO-- (refill-line)
     REFILL INVERT SWAP 			\ >in flag fid
  ENDCASE SWAP >IN ! ;

