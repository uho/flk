\  FLK high level definitions
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

\ $Id: flkhigh.fs,v 1.26 1998/09/17 13:12:23 root Exp $
\ $Log: flkhigh.fs,v $
\ Revision 1.26  1998/09/17 13:12:23  root
\ fixed ) to do multiline comments
\
\ Revision 1.25  1998/09/13 21:28:56  root
\ fixed cf stackk bugs
\
\ Revision 1.24  1998/09/13 16:19:39  root
\ moved to version 1.3
\
\ Revision 1.23  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.22  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.21  1998/07/18 10:49:59  root
\ bug corrected
\
\ Revision 1.20  1998/07/08 19:54:35  root
\ -e switch added
\
\ Revision 1.19  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.18  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.17  1998/06/01 18:05:39  root
\ changed to version 1.1
\
\ Revision 1.16  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.15  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.14  1998/05/21 19:24:49  root
\ XForms support
\
\ Revision 1.13  1998/05/17 08:27:09  root
\ script mode, ODOES>
\
\ Revision 1.12  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.11  1998/05/09 21:47:05  root
\ moved some words to flktools.fs
\
\ Revision 1.10  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.9  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.8  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.7  1998/04/29 18:20:30  root
\ initial search order: FORTH FORTH
\
\ Revision 1.6  1998/04/27 18:41:42  root
\ history support added
\
\ Revision 1.5  1998/04/24 16:47:39  root
\ bug fixes
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

\ Transform a signed number with or without dot into a double/single cell
\ number and notice the type in the flags.
\ Syntax:  -?d*\.?
: (parse-nr) 				( addr len -- d TRUE FALSE / n FALSE FALSE / TRUE )
( OK )
  DUP 0= IF 2DROP TRUE EXIT THEN 	\ addr len
  OVER C@ 45 ( [CHAR] - ) = 		\ addr len neg?
  DUP IF -ROT 1 /STRING ROT THEN 	\ addr len neg? 
  -ROT 0 0 2SWAP 			\ neg? xd addr len
  >NUMBER 				\ neg? xd addr len
  DUP 0= 				\ neg? xd addr len n,ok?
  IF
    2DROP DROP SWAP IF NEGATE THEN 	\ n
    FALSE FALSE EXIT
  THEN 					\ neg? xd addr len
  1 = SWAP C@ 46 ( [CHAR] . ) = AND 	\ neg? xd d,ok?
  IF
    ROT IF DNEGATE THEN TRUE FALSE EXIT
  THEN 					\ neg? xd
  2DROP DROP TRUE
;

\ Print the parse area.
: show-pa
( OK )
  ." area:"  SOURCE >IN @ /STRING q-TYPE CR ;

\ Complain which word is unknown and throw an exception. This word ends the
\ number conversion chain.
: (unknown-word) 			( addr len -- )
  CR ." Undefined word " q-TYPE SPACE
  -13 THROW ;

' (unknown-word) RVALUE other-numbers

\ Factor of INTERPRET. Handles the string when it is assumed to be a number.
: (interpret-number) 			( addr len -- )
( OK )
  2DUP (parse-nr) 			\ addr len d/n double? error?
  IF 					\ addr len 
    other-numbers EXECUTE
  ELSE   				\ addr len d/n double?
    STATE @ IF ( compiling) 		\ addr len d/n double?
      IF 				\ addr len d
        2SWAP 2DROP
        2LITERAL
      ELSE 				\ addr len n
        -ROT 2DROP
        LITERAL
      THEN 				\ addr len
    ELSE ( interpreting) 		\ addr len d/n double?
      IF 				\ addr len d
        2SWAP
      ELSE 				\ addr len n
        -ROT
      THEN
      2DROP
    THEN
  THEN ;
      
\ Another factor of INTERPRET. Handles a recognized word.
\ Cond. 				action
\ --------------------------------------------
\ immed, comm, nomac 			execute
\ immed, opt, 				flush, execute
\ immed, nomac 				flush, execute
\ immed, 				cache
\ nonimmed, opt,			compile
\ nonimmed, 				cache
: (interpret-xt) 			( xt flag -- )
  STATE @ 0= IF ( interpretation )
    DROP EXECUTE 
  ELSE ( compilation ) 			\ xt flag
    1 = 				\ xt immed?
    IF ( immed ) 			\ xt
      EXECUTE
    ELSE ( nonimmed ) 			\ xt
      COMPILE,
    THEN
  THEN ;

\ Interpret all the way through SOURCE.
: INTERPRET 				( -- )
( OK )
  BEGIN
    skip-space
    BL PARSE
\ ." found " 2DUP TYPE .S
    DUP
  WHILE
    2DUP 				\ addr len addr len
    SEARCH-WORDLISTS  			\ addr len ( 0 / xt flag )
    DUP 0= IF ( number? )
      DROP (interpret-number) 		\ 
    ELSE 				\ addr len xt flag
      2SWAP 2DROP
      (interpret-xt)
    THEN
  REPEAT 2DROP ;

0 VALUE include-log

\ Read one line and interpret. Factor of INCLUDE-FILE.
: (include-file) 			( -- )
( OK )
  BEGIN
    REFILL
    include-log IF
      ." line: " SOURCE q-TYPE .S CR .cs
    THEN
  WHILE
    0 >IN !
    INTERPRET
  REPEAT 
  ;

\ See standard.
: INCLUDE-FILE 				( fid -- )
  SAVE-INPUT n>R 			\ fid
  TO SOURCE-ID
  0 TO (refill-line)
  ['] (include-file) CATCH
  SOURCE-ID CLOSE-FILE THROW 
  nR> RESTORE-INPUT IF -37 THROW THEN
  THROW 
  ;

0 VALUE error-fn
0 VALUE error-line
0 VALUE error-col
0 VALUE error-len

\ Tell the user which file included which and store the first one.
: (.included) 				( str -- )
  BASE @ SWAP DECIMAL
  ." in file " DUP $COUNT q-TYPE 
  ."  line " (error-line) . 
  ." column " (error-parse-from) 1+ .
  SWAP BASE !
  CR 					\ str
  error-fn 0= IF
    TO error-fn
    (error-line) TO error-line
    (error-parse-from) TO error-col
    (error-parse-len) TO error-len
  ELSE
    FREE THROW
  THEN ; 

\ Stores the given string into the dictionary (cell count).
: ,S 					( ca n -- )
  DUP , 0 ?DO 				\ ca
    DUP C@ C, CHAR+ 
  LOOP DROP ;
  
\ Translate a C string into a FORTH string.
: strlen 					( addr -- addr len )
( OK )
  DUP  						\ addr addr
  0 BEGIN 					\ addr addr len
    OVER C@ 0<>
  WHILE
    1+ SWAP CHAR+ SWAP
  REPEAT NIP ;

CREATE (cur-fn) 0 , 1024 ALLOT
\ See standard.
: INCLUDED 				( i*x c-addr u -- j*x )
( OK )
  DUP IF
    OVER C@ 47 ( [CHAR] / ) <> IF
      GET-CWD strlen (cur-fn) $copy
      2DUP (cur-fn) $cat
    ELSE
      2DUP (cur-fn) $copy
    THEN
    HERE filename-list DUP r, 		\ addr len fn old-fn
    SWAP DUP TO filename-list 		\ addr len old-fn
    TO cur-fn
    >R
    (cur-fn) $COUNT ,S 
    2DUP $ALLOCOPY -ROT  			\ str caddr u 
    R/O OPEN-FILE 			\ str fid ior
    ?DUP IF 				\ str fid ior
      ROT FREE THROW 			\ fid ior
      NIP THROW 
    THEN 					\ str fid
    SWAP >R 				\ fid / r: str
    ['] INCLUDE-FILE CATCH 		\ ex
    R> SWAP 				\ str ex
    ?DUP IF 				\ str ex
      SWAP (.included) THROW
    ELSE 					\ str
      FREE THROW
    THEN
    R> 					\ old-fn
    TO cur-fn
  ELSE
    -38 THROW
  THEN
  ;

\ Support for user declared exceptions. The exception text are stored in a
\ single linked list. Each node has the following structure.
\ Offset	Meaning
\ 0		next
\ 1 cell 	exxception nr
\ 2 cells 	cell counted string 
0 RVALUE (exception-list)

\ Search the exception list for the given number. If it is found, print the
\ text. The returned flag is true when no code was found.
: .exception 				( nr -- unknown? )
  (exception-list) 			\ nr item
  BEGIN
    IMAGE-BASE OVER <>
  WHILE 				\ nr item
    DUP CELL+ @  			\ nr item item-nr
    PLUCK = IF 				\ nr item
      2 CELLS + $COUNT 			\ nr addr len
      TYPE CR
      DROP FALSE
      EXIT
    THEN 				\ nr item
    @
  REPEAT 				\ nr item
  2DROP TRUE ; 

\ Default exception handler. Tells the user and reset the stacks.
: QUIT-CATCHER 				( code -- )  
( OK )
  DUP IF
    ." Exception: "
    FALSE STATE !
    FALSE TO (coding)
    >R
    SP-BASE SP!
    R>
  THEN
  CASE
    0 OF ENDOF
   -1 OF ." Aborted" CR ENDOF
   -2 OF abort"-addr @ abort"-len @ TYPE CR ENDOF
  ( default ) 				\ ex
     DUP .exception 			\ ex not-found?
     IF DUP ." # " . CR THEN
  ENDCASE ;

\ See standard.
: QUIT 
( OK )
  RP-BASE RP!
  0 TO SOURCE-ID
  FALSE STATE !
  BEGIN
    REFILL
  WHILE
    0 >IN !
    ['] INTERPRET CATCH
    DUP 0= IF 
      STATE @ 0= IF 
        ."   OK " CR 
	DEPTH 0 <= IF DROP -4 THEN
	DEPTH HA-DATA-CELLS @ >= IF DROP -3 THEN
      ELSE 
        SOURCE-ID 0= IF ." ? " THEN  
      THEN 
    THEN
    QUIT-CATCHER
    error-fn ?DUP IF FREE THROW 0 TO error-fn THEN
  REPEAT 
  BYE ;

0 VALUE ARGC
0 VALUE ARGV

CREATE (console-fn) 0 , S" ..console.." ,S

\ Initialize all relevant variables and store the commandline in a save place.
\ Should be called by a user defined startup word before anything else. If
\ not, you can run into deep trouble.
: (setup-system) 			( argc argv -- )
( OK )
  TO ARGV
  TO ARGC
  10 BASE ! 
  CAPS ON
  ONLY ALSO DEFINITIONS
  HERE-INIT TO HERE
  CHERE-INIT TO CHERE
  init-history
  (asm-reset)
  0 TO (ek-cache-cnt)
  0 TO (ek-cache-ind)
  (console-fn) TO cur-fn
  -1 cf-sp !
  initializer EXECUTE
;

\ Display the GNU required text.
: (c)  					( -- )
  ." FLK version 1.3, Copyright (C) 1998 Lars Krueger" CR 
  ." FLK comes with ABSOLUTELY NO WARRANTY; for details type WARRANTY ." CR
  ." This is free software, and you are welcome to redistribute it" CR
  ." under certain conditions; type LICENSE for details." CR
  ;

\ Display some basic data about FLK.
: GREETING 					( -- )
( OK )
  (c)
  UNUSED . ." bytes free in data area" CR
  CUNUSED . ." bytes free in code area" CR 
  ." Image loaded at address " IMAGE-BASE .addr CR CR
  ;

\ See standard.
: EVALUATE 				( i*x c-addr u -- j*x )
( OK )
  SAVE-INPUT n>R
  -1 TO SOURCE-ID
  TO (eval-len)
  TO (eval-addr)
  0 >IN !
  ['] INTERPRET CATCH 			\ exc
  nR> RESTORE-INPUT IF ." Could not restore input after EVALUATE." ABORT THEN
  THROW ;

\ Try to open the files given in the commandline as source files.
: load-cmdline 					( -- )
( OK )
  ARGC 2 >= IF
    ARGV CELL+ @ strlen S" -q" COMPARE  
    IF 1 GREETING ELSE 2 THEN
  ELSE
    1 GREETING
  THEN 						\ init-argc
  ARGC BEGIN 					\ ind end
    2DUP < 
  WHILE
    OVER CELLS ARGV + @ strlen 			\ ind end addr len
    2DUP S" -e" COMPARE 0= 			\ ind end addr len -e?
    IF 						\ ind end addr len
      2DROP
      SWAP 1+ TUCK 				\ ind end ind
      CELLS ARGV + @ strlen 			\ ind end addr len
      \ Move ind and end to return stack because the words executed could
      \ change the stack.
      2SWAP 2>R 
      EVALUATE
      2R> 					\ ind end
    ELSE
      \ Move ind and end to return stack because the files included could
      \ change the stack.
      2SWAP 2>R 
      INCLUDED
      2R>
    THEN 					\ ind end
    SWAP 1+ SWAP
  REPEAT 2DROP
;     

\ Where is that Any-key here on that keyboard? Found it? Program continues.
: ANY-KEY CR ." Press any key to continue..." SPACE EKEY DROP ;

0 VALUE (lcmd-ex)

\ Gentlemen, start your engines... (This is the entry word.)
: WELCOME  					( argc argv -- )
( OK )
  (setup-system)

  0 TO SOURCE-ID
  FALSE STATE !
  0 TO error-fn
  ['] load-cmdline CATCH DUP TO (lcmd-ex) QUIT-CATCHER
  (lcmd-ex) IF ANY-KEY BYE THEN
  error-fn ?DUP IF FREE THROW 0 TO error-fn THEN
  QUIT ;

\ Comments. No comments at this time. Otherwise see standard.
: file-paren 				( -- )
( OK )
  BEGIN
    41 PARSE  				\ addr len 
    DUP IF
      + C@ 41 = 			\ found
    ELSE
      2DROP FALSE
    THEN 				\ found?
    INVERT 
  WHILE
    REFILL 0= -39 ?THROW
  REPEAT ;

: line-paren 				( -- )
  41 PARSE 2DROP ;

: ( 					( -- )
  SOURCE-ID CASE
   -1 OF line-paren ENDOF
    0 OF line-paren ENDOF
    file-paren
  ENDCASE
; IMMEDIATE

\ This version of \ does not work with blocks since it discards the whole
\ parse area. See standard what it does.
: \
( OK )
  10 PARSE 2DROP ; IMMEDIATE

\ Comment to enable the use of FLK in scripts.
: #! 10 PARSE 2DROP ; IMMEDIATE

\ See standard.
: [ (opt-flush) FALSE STATE ! ; IMMEDIATE
( OK )

\ See standard.
: ] TRUE  STATE ! ;
( OK )

: (.opt2tree) 				( ind node-var -- )
  BEGIN 				\ ind v
    @ DUP IMAGE-BASE <> 		\ ind node cont?
  WHILE 				\ ind node
    OVER SPACES
    DUP CELL+ @ 			\ ind node xt
    DUP IMAGE-BASE = IF  		\ ind node xt
      ." -- number -- " 
      DROP
    ELSE 				\ ind node xt
      >NAME COUNT
      TYPE
    THEN  				\ ind node
    DUP 2 CELLS + @ 			\ ind node opt?
    IMAGE-BASE <> IF ."  ***" THEN CR
    2DUP 3 CELLS + 			\ ind node ind son-var
    SWAP 2 + SWAP RECURSE 		\ ind node
  REPEAT 2DROP
;

: .opt2tree  				( -- )
  ." ##################### tree of optimizers ########################" CR
  0 opt2tree (.opt2tree)
  ." #################################################################" CR
;

