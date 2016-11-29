\  FLK Tools
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

\ $Id: flktools.fs,v 1.23 1998/09/17 13:12:23 root Exp $
\ $Log: flktools.fs,v $
\ Revision 1.23  1998/09/17 13:12:23  root
\ fixed in-chain to use non-immediate COMPILE,
\
\ Revision 1.22  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.21  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.20  1998/07/09 19:16:41  root
\ fixed bug in save code which disabled correct relocation after
\ change of code size
\
\ Revision 1.19  1998/07/08 19:54:35  root
\ moved EVALUATE to flkhigh.fs#
\
\ Revision 1.18  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.17  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.16  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.15  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.14  1998/06/05 20:02:10  root
\ forget corrected (chains)
\
\ Revision 1.13  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.12  1998/05/27 18:52:12  root
\
\ Revision 1.11  1998/05/23 17:52:02  root
\ library support
\
\ Revision 1.10  1998/05/21 19:24:49  root
\ XForms support
\
\ Revision 1.9  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.8  1998/05/09 21:47:05  root
\ words from flkkern.fs added
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
\ Revision 1.4  1998/04/29 18:26:32  root
\ editor, filename input
\
\ Revision 1.3  1998/04/27 18:41:42  root
\ redefinition corrected
\
\ Revision 1.2  1998/04/24 20:25:00  root
\ 0x... and '.' constants added
\
\ Revision 1.1  1998/04/24 16:47:39  root
\ Initial revision
\

\ Checkpoints are debugging tools. They tell the user their name and dump the
\ stack. So errorscan be found by placing checkpoints around the suspicious
\ words.

\ See standard.
: D= D- D0= ;
( OK )

\ See standard.
: D< D- D0< ;
( OK )

\ Short-cut.
: D> 2SWAP D< ;
( OK )

\ Short-cut.
: OCT 8 BASE ! ;
( OK )

\ Short-cut.
: BINARY 2 BASE ! ;
( OK )

\ See standard.
: DMAX 					( d1 d2 -- d3 )
( OK )
  2OVER 2OVER D> IF 2DROP ELSE 2SWAP 2DROP THEN ;
  
\ See standard.
: DMIN 
( OK )
  2OVER 2OVER D< IF 2DROP ELSE 2SWAP 2DROP THEN ;

\ See standard.
: [ELSE] 				( -- )
( OK )
  1 BEGIN                              	\ level
    BEGIN  
      skip-space BL PARSE DUP  
    WHILE 				\ level adr len
      2DUP  S" [IF]"  
      (swl-COMPARE) 0= IF    		\ level adr len
	2DROP 1+                       	\ level'
      ELSE                             	\ level adr len
	2DUP  S" [ELSE]"  
	(swl-COMPARE) 0= IF 		\ level adr len
	   2DROP 1- DUP IF 1+ THEN     	\ level'
	ELSE                           	\ level adr len
	  S" [THEN]"  
	  (swl-COMPARE) 0= IF    	\ level
	    1-                         	\ level'
	  THEN
	THEN
      THEN ?DUP 0=  IF EXIT THEN       	\ level'
    REPEAT  2DROP                      	\ level
  REFILL 0= UNTIL                      	\ level
  DROP ; IMMEDIATE 

\ See standard.
: [IF]  				( flag -- )
( OK )
  0= IF POSTPONE [ELSE] THEN
; IMMEDIATE 

\ See standard.
: [THEN]  				( -- )  
( OK )
; IMMEDIATE 

\ See standard.
: DUMP 					( addr u -- )
( OK )
  ." +----------+-------------------------------------------------+" CR
  ." | address  | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F |" CR
  ." +----------+-------------------------------------------------+" CR
  15 + 16 / 				\ addr u-lines
  BASE @ >R HEX
  0 DO
    ." | " DUP 8 U.0 ."  | " 		\ addr
    16 0 DO
      DUP C@ 2 U.0 SPACE CHAR+
    LOOP
    ." |" CR
  LOOP DROP 
  R> BASE ! ;

\ See standard.
: ? @ . ;
( OK )

\ See standard.
: .( 41 PARSE TYPE ;
( OK )

\ Display the short version of the GNU license.
: LICENSE 				( -- )
  ." This program is free software; you can redistribute it and/or modify" CR
  ." it under the terms of the GNU General Public License as published by" CR
  ." the Free Software Foundation; either version 2 of the License, or" CR
  ." (at your option) any later version." CR CR
  ." You should have received a copy of the GNU General Public License" CR
  ." along with this program; if not, write to the Free Software" CR
  ." Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA." CR ;

\ Display the warranty text.
: WARRANTY 				( -- )
  ." This program is distributed in the hope that it will be useful," CR
  ." but WITHOUT ANY WARRANTY; without even the implied warranty of" CR
  ." MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" CR
  ." GNU General Public License for more details." CR CR
  ." You should have received a copy of the GNU General Public License" CR
  ." along with this program; if not, write to the Free Software" CR
  ." Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA." CR ;


\ Write the name of the word and advance the counters.
: words-disp-word 				( l c xt -- l c xt )
( OK )
  DUP >NAME COUNT 				\ l c xt na nl
  FLOCK OVER 					\ l c xt na nl c nl
  + 2 + COLS >= IF 				\ l c xt na nl
    CR
    TURN DROP 0 -TURN
    TWIST 1+ -TWIST
  THEN
  TUCK TYPE ."   " 				\ l c xt nl
  ROT + 2 + SWAP 				\ l c xt 
  ;

\ Run through one hash line and display the words in it.
: words-disp-words 				( lines cols wid -- lines cols )
( OK )
  BEGIN
    @ DUP IMAGE-BASE <> 			\ lines cols xt running?
  WHILE 					\ lines cols xt
    words-disp-word
  REPEAT DROP ;

\ See standard.
: WORDS 					( -- )
( OK )
  TOP-VOC 0 0 					\ wid lines col
  #BUCKETS 0 DO 				\ wid lines col
    PLUCK 					\ wid lines col wid
    words-disp-words 				\ wid lines col
    ROT CELL+ -ROT 
  LOOP 2DROP DROP ;

\ See standard.
: ENVIRONMENT?  				( c-addr u -- false | i*x true )
( OK )
  ENVIRONMENT-WORDLIST 				\ caddr u wid
  SEARCH-WORDLIST 				\ xt flag
  IF EXECUTE TRUE
  ELSE FALSE THEN ;

\ Convert an inlined hex number. Nessesary for the assembler to be
\ ANS-compatible.
: $$ 						( -<number>- )
( OK )
  0 0
  BL PARSE 					\ wrd nwrd
  BASE @ >R
  HEX
  >NUMBER 2DROP
  R> BASE !
  DROP
  STATE @ IF POSTPONE LITERAL THEN
  ; IMMEDIATE 

\ Store TRUE in the data field of a CREATED word or VALUE.
: TURN-ON 					( -<name>- )
  ' >BODY 					\ dfa
  STATE @ IF
    >R TRUE (opt-add-const)
    R> (opt-add-r-const)
    POSTPONE !
  ELSE
    TRUE SWAP !
  THEN ; IMMEDIATE 

\ Store FALSE in the data field of a CREATED word or VALUE.
: TURN-OFF 					( -<name>- )
  ' >BODY 					\ dfa
  STATE @ IF
    >R FALSE (opt-add-const)
    R> (opt-add-r-const)
    POSTPONE !
  ELSE
    FALSE SWAP !
  THEN ; IMMEDIATE 

\ If true, checkpoints will be compiled.
FALSE VALUE checkpoints

\ Display the name of the checkpoint and the stack contents.
: (checkpoint) 				( addr len -- )
  ." Checkpoint " TYPE ."  " .S CR ;

\ Interpretetive checkpoint
: cp( 					( -<text<bracket>>- )
  [CHAR] ) PARSE 
  checkpoints IF
    (checkpoint) 
  ELSE 
    2DROP 
  THEN ; IMMEDIATE

\ Compiled checkpoint.
: cp" 					( -<text<quote>>- )
  checkpoints IF
    POSTPONE S" POSTPONE (checkpoint) 
  ELSE
    [CHAR] " PARSE 2DROP 
  THEN ; IMMEDIATE

\ Create a named wordlist. It puts itself into first position of the search
\ order when executed.
: VOCABULARY 				( -<name>- )
  >IN @ 				\ in
  skip-space
  BL PARSE 				\ in addr len
  ROT >IN !
  CREATE 
  HERE
  #BUCKETS 0 DO
    IMAGE-BASE r,
  LOOP 					\ addr len
  voc-link r,
  TO voc-link
  DUP C, 0 ?DO 				\ addr
    DUP C@ C,
    CHAR+
  LOOP DROP 
DOES> 					\ wid
  >R GET-ORDER NIP R> SWAP SET-ORDER ;

\ Create or open the file with the given name.
: get-image-file 			( addr len -- fid )
  2DUP R/W OPEN-FILE 			\ addr len fid ior
  0<> IF ( no file ) 			\ addr len fid 
    DROP
    R/W CREATE-FILE THROW 		\ fid
  ELSE ( file exists ) 			\ addr len fid
    NIP NIP
  THEN ;

\ Return the ID-String to find the beginning of an image. 
: IMAGE-HEADER-ID S" FLK Image 1.0" ;

\ Return the length of the ID-String.
: IMAGE-HEADER-ID-LEN IMAGE-HEADER-ID NIP ;

CREATE IMAGE-HEADER-BUF IMAGE-HEADER-ID-LEN CHARS ALLOT

\ Compare the read values with the original id.
: id-found? 			( -- flag )
  IMAGE-HEADER-ID 
  IMAGE-HEADER-BUF SWAP 	\ addr1 addr2 cnt
  0 DO 				\ id buf
    2DUP C@ SWAP C@ 128 OR <> 
    IF
      2DROP UNLOOP FALSE EXIT
    THEN
    CHAR+ SWAP CHAR+ SWAP
  LOOP 
  2DROP TRUE ;

\ Search through the file and leave the filepointer on the first byte of the
\ id.
: find-flk-header 		( fid -- )
  DUP FILE-SIZE THROW 		\ fid fsd
  ROT >R 			\ fsd
  0. BEGIN 			\ fsd find
    2DUP R@ REPOSITION-FILE
    THROW 			\ fsd find
    IMAGE-HEADER-BUF 
    IMAGE-HEADER-ID-LEN 		\ fsd find addr cnt
    R@ READ-FILE THROW 		\ fsd find read
    IMAGE-HEADER-ID-LEN <> IF 	\ fsd find
      2DROP 2DROP R> DROP EXIT
    THEN
    id-found? IF
      R> REPOSITION-FILE THROW 	\ fsd
      2DROP EXIT
    THEN
    1. D+
  AGAIN ;

VARIABLE cell-buf
\ Write the given value to the file (binary).
: write-cell 			( fid x -- )
  cell-buf !
  cell-buf 1 CELLS ROT 		\ addr x fid
  WRITE-FILE THROW ;

\ Write the header id.
: overwrite-header 		( fid -- )
  IMAGE-HEADER-ID IMAGE-HEADER-BUF SWAP 
  MOVE
  IMAGE-HEADER-BUF 
  IMAGE-HEADER-ID-LEN 0 DO
    DUP C@ 128 OR OVER C!
    CHAR+
  LOOP DROP
  IMAGE-HEADER-BUF 
  IMAGE-HEADER-ID-LEN ROT 	\ addr cnt fid
  WRITE-FILE THROW ;

\ Number of bytes in code area.
: tc-here CHERE IMAGE-BASE - ;

\ Number of bytes in data area.
: td-here HERE IMAGE-BASE - HA-CODESIZE @ - ;

HA-CODESIZE @ VALUE new-codesize
HA-DATASIZE @ VALUE new-datasize

\ Overwrite the relocation table.
: overwrite-relotable 			( fid -- )
  RELOCATION-TABLE@ 			\ fid relotab
  2DUP []# write-cell 			\ fid rt
  TUCK 0 []& 				\ rt fid addr
  ROT []# CELLS ROT 			\ addr cnt fid
  WRITE-FILE THROW ;

\ Overwrite the image itself.
: overwrite-image 			( fid -- )
  DUP new-codesize write-cell
  DUP new-datasize write-cell
  DUP tc-here write-cell
  DUP td-here write-cell 		\ fid
  DUP IMAGE-BASE write-cell
  DUP IMAGE-BASE tc-here ROT 		\ fid addr cnt fid
  WRITE-FILE THROW
  IMAGE-BASE HA-CODESIZE @ + td-here 
  ROT 					\ addr cnt fid
  WRITE-FILE THROW ;

0 RVALUE fence

\ Save the system to the file given by its name.
: SAVE-SYSTEM 			( addr len -- )
  ." Saving to file " 2DUP q-TYPE CR
  CHERE TO fence
  get-image-file
  DUP find-flk-header
  DUP overwrite-header
  DUP overwrite-relotable
  DUP overwrite-image 
  CLOSE-FILE THROW ;

\ Same as SAVE-SYSTEM, but sets a new entry word too.
: TURNKEY 				( fn-addr fn-len -<name>- )
  ' >CFA @ 				\ fn-addr fn-len cfa
  DUP IMAGE-BASE = 
  ABORT" Word without interpretation semantics."
  HA-ENTRY !
  SAVE-SYSTEM ;

1024 $variable (cmd)
\ Copy the current executable to a new name. Nessesary because you can't write
\ to a running file.
: copy-executable 			( name #name -- )
  s" cp " (cmd) $copy
  ARGV @ strlen (cmd) $cat
  s"  " (cmd) $cat
  (cmd) $cat
  (cmd) $COUNT SYSTEM DROP
;

\ Set code size for next SAVE-SYSTEM.
: set-codesize 				( n -- )
  DUP tc-here < ABORT" Can't set codesize this small."
  CELL+ ALIGNED 			\ n
  TO new-codesize ;

\ Set data size for next SAVE-SYSTEM.
: set-datasize 				( n -- )
  DUP td-here < ABORT" Can't set datasize this small."
  CELL+ ALIGNED 			\ n
  TO new-datasize ;

TRUE TO checkpoints

CREATE ($cr) 10 c,

\ Record a macro ended by a empty line.
: macro 				( -<name>- )
  CREATE IMMEDIATE
  S" " $ALLOCOPY 			\ str
  BEGIN
    SOURCE >IN @ /STRING 		\ str line #line
    -TRAILING				\ str line #line
    DUP 0= IF
      2DROP TRUE
    ELSE				\ str line #line
      $resize-cat
      ($cr) 1 $resize-cat
      REFILL INVERT
    THEN
  UNTIL 				\ str
  DUP $COUNT ,S
  FREE THROW
  DOES> $COUNT EVALUATE ;

\ Run through one hash line and find the word below xt. Since the lines are
\ finished by IMAGE-BASE (which is smaller than all words) no explicit check
\ for end of list is nessesary.
: (forget-word-below-xt) 		( xt wid -- wrd )
  BEGIN 				\ xt wid
    @ 					\ xt wrd
    2DUP >
  UNTIL NIP ;

\ Run through wid and remove all links to words above and equal xt.
: (forget-words-in-voc) 		( xt wid -- )
  #BUCKETS 0 DO
    2DUP (forget-word-below-xt) 	\ xt wid wrd
    OVER !
    CELL+
  LOOP 2DROP ;

\ Remove all wls above or equal to addr.
: (forget-vocs) 			( addr -- )
  voc-link BEGIN 			\ addr wid
    2DUP <=
  WHILE
    #BUCKETS CELLS + @ 			\ addr nwid
  REPEAT NIP TO voc-link ;

\ Remove all filenames above or equal to dfa.
: forget-fnlist 			( dfa -- )
  filename-list BEGIN 			\ dfa fn
    2DUP 				\ dfa fn dfa fn
    >=
  WHILE
    @
  REPEAT 				\ dfa fn
  TO filename-list DROP ;

' NOOP RVALUE forgetter

\ Walk down a chain, find that word just below xt-f and set addr to it.
: forget-chain 				( xt-f addr -- )
  TUCK BEGIN 				\ addr xt-f addr 
    @ 2DUP <
  UNTIL 				\ addr xt-f xt-w
  NIP >DFA @ SWAP ! ;

\ Get the dfa of a value.
: #? 					( -<name>- dfa )
  ' >BODY STATE @ IF 
    POSTPONE RLITERAL 
  THEN ; IMMEDIATE 
( OK )

\ Removes all words above and equal to xt by walking down all vocs and
\ checking those below xt. voc-link is set to the first wl below xt.
: (forget) 				( xt -- )
  fence OVER 				\ xt fence xt
  > IF -15 THROW THEN
  forgetter EXECUTE 
  DUP >DFA @ 
  DUP forget-fnlist
  DUP #? other-numbers forget-chain
  DUP #? initializer forget-chain
  DUP #? finisher forget-chain
  DUP #? forgetter forget-chain
  (forget-vocs)	 			\ xt
  voc-link BEGIN  			\ xt wid
    2DUP (forget-words-in-voc)
    #BUCKETS CELLS + @
    DUP IMAGE-BASE =
  UNTIL DROP 				\ xt 
  DUP >DFA @ TO HERE TO CHERE ;

\ See standard.
: FORGET 				( -<name>- )
  ' (forget) ;

\ See standard.
: MARKER 				( -<name>- )
  CHERE GET-CURRENT 
  CREATE r, r,
  GET-ORDER DUP , 0 ?DO r, LOOP 
  DOES> 				\ addr
  0 TO lastheader
  DUP @ SET-CURRENT
  CELL+ DUP @ DUP TO CHERE 		\ addr forgetaddr
  SWAP CELL+ DUP @ 			\ forgetaddr addr #order
  DUP 0 ?DO 				\ forgetaddr addr #order
    SWAP CELL+ DUP @ 			\ forgetaddr #order addr wid
    -ROT SWAP 				\ forgetaddr wid addr #order
  LOOP 					\ forgetaddr ... addr #order
  NIP SET-ORDER 			\ forgetaddr
  (forget) ;

\ Provide inline assembler access. Watch out! The system in put into
\ interpretation state.
: INLINE 				( -- )
  POSTPONE [ ALSO ASSEMBLER [ ALSO ASSEMBLER ] regalloc-reset [ PREVIOUS ]
; IMMEDIATE 

\ End an inline code piece.
: END-INLINE  				( -- )
  PREVIOUS ] ; 

\ Convert a 0x...(hex) 0o...(octal) 0b...(binary) constant from string to a
\ number and return type and success in the flags.
: C-like-number 			( addr len -- d true false / n false false / true )
( OK )
  DUP 2 > IF 				\ addr len
    OVER C@ [CHAR] 0 = IF 		\ addr len 
      OVER CHAR+ C@ 			\ addr len type
      -ROT 2 /STRING 			\ type addr' len'
      BASE @ -ROT TURN 			\ base addr len type
      >upper CASE
        [CHAR] X OF 16 ENDOF
	[CHAR] B OF  2 ENDOF
	[CHAR] O OF  8 ENDOF
	2DROP 2DROP 			\ 
	TRUE EXIT
      ENDCASE 				\ base addr len nbase
      BASE ! 				\ base addr len
      0. 2SWAP 				\ base 0. addr len
      >NUMBER 				\ base d addr len
      TWIST BASE ! 			\ d addr len
      DUP 0= IF
        2DROP DROP 			\ n 
	FALSE FALSE EXIT
      THEN 				\ d addr len
      1 = IF  				\ d addr
        C@ [CHAR] . = IF 		\ d 
	  TRUE FALSE EXIT
	THEN
      ELSE 				\ d addr 
        DROP
      THEN 				\ d
    THEN
  THEN
  2DROP TRUE
;

\ Convert a 'x' constant from string to number and return a flag for success.
: char-const 				( addr len -- n false / true )
( OK )
  3 = IF
    DUP DUP C@ [CHAR] ' = SWAP 		\ addr tick0? addr
    2 CHARS + C@ [CHAR] ' = AND 	\ addr ticks?
    IF 					\ addr
      CHAR+ C@ FALSE EXIT
    THEN
  THEN
  DROP TRUE
;

\ Compile a chain entry. Must be used before the first word that compiles to
\ the data area.
: in-chain 				( -<name>- )
  ' EXECUTE DUP r, COMPILE,
  ; IMMEDIATE 

\ Part of the number parser chain. Accepts C-like number and
\ character constants.
: C-like-parser 			( addr len -- )
( OK )
  2DUP C-like-number 			\ addr len ... error?
  IF 					\ addr len
    2DUP char-const IF 			\ addr len
      in-chain other-numbers
    ELSE 				\ addr len n
      -ROT 2DROP
      STATE @ IF POSTPONE LITERAL THEN
    THEN
  ELSE 					\ addr len d/n dbl?
    IF 					\ addr len d
      2SWAP 2DROP
      STATE @ IF POSTPONE 2LITERAL THEN
    ELSE 				\ addr len n
      -ROT 2DROP
      STATE @ IF POSTPONE LITERAL THEN
    THEN
  THEN
  ;

' C-like-parser TO other-numbers

\ Split a string containing a path and a (partial) filename into the path and
\ the filename.
: split-fn 				( addr len -- pth #pth fn #fn)
( OK )
  2DUP + 0 				\ addr len fn #fn
  BEGIN
    -1 /STRING 				\ addr len fn #fn
    PLUCK OVER 				\ addr len fn #fn len #fn
    1- <>
  WHILE 				\ addr len addr' len'
    OVER C@ [CHAR] / = IF 		\ addr len addr' len'
      1 /STRING
      2SWAP 				\ fn #fn pth len
      PLUCK - 				\ fn #fn pth #pth
      2SWAP EXIT
    THEN
  REPEAT 				\ addr len addr-1 -1
  ( no slash -> no dir, just fn )
  2DROP S" " 2SWAP
;

\ Factor of filename-lcs to shorten the string str to the longest common
\ string of fn,#fn and str. It is assumed that fn,#fn is preceeded by a #pth
\ long path string which is equal to the first #pth characters in str.
: ((fn-lcs) 				( #pth fn #fn str -- #pth str)
  FLOCK OVER $COUNT 			\ #pth fn #fn str #pth addr len
  ROT /STRING 				\ #pth fn #fn str addr len
  ROT -TWIST 				\ #pth str fn #fn addr len
  ROT MIN 				\ #pth str fn addr len1
  lcs 					\ #pth str len2
  PLUCK + 				\ #pth str len3
  OVER ! ;


256 CONSTANT #filename
CREATE slash-buf 1+ #filename ALLOT

\ Append a slash to the filename if it is a directory.
: append-slash 				( pth #pth fn #fn ok? -- pth #pth fn1 #fn1 ok? )
  DUP IF				\ pth #pth fn #fn ok?
    -TWIST 				\ ok? pth #pth fn #fn 
    2OVER 2OVER $allo-cat DUP $COUNT 	\ ok? pth #pth fn #fn str addr len
    FILE-STATUS THROW
    DIRECTORY? 				\ ok? pth #pth fn #fn str dir? 
    SWAP FREE THROW 			\ ok? pth #pth fn #fn dir? 
    IF					\ true pth #pth fn #fn
      SWAP slash-buf 			\ TRUE pth #pth #fn fn sb
      PLUCK CMOVE		 	\ true pth #pth #fn 
      DUP slash-buf + [CHAR] / SWAP C!	\ TRUE pth #pth #fn
      1+ slash-buf SWAP 		\ true pth #pth fn #fn
    THEN
    TWIST 
  THEN ;

\ Factor of filename completer. Return the longest common string str and the
\ hit count n from the path pth,#pth and the partial filename beg,#beg.
: filename-lcs 				( pth #pth beg #beg -- str n )
  2OVER 2DUP FIND-FIRST 		\ pth #pth beg #beg pth #pth fn #fn ok?
  append-slash TWIST TWIST 2DROP
  0 -TURN 				\ pth #pth beg #beg str fn #fn ok?
  0 -TURN 				\ pth #pth beg #beg str n fn #fn ok?
  BEGIN WHILE 				\ pth #pth beg #beg str n fn #fn 
    ROTARE ROTARE 			\ pth #pth str n fn #fn beg #beg 
    2OVER 2OVER 			\ pth #pth str n fn #fn beg #beg fn #fn beg #beg 
    starting? 				\ pth #pth str n fn #fn beg #beg starts?
    IF 					\ pth #pth str n fn #fn beg #beg 
      2ROT 				\ pth #pth fn #fn beg #beg str n 
      1+ SWAP DUP 0= IF 		\ pth #pth fn #fn beg #beg n str
        ( first found )
	DROP 				\ pth #pth fn #fn beg #beg n 
	>R >R >R 			\ pth #pth fn #fn 
	2OVER 2SWAP $allo-cat 		\ pth #pth str
	R> R> R> 			\ pth #pth str beg #beg n
	TURN 				\ pth #pth beg #beg n str
      ELSE 				\ pth #pth fn #fn beg #beg n str
        ( one found, shorten str )
	SWAP >R 			\ pth #pth fn #fn beg #beg str
	-ROT 2>R 			\ pth #pth fn #fn str
	((fn-lcs) 			\ pth #pth str
	2R> ROT 			\ pth #pth beg #beg str
	R> 				\ pth #pth beg #beg str n
	SWAP
      THEN 				\ pth #pth beg #beg n str
      SWAP 				\ pth #pth beg #beg str n
      2SWAP 				\ pth #pth str n beg #beg 
    ELSE 				\ pth #pth str n fn #fn beg #beg 
      2SWAP 2DROP 			\ pth #pth str n beg #beg 
    THEN 				\ pth #pth str n beg #beg 
    2SWAP 				\ pth #pth beg #beg str n 
    2ROT 				\ beg #beg str n pth #pth 
    FIND-NEXT				\ beg #beg str n pth #pth fn #fn ok?
    append-slash			\ beg #beg str n pth #pth fn #fn ok?
    >R >R >R 				\ beg #beg str n pth #pth
    2ROT 2ROT
    R> R> R>
  REPEAT				\ pth #pth beg #beg str n fn #fn 
  2DROP 				\ pth #pth beg #beg str n
  2ROT 2ROT 2DROP 2DROP 		\ str n
;

\ Completer for filenames. It is assumed that the whole inputline contains one
\ filename.
: filename-completer 			( -- )
  (ac-buf) (ac-len)
  split-fn 				\ pth #pth fn #fn
  2SWAP DUP 0= IF
    2DROP S" ./"
  THEN
  2SWAP
  filename-lcs 				\ str n
  DUP 1 > IF ALERT ALERT THEN
  IF
    DUP $COUNT (ac-maxlen) MIN 		\ str addr len
    DUP TO (ac-ind)
    DUP TO (ac-len)
    (ac-buf) SWAP MOVE
    FREE THROW
  ELSE
    ALERT
  THEN
;

CREATE fnbuf #filename ALLOT

\ Input of one filename with a special history list and completer.
: accept-filename 			( -- addr len )
  history-id 				\ hid
  completer 				\ hid compl
  1 TO history-id
  ['] filename-completer TO completer
  fnbuf #filename ACCEPT fnbuf SWAP 	\ hid compl addr len
  2SWAP
  TO completer
  TO history-id 			\ addr len
  ; 

\ Ask for a filename and include it.
: INCLUDE 				( -- )
  ." Enter filename:" 
  accept-filename INCLUDED ;

\ Return the index of char in the string addr,len or len if not found.
: look-for   				( addr len char -- ind )
  -ROT DUP -TURN 			\ len char addr len
  0 ?DO  				\ len char addr 
    DUP C@ PLUCK = IF 			\ len char addr
      I  				\ len char addr i
      -TURN ROT DROP 			\ i char addr
      LEAVE
    THEN
    CHAR+
  LOOP 					\ len char addr
  2DROP ; 

ALSO EDITOR DEFINITIONS

256 $variable edit-cmd
256 $variable view-cmd

\ Perform the function of sprintf( (cmd), cmd, (fn,#fn), line). The %f
\ mask is replaced by the filename (fn,#fn) , %l by the line number line. All
\ other masks are ignored.
: (cmd-instantiate) 			( fn #fn line cmd -- )
  $COUNT 				\ fn #fn line cmd #cmd
  0 TO (cmd)
  BEGIN
    DUP
  WHILE					\ fn #fn line cmd #cmd
    2DUP [CHAR] % look-for 		\ fn #fn line cmd #cmd ind 
    2DUP <> 				\ fn #fn line cmd #cmd ind found?
    IF					\ fn #fn line cmd #cmd ind 
      ROT SWAP 				\ fn #fn line #cmd cmd ind 
      2DUP (cmd) $cat 			\ fn #fn line #cmd cmd ind 
      SWAP -ROT 1+ /STRING 		\ fn #fn line cmd #cmd 
      DUP 0= IF
        2DROP DROP 2DROP EXIT
      THEN				\ fn #fn line cmd #cmd 
      OVER C@ 
      CASE 				\ fn #fn line cmd #cmd char
        [CHAR] f OF			\ fn #fn line cmd #cmd 
  	  TWIST TWIST 
	  2DUP (cmd) $cat
	  -TWIST -TWIST			\ fn #fn line cmd #cmd 
	ENDOF
	[CHAR] l OF			\ fn #fn line cmd #cmd 
           PLUCK (.) (cmd) $cat 	\ fn #fn line cmd #cmd 
	ENDOF
      ENDCASE
      1 /STRING
    ELSE 				\ fn #fn line cmd #cmd ind 
      DROP (cmd) $cat 			\ fn #fn line 
      DROP 2DROP EXIT
    THEN
  REPEAT
  2DROP 2DROP DROP ;

\ Edit a named file at a given line.
: EDIT 					( addr len line -- )
  edit-cmd (cmd-instantiate) 
  (cmd) $COUNT SYSTEM DROP ;

\ View a named file at a given line.
: VIEW 					( addr len line -- )
  view-cmd (cmd-instantiate) 
  (cmd) $COUNT SYSTEM DROP ;

S" vim +%l -i NONE %f" edit-cmd $copy
S" vim +%l -i NONE -R %f" view-cmd $copy

ALSO FORTH DEFINITIONS
\ Prompt for a file to edit and do so.
: ed 					( -- )
  ." File to edit: " accept-filename 1 EDIT ;

\ Prompt for a file to view and view it.
: vf					( -- )
  ." File to view: " accept-filename 1 VIEW ;
   
PREVIOUS PREVIOUS

\ Calculate the time of day in milliseconds
: TIME@ 				( -- d )
  TIME_OF_DAY 				\ sec usec
  1000 / SWAP 1000 UM*  		\ msec dsec
  ROT M+
;

\ Create a new executable and save the system to it.
: new-system 				( addr len -- )
  2DUP copy-executable
  save-system ;

\ Define a string constant which is terminated by a zero.  
: S0" 				( -<">- )
  HERE
  [CHAR] " PARSE 
  0 ?DO 			\ addr 
    DUP C@ C,
    CHAR+
  LOOP 0 C, DROP
  STATE @ IF
    POSTPONE RLITERAL
  THEN
  ; IMMEDIATE

\ Print all loaded files.
: .files 			( -- )
  ." Loaded files (in reverse order): " 
  filename-list BEGIN 		\ list
    DUP IMAGE-BASE <>
  WHILE
    DUP CELL+ $COUNT CR TYPE 
    @
  REPEAT DROP CR ;

\ See standard.
: SEE 					( "<spaces>name" -- )
( OK )
  ' DUP >FN @ CELL+ $COUNT 		\ xt addr len
  2DUP S" ..console.." COMPARE 0= IF
   2DROP >NAME COUNT TYPE 
   ."  was defined on the console. No SEE-ing possible." CR
  ELSE
    ROT >DL @ 
    [ ALSO EDITOR ] VIEW [ PREVIOUS ] 
  THEN ;

0 VALUE (faulty-word-dist)
0 VALUE (faulty-word-xt)
0 VALUE (faulty-word-cfa?)

\ Update the values if code is closer.
macro (update-faulty-word) 			( rel xt code cfa? -- rel xt )
  FLOCK ROT -  				\ rel xt cfa? dist
  DUP (faulty-word-dist) U< IF 		\ rel xt cfa? dist 
    TO (faulty-word-dist)
    TO (faulty-word-cfa?)
    DUP TO (faulty-word-xt)
  ELSE
    2DROP
  THEN

\ Update the values above, when xts ocfa or cfa are closer.
macro (((faulty-word))) 			( rel xt -- rel xt )
  DUP >OCFA @ IMAGE-BASE - 		\ rel xt rocfa
  PLUCK OVER 				\ rel xt rocfa rel ocfa
  >= IF  
    FALSE (update-faulty-word)
  ELSE
    DROP
  THEN 
  DUP >CFA @ IMAGE-BASE - 		\ rel xt rcfa
  PLUCK OVER >= IF
    TRUE (update-faulty-word)
  ELSE
    DROP
  THEN 

\ Search the given voc for a word with a relative cfa/ocfa lower than rel and
\ closer to rel than dist.
macro ((faulty-word)) 			( rel voc -- rel voc )
  DUP >R
  #BUCKETS 0 DO 			\ rel thread
    DUP >R
    BEGIN 				\ rel xt
      @
      IMAGE-BASE OVER <>
    WHILE 				\ rel xt
      (((faulty-word)))
    REPEAT 				\ rel xt
    DROP R> CELL+
  LOOP DROP R> 

\ Find the word thats cfa/ocfa is closest to the given address. base is the
\ image base printed at the run when the fault happend.
: (faulty-word) 			( address base -- xt cfa? )
  2DUP U< ABORT" Wrong address/base passed to (faulty-word)." 
  -
  -1 TO (faulty-word-dist)
  0 TO (faulty-word-xt)
  voc-link BEGIN 			\ rel voc
    ((faulty-word)) 			\ rel voc
    #BUCKETS CELLS + @ 
    DUP IMAGE-BASE =
  UNTIL 				\ rel voc 
  2DROP (faulty-word-xt) (faulty-word-cfa?) ;

\ Print the word thats cfa/ocfa is closest to the given address. base is the
\ image base printed at the run when the fault happend.
: .faulty-word 				( address base -- )
  (faulty-word) 			\ xt cfa?
  OVER 0= IF 
    ." This fault could not be located." CR 
    2DROP
  ELSE
    ." The fault was in the "
    IF 
      ." execution" 
    ELSE 
      ." optimization" 
    THEN ."  semantics of " 
    >NAME COUNT q-TYPE ." ." CR 
  THEN ;

\ Computes n megabytes from n.
: MB 20 LSHIFT ;
\ Computes n kilobytes from n.
: KB 10 LSHIFT ;

\ See standard.
: CS-PICK (opt-flush) (CS-PICK) ;

\ See standard.
: CS-ROLL (opt-flush) (CS-ROLL) ;

