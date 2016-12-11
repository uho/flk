\  FLK meta compiler
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

\ $Id: flkmeta.fs,v 1.28 1998/09/13 18:55:51 root Exp $
\ $Log: flkmeta.fs,v $
\ Revision 1.28  1998/09/13 18:55:51  root
\ fixed optimizers for cf stack
\
\ Revision 1.27  1998/09/13 16:14:56  root
\ added colon-sys handling to separate cf stack
\
\ Revision 1.26  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.25  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.24  1998/07/13 18:08:54  root
\ various optimizations
\
\ Revision 1.23  1998/07/05 18:45:25  root
\ bugs corrected, added X forms routines, .faulty-word added
\
\ Revision 1.22  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.21  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.20  1998/06/08 22:14:51  root
\ literals cache (preparation to level 2 optimizer)
\
\ Revision 1.19  1998/06/05 20:02:10  root
\ forget corrected (chains)
\
\ Revision 1.18  1998/06/03 16:31:37  root
\ added system calls (as primitives) both in host and target
\
\ Revision 1.17  1998/06/03 07:55:16  root
\ minor bug fixes
\
\ Revision 1.16  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.15  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.14  1998/05/09 21:47:05  root
\ changed order of includeds
\
\ Revision 1.13  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.12  1998/05/02 15:05:55  root
\ now pfe save
\
\ Revision 1.11  1998/05/02 14:27:58  root
\ compile only primitives
\ all files are in this directory
\
\ Revision 1.10  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.9  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.8  1998/04/29 18:20:30  root
\ better hash function
\
\ Revision 1.7  1998/04/24 16:47:39  root
\ bug fixes
\
\ Revision 1.6  1998/04/17 06:23:29  root
\ removed do-sys words
\
\ Revision 1.5  1998/04/15 18:15:30  root
\ float support added
\
\ Revision 1.4  1998/04/11 11:55:44  root
\ FORGET/MARKER support added
\
\ Revision 1.3  1998/04/10 14:42:50  root
\ bugs corrected
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ comment corrected
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ This file is the meta compiler to produce a standalone forth named FLK.
\ It can also be seen as an example how to build a meta compiler,
\ even though there are simpler ones.

\ FLK's and many other's working principle is to create a simulation of the
\ target memory in the data space of the host FORTH (the one, that runs this
\ FORTH-script), fill it with values that make sense for the target system and
\ then save it to a file that can be executed.

\ In FLK the target space is seen as a single piece of memory residing in the
\ RAM of an Intel 386 in 32-bit protected mode. The piece of memory contains
\ both code and data, therefore the CPU has to be programmed to use the
\ so-called flat-memory-mode. This mode has all selectors set to point to the
\ same memory only with different access rights and is used in i.e.
\ DOS-extenders and good OS like Linux.

\ The target space is logically divided into two areas, the code area and the
\ data area. This is done for two reasons: (i) it makes compilation a lot
\ simpler, because no data-forward references have to be used and (ii) it
\ accelerates execution, because the Pentium and above have speed problems
\ when mixing code and data in the same cache line (32 byte). The disadvantage
\ of the need to know the size of both areas before compiling is only a small
\ one.

\ For each word that is created in the target, a word with the same name is
\ created in the host system. 

\ The following comments (i) document the words provided and (ii) guide
\ through the universe of meta compiling.

\ One interesting fact: This meta compiler is intended to be run by any
\ ANS-Forth system. Therefore it is meant to be an ANS-Forth compatible
\ program.

\ At first we tell the user, what happens.
.( Loading FLK META compiler ) CR

\ Then we load some words, that aren't in the STANDARD. If your system
\ provides them, comment out the lines.
: -ROT ROT ROT ;
: PLUCK 2 PICK ;
: TURN 3 ROLL ;
: -TURN TURN TURN TURN ;
: FLOCK 3 PICK ;
: 3DUP PLUCK PLUCK PLUCK ;
: <= > INVERT ;
: >= < INVERT ;
: place SWAP 255 MIN SWAP 2DUP C! CHAR+ SWAP MOVE ;
: $COUNT DUP CELL+ SWAP @ ;
: SEAL GET-ORDER OVER 1 SET-ORDER 0 DO DROP LOOP ;
: VOCABULARY WORDLIST CREATE , DOES> @ >R GET-ORDER NIP R> SWAP SET-ORDER ;

S" flkstring.fs"	INCLUDED
S" array.fs"		INCLUDED

\ Now we create all the vocabularies at once.

\ Contains the host versions of the compilation actions.
VOCABULARY CSEM
\ Contains the host version of the interpretation actions.
VOCABULARY ISEM
\ Contains overrides for immediate words.
VOCABULARY OSEM
\ Contains the meta compiler itself.
VOCABULARY META
\ Contains that carry the COMMENT-flag in the target.
VOCABULARY COMMENTS

\ Setup initial search order.
ALSO META DEFINITIONS

\ Then we need some words to switch between the different search orders.
: [ISEM] ALSO ISEM ; IMMEDIATE
: [OSEM] ALSO OSEM ; IMMEDIATE
: [CSEM] ALSO CSEM ; IMMEDIATE
: [META] ALSO META ; IMMEDIATE
: [COMMENTS] ALSO COMMENTS ; IMMEDIATE
: [FORTH] ALSO FORTH ; IMMEDIATE
: [PREVIOUS] PREVIOUS ; IMMEDIATE
: ISEM-DEF ONLY FORTH ALSO META ALSO ISEM DEFINITIONS PREVIOUS ;
: META-DEF ONLY FORTH ALSO META ALSO DEFINITIONS ;
: OSEM-DEF ONLY FORTH ALSO META ALSO OSEM DEFINITIONS PREVIOUS ;
: CO-DEF ONLY FORTH ALSO META ALSO COMMENTS DEFINITIONS ;
: BINARY 2 BASE ! ;
: OCTAL 8 BASE ! ;

' COMMENTS >BODY @ CONSTANT CO-WID

\ Compiler dependend words. Every compiler has to setup all the words.
: defined 			( -<name>- flag )
  BL WORD FIND NIP 0<> ; 

defined sourceline# [IF] \ gforth
: curline sourceline# ;
[ELSE] \ Unknown compiler
cr .( Please define CURLINE in flkmeta.fs, using a placeholder for now )
: curline -1 ;
[THEN] [THEN]

CREATE curfile 0 , 1024 ALLOT

S" srcdir.fs" INCLUDED

ISEM-DEF
: CELLS 4 * ;
: CELL+ 4 + ;
: CHARS ;
: CHAR+ 1+ ;
: ( POSTPONE ( ; IMMEDIATE
: \ POSTPONE \ ; IMMEDIATE
: ORDER ORDER ;
: [META] POSTPONE [META] ; IMMEDIATE
: [PREVIOUS] POSTPONE [PREVIOUS] ; IMMEDIATE
: OCTAL OCTAL ;
: DECIMAL DECIMAL ;
: FALSE 0 ;
: * * ;

CO-DEF
: ( POSTPONE ( ; IMMEDIATE
: \ POSTPONE \ ; IMMEDIATE

META-DEF
\ exception code, thrown when a non-compiler word is found (check the level 2
\ compiler for details).
36914 CONSTANT RB-E-UNDERFLOW
36915 CONSTANT RB-E-OVERFLOW
36916 CONSTANT RB-E-RANGE
S" ring.fs" INCLUDED

\ The following constants describe the parameters of the target system in
\ general.
512000		CONSTANT CODE-SIZE		\ how many bytes of code
512000 		CONSTANT DATA-SIZE		\ how many bytes of data
 10000		CONSTANT DATA-STACK-SIZE	\ initial data stack size in cells
 10000		CONSTANT CALL-STACK-SIZE	\ initial call stack size in cells
    10 		CONSTANT MAXLOCALLABEL 		\ How many locals are allowed at once?
    10 		CONSTANT OPT2MAXWORDS 		\ Number of entries in level 2
  1000 		CONSTANT CF-ITEMS 		\ Number of control flow stack items
     						\ optimizer list.
ISEM-DEF
CF-ITEMS 	CONSTANT CF-ITEMS 		\ Number of control flow stack items
MAXLOCALLABEL 	CONSTANT MAXLOCALLABEL
OPT2MAXWORDS 	CONSTANT OPT2MAXWORDS
     0 		CONSTANT VREG-EAX  		\ virtual register numbers for 
     1 		CONSTANT VREG-ECX   		\   register allocator
     2 		CONSTANT VREG-EDX
     3 		CONSTANT VREG-EBX
     6 		CONSTANT #USEREGS 		\ # registers for caching
32		CONSTANT #BUCKETS		\ # buckets in vocabulary
16		CONSTANT #IN-ORDER		\ how many vocabularies can be
						\ set at most
256		CONSTANT #TIB-LEN		\ length of terminal input buffer
256		CONSTANT #WORD-PAD		\ max. length for buffer of WORD
80		CONSTANT ((pnolen))		\ length of buffer for
						\ pictured numeric output
256 		CONSTANT #PAD-LEN 		\ length of PAD
RB-S-EMPTY 	CONSTANT RB-S-EMPTY
META-DEF

\ When an error happens while we create the target, the user is told the
\ reason.
: error-exit				( addr len -- )
  CR TYPE CR ." Error loading FLK META compiler." CR BYE ;

\ Now it is time to create the target space. The easiest way to do this is the
\ dynamic allocation of memory. Some implementors CREATE the target space in
\ the vocabulary but this would cause an infinite recursion when the system is
\ recompiled by itself. In this case the size of the dictionary must be twice
\ as large as its size and this is impossible (at least here and now :-) ).
CODE-SIZE DATA-SIZE + ALLOCATE 0<> [IF]
S" Unable to allocate target memory." error-exit
[THEN]
DUP CONSTANT TARGET
CODE-SIZE + CONSTANT DATA-ORIGIN

\ Due to the fact, that the compiled image may be load to any address, a
\ relocation table is nessesary. This is done using a dynamic array. Its
\ address is stored in relotable.
[]( VALUE relotable

\ Add the address relative to TARGET to the relocation list. The address of
\ the array may change within []+=
: relocation!				( addr -- )
  relotable SWAP []+= TO relotable ;

\ The pointers for the last used address in the code area and the data area
\ must be declared. 
0 VALUE tc-here				\ code area pointer
0 VALUE td-here				\ data area pointer

\ The following words to allocate space in the areas check if there is enough
\ space to allocate with the next two words.
: tc-space?				( n -- )
  tc-here + CODE-SIZE < INVERT IF
    S" Out of code space." error-exit
  THEN ;

: tc-allot				( n -- )
  tc-here + TO tc-here ;
  
: td-space?				( n -- )
  td-here + DATA-SIZE < INVERT IF
    S" Out of data space." error-exit
  THEN ;

: td-allot				( n -- )
  td-here + TO td-here ;

: (chk-target-addr) 			( addr -- )
  CODE-SIZE DATA-SIZE + < INVERT IF
    ." Invalid target address passed to (chk-target-addr)." CR
    ." This is surely a bug in the metacompiler." CR
    BYE
  THEN  ;
  
\ Since this meta-compiler should have no environmental dependency, we need to
\ redefine ! and C! when data has to be stored in the target. While we are at
\ it, all addresses there are relative to the start of TARGET.
S" ADDRESS-UNIT-BITS" ENVIRONMENT? INVERT [IF]
.( Environment query failed.) CR
BYE
[THEN]

8 = [IF]

\ Store an 8 bit value at the target address addr.
: t-C! 					( char addr -- )
  DUP (chk-target-addr)
  TARGET + C! ;

: t-C@ 					( addr -- char )
  DUP (chk-target-addr) 
  TARGET + C@ ;

[ELSE]
.( Sorry. Only 8 bit/character are supported at the moment.) CR
.( Add the code yourself in flkmeta.fs or contact the author.) CR
BYE
[THEN]

\ Store a 32 bit value at the target address addr. Since we don't know if this
\ address is cell aligned and if the cell size of the host system is 32 bit
\ too, we need to fake Intels byte ordering. There is no need to character
\ align the address first because the number of bits per character is the same
\ in host and target.
: t-! 					( cell addr -- )
  4 0 DO
    2DUP t-C! 
    1+ SWAP 8 RSHIFT SWAP
  LOOP
  2DROP ;

\ Fetches a 32 bit value from the target.
: t-@ 					( addr -- cell )
  3 +
  0 					\ addr cell
  4 0 DO
    8 LSHIFT
    OVER t-C@ 				\ addr cell add
    + SWAP 1- SWAP
  LOOP NIP ;  				\ cell

\ The items put into code space are characters and cells. If the cells put
\ there are addresses they have to be relocated. This is accomplished by
\ putting an entry into the relocation table.

\ C, into the code area
: tc-c,					( char -- )
  1 tc-space?
  tc-here t-C!
  1 tc-allot ;

\ , into the code area
: tc-,				( cell -- )
  4 tc-space?
  tc-here t-!
  4 tc-allot ;

\ , into the code area with relocation 
: tc-r,					( cell -- )
  tc-here relocation! tc-, ;

\ This word stores a counted string into the code area.
: tc-",					( addr cnt -- )
  DUP tc-c, 0 DO
    DUP C@ tc-c,			\ addr
    [FORTH] CHAR+ [PREVIOUS]
  LOOP
  DROP ;

\ C, into the data area
: td-c,					( char -- )
  1 td-space?
  td-here CODE-SIZE + t-C!
  1 td-allot ;

\ , into the data area
: td-,					( cell -- )
  4 td-space?
  td-here CODE-SIZE + t-!
  4 td-allot ;

\ , into the data area with relocation 
: td-r,					( cell -- )
  td-here CODE-SIZE + relocation! td-, ;

\ This word stores an uncounted string into the data area.
: td-",					( addr cnt -- )
  0 ?DO
    DUP C@ td-c,			\ addr
    [FORTH] CHAR+ [PREVIOUS]
  LOOP
  DROP ;

\ The primitives need a special compiler when loaded into the target, so we
\ fake it in the host ;
: p: : ;
\ Compile-only primitives are handled in a similiar way.
: c: : ; 

\ Changes to the hexadecimal system are quite often, especially in the
\ assembler. The following word eases the pain of switching a lot.
: $$					( -<number>- )
  0.
  BL PARSE				\ wrd nwrd
  BASE @ >R
  HEX
  >NUMBER 2DROP
  R> BASE !
  DROP
  STATE @ IF POSTPONE LITERAL THEN
  ; IMMEDIATE

\ To make the assembler work, the next few words have to be defined. They
\ provide access to the target space ( code area ). 
: asm-,    tc-,  ;
: asm-c,   tc-c, ;
: asm-r,   tc-r, ;
: asm-here tc-here ;
: asm-c!   t-C! ;
: asm-!    t-! ;
\ With all these words, the assembler can be loaded.
S" flkasm.fs"	INCLUDED

BINARY
00000001 CONSTANT HF-IMMEDIATE		\ word is immediate
00000010 CONSTANT HF-VISIBLE		\ word is visible (not compiling)
00000100 CONSTANT HF-CREATED		\ word has been CREATEd
00001000 CONSTANT HF-OIMMEDIATE 	\ word's opt.sem. is immediate
00010000 CONSTANT HF-INDIRECT		\ header is indirect (alias)
00100000 CONSTANT HF-TEMPORARY		\ header is temporary
DECIMAL

\ Header structure
\ ================
\ 0	cell			back-link
\ 4	cell			cfa
\ 8	cell			o-cfa
\ 12	cell 			dfa
\ 16  	cell 			fn-addr
\ 20 	cell 			line
\ 24	char			flags
\ 25	char			namelen
\ 26	chars			name
\ ...	chars			code

26 CONSTANT HEADERLEN 

: .addr 0 HEX <# # # # # # # # # #> TYPE SPACE DECIMAL ;

0 VALUE t-lastheader
0 VALUE filename-list
\ create a header
: (buildHeader)			( back cfa o-cfa flags n-addr n-cnt -- )
\  ." header: " 2DUP TYPE ."  at " tc-here .addr ."  cfa " 4 PICK .addr CR
  tc-here TO t-lastheader
  >R >R >R >R >R
  tc-r, R> tc-r, R> tc-r,
  td-here CODE-SIZE + tc-r,
  filename-list tc-r,
  curline tc-,
  R> tc-c, R> R>			\ n-addr n-cnt
  tc-", ;

\ get to the fields
: t->cfa CELL+ ;			( addr -- cfa )
: t->ocfa CELL+ CELL+ ;			( addr -- ocfa )
: t->dfa CELL+ CELL+ CELL+ ;
: t->flags 24 + ;
: t->name 25 + ;

\ calculate hash value
: (calcVocHash)				( name lname -- hashval)
  OVER C@ 2* 				\ ca u hash
  OVER 1 > IF 				\ ca u hash
    ROT CHAR+ C@ + DUP 2* +		\ u hash
  ELSE
    ROT DROP 				\ u hash
  THEN
  +
  [ISEM] #BUCKETS [PREVIOUS] MOD ;

0 VALUE voc-link

\ Create a wordlist named like the given string.
: (createWL)				( addr len -- )
  td-here CODE-SIZE +
  [ISEM] #BUCKETS [PREVIOUS] 0 DO
    0 td-r,
  LOOP 
  voc-link td-r,
  TO voc-link 
  DUP td-c, td-", ;

\ Lists of words to equipp with an optimizer when the assember is ready.
[]( VALUE CREATE-list
[]( VALUE VALUE-list
[]( VALUE CONSTANT-list

\ Adresses of header fields
\ filled by loader
tc-here		CONSTANT HA-INIT-DATASTACK	\ initial value of EBP
0 tc-,
tc-here		CONSTANT HA-INIT-CALLSTACK	\ initial value of ESP
0 tc-,
tc-here		CONSTANT HA-RELTABLE		\ address of relocation table
0 tc-,
tc-here		CONSTANT HA-LOADER		\ loader table address
0 tc-,
tc-here		CONSTANT HA-HERE-LIMIT		\ first byte after data area
0 tc-,
tc-here		CONSTANT HA-CHERE-LIMIT		\ first byte after code area
0 tc-,
tc-here		CONSTANT HA-IMAGE-BASE		\ base address of image
0 tc-,
tc-here		CONSTANT HA-HERE-INIT		\ initial value of HERE
0 tc-,
tc-here		CONSTANT HA-CHERE-INIT		\ initial value of CHERE
0 tc-,
\ filled by meta-compiler
tc-here		CONSTANT HA-BOOT-CODE 		\ address of boot code
0 tc-r,
tc-here		CONSTANT HA-ENTRY		\ address of entry-point
0 tc-r,
tc-here		CONSTANT HA-CODESIZE		\ number of cells in code area
CODE-SIZE tc-,
tc-here		CONSTANT HA-DATASIZE		\ number of cells in data area
DATA-SIZE tc-,
tc-here		CONSTANT HA-DATA-CELLS		\ initial size of data stack in cells
DATA-STACK-SIZE tc-,
tc-here		CONSTANT HA-CALL-CELLS		\ initial size of call stack in cells
CALL-STACK-SIZE tc-,

\ filled by startup & syscall code
tc-here	CONSTANT HA-SAVE-F-EBP	\ save FORTH EBP for syscalls
0 tc-,
tc-here	CONSTANT HA-SAVE-C-EBP	\ save C EBP for syscalls
0 tc-,
tc-here	CONSTANT HA-SAVE-F-ESP	\ save FORTH ESP for syscalls
0 tc-,
tc-here	CONSTANT HA-SAVE-C-ESP	\ save C ESP for syscalls
0 tc-,
\ start of dictionary
td-here	CODE-SIZE + CONSTANT HA-DEF-WL		\ address of default wordlist
S" FORTH" (createWL)
td-here CODE-SIZE + CONSTANT HA-ASS-WL 		\ address of assembler wl
S" ASSEMBLER" (createWL)
td-here CODE-SIZE + CONSTANT HA-EDT-WL 		\ address of editor wl
S" EDITOR" (createWL)
td-here CODE-SIZE + CONSTANT HA-ENV-WL 		\ address of environment wordlist
S" ENVIRONMENT" (createWL)

\ assemble boot code
\ The boot code fakes a C function with two arguments.
tc-here	CONSTANT HA-BOOT		\ address of boot code
(asm-reset)
\ pop the arguments 
edx pop,				\ return address
edi pop, 				\ argv
esi pop,				\ argc
edi push,
esi push,
edx push, 				\ return address on stack again
\ save ebp,esp
ebp HA-SAVE-C-EBP #[] mov,
esp ebp mov,
ebp HA-SAVE-C-ESP #[] mov,
\ load ebp,esp
HA-INIT-CALLSTACK #[] ebp mov,
ebp esp mov,
HA-INIT-DATASTACK #[] ebp mov,
\ fake stack entries
edi -4 [ebp] mov,
esi eax mov,
-8 ## ebp add,
\ jump to entry
cld,
HA-ENTRY #[] ebx mov,
ebx jmp,

\ Fake FLK's IMAGE-BASE 
: IMAGE-BASE 0 ;

VARIABLE opt2tree
0 opt2tree !

\ Uniform level 2 optimizer interface.
: (opt,) ( xt -- )
\  ." Level 2 optimizer. " .S CR
  EXECUTE 
\  ." done " .S CR
;

\ Uniform level 2 compiler interface.
: (compile,)  			( xt -- )
\  ." Compiling ''" DUP >name name>string type  ." `` " .S CR
  EXECUTE 
\  ." done " .S CR .regalloc
  ;

S" flklevel2.fs" INCLUDED
S" flkcfstack.fs" INCLUDED
-1 cf-sp !

: ((inline-call)) 			( addr -- ) 
  regalloc-reset
  regalloc-flush
  ## call, ;

\ Defining word to compile a create inline. The CREATEd word is put into the
\ COMPILER vocabulary.
: <inline-create>			( header -<name>- )
  ALSO COMMENTS DEFINITIONS
  CREATE , 
  PREVIOUS DEFINITIONS
  DOES>
  @ 					\ header
  t->dfa t-@
  (opt-add-r-const) ;

: <inline-value>			( header -<name>- )
  ALSO COMMENTS DEFINITIONS
  CREATE , 
  PREVIOUS DEFINITIONS
  DOES>
  @ t->dfa t-@ 				\ addr
  >R (opt-flush) R>
  regalloc-reset
  req-free
  #[] free0 mov,
  0 free>tos ;

: <inline-const>			( nr -<name>- )
  ALSO COMMENTS DEFINITIONS
  CREATE , 
  PREVIOUS DEFINITIONS
  DOES>
  @ (opt-add-const) ;

: <inline-r-const>			( nr -<name>- )
  ALSO COMMENTS DEFINITIONS
  CREATE , 
  PREVIOUS DEFINITIONS
  DOES>
  @ (opt-add-r-const) ;

: <inline-call>				( header -<name>- )
  CREATE , DOES> @			\ header
  t->cfa t-@ 				\ cfa
  ((inline-call)) ;

\ Factor to create a inline constant by name.
: (normal-inline)			( na nc nr cmda cmdc -- )
  ROT >R
  ALSO CSEM DEFINITIONS META
  2SWAP $allo-cat			\ str
  R> SWAP				\ nr str
  DUP >R $COUNT EVALUATE		
  R> FREE THROW
  PREVIOUS DEFINITIONS ;

: (inline-create)			( n-ad n-cnt -- )
  tc-here S" <inline-create> " (normal-inline) ;

: (inline-const)			( n-ad n-cnt nr -- )
  S" <inline-const> " (normal-inline) ;

: (inline-r-const)			( n-ad n-cnt nr -- )
  S" <inline-r-const> " (normal-inline) ;

: (inline-value)			( n-ad n-cnt -- )
  tc-here S" <inline-value> " (normal-inline) ;

: (inline-call)				( n-ad n-cnt -- )
  t-lastheader S" <inline-call> "  (normal-inline) ;

0 VALUE meta-current

\ Hash the name and link the next available header address into the
\ vocabulary. The xt of the first word in the relevant hash line is returned.
: t-name>hash				( s l -- back )
  (calcVocHash)	CELLS 
  meta-current +  			\ hash
  DUP t-@ SWAP				\ back hash
  tc-here SWAP t-!			\ back
;

\ Create a default header by name in the target. If osem? is TRUE, the
\ optimization semantics is compiled first.
: (namedHeader)				( s l osem? -- )
  -ROT 					\ osem? s l
  TUCK 					\ osem? l s l
  2DUP >R >R t-name>hash		\ osem? l back 
  SWAP HEADERLEN + tc-here + 0 		\ osem? back cfa o-cfa
  TURN IF SWAP THEN 			\ back cfa o-cfa
  HF-VISIBLE				\ back cfa o-cfa flags
  R> R>					\ back cfa o-cfa flags s l 
  (buildHeader) 
  ;

\ Create a word in the target and execute the host version to produce inline
\ code. The compiled word itself acts like a normal colon definition.
: (t-default-is)			( s l -- )
  2DUP FALSE (namedHeader)		\ s l 
  (begin-word)
  (asm-reset)
  ALSO CSEM ALSO COMMENTS
  EVALUATE
  PREVIOUS PREVIOUS
  (end-word)
  ret, ;

ISEM-DEF ALSO ISEM
: ##NORMAL-ORDER## ONLY FORTH ALSO META ALSO DEFINITIONS ;

\ Set vocabulary into which is compiled.
: in-forth-wl HA-DEF-WL TO meta-current ;
: in-env-wl HA-ENV-WL TO meta-current ;
: in-asm-wl HA-ASS-WL TO meta-current ;
: in-edt-wl HA-EDT-WL TO meta-current ;

in-forth-wl

\ The following words are interpreted by the host to simulate the
\ interpretation semantics of the words with the same names in the target.
: , td-, ;
: r, td-r, ;

: S" [CHAR] " PARSE ;

: ,S 					( ca n -- )
  DUP td-, td-", ;

: ALLOT td-allot ;

: CREATE				( -<name>- )
  BL PARSE				\ n-ad n-cnt
  2DUP (inline-create)
  (t-default-is) 
  t-lastheader t->flags DUP t-C@ 
  [ HF-CREATED HF-OIMMEDIATE OR ] LITERAL 
  OR SWAP t-C!
  CREATE-list t-lastheader []+= TO CREATE-list
  ;

: VARIABLE CREATE 1 CELLS ALLOT ;
: RVARIABLE CREATE 0 r, ;

: VALUE					( n -<name>- )
  BL PARSE				\ n na nl
  2DUP (inline-value)
  (t-default-is) 
  t-lastheader t->flags DUP t-C@ 
  [ HF-CREATED HF-OIMMEDIATE OR ] LITERAL 
  OR SWAP t-C! td-, 
  VALUE-list t-lastheader []+= TO VALUE-list
  ;

\ Relocated VALUE 
: RVALUE				( n -<name>- )
  BL PARSE				\ n na nl
  2DUP (inline-value)
  (t-default-is) 
  t-lastheader t->flags DUP t-C@ 
  [ HF-CREATED HF-OIMMEDIATE OR ] LITERAL 
  OR SWAP t-C! td-r, 
  VALUE-list t-lastheader []+= TO VALUE-list
  ;

: ' 					( -<name>- xt )
  BL PARSE
  ALSO CSEM
  PAD place PAD FIND 			\ caddr 0 / xt 1
  0= ABORT" Unknown word to tick."
  >BODY @
  PREVIOUS ;

\ create a constant in the target
\ Algo:
\ - create word in host that compiles the constant inline
\ - create word in target that puts the constant in tos
: CONSTANT				( nr -<name>- )
  >R
  BL PARSE				\ s l
  2DUP R@ 				\ s l s l nr
  (inline-const)			\ s l 
  (t-default-is) 
  t-lastheader t->flags DUP t-C@ 
  HF-OIMMEDIATE OR SWAP t-C! 
  CONSTANT-list t-lastheader []+=
  FALSE []+=
  R> []+= TO CONSTANT-list
  ;

\ A relocated constant
: RCONSTANT				( nr -<name>- )
  >R
  BL PARSE				\ s l
  2DUP R@ 				\ s l s l nr
  (inline-r-const) 			\ s l 
  (t-default-is) 
  t-lastheader t->flags DUP t-C@ 
  HF-OIMMEDIATE OR SWAP t-C! 
  CONSTANT-list t-lastheader []+=
  TRUE []+=
  R> []+= TO CONSTANT-list
  ;

ISEM-DEF
\ Duplicate constants for the use in the interpretation state.
HA-INIT-DATASTACK 	CONSTANT HA-INIT-DATASTACK
HA-INIT-CALLSTACK 	CONSTANT HA-INIT-CALLSTACK
HA-RELTABLE 		CONSTANT HA-RELTABLE
HA-LOADER 		CONSTANT HA-LOADER
HA-HERE-LIMIT 		CONSTANT HA-HERE-LIMIT
HA-CHERE-LIMIT 		CONSTANT HA-CHERE-LIMIT
HA-IMAGE-BASE 		CONSTANT HA-IMAGE-BASE
HA-HERE-INIT 		CONSTANT HA-HERE-INIT
HA-CHERE-INIT 		CONSTANT HA-CHERE-INIT
HA-BOOT-CODE 		CONSTANT HA-BOOT-CODE
HA-ENTRY 		CONSTANT HA-ENTRY
HA-CODESIZE 		CONSTANT HA-CODESIZE
HA-DATASIZE 		CONSTANT HA-DATASIZE
HA-DATA-CELLS 		CONSTANT HA-DATA-CELLS
HA-CALL-CELLS 		CONSTANT HA-CALL-CELLS
HA-SAVE-F-EBP 		CONSTANT HA-SAVE-F-EBP
HA-SAVE-C-EBP 		CONSTANT HA-SAVE-C-EBP
HA-SAVE-F-ESP 		CONSTANT HA-SAVE-F-ESP
HA-SAVE-C-ESP 		CONSTANT HA-SAVE-C-ESP
HA-DEF-WL 		CONSTANT HA-DEF-WL
HA-ENV-WL 		CONSTANT HA-ENV-WL
HA-ASS-WL 		CONSTANT HA-ASS-WL
HA-EDT-WL 		CONSTANT HA-EDT-WL
HA-BOOT 		CONSTANT HA-BOOT
CALL-STACK-SIZE 	CONSTANT CALL-STACK-SIZE
DATA-STACK-SIZE 	CONSTANT DATA-STACK-SIZE

CO-DEF
\ Fake TO and friends to be comments. They take parse the streeam and search
\ COMMENTS because VALUES are there and only there.
: TO					( -<name>- )
( OK )
  BL PARSE 				\ na nl
  CO-WID SEARCH-WORDLIST 		\ xt 
  0= ABORT" Unknown value to TO to."
  >BODY @  				\ header
  >R (opt-flush) R>
  t->dfa t-@ 				\ dfa
  regalloc-reset
  req-any
  tos0 #[] mov, 
  1 reg-free 
  ; 

: +TO					( -<name>- )
( OK )
  BL PARSE 				\ na nl
  CO-WID SEARCH-WORDLIST 		\ xt 
  0= ABORT" Unknown value to +TO to."
  >BODY @  				\ header
  >R (opt-flush) R>
  t->dfa t-@ 				\ dfa
  regalloc-reset
  req-any
  tos0 #[] add, 
  1 reg-free ;

: TO++					( -<name>- )
  BL PARSE 				\ na nl
  CO-WID SEARCH-WORDLIST 		\ xt 
  0= ABORT" Unknown value to TO++ to."
  >BODY @   				\ header
  >R (opt-flush) R>
  t->dfa t-@ 				\ dfa
  regalloc-reset
  #[] inc, ; 

: TO--					( -<name>- )
  BL PARSE 				\ na nl
  CO-WID SEARCH-WORDLIST 		\ xt 
  0= ABORT" Unknown value to TO-- to."
  >BODY @   				\ header
  >R (opt-flush) R>
  t->dfa t-@ 				\ dfa
  regalloc-reset
  #[] dec, ;

: ['] 					( -<name>- )
  BL PARSE 				\ na nl
  ALSO CSEM
  PAD place PAD FIND			\ xt flg
  PREVIOUS
  0= ABORT" Unknown word to tick."
  >BODY @  				\ header
  (opt-add-r-const) ;

: $$					( -<number>- )
  0.
  BL PARSE				\ wrd nwrd
  BASE @ >R
  HEX
  >NUMBER 2DROP
  R> BASE !
  DROP
  (opt-add-const) ; 
  
META-DEF

FALSE VALUE (t-compiling)

\ Convert a number with optional sign.
: >SNUMBER					( addr len -- d a2 l2 )
  DUP 0= ABORT" >SNUMBER with empty string."
  0. 2SWAP 					\ 0. addr len 
  OVER C@ [CHAR] - =				\ 0. addr len neg
  DUP >R IF
    1 /STRING
  THEN
  >NUMBER 
  R> IF
    2SWAP DNEGATE 2SWAP
  THEN ;

\ Compile an item. If it is a word, execute its compiler word in the host to
\ generate the code in the target.
: (compile-thing)				( addr len -- )
  2DUP CO-WID SEARCH-WORDLIST 			\ addr len xt flag
  IF 						\ addr len xt
    NIP NIP EXECUTE EXIT 
  THEN 						\ addr len
  2DUP PAD place
  PAD FIND  					\ addr len xt flag
  CASE
    0 OF ( unknown ) 				\ addr len xt 
\      ." Possible number." .S CR
      DROP 					\ addr len
      >SNUMBER
      NIP					\ nr. len2
      0<> ABORT" Unknown word"
      D>S					\ nr
      (opt-add-const)
    ENDOF
    1 OF ( immediate ) 				\ addr len xt
\      ." immediate word" .S CR
      NIP NIP 
      >R (opt-flush) R>
\      ." cache flushed" .S CR DUP >name name>string type CR
      EXECUTE
\      ." word executed" .S CR
    ENDOF
    -1 OF ( normal ) 				\ addr len
      NIP NIP (opt-add-xt)
    ENDOF
    ." Unknown return code of FIND: " . CR ABORT
  ENDCASE ;

\ What is considered a space?
: whitespace?				( char -- flag )
  BL > INVERT ;

\ Ignore spaces by advancing >IN.
: (skip-space)				( -- )
  SOURCE				\ addr len
  BEGIN
    >IN @				\ addr len ind
    2DUP >
    IF					\ addr len ind
      ROT 2DUP + 			\ addr len ind caddr
      C@ whitespace? >R
      -rot R>
    ELSE
      FALSE
    THEN
  WHILE					\ addr len ind
    1+ >IN !
  REPEAT				\ addr len ind
  2DROP DROP ;

\ Provide my own outer interpreter. It does not know how to interpret, just
\ how to compile.
: (t-interpret)				( -- )
  BEGIN
    (t-compiling)
  WHILE
    (skip-space)
    BL PARSE				\ addr len
    DUP 0= IF
      2DROP REFILL 0= 			\ refill-fail
      ABORT" Refill failed."
    ELSE				\ addr len
\      2DUP ."   found " TYPE SPACE .S CR
      (compile-thing)
\      ." word compiled" .S CR 
    THEN
  REPEAT ;

CREATE (last-def-name) 0 , 256 CHARS ALLOT

\ Colon compiler. This word is executed in interpretation state. It changes
\ the search order to get to the compilation state and compiles to the end of
\ the definition. If osem? is TRUE, this word is a primitive and the host
\ version of the primitive is called to compile itself into the target.
: (:) 					( osem? prim? -<name>-)
  BL PARSE				\ osem? prim? str len
\  ." Word: " 2DUP TYPE .S CR
  (last-def-name) $copy
  (last-def-name) $count 
  DUP 0= ABORT" Empty definition name."
  tc-here -ROT 				\ osem? prim? header str len
  2>R					\ osem? prim? header
  ROT 2R> 				\ prim? header osem? str len
  ROT (namedHeader) 			\ prim? header
  (begin-word)
  TRUE TO (t-compiling)
  ALSO CSEM ALSO DEFINITIONS OSEM
  CFT-colon (new-cs-item) 		\ prim? header
  (curr-cf-item) 3 CELLS + !
  (curr-cf-item) 4 CELLS + !
  (t-interpret)
\  ." end " .S CR
  (end-word) ret,
  CFT-colon (check-cs-item)
  (curr-cf-item) 4 CELLS + @ 		\ prim?
  (curr-cf-item) 3 CELLS + @ 		\ prim? header
  (delete-cs-item)
  PREVIOUS PREVIOUS DEFINITIONS
  (last-def-name) $count 		\ isem? head str len
  TURN IF ( primitive ) 		\ head str len 
    3DUP 				\ head str len header str len
    ROT t->cfa 				\ head str len str len cfa
    tc-here SWAP t-! 			\ head str len str len
    PAD place
    ALSO OSEM PAD FIND PREVIOUS  	\ head str len xt flag
    0= IF TRUE ABORT" Can't find primitive." THEN
    (begin-word)
    EXECUTE
    (end-word) ret,
  THEN 					\ header str len 
  (inline-call) DROP ;

: include" 				( -<">- )
  [CHAR] " PARSE 2DUP 			\ fn #fn 
  SRCDIR curfile $copy
  curfile $cat
  td-here CODE-SIZE + 			\ fn #fn here
  filename-list td-r,
  TO filename-list			\ fm #fn
  curfile $COUNT DUP td-, td-",
  INCLUDED ;
  
OSEM-DEF
: ; 					( -- )
  FALSE TO (t-compiling) ; IMMEDIATE
ISEM-DEF
\ Compiler for normal word. Uses its own interpreter.
: :					( -<name>- )
  FALSE FALSE (:) ;

\ Primitive compiler. Does basically the same as : above but fills in
\ different fields in the header (ocfa instead of cfa). Even though the use of
\ some primitves like >R and friends while interpreting is ambiguous condition
\ they get an interpretation semantics too.
: p: 					( -<name>- )
  TRUE TRUE (:) ;

\ Compile only primitives. No interpretation semantics are compiled.
: c: 					( -<name>- )
  TRUE FALSE (:) ;
  
: IMMEDIATE					( -- )
  t-lastheader t->flags				\ addr
  DUP t-C@ HF-IMMEDIATE OR 			\ addr flags
  SWAP t-C! ;

: include" include" ;

META-DEF 

\ Create the OS-specific calls.
S" flksys.fs"   INCLUDED
OSEM-DEF
\ Load the host versions of the primitives and control-flow words.
S" flkprim.fs"  INCLUDED
S" linuxsc.fs"  INCLUDED
S" flkhcomp.fs" INCLUDED
META-DEF
S" flkhl2.fs"  INCLUDED
S" flkopt.fs"  INCLUDED

\ Put meta compiler into interpretation state.
ALSO ISEM
\ Create a first word by hand to copy the value from the host version.
voc-link 		RVALUE voc-link
\ From now on only the words in ISEM are executable.
ISEM DEFINITIONS SEAL
\ Duplicate the constants.
0 RCONSTANT IMAGE-BASE

\ Number of simultanious active assembler labels.
MAXLOCALLABEL 		CONSTANT MAXLOCALLABEL

\ Number of control flow stack items.
CF-ITEMS 		CONSTANT CF-ITEMS

\ Length of optimizer cache
OPT2MAXWORDS 		CONSTANT OPT2MAXWORDS 

\ Address of image header field. See flkmeta.fs for meaning.
HA-INIT-DATASTACK 	RCONSTANT HA-INIT-DATASTACK

\ Address of image header field. See flkmeta.fs for meaning.
HA-INIT-CALLSTACK 	RCONSTANT HA-INIT-CALLSTACK

\ Address of image header field. See flkmeta.fs for meaning.
HA-RELTABLE 		RCONSTANT HA-RELTABLE

\ Address of image header field. See flkmeta.fs for meaning.
HA-LOADER 		RCONSTANT HA-LOADER

\ Address of image header field. See flkmeta.fs for meaning.
HA-HERE-LIMIT 		RCONSTANT HA-HERE-LIMIT

\ Address of image header field. See flkmeta.fs for meaning.
HA-CHERE-LIMIT 		RCONSTANT HA-CHERE-LIMIT

\ Address of image header field. See flkmeta.fs for meaning.
HA-IMAGE-BASE 		RCONSTANT HA-IMAGE-BASE

\ Address of image header field. See flkmeta.fs for meaning.
HA-HERE-INIT 		RCONSTANT HA-HERE-INIT

\ Address of image header field. See flkmeta.fs for meaning.
HA-CHERE-INIT 		RCONSTANT HA-CHERE-INIT

\ Address of image header field. See flkmeta.fs for meaning.
HA-BOOT-CODE 		RCONSTANT HA-BOOT-CODE

\ Address of image header field. See flkmeta.fs for meaning.
HA-ENTRY 		RCONSTANT HA-ENTRY

\ Address of image header field. See flkmeta.fs for meaning.
HA-CODESIZE 		RCONSTANT HA-CODESIZE

\ Address of image header field. See flkmeta.fs for meaning.
HA-DATASIZE 		RCONSTANT HA-DATASIZE

\ Address of image header field. See flkmeta.fs for meaning.
HA-DATA-CELLS 		RCONSTANT HA-DATA-CELLS

\ Address of image header field. See flkmeta.fs for meaning.
HA-CALL-CELLS 		RCONSTANT HA-CALL-CELLS

\ Address of image header field. See flkmeta.fs for meaning.
HA-SAVE-F-EBP 		RCONSTANT HA-SAVE-F-EBP

\ Address of image header field. See flkmeta.fs for meaning.
HA-SAVE-C-EBP 		RCONSTANT HA-SAVE-C-EBP

\ Address of image header field. See flkmeta.fs for meaning.
HA-SAVE-F-ESP 		RCONSTANT HA-SAVE-F-ESP

\ Address of image header field. See flkmeta.fs for meaning.
HA-SAVE-C-ESP 		RCONSTANT HA-SAVE-C-ESP

\ Address of image header field. See flkmeta.fs for meaning.
HA-DEF-WL 		RCONSTANT HA-DEF-WL

\ Address of image header field. See flkmeta.fs for meaning.
HA-ENV-WL 		RCONSTANT HA-ENV-WL

\ Address of image header field. See flkmeta.fs for meaning.
HA-ASS-WL 		RCONSTANT HA-ASS-WL

\ Address of image header field. See flkmeta.fs for meaning.
HA-EDT-WL 		RCONSTANT HA-EDT-WL

\ Address of image header field. See flkmeta.fs for meaning.
HA-BOOT 		RCONSTANT HA-BOOT

\ Remember the directory flk was compiled in.
include" srcdir.fs"
\ Load the different wordsets.
include" flkkern.fs"
include" flkexcep.fs"
include" flkstring.fs"
include" flkio.fs"
include" array.fs"
include" flkdict.fs"
include" flkkey_unix.fs"
include" flkinput.fs"
\ A few other wordsets.
in-asm-wl
include" flkasm.fs"
in-forth-wl

\ Do the actual compilation.
: (compile,) 				( xt -- )
\  ." compiling '" DUP >NAME COUNT TYPE ." '" .S CR 
  DUP >OCFA @ 				\ xt ocall
  DUP IMAGE-BASE <> IF 			\ xt ocall
    NIP (EXECUTE)
  ELSE 					\ xt ocall
    DROP
    regalloc-reset
    regalloc-flush
    >CFA @ DWORD ## call,
  THEN 
\  ." compiled." .S CR
  ; 

: (opt,) 
\   ." Level 2 optimizer " .S CR
  (EXECUTE) 
\  ." done " .S CR
  ;

RVARIABLE opt2tree
include" ring.fs"
include" flklevel2.fs"
include" flkprim.fs"
include" linuxsc.fs"
include" flkcfstack.fs"
include" flktcomp.fs"
include" flkhigh.fs"

##NORMAL-ORDER## 

\ Copy a value to a target VALUE.
: t-TO 						( x -<name>- )
  ALSO COMMENTS ' PREVIOUS >BODY @ 		\ x t-xt
  t->dfa t-@ t-! ;

S" flktl2.fs" INCLUDED
include" flkopt.fs"
include" flktopt.fs"
##NORMAL-ORDER## 

\ Copy the list of loaded files into the target.
filename-list t-TO filename-list

\ This POSTPONE is a bit more nasty the average POSTPONE. It reads the word
\ following it from the inputstream of the host while compiling and extracts
\ the unrelocated xt in the target. This xt is compiled as a constant into the
\ host program. It increases the readability of the following words.
: t-postpone 				( -<name>- )
  BL WORD 
  ALSO CSEM FIND PREVIOUS
  0= ABORT" Can't find target colon definition."
  >BODY @ 				\ xt
  POSTPONE LITERAL
; IMMEDIATE

\ Take the given target xt and produce a call to the interpretation semantics.
: t-COMPILE, 				( xt -- )
  >R (opt-flush) R>
  t->cfa t-@ ((inline-call)) ;
  
\ Set the optimization semantics to the next free code address.
: (fix-ocfa) 				( xt -- )
  t->ocfa tc-here SWAP t-! ;

\ Mark the given target xt as an optimizer.
: (mark-opt) 				( xt -- )
  t->flags DUP t-C@ HF-OIMMEDIATE OR    \ flag-addr flags 
  SWAP t-C! ;

\ For every CREATED word the same code with different constants is produced
\ and linked to the optimization semantics. When the produced code is executed
\ later in the target, it compiles the constant inline.
: CREATE-compiler 			( -- )
  CREATE-list 				\ cl
  DUP []# 0 ?DO 			\ cl
    DUP I []@ 				\ cl xt
    DUP (fix-ocfa) 			\ cl xt
    DUP (mark-opt)
    (begin-word)
    t->dfa t-@ (opt-add-r-const)
    t-postpone (opt-add-r-const) t-COMPILE,
    (end-word)
    ret, 				
  LOOP
  )[] ;

\ Same as CREATE-compiler but with other code.
: VALUE-compiler 			( -- )
  VALUE-list 				\ vl
  DUP []# 0 ?DO 			\ vl
    DUP I []@ 				\ vl xt
    DUP (fix-ocfa)
    (begin-word)
    t->dfa t-@ (opt-add-r-const)
    t-postpone (opt-add-r-const) t-COMPILE,
    t-postpone @ (opt-add-r-const)
    t-postpone (opt-add-xt) t-COMPILE,
    (end-word)
    ret,
  LOOP
  )[] ;

\ Same as CREATE-compiler but with other code.
: CONSTANT-compiler 			( -- )
  CONSTANT-list DUP []# 0 ?DO 		\ cl
    DUP I []@ 				\ cl xt
    OVER I 1+ []@ 			\ cl xt relo?
    PLUCK I 2 + []@ 			\ cl xt relo? val
    PLUCK (mark-opt)
    ROT (fix-ocfa) 			\ cl relo? val
    (begin-word)
    SWAP IF 
      (opt-add-r-const) 
      t-postpone (opt-add-r-const) t-COMPILE,
    ELSE 
      (opt-add-const) 
      t-postpone (opt-add-const)   t-COMPILE,
    THEN 				\ cl relo?
    (end-word)
    ret,
  3 +LOOP
  )[] ;

CREATE-compiler 
VALUE-compiler
CONSTANT-compiler

\ Fill header fields with the cfa of the given word. name is the word to be
\ called immediate after start.
: FIRST-WORD 			( -<name> -)
  BL PARSE 			\ addr len
  CSEM GET-ORDER OVER >R	\ addr len ... cnt 
  0 DO DROP LOOP R>		\ addr len wid
  SEARCH-WORDLIST		\ 0 / xt flag
  0= ABORT" Unknown entry word."
  >BODY @			\ header 
  t->cfa t-@ 			\ cfa
  HA-ENTRY t-! 
  PREVIOUS ;

HA-BOOT	HA-BOOT-CODE t-!

\ Create a file if it does not exist.
: get-image-file 		( addr len -- fid )
  2DUP R/W OPEN-FILE		\ addr len fid ior
  0<> IF ( no file )		\ addr len fid 
    DROP
    R/W CREATE-FILE THROW	\ fid
  ELSE ( file exists )		\ addr len fid
    NIP NIP
  THEN ;

: HEADER-ID S" FLK Image 1.0" ;
: HEADER-ID-LEN HEADER-ID NIP ;

CREATE HEADER-BUF HEADER-ID NIP CHARS ALLOT

\ Check if the header read from the file is the right one.
: id-found?			( -- flag )
  HEADER-ID HEADER-BUF SWAP 	\ addr1 addr2 cnt
  0 DO				\ id buf
    2DUP C@ SWAP C@ 128 OR <> 
    IF
      2DROP UNLOOP FALSE EXIT
    THEN
    CHAR+ SWAP CHAR+ SWAP
  LOOP 
  2DROP TRUE ;

\ Walk through the file and find the header. The filepointer is positioned on
\ the first byte of the header-id.
: find-flk-header 		( fid -- )
  DUP FILE-SIZE THROW		\ fid fsd
  ROT >R			\ fsd
  0. BEGIN			\ fsd find
    2DUP R@ REPOSITION-FILE
    THROW			\ fsd find
    HEADER-BUF HEADER-ID-LEN	\ fsd find addr cnt
    R@ READ-FILE THROW		\ fsd find read
    HEADER-ID-LEN <> IF		\ fsd find
      2DROP 2DROP R> DROP EXIT
    THEN
    id-found? IF
      R> REPOSITION-FILE THROW	\ fsd
      2DROP EXIT
    THEN
    1. D+
  AGAIN ;

VARIABLE cell-buf
: write-cell			( fid x -- )
  cell-buf !
  cell-buf 1 CELLS ROT		\ addr x fid
  WRITE-FILE THROW ;

\ Write the header-id into the file.
: overwrite-header		( fid -- )
  HEADER-ID HEADER-BUF SWAP 
  MOVE
  HEADER-BUF HEADER-ID-LEN 0 DO
    DUP C@ 128 OR OVER C!
    CHAR+
  LOOP DROP
  HEADER-BUF HEADER-ID-LEN ROT	\ addr cnt fid
  WRITE-FILE THROW ;

\ Copy the image from the memory into the file. The relocation table preceeds
\ the actual image.
: overwrite-image		( fid -- )
  DUP relotable []#		\ fid fid relcnt
  write-cell			\ fid
  DUP relotable 0 []&		\ fid fid addr
  relotable []# CELLS ROT	\ fid addr cnt fid
  WRITE-FILE THROW 		\ fid
  DUP CODE-SIZE write-cell
  DUP DATA-SIZE write-cell
  DUP tc-here write-cell
  DUP td-here write-cell	\ fid
  DUP 0 write-cell
  DUP TARGET tc-here ROT	\ fid addr cnt fid
  WRITE-FILE THROW
  TARGET CODE-SIZE + td-here 
  ROT				\ addr cnt fid
  WRITE-FILE THROW ;

\ Create or overwrite the image in the given file.
: save-image 			( addr len -- )
  ." Saving to file " 2DUP TYPE CR
  get-image-file
  DUP find-flk-header
  DUP overwrite-header
  DUP overwrite-image 
  CLOSE-FILE THROW ;

\ save image
\ ----------
FIRST-WORD WELCOME
S" flk.flk" save-image

\ Some statistics.
.( code area: ) tc-here DUP 8 .R .(  bytes used, ) CODE-SIZE SWAP - 8 .R .(  bytes free ) CR
.( data area: ) td-here DUP 8 .R .(  bytes used, ) DATA-SIZE SWAP - 8 .R .(  bytes free ) CR

\ Clean up
TARGET FREE DROP
relotable )[]
.( Finished loading META compiler) CR

