\ FLK "Foreign" language support using shared objects

\ Since the shared objects have to be loaded on startup and closed on shutdown
\ we have to maitain a storage of the libraries. This is done in the words
\ using a linked list which is checked on startup and shutdown. To maintain
\ the correct order of the libraries all declared libraries are appended to
\ the list.

\ Open flags.
1 CONSTANT RTLD_LAZY
2 CONSTANT RTLD_NOW
0x100 CONSTANT RTLD_GLOBAL

\ Head of the library list. The end-of-list pointer is IMAGE-BASE. 
IMAGE-BASE RVALUE libraries

\ The tail of the library list.
#? libraries RVALUE last-lib

macro getlib 				( addr len -- lib )
  2DUP 
  RTLD_LAZY RTLD_GLOBAL OR OPENLIB 	\ addr len lib
  DUP 0= IF 
    DROP ." Error loading " q-TYPE 
    ." : " LIBERROR strlen TYPE CR 
    ABORT 
  ELSE
    -ROT 2DROP
  THEN 

macro get-symbol 			( lib addr len -- sym )
  LIBSYMBOL DUP 0= IF
    LIBERROR strlen TYPE CR ABORT 
  THEN 

\ Format of the data area of a library word:
\ offset 	meaning
\ 0		last library
\ 1 cell	library handle
macro lib>handle CELL+ 

\ 2 cells 	head of symbol chain
macro lib>sym 8 + 

\ 3 cells       filename
macro lib>name 12 + 

\ Format of the data area of a symbol word.
\ offset 	meaning
\ 0 		last lybol
\ 1 cell 	fix addr
macro sym>fix CELL+ 

\ 2 cells 	name
macro sym>name 8 + 

0 VALUE currentlib
\ Create a named library from the filename given. If the filename is not a
\ global one (i.e. starting with a slash) the default library search path is
\ used. See also: man page of dlopen.
: lib 					( addr len -<name>- )
  2DUP getlib CREATE HERE 		\ addr len lib link
  DUP TO currentlib
  ( link into chain )
  DUP last-lib !
  TO last-lib
  IMAGE-BASE r,
  ,					\ addr len  
  IMAGE-BASE r,				\ addr len  
  ,S
  DOES> TO currentlib
  ;

ALSO ASSEMBLER 
macro (push-param-reg) 			( regs -- )
  DUP 0 ?DO 				\ regs
    req-any
  LOOP
  DUP 0 ?DO
    I (tosn) push,
  LOOP
  reg-free 
  
macro (push-param-mem) 			( mems -- )
  0 ?DO
    DWORD offs-ebp [ebp] push,
    4 +TO offs-ebp
  LOOP

PREVIOUS

macro (link-sym) 				( -- )
  currentlib DUP 0= 
  ABORT" No library declared to load from."
  lib>sym DUP @ 			\ sh lsh
  HERE 					\ sh lsh here
  ROT 					\ lsh here sh
  ! 					\ lsh
  r, 

0 CONSTANT void
1 CONSTANT int
2 CONSTANT double

VOCABULARY C-CALLS
ALSO C-CALLS DEFINITIONS PREVIOUS

ALSO ASSEMBLER
\ Define an integer parameter for the use in a shared library function.
: int 					( n -- n+1 )
  regalloc-reset
  req-any
  tos0 push,
  1 reg-free
  1+ ; 

\ Define some integer parameters for the use in a shared library function.
: ints 					( n ints -- n+1)
  regalloc-reset
  TUCK #USEREGS MIN DUP			\ ints n regs regs
  (push-param-reg) 			\ ints n regs
  PLUCK SWAP - 				\ ints n mems
  (push-param-mem) 			\ ints n
  + ; 

\ Define a float parameter for the use in a shared library function.
: float 				( n -- n+1 )
  regalloc-reset
  req-free
  #? FSP #[] free0 mov,
  B/FLOAT ## free0 sub,
  0 jns,
  fstkuflo ## jmp,
0 $:
  FSTACK +relocate [free0] fldf,
  free0 #? FSP #[] mov,
  4 ## esp sub,
  esp free0 mov,
  0 [free0] fstp32,
  1+
;

\ Define a double parameter for the use in a shared library function.
: double 				( n -- n+1)
  regalloc-reset
  req-free
  req-free
  req-free
  #? FSP #[] free0 mov,
  B/FLOAT ## free0 sub,
  0 jns,
  fstkuflo ## jmp,
0 $:
  FSTACK +relocate [free0] fldf,
  free0 #? FSP #[] mov,
  8 ## esp sub,
  esp free0 mov,
  0 [free0] fstp64,
  2 +
  ;

\ Define an ignored parameter for the use in a shared library function.
: void 					( n -- n )
;

PREVIOUS DEFINITIONS
ALSO ASSEMBLER
\ Create a word name as a FORTH front end to the library function named
\ libname. returns is one of the constants above and determines the return
\ type of the function. 
: fct: 					( returns <name params> -- )
  (link-sym)
  currentlib lib>handle @
  skip-space 
  >IN @ SWAP				\ ret ind lib
  BL PARSE				\ ret ind lib addr len
  2DUP 2>R
  get-symbol 				\ ret ind sym
  SWAP >IN !				\ ret sym
  :  					\ ret sym 
  regalloc-reset 			\ ret sym
  DWORD HA-SAVE-F-EBP #[] push, 
  ALSO C-CALLS
  TURN-OFF STATE 
  0 INTERPRET 				\ ret sym cells-esp
  TURN-ON STATE
  PREVIOUS
  regalloc-flushjmp
  ebp HA-SAVE-F-EBP #[] mov, 
  SWAP ## call,				\ ret cells-esp
  CHERE r,
  2R> ,S 				\ ret cesp
  DUP IF 
    CELLS ## esp add,
  ELSE
    DROP
  THEN 					\ ret
  DWORD HA-SAVE-F-EBP #[] pop, 
  CASE 
    int OF 
      free-eax 
      0 free>tos 
    ENDOF
    double OF POSTPONE fpush ENDOF
  ENDCASE 				\ 
  POSTPONE ;
  ;
PREVIOUS

\ Create a variable found in lib.
: var: 					( -<name>- )
  (link-sym) 
  currentlib lib>handle @ 
  skip-space 
  >IN @ 				\ lib ind
  BL PARSE				\ lib ind ln #ln
  2DUP 2>R 				\ lib ind ln #ln
  ROT >IN !  : 				\ lib ln #ln 
  get-symbol 				\ sym
  POSTPONE LITERAL
  CHERE r,
  POSTPONE ;
  2R> ,S ;

VOCABULARY CB-PARAMS
ALSO CB-PARAMS DEFINITIONS PREVIOUS
ALSO ASSEMBLER

\ Define an integer parameter to be used in a callback function.
: int 					( ind -- ind+1 )
  (opt-flush)
  regalloc-reset
  req-free
  esp free0 mov,
  DUP CELLS [free0] free0 mov,
  0 free>tos
  1+
;

PREVIOUS DEFINITIONS

ALSO ASSEMBLER 
\ Define word that's execution semantics can only be used as a callback
\ function. A callback must be ended by ;callback
: callback: 				( returns -<name params>- ret cs)
  STATE @ (coding) OR 
  IF -29 THROW THEN
  skip-space
  BL PARSE 				\ na nl
  empty-def?
  CHERE 				\ na nl xt
  -ROT (namedHeader) 			\ xt
  CFT-callback (new-cs-item)
  (curr-cf-item) 3 CELLS + !
  0 TO #tos-cache
  0 TO offs-ebp 
  regalloc-reset
  ebp push,
  HA-SAVE-F-EBP	#[] ebp mov,
  ebx push,
  ecx push,
  edx push,
  esi push,
  edi push,
  OVER int <> IF eax push, 8 ELSE 7 THEN
  ALSO CB-PARAMS INTERPRET 
  DROP PREVIOUS 			\ ret 
  (curr-cf-item) 4 CELLS + !
  TRUE STATE !
  ;

\ End a callback definition.
: ;callback 				( -- )
  (opt-flush)
  CFT-callback (check-cs-item)
  (curr-cf-item) 4 CELLS + @
  DUP double = IF POSTPONE fpop THEN
  SWAP int = IF
    regalloc-flush 
  ELSE
    regalloc-flushjmp
    eax pop,
  THEN
  edi pop,
  esi pop,
  edx pop,
  ecx pop,
  ebx pop,
  ebp pop,
  ret,
  FALSE STATE ! 			\ cs=xt
  (curr-cf-item) 3 CELLS + @ 
  >FLAGS 				\ ffa
  DUP C@ HF-VISIBLE OR SWAP C!
  (delete-cs-item)
; IMMEDIATE 
PREVIOUS

\ Get the CFA of a word.
: callback  				( -<name>- )
  ' >CFA @ STATE @ IF
    POSTPONE RLITERAL
  THEN ; IMMEDIATE 

\ Correct all symbols in the chain.
: (init-lib-syms) 			( lib sym-dict -- )
  BEGIN
    DUP IMAGE-BASE <>
  WHILE					\ lib sd
    DUP sym>name $COUNT 		\ lib sd addr len
    FLOCK -ROT get-symbol 		\ lib sd sym
    OVER sym>fix @ 			\ lib sd sym fix
    TUCK - SWAP CELL- ! 		\ lib sd
    @
  REPEAT 2DROP ;

\ Load all libraries but not the symbols to be order independed.
: (load-libs) 				( -- )
  libraries BEGIN 			\ lib-dict
    DUP
    IMAGE-BASE <>
  WHILE 				\ lib-dict
    DUP lib>name $COUNT 		\ lib-dict addr len
    getlib 				\ lib-dict lib
    OVER lib>handle ! 			\ lib-dict lib
    @
  REPEAT DROP ;

\ Load all declared symbols from all libraries which have to be loaded
\ already.
: (load-symbols) 				( -- )
  libraries BEGIN 			\ lib-dict
    DUP
    IMAGE-BASE <>
  WHILE 				\ lib-dict
    DUP lib>handle @ 			\ lib-dict lib
    OVER lib>sym @ (init-lib-syms)
    @
  REPEAT DROP ;

\ Load all libraries in the chain and fix their symbols.
: init-lib 				( -- )
  in-chain initializer 
  (load-libs)
  (load-symbols)
  0 TO currentlib
  ;

\ close all libraries in the chain
: done-lib 				( -- )
  in-chain finisher
  libraries BEGIN
    DUP
    IMAGE-BASE <>
  WHILE 				\ lib-dict
    DUP lib>handle @ CLOSELIB
    @
  REPEAT DROP ;

\ Find the first library in the chain below xt and store its address into
\ libraries. All above xt are closed.
: forget-lib 				( xt -- xt )
  in-chain forgetter
  DUP >DFA @ 
  libraries BEGIN 			\ xt lib-dict
    2DUP
    <=
  WHILE 				\ lib-dict
    DUP lib>handle @ CLOSELIB
    @
  REPEAT TO libraries DROP ;

' init-lib TO initializer
' done-lib TO finisher
' forget-lib TO forgetter

\ show all loaded libs
: libs					( -- )
  libraries 
  DUP IMAGE-BASE = IF ." no libs loaded." CR THEN
  BEGIN
    DUP
    IMAGE-BASE <>
  WHILE 				\ lib-dict
    DUP lib>name $COUNT TYPE SPACE
    @
  REPEAT DROP ;

