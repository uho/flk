\  FLK simple postfix assembler
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

\ $Id: flkasm.fs,v 1.14 1998/09/20 09:34:14 root Exp $
\ $Log: flkasm.fs,v $
\ Revision 1.14  1998/09/20 09:34:14  root
\ changed ebp offset to byte (saving 3 bytes per stack flush)
\
\ Revision 1.13  1998/09/13 15:42:04  root
\ introduced separate control flow stack
\
\ Revision 1.12  1998/07/16 19:31:37  root
\ added conditional near jumps
\
\ Revision 1.11  1998/07/13 18:08:54  root
\ added [esp]
\
\ Revision 1.10  1998/07/03 20:57:50  root
\ ?+relocate added
\
\ Revision 1.9  1998/07/03 09:09:28  root
\ support for level 2 optimizer
\
\ Revision 1.8  1998/06/08 22:14:51  root
\ added SIB-addressing
\
\ Revision 1.7  1998/06/03 16:31:37  root
\ added int,
\
\ Revision 1.6  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.5  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.4  1998/04/24 16:47:39  root
\ added some instructions (fpu)
\ corrected allocator-store
\
\ Revision 1.3  1998/04/16 18:43:49  root
\ flow control improved
\
\ Revision 1.2  1998/04/15 18:15:30  root
\ float support
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ This file implements a classical postfix assembler for Intel 386+ and based
\ on that a register allocator for the optimizer. All operations are  
\ "src dest op,"  or  "dest op," . A postfix assembler is controlled by a few
\ variables that contain information about the type of operand, its size,
\ register number and offset. One set of variables is required per possible
\ operand i.e. a CPU supports commands like "add r0,r1,r17" meaning "add the
\ contents of r1 and r17 and store the result in r0", three sets of variables
\ are used. Apart from some stranger multiplication operations the Intel 386
\ never uses more than 2 operands, further referred to as source and
\ destination. 

\ At the begin of the operation and after each stored operation a pointer has
\ to be re-set that tells the system whether the source or the destination set
\ of variables is meant with the given operand.

\ For a detailed description of the 386 and above see Intels manuals.

\ The next few constants are here for readability.

\ The size of the operand. 
0 CONSTANT SZ-8 			\ al through dh
1 CONSTANT SZ-32 			\ eax through esi
2 CONSTANT SZ-UNKNOWN 			\ for memory references

\ The type of the operand.
0 CONSTANT RT-REG 			\ register
1 CONSTANT RT-INDEX 			\ [register]
2 CONSTANT RT-IMMED 			\ literal
3 CONSTANT RT-ABS 			\ [literal]

\ The scale of a sib-byte
0 CONSTANT SC-1
1 CONSTANT SC-2
2 CONSTANT SC-4
3 CONSTANT SC-8

\ Some error messages are nessesary too.
\ When this messages occurs something really went wrong.
: (internal-error) 			( -- ) 
  CR ." This is an internal assembler error." CR BYE ;
\ This message is printed whenever an invalid combination of operands is
\ found, i.e. after 0 [ebp] 0 [ecx] mov, .
: (unknown-combination) 	( -- )
  CR ." Illegal or unimplemented combination of operands." CR ABORT ;
\ If called with a true flag in TOS (top of stack) the register cache is full.
: TooManyRegs IF ." Too many registers requested." ABORT THEN ;
\ Called with TRUE when the sizes of the operands are different.
: (sz-mismatch) IF ." Mismatching sizes of operands." ABORT THEN ;

\ Called to complain about unknown size.
: (unknown-size) 		( size -- )
  SZ-UNKNOWN = IF ." Unknown operand size." ABORT THEN ;

\ Called to complain about wrong size.
: (wrong-size) 			( wrong? -- )
  IF ." Wrong operand size." ABORT THEN ;


\ These words check a type value for the given property.
: isreg? 				( type -- flag )
  RT-REG = ;
: ismem? 				( type -- flag )
  DUP RT-INDEX =  			\ type flag2
  SWAP RT-ABS = OR ;
: isr/m? 				( type -- flag )
  DUP isreg?  				\ type flag1
  SWAP ismem? OR ;
: isimm?  				( type -- flag )
  RT-IMMED = ;

\ The sets of variables containing the details of the operands.
VARIABLE (ts) VARIABLE (td) 		\ type
VARIABLE (rs) VARIABLE (rd) 		\ register number
VARIABLE (ss) VARIABLE (sd) 		\ size
VARIABLE (os) VARIABLE (od) 		\ offset/immediate ...

VARIABLE (sb) VARIABLE (db) 		\ sib base
VARIABLE (sc) VARIABLE (dc) 		\ sib scale
VARIABLE (si) VARIABLE (di) 		\ sib index

\ Word operations in the 386+ are distinguished from byte operations by
\ setting Bit 0 in the opcode. The following VALUE contains this Bit 0
0 VALUE (wrd)

\ To distinguish between the "normal" (non-SIB) addressing and the
\ SIB-addressing mode the following word is used. Setting if to TRUE makes the
\ mod/rm byte compiler switch to SIB-mode. It is reset by (asm-reset).
FALSE VALUE (sib)

\ To generate byte-offsets on demand the next VALUE is used. Does it contain
\ TRUE, byte offsets are generated.
FALSE VALUE (byte-offs)

\ Prefix to switch to byte offsets.
: BOFFS TRUE TO (byte-offs) ;
 
\ This word simply sets the word/byte bit in the opcode depending on the state
\ of (wrd).
: (w+) 					( opcode -- opcode )
  (wrd) + ;

\ This VALUE is the pointer telling whether source or destination VARIABLEs
\ are meant.
0 VALUE (#operands)

\ Since no syntax checking is performed (How? Why?) this check gives a minimum
\ security that the VARIABLEs contain valid numbers.
: (#operands?) 				( n -- )
  2 > IF ." Too many operands." ABORT THEN ;
  
\ Before giving the first operand to an operation the source/destination
\ pointer must be set to its initial state. This can be done by the user who
\ will forget that now and then or by the system that never forgets this.
\ Therefore this word must be executed before assembling the first instruction
\ and after the assembly of each instruction.
: (0operands) 				( -- )
  0 TO (#operands) ;
  
\ To advance the pointer from source to destination call this word. A check on
\ valid number of operands is placed here because this word is called by all
\ words that advance the pointer.
: (+operand) 				( -- )
  (#operands) DUP (#operands?)
  1+ TO (#operands) ;
  
\ The next word delivers the address of a source or destination VARIABLE
\ depending on the flag given. TRUE means the destination set, FALSE the
\ source.
: {s/d} 				( flag a-src a-dst -- addr )
  ROT IF
    NIP
  ELSE
    DROP
  THEN ;

\ For safety another check of valid operand number is performed before the
\ word decideds between source and destination depending on the state of the
\ source/destination pointer.
: (s/d) 				( addr-source addr-dest -- addr )
  (#operands) DUP (#operands?)
  0<> -ROT {s/d} ;
  
\ These words return the address of the variable depending on the state of the
\ source/destination pointer.
: (type) 	(ts) (td) (s/d) ;
: (size) 	(ss) (sd) (s/d) ;
: (regnum) 	(rs) (rd) (s/d) ;
: (offs) 	(os) (od) (s/d) ;

: (sib-scale) 	(sc) (dc) (s/d) ;
: (sib-index) 	(si) (di) (s/d) ;
: (sib-base) 	(sb) (db) (s/d) ;

\ These words return the address of the variable depending on the flag given.
\ FALSE means source, TRUE destination. All words have the stack effect 
\ ( flag -- addr ) .
: {type} 	(ts) (td) {s/d} ;
: {size} 	(ss) (sd) {s/d} ;
: {regnum} 	(rs) (rd) {s/d} ;
: {offs} 	(os) (od) {s/d} ;

: {sib-scale} 	(sc) (dc) {s/d} ;
: {sib-index} 	(si) (di) {s/d} ;
: {sib-base} 	(sb) (db) {s/d} ;

\ Since every instructions requires a different number of operands these
\ words perform the check.
: need0op 			( addr u -- )
  (#operands) 0 <> IF 
    ." Operation "  TYPE ."  needs no operands." CR BYE
  ELSE 2DROP THEN ;

: need1op 			( addr u -- )
  (#operands) 1 <> IF 
    ." Operation "  TYPE ."  needs one operand." CR BYE
  ELSE 2DROP THEN ;

: need2op 			( addr u -- )
  (#operands) 2 <> IF 
    ." Operation "  TYPE ."  needs two operands." CR BYE
  ELSE 2DROP THEN ;

\ When dealing with relocation tables one needs to know whether a value has to
\ be relocated or not. This VALUE does the job.
FALSE VALUE (relocate?)

\ In some cases the user of the assembler needs to override the default of not
\ relocating, i.e. when loading registers with a value that is an address, so
\ the next word turns relocation on for the the next call of ar?, .
: +relocate ( -- ) TRUE TO (relocate?) ;

\ Relocate the next compiled constant depending on the flag.
: ?+relocate ( flag -- ) IF +relocate THEN ;

\ This word chooses the right version of a the comma-operator depending on the
\ relocation flag, which is reset after use.
: ar?, 				( val -- )
  (relocate?) IF asm-r, ELSE asm-, THEN
  FALSE TO (relocate?) ;

\ Access to the source and destination operand variables is quite often used,
\ so make the job easier and define a few shortcuts.
: ts@ (ts) @ ;
: td@ (td) @ ;
: ss@ (ss) @ ;
: sd@ (sd) @ ;
: rs@ (rs) @ ;
: rd@ (rd) @ ;
: os@ (os) @ ;
: od@ (od) @ ;

\ Whenever the current operand should be a register, this word is called. It
\ is used both by the words for using a specific register and the meta
\ registers. Remember to give the size, because the number of the register
\ alone doesn't tell which register is meant.
: (#reg) 			( num size -- )
  RT-REG (type) !
  (size) !
  (regnum) !
  (+operand) ;

\ Refer to an indexed address. The base (or offset) is given by the user, the
\ number comes either from the words below or from the allocator.
: (#[reg]) 			( offset num -- )
  RT-INDEX (type) !
  (regnum) !
  (offs) ! (+operand) ;

\ Refer to an indexed address. Base, index, scale and offset are given by the
\ user.
: (#[sib]) 			( offs scale index base -- )
  RT-INDEX (type) !
  TRUE TO (sib)
  (sib-base) !
  (sib-index) !
  (sib-scale) ! 
  (offs) ! (+operand) ;

: [esp] 			( offs -- )
  SC-1 4 4 (#[sib]) ; 

\ Refer to an absolute address. It is nessesary to relocate this address.
: #[] 				( addr -- )
  RT-ABS (type) !
  (offs) ! (+operand) +relocate ;
  
\ The value in TOS is an immediate value. Even though it is allowed only as a
\ source, this is not checked here. If the value is an address, +relocate must
\ be called before calling the operation assembling word.
: ## 				( val -- )
  RT-IMMED (type) !
  (offs) ! (+operand) ;

: r## +relocate ## ;

\ Mark the current operand as a double-word (32 bit) or byte (8 bit).
\ Nessesary before xx ## [eax] mov,
: DWORD SZ-32 (size) ! ;
: BYTE SZ-8 (size) ! ;

\ Symbolic names for the registers are easier to remember, so provide words
\ for both 32 bit and 8 bit registers. Because of the register bl and the
\ constant BL collision, the 8 bit registers begin with "reg-" .
: eax 0 SZ-32 (#reg) ;
: ecx 1 SZ-32 (#reg) ;
: edx 2 SZ-32 (#reg) ;
: ebx 3 SZ-32 (#reg) ;
: esp 4 SZ-32 (#reg) ;
: ebp 5 SZ-32 (#reg) ;
: esi 6 SZ-32 (#reg) ;
: edi 7 SZ-32 (#reg) ;

: reg-al 0 SZ-8 (#reg) ;
: reg-cl 1 SZ-8 (#reg) ;
: reg-dl 2 SZ-8 (#reg) ;
: reg-bl 3 SZ-8 (#reg) ;
: reg-ah 4 SZ-8 (#reg) ;
: reg-ch 5 SZ-8 (#reg) ;
: reg-dh 6 SZ-8 (#reg) ;
: reg-bh 7 SZ-8 (#reg) ;

: [eax] 0 (#[reg]) ;
: [ecx] 1 (#[reg]) ;
: [edx] 2 (#[reg]) ;
: [ebx] 3 (#[reg]) ;
: [ebp] 5 (#[reg]) ;
: [esi] 6 (#[reg]) ;
: [edi] 7 (#[reg]) ;

\ This word performs three task. (i) it adjustes the sizes of operands, if one
\ size is unknown, (ii) it stops the execution if the sizes of both operands
\ and (iii) sets the word operation flag.
: (check-sizes) 			( -- )
  ss@ SZ-UNKNOWN = IF sd@ (ss) ! ELSE
  sd@ SZ-UNKNOWN = IF ss@ (sd) ! THEN THEN
  ss@ SZ-UNKNOWN = sd@ SZ-UNKNOWN = AND (sz-mismatch) 
  ss@ sd@ <> (sz-mismatch)
  ss@ SZ-32 = IF 1 ELSE 0 THEN 
  TO (wrd) ;
  
\ Writing an offset or not depends on the type of the memory operand. mop is
\ TRUE when the memory operand is the destination operand, rop when the
\ register operand is.
: (offs/rm), 			( mop rop -- )
  OVER DUP {offs} @ 		\ mop rop mop offs
  SWAP {regnum} @ 5 <> SWAP 0=
  AND IF ( [reg] ) 		\ mop rop
    {regnum} @ 8 * SWAP 
    {regnum} @ + asm-c,
  ELSE ( n [reg] ) 		\ mop rop
    OVER SWAP 			\ mop mop rop
    {regnum} @ 8 * SWAP 
    {regnum} @ + SWAP 		\ hmod/rm mop
    {offs} @ 			\ hmod/rm offs
    SWAP 
    (byte-offs) IF
      64 + asm-c, asm-c,
    ELSE
      128 + asm-c, ar?,
    THEN
  THEN ;

\ This word compiles a mod/rm byte from the settings of operand types,
\ register names etc. if no SIB-byte is required.
: (mod/rm-no-sib) 		( mop rop -- )
  OVER 				\ mop rop mop
  {type} @ 			\ mop rop mtype
  RT-ABS OVER = IF DROP 	\ mop rop 
    {regnum} @ 8 * 5 + asm-c, 	\ mop
    {offs} @ ar?, 		\ 
  ELSE 				\ mop rop mop
    RT-REG = IF 		\ mop rop 
      192 SWAP {regnum} @ 8 * + \ mop mod/rm
      SWAP {regnum} @ + asm-c, 	\ 
    ELSE ( index ) 		\ mop rop 
      (offs/rm), 		\ 
    THEN 
  THEN ;

\ This word compiles a mod/rm byte from the settings of operand types,
\ register names etc. if a SIB-byte is required.
: (mod/rm-sib) 			( mop rop -- )
  {regnum} @ 8 * OVER {offs} @ 
  IF 				\ mop opcocde
    128 + 
  THEN
  4 + asm-C, ( mod/rm ) 		\ mop
  DUP 2DUP 			\ mop mop mop mop
  {sib-scale} @ 6 LSHIFT 	\ mop mop mop sib
  SWAP {sib-index} @ 3 LSHIFT + \ mop mop sib
  SWAP {sib-base} @ + 		\ mop sib
  asm-C, 
  {offs} @ ?DUP IF ar?, THEN ;

\ This word is the main work horse of the assembler. It decides whether or not
\ to use sib addressing and calls the special compiler words for these cases.
: ((mod/rm)), 			( mop rop -- )
  (sib) IF
    (mod/rm-sib)
  ELSE
    (mod/rm-no-sib)
  THEN ;

\ The 386 can handle combinations with at least one memory operand. The
\ decision whether this memory operand is the source or the destination of an
\ operation is done by the opcode. This word is called with the value TRUE
\ when the memory operand is the source operand after the decision
\ produced this flag and compiled the opcode.
: (mod/rm), 			( source-is-rm -- )
  DUP INVERT SWAP ((mod/rm)), ;

\ In the reset-state, the sizes of both operands are unknown and no operands
\ have been accepted yet.
: (asm-reset) 			( -- )
  (0operands) 
  FALSE TO (sib)
  SZ-UNKNOWN (ss) !
  SZ-UNKNOWN (sd) ! 
  FALSE TO (byte-offs) ;

\ All tools for creating assembler operation words are ready so we can start
\ with the actual work.

\ The mov operation is a good example how such operation words are written. At
\ first the checks for valid number of operands and valid sizes are performed.
\ Then all supported register/memory combinations are compared with the
\ parameters and the right version is assembled then. If no combination can be
\ found, complain about it and leave. After successful assembly reset the
\ assembler.
: mov, 				( -- )
  S" mov," need2op
  (check-sizes)
  ts@ isreg? td@ isr/m? AND IF 	( mov r/m, r )
    $$ 88 (w+) asm-c, FALSE (mod/rm),
  ELSE 
  ts@ isr/m? td@ isreg? AND IF 	( mov r, r/m )
    $$ 8A (w+) asm-c, TRUE (mod/rm),
  ELSE
  td@ isreg? ts@ isimm? AND IF
    (wrd) 0<> IF $$ B8 ELSE $$ B0 THEN
    rd@ + asm-c, os@ 
    (wrd) 0<> IF  ar?, ELSE asm-c, THEN
  ELSE
  ts@ isimm? td@ isr/m? AND IF
    $$ C6 (w+) asm-c, 
    td@ CASE
      RT-REG   OF FALSE $$ C0 rd@ + ENDOF
      RT-ABS   OF TRUE 5 ENDOF
      RT-INDEX OF TRUE $$ 80 rd@ + ENDOF
      ." Can't address by ##. Use #[]. " ABORT
    ENDCASE
    asm-c, IF od@ asm-, THEN
    os@ ar?,
  ELSE
    (unknown-combination)
  THEN THEN THEN THEN
  (asm-reset) ;

\ Compile a jmp operation
: jmp, 				( -- )
  S" jmp," need1op
  ts@ RT-IMMED = IF 			( jmp 42 )
    os@ asm-here 5 + -
    $$ E9 asm-c, asm-,
  ELSE
  ts@ isr/m? ss@ SZ-8 <> AND IF
    ss@ SZ-32 <> (wrong-size)
    $$ FF asm-c, 4 ss@ (#reg) TRUE (mod/rm),
  ELSE
    (unknown-combination)
  THEN THEN
  (asm-reset) ;

\ Compile a conditional near (32 bit relative) jump
: (n-jcc,) 				( opcode addr len -- )
  need1op 				\ opcode
  ts@ RT-IMMED = IF
    $$ 0F asm-c, asm-c, os@ asm-here 4 + -
    asm-,
  ELSE
    (unknown-combination)
  THEN (asm-reset) ;

: n-ja,   $$ 87 S" n-ja,"   (n-jcc,) ;
: n-jae,  $$ 83 S" n-jae,"  (n-jcc,) ;
: n-jb,   $$ 82 S" n-jb,"   (n-jcc,) ;
: n-jbe,  $$ 86 S" n-jbe,"  (n-jcc,) ;
: n-jc,   $$ 82 S" n-jc,"   (n-jcc,) ;
: n-je,   $$ 84 S" n-je,"   (n-jcc,) ;
: n-jg,   $$ 8F S" n-jg,"   (n-jcc,) ;
: n-jge,  $$ 8D S" n-jge,"  (n-jcc,) ;
: n-jl,   $$ 8C S" n-jl,"   (n-jcc,) ;
: n-jle,  $$ 8E S" n-jle,"  (n-jcc,) ;
: n-jna,  $$ 86 S" n-jna,"  (n-jcc,) ;
: n-jnae, $$ 82 S" n-jnae," (n-jcc,) ;
: n-jnb,  $$ 83 S" n-jnb,"  (n-jcc,) ;
: n-jnbe, $$ 87 S" n-jnbe," (n-jcc,) ;
: n-jnc,  $$ 83 S" n-jnc,"  (n-jcc,) ;
: n-jne,  $$ 85 S" n-jne,"  (n-jcc,) ;
: n-jng,  $$ 8E S" n-jng,"  (n-jcc,) ;
: n-jnge, $$ 8C S" n-jnge," (n-jcc,) ;
: n-jnl,  $$ 8D S" n-jnl,"  (n-jcc,) ;
: n-jnle, $$ 8F S" n-jnle," (n-jcc,) ;
: n-jno,  $$ 81 S" n-jno,"  (n-jcc,) ;
: n-jnp,  $$ 8B S" n-jnp,"  (n-jcc,) ;
: n-jns,  $$ 89 S" n-jns,"  (n-jcc,) ;
: n-jnz,  $$ 85 S" n-jnz,"  (n-jcc,) ;
: n-jo,   $$ 80 S" n-jo,"   (n-jcc,) ;
: n-jp,   $$ 8A S" n-jp,"   (n-jcc,) ;
: n-jpe,  $$ 8A S" n-jpe,"  (n-jcc,) ;
: n-jpo,  $$ 8B S" n-jpo,"  (n-jcc,) ;
: n-js,   $$ 88 S" n-js,"   (n-jcc,) ;
: n-jz,   $$ 84 S" n-jz,"   (n-jcc,) ;

\ Compile a call
: call, 				( -- )
    S" call," need1op
    ss@ SZ-UNKNOWN = IF
      SZ-32 (ss) !
    THEN
    ss@ SZ-32 <> 
    IF ." Call address must be 32 bit" ABORT THEN
    ts@ RT-IMMED = IF 			( call 42 )
      os@ asm-here 5 + -
      $$ E8 asm-c, asm-,
    ELSE
    ts@ isr/m? IF
      $$ FF asm-c, 2 SZ-32 (#reg) TRUE (mod/rm),
    ELSE
      (unknown-combination)
    THEN THEN
    (asm-reset) ;
  
\ Compile a single byte instruction
: <single-byte> 			( byte -- )
  need0op asm-c, ;

: aaa, 		$$ 37 S" aaa,"   <single-byte> ;
: aas, 		$$ 3F S" aas,"   <single-byte> ;
: clc, 		$$ F8 S" clc,"   <single-byte> ;
: cld, 		$$ FC S" cld,"   <single-byte> ;
: cmc, 		$$ F5 S" cmc,"   <single-byte> ;
: cdq, 		$$ 99 S" cdq,"   <single-byte> ;
: cmpsb, 	$$ A6 S" cmpsb," <single-byte> ;
: cmpsd, 	$$ A7 S" cmpsd," <single-byte> ;
: daa, 		$$ 27 S" daa,"   <single-byte> ;
: das, 		$$ 2F S" das,"   <single-byte> ;
: movsb, 	$$ A4 S" movsb," <single-byte> ;
: movsd, 	$$ A5 S" movsd," <single-byte> ;
: movs, 	$$ A5 S" movs,"  <single-byte> ;
: lodsb, 	$$ AC S" lodsb," <single-byte> ;
: lodsd, 	$$ AD S" lodsd," <single-byte> ;
: nop, 		$$ 90 S" nop,"   <single-byte> ;
: repne, 	$$ F2 S" repne," <single-byte> ;
: repnz, 	$$ F2 S" repnz," <single-byte> ;
: repz, 	$$ F3 S" repz,"  <single-byte> ;
: popf, 	$$ 9D S" popf,"  <single-byte> ;
: ret, 		$$ C3 S" ret,"   <single-byte> ;
: pushf, 	$$ 9C S" pushf," <single-byte> ;
: rep, 		$$ F3 S" rep,"   <single-byte> ;
: repe, 	$$ F3 S" repe,"  <single-byte> ;
: scasb, 	$$ AE S" scasb," <single-byte> ;
: scasd, 	$$ AF S" scasd," <single-byte> ;
: stosd, 	$$ AB S" stosd," <single-byte> ;
: stc, 		$$ F9 S" stc,"   <single-byte> ;
: std, 		$$ FD S" std,"   <single-byte> ;
: xlat, 	$$ D7 S" xlat,"  <single-byte> ;
: stosb, 	$$ AA S" stosb," <single-byte> ;
: sahf, 	$$ E9 S" sahf,"  <single-byte> ;
: WORD: 	$$ 66 S" WORD:"  <single-byte> ;
: wait,         $$ 9B S" wait,"  <single-byte> ;

\ compile an alu-operation
: <alu> 					( eax,i32 r32,i32 col r,rm -- ) 
( OK )
  need2op
  (check-sizes)
  ts@ isimm? IF 				\ eax,i32 r32,i32 col r,rm
    DROP 					\ eax,i32 r32,i32 col
    td@ isreg? rd@ 0= AND IF
      2DROP (w+) asm-c, os@ ar?,
    ELSE
    td@ isr/m? IF 				\ eax,i32 r32,i32 col
      ROT DROP 					\ r32,i32 col
      SWAP (w+) asm-c, 				\ col
      (rs) ! 
      RT-REG (ts) ! 
      FALSE (mod/rm),
      os@ ar?,
    ELSE
     (unknown-combination)
   THEN THEN
  ELSE  					\ eax,i32 r32,i32 col r,rm
  NIP NIP NIP 					\ r,m
  td@ isr/m? ts@ isreg? AND IF
    (w+) asm-c, 
    FALSE (mod/rm),
  ELSE
  td@ isreg? ts@ isr/m? AND IF
    2 + (w+) asm-c, 
    TRUE (mod/rm),
  ELSE
    (unknown-combination)
  THEN THEN THEN
  (asm-reset) ;

: adc,  $$ 14 $$ 80 $$ 02 $$ 10 S" adc,"  <alu> ;
: add,  $$ 04 $$ 80 $$ 00 $$ 00 S" add,"  <alu> ;
: and,  $$ 24 $$ 80 $$ 04 $$ 20 S" and,"  <alu> ;
: cmp,  $$ 3C $$ 80 $$ 07 $$ 38 S" cmp,"  <alu> ;
: or,   $$ 0C $$ 80 $$ 01 $$ 08 S" or,"   <alu> ;
: sbb,  $$ 1C $$ 80 $$ 03 $$ 18 S" sbb,"  <alu> ;
: sub,  $$ 2C $$ 80 $$ 05 $$ 28 S" sub,"  <alu> ;
: test, $$ A8 $$ F6 $$ 00 $$ 84 S" test," <alu> ;
: xor,  $$ 34 $$ 80 $$ 06 $$ 30 S" xor,"  <alu> ;

\ produce a inc/dec
: <inc>, 		( column -- )
  S" inc/dec" need1op
  ts@ isreg? ss@ SZ-32 = AND IF
    8 * $$ 40 + rs@ + asm-c,
  ELSE
  ts@ isr/m? ts@ RT-ABS = OR IF
    $$ FE (w+) asm-c, SZ-32 (#reg) 
    TRUE (mod/rm),
  ELSE
    (unknown-combination)
  THEN THEN
  (asm-reset) ;

: inc, 0 <inc>, ;
: dec, 1 <inc>, ;

: sreg=ecx/cl? 				( -- flag )
  ts@ isreg? rs@ 1 = AND ;

\ produce a shift
: <shift>, 		( column -- )
( OK )
  S" shift" need2op
  sd@ SZ-32 = IF 1 ELSE 0 THEN TO (wrd)
  ts@ isimm? td@ isr/m? AND os@ 1 = AND IF
    $$ D0 (w+) asm-c, 
    RT-REG (ts) !
    (rs) !
    FALSE (mod/rm),
  ELSE
  ts@ isimm? td@ isr/m? AND IF
    $$ C0 (w+) asm-c,
    RT-REG (ts) !
    (rs) !
    FALSE (mod/rm),
    os@ asm-c,
  ELSE
  sreg=ecx/cl? td@ isr/m? AND IF
    $$ D2 (w+) asm-c,
    RT-REG (ts) !
    (rs) !
    FALSE (mod/rm),
  ELSE
    (unknown-combination)
  THEN THEN THEN
  (asm-reset) ;

: rol, 0 <shift>, ;
: ror, 1 <shift>, ;
: rcl, 2 <shift>, ;
: rcr, 3 <shift>, ;
: sal, 4 <shift>, ;
: shl, 4 <shift>, ;
: shr, 5 <shift>, ;
: sar, 7 <shift>, ;
 
\ compile a mul, div, neg or not
: <mul/neg>, 		( column -- )
  S" mul/neg/div" need1op
  ts@ isr/m? ss@ SZ-UNKNOWN <> AND IF
    $$ F6 (w+) asm-c, SZ-32 (#reg) TRUE (mod/rm),
  ELSE
    (unknown-combination)
  THEN
  (asm-reset) ;

: not,  2 <mul/neg>, ;
: neg,  3 <mul/neg>, ;
: mul,  4 <mul/neg>, ;
: imul, 5 <mul/neg>, ;
: div,  6 <mul/neg>, ;
: idiv, 7 <mul/neg>, ;

\ produce an exchange operation
: xchg, 					( -- )
  S" xchg," need2op (check-sizes)
  ts@ isreg? rs@ 0= AND ss@ SZ-32 = AND 
  td@ isreg? AND IF ( xchg eax, reg)
    rd@ $$ 90 + asm-c, 
  ELSE
    $$ 86 (w+) asm-c, ts@ ismem? (mod/rm),
  THEN 
  (asm-reset) ;

\ produce a push/pop
: <push/pop> 					( m32 col rd -- ) 
  S" push/pop" need1op
  ss@ SZ-32 <> 
  IF ." push, and pop, only work on DWORDs." ABORT THEN
  ts@ ismem? IF 				\ m32 col rd
    DROP 					\ m32 col
    SZ-32 (#reg) asm-c, TRUE (mod/rm), 
  ELSE
    ts@ isreg? IF 				\ m32 col
      rs@ + asm-c,
      2DROP
    ELSE
      (unknown-combination)
  THEN THEN (asm-reset) ;

: push, $$ FF 6 $$ 50 <push/pop> ;
: pop,  			( -- )
  S" pop," need1op
  ts@ isimm? IF
    $$ 68 asm-c,
    os@ ar?,
    (asm-reset)
  ELSE
    $$ 8F 0 $$ 58 <push/pop> 
  THEN ;

\ produce a setcc
: <setcc> 					( cc -- )
  S" setcc," need1op
  ss@ SZ-8 <> 
  IF ." setcc, requires a byte operand." ABORT THEN
  ts@ isr/m? IF
    $$ 0f asm-c, asm-c, 0 SZ-32 (#reg) TRUE (mod/rm),
  ELSE
    (unknown-combination)
  THEN
  (asm-reset) ;

: seta,   $$ 97 <setcc> ;
: setae,  $$ 93 <setcc> ;
: setb,   $$ 92 <setcc> ;
: setbe,  $$ 96 <setcc> ;
: setc,   $$ 92 <setcc> ;
: sete,   $$ 94 <setcc> ;
: setg,   $$ 9F <setcc> ;
: setge,  $$ 9D <setcc> ;
: setl,   $$ 9C <setcc> ;
: setle,  $$ 9E <setcc> ;
: setna,  $$ 96 <setcc> ;
: setnae, $$ 92 <setcc> ;
: setnb,  $$ 93 <setcc> ;
: setnbe, $$ 97 <setcc> ;
: setnc,  $$ 93 <setcc> ;
: setne,  $$ 95 <setcc> ;
: setng,  $$ 9E <setcc> ;
: setnge, $$ 9C <setcc> ;
: setnl,  $$ 9D <setcc> ;
: setnle, $$ 9F <setcc> ;
: setno,  $$ 91 <setcc> ;
: setnp,  $$ 9B <setcc> ;
: setns,  $$ 99 <setcc> ;
: setnz,  $$ 95 <setcc> ;
: seto,   $$ 90 <setcc> ;
: setp,   $$ 9A <setcc> ;
: setpe,  $$ 9A <setcc> ;
: setpo,  $$ 9B <setcc> ;
: sets,   $$ 98 <setcc> ;
: setz,   $$ 94 <setcc> ;

\ The local label mechanism is quite simple but useful.  The number passed to
\ jcond, is the number of the local label which can be used either as a
\ forward or backward jump label. Due to space constraints only one forward
\ jump can be used for one label, but an unlimited number of backward jumps to
\ this label. It is possible to use a label for a fwd jump first and then for
\ backward jumps. If you need more than one forward jump to the same place,
\ use different labels.
CREATE loclabel-tab MAXLOCALLABEL CELLS ALLOT

\ The given label number is checked and complained about if wrong.
: (chk-label-ind) 			( ind -- )
  MAXLOCALLABEL < INVERT 
  IF ." Label number too high." ABORT THEN ;

\ Provide simple access to the labels.
: >label 				( label -- addr )
  DUP (chk-label-ind)
  CELLS loclabel-tab + ;

\ An address of 0 for a label means that the label is not used yet. At the
\ start of each local label scope this state has to be set.
: reset-labels 			( -- )
  MAXLOCALLABEL 0 DO
    0 I >label !
  LOOP ;

: save-labels 			( -- labels n )
  MAXLOCALLABEL 0 DO
    I >label @
  LOOP MAXLOCALLABEL ;

: restore-labels 		( labels n -- )
  DROP
  MAXLOCALLABEL 0 DO
    MAXLOCALLABEL 1- I - >label !
  LOOP ;

\ Declare a local label. If the label is a forward jump calculate the offset,
\ check it and store it in the appropiate place.
: $: 					( label -- )
  DUP >label @ 0<> IF ( fwd jmp )
    DUP >label @ 			\ label dst-addr
    asm-here OVER 1+ - 			\ label dst-addr abs-dist
    DUP 127 < INVERT
    IF ." Jump out of bounds." ABORT THEN \ label dst-addr abs-dist
    SWAP asm-c!
  THEN
  asm-here SWAP >label ! ;

\ Change the short-branch-target to the give address.
: change-$: 				( addr label -- )
  >label ! ;

\ Compile a conditional jump.
: <jcc>, 					( label opcode -- )
  asm-c, 					\ label  
  DUP 
  >label @ 
  0= IF ( fwd jmp ) 				\ label 
    asm-here SWAP 				\ here label
    >label !
    0 asm-c,
  ELSE 	( bwd jmp) 				\ label
    asm-here 1+ SWAP >label @ 			\ dst orig
    - DUP 127 < INVERT
    IF ." Jump out of bounds." ABORT THEN  	\ abs-dist
    NEGATE asm-c,
  THEN ;

: jae,    $$ 73 <jcc>, ;
: jb,     $$ 72 <jcc>, ;
: jbe,    $$ 76 <jcc>, ;
: jc,     $$ 72 <jcc>, ;
: jcxz,   $$ E3 <jcc>, ;
: je,     $$ 74 <jcc>, ;
: jg,     $$ 7F <jcc>, ;
: jge,    $$ 7D <jcc>, ;
: jl,     $$ 7C <jcc>, ;
: ja,     $$ 77 <jcc>, ;
: jle,    $$ 7E <jcc>, ;
: jnle,   $$ 7F <jcc>, ;
: jna,    $$ 76 <jcc>, ;
: jno,    $$ 71 <jcc>, ;
: jnae,   $$ 72 <jcc>, ;
: jnp,    $$ 7B <jcc>, ;
: jnb,    $$ 73 <jcc>, ;
: jns,    $$ 79 <jcc>, ;
: jnbe,   $$ 77 <jcc>, ;
: jnz,    $$ 75 <jcc>, ;
: jnc,    $$ 73 <jcc>, ;
: jo,     $$ 70 <jcc>, ;
: jne,    $$ 75 <jcc>, ;
: jp,     $$ 7A <jcc>, ;
: jng,    $$ 7E <jcc>, ;
: jpe,    $$ 7A <jcc>, ;
: jnge,   $$ 7C <jcc>, ;
: jpo,    $$ 7B <jcc>, ;
: jnl,    $$ 7D <jcc>, ;
: js,     $$ 78 <jcc>, ;
: jz,     $$ 74 <jcc>, ;
: loopne, $$ E0 <jcc>, ;
: loopnz, $$ E0 <jcc>, ;
: loopz,  $$ E1 <jcc>, ;
: loop,   $$ E2 <jcc>, ;
: loope,  $$ E1 <jcc>, ;
\ Uncondition short jump.
: jmpn,   $$ EB <jcc>, ; 

\ ------------------------------------------------------------------------------
\ ---------------------------- floating point words ----------------------------
\ ------------------------------------------------------------------------------

\ FPU stack items
0 CONSTANT st0
1 CONSTANT st1
2 CONSTANT st2
3 CONSTANT st3
4 CONSTANT st4
5 CONSTANT st5
6 CONSTANT st6
7 CONSTANT st7

\ Check the given FPU register for valid index.
: st-range 				( st -- )
  0 8 WITHIN INVERT IF ." Invalid FP-Stack register." ABORT THEN ;

\ Compile an FPU operation requiring a mod/rm parameter.
: <fop-mod/rm>, 			( col op addr len -- )
  need1op asm-c,
  SZ-32 (#reg) TRUE (mod/rm),
  (asm-reset) ;

\ Compile an FPU operation without or with implicit parameters.
: <fop>, 			( oc1 oc2 addr len -- )
  need0op
  SWAP asm-c, asm-c,
  (asm-reset) ;

\ Compile an FPU operation with one FPU register as parameter.
: <fopst>, 			( st oc1 oc2 addr len -- )
  need0op PLUCK st-range
  SWAP asm-c, + asm-c,
  (asm-reset) ;

\ Store operations in different formats.
: fst32,   2 $$ D9 S" fst32,"   <fop-mod/rm>, ;
: fst64,   2 $$ DD S" fst64,"   <fop-mod/rm>, ;
: fstp32,  3 $$ D9 S" fstp32,"  <fop-mod/rm>, ;
: fstp64,  3 $$ DD S" fstp64,"  <fop-mod/rm>, ;
: fstp80,  7 $$ DB S" fstp80,"  <fop-mod/rm>, ;
: fist16,  2 $$ DF S" fist16,"  <fop-mod/rm>, ;
: fist32,  2 $$ DB S" fist32,"  <fop-mod/rm>, ;
: fistp16, 3 $$ DF S" fistp16," <fop-mod/rm>, ;
: fistp32, 3 $$ DB S" fistp32," <fop-mod/rm>, ;
: fistp64, 7 $$ DF S" fistp64," <fop-mod/rm>, ;
: fbstp,   6 $$ DF S" fbstp,"   <fop-mod/rm>, ;

\ Loads in different formats.
: fld32,   0 $$ D9 S" fld32,"   <fop-mod/rm>, ;
: fld64,   0 $$ DD S" fld64,"   <fop-mod/rm>, ;
: fld80,   5 $$ DB S" fld80,"   <fop-mod/rm>, ;
: fild16,  0 $$ DF S" fild16,"  <fop-mod/rm>, ;
: fild32,  0 $$ DB S" fild32,"  <fop-mod/rm>, ;
: fild64,  5 $$ DF S" fild64,"  <fop-mod/rm>, ;
: fbld,    4 $$ DF S" fbld,"    <fop-mod/rm>, ;

\ Other operations to memory.
: frstor,  4 $$ DD S" frstor,"  <fop-mod/rm>, ;
: fnsave,  6 $$ DD S" fnsave,"  <fop-mod/rm>, ;
: fnstcw,  7 $$ D9 S" fnstcw,"  <fop-mod/rm>, ;
: fldcw,   5 $$ D9 S" fldcw,"   <fop-mod/rm>, ;

\ Calculations, comparing ops, etc.
: fchs,     $$ D9 $$ E0 S" fchs,"     <fop>, ;
: fabs,     $$ D9 $$ E1 S" fabs,"     <fop>, ;
: f2xm1,    $$ D9 $$ F0 S" f2xm1,"    <fop>, ;
: fcos,     $$ D9 $$ FF S" fcos,"     <fop>, ;
: fscale,   $$ D9 $$ FD S" fscale,"   <fop>, ;
: fsin,     $$ D9 $$ FE S" fsin,"     <fop>, ;
: fsincos,  $$ D9 $$ FB S" fsincos,"  <fop>, ;
: fsqrt,    $$ D9 $$ FA S" fsqrt,"    <fop>, ;
: ftst,     $$ D9 $$ E4 S" ftst,"     <fop>, ;
: fxtract,  $$ D9 $$ F4 S" fxtract,"  <fop>, ;
: fyl2x,    $$ D9 $$ F1 S" fyl2x,"    <fop>, ;
: fyl2xp1,  $$ D9 $$ F9 S" fyl2xp1,"  <fop>, ;
: fnstswax, $$ DF $$ E0 S" fnstswax," <fop>, ;
: fcompp,   $$ DE $$ D9 S" fcompp,"   <fop>, ;
: fld1,     $$ D9 $$ E8 S" fld1,"     <fop>, ;
: fldl2t,   $$ D9 $$ E9 S" fldl2t,"   <fop>, ;
: fldl2e,   $$ D9 $$ EA S" fldl2e,"   <fop>, ;
: fldpi,    $$ D9 $$ EB S" fldpi,"    <fop>, ;
: fldlg2,   $$ D9 $$ EC S" fldlg2,"   <fop>, ;
: fldln2,   $$ D9 $$ ED S" fldln2,"   <fop>, ;
: fldz,     $$ D9 $$ EE S" fldz,"     <fop>, ;
: fincstp,  $$ D9 $$ F7 S" fincstp,"  <fop>, ;
: frndint,  $$ D9 $$ FC S" frndint,"  <fop>, ;
: fxam,     $$ D9 $$ E5 S" fxam,"     <fop>, ;
: fninit,   $$ DB $$ E3 S" fninit,"   <fop>, ;
: fpatan,   $$ D9 $$ F3 S" fpatan,"   <fop>, ;
: fprem,    $$ D9 $$ F8 S" fprem,"    <fop>, ;
: fprem1,   $$ D9 $$ F5 S" fprem1,"   <fop>, ;
: fptan,    $$ D9 $$ F2 S" fptan,"    <fop>, ;
: ftst,     $$ D9 $$ E4 S" ftst,"     <fop>, ;

: fld,      $$ D9 $$ C0 S" fld,"    <fopst>, ;
: fmulp,    $$ DE $$ C8 S" fmulp,"  <fopst>, ;
: faddp,    $$ DE $$ C0 S" faddp,"  <fopst>, ;
: fsubp,    $$ DE $$ E8 S" fsubp,"  <fopst>, ;
: fdivp,    $$ DE $$ F8 S" fdivp,"  <fopst>, ;
: ffree,    $$ DD $$ C0 S" ffree,"  <fopst>, ;
: fxch,     $$ D9 $$ C8 S" fxch,"   <fopst>, ;
: fstp,     $$ DD $$ D8 S" fstp,"   <fopst>, ;
: fmul,     $$ D8 $$ C8 S" fmul,"   <fopst>, ;
: fmulr,    $$ DC $$ C8 S" fmulr,"  <fopst>, ;
: fcomp,    $$ D8 $$ D8 S" fcomp,"  <fopst>, ;
: fcom,     $$ D8 $$ D0 S" fcom,"   <fopst>, ;
\ Since I am much too lazy to write a special kind of assembler word for fsub
\ I invented a mnemonic: fssub. It means: float-swap-subtract and is written
\ as: fsub st(i), st.
: fssub,    $$ DC $$ E8 S" fssub,"  <fopst>, ;
\ This is the normal subtraction operator: fsub st, st(i)
: fsub,     $$ D8 $$ E0 S" fsub,"   <fopst>, ;
\ The reverse subtraction with pop.
: fsubrp,   $$ DE $$ E0 S" fsubrp," <fopst>, ;

\ Compile a software interrupt.
: int, 				( nr -- )
  S" int," need0op
  $$ CD asm-c, asm-c, (asm-reset) ;

\ The load-effective-address operation. It is most useful with sib-addressing.
: lea, 				( -- )
  S" lea," need2op
  $$ 8D asm-c, TRUE (mod/rm), (asm-reset) ;

\ ==============================================================================
\ =============================== meta assembler ===============================
\ ==============================================================================

\ The register allocator uses the registers eax, ebx, ecx, edx, esi and esi to
\ cache the upper 6 items on the data stack. It is very time expensive to
\ restore the correct possitions of all 6 registers especially if we assume
\ that an average of 3 registers is cached. But caching only one register by
\ default is never a bad idea.

\ At the begin of each word, EAX caches TOS. Any other register is available.
\ Using 32-bit-flat-memory-mode, only near calls are used. EBP is used as the
\ data stack pointer. Therefore an offset is always nessesary. This offset
\ is accumulated when moving data to and from the stack. It is assumed to be
\ zero at the begin and end of each word, therefore EBP points to TOS. 
\ In conjunction with the (future) use
\ of inlineable words, this saves one ADD/SUB EBP, 4 in every word using the 
\ stack, but introduces on ADD EBP,x in the return. This can be paired with the
\ last store of the register, that have to be flushed to stack or the move
\ to make EAX TOS or the RET itself.

\ Using the registers as virtual TOS+x "stackrobatics" with less than seven
\ registers do not cost any cycle, if no register has to be loaded before or
\ flushed after. Only in case the op. request a special register in a special
\ place, some mov, or xchg, is produced.
 
\ An example:
\ : test 		( a b c -- e )
\ (1) ROT 		\ b c a 		2 cycles for loading ebx, ecx
\ (2) ROT 		\ c a b 		no cycle, just changing the
\ 						the order while compiling
\ (3) + 		\ c a+b 		one cycle for the add
\ (4) SWAP 		\ a+b c 		no cycle
\ (5) 2* 2* 		\ a+b 4*c 		2 cycles for the shifts
\ (6) + 		\ a+b+4*c 		1 cycle for the add
\ (7) ; 					2 cycles for flush + bp ofs
\
\ In line (1) EBX and ECX are loaded, because ROT asks for 3 used registers.
\ In line (2) only the tables in the compiler are changed, but no code is
\ produced.
\ Lines (3) and (5) perform their calculations by producing one ADD and 
\ two SHL's.
\ Line (4) produces no code, just the tables are changed.
\ In Line (7) the consistent state must be accomplished, therefore
\ one register has to be flushed (and EAX may be loaded from an other register, I
\ haven't tracked this) and EBP has to be increased by 8.

\ The registers need numbers to identify them.
    0 		CONSTANT VREG-EAX  		\ virtual register numbers for 
    1 		CONSTANT VREG-ECX   		\   register allocator
    2 		CONSTANT VREG-EDX
    3 		CONSTANT VREG-EBX
    4 		CONSTANT VREG-ESI
    5 		CONSTANT VREG-EDI
    6 	 	CONSTANT #USEREGS
-1 	CONSTANT REG-NONE

\ Translate vreg to reg using a look-up table.
CREATE ((vreg>reg)) 0 , 1 , 2 , 3 , 6 , 7 ,
: (vreg>reg) 			( vreg -- reg )
  DUP REG-NONE = 
  IF ." Called (vreg>reg) with invalid register." ABORT THEN
  CELLS ((vreg>reg)) + @ ;

\ This array contains the state of the register allocator. The cell i contains
\ the number of the register caching TOS+i.
CREATE tos-cache #USEREGS CHARS ALLOT
: cache! 				( reg ind -- )
  CHARS tos-cache + C! ;
: cache@ 				( ind -- reg )
  CHARS tos-cache + C@ ;

\ This array contains the numbers of registers that were marked free.
CREATE free-cache #USEREGS CHARS ALLOT
: free! 					( reg ind -- )
  CHARS free-cache + C! ;
: free@ 					( ind -- reg )
  CHARS free-cache + C@ ;

\ Number of items in tos-cache
0 VALUE #tos-cache

\ Increase #tos-cache
: (#tc++) #tos-cache 1+ TO #tos-cache ;

\ Number of items in free-cache
0 VALUE #free-req

\ Who are you?
: vreg>name 				( vreg -- addr len )
  CASE 
    VREG-EAX OF S" eax" ENDOF
    VREG-EBX OF S" ebx" ENDOF
    VREG-ECX OF S" ecx" ENDOF
    VREG-EDX OF S" edx" ENDOF
    VREG-ESI OF S" esi" ENDOF
    VREG-EDI OF S" edi" ENDOF
    >R S" unknown" R>
  ENDCASE ;

\ Mark a register as requested with free.
: (mark-free) 				( vreg -- )
  #free-req free! #free-req 1+ TO #free-req ;

\ Check if vreg is marked free.
: (#marked) 				( vreg -- flag )
  #free-req 0 ?DO 			\ vreg
    I free@ OVER = IF
      DROP TRUE UNLOOP EXIT
    THEN
  LOOP 
  DROP FALSE ;

\ Number of consequtively requested registers in compiler
0 VALUE #reg-req

\ Increase #reg-req .
: (#rr++) #reg-req 1+ TO #reg-req ;

\ Accumulated offset to ebp
0 VALUE offs-ebp

\ Maintain a offs-ebp to be within +/- 124 to fit into a byte.
: (add-ebp) 			( n -- )
  offs-ebp + 			\ no
  DUP ABS 124 >= IF 		\ no
    ## ebp add,
    0
  THEN
  TO offs-ebp ;

\ Save the state of the allocator on the stack. No code is produced.
: save-allocator 			( -- allocator )
  #USEREGS 0 DO I cache@ LOOP
  #USEREGS 0 DO I free@ LOOP
  #tos-cache #free-req offs-ebp ;

\ Restore the state of the allocator from the stack. No code is produced.
: restore-allocator 			( allocator -- )
  TO offs-ebp
  TO #free-req TO #tos-cache
  #USEREGS 0 DO #USEREGS 1- I - free! LOOP
  #USEREGS 0 DO #USEREGS 1- I - cache! LOOP ;

\ Print the state of the allocator.
: .regalloc 				( -- )
  ." Used: "
  #tos-cache 0= IF
    ." none"
  ELSE
    #tos-cache 0 DO
      #tos-cache 1- I - cache@ vreg>name TYPE ."  "
    LOOP
  THEN
  ."   Free: "
  #free-req 0= IF
    ." none" 
  ELSE
    #free-req 0 DO
      #free-req 1- I - free@ vreg>name TYPE ."  "
    LOOP
  THEN ."  offs: " offs-ebp . CR ;

\ Check whether vreg is used
: (#used) 				( vreg -- flag )
  #tos-cache 0 ?DO
     I cache@ OVER = IF
        DROP UNLOOP TRUE EXIT
     THEN
  LOOP
  DROP  FALSE ;

\ find the vreg in current requested cache slot
: tc(#rr) 				( -- vreg )
  #reg-req cache@ ;
  
\ check whether enough registers are in use
: (#enough) 				( -- flag )
  #reg-req #tos-cache < ; 

\ load the register vreg into current requested slot
: (#load) 				( vreg -- )
  BOFFS offs-ebp #tos-cache CELLS + [ebp] 
  DUP (vreg>reg) SZ-32 (#reg) mov,
  #tos-cache cache! 
  #tos-cache 1+ TO #tos-cache ;

\ find the cache-slot vreg is in. Return -1 for non-cached register
: (#find) 				( vreg -- nr )
  #tos-cache 0 ?DO
    I cache@ OVER = IF
      DROP I UNLOOP EXIT
    THEN
  LOOP
  DROP -1 ;

\ exchange the meanings and the contents of the regs
\ free(vreg1) used(vreg2)  -> mov vreg1, vreg2
\ used(vreg1) free(vreg2)  -> mov vreg2, vreg1
\ else                     -> xchg vreg1, vreg2
: tc-xchg 				( vreg1 vreg2 -- )
  2DUP = IF 2DROP EXIT THEN
  DUP (#used) INVERT IF 		( vreg2 free ) \ vreg1 vreg2
    2DUP SWAP 
    (vreg>reg) SZ-32 (#reg) 
    (vreg>reg) SZ-32 (#reg) mov, 	\ vreg1 vreg2
    SWAP (#find) 			\ vreg2 tos1
    cache!
  ELSE
    OVER (#used) INVERT IF 		( vreg1 free ) \ vreg1 vreg2
      2DUP 
      (vreg>reg) SZ-32 (#reg) 
      (vreg>reg) SZ-32 (#reg) mov, 	\ vreg1 vreg2
      (#find) 				\ vreg1 tos2
      cache!
    ELSE
      2DUP
      (vreg>reg) SZ-32 (#reg) 
      (vreg>reg) SZ-32 (#reg) xchg, 	\ vreg1 vreg2
      2DUP (#find) SWAP (#find) 	\ vreg1 vreg2 tos2 tos1
      ROT SWAP 				\ vreg1 tos2 vreg2 tos1
      cache! cache!
    THEN
  THEN ;

\ Set the bit vreg in mask, if vreg is cached.
\ This word has an environmental dependency. It assumes, that one cell
\ has more than #USEREGS bits.
: (cached-mask) 				( -- mask )  
  0 #tos-cache 0 ?DO
    1 I cache@ LSHIFT
    OR
  LOOP ;
  
\ Find the first unrequested register in cache or REG-NONE if all are used.
: (unrequested) 				( -- vreg )
  (cached-mask)
  #free-req 0 ?DO
    1 I free@ LSHIFT OR
  LOOP
  #USEREGS 0 DO
    DUP 1 I LSHIFT AND 0= IF
      DROP I UNLOOP EXIT
    THEN
  LOOP
  DROP REG-NONE ;

\ flush the lowest cache slot to memory and return the free register
: (flushreg) 				( -- vreg )
  #tos-cache 1- cache@ DUP 		\ vreg vreg
  ( mov [ebp + offs + {#tc-1}*4], vreg )
  (vreg>reg) SZ-32 (#reg) 	
  BOFFS #tos-cache 1- CELLS offs-ebp + [ebp] mov,
  #tos-cache 1- TO #tos-cache ;
  
\ request a virtual register by number
\ n is tos + #reg-req
\ condition 				action
\ --------------------------------------------------------
\ free(vreg) & #rr<#tc 			xchg(vreg,tc(#rr))
\ free(vreg) & #rr=#tc 			load(vreg)
\ used(vreg) & #rr<#tc 			xchg(vreg,tc(#rr))
\ used(vreg) & #rr=#tc & free(vreg2) 	load(vreg2), xchg(vreg2,vreg)
\ used(vreg) & #rr=#tc & used(vreg2) 	flush(#ur-1), xchg( #ur-1, vreg)
: (#req) 				( vreg -- )
( OK )
  DUP (#used) IF 			\ vreg
    (#enough) IF
      tc(#rr) tc-xchg
    ELSE ( req. reg in use ) 		\ vreg
      (unrequested) 			\ vreg vreg2
      DUP REG-NONE = IF ( none free ) 		
        DROP (flushreg) 		\ vreg vreg2
      ELSE ( 1 free found )
        DUP (#load)
      THEN
      tc-xchg
    THEN
  ELSE 					\ vreg
    (#enough) IF
      tc(#rr) tc-xchg
    ELSE
      (unrequested) 			\ vreg vreg2
      DUP REG-NONE <> IF
        DUP (#load)
	tc-xchg
      ELSE
        DROP
	(flushreg) tc-xchg
      THEN
    THEN
  THEN (#rr++) ;

: req-eax VREG-EAX (#req) ;
: req-ebx VREG-EBX (#req) ;
: req-ecx VREG-ECX (#req) ;
: req-edx VREG-EDX (#req) ;
: req-edi VREG-EDI (#req) ;
: req-esi VREG-ESI (#req) ;

\ Is register eax, ebx, ecx or edx?
: (is-a-d) 				( vreg -- flag )
  DUP VREG-EAX = 
  OVER VREG-EBX = OR
  OVER VREG-ECX = OR
  SWAP VREG-EDX = OR ;
  
\ Request the register if unused.
: (#req-unused) 			( vreg -- ok? )
  DUP (#used) INVERT DUP 		\ vreg ok? ok?
  IF SWAP (#req) ELSE NIP THEN ;

\ Request any one of eax-edx. Check if one of them is not used. If so request
\ it else check all other requested below if they are eax-edx. If so request
\ it. Else error.
: req-a-d 					( -- )
  (#enough) IF
    #reg-req cache@ (is-a-d) IF
     (#rr++) EXIT
    THEN
  THEN
  VREG-EAX (#req-unused) IF EXIT THEN
  VREG-ECX (#req-unused) IF EXIT THEN
  VREG-EDX (#req-unused) IF EXIT THEN
  VREG-EBX (#req-unused) IF EXIT THEN
  #tos-cache #reg-req ?DO
    I cache@ DUP (is-a-d) 			\ vreg general?
    IF (#req) UNLOOP EXIT ELSE DROP THEN
  LOOP
  ." Can't request that many general registers." ABORT ;

\ request any virtual register
: req-any 				( -- )
  (#enough) INVERT IF
    (unrequested) 				\ vreg
\    DUP ." req-any: " . CR
    (#load)
  THEN (#rr++) ;

\ request any BUT the register vreg
\ n is tos + #reg-req
\ condition 				action
\ --------------------------------------------------------
\ enough, n<>vreg 			---
\ enough, n=vreg, free(vreg2) 		xchg(vreg,vreg2)
\ enough, n=vreg, none free, #rr<#ur-1  xchg(vreg,#ur-1)
\ enough, n=vreg, none free, #rr=#ur-1 	xchg(vreg,0)
\ not enough, used(vreg) 		load(vreg2)
\ not enough, free(vreg), free(vreg2) 	load(vreg2)
\ not enough, free(vreg), used(vreg2) 	xchg(vreg,vreg2), load(vreg2)
: (xchg-not) 				( vreg -- )
  (unrequested) 				\ vreg vreg2
  DUP REG-NONE <> IF
    tc-xchg
  ELSE
    #reg-req #USEREGS 1- = IF
      0 
    ELSE
      #USEREGS 1- 
    THEN
    cache@ tc-xchg
  THEN ;
  
: (#req-not) 				( vreg -- )
  (#enough) IF
    tc(#rr) 				\ vreg n
    OVER = IF 				\ vreg
      (xchg-not)
    ELSE
      DROP
    THEN
  ELSE 					\ vreg
    DUP (#used) IF 			\ vreg
      DUP
      (unrequested) DUP REG-NONE = 
      TooManyRegs 			\ vreg2
      (#load)
    ELSE 				\ vreg
      (unrequested) DUP REG-NONE <>
      IF 				\ vreg vreg2
        (#load) DROP
      ELSE
        TUCK 				\ vreg2 vreg vreg2
	tc-xchg (#load)
      THEN
    THEN
  THEN (#rr++) ;

: req-not-eax VREG-EAX (#req-not) ;

CREATE a-d-table VREG-EAX , VREG-EBX , VREG-ECX , VREG-EDX ,
: forall-a-d 4 0 ;

\ find the first unmarked register eax, ebx, ecx or edx
\ return nr or -1 if all marked
: (unmarked-a-d) 			( -- vreg )
  a-d-table
  forall-a-d DO 			\ addr
    DUP @ DUP (#marked) INVERT 		\ addr vreg mark
    IF
      NIP UNLOOP EXIT
    THEN
    DROP CELL+
  LOOP DROP REG-NONE ;

\ all eax-edx marked
: (a-d-marked) 				( -- flag )
  (unmarked-a-d) REG-NONE = ;

\ request a free register, but only eax, ebx, ecx or edx
\ cond 					action
\ ----------------------------------------------
\ a-d marked 				error
\ a-d req, s-d req 			flush
\ a-d unreq. 				mark
\ a-d req, s-d unreq 			swap, mark 			
: a-d-free 				( -- )
( OK )
  (a-d-marked) 
  IF ." Can't request this many general registers." ABORT THEN
  (unrequested) 
  DUP REG-NONE = IF 			\ vreg
    DROP (flushreg) 			\ vreg
  THEN
  DUP (is-a-d) IF 			\ vreg
    (mark-free)
  ELSE 					\ vreg-s-d 
    (unmarked-a-d) 			\ vreg-s-d vreg-a-d
    TUCK tc-xchg (mark-free)
  THEN ;

\ request a free register
\ cond 					action
\ ----------------------------------------------
\ all marked 				error
\ all requested 				error
\ all cached 				flush, mark
\ uncached(vreg) 			mark
: req-free 				( -- ) ( OK )
  #free-req #reg-req +
  #USEREGS = IF ." All registers requested." ABORT THEN
  (unrequested) 				\ vreg
  DUP REG-NONE = IF
     DROP (flushreg) 			\ vreg
  THEN
  (mark-free) ;

\ request a free register by number vreg 		TODO more efficient
\ cond 						action
\ ----------------------------------------------------
\ marked(vreg) 					error
\ unused(vreg) 					mark
\ unmarked(vreg), cached(vreg), unused(v2) 	swap, mark
\ unmarked(vreg), all cached, vreg=#ur-1 	flush, mark
\ unmarked(vreg), all cached, vreg<>#ur-1 	flush, swap, mark
: (#req-free) 				( vreg -- )
  DUP (#marked) IF ." Register is already marked." ABORT THEN
  DUP (#used) INVERT IF
    (mark-free)
  ELSE 					\ vreg
    (unrequested) 			\ vreg vreg2
    2DUP = IF (internal-error) THEN 	\ vreg vreg2
    DUP REG-NONE <> IF 			\ vreg vreg2
      SWAP TUCK tc-xchg 		\ vreg
      (mark-free)
    ELSE 				\ vreg vreg2
      DROP
      DUP (#find) 			\ vreg nr
      DUP -1 = IF (internal-error) THEN
      #USEREGS 1- OVER = IF
        (flushreg) 			\ vreg vreg
	2DUP <> IF (internal-error) THEN
	DROP (mark-free)
      ELSE
	(flushreg) 			\ vreg vreg2
	SWAP TUCK tc-xchg (mark-free)
      THEN
    THEN
  THEN ;

: free-eax VREG-EAX (#req-free) ;
: free-ecx VREG-ECX (#req-free) ;
: free-edx VREG-EDX (#req-free) ;
: free-edi VREG-EDI (#req-free) ;
: free-esi VREG-ESI (#req-free) ;

\ swap the vregs in tos+n1 and tos+n2
: tos-swap 				( n1 n2 -- )
  2DUP #reg-req < SWAP #reg-req < AND
  INVERT IF ." Too few registers requested." ABORT THEN
  2DUP 					\ n1 n2 n1 n2
  cache@ SWAP cache@ 			\ n1 n2 r2 r1
  ROT 					\ n1 r2 r1 n2
  cache! SWAP cache! ;

\ drop the tos+0
: (reg-free) 				( -- )
  #tos-cache DUP 0= IF ." No register in cache." ABORT THEN
  1- DUP TO #tos-cache
  0 
  BEGIN 					\ end curr
    DUP 1+ cache@ 			\ end curr vreg
    OVER cache! 				\ end curr
    1+ 2DUP <=
  UNTIL 2DROP 
  4 (add-ebp) ;

\ free n times the tos+0
: reg-free 				( n -- )
  0 ?DO
    (reg-free)
  LOOP ;

\ put the register in free-cache+n on top of stack
: free>tos 				( n  -- )
  DUP #free-req >= IF ." Too few free registers requested." ABORT THEN
  free@ 					\ vreg
  ( make space for register )
  #tos-cache BEGIN 			\ vreg i
    1- DUP 0< INVERT
  WHILE 					\ vreg i-1
    DUP cache@ OVER 1+ cache! 		
  REPEAT DROP
  0 cache!
  -4 (add-ebp)
  (#tc++) ;

\ reset register allocator ( at start of compiler word )
: regalloc-reset 			( -- )
  0 TO #reg-req 0 TO #free-req reset-labels ;

\ initialize register allocator ( at start of word compilation)
: regalloc-init 				( -- )
  1 TO #tos-cache
  VREG-EAX 0 cache! 
  0 TO offs-ebp ;

\ create a consistent state before ret/call
: regalloc-flush 			( -- )
  #tos-cache 0= IF
    req-eax
  ELSE
    \ make sure only reg is loaded
    BEGIN
      #tos-cache 1 <>
    WHILE
      (flushreg) DROP
    REPEAT
    \ make sure it is eax
    0 cache@ VREG-EAX <> IF
       0 cache@ VREG-EAX tc-xchg
    THEN
  THEN 
  offs-ebp IF
    offs-ebp ## ebp add,
  THEN
  regalloc-init ;

\ flush all registers to stack and correct ebp
: regalloc-flushjmp 			( -- )
  BEGIN
    #tos-cache 0<> 
  WHILE
    (flushreg) DROP
  REPEAT
  offs-ebp IF
    offs-ebp ## ebp add,
  THEN 0 TO offs-ebp ;

\ flush all except 2 registers to stack and correct ebp
: regalloc-flush-do 			( -- )
  BEGIN
    #tos-cache 2 > 
  WHILE
    (flushreg) DROP
  REPEAT
  offs-ebp -8 <> IF
    offs-ebp 8 + ## ebp add,
  THEN
  -8 TO offs-ebp 
  ;

\ access to meta-register
: (tosn) 				( n -- )
  DUP #tos-cache >= IF ." Request more registers." ABORT THEN
  cache@ (vreg>reg) SZ-32 (#reg) ;

: tos0 0 (tosn) ;
: tos1 1 (tosn) ;
: tos2 2 (tosn) ;
: tos3 3 (tosn) ;
: tos4 4 (tosn) ;
: tos5 5 (tosn) ;

: ([tosn]) 				( offs n -- )
  DUP #tos-cache >= IF ." Request more registers." ABORT THEN
  cache@ (vreg>reg) (#[reg]) ;

: [tos0] 0 ([tosn]) ;
: [tos1] 1 ([tosn]) ;
: [tos2] 2 ([tosn]) ;
: [tos3] 3 ([tosn]) ;
: [tos4] 4 ([tosn]) ;
: [tos5] 5 ([tosn]) ;
 
: (freen) 				( n -- )
  DUP #free-req >= IF ." Request more free registers." ABORT THEN
  free@ (vreg>reg) SZ-32 (#reg) ;

: free0 0 (freen) ;
: free1 1 (freen) ;
: free2 2 (freen) ;
: free3 3 (freen) ;
: free4 4 (freen) ;
: free5 5 (freen) ;

: ([freen]) 				( offs n -- )
  DUP #free-req >= IF ." Request more free registers." ABORT THEN
  free@ (vreg>reg) (#[reg]) ;
  
: [free0] 0 ([freen]) ;
: [free1] 1 ([freen]) ;
: [free2] 2 ([freen]) ;
: [free3] 3 ([freen]) ;
: [free4] 4 ([freen]) ;
: [free5] 5 ([freen]) ;

: (vreg>reg_l) 				( vreg -- )
  DUP (is-a-d) INVERT IF ." Can't get lower part of edi or esi." ABORT THEN
  CASE
    VREG-EAX OF reg-al ENDOF
    VREG-EDX OF reg-dl ENDOF
    VREG-ECX OF reg-cl ENDOF
    VREG-EBX OF reg-bl ENDOF
  ENDCASE ;

: (vreg>reg_h) 				( vreg -- )
  DUP (is-a-d) INVERT IF ." Can't get higher part of edi or esi." ABORT THEN
  CASE
    VREG-EAX OF reg-ah ENDOF
    VREG-EDX OF reg-dh ENDOF
    VREG-ECX OF reg-ch ENDOF
    VREG-EBX OF reg-bh ENDOF
  ENDCASE ;

\ n is the nr of the 32 bit virtual register
: (free_l) 				( n -- )
  DUP #free-req >= IF ." Request more free registers." ABORT THEN
  free@ (vreg>reg_l) ;

: (free_h) 				( n -- )
  DUP #free-req >= IF ." Request more free registers." ABORT THEN
  free@ (vreg>reg_h) ;
  
: free0l 0 (free_l) ;
: free1l 1 (free_l) ;
: free2l 2 (free_l) ;
: free3l 3 (free_l) ;
: free4l 4 (free_l) ;
: free5l 5 (free_l) ;
: free0h 0 (free_h) ;
: free1h 1 (free_h) ;
: free2h 2 (free_h) ;
: free3h 3 (free_h) ;
: free4h 4 (free_h) ;
: free5h 5 (free_h) ;

: (tos_l) 				( n -- )
  DUP #tos-cache >= IF ." Request more registers." ABORT THEN
  cache@ (vreg>reg_l) ;

: (tos_h) 				( n -- )
  DUP #tos-cache >= IF ." Request more registers." ABORT THEN
  cache@ (vreg>reg_h) ;

: tos0l 0 (tos_l) ;
: tos1l 1 (tos_l) ;
: tos2l 2 (tos_l) ;
: tos3l 3 (tos_l) ;
: tos4l 4 (tos_l) ;
: tos5l 5 (tos_l) ;
: tos0h 0 (tos_h) ;
: tos1h 1 (tos_h) ;
: tos2h 2 (tos_h) ;
: tos3h 3 (tos_h) ;
: tos4h 4 (tos_h) ;
: tos5h 5 (tos_h) ;

\ Scale index base addressing. All words are ( offs -- )
: [tos0+tos1]   SC-1 0 cache@ (vreg>reg) 1 cache@ (vreg>reg) (#[sib]) ;
: [4*tos0+tos1] SC-4 0 cache@ (vreg>reg) 1 cache@ (vreg>reg) (#[sib]) ;

\ ==============================================================================
\ =============================== compiler support =============================
\ ==============================================================================

\ Produce a near jump and put the address of the cell with the distance on the
\ stack. Due to the fact that these jumps are relative no relocation is
\ nessesary.
: fwd-jmp 				( xt -- addr )
  0 ## EXECUTE asm-here 4 - ;

\ Resolve the jump to this address.
: resolve-jmp 				( fwd-addr -- )
  asm-here 					\ addr here
  over 4 + - 				\ addr rel
  SWAP asm-! ;

\ Save the allocator state in the returned dyn-array.
: allocator-state 			( addr --  )
  #tos-cache OVER C! CHAR+ 		\ addr
  #tos-cache 0 ?DO 			\ addr
    I cache@  				\ addr reg
    OVER C! CHAR+
  LOOP 
  offs-ebp SWAP C!
  ;

\ Set the bit with the number vreg for each register vreg in the state.
: (state-mask) 				( state #regs -- mask )
  0 -ROT 0 ?DO 				\ mask state 
    DUP I CHARS + C@ 			\ mask state vreg
    1 SWAP LSHIFT 			\ mask state vrmask
    ROT OR SWAP 			\ mask state
  LOOP DROP ;

\ Find the lowest bit set in x and return it's number.
: lowest-bit 				( x -- nr )
  #USEREGS 0 DO 			\ x
    1 I LSHIFT OVER AND 		\ x reg?
    IF 					\ x
      DROP I UNLOOP EXIT
    THEN
  LOOP DROP REG-NONE ;

\ Try to load a register that is in state but not in cache. There must be at
\ least one of them since (alloc-load) is called only with fewer regs in cache
\ than in state.
: ((alloc-load)) 			( state #regs -- )
  ( find regs in cache )
  (cached-mask) 			\ state #regs cache-mask
  ( find regs in state )
  -ROT (state-mask) 			\ cache-mask state-mask
  ( leave all flags that are in state-mask AND NOT in cache-mask )
  SWAP INVERT AND 			\ load-mask
  DUP 0= IF (internal-error) THEN 	\ load-mask
  lowest-bit 				\ vreg
  (#load) ;

\ Load as many registers as nessesary. Try to use those that needed.
: (alloc-load) 				( state #regs -- state #regs )
  BEGIN
    #tos-cache 				\ state #regs #tc
    OVER 				\ state #regs #tc #regs
    <
  WHILE
    2DUP ((alloc-load))
  REPEAT 
  ;

\ Flush some register till the same number as in state in reached.
: (alloc-flush) 			( #regs -- #regs )
  BEGIN
    #tos-cache OVER 			\ #regs #tc #regs
    >
  WHILE
    (flushreg) DROP
  REPEAT ;

\ Exchange vreg and cache(ind) if nessesary.
: (alloc-adjust) 			( vreg ind -- )
  cache@ 				\ vreg creg
  2DUP = IF
    2DROP
  ELSE
    tc-xchg
  THEN ;

\ Perform sign extension from a byte to a cell.
: (sign-extend) 			( c -- n )
  DUP 128 AND 				\ c sign-bit
  0<> 255 INVERT AND OR
;

\ Retrieve the save offset and correct ebp if nessesary. If save-flags? is
\ true, the CPU flags are saved before and restored afterwards ( add could
\ change them).
: (fix-offs) 				( save-flags? state ind -- )
  CHARS + C@ (sign-extend) 		\ save? dest-offs
  offs-ebp OVER = IF
    2DROP
  ELSE 					\ save? dest-offs
    OVER IF pushf, THEN 		\ save? dest-offs
    offs-ebp OVER - 
    ## ebp add,
    TO offs-ebp 			\ save?
    IF popf, THEN
  THEN ;

\ Rebuild the allocator to the given state.  The index of the user data in
\ state is returned.
\ Algo:
\ 1. too few regs cached? -> load registers
\ 2. too many regs cached? -> flush registers
\ 3. exchange regs
: allocator-rebuild 			( save-flags? state -- )
  DUP C@ SWAP CHAR+ SWAP 		\ save? state #regs
  (alloc-load) 
  (alloc-flush) 			\ save? state #regs
  0 SWAP 				\ save? state ind #regs
  0 ?DO 				\ save? state ind
    2DUP CHARS + C@ 			\ save? state ind reg(ind)
    OVER (alloc-adjust) 		\ save? state ind
    1+
  LOOP 					\ save? state ind
  (fix-offs) ;

\ Store the state in the allocator without generating code.
: allocator-store 			( state -- )
  DUP C@ 				\ state #regs
  DUP TO #tos-cache
  1 CHARS SWAP 				\ state ind #regs
  0 ?DO 				\ state ind
    2DUP CHARS + C@ 			\ state ind vreg
    I cache! 				\ state ind
    CHAR+
  LOOP 					\ state ind
  + C@ (sign-extend) TO offs-ebp ;

