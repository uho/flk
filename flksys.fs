\ FLK system calls
\
\ Copyright (C) 1998 Lars Krueger 
\
\ This file is part of FLK.
\
\ FLK is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License
\ as published by the Free Software Foundation; either version 2
\ of the License, or (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
\ GNU General Public License for more details.
\
\ You should have received a copy of the GNU General Public License
\ along with this program; if not, write to the Free Software
\ Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

\ $Id: flksys.fs,v 1.13 1998/06/03 07:55:16 root Exp $
\ $Log: flksys.fs,v $
\ Revision 1.13  1998/06/03 07:55:16  root
\ renamed (sys-call) to (loader-call) in preparation to Linux system calls
\
\ Revision 1.12  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.11  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.10 1998/05/23 17:52:02 root
\ background processing
\
\ Revision 1.9 1998/05/16 16:19:24 root
\ direct terminfo access
\
\ Revision 1.8 1998/05/09 21:47:05 root
\ library support
\
\ Revision 1.7 1998/05/01 18:11:25 root
\ GNU license text added
\ comments checked
\
\ Revision 1.6 1998/04/30 09:42:25 root
\ Comments added.
\
\ Revision 1.5 1998/04/29 18:20:30 root
\ SYSTEM, FLASH, BELL
\
\ Revision 1.4 1998/04/27 18:41:42 root
\ directory primitves
\
\ Revision 1.3 1998/04/10 14:42:50 root
\ bugs corrected
\
\ Revision 1.2 1998/04/09 11:35:03 root
\ all words checked and OK
\
\ Revision 1.1 1998/04/07 20:10:33 root
\ Initial revision
\

\ Compile a complete word in targetspace to call the loader primitive prim
\ with in stack items consumend and out stack items produced. A version to
\ compile a call to this word is also generated.
: (loader-call)				( in out prim -<name>- )
( OK )
 BL PARSE				\ in out prim addr len
 2DUP >R >R
 FALSE (namedHeader)			\ in out prim
 (asm-reset)
 eax 0 [ebp] mov,			( flush tos )
 ebp eax mov,				( remember address of tos )
 HA-LOADER #[] ebx mov,		( load address of system primitive )
 [ISEM] CELLS [PREVIOUS] 
 ## ebx add,
 0 [ebx] ebx mov,
 ebp HA-SAVE-F-EBP #[] mov,		( save forth stack pointers )
 esp ebp mov,
 ebp HA-SAVE-F-ESP #[] mov,
 HA-SAVE-C-ESP #[] ebp mov,		( load c stack pointers) 
 ebp esp mov,
 HA-SAVE-C-EBP #[] ebp mov,
 eax push,				( address of data tos )
 ebx call,				( call system )
 HA-SAVE-F-ESP #[] ebp mov,		( restore forth stack pointers)
 ebp esp mov,
 HA-SAVE-F-EBP #[] ebp mov,		\ in out
 - [ISEM] CELLS [PREVIOUS] 
 ## ebp add,		 		( consume and produce )
 0 [ebp] eax mov,			( cache tos )
 cld,
 ret,					( go on forthing )
 R> R>					\ addr len cfa
 (inline-call) ; 

\ See standard.
1 0 0 (loader-call) EMIT ( x -- )

\ Primitve version of KEY. Gets keys until one is an ascii character.
0 1 1 (loader-call) (KEY) ( -- char )

\ See standard
2 0 2 (loader-call) TYPE ( c-addr n -- )

\ See standard
0 0 3 (loader-call) CR ( -- )

\ See standard
2 0 4 (loader-call) AT-XY ( u1 u2 -- )

\ See standard
0 1 5 (loader-call) KEY? ( -- flag )

\ See standard
0 0 6 (loader-call) PAGE ( -- )

\ Primitive version of (EKEY). Reads a key and returns it.
0 1 7 (loader-call) (EKEY) ( -- u )

\ See standard
1 2 8 (loader-call) EKEY>CHAR ( u -- u false | char true )

\ See standard
0 1 9 (loader-call) EKEY? ( -- flag )

\ See standard
0 1 10 (loader-call) EMIT? ( -- flag )

\ See standard
1 0 11 (loader-call) MS ( u -- )

\ See standard
0 6 12 (loader-call) TIME&DATE ( -- sec min hr day mn yr )

\ See standard
1 2 13 (loader-call) ALLOCATE ( u -- a-addr ior )

\ See standard
1 1 14 (loader-call) FREE ( u -- ior )

\ See standard
2 2 15 (loader-call) RESIZE ( addr u -- addr2 ior )

\ Primitive version of BYE. Shuts down the terminal and exits.
0 0 16 (loader-call) (BYE) ( -- )

\ See standard
1 1 17 (loader-call) CLOSE-FILE ( fid -- ior )

\ See standard
3 2 18 (loader-call) CREATE-FILE ( addr u mode -- fid ior )

\ See standard
2 1 19 (loader-call) DELETE-FILE ( addr n -- ior )

\ See standard
1 3 20 (loader-call) FILE-POSITION ( fid -- ud ior )

\ See standard
1 3 21 (loader-call) FILE-SIZE ( fid -- ud ior )

\ See standard
3 2 22 (loader-call) OPEN-FILE ( addr u mode -- fid ior )

\ See standard
3 2 23 (loader-call) READ-FILE ( addr u fid -- u2 ior )

\ See standard
3 3 24 (loader-call) READ-LINE ( addr u fid -- u2 flag ior )

\ See standard
3 1 25 (loader-call) REPOSITION-FILE ( ud fid -- ior )

\ See standard
3 1 26 (loader-call) RESIZE-FILE ( ud fid -- ior )

\ See standard
4 1 27 (loader-call) RENAME-FILE ( a1 u1 a2 u2 -- ior )

\ See standard
3 1 28 (loader-call) WRITE-FILE ( addr u fid -- ior )

\ See standard
3 1 29 (loader-call) WRITE-LINE ( addr u fid -- ior )

\ See standard
2 2 30 (loader-call) FILE-STATUS ( addr u -- fstat ior )

\ See standard
1 1 31 (loader-call) FLUSH-FILE ( fid -- ior )

\ Return rows on screen.
0 1 32 (loader-call) ROWS ( -- rows )

\ Return columns on screen.
0 1 33 (loader-call) COLS ( -- cols )

\ Find first file caddr2,u2 in directory caddr1,u1. flag is FALSE for
\ failure..
2 3 34 (loader-call) FIND-FIRST ( caddr1 u1 -- caddr2 u2 flag )

\ Find next file in the last directory searched. flag is FALSE for failure..
0 3 35 (loader-call) FIND-NEXT ( -- caddr u flag )

\ Decide whether or not the fstat describes a directory. fstat is produced by
\ FILE-STATUS.
1 1 36 (loader-call) DIRECTORY? ( fstat -- flag )

\ Ring the bell.
0 0 37 (loader-call) BELL ( -- )

\ Flash the screen.
0 0 38 (loader-call) FLASH ( -- )

\ Execute the given command string in a shell. Retval is the exit code of the
\ command.
2 1 39 (loader-call) SYSTEM ( addr len -- retval )

\ Return the current time of day with the best resolution possible.
0 2 40 (loader-call) TIME_OF_DAY ( -- sec usec )

\ Opens a shared object and returns its handle.
3 1 41 (loader-call) OPENLIB ( addr len flag -- lib )

\ Returns a zero terminated string containing the most recent error message.
0 1 42 (loader-call) LIBERROR ( -- cstring )

\ Searches the named symbol in the given library and returns the entry point
\ or start of data space.
3 1 43 (loader-call) LIBSYMBOL ( lib addr len -- fct )

\ Closes a library.
1 0 44 (loader-call) CLOSELIB ( lib -- )

\ Returns the array of boolean terminal capabilities. Size of items: char
0 1 45 (loader-call) TERM-BOOL ( -- addr )

\ Return the array of numeric terminal capabilities. Size of items: short
0 1 46 (loader-call) TERM-NUMBER ( -- addr )

\ Return the array of string terminal capabilities: Size of items: cell
0 1 47 (loader-call) TERM-STRING ( -- addr )

\ Return the current work directory.
0 1 48 (loader-call) GET-CWD ( -- addr )

