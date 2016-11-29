\  FLK environment constants
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

\ $Id: flkenv.fs,v 1.5 1998/05/09 21:47:05 root Exp $
\ $Log: flkenv.fs,v $
\ Revision 1.5  1998/05/09 21:47:05  root
\ fixed stack size words
\
\ Revision 1.4  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.3  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.2  1998/04/24 16:47:39  root
\ ENVIRONMENT-WORDLIST is now a normal wordlist
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ Environmental query strings. All wordsets created by the metacompiler are
\ handled below to keep everything in one place.
ALSO ENVIRONMENT DEFINITIONS

: /COUNTED-STRING 255 ;
: /HOLD ((pnolen)) ;
: /PAD #PAD-LEN ;
: ADDRESS-UNIT-BITS 8 ;
: CORE TRUE ;
: CORE-EXT TRUE ;
: FLOORED FALSE ;
: MAX-CHAR 255 ;
: MAX-D 4294967295 2147483647 ;
: MAX-N 2147483647 ;
: MAX-U 4294967295 ;
: MAX-UD 4294967295 4294967295 ;

: RETURN-STACK-CELLS HA-CALL-CELLS @ ;
: DATA-STACK-SIZE HA-DATA-CELLS @ ;

: DOUBLE ;
: DOUBLE-EXT ;

: EXCEPTION ;
: EXCEPTION-EXT ;

: FACILITY ;
: FACILITY-EXT ;
	
: FILE ;
: FILE-EXT ;

: MEMORY-ALLOC ;
: MEMORY-ALLOC-EXT ;

: TOOLS ;
: TOOLS-EXT ;

: SEARCH-ORDER ;
: SEARCH-ORDER-EXT ;
: WORDLISTS #IN-ORDER ;

: STRING ;
: STRING-EXT ;
	  
PREVIOUS DEFINITIONS

