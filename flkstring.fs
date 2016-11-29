\  FLK cell counted string tools
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

\ $Id: flkstring.fs,v 1.8 1998/06/01 17:51:42 root Exp $
\ $Log: flkstring.fs,v $
\ Revision 1.8  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.7  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.6  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.5  1998/05/03 12:06:37  root
\ added macro support
\
\ Revision 1.4  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.3  1998/04/29 18:26:32  root
\ >UPPER removed (already in flkkern.fs)
\
\ Revision 1.2  1998/04/24 20:23:34  root
\ upper-case converter
\
\ Revision 1.1  1998/04/11 11:55:44  root
\ Initial revision
\

\ Allocate a piece of memory long enough to hold a cell-counted string
: $ALLOCATE 			( n -- addr )
  CHARS CELL+ ALLOCATE THROW
  DUP OFF
;

\ resize a dynamically allocated string
: $RESIZE 			( addr1 n -- addr2)
  CHARS CELL+ RESIZE THROW
;

\ copy a string to a stringvar 
: $copy 			( src len dst -- )
  2DUP 				\ src len dst len dst
  !
  CELL+ SWAP 			\ src dst+1 len
  MOVE
;

\ allocate and copy
: $ALLOCOPY 			( src slen -- addr )
  DUP $ALLOCATE DUP >R $copy R>
;

\ Append a string to a stringvar without resizing.
: $cat 				( addr len str -- )
  DUP 				\ s sl d d 
  @ 				\ s sl d dl
  PLUCK 			\ s sl d dl sl
  OVER + 			\ s sl d dl nl
  PLUCK 			\ s sl d dl nl d
  ! 				\ s sl d dl
  CHARS + CELL+ 		\ s sl de
  SWAP MOVE
;

\ Append a string to a stringvar and resize it.
: $resize-cat 			( str addr len -- )
( OK )
  ROT DUP @ 			\ addr len str slen
  PLUCK + 			\ addr len str total
  $RESIZE 			\ addr len str
  DUP -TURN $cat ;

\ print a the contents of an address as a cell-counted string
: ?$ 				( addr -- )
  @ ?DUP 0<> IF
    $COUNT TYPE
  THEN
;

10 CONSTANT LINESEP

\ Split a string at the first character c. The new strings don't contain
\ the separator. If no separator is found, the string s1 contains
\ the whole original string, the string s2 has a zero length and its address 
\ might be illegal.
: $split 			( str lstr c -- s1 ls1 s2 ls2 )
  0 				\ str lstr c ind 
  BEGIN
    PLUCK OVER 			\ str lstr c ind lstr ind
    >
  WHILE 				\ str lstr c ind
    3 PICK OVER 			\ str lstr c ind str ind
    CHARS + C@ 			\ str lstr c ind char
    PLUCK = IF 			\ str lstr c ind 
      NIP 			\ str lstr ind
      PLUCK 			\ str lstr ind str
      SWAP 2SWAP 		\ str ind str lstr
      PLUCK 1+ 			\ str ind str lstr ind+1
      ROT OVER + 		\ str ind lstr ind+1 str+ind+1
      -ROT 			\ str ind str+ind+1 lstr ind+1
      -
      EXIT 			\ str ind str+ind+1 lstr-(ind+1)
    THEN 			\ str lstr c ind
    1+
  REPEAT 			\ str lstr c ind
  2DROP 2DUP + 0
;

\ Print a string containing CRs by breaking in lines. Prepend the first line 
\ with s1 and any other lines with s2. The user is responsible that the lines
\ are short enough to fit into one line with the prepended string. It will
\ work when the TYPEd lines are longer than the physical line but it will
\ look bad.
: $.broken 			( s1 ls1 s2 ls2 str lstr -- )
  2ROT TYPE 			\ s2 ls2 str lstr 
  LINESEP $split 		\ s2 ls2 str1 lstr1 str2 lstr2
  2SWAP TYPE CR
  BEGIN 				\ s2 ls2 str lstr
    DUP 0<>
  WHILE
    2OVER TYPE
    LINESEP $split 		\ s2 ls2 str1 lstr1 str2 lstr2
    2SWAP TYPE CR
  REPEAT
  2DROP 2DROP
;

\ concat the strings and return the allocated result
: $allo-cat 			( s1 l1 s2 l2 -- addr )
  ROT 				\ s1 s2 l2 l1
  2DUP + $ALLOCATE 		\ s1 s2 l2 l1 addr
  >R >R ROT R> R> 		\ s2 l2 s1 l1 addr
  DUP >R $copy R@ 		\ s2 l2 addr
  $cat R>
;

