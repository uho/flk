\  FLK dynamic array 
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

\ $Id: array.fs,v 1.6 1998/08/30 10:50:59 root Exp $
\ $Log: array.fs,v $
\ Revision 1.6  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.5  1998/06/03 07:55:16  root
\ minor bug fixes
\
\ Revision 1.4  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.3  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.2  1998/05/09 21:47:05  root
\ []-delete added
\
\ Revision 1.1  1998/05/01 18:11:25  root
\ Initial revision
\

\ These dynamic arrays originate from a C++ class I use for some time now.
\ I found them to be faster, easier, more reliable and more useful than e.g.
\ linked lists. 
\ You can append items to an dynamic array, shrink it and access items by
\ index with the same speed as arrays in the data space. Only the reallocation
\ takes some time therefore size is doubled if you access it linearily. If you
\ know how many items you know, store a zero into the last one to save all the
\ re-allocations before you start fill the slots. 
\ Since this file contains only standard words it can be used independendly
\ from FLK.

\ A dynamic array contains the number of entries in the second cell and the
\ number of items allocated in the first cell. 
2 CELLS CONSTANT ARRAY-HEADER-LEN
-4097 CONSTANT ARRAY-INDEX-EXCEPTION

\ Create an empty array.
: []( 	 				( -- arr )
  ARRAY-HEADER-LEN ALLOCATE THROW  	\ arr
  0 OVER !
  0 OVER CELL+ ! 
  ;

\ Close the array (free it).
: )[] 					( arr -- )
  FREE THROW ;

\ Return the number of items in the array.
: []# 					( arr -- cnt )
  CELL+ @ ;

\ Reallocate the array so that 2*ind+1 items fit into it before the next
\ reallocation is nessesary and return the index and the new array pointer.
: (realloc-array) 			( arr ind -- narr ind )
  DUP 1+ 2*  				\ arr ind nalloc
  ROT 					\ ind nalloc arr
  OVER CELLS ARRAY-HEADER-LEN + 	\ ind nalloc arr nmem
  RESIZE THROW 				\ ind nalloc narr
  TUCK ! 				\ ind narr
  SWAP ; 

\ Reallocate the array and store the new size in it.
: (resize-array) 			( arr ind  -- )
  1+ SWAP CELL+ ! ;

\ Grow the array that it can hold at least ind+1 entries and store cont at
\ position ind in the array.
: []! 					( arr ind cont -- arr )
  >R 					\ arr ind
  OVER @ 				\ arr ind alloc
  OVER U> INVERT IF 
    (realloc-array)
  THEN 					\ arr ind
  OVER []# 				\ arr ind cnt
  OVER U> INVERT IF 			\ arr ind
    2DUP (resize-array)
  THEN 					\ arr ind
  CELLS ARRAY-HEADER-LEN + 		\ arr offs
  OVER + 				\ arr addr
  R> SWAP ! ;

\ Return the address of the cell with the index ind or throw an exception if
\ there is no such index.
: []& 					( arr ind -- addr )
  OVER CELL+ @ 				\ arr ind cnt
  OVER U> INVERT IF 
    ARRAY-INDEX-EXCEPTION THROW 
  THEN
  CELLS ARRAY-HEADER-LEN + + ;
  
\ Return the value at index ind.
: []@ 					( arr ind -- cont )
  []& @ ;

\ Append one item to the array.
: []+= 					( arr cont -- arr )
  OVER []# 				\ arr cont ind
  SWAP []! ;

\ Shrink array so that it contains at most #new items afterwards.
: []-truncate 				( arr #new -- )
  OVER []# 				\ arr #new #this
  OVER 					\ arr #new #this #new
  > IF 					\ arr #new
    SWAP CELL+ !
  ELSE 					\ arr #new
    2DROP
  THEN ;

\ Delete the item at the givenm index by moving the last one there.
: []-delete 				( arr ind -- )
( OK )
  OVER DUP []# 1- []@ 			\ arr ind lastcont
  PLUCK ROT 				\ arr lc arr ind
  []& ! 				\ arr 
  CELL+ -1 SWAP +! ;

