\  FLK vectored execution
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

\ deferred words                                              Lars Krueger 1997

\ Stores the given string into the dictionary (character count).
: ,C					( ca n -- )
  255 MIN
  DUP C, 0 ?DO				\ ca
    DUP C@ C, CHAR+ 
  LOOP DROP ;
  
\ Creates the word name for vectored execution. To assign a function to name,
\ use TO name with the xt of the word to assign on the stack.
: DEFER 				( -<name>- )
  SAVE-INPUT 
  BL PARSE >R >R
  RESTORE-INPUT 
  ABORT" Could not restore input."	\ a n
  R> R>
  CREATE IMAGE-BASE r, ,C
  DOES> DUP @ 				\ a xt
  DUP IMAGE-BASE = IF			\ a xt
    DROP CELL+				\ a
    ." Uninitialized use of "
    COUNT TYPE ." ." CR 
    TRUE ABORT" Error."
  ELSE
    NIP EXECUTE
  THEN ;

