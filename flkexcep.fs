\  FLK exception handling
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

\ $Id: flkexcep.fs,v 1.11 1998/09/17 13:15:41 root Exp $
\ $Log: flkexcep.fs,v $
\ Revision 1.11  1998/09/17 13:15:41  root
\ added ?THROW
\
\ Revision 1.10  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.9  1998/07/03 20:57:50  root
\ level 2 optimimizer added
\
\ Revision 1.8  1998/06/05 20:02:10  root
\ forget corrected (chains)
\
\ Revision 1.7  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.6  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.5  1998/05/09 21:47:05  root
\ initializer and finisher added
\
\ Revision 1.4  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.3  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.2  1998/04/09 11:35:03  root
\ all words checked and OK
\
\ Revision 1.1  1998/04/07 20:10:33  root
\ Initial revision
\

\ THROW and CATCH are the same as in the standard. CATCH is a bit optimized to
\ throw a value if the xt is illegal.
VARIABLE THROW-HANDLER 
-256 CONSTANT TODO-EXCEPTION 
-257 CONSTANT ILLEGAL-IMMEDIATE-EXCEPTION
-258 CONSTANT LIT-CACHE-EXCEPTION
-259 CONSTANT RB-E-UNDERFLOW
-260 CONSTANT RB-E-OVERFLOW
-261 CONSTANT RB-E-RANGE

\ See standard.
: CATCH 				( xt -- exception# | 0 )
( OK )
  ( fake EXECUTE )
  >CFA @ DUP 				\ call call
  IMAGE-BASE = IF ( invalid ) 		\ call 
    DROP -14 		 		\ excep
  ELSE 					\ call
    SP@ 				\ call sp 
    >R         				\ call / r: sp
    FSP >R 				\ call / r: sp fsp
    THROW-HANDLER @ >R 			\ call / r: sp fsp oh
    RP@ 				\ call rp
    THROW-HANDLER !   			\ call
    (EXECUTE)
    ( no throw happended )
    R> 					\ oh / r: sp fsp
    THROW-HANDLER !   
    R> DROP 				\ / r: sp
    R> DROP        			\ / r:
    0 
  THEN
  ;

\ See standard.
: THROW   				( ??? exception# -- ??? exception# )
( OK )
  ?DUP IF 				\ xc 
    THROW-HANDLER @ 	 		\ xc handler / r: 
    RP!   				\ xc / r: sp fsp oh
    R> 					\ xc oh / r: sp
    THROW-HANDLER ! 			\ xc / r: sp fsp
    R> TO FSP 				\ xc / r: sp
    R> 					\ xc sp / r: 
    SWAP 				\ sp xc / r:
    >R 					\ sp / r: xc
    SP! 				\ xt / r: xc
    DROP 				\ / r: xc
    R> 					\ xc
  THEN ;

\ See standard.
: ABORT -1 THROW ;
( OK )

\ Throw a special exception to indicate an unfinished part.
: TODO TODO-EXCEPTION THROW ;
( OK )

\ Throw an exception if flag is TRUE.
: ?THROW ( flag code -- ) SWAP IF THROW THEN DROP ;

\ See standard.
: EXECUTE 				( xt -- )
( OK )
  >CFA @ DUP  				\ call call
  IMAGE-BASE = 				\ call invalid?
  IF -14 THROW THEN 			\ call
  (EXECUTE) ;

\ No operation. Does nothing. Has no effect. Performs no task. Ignores you. It
\ contains an end-of-chain marker in it's dfa.
: NOOP ;
0 r,

' NOOP RVALUE initializer
' NOOP RVALUE finisher

\ See standard.
: BYE finisher EXECUTE (BYE) ;

