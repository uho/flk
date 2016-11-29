\ FLK exception creator
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

\ $Id: flkhiex.fs,v 1.2 1998/08/30 10:50:59 root Exp $
\ $Log: flkhiex.fs,v $
\ Revision 1.2  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.1  1998/07/18 10:49:59  root
\ Initial revision
\

\ Define an exception text for the given nr and lik it into the exception text
\ list. Each node has the following structure.
\ Offset	Meaning
\ 0		next
\ 1 cell 	exxception nr
\ 2 cells 	cell counted string 
: #exception 			( addr len nr -- )
  HERE 				\ addr len nr node
  (exception-list) r, 		\ addr len nr node
  TO (exception-list) ,		\ addr len 
  ,S ;

-4096 VALUE (exception-nr)

\ Define an exception with decreasing numbers.
: exception 			( addr len -- ex-nr )
  (exception-nr) DUP -TURN 	\ nr addr len nr
  #exception
  TO-- (exception-nr) ;

\ Define standard exception texts
S" stack overflow"                                   -3  #exception
S" stack underflow"                                  -4  #exception
S" return stack overflow"                            -5  #exception
S" return stack underflow"                           -6  #exception
S" do-loops nested too deeply during execution"      -7  #exception
S" dictionary overflow"                              -8  #exception
S" invalid memory address"                           -9  #exception
S" division by zero"                                -10  #exception
S" result out of range"                             -11  #exception
S" argument type mismatch"                          -12  #exception
S" undefined word"                                  -13  #exception
S" interpreting a compile-only word"                -14  #exception
S" invalid FORGET"                                  -15  #exception
S" attempt to use zero-length string as a name"     -16  #exception
S" pictured numeric output string overflow"         -17  #exception
S" parsed string overflow"                          -18  #exception
S" definition name too long"                        -19  #exception
S" write to a read-only location"                   -20  #exception
S" unsupported operation"                           -21  #exception
S" control structure mismatch"                      -22  #exception
S" address alignment exception"                     -23  #exception
S" invalid numeric argument"                        -24  #exception
S" return stack imbalance"                          -25  #exception
S" loop parameters unavailable"                     -26  #exception
S" invalid recursion"                               -27  #exception
S" user interrupt"                                  -28  #exception
S" compiler nesting"                                -29  #exception
S" obsolescent feature"                             -30  #exception
S" >BODY or DOES> used on non-CREATEd definition"   -31  #exception
S" invalid name argument"                           -32  #exception
S" block read exception"                            -33  #exception
S" block write exception"                           -34  #exception
S" invalid block number"                            -35  #exception
S" invalid file position"                           -36  #exception
S" file I/O exception"                              -37  #exception
S" non-existent file"                               -38  #exception
S" unexpected end of file"                          -39  #exception
S" invalid BASE for floating point conversion"      -40  #exception
S" loss of precision"                               -41  #exception
S" floating-point divide by zero"                   -42  #exception
S" floating-point result out of range"              -43  #exception
S" floating-point stack overflow"                   -44  #exception
S" floating-point stack underflow"                  -45  #exception
S" floating-point invalid argument"                 -46  #exception
S" compilation word list deleted"                   -47  #exception
S" invalid POSTPONE"                                -48  #exception
S" search-order overflow"                           -49  #exception
S" search-order underflow"                          -50  #exception
S" compilation word list changed"                   -51  #exception
S" control-flow stack overflow"                     -52  #exception
S" exception stack overflow"                        -53  #exception
S" floating-point underflow"                        -54  #exception
S" floating-point unidentified fault"               -55  #exception
S" QUIT (execute it yourself)"                      -56  #exception
S" exception in sending or receiving a character"   -57  #exception
S" [IF], [ELSE], or [THEN] exception"               -58  #exception

\ System exceptions
S" Unimplemented feature used."          TODO-EXCEPTION              #exception
S" IMMEDIATE without colon definfition." ILLEGAL-IMMEDIATE-EXCEPTION #exception
S" Invalid index for dynamic array."     ARRAY-INDEX-EXCEPTION       #exception
S" Accessing empty literal cache."       LIT-CACHE-EXCEPTION         #exception
S" Ring buffer underflow."               RB-E-UNDERFLOW              #exception
S" Ring buffer overflow."                RB-E-OVERFLOW               #exception
S" Ring buffer range check error."       RB-E-RANGE                  #exception

