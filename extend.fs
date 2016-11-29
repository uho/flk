\  FLK kernel extension
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

\ $Id: extend.fs,v 1.13 1998/07/18 10:49:59 root Exp $
\ $Log: extend.fs,v $
\ Revision 1.13  1998/07/18 10:49:59  root
\ added flkkhiex.fs
\
\ Revision 1.12  1998/06/01 17:51:42  root
\ SEE shows the sourcefile using VIEW
\
\ Revision 1.11  1998/05/27 18:52:12  root
\ \: commants added for SEE and HELP
\
\ Revision 1.10  1998/05/23 17:52:02  root
\ library support
\
\ Revision 1.9  1998/05/17 08:27:09  root
\ script mode, ODOES>
\
\ Revision 1.8  1998/05/16 16:19:24  root
\ direct terminfo access
\
\ Revision 1.7  1998/05/09 21:47:05  root
\ files re-arranged
\
\ Revision 1.6  1998/05/01 18:11:25  root
\ GNU license text added
\ comments checked
\
\ Revision 1.5  1998/04/30 09:42:25  root
\ Comments added.
\
\ Revision 1.4  1998/04/24 16:47:39  root
\ changed to win32forth-like floating support
\
\ Revision 1.3  1998/04/15 18:15:30  root
\ float support
\
\ Revision 1.2  1998/04/11 11:55:44  root
\ FORGET/MARKER added
\
\ Revision 1.1  1998/04/10 14:42:50  root
\ Initial revision
\

S" flkhiex.fs" INCLUDED
S" defer.fs" INCLUDED
S" flkenv.fs" INCLUDED
S" flktools.fs" INCLUDED  
S" flkfloat.fs" INCLUDED
S" flklib.fs" INCLUDED

S" flk" SAVE-SYSTEM
S" default.flk" SAVE-SYSTEM
BYE

