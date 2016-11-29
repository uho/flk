\  FLK level 2 optimizer (target only)
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
\ $Id: flktopt.fs,v 1.3 1998/09/13 18:55:51 root Exp $
\ $Log: flktopt.fs,v $
\ Revision 1.3  1998/09/13 18:55:51  root
\ fixed optimizers for cf stack
\
\ Revision 1.2  1998/08/30 10:50:59  root
\ new optimizing algorithm
\
\ Revision 1.1  1998/07/13 18:08:54  root
\ Initial revision
\

opt( ''# '' CHAR- )opt: ['] 1-    (_sizing) ;opt

opt( '' DUP '' +LOOP )opt: 		( -- )
  CFT-do (check-cs-item)
  regalloc-reset
  req-any
  req-free
  free0 pop,
  tos0 free0 add,
  free0 push,
  TRUE do-state allocator-rebuild
  (curr-cf-item) 3 CELLS + @ 		\ jmp-to
  ## n-jno,
  (resolve-leave)
  8 ## esp add, 
  (curr-cf-item) 4 CELLS + @ ?DUP IF
    resolve-jmp
  THEN 
  (delete-cs-item)
  0 2 opt-remove
;opt

