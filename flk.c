/* FLK loader program

  Copyright (C) 1998 Lars Krueger 

  This file is part of FLK.

  FLK is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/* 
 * $Id: flk.c,v 1.25 1998/08/30 10:50:59 root Exp $
 *****************************************************************************
 * $Log: flk.c,v $
 * Revision 1.25  1998/08/30 10:50:59  root
 * added diagnostics
 *
 * Revision 1.24  1998/07/15 16:38:23  root
 * TERM=dumb bugss removed
 *
 * Revision 1.23  1998/07/09 19:16:41  root
 * fixed relocation to enable code size changes
 *
 * Revision 1.22  1998/06/24 05:30:46  root
 * corrected terminal setup and cleanup
 *
 * Revision 1.21  1998/06/03 07:55:16  root
 * corrected flush on exit
 *
 * Revision 1.20  1998/06/01 18:05:39  root
 * changed image search order (argv[0] doesn't contain a path)
 *
 * Revision 1.19  1998/06/01 17:51:42  root
 * GET-CWD added
 *
 * Revision 1.18  1998/05/24 18:43:16  root
 * delayed flush corrected
 *
 * Revision 1.17  1998/05/24 15:41:26  root
 * Delayed flush can be turned off
 *
 * Revision 1.16  1998/05/23 19:23:54  root
 * delayed flushing of stdout
 *
 * Revision 1.15  1998/05/21 19:24:49  root
 * XForms support
 *
 * Revision 1.14  1998/05/17 08:27:09  root
 * script mode, ODOES>
 *
 * Revision 1.13  1998/05/16 16:19:24  root
 * direct terminfo access
 *
 * Revision 1.12  1998/05/09 21:47:05  root
 * S, renamed to ,C
 * ,S included
 *
 * Revision 1.11  1998/05/02 15:05:55  root
 * CREATE-FILE corrected
 *
 * Revision 1.10  1998/05/01 18:13:33  root
 * Image search order changed to:
 * executable, ./flk.flk, default.flk
 *
 * Revision 1.9  1998/05/01 18:11:25  root
 * GNU license text added
 * comments checked
 *
 * Revision 1.8  1998/04/30 09:42:25  root
 * other image search order
 *
 * Revision 1.7  1998/04/29 18:26:32  root
 * SYSTEM, BELL, ALERT
 * TYPE fixed (was printf like, now puts like)
 *
 * Revision 1.6  1998/04/27 18:41:42  root
 * directory primitives
 *
 * Revision 1.5  1998/04/25 11:03:28  root
 * added flk.c to check in list
 *
 * Revision 1.4  1998/04/11 11:58:14  root
 * REPOSITION-FILE checked
 *
 * Revision 1.3  1998/04/10 14:42:50  root
 * new header format to ease SAVE-SAYSTEM
 *
 * Revision 1.2  1998/04/07 20:10:33  root
 * final .flk format, OS calls, image search
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#ifdef __UNIX__
#include <curses.h>
#include <dirent.h>
#include <sys/stat.h>
#include <dlfcn.h>
#include <term.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/mman.h>
static DIR * glob_dir=0;
static struct dirent * glob_dirent=0;
static struct termios shell_ios;
static struct termios prog_ios;

#ifdef DELAY_FLUSH
#define FLUSHBUFFERLEN 5000
static char flushBuffer[FLUSHBUFFERLEN];
static unsigned flushBufferInd;

static void myFlush()
{
  write( 1, flushBuffer, flushBufferInd);
  flushBufferInd=0;
}

static void myputp( const char * s)
{
  unsigned l;
  assert( s);
  l=strlen(s);
  if( l+flushBufferInd>=FLUSHBUFFERLEN)
  {
    signal( SIGALRM, SIG_IGN);
    myFlush();
  }
  memcpy( flushBuffer+flushBufferInd, s, l);
  flushBufferInd+=l;
}

static void flushHandler( int sig)
{
  myFlush();
}

static void startFlusher() 
{
  if( flushBufferInd)
  {
    struct itimerval to;
    to.it_value.tv_sec=0;
    to.it_value.tv_usec=40000;
    to.it_interval.tv_sec=0;
    to.it_interval.tv_usec=0;
    setitimer( ITIMER_REAL, &to, NULL);
    signal( SIGALRM, flushHandler);
  }
}
#else
#define myputp putp
#endif

#endif

#ifdef MSDOS
#include <conio.h> 
#include <i86.h> 

#define COLS  (*(unsigned short *)0x44a)
#define LINES ((*(unsigned short *)0x44c)/COLS)
#define CSRPOSy (*(unsigned char *)0x450)
#define CSRPOSx (*(unsigned char *)0x451)

static void gotoxy( int x, int y)
{
  union REGPACK regs;
  regs.h.ah=2;
  regs.h.bh=0;
  regs.h.dh=y;
  regs.h.dl=x;
  intr( 0x10, &regs);
}

static void clrscr()
{
  union REGPACK regs;
  regs.h.ah=6;
  regs.h.al=0;
  regs.w.cx=0;
  regs.h.dh=LINES-1;
  regs.h.dl=COLS-1;
  intr( 0x10, &regs);
}

#define getsyx( y,x) {y=CSRPOSy; x=CSRPOSx;}

#endif

/*==============================global constants==============================*/

static const unsigned char headerID[]="FLK Image 1.0";
#define HEADER_ID_LEN (sizeof( headerID)-1)

#define FTRUE ((unsigned)(~0))

#define STACK unsigned _stptr_=0
#define POP( var,erg) {erg=stack[_stptr_].var; _stptr_++;}
#define PUSH( var, val) {_stptr_--; stack[_stptr_].var=val;}
#define POPSTRING(erg) { unsigned len; char * s; POP( uval,len); POP( caddr, \
    s); erg=makeCString( s, len); }
#define DPUSH(x) { PUSH( nval,x); PUSH( nval, (x<0)?-1:0); }
#define UDPUSH(x) { PUSH( uval,x); PUSH( uval,0); }

/*==============================global variables==============================*/
static unsigned * ReloTable;
static unsigned * ForthDataStack, * ForthCallStack;
static unsigned * Image;
static unsigned MemorySize=0;

/*==============================static functions==============================*/
/* This could be done more eloquently, right? */
static void printErrorBanner()
{
  printf("FLK Error: ");
}

char * makeCString( char * fstr, unsigned len)
{
  char * res=(char*)malloc(len+1);
  assert( res);
  memcpy( res,fstr,len);
  res[len]=0;
  return res;
}

static void setupTerm()
{
#ifdef __UNIX__
  setupterm(NULL, 1, NULL);
  def_shell_mode();
  tcgetattr( 1, &shell_ios);
  prog_ios=shell_ios;
  prog_ios.c_lflag&=~(ICANON|ECHO);
  prog_ios.c_iflag&=~(ICRNL|INLCR);
  tcsetattr( 1, TCSANOW, &prog_ios);
  tcflow (1, TCOON);
  def_prog_mode();
#ifdef DELAY_FLUSH
  flushBufferInd=0;
#else
  setbuf(stdout,0);
#endif
#endif
#ifdef MSDOS

#endif
}

static void cleanupTerm()
{
#ifdef __UNIX__
  if( glob_dir)
    closedir( glob_dir);
  reset_shell_mode();
  tcsetattr( 1, TCSADRAIN, &shell_ios);
#ifdef DELAY_FLUSH
  signal( SIGALRM, SIG_IGN);
  myFlush();
#endif
#endif
#ifdef MSDOS
 
#endif
}

/*==============================system primitives=============================*/

typedef union 
{
  unsigned uval;
  int nval;
  char * caddr;
  void * vaddr;
} Cell;

typedef void(*SystemPrimitive)( Cell * stack);

void EMIT( Cell * stack) /* x -- */
{
  STACK;
  int x;
  char xx[2];
  POP( nval, x);
#ifdef __UNIX__
  switch( x)
  {
    case -1:/* backspace */
      if( !cursor_left)
	myputp( "\010");
      else
	myputp( cursor_left);
      break;
    default: 
      xx[0]=x;
      xx[1]=0;
      myputp(xx);
      break;      
  }
#ifdef DELAY_FLUSH
  startFlusher();
#endif
#endif
#ifdef MSDOS
  putch(x);
#endif
}

void KEY( Cell * stack) 			/* -- char */
{
  STACK;
  int key;
  do
  {
    key=getchar();
  }while(!(32<=key && key<127));
  PUSH(nval,key);
}

void TYPE( Cell * stack)   /* c-addr n  --  */
{
  STACK;
  char * str;
  POPSTRING(str);
#ifdef __UNIX__
  myputp(str);
#ifdef DELAY_FLUSH
  startFlusher();
#endif
#endif
#ifdef MSDOS
  cputs(str);
#endif
  free(str);
}

void CR( Cell * stack)   /*  --  */
{
#ifdef __UNIX__
  if( newline)
    myputp(newline);
  else
  {
    myputp(cursor_down);
    myputp("\r");
  }
#ifdef DELAY_FLUSH
  startFlusher();
#endif
#endif
#ifdef MSDOS
  cputs("\n");
#endif
}

void AT_XY( Cell * stack)  /* x y --  */
{
  STACK;
  int y,x;
  POP( uval, y);
  POP( uval, x);
#ifdef __UNIX__
  if( cursor_address)
  {
    myputp(tgoto( cursor_address, x, y));
#ifdef DELAY_FLUSH
    startFlusher();
#endif
  }
#endif
#ifdef MSDOS
  gotoxy(x,y);
#endif  
}

void KEY_IF( Cell * stack)   /*  -- flag  */
{
  STACK;
#ifdef __UNIX__
  fd_set set;
  struct timeval tv;
  FD_ZERO( &set);
  FD_SET( 1, &set);
  tv.tv_sec=0;
  tv.tv_usec=0;
  PUSH( nval, select( 2, &set, NULL, NULL, &tv)?FTRUE:0);
#endif
#ifdef MSDOS
  PUSH( uval, kbhit()?FTRUE:0);
#endif
}

void PAGE( Cell * stack)   /*  --  */
{
#ifdef __UNIX__
  if( clear_screen)
  {
    myputp( clear_screen);
#ifdef DELAY_FLUSH
    startFlusher();
#endif
  }
#endif
#ifdef MSDOS
  clrscr();
#endif
}

void EKEY( Cell * stack)   /*  -- u  */
{
  STACK;
  int key;
  key=getchar();
  PUSH(nval,key);
}

void EKEY_2_CHAR( Cell * stack)  /* u  -- u false | char true */
{
  STACK;
  int key;
  POP(nval,key);
  PUSH(nval,key);
  PUSH( uval, (32<=key && key<127)?FTRUE:0);
}

void EKEY_IF( Cell * stack)  /* -- flag */
{
  KEY_IF(stack);
}

void EMIT_IF( Cell * stack)  /* -- flag */
{
  STACK;
  PUSH(uval,FTRUE);
}

void MS( Cell * stack)   /* u -- */
{
  STACK;
  unsigned ms;
#ifdef __UNIX__
  struct timeval tv;
  POP(uval,ms);
#ifdef DELAY_FLUSH
  signal( SIGALRM, SIG_IGN);
  myFlush();
#endif
  tv.tv_sec=ms/1000;
  tv.tv_usec=(ms%1000)*1000;
  select( 0,0,0,0, &tv);
#endif
#ifdef MSDOS
  POP( uval,ms);
  delay( ms);
#endif  
}

void TIME_DATE( Cell * stack)  /* -- sec min hr day mn yr */
{
  STACK;
  time_t t=time(NULL);
  struct tm * tms=localtime( &t);
  assert( tms);
  PUSH( uval, tms->tm_sec);
  PUSH( uval, tms->tm_min);
  PUSH( uval, tms->tm_hour);
  PUSH( uval, tms->tm_mday);
  PUSH( uval, tms->tm_mon+1);
  PUSH( uval, tms->tm_year+1900);
}

void ALLOCATE( Cell * stack)  /* u -- a-addr ior */
{
  STACK;
  unsigned cnt;
  void * p;
  
  POP(uval,cnt);
  p=malloc( cnt);
  PUSH( vaddr, p);
  PUSH( uval, p?0:-9);
}

void _FREE( Cell * stack)   /* u -- ior  */
{
  STACK;
  void * buf;
  
  POP(vaddr,buf);
  free(buf);
  PUSH(uval,0);
}

void RESIZE( Cell * stack)  /* addr u  -- addr2 ior */
{
  STACK;
  unsigned u;
  void * p,* p2;
  
  POP( uval,u);
  POP( vaddr, p);
  p2=realloc( p, u);
  PUSH( vaddr, p2);
  PUSH( nval, p2?0:-9);
}

void BYE( Cell * stack)   /* -- */
{
  cleanupTerm();
  exit(0);
}

void SCR_ROWS( Cell * stack) /* -- rows */
{
  STACK;
  PUSH( nval, LINES);
}

void SCR_COLS( Cell * stack) /* -- cols */
{
  STACK;
  PUSH( nval, COLS);
}

void CLOSE_FILE( Cell * stack)  /* fid -- ior  */
{
  STACK;
  int fid;
  POP( nval, fid);
  if( close( fid))
    PUSH( nval, -37)
  else
    PUSH( nval,0);
}

#define FM_RO		1
#define FM_RW		2
#define FM_BIN		4
#define FM_WO		8

static int c_mode( int forth_mode)
{
  int erg=0;
  if( forth_mode & FM_RO)
    erg=O_RDONLY;
  else
    if( forth_mode & FM_RW)
      erg=O_RDWR;
    else
      if( forth_mode & FM_WO)
	erg=O_WRONLY;
  return erg;
}

void CREATE_FILE( Cell * stack)  /* addr u mode -- fid ior */
{
  STACK;
  int mode,fid,nmode;
  char * name;
  POP( nval, mode);
  POPSTRING( name);
  nmode=c_mode( mode);
  fid=open( name, nmode | O_CREAT, 0666);
  PUSH( nval, fid);
  if( fid==-1)
    PUSH( nval, -37)
  else
    PUSH( nval,0);
  free(name);
}

void DELETE_FILE( Cell * stack)  /* addr n -- ior  */
{
  STACK;
  char * name;
  POPSTRING( name);
  PUSH(uval, (unlink(name)==-1)?-37:0);
  free(name);
}

void FILE_POSITION( Cell * stack)  /* fid  -- ud ior */
{
  STACK; 
  int fid,where;
  POP( nval, fid);
  where=lseek( fid, 0, SEEK_CUR);
  PUSH( uval, where);
  PUSH( nval,0);
  if( where==-1)
    PUSH( nval,-37)
  else
    PUSH( nval,0);
}

void FILE_SIZE( Cell * stack)  /* fid -- ud ior */
{
  STACK;
  int fid;
  off_t where;
  POP( nval, fid);
  where=lseek( fid, 0, SEEK_CUR);
  if( where==-1)
  {
    PUSH( nval,0);
    PUSH( nval,0);
    PUSH( nval, -37);
  }
  else
  {
    off_t end=lseek( fid, 0, SEEK_END);
    PUSH( uval, end);
    PUSH( nval,0);
    if( -1==lseek( fid, where, SEEK_SET))
      PUSH( nval,-37)
    else
      PUSH( nval,0);
  }
}

void OPEN_FILE( Cell * stack)  /* addr u mode -- fid ior */
{
  STACK;
  int mode,fid,nmode;
  char * name;
  POP( nval, mode);
  POPSTRING( name);
  nmode=c_mode( mode);
  fid=open( name, nmode);
  PUSH( nval, fid);
  if( fid==-1)
    PUSH( nval, -38)
  else
    PUSH( nval,0);
  free(name);
}

void READ_FILE( Cell * stack)  /* addr u fid -- u2 ior */
{
  STACK;
  int fid;
  unsigned len, lread;
  char * buf;
  POP( nval, fid);
  POP( uval, len);
  POP( caddr, buf);
  lread=read( fid, buf, len);
  PUSH( uval, lread);
  if( lread==-1)
    PUSH( nval, -37)
  else
    PUSH( nval,0);
}

void READ_LINE( Cell * stack)  /* addr u fid -- u2 flag ior */
{
  STACK;
  int fid,seek_res, len, lread,i;
  char * buf;
  POP( nval, fid);
  POP( uval, len);
  POP( caddr, buf);
  lread=read( fid, buf, len);
  for( i=0; i<lread && buf[i]!='\n'; i++);
  seek_res=lseek( fid, i-lread+1, SEEK_CUR);
  PUSH( nval,i);
  PUSH( nval, lread?FTRUE:0);
  PUSH( nval, (lread==-1 || seek_res==-1)?-37:0);
}

void REPOSITION_FILE( Cell * stack)  /* ud fid -- ior  */
{
  STACK;
  int fid,res=-1;
  unsigned udh,udl;
  POP( nval, fid);
  POP( uval, udh);
  POP( uval, udl);
  if( !udh)
    res=lseek( fid, udl, SEEK_SET);
  PUSH( nval, (res==-1)?-37:0);
}

void RESIZE_FILE( Cell * stack)  /* ud fid -- ior  */
{
  STACK;
  int fid,res=-1;
  unsigned udh,udl,flen;
  POP( nval, fid);
  POP( uval, udh);
  POP( uval, udl);
  if( !udh)
  {
    flen=lseek( fid, 0, SEEK_END);
    if( (int)flen!=-1)
    {
      if( udl>flen)
      {
	/* write some */
	char * buf=malloc( udl-flen);
	if( buf)
	{
  	  res=write( fid, buf, udl-flen);
  	  free(buf);
	}
      }
      else
	res=ftruncate( fid, udl);
    }
  }
  PUSH( nval, (res==-1)?-37:0);
}

void RENAME_FILE( Cell * stack)  /* a1 u1 a2 u2 -- ior  */
{
  STACK;
  char * from, *to;
  POPSTRING( from);
  POPSTRING( to);
  PUSH( nval, (-1==rename( from, to))?-37:0);
  free( to);
  free( from);
}

void WRITE_FILE( Cell * stack)  /* addr u fid -- ior  */
{
  STACK;
  int fid;
  unsigned len, lwr;
  char * buf;
  POP( nval, fid);
  POP( uval, len);
  POP( caddr, buf);
  lwr=write( fid, buf, len);
  PUSH( nval, (lwr!=len)?-37:0);
}

void WRITE_LINE( Cell * stack)  /* addr u fid -- ior  */
{
  STACK;
  int fid;
  unsigned len, lwr;
  char * buf, cr;
  POP( nval, fid);
  POP( uval, len);
  POP( caddr, buf);
  lwr=write( fid, buf, len);
  if( lwr==len)
  {
    cr='\n';
    if( 1!=write( fid, &cr, 1))
      lwr=-1;
  }
  PUSH( nval, (lwr!=len)?-37:0);
}

void FILE_STATUS( Cell * stack)  /* addr u -- x ior */
{
  STACK;
  char * name;
  static struct stat buf;
  int erg;
  POPSTRING( name);
  erg=stat( name, &buf);
  PUSH( vaddr, &buf);
  PUSH( nval, (erg==0)?0:-38);
  free(name);
}

void FLUSH_FILE( Cell * stack)  /* fid -- ior  */
{
  STACK;
  int fid;
  POP( nval, fid);
  if( -1==fsync( fid))
    PUSH( nval, -37)
  else
    PUSH( nval,0);
  PUSH( nval,0);
}

void IS_DIRECTORY( Cell * stack) /* fstat -- flag */
{
  STACK;
  struct stat * buf;
  POP( vaddr, buf);
  PUSH( uval, S_ISDIR( buf->st_mode)?FTRUE:0);
}

void SCR_BELL( Cell * stack) /* -- */
{
  if( bell)
  {
    myputp(bell);
#ifdef DELAY_FLUSH
    startFlusher();
#endif
  }
}

void SCR_FLASH( Cell * stack ) /* -- */
{
  if( flash_screen)
  {
    myputp( flash_screen);
#ifdef DELAY_FLUSH
    startFlusher();
#endif
  }
}

void SYSTEM( Cell * stack) /* addr len -- retval */
{
  STACK;
  char * cmd;
  POPSTRING( cmd);
  reset_shell_mode();
  tcsetattr( 1, TCSADRAIN, &shell_ios);
#ifdef DELAY_FLUSH
  signal( SIGALRM, SIG_IGN);
  myFlush();
#endif
  PUSH( nval, system( cmd));
  reset_prog_mode();
  tcsetattr( 1, TCSADRAIN, &prog_ios);
  free(cmd);
}

#define READDIR { if( !glob_dir) { PUSH( uval,0); PUSH( uval,0); \
  PUSH( uval,0);} else { glob_dirent=readdir( glob_dir); if( !glob_dirent) {\
  PUSH( uval, 0); PUSH( uval, 0); PUSH( uval, 0); PUSH( uval, 0); closedir(\
  glob_dir); glob_dir=0;} else { PUSH( caddr, glob_dirent->d_name); PUSH(\
  uval, strlen(glob_dirent->d_name)); PUSH( uval, FTRUE); }}}

void FIND_FIRST( Cell * stack) /* addr1 u1 -- addr2 u2 flag1 */
{
  STACK;
  char * dir;
  POPSTRING( dir);
  if( glob_dir)
    closedir( glob_dir);
  glob_dir=opendir( dir);
  READDIR;
  free(dir);
}

void FIND_NEXT( Cell * stack) /* -- addr u flag1 */
{
  STACK;
  READDIR;
}

void TIME_OF_DAY( Cell * stack) /* -- secs usecs */
{
  STACK;
  struct timeval tv;
  gettimeofday( &tv, NULL);
  PUSH( uval, tv.tv_sec);
  PUSH( uval, tv.tv_usec);
}

void OPENLIB ( Cell * stack) /* addr len flag -- lib */
{
  STACK;
  char * name;
  int flags;
  void * lib;
  POP( nval, flags);
  POPSTRING( name);
  lib=dlopen( name, flags);
  PUSH( vaddr, lib);
  free(name);
}

void LIBERROR( Cell * stack)   /*   -- cstring  */ 
{
  STACK;
  PUSH( caddr, (char*)dlerror());
}

void LIBSYMBOL( Cell * stack)   /* lib addr len -- fct   */ 
{
  STACK;
  void * lib, *sym;
  char * name;
  POPSTRING( name);
  POP( vaddr, lib);
  sym=dlsym( lib, name);
  PUSH( vaddr, sym);
  free(name);
}

void CLOSELIB( Cell * stack)   /* lib   --   */ 
{
  STACK;
  void * lib;
  POP( vaddr, lib);
  dlclose( lib);
}

void TERM_BOOL( Cell * stack) /* -- bool-array-addr */
{
  STACK;
  PUSH( vaddr, cur_term->type.Booleans);
}

void TERM_NUMBER( Cell * stack) /* -- short-array-addr */
{
  STACK;
  PUSH( vaddr, cur_term->type.Numbers);
}

void TERM_STRING( Cell * stack) /* -- char*-array-addr */
{
  STACK;
  PUSH( vaddr, cur_term->type.Strings);
}

void GET_CWD( Cell * stack) /* -- cwd */
{
  STACK;
  static char buf[PATH_MAX];
  getcwd(buf, sizeof(buf));
  strcat(buf,"/");
  PUSH( vaddr, buf);
}

static SystemPrimitive primitives[]={
  EMIT, KEY, TYPE, CR, AT_XY, KEY_IF, PAGE, EKEY, EKEY_2_CHAR, EKEY_IF,
  EMIT_IF, MS, TIME_DATE, ALLOCATE, _FREE, RESIZE, BYE, CLOSE_FILE,
  CREATE_FILE, DELETE_FILE, FILE_POSITION, FILE_SIZE, OPEN_FILE, READ_FILE,
  READ_LINE, REPOSITION_FILE, RESIZE_FILE, RENAME_FILE, WRITE_FILE,
  WRITE_LINE, FILE_STATUS, FLUSH_FILE, SCR_ROWS, SCR_COLS, FIND_FIRST,
  FIND_NEXT, IS_DIRECTORY, SCR_BELL, SCR_FLASH, SYSTEM, TIME_OF_DAY, OPENLIB,
  LIBERROR, LIBSYMBOL, CLOSELIB, TERM_BOOL, TERM_NUMBER, TERM_STRING, GET_CWD,
};

/* Fill the global variables with values. */

/* initial value of EBP */
#define HA_INIT_DATASTACK	0
/* initial value of ESP */
#define HA_INIT_CALLSTACK	1
/* address of relocation table */
#define HA_RELTABLE		2
/* loader table address */
#define HA_LOADER		3
/* first byte after data area */
#define HA_HERE_LIMIT		4
/* first byte after code area */
#define HA_CHERE_LIMIT 		5
/* base address of image */
#define HA_IMAGE_BASE 		6
/* initial value of HERE */
#define HA_INIT_HERE		7
/* initial value of CHERE */
#define HA_INIT_CHERE		8
/* address of boot code */
#define HA_BOOT_CODE 		9
/* address of entry_point */
#define HA_ENTRY		10
/* number of cells in code area */
#define HA_CODESIZE		11
/* number of cells in data area */
#define HA_DATASIZE		12
/* initial size of data stack in cells */
#define HA_DATA_CELLS		13
/* initial size of call stack in cells */
#define HA_CALL_CELLS		14

#define READCELL( x) read( inFile, &x, sizeof( unsigned ))
static int readImage( int inFile, unsigned offs)
{
  unsigned imglen, caLen, daLen, reloAlloc, reloCnt, caUse, daUse,i, imgBase,
    oldCaLen /*, oldDaLen*/;
  /* inFile is correctly positioned. */
#define FP printf("%d\n",lseek(inFile,0,SEEK_CUR));
  READCELL( reloCnt);
  reloAlloc=(3*reloCnt)/2;
  ReloTable=(unsigned *)calloc( reloAlloc+2, sizeof( unsigned ));
  if(!ReloTable)
    return 0;
  ReloTable[0]=reloAlloc;
  ReloTable[1]=reloCnt;
  
  read( inFile, &ReloTable[2], reloCnt*sizeof(unsigned )); 
  
  READCELL( caLen); 
  READCELL( daLen);
  READCELL( caUse);
  READCELL( daUse);
  READCELL( imgBase);
#ifndef NDEBUG
  printf("OK.\n");
#endif
  if( MemorySize>caLen)
  {
    caLen=MemorySize;
#ifndef NDEBUG
    printf("Memory size override for code area used.\n");
#endif
  }
  if( MemorySize>daLen)
  {
    daLen=MemorySize;
#ifndef NDEBUG
    printf("Memory size override for data area used.\n");
#endif
  }
  imglen=caLen+daLen;
  
  /* Image=(unsigned *)calloc( imglen, sizeof( char)); */
  Image=(unsigned *)mmap(NULL, imglen, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if(Image==(unsigned *)MAP_FAILED)
  {
#ifndef NDEBUG
   printf("Could not allocate memory for image.\n");
#endif 
    return 0;
  }
#ifndef NDEBUG
  printf("code area length: %d bytes\ndata area length: %d bytes\n",caLen,
      daLen);
#endif
  read( inFile, Image, caUse);
  read( inFile, Image+caLen/sizeof( unsigned ), daUse);
  ForthDataStack=(unsigned *)calloc(Image[HA_DATA_CELLS]+1,sizeof(unsigned ));
  if( !ForthDataStack)
    return 0;
  ForthCallStack=(unsigned *)calloc(Image[HA_CALL_CELLS],sizeof(unsigned ));
  if( !ForthCallStack)
    return 0;
  
  Image[HA_INIT_DATASTACK]=(unsigned) ForthDataStack+Image[HA_DATA_CELLS]*sizeof( unsigned );
  Image[HA_INIT_CALLSTACK]=(unsigned) ForthCallStack+Image[HA_CALL_CELLS]*sizeof( unsigned );
  Image[HA_RELTABLE]=(unsigned )ReloTable;
  Image[HA_LOADER]=(unsigned )primitives;
  Image[HA_HERE_LIMIT]=(unsigned )Image+imglen;
  Image[HA_CHERE_LIMIT]=(unsigned )Image+caLen;
  Image[HA_IMAGE_BASE]=(unsigned )Image;
  Image[HA_INIT_CHERE]=(unsigned )Image+caUse;
  Image[HA_INIT_HERE]=(unsigned )Image+daUse+caLen;
  oldCaLen=Image[HA_CODESIZE];
  /* oldDaLen=Image[HA_DATASIZE]; */
  Image[HA_CODESIZE]=caLen;
  Image[HA_DATASIZE]=daLen;
  
  for( i=0; i<ReloTable[1]; i++)
  {
    unsigned * relAddr, origVal;
    char * cImage=(char*)Image;
    unsigned relItem=ReloTable[i+2];
    if( relItem>=oldCaLen)
    {
      relItem=relItem-oldCaLen+caLen;
      ReloTable[i+2]=relItem;
    }
    relAddr=(unsigned *)(&cImage[relItem]);
    origVal=*relAddr-imgBase;
    if( origVal>=oldCaLen)
      *relAddr=(unsigned )Image+origVal-oldCaLen+caLen;
    else
      *relAddr=(unsigned )Image+origVal;
  }

  return 1;
}

/* Jump to the start of the image. */
typedef void (*GoForth)(int argc, char ** argv);
static void startImage(int argc, char ** argv)
{
  GoForth goForth=(GoForth)Image[HA_BOOT_CODE];
  goForth(argc,argv);
}

/* Free all allocated space. */
#define DELETE(x) if( x) {free(x); x=NULL; }
static void freeImage()
{
  DELETE( ReloTable);
  DELETE( Image);
  DELETE( ForthCallStack);
  DELETE( ForthDataStack);
}

static int idFound( int fi, unsigned offs)
{
  unsigned char buf[HEADER_ID_LEN];
  unsigned i;
  lseek( fi, offs, SEEK_SET);
  read( fi, buf, HEADER_ID_LEN);
  for( i=0; i<HEADER_ID_LEN; i++)
    if( buf[i]!=(headerID[i]|0x80))
      return 0;
  return 1;
}

static int findImageHeader( int fi, unsigned * offs)
{
  unsigned i;
  unsigned len=lseek( fi, 0, SEEK_END);
  assert( offs);
  for( i=0; i<len; i++)
  {
    if( idFound( fi, i))
    {
      *offs=i;
      return 1;
    } 
  }
  return 0;
}

/* The function needs no return value, because it does not return, if it is
   successful. Otherwise you may try to run a different image. */
static void runImage(const char * fileName, int argc, char ** argv)
{
  /* We want to open a binary file. Therefore we use handles, not FILEs, 
     because some systems (e.g. NeXTStep 3.1 for Intel 386) can't open binary
     FILES, only binary handles. */
  int inFile;
  unsigned imageOk,offs;
  assert( fileName);
#ifndef NDEBUG
  printf("Trying image file '%s': ", fileName);
  fflush(stdout);
#endif
  
  inFile=open(fileName,O_RDONLY
#ifdef MSDOS
    |O_BINARY
#endif  
  );
  if( inFile==-1)
  {
    /* Some error occured. Tell what happened and return. */
#ifndef NDEBUG
    printf("%s\n",strerror(errno));
#endif
    return;
  }
  imageOk=0;
  if( findImageHeader( inFile, &offs))
  {
    if( readImage( inFile,offs))
    {
      imageOk=1;
    }
#ifndef NDEBUG
    else
      printf("Error loading image.\n");
#endif  
  }
#ifndef NDEBUG
  else
    printf("Contains no image.\n");
#endif  
  close(inFile);
  if( imageOk)
  {
    setupTerm();
#ifndef NDEBUG
    printf("Image started.\n");
#endif  
    /* This function never returns. */
    startImage(argc, argv);
  }
  /* Clean up. */
  freeImage();
}

static void processOptions( int * argc, char ** argv)
{
  int i;
  for( i=1; i<*argc; i++)
  {
    if( !strcmp( argv[i], "-m"))
    {
      i++;
      if( i<*argc)
      {
	MemorySize=(atoi( argv[i])+3)&~3;
#ifndef NDEBUG 
	printf("Memory size overrride: %d bytes\n",MemorySize);
#endif
	(*argc)-=2;
	memmove( argv+(i-1), argv+(i+1), sizeof( char*)*(*argc-i+1));
      }
    }
    else
      return;
  }
}

/*================================main program================================*/
int main(int argc, char ** argv)
{
  char buf[10000];
  processOptions( &argc, argv);
  /* Try to find an image in this program. */
  runImage( argv[0], argc, argv);
  strcpy(buf, INSTALL_BIN_DIR);
  strcat(buf, argv[0]);
  runImage( buf, argc, argv);
  runImage( "flk.flk", argc, argv);
  runImage( INSTALL_DIR "default.flk", argc, argv);
  
  /* No image could be found. This must be a User-to-stupid error. */
 
  printErrorBanner();
  printf("I could not load any image. This is all your fault.\n");
  
  return 1;
}

