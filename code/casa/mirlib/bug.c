/*
    bug.c: Miriad library routine for error handling.
    Copyright (C) 1999,2000
    Associated Universities, Inc. Washington DC, USA.

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This library is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.

    Correspondence concerning AIPS++ should be addressed as follows:
           Internet email: aips2-request@nrao.edu.
           Postal address: AIPS++ Project Office
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA

    $Id$
*/

/************************************************************************/
/*									*/
/* This handles errors and can abort your program.			*/
/*									*/
/*  History:								*/
/*    rjs,mjs ????    Very mixed history. Created, destroyed, rewritten.*/
/*    rjs     26aug93 Call habort_c.					*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>

static char *errmsg_c();
void bug_c();

char *Name = NULL;
int reentrant=0;
/************************************************************************/
void buglabel_c(name)
char *name;
/** buglabel -- Give the "program name" to be used as a label in messages. */
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
	subroutine buglabel(name)

	implicit none
	character name*(*)

  Give the name that is to be used as a label in error messages.

  Input:
    name	The name to be given as a label in error messages.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if(Name != NULL)free(Name);
  Name = malloc(strlen(name)+1);
  strcpy(Name,name);
}
/************************************************************************/
void bugno_c(s,n)
char s;
int n;
/** bugno -- Issue an error message, given a system error number.	*/
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
	subroutine bugno(severity,errno)

	implicit none
	character severity*1
	integer errno

  Output the error message associated with a particular error number.

  Input:
    severity	Error severity. Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    errno	host error number.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if (n == -1)bug_c(s,"End of file detected");
  else bug_c(s,errmsg_c(n));
}
/************************************************************************/
void bug_c(s,m)
char s,*m;
/** bug -- Issue an error message, given by the caller.			*/
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
	subroutine bug(severity,message)

	implicit none
	character severity*1
	character message*(*)

  Output the error message given by the caller, and abort if needed.

  Input:
    severity	Error severity. Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    message	The error message text.
/*--									*/
/*----------------------------------------------------------------------*/
{
  char *p;
  int doabort;

  doabort = 0;
  if      (s == 'i' || s == 'I') p = "Informational";
  else if (s == 'w' || s == 'W') p = "Warning";
  else if (s == 'e' || s == 'E') p = "Error";
  else {doabort = 1;		 p = "Fatal Error"; }

  fprintf(stderr,"### %s:  %s\n",p,m);
  if(doabort){
    reentrant = !reentrant;
    if(reentrant)habort_c();
#ifdef vaxc
# include ssdef
    lib$stop(SS$_ABORT);
#else
    if(Name != NULL)fprintf(stderr,"### Called by: %s\n",Name);    
    fprintf(stderr,"### Program exiting with return code = 1 ###\n");
    exit (1);
#endif
  }
}
/************************************************************************/
static char *errmsg_c(n)
int n;
/*
  Return the error message associated with some error number.
------------------------------------------------------------------------*/
{
  static char string[128];
#ifdef vaxc
#include <descrip.h>
  $DESCRIPTOR(string_descriptor,string);
  short int len0;
  int one;

  one = 1;
  lib$sys_getmsg(&n,&len0,&string_descriptor,&one);
  string[len0] = 0;
  return(string);
#else
  extern int sys_nerr;
  extern const char *const sys_errlist[];


  if(n > 0 && n <= sys_nerr)return(sys_errlist[n]);
  else{
    sprintf(string,"Unknown error with number %d detected.",n);
    return(string);
  }
#endif
}
