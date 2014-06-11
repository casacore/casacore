/************************************************************************/
/*									*/
/* This handles errors and can abort your program.			*/
/*									*/
/*  History:								*/
/*    rjs,mjs ????    Very mixed history. Created, destroyed, rewritten.*/
/*    rjs     26aug93 Call habort_c.					*/
/*    rjs     14jul98 Add a caste operation in errmsg_c, to attempt	*/
/*		      to appease some compilers.			*/
/*    pjt     23sep01 darwin						*/
/*    pjt      4dec01 bypass fatal errors (for alien clients) if req'd  */
/*                    through the new bugrecover_c() routine            */
/*    pjt     17jun02 prototypes for MIR4                               */
/*    pjt/ram  5dec03 using strerror() for unix                         */
/*    pjt      1jan05 bugv_c: finally, a real stdargs version!!!        */
/*                    though cannot be exported to Fortran              */
/*    pjt     26mar07 bugmessage_c: retrieve last fatal bug message     */
/*    pjt     27mar07 bugseverity_c: also overhauled bug recovery       */
/*                    and removed VMS specific code                     */
/*    pjt     17may07 removed old-non ANSI declaration                  */
/*    pjt      5dec07 add Name to bug output - why took us so long?     */
/*    pkgw     6mar08 declare Name as static to prevent symbol clashes  */
/*    dhem    12feb09 added hooks to allow alien clients to completely  */
/*                    override the default bug handler                  */
/*    pkgw    14dec11 Make errmsg_c public for use in uvio.c            */
/************************************************************************/

#if defined(HAVE_CONFIG_H) && HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "miriad.h"

char *errmsg_c(int n);
static int  handle_bug_cleanup(int d, char s, Const char *m);

static char *Name = NULL;     /* a slot to store the program name       */
int reentrant=0;              /* keep track of state                    */

/* helper definitions for function pointers */
typedef void (*bug_cleanup_proc)(void);
typedef void (*bug_handler_proc)(char s, Const char *m);

/* external bug cleanup handler, if any */
/* only used by internal bug handler    */
static bug_cleanup_proc bug_cleanup=NULL;

/* bug handler function pointer */
static bug_handler_proc bug_handler=NULL;

/* forward declaration */
static void default_bug_handler_c(char s,Const char *m);

static char *bug_message=0;   /* last message                           */ 
static char bug_severity=0;   /* last severity level (i,w,e or f)       */


#define MAXMSG 256
static char msg[MAXMSG];      /* formatted message for bugv_c()         */


/************************************************************************/
char *bugmessage_c(void)
/** bugmessage_c -- return last fatal error message string              */
/*& pjt                                                                 */
/*: error-handling                                                      */
/*+                                                                    
    This routine does not have a FORTRAN counterpart, as it is normally 
    only called by C clients who have set their own error handler if
    for some reason they don't like the MIRIAD one (e.g. C++ or java
    exceptions, or NEMO's error handler. This way the bugrecover handler
    can call this routine to retrieve the last fatal error message. 

    bugrecover_c(my_handler);
    
    void my_handler(void) {
       char *m = bugmessage_c();
       printf("RECOVERED: %s\n",m);
    }
    ..                                                                  */
/*--                                                                    */
/*----------------------------------------------------------------------*/
{
  return bug_message;
}

/************************************************************************/
char bugseverity_c(void)
/** bugseverity_c -- return last severity level                         */
/*& pjt                                                                 */
/*: error-handling                                                      */
/*+                                                                    
    This routine does not have a FORTRAN counterpart, as it is normally 
    only called by C clients who have set their own error handler if
    for some reason they don't like the MIRIAD one (e.g. C++ or java
    exceptions, or NEMO's error handler. This way the bugrecover handler
    can call this routine to retrieve the last severity level 

    bugrecover_c(my_handler);
    
    void my_handler(void) {
       char  s = bugseverity_c();
       char *m = bugmessage_c();
       printf("RECOVERED: (%c) %s\n",s,m);
       if (s=='f') exit(1);
    }
    ..                                                                  */
/*--                                                                    */
/*----------------------------------------------------------------------*/
{
  return bug_severity;
}

/************************************************************************/
void bughandler_c(bug_handler_proc new_bug_handler)
/** bughandler_c -- specify the bug handler callback function           */
/*& dhem                                                                */
/*: error-handling                                                      */
/*+                                                                    
    This routine does not have a FORTRAN counterpart, as it is normally 
    only called by C clients who need to set their own error handler if
    for some reason they don't like the MIRIAD one (e.g. C++ or java
    exceptions, or NEMO's error handler, or scripting languages such as
    Ruby and Python that provide their own exception handling
    capabilities).
    
    Absolutely nothing is done before or after the bug handler is
    called.  This is even more of an override than bugrecover_c
    provides because on fatal errors, habort_c is called before the bug
    cleanup routine installed by bugrecover_c is called.  Another
    difference is that bug_message and bug_severity are not set;
    instead the severity and message are passed as parameters to the
    bug handler callback function..

    If NULL is passed as the new_bug_handler parameter, the default bug
    handler will be reinstated.

    Example of usage:

    void my_bug_handler(char s, onst char *m) {
        ....
    }


    ..
    bughandler_c(my_bug_handler);
    ..                                                                  */
/*--                                                                    */
/*----------------------------------------------------------------------*/
{
    bug_handler = new_bug_handler;
    if (bug_message) free(bug_message);
    bug_message = strdup("no bug_message has been set yet");
}

/************************************************************************/
void bugrecover_c(void (*cl)(void))
/** bugrecover_c -- bypass fatal bug calls for alien clients            */
/*& pjt                                                                 */
/*: error-handling                                                      */
/*+                                                                    
    This routine does not have a FORTRAN counterpart, as it is normally 
    only called by C clients who need to set their own error handler if
    for some reason they don't like the MIRIAD one (e.g. C++ or java
    exceptions, or NEMO's error handler 
    Example of usage:

    void my_bug_cleanup(void) {
        ....
    }


    ..
    bugrecover_c(my_bug_cleanup);
    ..                                                                  */
/*--                                                                    */
/*----------------------------------------------------------------------*/
{
    bug_cleanup = cl;
    if (bug_message) free(bug_message);
    bug_message = strdup("no bug_message has been set yet");
}

/************************************************************************/
void buglabel_c(Const char *name)
/** buglabel -- Give the "program name" to be used as a label in messages. */
/*& pjt									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
	subroutine buglabel(name)

	implicit none
	character name*(*)

  Give the name that is to be used as a label in error messages. Usually
  this is the program name and should be set by the user interface.

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
void bug_c(char s,Const char *m)
/** bug -- Issue an error message, given by the caller.			*/
/*& pjt									*/
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
    message	The error message text.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if(bug_handler == NULL) {
    bug_handler = default_bug_handler_c;
  }

  bug_handler(s, m);
}
/************************************************************************/
static void default_bug_handler_c(char s, Const char *m)
/*
  Default bug handler.
------------------------------------------------------------------------*/
{
  char *p;
  int doabort;

  doabort = 0;
  if      (s == 'i' || s == 'I') p = "Informational";
  else if (s == 'w' || s == 'W') p = "Warning";
  else if (s == 'e' || s == 'E') p = "Error";
  else {doabort = 1;		 p = "Fatal Error"; }

  if (!bug_cleanup)
  {
    if ( Name == NULL )
      buglabel_c("(NOT SET)");
    fprintf(stderr,"### %s [%s]:  %s\n",p,Name,m);
  }

  if(doabort){
    reentrant = !reentrant;
    if(reentrant)habort_c();
    if (!handle_bug_cleanup(doabort,s,m))
      exit(1);
  } else
    handle_bug_cleanup(doabort,s,m);
}
/************************************************************************/
void bugv_c(char s,Const char *m, ...)
/** bugv_c -- Issue a dynamic error message, given by the caller.	*/
/*& pjt									*/
/*: error-handling							*/
/*+ C call sequence:
	bugv_c(severity,message,....)

  Output the error message given by the caller, and abort if needed.
  Note this routine has no counterpart in FORTRAN.

  Input:
    severity	Error severity character. 
                Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    message	The error message string, can contain %-printf style 
                directives, as used by the following arguments.
     ...         Optional argument, in the printf() style               */
/*--									*/
/*----------------------------------------------------------------------*/
{
  va_list ap;

  va_start(ap,m);
  vsnprintf(msg,MAXMSG,m,ap);
  msg[MAXMSG-1] = '\0'; /* backstop */
  va_end(ap);

  bug_c(s, msg);
}

/************************************************************************/
void bugno_c(char s,int n)
/** bugno -- Issue an error message, given a system error number.	*/
/*& pjt									*/
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
char *errmsg_c(int n)
/*
  Return the error message associated with some error number.
------------------------------------------------------------------------*/
{
/* check for linux leaves this compat with old style build
 * this should be removed in favor of HAVE_STRERROR once
 * is only supported using autotools/configure
 */
#if defined(linux) || (defined(HAVE_STRERROR) && HAVE_STRERROR)
  /* new POSIX.1 style, 20 years old now... (1988) */
  if(n == -1)
    return "End of file detected";
  return strerror(n);
#else
  /* very old style code -- stdio.h is supposed to supply this */
#  if 0
  extern int sys_nerr;
  extern char *sys_errlist[];
#  endif
  if(n > 0 && n <= sys_nerr)return((char *)sys_errlist[n]);
  else {
    sprintf(msg,"Unknown error with number %d detected.",n);
    return msg;
  }
#endif
}
/************************************************************************/
static int handle_bug_cleanup(int doabort, char s, Const char *m)
/*
  Handle cleaning up a bug 
------------------------------------------------------------------------*/
{
  if (bug_cleanup) {
    if (bug_message) free(bug_message);
    bug_message = strdup(m);      /* save last message */
    bug_severity = s;             /* save last severity */
    (*bug_cleanup)();             /* call handler ; this may exit */
    if (doabort)                  /* if it got here, handler didn't exit */
      fprintf(stderr,"### handle_bug_cleanup: WARNING: code should not come here\n");
    return 1;
  }
  return 0;
}
