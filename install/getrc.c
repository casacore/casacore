/*----------------------------------------------------------------------------
* getrc: get the value of the specified AIPS++ resource
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1992,1993,1994
*   Associated Universities, Inc. Washington DC, USA.
*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program; if not, write to the Free Software
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning AIPS++ should be addressed as follows:
*          Internet email: aips2-request@nrao.edu.
*          Postal address: AIPS++ Project Office
*                          National Radio Astronomy Observatory
*                          520 Edgemont Road
*                          Charlottesville, VA 22903-2475 USA
*
*-----------------------------------------------------------------------------
* getrc gets the value of the specified AIPS++ resource from the aipsrc
* databases.  These are searched in the following sequence:
*
*    ~/.aipsrc
*    $AIPSROOT/.aipsrc
*    $AIPSHOST/aipsrc
*    $AIPSSITE/aipsrc
*    $AIPSARCH/aipsrc
*
* It is not an error for any of the aipsrc files to be absent or empty.
*
* Options:
*   -i   Ignore ~/.aipsrc
*   -v   Verbose mode.
*
* Exit status:
*    0:  Success, value returned on stdout.
*    1:  Usage error.
*    2:  AIPSPATH not defined.
*    3:  HOME not defined.
*    4:  Invalid regular expression.
*    5:  Pattern not matched.
*
* Original: 1992/03/05 by Mark Calabretta, ATNF
* $Id$
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(AIPS_DARWIN)
   #define EXPSIZ 1024
   #include <regex.h>
    regex_t expbuf;
#endif
int getrc(int douser, char *expr, char *match, char *rcfile, char *line);

#if defined (AIPS_SVID3) && !defined(HAVE_LINUX_GLIBC)
   #define INIT       register char *sp = instring;
   #define GETC()     (*sp++)
   #define PEEKC()    (*sp)
   #define UNGETC(c)  (--sp)
   #define RETURN(ep) return(0);
   #define ERROR(c)   return((char *)c)

   #define EXPSIZ 1024
   char expbuf[EXPSIZ];

   #include <regexp.h>
#endif

#if defined(TOOLBOX)

int main(int argc, char *argv[])
{
   int  douser, j, verbose;
   char *expr, line[256], match[256], rcfile[256];

   /* Defaults. */
   douser = 1;
   verbose = 0;
   expr = "";

   /* Parse arguments. */
   for (j = 1; j < argc; j++) {
      switch (argv[j][0]) {
      case '-':
         /* Option flags. */
         switch (argv[j][1]) {
         case 'i':
            /* Ignore user's .aipsrc file. */
            douser = 0;
            break;
         case 'v':
            /* Verbose mode. */
            verbose = 1;
            break;
         default:
            fprintf(stderr, "Usage: getrc [-i] [-v] expression\n");
            exit(1);
         }
         break;

      default:
         /* Expression to be matched. */
         expr = argv[j];
         break;
      }
   }

   if (strcmp(expr, "") == 0) {
      fprintf(stderr, "Usage: getrc [-i] [-v] expression\n");
      exit(1);
   }

   switch (getrc(douser, expr, match, rcfile, line)) {
   case 0:
      printf ("%s", match);
      if (verbose) {
         fprintf(stderr, "Line matched in file %s was\n", rcfile);
         fprintf(stderr, "%s", line);
      }
      exit(0);
   case 2:
      fprintf(stderr, "getrc: AIPSPATH is undefined.\n");
      exit(2);
   case 3:
      fprintf(stderr, "getrc: HOME is undefined.\n");
      exit(3);
   case 4:
      fprintf(stderr, "getrc: Regular expression compilation failed.\n");
      exit(4);
   case 5:
      fprintf(stderr, "getrc: Pattern not matched.\n");
      exit(5);
   }
}

#endif


int getrc(int douser, char *expr, char *match, char *rcfile, char *line)
{
   int  i, j, k, n;
   char pattern[256];
   char *aipspath, *cp, *home;
   FILE *fd;


   /* Check that $AIPSPATH is defined. */
   if ((aipspath = getenv("AIPSPATH")) == NULL) {
      return (2);
   }

   /* Translate $HOME. */
   if (douser && (home = getenv("HOME")) == NULL) {
      return (3);
   }

   /* Test for a blank expression. */
   if (strcmp(expr, "") == 0) {
      printf("Blank expression\n");
      return(4);
   }

   /* Search through each aipsrc file in sequence. */
   for (i = -1; i < 4; i++) {
      /* Set the aipsrc search path. */
      if (i == -1) {
         /* User-specific aipsrc file */
         if (! douser) continue;
         strcpy(rcfile, home);
         strcat(rcfile, "/.aipsrc");
      } else {
         /* Root-, host-, site-, and default aipsrc files. */
         if (i == 0) {
            for (k = 0 ; aipspath[k] != '\0' ; k++) {
               if (aipspath[k] == ' ') {
                  break;
               } else {
                  rcfile[k] = aipspath[k];
               }
            }
            rcfile[k] = '\0';
            strcat(rcfile, "/.aipsrc");
         } else {
            for (n = 0 , k = 0 ; aipspath[k] != '\0' ; k++) {
               if (aipspath[k] == ' ') {
                  if (++n == 5-i) break;
                  rcfile[k] = '/';
               } else {
                  rcfile[k] = aipspath[k];
               }
            }
            rcfile[k] = '\0';
            strcat(rcfile, "/aipsrc");
         }
      }

      /* Open the file. */
      if ((fd = fopen(rcfile, "r")) == NULL) continue;

      /* Read through the file. */
      while (fgets(line, 256, fd) != (char *)NULL ) {
         /* Discard comment lines. */
         if (line[0] == '#') continue;
         if (line[0] == '\n') continue;

         /* Build a regular expression. */
         for (j = 0, k = 0 ; line[j] ; j++) {
            if (line[j] == ':') break;

            /* Don't copy leading whitespace. */
            if (k == 0) {
               if (line[j] == ' ') continue;
               if (line[j] == '\011') continue;
            }

            /* Take care of special characters. */
            if (line[j] == '*') {
               pattern[k++] = '.';
               pattern[k++] = '*';
            } else if (line[j] == '.') {
               pattern[k++] = '\\';
               pattern[k++] = '.';
            } else {
               pattern[k++] = line[j];
            }
         }
         if (k == 0) continue;
         pattern[k] = '\0';


         /* Compile the regular expression. */
#if defined (AIPS_SVID3) && !(defined(HAVE_LINUX_GLIBC) || defined(AIPS_DARWIN))
         if (compile(pattern, expbuf, &expbuf[EXPSIZ], '\0') != 0) return (4);
#else
#if defined(AIPS_DARWIN)
         if (regcomp(&expbuf, pattern, REG_EXTENDED) != 0){
             
             return (4);
         }
#else
         if (re_comp(pattern) != 0) return (4);
#endif
#endif


         /* Test for regular expression match. */
#if defined (AIPS_SVID3) && !(defined(HAVE_LINUX_GLIBC) || defined(AIPS_DARWIN))
         if (step(expr, expbuf)) {
#else
#if defined(AIPS_DARWIN)
         if (!regexec(&expbuf, expr,  0, 0, REG_NOTBOL)) {
#else
         if (re_exec(expr)) {
#endif
#endif
            /* Return the value. */
            for (k = 0, cp = line ; *cp != '\0' ; cp++) {
               if (k == 0) {
                  if (*cp == ':') k = 1;
               } else {
                  if (*cp != ' ' && *cp != '\011') {
                     strcpy(match, cp);
                     return (0);
                  }
               }
            }
         }
      }
   }

   /* Pattern not found. */
   return (5);
}
