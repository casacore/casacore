/*----------------------------------------------------------------------------
* tract: Report the age of a file or directory.
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1995
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
* Usage: tract [-a | -c | -m ] [-s] [-q#] <file|directory>
*-----------------------------------------------------------------------------
* tract reports the age of a file or directory; by default it reports the time
* in seconds since the file or directory was last modified.  In query mode it
* can be used to determine if a file or directory is older than a specified
* number of seconds.
*
* Options:
*   -a   Report time since last access.
*   -c   Report time since last status change.
*   -m   Report time since last modification (default).
*   -s   Report the time in sexagesimal (h:m:s) format (default is seconds).
*   -q#  Return true if the file is older than the specified number of
*        seconds.  No age is reported.
*
* Exit status:
*   -1:  In query mode, the file is younger than the specified timespan.
*    0:  Success, value returned on stdout.
*    1:  Usage error.
*    2:  File access error.
*
* Original: 1995/02/21 by Mark Calabretta, ATNF
* $Id$
*---------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

char *usage="Usage: tract [-a | -c | -m] [-s] [-q#] <file|directory>";

main(argc,argv)

int   argc;
char  *argv[];

{
   int atoi(), gotpath, hms, j, query;
   char mode;
   char *path;
   time_t tract, now, qtime;
   struct stat buf;

   /* Set defaults. */
   gotpath = 0;
   hms   = 0;
   mode  = 'm';
   query = 0;

   /* Parse arguments. */
   for (j = 1; j < argc; j++) {
      switch (argv[j][0]) {
      case '-':
         /* Option flags. */
         switch (argv[j][1]) {
         case 'a':
            /* Last access time. */
            mode = 'a';
            break;
         case 'c':
            /* Last status change time. */
            mode = 'c';
            break;
         case 'm':
            /* Last modification time. */
            mode = 'm';
            break;
         case 's':
            /* Use h:m:s format. */
            hms = 1;
            break;
         case 'q':
            /* Query if age exceeds the specified number of seconds. */
            query = 1;
            qtime = atoi(&argv[j][2]);
            break;
         default:
            fprintf(stderr, "%s\n", usage);
            exit(1);
         }
         break;

      default:
         /* Pathname. */
         path = argv[j];
         gotpath = 1;
         break;
      }
   }

   if (! gotpath) {
      fprintf(stderr, "%s\n", usage);
      exit(1);
   }

   /* Check accessibility of the specified file. */
   if (access(path, R_OK) == -1) {
      fprintf(stderr, "tract: Cannot access %s.\n", path);
      perror("tract");
      exit(2);
   }

   /* Get the file status block. */
   if (stat(path, &buf)) {
      fprintf(stderr, "tract: Cannot stat %s.\n", path);
      perror("tract");
      exit(2);
   }

   /* Get the current time. */
   time(&now);

   /* Determine the age. */
   switch (mode) {
   case 'a':
      /* Last access time. */
      tract = now - buf.st_atime;
      break;
   case 'c':
      /* Last status change time. */
      tract = now - buf.st_ctime;
      break;
   case 'm':
      /* Last modification time. */
      tract = now - buf.st_mtime;
      break;
   }

   /* Return the result. */
   if (query) {
      if (tract < qtime) exit(-1);
   } else if (hms) {
      printf("%ld:", tract/3600);
      printf("%02ld:", (tract%=3600)/60);
      printf("%02ld\n", tract%60);
   } else {
      printf("%ld\n", tract);
   }


   exit(0);
}
