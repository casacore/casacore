/*----------------------------------------------------------------------------
* rcscat: Concatenate two RCS version files
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1995,1996
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
* Usage: rcscat rcsfile rcsarkv
*-----------------------------------------------------------------------------
* rcscat is a special-purpose utility for concatenating the archived RCS
* version file to the current RCS version file for an AIPS++ source.  It is
* used by 'exhale' to update the AIPS++ RCS archive when creating a new
* base release.
*
* It is assumed that the first RCS file has just been upreved to the new base
* release and that all revisions older than the old base release have been
* outdated via the 'rcs -o' command.  The most recent revision in the archive
* file is that of the old base release.
*
* Options:
*   none
*
* Exit status:
*    0:  Success, value returned on stdout.
*    1:  Usage error.
*    2:  File access error.
*    3:  RCS file format error.
*
* Original: 1995/07/20 by Mark Calabretta, ATNF
* $Id$
*---------------------------------------------------------------------------*/

#include <errno.h>
#include <stdio.h>
#include <unistd.h>

char *usage = "Usage: rcscat rcsfile rcsarkv";

main(argc,argv)

int   argc;
char  *argv[];

{
   char text[256];
   int  ch, j;
   FILE *file, *rcsfile, *rcsarkv;

   if (argc != 3) {
      fprintf(stderr, "%s\n", usage);
      exit(1);
   }

   for (j = 1; j <= 2 ; j++) {
      /* Check for option flags - none allowed. */
      if (argv[j][0] == '-') {
         fprintf(stderr, "%s\n", usage);
         exit(1);
      }

      /* Check accessibility of the specified files. */
      if (access(argv[j], R_OK) == -1) {
         fprintf(stderr, "rcscat: Cannot access %s.\n", argv[j]);
         perror("rcscat");
         exit(2);
      }

      /* Open readonly. */
      if ((file = fopen(argv[j], "r")) == (FILE *)NULL) {
         fprintf(stderr, "rcscat: Cannot open %s.\n", argv[j]);
         perror("rcscat");
         exit(2);
      }

      if (j == 1) {
         rcsfile = file;
      } else {
         rcsarkv = file;
      }
   }


   /* Get the current RCS header from start through to symbol list. */
   j = 0;
   strcpy(text, "symbols");
   while (1) {
      if ((ch = getc(rcsfile)) == EOF)
         exit(3);
      putchar(ch);
      if (ch == text[j]) {
         j++;
         if (j == 7) break;
      } else {
         j = 0;
      }
   }

   j = 0;
   while (1) {
      if (fgets(text,256,rcsfile) == (char *)NULL)
         exit(3);
      text[strlen(text)-1] = '\0';
      if (text[strlen(text)-1] == ';')
         break;
      if (j) putchar('\n');
      printf("%s", text);
      j = 1;
   }


   /* Append the symbol list from the archive file. */
   j = 0;
   strcpy(text, "symbols");
   while (1) {
      if ((ch = getc(rcsarkv)) == EOF)
         exit(3);
      if (ch == text[j]) {
         j++;
         if (j == 7) break;
      } else {
         j = 0;
      }
   }

   while (1) {
      if (fgets(text,256,rcsarkv) == (char *)NULL)
         exit(3);
      printf("%s", text);
      if (text[strlen(text)-2] == ';')
         break;
   }


   /* Get the rest of the header from the current file. */
   while (1) {
      if (fgets(text,256,rcsfile) == (char *)NULL)
         exit(3);
      if (strcmp(text,"next	;\n") == 0)
         break;
      printf("%s", text);
   }


   /* Get the rest of the header from the archive file. */
   while (1) {
      if (fgets(text,256,rcsarkv) == (char *)NULL)
         exit(3);
      if (strncmp(text,"next	",5) == 0) {
         printf("%s", text);

         if (strcmp(text,"next	;\n") != 0) {
            while (1) {
               if (fgets(text,256,rcsarkv) == (char *)NULL)
                  exit(3);
               printf("%s", text);
               if (strcmp(text,"next	;\n") == 0)
                  break;
            }
         }

         break;
      }
   }


   /* Get the rest of the current file. */
   while (1) {
      if ((ch = getc(rcsfile)) == EOF)
         break;
      putchar(ch);
   }


   /* Everything after the first revision of the archive file. */
   while (1) {
      fgets(text,256,rcsarkv);
      if (strcmp(text,"text\n") == 0)
         break;
   }
   ch = getc(rcsarkv);

   while (1) {
      if ((ch = getc(rcsarkv)) == EOF)
         break;
      if (ch == '@') {
         if ((ch = getc(rcsarkv)) == EOF)
            break;
         if (ch == '@') {
            continue;
         } else if (ch == '\n') {
            break;
         } else {
            exit(3);
         }
      }
   }

   while (1) {
      if ((ch = getc(rcsarkv)) == EOF)
         break;
      putchar(ch);
   }


   exit(0);
}
