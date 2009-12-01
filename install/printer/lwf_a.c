/* lwf - convert plain text to PostScript.
 *
 * Usage: lwf [-1] [-a] [-b] [-c#] [-d#] [-D] [-f<font>] [-g#] [-i#]
 *            [-l] [-nfile] [-olist] [-p<paper>] [-r] [-s#] [-t#] [-v]
 *            [file1 [file2 ...]]
 *
 * Options:
 *      -1          Print in single-sided mode; the default is to print in
 *                  two-sided (duplex) mode if the printer supports it.
 *
 *      -a          Anonymous mode - don't print the page headers or banner
 *                  page (undocumented).
 *
 *      -b          Print banner page instead of page headers.
 *
 *      -c#         Print file in multiple columns (default 1).
 *
 *      -d#         Display ASCII control codes (ASCII 00-1f, 7f-ff):
 *
 *                     0: Interpret BS, HT, NL, NP, and CR and ignore
 *                        all other control codes.
 *                     1: Interpret NUL, BS, HT, NL, NP, and CR and display
 *                        all other control codes in two-character mnemonic
 *                        form (default).
 *                     2: Display all control codes in two-character mnemonic
 *                        form and also interpret HT, NL, NP, and CR.
 *                     3: Same as 2 except that lines are broken at 80
 *                        characters.
 *                     4: Display all control codes in two-character mnemonic
 *                        form and don't interpret any of them.
 *                        Lines will be broken at 80 characters.
 *                     5: Interpret the input as a non-ASCII byte stream,
 *                        displaying each byte in hexadecimal notation.
 *                        Lines will be broken at 80 characters.
 *
 *      -D          Debug mode for page reversal, preserves temporary file
 *                  (undocumented).
 *
 *      -f<font>    Font selection.
 *
 *      -g#         Grey page banding.
 *
 *      -i#         Page indentation (cm).
 *
 *      -l          Use landscape instead of portrait format.
 *
 *      -n<file>    Use "file" as the file name in the page header if input
 *                  is from stdin.
 *
 *      -o<list>    Only pages whose page numbers appear in the comma-
 *                  separated list of numbers and ranges will be printed.  A
 *                  range "N:M" means pages N through M inclusive.  An initial
 *                  ":N" means from the beginning to page N, and a final "N:"
 *                  means from N to the end.  The default, ":", is to print
 *                  all pages.
 *
 *                  Formfeeds are inserted to maintain the correct parity when
 *                  printing in duplex mode (see the "-x" option).
 *
 *      -p<paper>   Paper selection:
 *                    -----------------------------------------------------
 *                      name          point        inch           cm
 *                    -----------------------------------------------------
 *                     A3          (842 x 1190)              29.7  x 42.0
 *                     A4          (595 x  842)              21.0  x 29.7
 *                     A5          (420 x  595)              14.82 x 21.0
 *                     B4          (729 x 1032)              25.72 x 36.41
 *                     B5          (516 x  729)              18.20 x 25.72
 *                     statement   (396 x  612)  5.5 x 8.5  (13.97 x 21.59)
 *                     executive   (540 x  720)  7.5 x 10   (19.05 x 25.40)
 *                     quarto      (610 x  780)             (21.52 x 27.52)
 *                     letter      (612 x  792)  8.5 x 11   (21.59 x 27.94)
 *                     folio       (612 x  936)  8.5 x 13   (21.59 x 33.02)
 *                     legal       (612 x 1008)  8.5 x 14   (21.59 x 35.56)
 *                     10x14       (720 x 1008)  10  x 14   (25.40 x 35.56)
 *                     tabloid     (792 x 1224)  11  x 17   (27.94 x 43.18)
 *                     ledger     (1224 x  792)  17  x 11   (43.18 x 27.94)
 *
 *                  In addition, "A4/letter" is recognized as being the width
 *                  of A4 and height of letter size paper.
 *
 *      -r          Reverse the page and file order for printing.
 *
 *      -s#         Font size in points (5 - 1000, default 10).
 *
 *      -t#         Line spacing, 1.0 = single, 2.0 = double, etc. (1.0 - 3.0)
 *                  (fractional values are allowed).
 *
 *      -u          Invert text on the front side of the page when printing
 *                  in duplex mode.  This may be used when the pages are to
 *                  be bound on the upper edge of the paper (as defined by
 *                  the orientation of the text on the front side).
 *
 *      -v          Verbose mode, informs of major steps (undocumented).
 *
 *      -x          Exchange parity when printing in duplex mode so that odd
 *                  numbered pages are printed on the reverse side of the
 *                  paper.
 *
 * If no files are specified, stdin is used.  Form feeds should work.
 * Backspacing with underlining (or overprinting) should work as expected.
 *
 * BJB - Nov/85
 * ===========================================================================
 *
 * Copyright (c) Barry Brachman
 *
 * Permission is given to freely copy or distribute this program
 * with the following restrictions:
 *
 *      1) The author is not responsible for the consequences of use of
 *              this software and is not responsible for correcting any defects
 *      2) The origin of this software must not be misrepresented, either by
 *              explicit claim or by omission
 *      3) You may not sell this program
 *      4) Altered versions must be plainly marked as such, and must not
 *              be misrepresented as being the original software
 *
 * Barry Brachman
 * Dept. of Computer Science
 * Univ. of British Columbia
 * Vancouver, B.C. V6T 1W5
 *
 * ===========================================================================
 *
 * This copy of lwf and its manual page has been substantially modified by
 * Mark Calabretta, Australia Telescope National Facility, CSIRO, Australia
 *
 * $Id$
 * ===========================================================================
 */

#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <string.h>
#include <time.h>
#include <sys/file.h>

#if defined (AIPS_SVID3)
#include <sys/utsname.h>
#endif

/* Paper sizes in cm. */
#define XSPANA3 29.70
#define YSPANA3 42.00
#define XSPANA4 21.00
#define YSPANA4 29.70
#define XSPANA5 14.82
#define YSPANA5 21.00
#define XSPANB4 25.72
#define YSPANB4 36.41
#define XSPANB5 18.20
#define YSPANB5 25.72
#define XSPANST 13.97
#define YSPANST 21.59
#define XSPANEX 19.05
#define YSPANEX 25.40
#define XSPANQT 21.52
#define YSPANQT 27.52
#define XSPANLT 21.59
#define YSPANLT 27.94
#define XSPANFO 21.59
#define YSPANFO 33.02
#define XSPANLG 21.59
#define YSPANLG 35.56
#define XSPAN10 25.40
#define YSPAN10 35.56
#define XSPANTA 27.94
#define YSPANTA 43.18
#define XSPANLD 43.18
#define YSPANLD 27.94

#define LMARGIN               1.0    /* Left margin in cm. */
#define TMARGIN               1.0    /* Top margin in cm. */
#define RMARGIN               1.0    /* Right margin in cm. */
#define BMARGIN               0.9    /* Bottom margin in cm. */
#define PINDENT               2.5    /* Page numbering indent from RH edge. */
#define HGAP                  1.0    /* Gap between header and first line. */

#define X0                    0.1    /* Print origin with respect to  */
#define Y0                    0.2    /* the BLH edge of the paper.    */

#define PSTRLEN               255    /* Maximum length of PS input line. */

#define HEAD_SIZE               7    /* Font size for the page header. */
#define BANR_SIZE              22    /* Font size for the banner page. */

#define PS_EOF                 04    /* PostScript end-of-file marker. */

double  points_per_cm = 72.0/2.54;
double  xspan, yspan, x0, y0;
int     fontSize, lineSpace;
double  spacing;
int     page_x, header_y;
int     lines_per_page;
int     colsep, start_x, start_y;

struct {
   unsigned char buf[BUFSIZ];
   int  count;
   int  formfeed;
} intext;

                                     /* Parameters for page reversal */
#define MAXPAGES            10000    /* Maximum number of pages */
char    bigbuf[32768];               /* Maximum page size */
long    pageMap[MAXPAGES];           /* Offset of first byte of each page */
int     pageNum[MAXPAGES];           /* Document page numbers */
int     pageIndex;                   /* Page map index */

int     pageNumber;                  /* Document page number */
int     pagePrevious;                /* Previous page number */
int     pageOrdinal;                 /* PostScript page sequence number */
int     pageParity;                  /* Duplex mode page parity control */
int     pageSide;                    /* 1 = front, 0 = reverse */
char    *range;

int     doBanner, doDebug, doDuplex, doLandsc, doBindTop, doReverse,
        doTumble, doVerbose;
int     ctrlCode, ctrlCols, greybands, ncols;

int     getline();
char    *progname;
char    *fontName, *paperName;
char    *fileName, *name;





main(argc, argv)
int argc;
char **argv;
{
   register int  i, j;
   int    usename;
   char   *pc;
   double  offset, temp;
   double  atof();
   FILE   *infile, *popen();

   /* Name under which this program has been invoked. */
   if ((pc = strrchr(argv[0], '/')) != NULL)
      progname = pc + 1;
   else
      progname = argv[0];

   /* Default options and flags. */
   doBanner   = 0;
   doBindTop  = 0;
   doDebug    = 0;
   doDuplex   = 1;
   doLandsc   = 0;
   doReverse  = 0;
   doVerbose  = 0;

   ctrlCode   = 1;
   ctrlCols   = 80;
   fontName   = "Courier";
   fontSize   = 10;
   greybands  = 0;
   name       = "<stdin>";
   ncols      = 1;
   offset     = LMARGIN;
   pageParity = 0;
   range      = ":";
   spacing    = 1.0;
   usename    = 0;
   paperName  = "A4";
   xspan      = XSPANA4;
   yspan      = YSPANA4;

   /* Parse user supplied options. */
   for (i = 1; i < argc && argv[i][0] == '-'; i++) {
      switch (argv[i][1]) {
      case '1':
         doDuplex = 0;
         break;
      case 'a':
         doBanner = -1;
         break;
      case 'b':
         doBanner = 1;
         break;
      case 'c':
         ncols = atoi(&argv[i][2]);
         if (ncols < 1) ncols = 1;
         break;
      case 'd':
         ctrlCode = atoi(&argv[i][2]);
         if (ctrlCode < 0 || ctrlCode > 5) ctrlCode = 1;
         break;
      case 'D':
         doDebug = 1;
         break;
      case 'f':
         fontName = &argv[i][2];

         /* Avant Garde font family. */
         if (strncmp(fontName, "A", 1) == 0) {
            if (strcmp(fontName, "AGB") == 0) {
               fontName = "AvantGarde-Book"; break; }
            if (strcmp(fontName, "AGBO") == 0) {
               fontName = "AvantGarde-BookOblique"; break; }
            if (strcmp(fontName, "AGD") == 0) {
               fontName = "AvantGarde-Demi"; break; }
            if (strcmp(fontName, "AGDO") == 0) {
               fontName = "AvantGarde-DemiOblique"; break; }
            if (strcmp(fontName, "AvantGarde-Book") == 0) break;
            if (strcmp(fontName, "AvantGarde-BookOblique") == 0) break;
            if (strcmp(fontName, "AvantGarde-Demi") == 0) break;
            if (strcmp(fontName, "AvantGarde-DemiOblique") == 0) break;
         }

         /* ITC Bookman font family. */
         if (strncmp(fontName, "B", 1) == 0) {
            if (strcmp(fontName, "BD") == 0) {
               fontName = "Bookman-Demi"; break; }
            if (strcmp(fontName, "BDI") == 0) {
               fontName = "Bookman-DemiItalic"; break; }
            if (strcmp(fontName, "BL") == 0) {
               fontName = "Bookman-Light"; break; }
            if (strcmp(fontName, "BLI") == 0) {
               fontName = "Bookman-LightItalic"; break; }
            if (strcmp(fontName, "Bookman-Demi") == 0) break;
            if (strcmp(fontName, "Bookman-DemiItalic") == 0) break;
            if (strcmp(fontName, "Bookman-Light") == 0) break;
            if (strcmp(fontName, "Bookman-LightItalic") == 0) break;
         }

         /* Courier font family. */
         if (strncmp(fontName, "C", 1) == 0) {
            if (strcmp(fontName, "C") == 0) {
               fontName = "Courier"; break; }
            if (strcmp(fontName, "CO") == 0) {
               fontName = "Courier-Oblique"; break; }
            if (strcmp(fontName, "CB") == 0) {
               fontName = "Courier-Bold"; break; }
            if (strcmp(fontName, "CBO") == 0) {
               fontName = "Courier-BoldOblique"; break; }
            if (strcmp(fontName, "Courier") == 0) break;
            if (strcmp(fontName, "Courier-Oblique") == 0) break;
            if (strcmp(fontName, "Courier-Bold") == 0) break;
            if (strcmp(fontName, "Courier-BoldOblique") == 0) break;
         }

         /* Helvetica and Helvetica-Narrow font families. */
         if (strncmp(fontName, "H", 1) == 0) {
            if (strcmp(fontName, "H") == 0) {
               fontName = "Helvetica"; break; }
            if (strcmp(fontName, "HB") == 0) {
               fontName = "Helvetica-Bold"; break; }
            if (strcmp(fontName, "HO") == 0) {
               fontName = "Helvetica-Oblique"; break; }
            if (strcmp(fontName, "HBO") == 0) {
               fontName = "Helvetica-BoldOblique"; break; }
            if (strcmp(fontName, "HN") == 0) {
               fontName = "Helvetica-Narrow"; break; }
            if (strcmp(fontName, "HNB") == 0) {
               fontName = "Helvetica-Narrow-Bold"; break; }
            if (strcmp(fontName, "HNO") == 0) {
               fontName = "Helvetica-Narrow-Oblique"; break; }
            if (strcmp(fontName, "HNBO") == 0) {
               fontName = "Helvetica-Narrow-BoldOblique"; break; }
            if (strcmp(fontName, "Helvetica") == 0) break;
            if (strcmp(fontName, "Helvetica-Bold") == 0) break;
            if (strcmp(fontName, "Helvetica-Oblique") == 0) break;
            if (strcmp(fontName, "Helvetica-BoldOblique") == 0) break;
            if (strcmp(fontName, "Helvetica-Narrow") == 0) break;
            if (strcmp(fontName, "Helvetica-Narrow-Bold") == 0) break;
            if (strcmp(fontName, "Helvetica-Narrow-Oblique") == 0) break;
            if (strcmp(fontName, "Helvetica-Narrow-BoldOblique") == 0) break;
         }

         /* New Century Schoolbook font family. */
         if (strncmp(fontName, "N", 1) == 0) {
            if (strcmp(fontName, "NCSR") == 0) {
               fontName = "NewCenturySchlbk-Roman"; break; }
            if (strcmp(fontName, "NCSB") == 0) {
               fontName = "NewCenturySchlbk-Bold"; break; }
            if (strcmp(fontName, "NCSI") == 0) {
               fontName = "NewCenturySchlbk-Italic"; break; }
            if (strcmp(fontName, "NCSBI") == 0) {
               fontName = "NewCenturySchlbk-BoldItalic"; break; }
            if (strcmp(fontName, "NewCenturySchlbk-Roman") == 0) break;
            if (strcmp(fontName, "NewCenturySchlbk-Bold") == 0) break;
            if (strcmp(fontName, "NewCenturySchlbk-Italic") == 0) break;
            if (strcmp(fontName, "NewCenturySchlbk-BoldItalic") == 0) break;
         }

         /* Palatino font family. */
         if (strncmp(fontName, "P", 1) == 0) {
            if (strcmp(fontName, "PR") == 0) {
               fontName = "Palatino-Roman"; break; }
            if (strcmp(fontName, "PB") == 0) {
               fontName = "Palatino-Bold"; break; }
            if (strcmp(fontName, "PI") == 0) {
               fontName = "Palatino-Italic"; break; }
            if (strcmp(fontName, "PBI") == 0) {
               fontName = "Palatino-BoldItalic"; break; }
            if (strcmp(fontName, "Palatino-Roman") == 0) break;
            if (strcmp(fontName, "Palatino-Bold") == 0) break;
            if (strcmp(fontName, "Palatino-Italic") == 0) break;
            if (strcmp(fontName, "Palatino-BoldItalic") == 0) break;
         }

         /* Times font family. */
         if (strncmp(fontName, "T", 1) == 0) {
            if (strcmp(fontName, "TR") == 0) {
               fontName = "Times-Roman"; break; }
            if (strcmp(fontName, "TB") == 0) {
               fontName = "Times-Bold"; break; }
            if (strcmp(fontName, "TI") == 0) {
               fontName = "Times-Italic"; break; }
            if (strcmp(fontName, "TBI") == 0) {
               fontName = "Times-BoldItalic"; break; }
            if (strcmp(fontName, "Times-Roman") == 0) break;
            if (strcmp(fontName, "Times-Bold") == 0) break;
            if (strcmp(fontName, "Times-Italic") == 0) break;
            if (strcmp(fontName, "Times-BoldItalic") == 0) break;
         }

         /* Zapf Chancery font family. */
         if (strncmp(fontName, "Z", 1) == 0) {
            if (strcmp(fontName, "ZCMI") == 0) {
               fontName = "ZapfChancery-MediumItalic"; break; }
            if (strcmp(fontName, "ZapfChancery-MediumItalic") == 0) break;
         }

         fprintf(stderr, "%s: Unknown font, %s.\n", progname, fontName);
         exit(1);
      case 'g':
         greybands = atoi(&argv[i][2]);
         break;
      case 'i':
         offset = atof(&argv[i][2]);
         break;
      case 'l':
         doLandsc = 1;
         break;
      case 'n':
         name = &argv[i][2];
         usename = 1;
         break;
      case 'o':
         range = &argv[i][2];
         if (checkrange(range)) {
            fprintf(stderr, "%s: Invalid range specification, %s.\n",
                    progname, range);
            exit(1);
         }
         break;
      case 'p':
         paperName = &argv[i][2];
         if (strcmp(paperName, "A3") == 0) {
            xspan = XSPANA3;
            yspan = YSPANA3;
            break;
         }
         if (strcmp(paperName, "A4") == 0) {
            xspan = XSPANA4;
            yspan = YSPANA4;
            break;
         }
         if (strcmp(paperName, "A5") == 0) {
            xspan = XSPANA5;
            yspan = YSPANA5;
            break;
         }
         if (strcmp(paperName, "B4") == 0) {
            xspan = XSPANB4;
            yspan = YSPANB4;
            break;
         }
         if (strcmp(paperName, "B5") == 0) {
            xspan = XSPANB5;
            yspan = YSPANB5;
            break;
         }
         if (strcmp(paperName, "A4/letter") == 0) {
            xspan = XSPANA4;
            yspan = YSPANLT;
            break;
         }
         if (strcmp(paperName, "statement") == 0) {
            xspan = XSPANST;
            yspan = YSPANST;
            break;
         }
         if (strcmp(paperName, "executive") == 0) {
            xspan = XSPANEX;
            yspan = YSPANEX;
            break;
         }
         if (strcmp(paperName, "quarto") == 0) {
            xspan = XSPANQT;
            yspan = YSPANQT;
            break;
         }
         if (strcmp(paperName, "letter") == 0) {
            xspan = XSPANLT;
            yspan = YSPANLT;
            break;
         }
         if (strcmp(paperName, "folio") == 0) {
            xspan = XSPANFO;
            yspan = YSPANFO;
            break;
         }
         if (strcmp(paperName, "legal") == 0) {
            xspan = XSPANLG;
            yspan = YSPANLG;
            break;
         }
         if (strcmp(paperName, "10x14") == 0) {
            xspan = XSPAN10;
            yspan = YSPAN10;
            break;
         }
         if (strcmp(paperName, "tabloid") == 0) {
            xspan = XSPANTA;
            yspan = YSPANTA;
            break;
         }
         if (strcmp(paperName, "ledger") == 0) {
            xspan = XSPANLD;
            yspan = YSPANLD;
            break;
         }
         break;
      case 'r':
         doReverse = 1;
         break;
      case 's':
         fontSize = atoi(&argv[i][2]);
         if (fontSize < 5 || fontSize > 1000) {
            fprintf(stderr, "%s: Invalid font size (%d) specified.\n",
                    progname, fontSize);
            exit(1);
         }
         break;
      case 't':
         spacing = atof(&argv[i][2]);
         if (spacing < 1.0 || spacing > 3.0) {
            fprintf(stderr, "%s: Invalid line spacing (%d) specified.\n",
                    progname, spacing);
            exit(1);
         }
         break;
      case 'u':
         doBindTop = 1;
         break;
      case 'v':
         doVerbose = 1;
         break;
      case 'x':
         pageParity = 1;
         break;
      default:
         fprintf(stderr, "%s: Unrecognized option '%c'.\n",
            progname, argv[i][1]);
         exit(1);
      }
   }

   if (doLandsc) {
      doTumble = !doBindTop;
   } else {
      doTumble = doBindTop;
   }

   if (doReverse) {
      pageParity = !pageParity;
   }

   /* Check accessibility of all requested files. */
   for (j = i; j < argc; j++) {
      if (access(argv[j], R_OK) == -1) {
         fprintf(stderr, "%s: Cannot access %s.\n", progname, argv[j]);
         exit(1);
      }
   }

   /* Get font and page size parameters. */
   if (!doLandsc) {
      /* Portrait format. */
      x0    = X0;
      y0    = Y0;
   }
   else {
      /* Landscape format. */
      temp  = xspan;
      xspan = yspan;
      yspan = temp;
      x0    = Y0;
      y0    = X0;
   }

   page_x   = (int) ((xspan - x0 - PINDENT) * points_per_cm);
   header_y = (int) ((yspan - y0 - TMARGIN) * points_per_cm) - HEAD_SIZE;
   lineSpace = (int)(fontSize * spacing);
   lines_per_page = ((int) ((yspan - TMARGIN - BMARGIN - HGAP)
      * points_per_cm) - HEAD_SIZE + lineSpace - fontSize) / lineSpace;
   if (offset < 0.0 || offset >= xspan) {
     fprintf(stderr, "%s: Bad indentation requested.\n", progname);
     exit(1);
   }

   start_x  = (int) ((offset - x0) * points_per_cm);
   start_y  = header_y - (int) (HGAP * points_per_cm) - fontSize;
   colsep   = (int) (((xspan - (offset - x0)) / ncols) * points_per_cm);

   /* Setup the PostScript header. */
   if (i == argc) {
      fileName = "<stdin>";
   } else {
      fileName = &argv[i][0];
   }
   preamble();

   if (i == argc) {        /* no files on command line */
      infile = stdin;
      if (doVerbose) fprintf(stderr, "%s: Printing stdin.\n", progname);
      print(infile);
   } else {
      j = argc;
      for (; i < argc; i++) {
         /* Print files in reverse order if required. */
         doReverse ? (j--) : (j = i);

         infile = stdin;
         if (freopen(argv[j], "r", stdin) == NULL) {
            fprintf(stderr, "%s: Can't open %s.\n", progname, argv[j]);
            exit(1);
         }

         if (! usename) name = &argv[j][0];

         if (doVerbose) {
            fprintf(stderr, "%s: Printing %s.\n", progname, argv[j]);
         }
         print(infile);
      }
   }

   printf("%%%%Trailer\n");
   printf("userdict /end-hook known {end-hook} if\n");
   printf("%%%%Pages: %d\n", pageOrdinal);
   printf("%%%%EOF\n");
   exit(0);
}





/* Initial lines sent to the PostScript device.
 */
preamble()
{
   register char *user;
   int  end_x;
   int  border_x0, border_y0;
   int  border_x1, border_y1;
   int  border_x2, border_y2;
   int  border_x3, border_y3;
   int  banner_start_x, banner_start_y;
   int  band_n, band_x, band_y;
   char adate[32], host[20];
   char *ctime(), *getlogin();
   time_t clock;
   struct tm *tmstruct;
   struct passwd *pw;
#if defined (AIPS_SVID3)
   struct utsname hostid;
#endif

/* Determine the time. */
   time(&clock);
   tmstruct = localtime(&clock);
   strftime(adate, 32, "%a %Y/%m/%d %T %Z", tmstruct);

/* User. */
   if ((user = getlogin()) == (char *) NULL) {
      if ((pw = getpwuid(getuid())) == NULL)
         user = "unknown";
      else
         user = pw->pw_name;
         endpwent();
   }

/* Host. */
#if defined (AIPS_SVID3)
   uname(&hostid);
   strcpy(host, hostid.nodename);
#else
   gethostname(host, sizeof(host));
#endif

/* Conforming PostScript header comments. */
   printf("%%!PS-Adobe-3.0\n");
   printf("%%%%Creator: lwf 2.20 Barry Brachman & Mark Calabretta, 1985-95\n");
   printf("%%%%CreationDate: %s\n", adate);
   printf("%%%%For: %s@%s\n", user, host);
   printf("%%%%Title: %s\n", fileName);
   printf("%%%%Pages: (atend)\n");
   printf("%%%%PageOrder: %s\n", doReverse ? "Descend" : "Ascend");
   printf("%%%%DocumentNeededResources: font %s", fontName);
   if (doBanner == 1) {
      printf(" Courier-BoldOblique\n");
   } else {
      printf("\n");
   }
   printf("%%%%Orientation: %s\n", doLandsc ? "Landscape" : "Portrait");
   printf("%%%%BoundingBox: 0 0 %.0f %.0f\n",
      (xspan/2.54)*72.0, (yspan/2.54)*72.0);
   printf("%%%%LanguageLevel: 1\n");
   printf("%%%%EndComments\n\n");

   printf("%%%%BeginProlog\n");
   printf("%%%%BeginResource: procset lwfSetup\n");

   printf("   /Setup {\n");
   printf("      userdict /start-hook known {start-hook} if\n");

   if (strcmp(paperName, "A3") == 0) {
      /* Select A3 size paper tray. */
      printf("      statusdict /a3tray known {\n");
      printf("         {statusdict begin\n");
      printf("            a3tray\n");
      printf("         end} stopped {} if\n");
      printf("      } if\n");
   } else if (strcmp(paperName, "A4") == 0) {
      /* Select A4 size paper tray. */
      printf("      statusdict /a4tray known {\n");
      printf("         {statusdict begin\n");
      printf("            a4tray\n");
      printf("         end} stopped {} if\n");
      printf("      } if\n");
   } else if (strcmp(paperName, "letter") == 0) {
      /* Select letter size paper tray. */
      printf("      statusdict /lettertray known {\n");
      printf("         {statusdict begin\n");
      printf("            lettertray\n");
      printf("         end} stopped {} if\n");
      printf("      } if\n");
   } else if (strcmp(paperName, "legal") == 0) {
      /* Select legal size paper tray. */
      printf("      statusdict /legaltray known {\n");
      printf("         {statusdict begin\n");
      printf("            legaltray\n");
      printf("         end} stopped {} if\n");
      printf("      } if\n");
   } else if (strcmp(paperName, "executive") == 0) {
      /* Select executive size paper tray. */
      printf("      statusdict /executivetray known {\n");
      printf("         {statusdict begin\n");
      printf("            executivetray\n");
      printf("         end} stopped {} if\n");
      printf("      } if\n");
   }

   if (doDuplex) {
      /* Duplex mode setup. */
      printf("      /DUPLEX statusdict /setduplexmode known def\n");
      printf("      DUPLEX {\n");
      printf("         statusdict begin\n");
      printf("            true setduplexmode\n");
      if (doTumble) {
         printf("            statusdict /settumble known {\n");
         printf("               true settumble\n");
         printf("            } if\n");
      }
      printf("         end\n");
      printf("      } if\n");
   } else {
      /* Override the printer default setting. */
      printf("      statusdict /setduplexmode known {\n");
      printf("         statusdict begin\n");
      printf("            false setduplexmode\n");
      printf("         end\n");
      printf("      } if\n");
   }
   printf("   } def\n\n");


   if (doDuplex) {
      /* Only required for duplex mode printing. */
      printf("   /FormFeed {\n");
      printf("      showpage\n");
      printf("   } def\n\n");
   }

   printf("   /PageSetup {\n");
   printf("      /VMlwf save def\n");
   printf("      userdict /bop-hook known {bop-hook} if\n");
   if (doLandsc) {
      printf("      Landscape\n");
   }
   printf("   } def\n\n");

   if (doLandsc) {
      end_x = (int) ((yspan - y0) * points_per_cm);
      printf("   /Landscape {\n");
      printf("      %d 0 translate\n", end_x);
      printf("      90 rotate\n");
      printf("   } def\n", end_x);
   }
   printf("%%%%EndResource\n\n");

   printf("%%%%BeginResource: procset lwfProcedures\n");
   if (doBanner == 1) {
      /* Procedures for printing the banner page. */
      border_x0 = (int) ((LMARGIN - x0) * points_per_cm);
      border_y0 = (int) ((BMARGIN - y0) * points_per_cm);
      border_x1 = (int) ((xspan - x0 - RMARGIN) * points_per_cm);
      border_y1 = (int) ((yspan - y0 - TMARGIN) * points_per_cm);
      border_x2 = border_x0 + 10;
      border_y2 = border_y0 + 10;
      border_x3 = border_x1 - 10;
      border_y3 = border_y1 - 10;
      banner_start_x = border_x2 + 36;
      banner_start_y = (int) ((border_y1 - border_y0)/2) + (4 * BANR_SIZE);

      printf("   /Banner {\n");
      printf("      BanrFont setfont\n");
      printf("      newpath\n");
      printf("      8 setlinewidth\n");
      printf("      %d %d moveto\n", border_x0, border_y0);
      printf("      %d %d lineto\n", border_x0, border_y1);
      printf("      %d %d lineto\n", border_x1, border_y1);
      printf("      %d %d lineto\n", border_x1, border_y0);
      printf("      closepath stroke\n");
      printf("      2 setlinewidth\n");
      printf("      %d %d moveto\n", border_x2, border_y2);
      printf("      %d %d lineto\n", border_x2, border_y3);
      printf("      %d %d lineto\n", border_x3, border_y3);
      printf("      %d %d lineto\n", border_x3, border_y2);
      printf("      closepath stroke\n");
      printf("      %d %d moveto\n", banner_start_x, banner_start_y);
      printf("      (Date: ) show TIME b\n");
      printf("      (Host: ) show HOST b\n");
      printf("      (User: ) show USER b\n");
      printf("      (Name: ) show NAME b\n");
      printf("   } def\n\n");

      printf("   /b {\n");
      printf("      show\n");
      printf("      %d currentpoint exch pop\n", banner_start_x);
      printf("      %d sub moveto\n", 2 * BANR_SIZE);
      printf("   } def\n\n");
   }
   else {
      /* Procedure for printing the page header. */
      printf("   /PageHead {\n");
      printf("      HeadFont setfont\n");
      printf("      %d %d moveto\n", start_x, header_y);
      printf("      TIME show\n");
      printf("      (   Host: ) show HOST show\n");
      printf("      (   User: ) show USER show\n");
      printf("      (   Name: ) show NAME show\n");
      printf("      %d %d moveto\n", page_x, header_y);
      printf("      (Page ) show PAGE 5 string cvs show\n");
      printf("   } def\n\n");
   }

   /* Procedure for starting a new page. */
   printf("   /NewPage {\n");
   printf("      /PAGE exch def\n");
   if (greybands) {
      printf("      PageBand\n");
   }
   if (doBanner == 0) {
      printf("      PageHead\n");
   }
   printf("      TextFont setfont\n");
   printf("      %d %d translate\n", start_x, start_y);
   printf("      0 0 moveto\n");
   printf("   } def\n\n");

   if (greybands) {
      /* Procedures for printing grey page banding. */
      band_x = xspan * points_per_cm;
      band_y = greybands * lineSpace;
      band_n = (lines_per_page + greybands - 1) / (2 * greybands);

      printf("   /GreyBand {\n");
      printf("      newpath\n");
      printf("      0 0 moveto\n");
      printf("      0 %d rlineto\n", band_y);
      printf("      %d 0 rlineto\n", band_x);
      printf("      0 %d rlineto\n", -band_y);
      printf("      closepath fill\n");
      printf("      } def\n");

      printf("   /PageBand {\n");
      printf("      gsave\n");
      printf("      0.98 setgray\n");
      printf("      0 %d translate\n", start_y + (2 * lineSpace)/3);
      printf("      %d {0 %d translate GreyBand} repeat\n", band_n,
                    -2*band_y);
      printf("      grestore\n");
      printf("   } def\n\n");
   }

   if (ncols > 1) {
      /* Procedure for finishing a column. */
      printf("   /EndCol {\n");
      printf("      %d 0 translate\n", colsep);
      printf("      0 0 moveto\n");
      printf("   } def\n\n");
   }

   /* Procedure for finishing a page. */
   printf("   /EndPage {\n");
   printf("      userdict /eop-hook known {eop-hook} if\n");
   printf("      VMlwf restore\n");
   printf("      showpage\n");
   printf("   } def\n\n");

   /* Procedure for printing on the current line. */
   printf("   /s {\n");
   printf("      show\n");
   printf("   } def\n\n");

   /* Procedure for backspacing. */
   printf("   /bs {\n");
   printf("      stringwidth pop neg 0 rmoveto\n");
   printf("   } def\n\n");

   /* Procedure for printing text and moving to the next line. */
   printf("   /p {\n");
   printf("      show\n");
   printf("      0 currentpoint exch pop %d sub moveto\n", lineSpace);
   printf("   } def\n\n");

   /* Procedure for displaying a control code. */
   printf("   /c {\n");
   printf("      0.5 0.5 scale\n");
   printf("      0 %d rmoveto\n", fontSize/2);
   printf("      dup 0 1 getinterval show\n");
   printf("      0 %d rmoveto\n", -fontSize/2);
   printf("      1 1 getinterval show\n");
   printf("      2.0 2.0 scale\n");
   printf("   } def\n");
   printf("%%%%EndResource\n\n");

   printf("%%%%BeginResource: procset lwfStaticVariables\n");
   /* Assign fonts to variables so they will be retained. */
   printf("   /TextFont /%s findfont %d scalefont def\n", fontName, fontSize);
   if (doBanner == 1) {
      printf("   /BanrFont /Courier-BoldOblique findfont %d scalefont def\n\n",
              BANR_SIZE);
   }
   else {
      printf("   /HeadFont /%s findfont %d scalefont def\n\n", fontName,
              HEAD_SIZE);
   }

   /* Assign PostScript variables. */
   printf("   /TIME (%s) def\n", adate);
   printf("   /HOST (%s) def\n", host);
   printf("   /USER (%s) def\n", user);

   printf("%%%%EndResource\n");
   printf("%%%%EndProlog\n\n");

   printf("%%%%BeginSetup\n");
   printf("Setup\n");
   printf("%%%%EndSetup\n");
}





/* Print a file.
 *
 * The input stream may be stdin, a file, or a pipe.  If the document is to
 * be printed in reverse order the output goes to a temporary file, and then
 * reverse() is called to do the page reversal to stdout.
 */
print(infile)
FILE *infile;
{
   register int eof, r;
   register char *p;
   char revfile[20];
   FILE *outfile;
   char *gets(), *mktemp();


   pageIndex = 0;
   pageMap[pageIndex] = 0L;

   pageOrdinal = 0;
   pageSide = 1;

   printf("\n\n");

   if (doBanner == 1) {
      if (! doReverse) {
         printf("%%%%Page: 0 %d\n", ++pageOrdinal);
         printf("%%%%BeginPageSetup\n");
         printf("/NAME (%s) def\n", name);
         printf("PageSetup\n");
         printf("%%%%EndPageSetup\n\n");
         printf("Banner\n");
         printf("EndPage\n\n");
         printf("%%%%PageTrailer\n\n");
         pageSide = !pageSide;

         if (doDuplex) {
            printf("%%%%Page: - %d\n", ++pageOrdinal);
            printf("DUPLEX {FormFeed} if\n\n");
            printf("%%%%PageTrailer\n\n");
            pageSide = !pageSide;
         }
      }
   }

   if (doReverse) {
      sprintf(revfile, "/tmp/%sXXXXXX", progname);
      p = mktemp(revfile);
      if (doVerbose) {
         fprintf(stderr, "%s: Scratch file will be %s.\n", progname, p);
      }

      if ((outfile = fopen(p, "w+")) == (FILE *) NULL) {
         fprintf(stderr, "%s: Can't create %s\n", progname, p);
         exit(1);
      }

      if (!doDebug)
         unlink(p);
      else
         fprintf(stderr, "%s: Preserving %s for debugging purposes.\n",
                 progname, p);
   } else {
      outfile = stdout;
   }

   pagePrevious = 0;
   pageNumber = 1;
   eof = 0;
   while (!eof) {
      if ((r = inrange(pageNumber, range)) == -1) {
         exit(1);
      }
      else if (r == 1)
         eof = printpage(infile, outfile);
      else if (r == 0)
         eof = flushpage(infile);
      else {
         fprintf(stderr, "%s: Page range error.\n", progname);
         exit(1);
      }
      pageNumber++;
   }

   if (doReverse) {
      fflush(stdout);
      fflush(outfile);
      reverse(outfile);
      fclose(outfile);
      if (doBanner == 1) {
         if (doDuplex) {
            printf("%%%%Page: - %d\n", ++pageOrdinal);
            printf("DUPLEX {FormFeed} if\n\n");
            printf("%%%%PageTrailer\n\n");
            pageSide = !pageSide;

            if (pageSide) {
               printf("%%%%Page: - %d\n", ++pageOrdinal);
               printf("DUPLEX {FormFeed} if\n\n");
               printf("%%%%PageTrailer\n\n");
               pageSide = !pageSide;
            }
         }
         printf("%%%%Page: 0 %d\n", ++pageOrdinal);
         printf("%%%%BeginPageSetup\n");
         printf("/NAME (%s) def\n", name);
         printf("PageSetup\n");
         printf("%%%%EndPageSetup\n\n");
         printf("Banner\n\n");
         printf("EndPage\n\n");
         printf("%%%%PageTrailer\n\n");
         pageSide = !pageSide;
      }
   }
}





/* Process the next page.
 */
printpage(infile, outfile)
FILE *infile, *outfile;
{
   register int icol, lineno;

   for (icol = 0; icol < ncols; icol++) {
      for (lineno = 0; lineno < lines_per_page; lineno++) {
         if (getline(infile) == 0) {
            if (icol > 0 || lineno > 0) endpage(outfile);
            return(1);
         }

         if (icol == 0 && lineno == 0) {
            if (! doReverse) {
               if (doDuplex) {
                  if (!pageSide && (pageNumber-pagePrevious) > 1) {
                     printf("%%%%Page: - %d\n", ++pageOrdinal);
                     printf("DUPLEX {FormFeed} if\n\n");
                     printf("%%%%PageTrailer\n\n");
                     pageSide = !pageSide;
                  }

                  if ((pageNumber+pageParity)%2 != pageSide) {
                     printf("%%%%Page: - %d\n", ++pageOrdinal);
                     printf("DUPLEX {FormFeed} if\n\n");
                     printf("%%%%PageTrailer\n\n");
                     pageSide = !pageSide;
                  }
               }

               printf("%%%%Page: %d %d\n", pageNumber, ++pageOrdinal);
               printf("%%%%BeginPageSetup\n");
               printf("/NAME (%s) def\n", name);
               printf("PageSetup\n");
               printf("%%%%EndPageSetup\n\n");
               printf("%d NewPage\n", pageNumber);
               pagePrevious = pageNumber;
               pageSide = !pageSide;
            } else {
               pageNum[pageIndex] = pageNumber;
               printf("%%%%BeginPageSetup\n");
               printf("/NAME (%s) def\n", name);
               printf("PageSetup\n");
               printf("%%%%EndPageSetup\n\n");
               fprintf(outfile, "%d NewPage\n", pageNumber);
            }
         }

         PStext(outfile);
         if (intext.formfeed) break;
      }

      if (icol + 1 < ncols) fprintf(outfile, "EndCol\n\n");
   }

   endpage(outfile);
   return(0);
}





/* The next page will not be printed, just consume the input and discard.
 */
flushpage(infile)
FILE *infile;
{
   register int icol, lineno;

   for (icol = 0; icol < ncols; icol++) {
      for (lineno = 0; lineno < lines_per_page; lineno++) {
         if (getline(infile) == 0) {
            return(1);
         }
         if (intext.formfeed) break;
      }
   }
   return(0);
}





/* A page has been written to the temp file.
 * Record the start of the next page.
 */
endpage(outfile)
FILE *outfile;
{
   long ftell();

   if (pageIndex == MAXPAGES) {
      fprintf(stderr, "%s: Page limit (%d) reached.\n", progname, MAXPAGES);
      exit(1);
   }
   fprintf(outfile, "EndPage\n\n");
   fprintf(outfile, "%%%%PageTrailer\n\n");
   fflush(outfile);
   pageMap[++pageIndex] = ftell(outfile);
   if (doVerbose) fprintf(stderr, "x");
}





/* Print the pages to stdout in reverse order.
 */
reverse(outfile)
FILE *outfile;
{
   register int fd, i, nbytes;

   if (doVerbose) {
      fprintf(stderr, "\n%s: Reversing %d page%s.\n", progname,
                      pageIndex, pageIndex > 1 ? "s" : "");
   }
   fd = fileno(outfile);

   if (doDebug) {
      for (i = 0; i <= pageIndex; i++)
         fprintf(stderr, "[%ld]\n", pageMap[i]);
   }

   pageOrdinal = 0;
   pagePrevious = pageNum[pageIndex-1];

   for (i = pageIndex - 1; i >= 0; i--) {
      if (lseek(fd, pageMap[i], 0) == -1) {
         fprintf(stderr, "%s: lseek error.\n", progname);
         exit(1);
      }
      nbytes = pageMap[i + 1] - pageMap[i];
      if (nbytes > sizeof(bigbuf)) {
         fprintf(stderr, "%s: Page too big, nbytes = %d.\n",
                 progname, nbytes);
         exit(1);
      }
      nbytes = read(fd, bigbuf, nbytes);

      pageNumber = pageNum[i];

      if (doDuplex) {
         if (!pageSide && (pagePrevious-pageNumber) > 1) {
            printf("%%%%Page: - %d\n", ++pageOrdinal);
            printf("DUPLEX {FormFeed} if\n\n");
            printf("%%%%PageTrailer\n\n");
            pageSide = !pageSide;
         }

         if ((pageNumber+pageParity)%2 != pageSide) {
            printf("%%%%Page: - %d\n", ++pageOrdinal);
            printf("DUPLEX {FormFeed} if\n\n");
            printf("%%%%PageTrailer\n\n");
            pageSide = !pageSide;
         }
      }
      printf("%%%%Page: %d %d\n", pageNumber, ++pageOrdinal);
      pagePrevious = pageNumber;
      pageSide = !pageSide;
      fflush(stdout);
      write(1, bigbuf, nbytes);
   }
}





/* Encode a line of ASCII text in PostScript. */
PStext(outfile)
FILE *outfile;
{
   register int i, j, k, l;
   register unsigned char *p, *savep;
   int      currentp, doCode;

   currentp = 0;

   p = intext.buf;

   fprintf(outfile, "(");
   k = 0;
   l = 1;

   for (j = 0; j < intext.count; j++, p++) {

      if (ctrlCode == 5) {
         /* Hexadecimal representation. */ 
         fprintf(outfile, "%.2x) c (", *p);
         l += 7;
      } else {
         if (ctrlCode != 0) {
            if (*p <= 037 || *p >= 0177) {
               /* Display control codes. */
               doCode = 1;

               if (ctrlCode == 1) {
                  switch (*p) {
                  case '\000':  /* NUL - null character */
                  case '\010':  /* BS - backspace */
                  case '\011':  /* HT - horizontal tab */
                  case '\012':  /* NL - new line (linefeed) */
                  case '\014':  /* NP - new page (formfeed) */
                  case '\015':  /* CR - carriage return */
                     doCode = 0;
                     break;
                  }
               }

               if (doCode) {
                  if (k != 0) {
                     fprintf(outfile, ") s (");
                     k = 0;
                     l += 5;
                  }
      
                  if (*p < '\010') {
                     switch (*p) {
                     case '\000':
                        /* NUL - null character */
                        fprintf(outfile, "NU) c (");
                        break;
                     case '\001':
                        /* SOH - start of heading */
                        fprintf(outfile, "SH) c (");
                        break;
                     case '\002':
                        /* STX - start of text */
                        fprintf(outfile, "SX) c (");
                        break;
                     case '\003':
                        /* ETX - end of text */
                        fprintf(outfile, "EX) c (");
                        break;
                     case '\004':
                        /* EOT - end of transmission */
                        fprintf(outfile, "ET) c (");
                        break;
                     case '\005':
                        /* ENQ - enquiry */
                        fprintf(outfile, "EQ) c (");
                        break;
                     case '\006':
                        /* ACK - acknowledge */
                        fprintf(outfile, "AK) c (");
                        break;
                     case '\007':
                        /* BEL - bell */
                        fprintf(outfile, "BL) c (");
                        break;
                     }
                  } else if (*p < '\020') {
                     switch (*p) {
                     case '\010':
                        /* BS - backspace */
                        fprintf(outfile, "BS) c (");
                        break;
                     case '\011':
                        /* HT - horizontal tab */
                        fprintf(outfile, "HT) c (");
                        break;
                     case '\012':
                        /* NL - new line (linefeed) */
                        fprintf(outfile, "NL) c (");
                        break;
                     case '\013':
                        /* VT - vertical tab */
                        fprintf(outfile, "VT) c (");
                        break;
                     case '\014':
                        /* NP - new page (formfeed) */
                        fprintf(outfile, "NP) c (");
                        break;
                     case '\015':
                        /* CR - carriage return */
                        fprintf(outfile, "CR) c (");
                        break;
                     case '\016':
                        /* SO - shift out */
                        fprintf(outfile, "SO) c (");
                        break;
                     case '\017':
                        /* SI - shift in */
                        fprintf(outfile, "SI) c (");
                        break;
                     }
                  } else if (*p < '\030') {
                     switch (*p) {
                     case '\020':
                        /* DLE - data link escape */
                        fprintf(outfile, "DL) c (");
                        break;
                     case '\021':
                        /* DC1 - device control 1 */
                        fprintf(outfile, "D1) c (");
                        break;
                     case '\022':
                        /* DC2 - device control 2 */
                        fprintf(outfile, "D2) c (");
                        break;
                     case '\023':
                        /* DC3 - device control 3 */
                        fprintf(outfile, "D3) c (");
                        break;
                     case '\024':
                        /* DC4 - device control 4 */
                        fprintf(outfile, "D4) c (");
                        break;
                     case '\025':
                        /* NAK - negative acknowledge */
                        fprintf(outfile, "NK) c (");
                        break;
                     case '\026':
                        /* SYN - synchonous idle */
                        fprintf(outfile, "SY) c (");
                        break;
                     case '\027':
                        /* ETB - end of transmission block */
                        fprintf(outfile, "EB) c (");
                        break;
                     }
                  } else {
                     switch (*p) {
                     case '\030':
                        /* CAN - cancel */
                        fprintf(outfile, "CN) c (");
                        break;
                     case '\031':
                        /* EM - end of medium */
                        fprintf(outfile, "EM) c (");
                        break;
                     case '\032':
                        /* SUB - substitute */
                        fprintf(outfile, "SB) c (");
                        break;
                     case '\033':
                        /* ESC - escape */
                        fprintf(outfile, "ES) c (");
                        break;
                     case '\034':
                        /* FS - file separator */
                        fprintf(outfile, "FS) c (");
                        break;
                     case '\035':
                        /* GS - group separator */
                        fprintf(outfile, "GS) c (");
                        break;
                     case '\036':
                        /* RS - record separator */
                        fprintf(outfile, "RS) c (");
                        break;
                     case '\037':
                        /* US - unit separator */
                        fprintf(outfile, "US) c (");
                        break;
                     case '\177':
                        /* DEL - delete */
                        fprintf(outfile, "DE) c (");
                        break;
                     default:
                        /* Print eight-bit control codes in hex */ 
                        fprintf(outfile, "%.2x) c (", *p);
                        break;
                     }
                  }
   
                  l += 7;
                  currentp++;
               }
            }
         }

         if (ctrlCode <= 3) {
            if (*p == '\010') {
               /* Backspace */
               if (ctrlCode != 2 && ctrlCode != 3) {
                  savep = p++;
                  for (i = 1; *p == '\010'; p++) i++;
                  if (k != 0) {
                     fprintf(outfile, ") s (");
                     k = 0;
                     l += 5;
                  }

                  fprintf(outfile, "%.*s) bs (", i, savep - i);
                  l += i + 6;
                  currentp -= i;
                  p--;
               }
            } else if (*p == '\011') {
               /* Horizontal Tab */
               i = 8 - (currentp % 8);
               fprintf(outfile, "%*s", i, "");
               k += i;
               l += i;
               currentp += i;
            } else if (*p == '\015') {
               /* Carriage Return */
               fprintf(outfile, "\\r");
               k += 2;
               l += 2;
               currentp = 0;
            }
         }

         /* PostScript escapes */
         if (*p == '\\') {
            /* Backslash */
            fprintf(outfile, "\\\\");
            k += 2;
            l += 2;
            currentp++;
         } else if (*p == '(') {
            /* Left Parenthesis */
            fprintf(outfile, "\\(");
            k += 2;
            l += 2;
            currentp++;
         } else if (*p == ')') {
            /* Right Parenthesis */
            fprintf(outfile, "\\)");
            k += 2;
            l += 2;
            currentp++;
         } else if (*p >= 040 && *p < 0177) {
            fprintf(outfile, "%c", *p);
            k += 1;
            l += 1;
            currentp++;
         }
      }

      if (l > PSTRLEN-15) {
         fprintf(outfile, "\\\n");
         l = 0;
      }
   }

   fprintf(outfile, ") p\n");
}





int getline(iop)
register FILE *iop;
{
   register int ch;
   register unsigned char *cs;


   cs = intext.buf;
   intext.count = 0;
   intext.formfeed = 0;

   while (intext.count < sizeof(intext.buf)) {
      if ((ch = getc(iop)) == EOF) {
         break;
      }

      *cs++ = ch;
      intext.count++;

      if (ctrlCode >= 3) {
         if (intext.count >= ctrlCols) {
            break;
         }
      } else {
         if (ch == '\n') {
            break;
         }

         if (ch == '\f') {
            /* Bind a newline following a formfeed to the formfeed. */
            if ((ch = getc(iop)) == '\n') {
               *cs++ = ch;
               intext.count++;
            } else {
               ungetc(ch, iop);
            }

            intext.formfeed = 1;
            break;
         }
      }
   }

   return(intext.count);
}
