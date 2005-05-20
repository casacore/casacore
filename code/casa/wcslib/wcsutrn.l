/*============================================================================
*
*   WCSLIB 4.1 - an implementation of the FITS WCS standard.
*   Copyright (C) 1995-2005, Mark Calabretta
*
*   WCSLIB is free software; you can redistribute it and/or modify it under
*   the terms of the GNU General Public License as published by the Free
*   Software Foundation; either version 2 of the License, or (at your option)
*   any later version.
*
*   WCSLIB is distributed in the hope that it will be useful, but WITHOUT ANY
*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
*   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
*   details.
*
*   You should have received a copy of the GNU General Public License along
*   with WCSLIB; if not, write to the Free Software Foundation, Inc.,
*   59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta
*                      Australia Telescope National Facility, CSIRO
*                      PO Box 76
*                      Epping NSW 1710
*                      AUSTRALIA
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=============================================================================
*
*   wcsutrn.l is a Flex description file containing the definition of a
*   lexical scanner that translates non-standard FITS units specifications.
*
*   It requires Flex v2.5.4 or later.
*
*   Refer to wcsunits.h for a description of the user interface and operating
*   notes.
*
*===========================================================================*/

/* Options. */
%option full
%option never-interactive
%option noyywrap

/* Exclusive start states. */
%x NEXT FLUSH

%{
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "wcsunits.h"

#define YY_DECL int wcsutrn(int ctrl, char unitstr[])

#ifdef DEBUG
  /* Print text as it is matched. */
  #define YY_USER_ACTION printf("Rule %03d: \"%s\"\n", yy_act, yytext);
#endif

%}

%%
	char orig[8], subs[8];
	int bracket = 0;
	int unsafe  = 0;
	int status  = -1;
	
	*orig = '\0';
	*subs = '\0';
	
	yy_scan_string(unitstr);
	*unitstr = '\0';
	BEGIN(INITIAL);
	
	#ifdef DEBUG
	printf("\n%s ->\n", unitstr);
	#endif

^" "*"[" {
	  /* Rule 001: Looks like an inline comment. */
	  strcat(unitstr, "[");
	  bracket = 1;
	}

" "+	  /* Rule 002: Discard leading whitespace. */

[^A-Za-z] {
	  /* Rule 003: Non-alphabetic character. */
	  strcat(unitstr, yytext);
	  if (bracket && *yytext == ']') {
	    BEGIN(FLUSH);
	  }
	}

arcmins|ARCMIN|ARCMINS {
	  /* Rule 004. */
	  strcpy(orig, yytext);
	  strcpy(subs, "arcmin");
	  BEGIN(NEXT);
	}

arcsecs|ARCSEC|ARCSECS {
	  /* Rule 005. */
	  strcpy(orig, yytext);
	  strcpy(subs, "arcsec");
	  BEGIN(NEXT);
	}

BEAM {
	  /* Rule 006. */
	  strcpy(orig, yytext);
	  strcpy(subs, "beam");
	  BEGIN(NEXT);
	}

day|days|DAY|DAYS {
	  /* Rule 007. */
	  strcpy(orig, yytext);
	  strcpy(subs, "d");
	  BEGIN(NEXT);
	}

D {
	  /* Rule 008. */
	  unsafe = 1;
	  strcpy(orig, yytext);
	  strcpy(subs, (ctrl & 4) ? "d" : "D");
	  BEGIN(NEXT);
	}

degree|degrees|DEG|DEGREE|DEGREES {
	  /* Rule 009. */
	  strcpy(orig, yytext);
	  strcpy(subs, "deg");
	  BEGIN(NEXT);
	}

GHZ {
	  /* Rule 010. */
	  strcpy(orig, yytext);
	  strcpy(subs, "GHz");
	  BEGIN(NEXT);
	}

hr|HR {
	  /* Rule 011. */
	  strcpy(orig, yytext);
	  strcpy(subs, "h");
	  BEGIN(NEXT);
	}

H {
	  /* Rule 012. */
	  unsafe = 1;
	  strcpy(orig, yytext);
	  strcpy(subs, (ctrl & 2) ? "h" : "H");
	  BEGIN(NEXT);
	}

hz|HZ {
	  /* Rule 013. */
	  strcpy(orig, yytext);
	  strcpy(subs, "Hz");
	  BEGIN(NEXT);
	}

KHZ {
	  /* Rule 014. */
	  strcpy(orig, yytext);
	  strcpy(subs, "kHz");
	  BEGIN(NEXT);
	}

JY {
	  /* Rule 015. */
	  strcpy(orig, yytext);
	  strcpy(subs, "Jy");
	  BEGIN(NEXT);
	}

kelvin|kelvins|Kelvin|Kelvins|KELVIN|KELVINS {
	  /* Rule 016. */
	  strcpy(orig, yytext);
	  strcpy(subs, "K");
	  BEGIN(NEXT);
	}

KM {
	  /* Rule 017. */
	  strcpy(orig, yytext);
	  strcpy(subs, "km");
	  BEGIN(NEXT);
	}

metre|meter|metres|meters|M|METRE|METER|METRES|METERS {
	  /* Rule 018. */
	  strcpy(orig, yytext);
	  strcpy(subs, "m");
	  BEGIN(NEXT);
	}

MIN {
	  /* Rule 019. */
	  strcpy(orig, yytext);
	  strcpy(subs, "min");
	  BEGIN(NEXT);
	}

MHZ {
	  /* Rule 020. */
	  strcpy(orig, yytext);
	  strcpy(subs, "MHz");
	  BEGIN(NEXT);
	}

pascal|pascals|PASCAL|PASCALS {
	  /* Rule 021. */
	  strcpy(orig, yytext);
	  strcpy(subs, "Pa");
	  BEGIN(NEXT);
	}

pixels|PIXEL|PIXELS {
	  /* Rule 022. */
	  strcpy(orig, yytext);
	  strcpy(subs, "pixel");
	  BEGIN(NEXT);
	}

radian|radians|RAD|RADIAN|RADIANS {
	  /* Rule 023. */
	  strcpy(orig, yytext);
	  strcpy(subs, "rad");
	  BEGIN(NEXT);
	}

sec|second|seconds|SEC|SECOND|SECONDS {
	  /* Rule 024. */
	  strcpy(orig, yytext);
	  strcpy(subs, "s");
	  BEGIN(NEXT);
	}

S {
	  /* Rule 025. */
	  unsafe = 1;
	  strcpy(orig, yytext);
	  strcpy(subs, (ctrl & 1) ? "s" : "S");
	  BEGIN(NEXT);
	}

volt|volts|VOLT|VOLTS {
	  /* Rule 026. */
	  strcpy(orig, yytext);
	  strcpy(subs, "V");
	  BEGIN(NEXT);
	}

year|years|YR|YEAR|YEARS {
	  /* Rule 027. */
	  strcpy(orig, yytext);
	  strcpy(subs, "yr");
	  BEGIN(NEXT);
	}

[A-Za-z]+ {
	  /* Rule 028: Not a recognized alias. */
	  strcpy(orig, yytext);
	  strcpy(subs, orig);
	  BEGIN(NEXT);
	}

<NEXT>[A-Za-z]+ {
	  /* Rule 029: Reject the alias match. */
	  strcat(orig, yytext);
	  strcpy(subs, orig);
	}

<NEXT>" "+[^A-Za-z] {
	  /* Rule 030: Discard separating whitespace. */
	  unput(yytext[yyleng-1]);
	}

<NEXT>" "+[A-Za-z] {
	  /* Rule 031: Compress separating whitespace. */
	  strcat(unitstr, subs);
	  strcat(unitstr, " ");
	  if (strcmp(orig, subs)) status = 0;
	  unput(yytext[yyleng-1]);
	  *subs = '\0';
	  BEGIN(INITIAL);
	}

<NEXT>. {
	  /* Rule 032: Copy anything else unchanged. */
	  strcat(unitstr, subs);
	  if (strcmp(orig, subs)) status = 0;
	  unput(*yytext);
	  *subs = '\0';
	  BEGIN(INITIAL);
	}

<FLUSH>.* {
	  /* Rule 033: Copy out remaining input. */
	  strcat(unitstr, yytext);
	}

<<EOF>> {
	  /* Rule unnumbered: End-of-string. */
	  if (*subs) {
	    strcat(unitstr, subs);
	    if (strcmp(orig, subs)) status = 0;
	  }
	  return unsafe ? 12 : status;
	}

%%
