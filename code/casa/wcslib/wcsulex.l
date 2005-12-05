/*============================================================================
*
*   WCSLIB 4.3 - an implementation of the FITS WCS standard.
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
*   wcsulex.l is a Flex description file containing the definition of a
*   recursive, multi-buffered lexical scanner that parses FITS units
*   specifications.
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

/* Exponents. */
INTEGER	  [+-]?[1-9][0-9]*
FRAC	  {INTEGER}"/"[1-9][0-9]*
FLOAT	  [+-]?([0-9]+\.?[0-9]*|\.[0-9]+)

/* Metric prefixes. */
SUB3	  [munpfazy]
SUBPREFIX [dc]|{SUB3}
SUP3	  [kMGTPEZY]
SUPPREFIX da|h|{SUP3}
PREFIX	  {SUBPREFIX}|{SUPPREFIX}

/* Basic and derived SI units. */
BASIC	  m|s|g|rad|sr|K|A|mol|cd
DERIVED	  Hz|J|W|V|N|Pa|C|[Oo]hm|S|F|Wb|T|H|lm|lx
SI_UNIT	  {BASIC}|{DERIVED}

/* Additional recognized units: all metric prefixes allowed. */
ADD_ALL	  eV|Jy|R|G|barn

/* Additional recognized units: only super-metric prefixes. */
ADD_SUP	  a|yr|pc|bit|[bB]yte

/* Additional recognized units: only sub-metric prefixes allowed. */
ADD_SUB	  mag

/* Additional recognized units for which NO metric prefixes are allowed. */
GENERAL	  deg|arcmin|arcsec|mas|d|h|min|erg|Ry|u|D
ASTRO	  [Aa]ngstrom|AU|lyr|beam|solRad|solMass|solLum|Sun
DEVICE	  adu|bin|chan|count|ct|photon|ph|pixel|pix|voxel
ADD_NONE  {GENERAL}|{ASTRO}|{DEVICE}

/* All additional recognized units. */
ADD_UNIT  {ADD_ALL}|{ADD_SUP}|{ADD_SUB}|{ADD_NONE}

/* Exclusive start states. */
%x PAREN PREFIX UNITS EXPON FLUSH

%{
#include <math.h>
#include <stdio.h>

#include "wcsmath.h"
#include "wcsunits.h"

#define YY_DECL int wcsulex(const char unitstr[], int *func, double *scale, \
                            double units[])

#ifdef DEBUG
  /* Print text as it is matched. */
  #define YY_USER_ACTION printf("Rule %03d: \"%s\"\n", yy_act, yytext);
#endif

void wcsulex_add(double *factor, double types[], double *expon,
                 double *scale,  double units[]);

%}

%%
	int bracket  = 0;
	int operator = 0;
	int paren    = 0;
	int status   = 0;
	int func_r, i, j;
	double dexp, expon, factor, factor_r, types[WCSUNITS_NTYPE];
	YY_BUFFER_STATE buf;
	
	*func = 0;
	for (i = 0; i < WCSUNITS_NTYPE; i++) {
	  units[i] = 0.0;
	  types[i] = 0.0;
	}
	expon  = 1.0;
	factor = 1.0;
	*scale = 1.0;
	
	yy_scan_string(unitstr);
	BEGIN(INITIAL);
	
	#ifdef DEBUG
	printf("\n%s ->\n", unitstr);
	#endif

^" "+	{
	  /* Rule 001: Pretend initial whitespace doesn't exist. */
	  yy_set_bol(1);
	}

^"[" {
	  /* Rule 002. */
	  if (bracket++) {
	    BEGIN(FLUSH);
	  } else {
	    yy_set_bol(1);
	  }
	}

^10[0-9] {
	  /* Rule 003. */
	  status = 1;
	  BEGIN(FLUSH);
	}

^10 {
	  /* Rule 004. */
	  factor = 10.0;
	  BEGIN(EXPON);
	}

^log" "*"(" {
	  /* Rule 005. */
	  *func = 1;
	  unput('(');
	  BEGIN(PAREN);
	}

^ln" "*"(" {
	  /* Rule 006. */
	  *func = 2;
	  unput('(');
	  BEGIN(PAREN);
	}

^exp" "*"(" {
	  /* Rule 007. */
	  *func = 3;
	  unput('(');
	  BEGIN(PAREN);
	}

^[*.] {
	  /* Rule 008: Leading binary multiply. */
	  status = 2;
	  BEGIN(FLUSH);
	}

" "+	  /* Rule 009: Discard whitespace in INITIAL context. */

sqrt" "*"(" {
	  /* Rule 010. */
	  expon /= 2.0;
	  unput('(');
	  BEGIN(PAREN);
	}

"(" {
	  /* Rule 011: Gather terms in parentheses. */
	  yyless(0);
	  BEGIN(PAREN);
	}

[*.] {
	  /* Rule 012. */
	  if (operator++) {
	    BEGIN(FLUSH);
	  }
	}

"/" {
	  /* Rule 013. */
	  if (operator++) {
	    BEGIN(FLUSH);
	  } else if (*yytext == '/') {
	    expon *= -1.0;
	  }
	}

{SI_UNIT}|{ADD_UNIT} {
	  /* Rule 014. */
	  operator = 0;
	  yyless(0);
	  BEGIN(UNITS);
	}

{PREFIX}({SI_UNIT}|{ADD_ALL}) |
{SUPPREFIX}{ADD_SUP} |
{SUBPREFIX}{ADD_SUB} {
	  /* Rules 015-017. */
	  operator = 0;
	  yyless(0);
	  BEGIN(PREFIX);
	}

"]"	{
	  /* Rule 018. */
	  bracket = !bracket;
	  BEGIN(FLUSH);
	}

. {
	  /* Rule 019. */
	  status = 3;
	  BEGIN(FLUSH);
	}

<PAREN>"(" {
	  /* Rule 020. */
	  paren++;
	  operator = 0;
	  yymore();
	}

<PAREN>")" {
	  /* Rule 022. */
	  paren--;
	  if (paren) {
	    /* Not balanced yet. */
	    yymore();
	
	  } else {
	    /* Balanced; strip off the outer parentheses and recurse. */
	    yytext[yyleng-1] = '\0';
	
	    buf = YY_CURRENT_BUFFER;
	    status = wcsulex(yytext+1, &func_r, &factor_r, types);
	    yy_switch_to_buffer(buf);
	
	    if (func_r) {
	      status = 4;
	    }
	
	    if (status) {
	      BEGIN(FLUSH);
	    } else {
	      factor *= factor_r;
	      BEGIN(EXPON);
	    }
	  }
	}

<PAREN>[^()]+ {
	  /* Rule 022. */
	  yymore();
	}

<PREFIX>d {
	  /* Rule 023. */
	  factor = 1e-1;
	  BEGIN(UNITS);
	}

<PREFIX>c {
	  /* Rule 024. */
	  factor = 1e-2;
	  BEGIN(UNITS);
	}

<PREFIX>m {
	  /* Rule 025. */
	  factor = 1e-3;
	  BEGIN(UNITS);
	}

<PREFIX>u {
	  /* Rule 026. */
	  factor = 1e-6;
	  BEGIN(UNITS);
	}

<PREFIX>n {
	  /* Rule 027. */
	  factor = 1e-9;
	  BEGIN(UNITS);
	}

<PREFIX>p {
	  /* Rule 028. */
	  factor = 1e-12;
	  BEGIN(UNITS);
	}

<PREFIX>f {
	  /* Rule 029. */
	  factor = 1e-15;
	  BEGIN(UNITS);
	}

<PREFIX>a {
	  /* Rule 030. */
	  factor = 1e-18;
	  BEGIN(UNITS);
	}

<PREFIX>z {
	  /* Rule 031. */
	  factor = 1e-21;
	  BEGIN(UNITS);
	}

<PREFIX>y {
	  /* Rule 032. */
	  factor = 1e-24;
	  BEGIN(UNITS);
	}

<PREFIX>da {
	  /* Rule 033. */
	  factor = 1e+1;
	  BEGIN(UNITS);
	}

<PREFIX>h {
	  /* Rule 034. */
	  factor = 1e+2;
	  BEGIN(UNITS);
	}

<PREFIX>k {
	  /* Rule 035. */
	  factor = 1e+3;
	  BEGIN(UNITS);
	}

<PREFIX>M {
	  /* Rule 036. */
	  factor = 1e+6;
	  BEGIN(UNITS);
	}

<PREFIX>G {
	  /* Rule 037. */
	  factor = 1e+9;
	  BEGIN(UNITS);
	}

<PREFIX>T {
	  /* Rule 038. */
	  factor = 1e+12;
	  BEGIN(UNITS);
	}

<PREFIX>P {
	  /* Rule 039. */
	  factor = 1e+15;
	  BEGIN(UNITS);
	}

<PREFIX>E {
	  /* Rule 040. */
	  factor = 1e+18;
	  BEGIN(UNITS);
	}

<PREFIX>Z {
	  /* Rule 041. */
	  factor = 1e+21;
	  BEGIN(UNITS);
	}

<PREFIX>Y {
	  /* Rule 042. */
	  factor = 1e+24;
	  BEGIN(UNITS);
	}

<PREFIX>. {
	  /* Rule 043: Internal parser error. */
	  status = 9;
	  BEGIN(FLUSH);
	}

<UNITS>A {
	  /* Rule 044: Ampere. */
	  types[WCSUNITS_CHARGE] += 1.0;
	  types[WCSUNITS_TIME]   -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>a|yr {
	  /* Rule 045: Year (annum). */
	  factor *= 31557600.0;
	  types[WCSUNITS_TIME] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>adu {
	  /* Rule 046: Analogue-to-digital converter units. */
	  types[WCSUNITS_COUNT] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>[Aa]ngstrom {
	  /* Rule 047: Angstrom. */
	  factor *= 1e-10;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>arcmin {
	  /* Rule 048: Minute of arc. */
	  factor /= 60.0;
	  types[WCSUNITS_PLANE_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>arcsec {
	  /* Rule 049: Second of arc. */
	  factor /= 3600.0;
	  types[WCSUNITS_PLANE_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>AU {
	  /* Rule 050: Astronomical unit. */
	  factor *= 1.49598e+11;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>barn {
	  /* Rule 051: Barn. */
	  factor *= 1e-28;
	  types[WCSUNITS_LENGTH] += 2.0;
	  BEGIN(EXPON);
	}

<UNITS>beam {
	  /* Rule 052: Beam, as in Jy/beam. */
	  types[WCSUNITS_BEAM] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>bin {
	  /* Rule 053: Bin (e.g. histogram). */
	  types[WCSUNITS_BIN] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>bit {
	  /* Rule 054: Bit. */
	  types[WCSUNITS_BIT] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>[bB]yte {
	  /* Rule 055: Byte. */
	  factor *= 8.0;
	  types[WCSUNITS_BIT] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>C {
	  /* Rule 056: Coulomb. */
	  types[WCSUNITS_CHARGE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>cd {
	  /* Rule 057: Candela. */
	  types[WCSUNITS_LUMINTEN] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>chan {
	  /* Rule 058: Channel. */
	  types[WCSUNITS_BIN] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>count|ct {
	  /* Rule 059: Count. */
	  types[WCSUNITS_COUNT] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>D {
	  /* Rule 060: Debye. */
	  factor *= 1e-29 / 3.0;
	  types[WCSUNITS_CHARGE] += 1.0;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>d {
	  /* Rule 061: Day. */
	  factor *= 86400.0;
	  types[WCSUNITS_TIME] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>deg {
	  /* Rule 062: Degree. */
	  types[WCSUNITS_PLANE_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>erg {
	  /* Rule 063: Erg. */
	  factor *= 1e-7;
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>eV {
	  /* Rule 064: Electron volt. */
	  factor *= 1.6021765e-19;
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>F {
	  /* Rule 065: Farad. */
	  types[WCSUNITS_MASS]   -= 1.0;
	  types[WCSUNITS_LENGTH] -= 2.0;
	  types[WCSUNITS_TIME]   += 3.0;
	  types[WCSUNITS_CHARGE] += 2.0;
	  BEGIN(EXPON);
	}

<UNITS>G {
	  /* Rule 066: Gauss. */
	  factor *= 1e-4;
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_TIME]   += 1.0;
	  types[WCSUNITS_CHARGE] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>g {
	  /* Rule 067: Gram. */
	  factor *= 1e-3;
	  types[WCSUNITS_MASS] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>H {
	  /* Rule 068: Henry. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   += 2.0;
	  types[WCSUNITS_CHARGE] -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>h {
	  /* Rule 069: Hour. */
	  factor *= 3600.0;
	  types[WCSUNITS_TIME] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>Hz {
	  /* Rule 070: Hertz. */
	  types[WCSUNITS_TIME] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>J {
	  /* Rule 071: Joule. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>Jy {
	  /* Rule 072: Jansky. */
	  factor *= 1e-26;
	  types[WCSUNITS_MASS] += 1.0;
	  types[WCSUNITS_TIME] -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>K {
	  /* Rule 073: Kelvin. */
	  types[WCSUNITS_TEMPERATURE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>lm {
	  /* Rule 074: Lumen. */
	  types[WCSUNITS_LUMINTEN]    += 1.0;
	  types[WCSUNITS_SOLID_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>lx {
	  /* Rule 075: Lux. */
	  types[WCSUNITS_LUMINTEN]    += 1.0;
	  types[WCSUNITS_SOLID_ANGLE] += 1.0;
	  types[WCSUNITS_LENGTH]      -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>lyr {
	  /* Rule 076: Light year. */
	  factor *= 2.99792458e8 * 31557600.0;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>m {
	  /* Rule 077: Metre. */
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>mag {
	  /* Rule 078: Stellar magnitude. */
	  types[WCSUNITS_MAGNITUDE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>mas {
	  /* Rule 079: Milli-arcsec. */
	  factor /= 3600e+3;
	  types[WCSUNITS_PLANE_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>min {
	  /* Rule 080: Minute. */
	  factor *= 60.0;
	  types[WCSUNITS_TIME] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>mol {
	  /* Rule 081: Mole. */
	  types[WCSUNITS_MOLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>N {
	  /* Rule 082: Newton. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 1.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>[Oo]hm {
	  /* Rule 083: Ohm. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 1.0;
	  types[WCSUNITS_CHARGE] -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>Pa {
	  /* Rule 084: Pascal. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] -= 1.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>pc {
	  /* Rule 085: Parsec. */
	  factor *= 3.0857e16;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>photon|ph {
	  /* Rule 086: Photon. */
	  types[WCSUNITS_COUNT] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>pixel|pix {
	  /* Rule 087: Pixel. */
	  types[WCSUNITS_PIXEL] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>R {
	  /* Rule 088: Rayleigh. */
	  factor *= 1e10 / (4.0 * PI);
	  types[WCSUNITS_LENGTH]      -= 2.0;
	  types[WCSUNITS_TIME]        -= 1.0;
	  types[WCSUNITS_SOLID_ANGLE] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>rad {
	  /* Rule 089: Radian. */
	  factor *= 180.0 / PI;
	  types[WCSUNITS_PLANE_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>Ry {
	  /* Rule 090: Rydberg. */
	  factor *= 13.605692 * 1.6021765e-19;
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  BEGIN(EXPON);
	}

<UNITS>S {
	  /* Rule 091: Siemen. */
	  types[WCSUNITS_MASS]   -= 1.0;
	  types[WCSUNITS_LENGTH] -= 2.0;
	  types[WCSUNITS_TIME]   += 1.0;
	  types[WCSUNITS_CHARGE] += 2.0;
	  BEGIN(EXPON);
	}

<UNITS>s {
	  /* Rule 092: Second. */
	  types[WCSUNITS_TIME] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>solLum {
	  /* Rule 093: Solar luminosity. */
	  factor *= 3.8268e26;
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 3.0;
	  BEGIN(EXPON);
	}

<UNITS>solMass {
	  /* Rule 094: Solar mass. */
	  factor *= 1.9891e30;
	  types[WCSUNITS_MASS] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>solRad {
	  /* Rule 095: Solar radius. */
	  factor *= 6.9599e8;
	  types[WCSUNITS_LENGTH] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>sr {
	  /* Rule 096: Steradian. */
	  types[WCSUNITS_SOLID_ANGLE] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>Sun {
	  /* Rule 097: Sun (with respect to). */
	  types[WCSUNITS_SOLRATIO] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>T {
	  /* Rule 098: Tesla. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_TIME]   += 1.0;
	  types[WCSUNITS_CHARGE] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>u {
	  /* Rule 099: Unified atomic mass unit. */
	  factor *= 1.6605387e-27;
	  types[WCSUNITS_MASS] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>V {
	  /* Rule 100: Volt. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 1.0;
	  types[WCSUNITS_TIME]   -= 2.0;
	  types[WCSUNITS_CHARGE] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>voxel {
	  /* Rule 101: Voxel. */
	  types[WCSUNITS_VOXEL] += 1.0;
	  BEGIN(EXPON);
	}

<UNITS>W {
	  /* Rule 102: Watt. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   -= 3.0;
	  BEGIN(EXPON);
	}

<UNITS>Wb {
	  /* Rule 103: Weber. */
	  types[WCSUNITS_MASS]   += 1.0;
	  types[WCSUNITS_LENGTH] += 2.0;
	  types[WCSUNITS_TIME]   += 1.0;
	  types[WCSUNITS_CHARGE] -= 1.0;
	  BEGIN(EXPON);
	}

<UNITS>. {
	  /* Rule 104: Internal parser error. */
	  status = 9;
	  BEGIN(FLUSH);
	}

<EXPON>" "*("**"|^) {
	  /* Rule 105: Exponentiation. */
	  if (operator++) {
	    BEGIN(FLUSH);
	  }
	}

<EXPON>" "*{INTEGER} {
	  /* Rule 106. */
	  sscanf(yytext, " %d", &i);
	  expon *= (double)i;
	  wcsulex_add(&factor, types, &expon, scale, units);
	  operator = 0;
	  BEGIN(INITIAL);
	}

<EXPON>" "*"("" "*{INTEGER}" "*")" {
	  /* Rule 107. */
	  sscanf(yytext, " (%d)", &i);
	  expon *= (double)i;
	  wcsulex_add(&factor, types, &expon, scale, units);
	  operator = 0;
	  BEGIN(INITIAL);
	}

<EXPON>" "*"("" "*{FRAC}" "*")" {
	  /* Rule 108. */
	  sscanf(yytext, " (%d/%d)", &i, &j);
	  expon *= (double)i / (double)j;
	  wcsulex_add(&factor, types, &expon, scale, units);
	  operator = 0;
	  BEGIN(INITIAL);
	}

<EXPON>" "*"("" "*{FLOAT}" "*")" {
	  /* Rule 109. */
	  sscanf(yytext, " (%lf)", &dexp);
	  expon *= dexp;
	  wcsulex_add(&factor, types, &expon, scale, units);
	  operator = 0;
	  BEGIN(INITIAL);
	}

<EXPON>" "*[.*]" "* {
	  /* Rule 110: Multiply. */
	  if (operator++) {
	    BEGIN(FLUSH);
	  } else {
	    wcsulex_add(&factor, types, &expon, scale, units);
	    BEGIN(INITIAL);
	  }
	}

<EXPON>" "*"(" {
	  /* Rule 111: Multiply. */
	  if (operator) {
	    BEGIN(FLUSH);
	  } else {
	    wcsulex_add(&factor, types, &expon, scale, units);
	    unput('(');
	    BEGIN(INITIAL);
	  }
	}

<EXPON>" "+ {
	  /* Rule 112: Multiply. */
	  if (operator) {
	    BEGIN(FLUSH);
	  } else {
	    wcsulex_add(&factor, types, &expon, scale, units);
	    BEGIN(INITIAL);
	  }
	}

<EXPON>" "*"/"" "* {
	  /* Rule 113: Divide. */
	  if (operator++) {
	    BEGIN(FLUSH);
	  } else {
	    wcsulex_add(&factor, types, &expon, scale, units);
	    expon = -1.0;
	    BEGIN(INITIAL);
	  }
	}

<EXPON>" "*"]" {
	  /* Rule 114. */
	  wcsulex_add(&factor, types, &expon, scale, units);
	  bracket = !bracket;
	  BEGIN(FLUSH);
	}

<EXPON>. {
	  /* Rule 115. */
	  status = 5;
	  BEGIN(FLUSH);
	}

<FLUSH>.* {
	  /* Rule 116: Discard any remaining input. */
	}

<<EOF>> {
	  /* Rule unnumbered: End-of-string. */
	  if (YY_START == EXPON) {
	    wcsulex_add(&factor, types, &expon, scale, units);
	  }
	
	  yy_delete_buffer(YY_CURRENT_BUFFER);
	
	  if (bracket) {
	    status = 6;
	  } else if (paren) {
	    status = 7;
	  } else if (operator) {
	    status = (operator == 1) ? 2 : 8;
	  #ifdef DEBUG
	  } else {
	    printf("EOS\n");
	  #endif
	  }
	
	  if (status) {
	    for (i = 0; i < WCSUNITS_NTYPE; i++) {
	      units[i] = 0.0;
	      *scale = 0.0;
	    }
	  }
	
	  return status;
	}

%%

/*----------------------------------------------------------------------------
*   Accumulate a term in a units specification and reset work variables.
*---------------------------------------------------------------------------*/

void wcsulex_add(
  double *factor,
  double types[],
  double *expon,
  double *scale,
  double units[])

{
  int i;

  *scale *= pow(*factor, *expon);

  for (i = 0; i < WCSUNITS_NTYPE; i++) {
    units[i] += *expon * types[i];
    types[i] = 0.0;
  }

  *expon  = 1.0;
  *factor = 1.0;

  return;
}
