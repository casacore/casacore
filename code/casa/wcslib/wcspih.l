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
*   wcspih.l is a Flex description file containing a lexical scanner
*   definition for extracting the WCS cards from a FITS header.
*
*   It requires Flex v2.5.4 or later.
*
*   Refer to wcshdr.h for a description of the user interface and operating
*   notes.
*
*   The flawed definition of WCSAXESa and its nullifying effect on NAXIS
*   forces the scanner to be implemented in two passes.  A first pass is
*   required to determine the number of coordinate representations (up to 27)
*   and the number of coordinate axes in each; it also counts the number of
*   PVi_ma and PSi_ma cards in each representation.
*
*   On completion of the first pass, memory is allocated for an array of the
*   required number of wcsprm structs and each of these is initialized
*   appropriately.  These structs are filled in the second pass.  The parser
*   does not check for duplicated cards, it accepts the last encountered.
*
*===========================================================================*/

/* Options. */
%option full
%option never-interactive
%option noyywrap

/* Indices for parameterized keywords. */
I0	[0-9]
I1	[1-9]
I2	[1-9][0-9]
I3	[1-9][0-9]{2}
I4	[1-9][0-9]{3}

/* Alternate coordinate system identifier. */
ALT	[ A-Z]

/* Keyvalue data types. */
INTEGER	[+-]?[0-9]+
FLOAT	[+-]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?
STRING	'([^']|'')*'

/* Exclusive start states. */
%x CROTAi PROJPn
%x CCCCCia CCi_ja CCi_ma CCCCCCCa CCCCCCCC
%x VALUE
%x INTEGER_VAL FLOAT_VAL STRING_VAL
%x COMMENT
%x DISCARD ERROR FLUSH

%{
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wcs.h"
#include "wcshdr.h"
#include "wcsmath.h"

#define INTEGER 0
#define FLOAT   1
#define STRING  2

#define YY_DECL int wcspih(char *header, int ncards, int relax, int ctrl, \
                           int *nreject, int *nwcs, struct wcsprm **wcs)

#define YY_INPUT(inbuff, count, bufsize) \
	{ \
	  if (wcspih_ncards) { \
	    strncpy(inbuff, wcspih_hdr, 80); \
	    inbuff[80] = '\n'; \
	    wcspih_hdr += 80; \
	    wcspih_ncards--; \
	    count = 81; \
	  } else { \
	    count = YY_NULL; \
	  } \
	}

#ifdef DEBUG
  /* Print text as it is matched. */
  #define YY_USER_ACTION printf("Rule %03d: \"%s\"\n", yy_act, yytext);
#endif

/* These global variables are required by YY_INPUT. */
char *wcspih_hdr;
int  wcspih_ncards;

void wcspih_naxes(int naxis, int i, int j, char alt, int alts[], int *npptr);
int  wcspih_inits(int alts[], int npv[], int nps[], int *nwcs,
                  struct wcsprm **wcs);
int  wcspih_final(int alts[], double epoch[], int velref[], double vsource[],
                  int *nwcs, struct wcsprm **wcs);

%}

%%

	char alt, *cptr, errmsg[80], *hptr;
	int  a, altlin, alts[27], i, j, k, m, naxis, ndx, *npptr, nps[27],
	     npv[27], pass, status, type, velref[27], voff;
	double epoch[27], vsource[27];
	void *vptr, *wptr;
	struct wcsprm *wcsp;
	
	naxis = 0;
	for (a = 0; a < 27; a++) {
	  alts[a] = 0;
	  npv[a] = 0;
	  nps[a] = 0;
	  epoch[a]   = UNDEFINED;
	  velref[a]  = 0;
	  vsource[a] = UNDEFINED;
	}
	
	wcspih_hdr = header;
	wcspih_ncards = ncards;
	
	hptr = header;
	*nreject = 0;
	*nwcs = 0;
	*wcs  = 0;
	
	pass = 1;
	i = j = m = 0;
	alt = ' ';
	ndx  = -1;
	type = -1;
	altlin = 0;
	npptr = 0;
	vptr  = 0;
	
	BEGIN(INITIAL);


^NAXIS"   "="  "*{INTEGER} {
	  /* Rule 001. */
	  if (pass == 1) {
	    sscanf(yytext, "NAXIS   = %d", &naxis);
	  }
	
	  if (naxis < 0) {
	    sprintf(errmsg, "Negative value of NAXIS ignored: %d", naxis);
	    naxis = 0;
	    BEGIN(ERROR);
	  } else {
	    yyless(0);
	    BEGIN(DISCARD);
	  }
	}

^WCSAXES{ALT}="  "*{INTEGER} {
	  /* Rule 002. */
	  if (pass == 1) {
	    sscanf(yytext, "WCSAXES%c= %d", &alt, &i);
	    wcspih_naxes(naxis, i, 0, alt, alts, 0);
	  }
	  BEGIN(FLUSH);
	}

^WCSNAME {
	  /* Rule 003. */
	  if (*wcs) vptr = (*wcs)->wcsname;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^CRPIX	{
	  /* Rule 004. */
	  if (*wcs) vptr = &((*wcs)->crpix);
	  type = FLOAT;
	  BEGIN(CCCCCia);
	}

^PC	{
	  /* Rule 005. */
	  if (*wcs) {
	    vptr = &((*wcs)->pc);
	    altlin = 1;
	  }
	  type = FLOAT;
	  BEGIN(CCi_ja);
	}

^CD	{
	  /* Rule 006. */
	  if (*wcs) {
	    vptr = &((*wcs)->cd);
	    altlin = 2;
	  }
	  type = FLOAT;
	  BEGIN(CCi_ja);
	}

^CDELT	{
	  /* Rule 007. */
	  if (*wcs) vptr = &((*wcs)->cdelt);
	  type = FLOAT;
	  BEGIN(CCCCCia);
	}

^CROTA	{
	  /* Rule 008. */
	  if (*wcs) {
	    vptr = &((*wcs)->crota);
	    altlin = 4;
	  }
	  type = FLOAT;
	  BEGIN(CROTAi);
	}

^CUNIT	{
	  /* Rule 009. */
	  if (*wcs) vptr = &((*wcs)->cunit);
	  type = STRING;
	  BEGIN(CCCCCia);
	}

^CTYPE	{
	  /* Rule 010. */
	  if (*wcs) vptr = &((*wcs)->ctype);
	  type = STRING;
	  BEGIN(CCCCCia);
	}

^CRVAL	{
	  /* Rule 011. */
	  if (*wcs) vptr = &((*wcs)->crval);
	  type = FLOAT;
	  BEGIN(CCCCCia);
	}

^CNAME	{
	  /* Rule 012. */
	  if (*wcs) vptr = &((*wcs)->cname);
	  type = STRING;
	  BEGIN(CCCCCia);
	}

^CRDER	{
	  /* Rule 013. */
	  if (*wcs) vptr = &((*wcs)->crder);
	  type = FLOAT;
	  BEGIN(CCCCCia);
	}

^CSYER	{
	  /* Rule 014. */
	  if (*wcs) vptr = &((*wcs)->csyer);
	  type = FLOAT;
	  BEGIN(CCCCCia);
	}

^PV	{
	  /* Rule 015. */
	  npptr = npv;
	  if (*wcs) vptr = &((*wcs)->pv);
	  type = FLOAT;
	  BEGIN(CCi_ma);
	}

^PROJP	{
	  /* Rule 016. */
	  npptr = npv;
	  if (*wcs) vptr = &((*wcs)->pv);
	  type = FLOAT;
	  BEGIN(PROJPn);
	}

^PS	{
	  /* Rule 017. */
	  npptr = nps;
	  if (*wcs) vptr = &((*wcs)->ps);
	  type = STRING;
	  BEGIN(CCi_ma);
	}

^LONPOLE {
	  /* Rule 018. */
	  if (*wcs) vptr = &((*wcs)->lonpole);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^LATPOLE {
	  /* Rule 019. */
	  if (*wcs) vptr = &((*wcs)->latpole);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^RADECSYS {
	  /* Rule 020. */
	  unput(' ');
	  if (*wcs) vptr = (*wcs)->radesys;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^RADESYS {
	  /* Rule 021. */
	  if (*wcs) vptr = (*wcs)->radesys;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^EQUINOX {
	  /* Rule 022. */
	  if (*wcs) vptr = &((*wcs)->equinox);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^EPOCH{ALT}"  " {
	  /* Rule 023. */
	  sscanf(yytext, "EPOCH%c", &alt);
	
	  if (alt == ' ' || relax & (WCSHDR_all | WCSHDR_EPOCHa)) {
	    if (*wcs) {
	      vptr = epoch;
	      if (alt >= 'A') {
	        vptr = (void *)((double *)vptr + alts[alt-'A'+1]);
	      }
	    }
	
	    unput(' ');
	    type = FLOAT;
	    BEGIN(CCCCCCCa);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "EPOCH keyword may not have an alternate "
	                    "version code");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

^RESTFREQ {
	  /* Rule 024. */
	  unput(' ');
	  if (*wcs) vptr = &((*wcs)->restfrq);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^RESTFRQ {
	  /* Rule 025. */
	  if (*wcs) vptr = &((*wcs)->restfrq);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^RESTWAV {
	  /* Rule 026. */
	  if (*wcs) vptr = &((*wcs)->restwav);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^SPECSYS {
	  /* Rule 027. */
	  if (*wcs) vptr = (*wcs)->specsys;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^SSYSOBS {
	  /* Rule 028. */
	  if (*wcs) vptr = (*wcs)->ssysobs;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^VELREF{ALT}" " {
	  /* Rule 029. */
	  sscanf(yytext, "VELREF%c", &alt);
	
	  if (alt == ' ' || relax & (WCSHDR_all | WCSHDR_VELREFa)) {
	    if (*wcs) {
	      vptr = velref;
	      if (alt >= 'A') {
	        vptr = (void *)((int *)vptr + alts[alt-'A'+1]);
	      }
	    }
	
	    unput(' ');
	    type = INTEGER;
	    BEGIN(CCCCCCCa);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "VELREF keyword may not have an alternate "
	                    "version code");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

^VELOSYS {
	  /* Rule 030. */
	  if (*wcs) vptr = &((*wcs)->velosys);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^SSYSSRC {
	  /* Rule 031. */
	  if (*wcs) vptr = (*wcs)->ssyssrc;
	  type = STRING;
	  BEGIN(CCCCCCCa);
	}

^VSOURCE{ALT} {
	  /* Rule 032. */
	  sscanf(yytext, "VSOURCE%c", &alt);
	
	  if (relax & (WCSHDR_all | WCSHDR_VSOURCEa)) {
	    if (*wcs) {
	      vptr = vsource;
	      if (alt >= 'A') {
	        vptr = (void *)((double *)vptr + alts[alt-'A'+1]);
	      }
	    }
	
	    unput(' ');
	    type = FLOAT;
	    BEGIN(CCCCCCCa);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "Deprecated VSOURCEa keyword rejected");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

^ZSOURCE {
	  /* Rule 033. */
	  if (*wcs) vptr = &((*wcs)->zsource);
	  type = FLOAT;
	  BEGIN(CCCCCCCa);
	}

^OBSGEO-X {
	  /* Rule 034. */
	  if (*wcs) vptr = (*wcs)->obsgeo;
	  type = FLOAT;
	  BEGIN(CCCCCCCC);
	}

^OBSGEO-Y {
	  /* Rule 035. */
	  if (*wcs) vptr = (*wcs)->obsgeo + 1;
	  type = FLOAT;
	  BEGIN(CCCCCCCC);
	}

^OBSGEO-Z {
	  /* Rule 036. */
	  if (*wcs) vptr = (*wcs)->obsgeo + 2;
	  type = FLOAT;
	  BEGIN(CCCCCCCC);
	}

^DATE-OBS {
	  /* Rule 037. */
	  if (*wcs) vptr = (*wcs)->dateobs;
	  type = STRING;
	  BEGIN(CCCCCCCC);
	}

^MJD-OBS" " {
	  /* Rule 038. */
	  if (*wcs) vptr = &((*wcs)->mjdobs);
	  type = FLOAT;
	  BEGIN(CCCCCCCC);
	}

^MJD-AVG" " {
	  /* Rule 039. */
	  if (*wcs) vptr = &((*wcs)->mjdavg);
	  type = FLOAT;
	  BEGIN(CCCCCCCC);
	}

^END" "{77} {
	  /* Rule 040. */
	  yyless(0);
	  if (wcspih_ncards) {
	    sprintf(errmsg, "Illegal cards following END card");
	    BEGIN(ERROR);
	  } else {
	    BEGIN(DISCARD);
	  }
	}

^.	{
	  /* Rule 041. */
	  yyless(0);
	  BEGIN(DISCARD);
	}

<CROTAi>{I1}{ALT}" " |
<CROTAi>{I2}{ALT} {
	  /* Rules 042-043. */
	  sscanf(yytext, "%d%c", &i, &alt);
	  if (alt == ' ' || relax & (WCSHDR_all | WCSHDR_CROTAia)) {
	    ndx = i - 1;
	    BEGIN(VALUE);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "CROTAn keyword may not have an alternate "
	                    "version code");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

<CROTAi>{I3} {
	  /* Rule 044. */
	  sscanf(yytext, "%d", &i);
	  alt = ' ';
	  ndx = i - 1;
	  BEGIN(VALUE);
	}

<CROTAi>. {
	  /* Rule 045. */
	  BEGIN(DISCARD);
	}

<PROJPn>{I0}"  " {
	  /* Rule 046. */
	  if (relax & (WCSHDR_all | WCSHDR_PROJPn)) {
	    sscanf(yytext, "%d", &m);
	    i = 0;
	    alt = ' ';
	    ndx = -1;
	    BEGIN(VALUE);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "Defunct PROJPn keyword rejected");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

<PROJPn>. {
	  /* Rule 047. */
	  BEGIN(DISCARD);
	}

<CCCCCia>{I1}{ALT}" " |
<CCCCCia>{I2}{ALT} {
	  /* Rules 048-049. */
	  sscanf(yytext, "%d%c", &i, &alt);
	  ndx = i - 1;
	  BEGIN(VALUE);
	}

<CCCCCia>{I3} {
	  /* Rule 050. */
	  sscanf(yytext, "%3d", &i);
	  alt = ' ';
	  ndx = i - 1;
	  BEGIN(VALUE);
	}

<CCCCCia>. {
	  /* Rule 051. */
	  BEGIN(DISCARD);
	}

<CCi_ja>{I1}_{I1}{ALT}"  " |
<CCi_ja>{I1}_{I2}{ALT}" " |
<CCi_ja>{I2}_{I1}{ALT}" " |
<CCi_ja>{I1}_{I3}{ALT} |
<CCi_ja>{I2}_{I2}{ALT} |
<CCi_ja>{I3}_{I1}{ALT} {
	  /* Rules 052-057. */
	  sscanf(yytext, "%d_%d%c", &i, &j, &alt);
	  if (*wcs) {
	    wcsp = *wcs;
	    if (alt != ' ') {
	      wcsp += alts[alt-'A'+1];
	    }
	
	    ndx = (i-1)*(wcsp->naxis) + j - 1;
	  }
	  BEGIN(VALUE);
	}

<CCi_ja>{I1}_{I4} |
<CCi_ja>{I2}_{I3} |
<CCi_ja>{I3}_{I2} |
<CCi_ja>{I4}_{I1} {
	  /* Rules 058-061. */
	  sscanf(yytext, "%d_%d", &i, &j);
	  alt = ' ';
	  if (*wcs) {
	    ndx = (i-1)*((*wcs)->naxis) + j - 1;
	  }
	  BEGIN(VALUE);
	}

<CCi_ja>{I0}{6} {
	  /* Rule 062: This covers the defunct forms CD00i00j and PC00i00j. */
	  if ((relax & WCSHDR_all) ||
	      (relax & WCSHDR_PC00i00j) && altlin == 1 ||
	      (relax & WCSHDR_CD00i00j) && altlin == 2) {
	    sscanf(yytext, "%3d%3d", &i, &j);
	    alt = ' ';
	    if (*wcs) {
	      ndx = (i-1)*((*wcs)->naxis) + j - 1;
	    }
	    BEGIN(VALUE);
	
	  } else if (relax & WCSHDR_reject) {
	    sprintf(errmsg, "Defunct form of %si_ja keyword",
	                     (altlin==1) ? "PC" : "CD");
	    BEGIN(ERROR);
	
	  } else {
	    BEGIN(DISCARD);
	  }
	}

<CCi_ja>. {
	  /* Rule 063. */
	  BEGIN(DISCARD);
	}

<CCi_ma>{I1}_{I0}{ALT}"  " |
<CCi_ma>{I1}_{I2}{ALT}" " |
<CCi_ma>{I2}_{I1}{ALT}" " |
<CCi_ma>{I1}_{I3}{ALT} |
<CCi_ma>{I2}_{I2}{ALT} |
<CCi_ma>{I3}_{I1}{ALT} {
	  /* Rules 064-069. */
	  sscanf(yytext, "%d_%d%c", &i, &m, &alt);
	  ndx = -1;
	  BEGIN(VALUE);
	}

<CCi_ma>{I1}_{I4} |
<CCi_ma>{I2}_{I3} |
<CCi_ma>{I3}_{I2} |
<CCi_ma>{I4}_{I1} {
	  /* Rules 070-073. */
	  sscanf(yytext, "%d_%d", &i, &m);
	  alt = ' ';
	  ndx = -1;
	  BEGIN(VALUE);
	}

<CCi_ma>. {
	  /* Rule 074. */
	  BEGIN(DISCARD);
	}

<CCCCCCCa>{ALT} {
	  /* Rule 075. */
	  ndx = -1;
	  sscanf(yytext, "%c", &alt);
	  BEGIN(VALUE);
	}

<CCCCCCCa>. {
	  /* Rule 076. */
	  BEGIN(DISCARD);
	}

<CCCCCCCC>. {
	  /* Rule 077. */
	  unput(yytext[0]);
	  ndx = -1;
	  alt = 0;
	  BEGIN(VALUE);
	}

<VALUE>=" "+ {
	  /* Rule 078. */
	  if (type == INTEGER) {
	    BEGIN(INTEGER_VAL);
	  } else if (type == FLOAT) {
	    BEGIN(FLOAT_VAL);
	  } else if (type == STRING) {
	    BEGIN(STRING_VAL);
	  } else {
	    sprintf(errmsg, "Internal parser ERROR, bad data type: %d", type);
	    BEGIN(ERROR);
	  }
	}

<VALUE>. {
	  /* Rule 079. */
	  sprintf(errmsg, "Invalid KEYWORD = VALUE syntax");
	  BEGIN(ERROR);
	}

<INTEGER_VAL>{INTEGER} {
	  /* Rule 080. */
	  if (pass == 1) {
	    wcspih_naxes(naxis, i, j, alt, alts, npptr);
	    BEGIN(FLUSH);
	
	  } else {
	    if (vptr) {
	      /* Determine the coordinate representation. */
	      for (a = 0; a < *nwcs; a++) {
	        /* The loop here is for keywords that apply */
		/* to every alternate; these have alt == 0. */
	        if (alt >= 'A') {
	          a = alts[alt-'A'+1];
	        }
	
	        wptr = vptr;
	        if (a) {
	          voff = (char *)(*wcs+a) - (char *)(*wcs);
	          wptr = (void *)((char *)vptr + voff);
	        }
	
	        /* Apply keyword parameterization. */
	        if (ndx >= 0) {
	          wptr = *((int **)wptr) + ndx;
	        }
	
	        /* Read the keyvalue. */
	        sscanf(yytext, "%d", (int *)wptr);
	
	        if (alt) break;
	      }
	
	      BEGIN(COMMENT);
	
	    } else {
	      sprintf(errmsg, "Internal parser ERROR, null int pointer");
	      BEGIN(ERROR);
	    }
	  }
	}

<INTEGER_VAL>. {
	  /* Rule 081. */
	  sprintf(errmsg, "An integer value was expected");
	  BEGIN(ERROR);
	}

<FLOAT_VAL>{FLOAT} {
	  /* Rule 082. */
	  if (pass == 1) {
	    wcspih_naxes(naxis, i, j, alt, alts, npptr);
	    BEGIN(FLUSH);
	
	  } else {
	    if (vptr) {
	      /* Determine the coordinate representation. */
	      for (a = 0; a < *nwcs; a++) {
	        /* The loop here is for keywords like MJD-OBS that */
		/* apply to every alternate; these have alt == 0.  */
	        if (alt >= 'A') {
	          a = alts[alt-'A'+1];
	        }
	
	        wptr = vptr;
	        if (a) {
	          voff = (char *)(*wcs+a) - (char *)(*wcs);
	          wptr = (void *)((char *)vptr + voff);
	        }
	
	        /* Apply keyword parameterization. */
	        if (ndx >= 0) {
	          wptr = *((double **)wptr) + ndx;
	
	        } else if (npptr == npv) {
	          k = (*wcs+a)->npv++;
	          (*wcs+a)->pv[k].i = i;
	          (*wcs+a)->pv[k].m = m;
	          wptr = &((*wcs+a)->pv[k].value);
	        }
	
	        /* Read the keyvalue. */
	        sscanf(yytext, "%lf", (double *)wptr);
	
	        /* Flag the presence of PCi_ja, or CDi_ja and/or CROTAia. */
	        if (altlin) {
	          (*wcs+a)->altlin |= altlin;
	          altlin = 0;
	        }
	
	        if (alt) break;
	      }
	
	      BEGIN(COMMENT);
	
	    } else {
	      sprintf(errmsg, "Internal parser ERROR, null float pointer");
	      BEGIN(ERROR);
	    }
	  }
	}

<FLOAT_VAL>. {
	  /* Rule 083. */
	  sprintf(errmsg, "A floating-point value was expected");
	  BEGIN(ERROR);
	}

<STRING_VAL>{STRING} {
	  /* Rule 084. */
	  if (pass == 1) {
	    wcspih_naxes(naxis, i, j, alt, alts, npptr);
	    BEGIN(FLUSH);
	
	  } else {
	    if (vptr) {
	      /* Determine the coordinate representation. */
	      for (a = 0; a < *nwcs; a++) {
	        /* The loop here is for keywords like DATE-OBS that */
		/* apply to every alternate; these have alt == 0.   */
	        if (alt >= 'A') {
	          a = alts[alt-'A'+1];
	        }
	
	        wptr = vptr;
	        if (a) {
	          voff = (char *)(*wcs+a) - (char *)(*wcs);
	          wptr = (void *)((char *)vptr + voff);
	        }
	
	        /* Apply keyword parameterization. */
	        if (ndx >= 0) {
	          wptr = *((char (**)[72])wptr) + ndx;
	
	        } else if (npptr == nps) {
	          k = (*wcs+a)->nps++;
	          (*wcs+a)->ps[k].i = i;
	          (*wcs+a)->ps[k].m = m;
	          wptr = &((*wcs+a)->ps[k].value);
	        }
	
	        /* Read the keyvalue. */
	        cptr = (char *)wptr;
	        strcpy(cptr, yytext+1);
	
	        /* Squeeze out repeated quotes. */
	        k = 0;
	        for (j = 0; j < 72; j++) {
	          if (k < j) {
	            cptr[k] = cptr[j];
	          }
	
	          if (cptr[j] == '\0') {
	            if (k) cptr[k-1] = '\0';
	            break;
	          } else if (cptr[j] == '\'' && cptr[j+1] == '\'') {
	            j++;
	          }
	
	          k++;
	        }
	
	        if (alt) break;
	      }
	
	      BEGIN(COMMENT);
	
	    } else {
	      sprintf(errmsg, "Internal parser ERROR, null string pointer");
	      BEGIN(ERROR);
	    }
	  }
	}

<STRING_VAL>. {
	  /* Rule 085. */
	  sprintf(errmsg, "A string value was expected");
	  BEGIN(ERROR);
	}

<COMMENT>" "*\/.* |
<COMMENT>" "* {
	  /* Rules 086-087. */
	  BEGIN(FLUSH);
	}

<COMMENT>. {
	  /* Rule 088. */
	  sprintf(errmsg, "Malformed inline comment");
	  BEGIN(ERROR);
	}

<DISCARD>.* {
	  /* Rule 089. */
	  if (pass == 2) {
	    if (ctrl < 0) {
	      /* Preserve discards. */
	      if (hptr < wcspih_hdr-80) {
	        strncpy(hptr, wcspih_hdr-80, 80);
	      }
	      hptr += 80;
	
	    } else if (ctrl > 2) {
	      fprintf(stderr, "%.80s\n  Discarded.\n", wcspih_hdr-80);
	    }
	  }
	  BEGIN(FLUSH);
	}

<ERROR>.* {
	  /* Rule 090. */
	  (*nreject)++;
	  if (pass == 2) {
	    if (ctrl == -1) {
	      if (hptr < wcspih_hdr-80) {
	        /* Preserve rejects. */
	        strncpy(hptr, wcspih_hdr-80, 80);
	      }
	      hptr += 80;
	    }
	
	    if (abs(ctrl) > 1) {
	      fprintf(stderr, "%.80s\n%4d: %s.\n", wcspih_hdr-80, *nreject,
	        errmsg);
	    }
	  }
	  BEGIN(FLUSH);
	}

<FLUSH>.*\n {
	  /* Rule 091. */
	  i = j = m = 0;
	  alt = ' ';
	  type = -1;
	  altlin = 0;
	  npptr = 0;
	  BEGIN(INITIAL);
	}

<<EOF>> {
	  /* Rule unnumbered: End-of-input. */
	  if (pass == 1) {
	    if (status = wcspih_inits(alts, npv, nps, nwcs, wcs) ||
	        *nwcs == 0) {
	      return status;
	    }
	
	    if (abs(ctrl) > 2) {
	      if (*nwcs == 1) {
	        fprintf(stderr, "Found one coordinate representation.\n");
	      } else {
	        fprintf(stderr, "Found %d coordinate representations.\n",
	          *nwcs);
	      }
	    }
	
	    wcspih_hdr = header;
	    wcspih_ncards = ncards;
	    *nreject = 0;
	
	    pass = 2;
	    i = j = m = 0;
	    alt = ' ';
	    type = -1;
	
	    yyrestart(yyin);
	
	  } else {
	    if (ctrl < 0) {
	      *hptr = '\0';
	    } else if (ctrl == 1) {
	      fprintf(stderr, "%d WCS cards were rejected.\n", *nreject);
	    }
	
	    wcspih_final(alts, epoch, velref, vsource, nwcs, wcs);
	
	    return 0;
	  }
	}

%%

/*----------------------------------------------------------------------------
*   Determine the number of coordinate representations (up to 27), the number
*   of coordinate axes in each, and count the number of PVi_ma and PSi_ma
*   cards in each representation.
*---------------------------------------------------------------------------*/

void wcspih_naxes(int naxis, int i, int j, char alt, int alts[], int *npptr)

{
  /* On the first pass alts[] is used to determine the number of axes */
  /* for each of the 27 possible alternate coordinate descriptions.   */
  int a, *ip;

  a = 0;
  if (alt != ' ') {
    a = alt - 'A' + 1;
  }

  ip = alts + a;

  if (*ip < naxis) {
    *ip = naxis;
  }

  /* i or j can be greater than naxis. */
  if (*ip < i) {
    *ip = i;
  }

  if (*ip < j) {
    *ip = j;
  }

  if (npptr) {
    npptr[a]++;
  }
}


/*----------------------------------------------------------------------------
*   Allocate memory for an array of the required number of wcsprm structs and
*   initialize each of them.
*---------------------------------------------------------------------------*/

int wcspih_inits(
  int alts[],
  int npv[],
  int nps[],
  int *nwcs,
  struct wcsprm **wcs)

{
  int a, npsmax, npvmax, status = 0;
  struct wcsprm *wcsp;

  /* Find the number of coordinate descriptions. */
  *nwcs = 0;
  for (a = 0; a < 27; a++) {
    if (alts[a]) (*nwcs)++;
  }

  if (*nwcs) {
    /* Allocate memory for the required number of wcsprm structs. */
    if (!(*wcs = (struct wcsprm *)calloc(*nwcs, sizeof(struct wcsprm)))) {
      return 2;
    }

    /* Record the current values of NPVMAX and NPSMAX. */
    npvmax = wcsnpv(-1);
    npsmax = wcsnps(-1);

    /* Initialize each wcsprm struct. */
    wcsp = *wcs;
    *nwcs = 0;
    for (a = 0; a < 27; a++) {
      if (alts[a]) {
        wcsp->flag = -1;
        wcsnpv(npv[a]);
        wcsnps(nps[a]);
        if (status = wcsini(1, alts[a], wcsp)) {
          wcsvfree(nwcs, wcs);
          break;
        }

        if (a) {
          wcsp->alt[0] = 'A' + a - 1;
        }

        /* On the second pass alts[] indexes the array of wcsprm structs. */
        alts[a] = (*nwcs)++;

        wcsp++;
      }
    }

    /* Restore the original values of NPVMAX and NPSMAX. */
    wcsnpv(npvmax);
    wcsnps(npsmax);
  }

  return status;
}


/*----------------------------------------------------------------------------
*   Interpret any VELREF cards encountered for each coordinate representation.
*---------------------------------------------------------------------------*/

int wcspih_final(
  int alts[],
  double epoch[],
  int velref[],
  double vsource[],
  int *nwcs,
  struct wcsprm **wcs)

{
  static char *specsys[] = {"LSRK", "BARYCENT", "TOPOCENT",
                            "LSRD", "GEOCENTR", "SOURCE", "GALACTOC"};
  int a, i;
  double beta, c = 299792458.0;

  for (a = 0; a < *nwcs; a++) {
    /* Check for EPOCH overriding EQUINOXa. */
    if (undefined((*wcs+a)->equinox) && !undefined(epoch[a])) {
      /* Set EQUINOXa. */
      (*wcs+a)->equinox = epoch[a];
    }

    /* Check for VELREF overriding SPECSYSa. */
    if (velref[a]) {
      /* Set SPECSYSa. */
      if ((*wcs+a)->specsys[0] == '\0') {
        i = velref[a]%256 - 1;
        if (0 <= i && i < 7) {
          sprintf((*wcs+a)->specsys, "%s", specsys[i]);
        }
      }
    }

    /* Check for VSOURCEa overriding ZSOURCEa. */
    if (undefined((*wcs+a)->zsource) && !undefined(vsource[a])) {
      /* Convert relativistic Doppler velocity to redshift. */
      beta = vsource[a]/c;
      (*wcs+a)->zsource = (1.0+beta)/sqrt(1.0 - beta*beta) - 1.0;
    }
  }

  return 0;
}
