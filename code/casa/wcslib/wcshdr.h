/*============================================================================
*
*   WCSLIB 3.5 - an implementation of the FITS WCS convention.
*   Copyright (C) 1995-2004, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
*   http://www.atnf.csiro.au/~mcalabre/index.html
*   $Id$
*=============================================================================
*
*   WCSLIB 3.5 - C routines that implement the FITS World Coordinate System
*   (WCS) convention.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (paper II)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2004, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   wcspih() is a high-level FITS WCS routine that parses an image header.  It
*   returns an array of up to 27 wcsprm structs.
*
*   wcsvfree() deallocates memory for an array of wcsprm structs, such as
*   returned by wcspih().
*
*   Refer to the prologue of wcs.h for a description of the high level driver
*   routines for the WCS linear, celestial, and spectral transformation
*   routines.
*
*
*   FITS WCS parser routine; wcspih()
*   ---------------------------------
*   wcspih() is a high-level FITS WCS routine that parses an image header,
*   either that of a primary HDU or of an image extension.  All WCS keywords
*   defined in Papers I, II, and III are recognized, and also those used by
*   the AIPS convention and certain other keywords that existed in early
*   drafts of the WCS papers (see note 1 below).
*
*   Given a character array containing a FITS header it identifies and reads
*   all WCS cards for the primary coordinate description and up to 26
*   alternate descriptions.  It returns this information as an array of wcsprm
*   structs.
*
*   Given:
*      header   char[]   Character array containing the (entire) FITS header
*                        from which to idenfify and construct the coordinate
*                        representations, for example, as might be obtained
*                        conveniently via the CFITSIO routine fits_hdr2str().
*      nkeys    int      Number of keywords in the header.
*      relax    int      Degree of permissiveness:
*                           0: Recognize only FITS keywords defined by the
*                              published WCS standard.
*                           1: Admit all recognized informal extensions of the
*                              WCS standard.
*                        Fine-grained control of the degree of permissiveness
*                        is also possible, see note 1 below.
*      errlvl   int      Error reporting control:
*                           0: Do not report any rejected header cards.
*                           1: Produce a one-line message stating the number
*                              of WCS cards rejected (nreject).
*                           2: Report each rejected card and the reason why it
*                              was rejected.
*                           3: As above, but also report all non-WCS cards
*                              that were discarded, and the number of
*                              coordinate representations (nwcs) found.
*                        The report is written to stderr.
*
*   Returned:
*      nreject  int*     Number of WCS cards rejected for syntax errors,
*                        illegal values, etc.  Cards not recognized as WCS
*                        cards are simply ignored (but see also note 1 below).
*      nwcs     int*     Number of coordinate representations found.
*      wcs      struct wcsprm**
*                        Pointer to an array of wcsprm structs containing up
*                        to 27 coordinate representations.
*
*                        Memory for the array is allocated by wcspih() which
*                        also invokes wcsini() for each struct to allocate
*                        memory for internal arrays and initialize their
*                        members to default values (but note that wcsset() is
*                        not invoked on these structs).
*
*                        This allocated memory must be freed by the caller,
*                        first by invoking wcsfree() for each struct, and then
*                        by freeing the array itself.  A service routine,
*                        wcsvfree(), is provided to do this (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
*
*
*   Service routine for the array of wcsprm structs; wcsvfree()
*   -----------------------------------------------------------
*   wcsvfree() frees the memory allocated by wcspih() for the array of wcsprm
*   structs, first invoking wcsfree() on each of the array members.
*
*   Given and returned:
*      nwcs     int*     Number of coordinate representations found; set to 0
*                        on return.
*      wcs      struct wcsprm**
*                        Pointer to the array of wcsprm structs; set to 0 on
*                        return.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Notes
*   -----
*    1) The parser interprets its "relax" argument as a vector of flag bits to
*       provide fine-grained control over what non-standard WCS cards to
*       accept.  The flag bits are subject to change in future and should be
*       set by using the preprocessor macros defined below for the purpose.
*
*         WCSHDR_all: Accept all extensions recognized by the parser (the
*               other flag bits are ignored).
*
*         WCSHDR_reject: Reject non-standard cards (that are not otherwise
*               accepted) and increment nreject.  This signals the presence of
*               such cards, otherwise they are simply discarded as though they
*               did not exist in the header.
*
*         WCSHDR_CROTAia: Accept CROTAia.
*         WCSHDR_EPOCHa:  Accept EPOCHa.
*         WCSHDR_VELREFa: Accept VELREFa.
*               The parser always recognizes the AIPS-convention keywords,
*               CROTAn, EPOCH, and VELREF for the primary representation
*               (a = ' ') but alternates are non-standard.
*
*         WCSHDR_CD00i00j: Accept CD00i00j.
*         WCSHDR_PC00i00j: Accept PC00i00j.
*         WCSHDR_PROJPn:   Accept PROJPn.
*               These appeared in early drafts of the WCS proposal and are
*               equivalent to CDi_ja, PCi_ja, and PVi_ma for the primary
*               representation (a = ' ').  PROJPn is equivalent to PVi_ma with
*               m = n <= 9, and is associated exclusively with the latitude
*               axis.
*
*       For example
*
*         relax = WCSHDR_reject | WCSHDR_CD00i00j | WCSHDR_PC00i00j;
*
*       Note that where CROTAn, CDi_ja, and PCi_ja occur together in one
*       header the parser treats them as described in the prologue to wcs.h.
*
*       The parser always treats EPOCH as subordinate to EQUINOXa if both are
*       present.
*
*       Likewise, VELREF is subordinate to the formalism of WCS Paper III.  In
*       the AIPS convention VELREF has the following values:
*         1: LSR kinematic, originally described as "LSR" without distinction
*         2: Barycentric,   originally described as "HEL" meaning heliocentric
*         3: Topocentric,   originally described as "OBS" meaning geocentric
*                           but widely interpreted as topocentric
*       AIPS++ extensions to VELREF are also recognized:
*         4: LSR dynamic
*         5: Geocentric
*         6: Source rest frame
*         7: Galactocentric
*       A radio convention velocity is denoted by adding 256 to these,
*       otherwise an optical velocity is indicated.
*
*       The parser does not currently recognize the AIPS-convention keywords
*       ALTRPIX or ALTRVAL which effectively define an alternative
*       representation for a spectral axis.
*
*
*   Error codes
*   -----------
*   Error messages to match the error codes returned from each function are
*   encoded in the wcs_errmsg character array.
*
*===========================================================================*/

#ifndef WCSLIB_WCSHDR
#define WCSLIB_WCSHDR

#include "wcs.h"

#define WCSHDR_all      1
#define WCSHDR_reject   2
#define WCSHDR_CROTAia  4
#define WCSHDR_EPOCHa   8
#define WCSHDR_VELREFa  16
#define WCSHDR_CD00i00j 32
#define WCSHDR_PC00i00j 64
#define WCSHDR_PROJPn   128

#ifdef __cplusplus
extern "C" {
#endif

#if __STDC__ || defined(__cplusplus)
   int wcspih(char *, int, int, int, int *, int *, struct wcsprm **);
   int wcsvfree(int *, struct wcsprm **);
#else
   int wcspih(), wcsvfree();
#endif

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCSHDR */
