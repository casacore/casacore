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
*   http://www.atnf.csiro.au/~mcalabre/index.html
*   $Id$
*=============================================================================
*
*   WCSLIB 4.1 - C routines that implement the FITS World Coordinate System
*   (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (paper II)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2005, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   wcspih() is a high-level FITS WCS routine that parses an image header.  It
*   returns an array of up to 27 wcsprm structs.
*
*   wcsidx() is a utility routine that returns the index for a specified
*   alternate coordinate descriptor in the array of wcsprm structs returned
*   by wcspih().
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
*   drafts of the WCS papers (see note 3 below).
*
*   Given a character array containing a FITS header, wcspih() identifies and
*   reads all WCS cards for the primary coordinate description and up to 26
*   alternate descriptions.  It returns this information as an array of wcsprm
*   structs.
*
*   Given and returned:
*      header   char[]   Character array containing the (entire) FITS header
*                        from which to idenfify and construct the coordinate
*                        representations, for example, as might be obtained
*                        conveniently via the CFITSIO routine fits_hdr2str().
*
*                        Each header "card" consists of exactly 80 7-bit ASCII
*                        printing characters in the range 0x20 to 0x7e (which
*                        excludes NUL, BS, TAB, LF, FF and CR) especially
*                        noting that the cards are NOT null-terminated.
*
*                        For negative values of ctrl (see below), header[]
*                        is modified so that WCS cards consumed by wcspih()
*                        are removed from it.
*
*   Given:
*      ncards   int      Number of cards in header[].
*      relax    int      Degree of permissiveness:
*                           0: Recognize only FITS keywords defined by the
*                              published WCS standard.
*                           1: Admit all recognized informal extensions of the
*                              WCS standard.
*                        Fine-grained control of the degree of permissiveness
*                        is also possible, see note 3 below.
*      ctrl  int         Error reporting and other control options for invalid
*                        WCS and other header cards:
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
*                        For ctrl < 0, WCS cards processed by wcspih() are
*                        removed from header[]:
*                          -1: Remove only valid WCS cards whose values were
*                              successfully extracted, nothing is reported.
*                          -2: Also remove WCS cards that were rejected,
*                              reporting each one and the reason that it was
*                              rejected.
*                          -3: As above, and also report the number of
*                              coordinate representations (nwcs) found.
*                        If any cards are removed from header[] it will be
*                        null-terminated (NUL not being a legal FITS header
*                        character), otherwise it will contain its original
*                        complement of ncards cards and possibly not be null-
*                        terminated.  
*
*   Returned:
*      nreject  int*     Number of WCS cards rejected for syntax errors,
*                        illegal values, etc.  Cards not recognized as WCS
*                        cards are simply ignored (but see also note 3 below).
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
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*
*
*   Utility routine for indexing alternate coordinate descriptions; wcsidx()
*   ------------------------------------------------------------------------
*   wcsidx() returns an array of 27 indices for the alternate coordinate
*   descriptions in the array of wcsprm structs returned by wcspih().
*
*   Given:
*      nwcs     int      Number of coordinate representations in the array.
*      wcs      const struct wcsprm**
*                        Pointer to an array of wcsprm structs containing up
*                        to 27 coordinate representations.
*
*   Returned:
*      alts     int[27]  Index of each alternate coordinate description in the
*                        array: alts[0] for the primary, alts[1] for 'A',
*                        etc., set to -1 if not present.  For example, the
*                        address of the wcsprm struct for the 'P' description
*                        would be wcs + alts['P'-'A'+1].  If the 'P'
*                        description was not present then alts['P'-'A'+1]
*                        would be set to -1.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
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
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Notes
*   -----
*    1) The parser enforces correct FITS "keyword = value" syntax with regard
*       to "= " occurring in columns 9 and 10.
*
*       However, it does recognize free-format character (NOST 100-2.0,
*       Sect. 5.2.1), integer (Sect. 5.2.3), and floating-point values
*       (Sect. 5.2.4) for all keywords.
*
*    2) Where CROTAn, CDi_ja, and PCi_ja occur together in one header the
*       parser treats them as described in the prologue to wcs.h.
*
*    3) The parser interprets its "relax" argument as a vector of flag bits to
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
*         WCSHDR_VSOURCEa: Accept VSOURCEa.
*               This appeared in early drafts of the WCS Paper III and was
*               subsequently dropped in favour of ZSOURCEa.
*
*       For example, to accept CD00i00j and PC00i00j and reject all other
*       extensions, use
*
*         relax = WCSHDR_reject | WCSHDR_CD00i00j | WCSHDR_PC00i00j;
*
*       The parser always treats EPOCH as subordinate to EQUINOXa if both are
*       present, and VSOURCEa is always subordinate to ZSOURCEa.
*
*       Likewise, VELREF is subordinate to the formalism of WCS Paper III.  In
*       the AIPS convention VELREF has the following integer values:
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
*    4) The parser does not check for duplicated cards, it accepts the last
*       encountered.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   encoded in the wcs_errmsg character array.
*
*===========================================================================*/

#ifndef WCSLIB_WCSHDR
#define WCSLIB_WCSHDR

#include "wcs.h"

#ifdef __cplusplus
extern "C" {
#endif


#define WCSHDR_all      0x001
#define WCSHDR_reject   0x002
#define WCSHDR_CROTAia  0x004
#define WCSHDR_EPOCHa   0x008
#define WCSHDR_VELREFa  0x010
#define WCSHDR_CD00i00j 0x020
#define WCSHDR_PC00i00j 0x040
#define WCSHDR_PROJPn   0x080
#define WCSHDR_VSOURCEa 0x100


int wcspih(char *, int, int, int, int *, int *, struct wcsprm **);
int wcsidx(int, struct wcsprm **, int [27]);
int wcsvfree(int *, struct wcsprm **);


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCSHDR */
