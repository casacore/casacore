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
*   Routines in this suite identify and translate various forms of non-
*   standard construct that are known to occur in FITS WCS headers.  These
*   range from the translation of non-standard values for standard WCS
*   keywords, to the repair of malformed coordinate representations.
*
*   Non-standard keyvalues:
*      AIPS-convention celestial projection types, -NCP and -GLS, and spectral
*      types, FREQ-LSR, FELO-HEL, etc., set in CTYPEia are translated on-the-
*      fly by wcsset() but without modifying the relevant ctype[], pv[] or
*      specsys members of the wcsprm struct.  That is, only the information
*      extracted from ctype[] is translated when wcsset() fills in the prj
*      (struct prjprm) or spc (struct spcprm) members of the wcsprm struct.
*
*      On the other hand, these routines do change the values of the ctype[],
*      pv[], specsys and other wcsprm struct members as appropriate to produce
*      the same result as if the FITS header itself had been translated.
*
*      Auxiliary WCS header information not used directly by WCSLIB may also
*      be translated.  For example, the older DATE-OBS date format (dateobs
*      wcsprm struct member) is recast to year-2000 standard form, and MJD-OBS
*      (mjdobs) will be deduced from it if not already set.
*
*      Certain combinations of keyvalues that result in malformed coordinate
*      systems, as described in Sect. 7.3.4 of Paper I, may also be repaired.
*      These are handled by cylfix().
*
*   Non-standard keywords:
*      The AIPS-convention CROTAn keywords are recognized as quasi-standard
*      and as such are accomodated by the crota[] member of the wcsprm struct
*      and translated to pc[][] by wcsset().  These are not dealt with here,
*      nor are any other non-standard keywords since these routines work only
*      on the contents of a wcsprm struct and do not deal with FITS headers
*      per se.  In particular, they do not identify or translate CD00i00j,
*      PC00i00j, PROJPn, EPOCH, VELREF or VSOURCEa header cards; this may be
*      done by the FITS WCS header parser supplied with WCSLIB, refer to the
*      prologue of wcshdr.h.
*
*   wcsfix() applies all of the corrections handled by the following specific
*   functions which may also be invoked separately:
*
*      datfix(): recast the older DATE-OBS date format in dateobs to year-2000
*         standard form, and derive mjdobs from it if not already set.
*         Alternatively, if dateobs isn't set and mjdobs is, then derive
*         dateobs from it.
*
*      unitfix(): translate some commonly used but non-standard unit strings
*         in the CUNITia keyvalues, e.g. 'DEG' -> 'deg'.
*
*      celfix(): translate AIPS-convention celestial projection types, -NCP
*         and -GLS, in ctype[] as set from CTYPEia.
*
*      spcfix(): translate AIPS-convention spectral types, FREQ-LSR, FELO-HEL,
*         etc., in ctype[] as set from CTYPEia.
*
*      cylfix(): fixes WCS FITS header cards for malformed cylindrical
*         projections that suffer from the problem described in Sect. 7.3.4 of
*         Paper I.
*
*
*   Translate a non-standard WCS struct
*   -----------------------------------
*   wcsfix() applies all of the corrections handled separately by datfix(),
*   unitfix(), celfix(), spcfix() and cylfix().
*
*   Given:
*      naxis    const int []
*                        Image axis lengths.  If this array pointer is set to
*                        zero then cylfix() will not be invoked.
*
*      ctrl     int      Do potentially unsafe translations of non-standard
*                        unit strings as described in the usage notes to
*                        wcsutrn() (refer to the prologue of wcsunits.h).
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Returned:
*      stat     int [NWCSFIX]
*                        Status returns from each of the functions.  Use the
*                        preprocessor macros defined below (NWCSFIX, DATFIX,
*                        UNITFIX, CELFIX, SPCFIX and CYLFIX) to dimension this
*                        vector and access its elements.  A status value of -2
*                        is set for functions that were not invoked.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: One or more of the translation functions
*                              returned an error.
*
*
*   Translate DATE-OBS and derive MJD-OBS or vice versa
*   ---------------------------------------------------
*   datfix() translates the old DATE-OBS date format set in the dateobs member
*   of the wcsprm struct to year-2000 standard form (yyyy-mm-ddThh:mm:ss) and
*   derives MJD-OBS from it if not already set.  Alternatively, if mjdobs is
*   set and dateobs isn't, then datfix() derives dateobs from it.  If both are
*   set but disagree by more than half a day then status 5 is returned.  The
*   dateobs and/or mjdobs members of the wcsprm struct may be changed.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Function return value:
*               int      Status return value:
*                          -1: No change required (not an error).
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           5: Invalid parameter value.
*
*
*   Correct aberrant CUNITia keyvalues
*   ----------------------------------
*   unitfix() applies wcsutrn() to translate non-standard CUNITia keyvalues,
*   e.g. 'DEG' -> 'deg', also stripping off unnecessary whitespace.
*
*   Given:
*      ctrl     int      Do potentially unsafe translations described in the
*                        usage notes to wcsutrn() (refer to the prologue of
*                        wcsunits.h).
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Function return value:
*               int      Status return value:
*                          -1: No change required (not an error).
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Translate AIPS-convention celestial projection types
*   ----------------------------------------------------
*   celfix() translates AIPS-convention celestial projection types, -NCP
*   and -GLS, set in the ctype[] member of the wcsprm struct.  The ctype[]
*   and/or pv[] members of the wcsprm struct may be changed.
*
*   Two additional pv[] cards are created when translating -NCP.  If the pv[]
*   array was initially allocated by wcsini() then the array will be expanded
*   if necessary.  Otherwise, error 2 will be returned if two empty slots are
*   not already available for use.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Function return value:
*               int      Status return value:
*                          -1: No change required (not an error).
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*
*
*   Translate AIPS-convention spectral types
*   ----------------------------------------
*   spcfix() translates AIPS-convention spectral coordinate types,
*   {FREQ,VELO,FELO}-{OBS,HEL,LSR} (e.g. FREQ-LSR, VELO-OBS, FELO-HEL) set in
*   the ctype[] member of the wcsprm struct.  The ctype[] and/or specsys
*   members of the wcsprm struct may be changed.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Function return value:
*               int      Status return value:
*                          -1: No change required (not an error).
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*
*
*   Fix malformed cylindrical projections
*   -------------------------------------
*   cylfix() fixes WCS FITS header cards for malformed cylindrical projections
*   that suffer from the problem described in Sect. 7.3.4 of Paper I.
*
*   Given:
*      naxis    const int []
*                        Image axis lengths.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (refer to the
*                        prologue of wcs.h).
*
*   Function return value:
*               int      Status return value:
*                          -1: No change required (not an error).
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*                           8: All of the corner pixel coordinates are
*                              invalid.
*                           9: Could not determine reference pixel coordinate.
*                          10: Could not determine reference pixel value.
*
*
*   Notes
*   -----
*    1) The MJD algorithms used by datfix() are from D.A. Hatcher, QJRAS 25,
*       53-55, as modified by P.T. Wallace for use in SLALIB subroutines CLDJ
*       and DJCL.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   encoded in the wcsfix_errmsg character array.
*
*===========================================================================*/

#ifndef WCSLIB_WCSFIX
#define WCSLIB_WCSFIX

#include "wcs.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DATFIX   0
#define UNITFIX  1
#define CELFIX   2
#define SPCFIX   3
#define CYLFIX   4
#define NWCSFIX  5

extern const char *wcsfix_errmsg[];
#define cylfix_errmsg wcsfix_errmsg


int wcsfix(int, const int [], struct wcsprm *, int []);
int datfix(struct wcsprm *);
int unitfix(int, struct wcsprm *);
int celfix(struct wcsprm *);
int spcfix(struct wcsprm *);
int cylfix(const int [], struct wcsprm *);

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCSFIX */
