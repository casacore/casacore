/*============================================================================
*
*   WCSLIB 4.0 - an implementation of the FITS WCS standard.
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
*   WCSLIB 4.0 - C routines that implement the spectral coordinate systems
*   recognized by the FITS World Coordinate System (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2005, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   These routines implement the part of the FITS WCS standard that deals
*   with spectral coordinates.  They define methods to be used for computing
*   spectral world coordinates from intermediate world coordinates (a linear
*   transformation of image pixel coordinates), and vice versa.  They are
*   based on the spcprm struct, described in detail below, which contains all
*   information needed for the computations.  The struct contains some
*   members that must be set by the caller, and others that are maintained by
*   these routines, somewhat like a C++ class but with no encapsulation.
*
*   A service routine, spcini(), is provided to initialize the spcprm struct
*   with default values, and another, spcprt(), to print its contents.
*
*   A setup routine, spcset(), computes intermediate values in the spcprm
*   struct from parameters in it that were supplied by the caller.  The
*   struct always needs to be set up by spcset() but it need not be called
*   explicitly - see the explanation of spc.flag below.
*
*   spcx2s() and spcs2x() implement the WCS spectral coordinate
*   transformations.  In fact, they are high level driver routines for the
*   lower level spectral coordinate transformation routines described in
*   spx.h.
*
*   A number of routines are provided to aid in analysing or synthesising sets
*   of FITS spectral axis keywords:
*
*      spctyp() checks a spectral CTYPEia keyword for validity and returns
*      information derived from it.
*
*      Spectral keyword analysis routine spcspx() computes the values of the
*      X-type spectral variables for the S-type variables supplied.
*
*      Spectral keyword synthesis routine, spcxps(), computes the S-type
*      variables for the X-types supplied.
*
*      Given a set of spectral keywords, a translation routine, spctrn(),
*      produces the corresponding set for the specified spectral CTYPEia.
*
*
*   Default constructor for the spcprm struct; spcini()
*   ---------------------------------------------------
*   spcini() sets all members of a spcprm struct to default values.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spcprm pointer passed.
*
*
*   Print routine for the spcprm struct; spcprt()
*   ---------------------------------------------
*   spcprt() prints the contents of a spcprm struct.
*
*   Given:
*      spc      const struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spcprm pointer passed.
*
*
*   Setup routine; spcset()
*   -----------------------
*   spcset() sets up a spcprm struct according to information supplied within
*   it (see "Spectral transformation parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   spcx2s() and spcs2x() if the "flag" struct member is anything other than a
*   predefined magic value.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*
*
*   Transform to spectral coordinates; spcx2s()
*   -------------------------------------------
*   spcx2s() transforms intermediate world coordinates to spectral
*   coordinates.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Given:
*      nx       int      Vector length, see below.
*      sx,sspec int      Vector strides, see below.
*      x        const double[]
*                        Intermediate world coordinates, in SI units.
*
*   Returned:
*      spec     double[] Spectral coordinate, in SI units.
*      stat     int[]    Status return value status for each vector element:
*                           0: Success.
*                           1: Invalid value of x.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*                           3: One or more of the x coordinates were invalid,
*                              as indicated by the stat vector.
*
*
*   Transform spectral coordinates; spcs2x()
*   ----------------------------------------
*   spcs2x() transforms spectral world coordinates to intermediate world
*   coordinates.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Given:
*      nspec    int      Vector length, see below.
*      sspec    int
*      sx       int      Vector strides, see below.
*      spec     const double[]
*                        Spectral coordinate, in SI units.
*
*   Returned:
*      x        double[] Intermediate world coordinates, in SI units.
*      stat     int[]    Status return value status for each vector element:
*                           0: Success.
*                           1: Invalid value of spec.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*                           4: One or more of the spec coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Spectral CTYPEia keyword analysis; spctyp()
*   -------------------------------------------
*   spctyp() checks whether a CTYPEia keyvalue is a valid spectral axis type
*   and if so return information derived from it relating to the associated
*   S-, P-, and X-type spectral variables (explained below).
*
*   Given:
*      ctype    const char[9]
*                        The CTYPEia keyvalue, (eight characters with null
*                        termination).
*
*   Returned:
*      cname    char[]   Descriptive name of the S-type spectral variable.
*                        If a non-zero pointer is given, the array must
*                        accomodate a null-terminated string of length 22.
*      units    char[]   SI units of the S-type spectral variable.
*                        If a non-zero pointer is given, the array must
*                        accomodate a null-terminated string of length 8.
*      ptype    char*    Character code for the P-type spectral variable
*                        derived from ctype, one of 'F', 'W', 'A', or 'V'.
*      xtype    char*    Character code for the X-type spectral variable
*                        derived from ctype, one of 'F', 'W', 'A', or 'V'.
*                        Also, 'w' and 'a' are synonymous to 'W' and 'A' for
*                        grisms in vacuo and air respectively.
*      restreq  int*     Multivalued flag that indicates whether rest
*                        frequency or wavelength is required to compute
*                        spectral variables for this CTYPEia:
*                          0: Not required.
*                          1: Required for the conversion between S- and
*                             P-types (e.g. ZOPT-F2W).
*                          2: Required for the conversion between P- and
*                             X-types (e.g. BETA-W2V).
*                          3: Required for the conversion between S- and
*                             P-types, and between P- and X-types, but not
*                             between S- and X-types (this applies only for
*                             VRAD-V2F, VOPT-V2W, and ZOPT-V2W).
*                         Thus the rest frequency or wavelength is required
*                         for spectral coordinate computations (i.e. S-X) only
*                         if restreq%3 != 0.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*
*
*   Spectral keyword analysis; spcspx()
*   -----------------------------------
*   spcspx() analyses the CTYPEia and CRVALia FITS spectral axis keyword
*   values and return information about the associated X-type spectral
*   variable.
*
*   Given:
*      ctypeS   const char[9]
*                        Spectral axis type, i.e. the CTYPEia keyvalue, (eight
*                        characters with null termination).  For non-grism
*                        axes, the character code for the P-type spectral
*                        variable in the algorithm code (i.e. the eighth
*                        character of CTYPEia) may be set to '?' (it will not
*                        be reset).
*      crvalS   double   Value of the S-type spectral variable at the
*                        reference point, i.e. the CRVALia keyvalue, SI units.
*      restfrq  double   Rest frequency (Hz) and rest wavelength in vacuo (m),
*      restwav  double   only one of which need be given, the other should be
*                        set to zero.  Neither are required if the translation
*                        is between wave-characteristic types, or between
*                        velocity-characteristic types.  E.g., required for
*                        'VELO-F2V' -> 'ZOPT-F2W', but not required for
*                        'FREQ'     -> 'ZOPT-F2W'.
*
*   Returned:
*      ptype    char*    Character code for the P-type spectral variable
*                        derived from ctypeS, one of 'F', 'W', 'A', or 'V'.
*      xtype    char*    Character code for the X-type spectral variable
*                        derived from ctypeS, one of 'F', 'W', 'A', or 'V'.
*                        Also, 'w' and 'a' are synonymous to 'W' and 'A' for
*                        grisms in vacuo and air respectively; crvalX and dXdS
*                        (below) will conform to these.
*      restreq  int*     Multivalued flag that indicates whether rest
*                        frequency or wavelength is required to compute
*                        spectral variables for this CTYPEia, as for spctyp().
*      crvalX   double*  Value of the X-type spectral variable at the
*                        reference point, SI units.
*      dXdS     double*  The derivative, dX/dS, evaluated at the reference
*                        point, SI units.  Multiply the CDELTia keyvalue by
*                        this to get the pixel spacing in the X-type spectral
*                        coordinate.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*
*
*   Spectral keyword synthesis; spcxps()
*   ------------------------------------
*   spcxps(), for the spectral axis type specified and the value provided for
*   the X-type spectral variable at the reference point, deduces the value of
*   the FITS spectral axis keyword CRVALia and also the derivative dS/dX which
*   may be used to compute CDELTia.  See below for an explanation of the S-,
*   P-, and X-type spectral variables.
*
*   Given:
*      ctypeS   const char[9]
*                        The required spectral axis type, i.e. the CTYPEia
*                        keyvalue, (eight characters with null termination).
*                        For non-grism axes, the character code for the P-type
*                        spectral variable in the algorithm code (i.e. the
*                        eighth character of CTYPEia) may be set to '?' (it
*                        will not be reset).
*      crvalX   double   Value of the X-type spectral variable at the
*                        reference point (N.B. NOT the CRVALia keyvalue), SI
*                        units.
*      restfrq  double   Rest frequency (Hz) and rest wavelength in vacuo (m),
*      restwav  double   only one of which need be given, the other should be
*                        set to zero.  Neither are required if the translation
*                        is between wave-characteristic types, or between
*                        velocity-characteristic types.  E.g., required for
*                        'VELO-F2V' -> 'ZOPT-F2W', but not required for
*                        'FREQ'     -> 'ZOPT-F2W'.
*
*   Returned:
*      ptype    char*    Character code for the P-type spectral variable
*                        derived from ctypeS, one of 'F', 'W', 'A', or 'V'.
*      xtype    char*    Character code for the X-type spectral variable
*                        derived from ctypeS, one of 'F', 'W', 'A', or 'V'.
*                        Also, 'w' and 'a' are synonymous to 'W' and 'A' for
*                        grisms; crvalX and cdeltX must conform to these.
*      restreq  int*     Multivalued flag that indicates whether rest
*                        frequency or wavelength is required to compute
*                        spectral variables for this CTYPEia, as for spctyp().
*      crvalS   double*  Value of the S-type spectral variable at the
*                        reference point (i.e. the appropriate CRVALia
*                        keyvalue), SI units.
*      dSdX     double*  The derivative, dS/dX, evaluated at the reference
*                        point, SI units.  Multiply the pixel spacing in the
*                        X-type spectral coordinate by this to get the CDELTia
*                        keyvalue.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*
*
*   Spectral keyword translation; spctrn()
*   --------------------------------------
*   spctrn() translates a set of FITS spectral axis keywords into the
*   corresponding set for the specified spectral axis type.  For example, a
*   'FREQ' axis may be translated into 'ZOPT-F2W' and vice versa.
*
*   Given:
*      ctypeS1  const char[9]
*                        Spectral axis type, i.e. the CTYPEia keyvalue, (eight
*                        characters with null termination).  For non-grism
*                        axes, the character code for the P-type spectral
*                        variable in the algorithm code (i.e. the eighth
*                        character of CTYPEia) may be set to '?' (it will not
*                        be reset).
*      crvalS1  double   Value of the S-type spectral variable at the
*                        reference point, i.e. the CRVALia keyvalue, SI units.
*      cdeltS1  double   Increment of the S-type spectral variable at the
*                        reference point, SI units.
*      restfrq  double   Rest frequency (Hz) and rest wavelength in vacuo (m),
*      restwav  double   only one of which need be given, the other should be
*                        set to zero.  Neither are required if the translation
*                        is between wave-characteristic types, or between
*                        velocity-characteristic types.  E.g., required for
*                        'VELO-F2V' -> 'ZOPT-F2W', but not required for
*                        'FREQ'     -> 'ZOPT-F2W'.
*
*   Given and returned:
*      ctypeS2  char[9]  Required spectral axis type (eight characters with
*                        null termination).  The first four characters are
*                        required to be given and are never modified.  The
*                        remaining four, the algorithm code, are completely
*                        determined by, and must be consistent with, ctypeS1
*                        and the first four characters of ctypeS2.  A non-zero
*                        status value will be returned if they are
*                        inconsistent (see below).  However, if the final
*                        three characters are specified as "???", or if just
*                        the eighth character is specified as '?', the correct
*                        algorithm code will be substituted (applies for grism
*                        axes as well as non-grism).
*
*   Returned:
*      crvalS2  double*  Value of the new S-type spectral variable at the
*                        reference point, i.e. the new CRVALia keyvalue, SI
*                        units.
*      cdeltS1  double   Increment of the new S-type spectral variable at the
*                        reference point, i.e. the new CDELTia keyvalue, SI
*                        units.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*
*                        A status value of 2 will be returned if restfrq or
*                        restwav are not specified when required, or if
*                        ctypeS1 or ctypeS2 are self-inconsistent, or have
*                        different spectral X-type variables.
*
*
*   Spectral variable types: S, P, and X
*   ------------------------------------
*   Every FITS spectral axis has three associated spectral variables:
*
*      S-type: the spectral variable in which coordinates are to be expressed.
*              Each S-type is encoded as four characters and is linearly
*              related to one of four basic types as follows:
*
*              F, frequency:
*                FREQ  frequency
*                AFRQ  angular frequency
*                ENER  photon energy
*                WAVN  wave number
*                VRAD  radio velocity
*
*              W, wavelength in vacuo:
*                WAVE  wavelength
*                VOPT  optical velocity
*                ZOPT  redshift
*
*              A, wavelength in air:
*                AWAV  wavelength in air
*
*              V, velocity:
*                VELO  relativistic velocity
*                BETA  relativistic beta factor
*
*              The S-type forms the first four characters of the CTYPEia
*              keyvalue, and CRVALia and CDELTia are expressed as S-type
*              quantities.
*
*      P-type: the basic spectral variable (F, W, A, or V) with which the
*              S-type variable is associated, as in the above list.
*
*              For non-grism axes, the P-type is encoded as the eighth
*              character of CTYPEia.
*
*      X-type: the basic spectral variable (F, W, A, or V) for which the
*              spectral axis is linear (except for grisms, see below).
*
*              For non-grism axes, the X-type is encoded as the sixth
*              character of CTYPEia.
*
*   Grism axes have normal S-, and P-types but the axis is linear, not in any
*   spectral variable, but in a special "grism parameter".  The X-type
*   spectral variable is either W or A for grisms in vacuo or air
*   respectively, but is encoded as 'w' or 'a' to indicate that an additional
*   transformation is required to convert to or from the grism parameter.  The
*   spectral algorithm code for grisms also has a special encoding in CTYPEia,
*   either '-GRI' (in vacuo) or '-GRA' (in air).
*
*   In the algorithm chain, the non-linear transformation occurs between the
*   X-type and the P-type variables; the transformation between P-type and
*   S-type variables is always linear.
*
*   When the P-type and X-type variables are the same, the spectral axis is
*   linear in the S-type variable and the second four characters of CTYPEia
*   are blank.  This can never happen for grism axes.
*
*   As an example, correlating radio spectrometers always produce spectra that
*   are regularly gridded in frequency; a redshift scale on such a spectrum is
*   non-linear.  The required value of CTYPEia would be 'ZOPT-F2W', where the
*   S-type is 'ZOPT' (redshift), the P-type is necessarily 'W' (wavelength),
*   and the X-type is 'F' (frequency) by the nature of the instrument.
*
*
*   Spectral transformation parameters
*   ----------------------------------
*   The spcprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following spcprm
*         structure members are set or changed: type, code, crval, restfrq,
*         restwav, or pv[].  This signals the initialization routine,
*         spcset(), to recompute intermediaries.
*
*      char type[8]
*         Four-letter spectral variable type, e.g "ZOPT" for
*         CTYPEia = "ZOPT-F2W".  (Declared as char[8] for alignment reasons.)
*
*      char code[4]
*         Three-letter spectral algorithm code, e.g "F2W" for
*         CTYPEia = "ZOPT-F2W".
*
*      double crval
*         Reference value (CRVALia), SI units.
*
*      double restfrq, restwav
*         Rest frequency (Hz) and rest wavelength in vacuo (m), only one of
*                        which need be given, the other should be set to zero.
*                        Neither are required if the X and S spectral
*                        variables are both wave-characteristic, or both
*                        velocity-characteristic, types.
*
*      double pv[7]
*         Grism parameters for "-GRI" and "-GRA" algorithm codes:
*            0: G, grating ruling density.
*            1: m, interference order.
*            2: alpha, angle of incidence.
*            3: n_r, refractive index at lambda_r.
*            4: n'_r, dn/dlambda at lambda_r.
*            5: epsilon, grating tilt angle.
*            6: theta, detector tilt angle.
*
*   The remaining members of the spcprm struct are maintained by spcset() and
*   must not be modified elsewhere:
*
*      double w[6]
*         Intermediate values:
*            0: Rest frequency or wavelength (SI).
*            1: The value of the X-type spectral variable at the reference
*               point (SI units).
*            2: dX/dS at the reference point (SI units).
*         The remainder are grism intermediates.
*
*      int isGrism
*         Grism coordinates?  1: vacuum, 2: air.
*
*      int (*spxX2P)(SPX_ARGS)
*      int (*spxP2S)(SPX_ARGS)
*         Pointers to the transformation functions in the two-step algorithm
*         chain X -> P -> S in the pixel-to-spectral direction.
*
*      int (*spxS2P)(SPX_ARGS)
*      int (*spxP2X)(SPX_ARGS)
*         Pointers to the transformation functions in the two-step algorithm
*         chain S -> P -> X in the spectral-to-pixel direction.
*
*
*   Vector lengths and strides
*   --------------------------
*   Vector computation in the spectral code mainly speeds it by amortising the
*   function call overhead.  The vector length function argument refers to the
*   number of spectral coordinates in both the input and output spectral
*   vectors.
*
*   The vector stride arguments allow the caller to specify that successive
*   elements of a vector are not contiguous in memory.  This applies equally
*   to vectors given to, or returned from a function.  As a simple example
*   consider the following two arrangements in memory of a spectral vector
*   s[], with four elements, s1, s2, s3, and s4, together with x[] and y[]
*   vectors:
*
*      1) s1 s2 s3 s4 x1 x2 x3 x4 y1 y2 y3 y4
*
*      2) x1 y1 s1 x2 y2 s2 x3 y3 s3 x4 y4 s4
*
*   In (1), the address of s[] is s1, its stride is 1, and length 4.
*   In (2), the address of s[] is s1, its stride is 3, and length 4.
*
*   If the vector length is 1 then the stride is ignored and may be set to 0.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   are encoded in the spc_errmsg character array.
*
*
*   Argument checking
*   -----------------
*   The input spectral values are only checked for values that would result
*   in floating point exceptions.  In particular, negative frequencies and
*   wavelengths are allowed, as are velocities greater than the speed of
*   light.  The same is true for the spectral parameters - rest frequency and
*   wavelength.
*
*
*   Accuracy
*   --------
*   No warranty is given for the accuracy of these routines (refer to the
*   copyright notice above); intending users must satisfy for themselves their
*   adequacy for the intended purpose.  However, closure effectively to within
*   double precision rounding error was demonstrated by test routine tspc.c
*   which accompanies this software.
*
*===========================================================================*/

#ifndef WCSLIB_SPC
#define WCSLIB_SPC

#include "spx.h"

#ifdef __cplusplus
extern "C" {
#endif


extern const char *spc_errmsg[];
#define spcini_errmsg spc_errmsg
#define spcprt_errmsg spc_errmsg
#define spcset_errmsg spc_errmsg
#define spcx2s_errmsg spc_errmsg
#define spcs2x_errmsg spc_errmsg

extern const int  spc_ncode;
extern const char spc_codes[15][4];


struct spcprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* Parameters to be provided (see the prologue above).                   */
   /*-----------------------------------------------------------------------*/
   char   type[8];		/* Four-letter spectral variable type.      */
   char   code[4];		/* Three-letter spectral algorithm code.    */

   double crval;		/* Reference value (CRVALia), SI units.     */
   double restfrq;		/* Rest frequency, Hz.                      */
   double restwav;		/* Rest wavelength, m.                      */

   double pv[7];		/* Grism parameters:                        */
				/*   0: G, grating ruling density.          */
				/*   1: m, interference order.              */
				/*   2: alpha, angle of incidence.          */
				/*   3: n_r, refractive index at lambda_r.  */
				/*   4: n'_r, dn/dlambda at lambda_r.       */
				/*   5: epsilon, grating tilt angle.        */
				/*   6: theta, detector tilt angle.         */

   /* Information derived from the parameters supplied.                     */
   /*-----------------------------------------------------------------------*/
   double w[6];			/* Intermediate values.                     */
				/*   0: Rest frequency or wavelength (SI).  */
				/*   1: CRVALX (SI units).                  */
				/*   2: CDELTX/CDELTia = dX/dS (SI units).  */
				/* The remainder are grism intermediates.   */

   int isGrism;			/* Grism coordinates?  1: vacuum, 2: air.   */

   int (*spxX2P)(SPX_ARGS);	/* Pointers to the transformation functions */
   int (*spxP2S)(SPX_ARGS);	/* in the two-step algorithm chain in the   */
				/* pixel-to-spectral direction.             */

   int (*spxS2P)(SPX_ARGS);	/* Pointers to the transformation functions */
   int (*spxP2X)(SPX_ARGS);	/* in the two-step algorithm chain in the   */
				/* spectral-to-pixel direction.             */
   int padding;			/* (Dummy inserted for alignment purposes.) */
};

#define SPCLEN (sizeof(struct spcprm)/sizeof(int))


int spcini(struct spcprm *);
int spcprt(const struct spcprm *);
int spcset(struct spcprm *);
int spcx2s(struct spcprm *, int, int, int, const double[], double[], int[]);
int spcs2x(struct spcprm *, int, int, int, const double[], double[], int[]);
int spctyp(const char[], char[], char[], char *, char *, int *);
int spcspx(const char[], double, double, double, char *, char *, int *,
           double *, double *);
int spcxps(const char[], double, double, double, char *, char *, int *,
           double *, double *);
int spctrn(const char[], double, double, double, double, char[], double *,
           double *);


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_SPC */
