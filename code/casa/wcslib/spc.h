/*============================================================================
*
*   WCSLIB 3.4 - an implementation of the FITS WCS convention.
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
*=============================================================================
*
*   WCSLIB 3.4 - C routines that implement the spectral coordinate systems
*   recognized by the FITS World Coordinate System (WCS) convention.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2004, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   These routines are provided as drivers for the lower level spectral
*   transformation routines.  Separate routines, spcx2s() and spcs2x(),
*   perform the transformation in either direction.
*
*   An initialization routine, spcset(), computes intermediate values from
*   the transformation parameters but need not be called explicitly - see the
*   explanation of spc.flag below.
*
*   A service routine, spcini(), is provided to initialize the spcprm struct,
*   and another, spcprt(), to print its contents.
*
*
*   Initialization routine for the spcprm struct; spcini()
*   ------------------------------------------------------
*   This service routine may be used to set the members of a spcprm struct to
*   default values.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null spcprm pointer passed.
*
*
*   Print routine for the spcprm struct; spcprt()
*   ---------------------------------------------
*   This service routine may be used to print the members of a spcprm struct.
*
*   Given:
*      spc      const struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null spcprm pointer passed.
*
*
*   Initialization routine; spcset()
*   --------------------------------
*   Initializes a spcprm data structure according to information supplied
*   within it (see "Spectral transformation parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   spcx2s() and spcs2x() if the "flag" structure member is anything other
*   than a predefined magic value.
*
*   Given and returned:
*      spc      struct spcprm*
*                        Spectral transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*
*
*   Transform to spectral coordinates; spcx2s()
*   -------------------------------------------
*   Transform intermediate world coordinates to spectral coordinates.
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
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of x.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*                           3: One or more of the x coordinates were invalid,
*                              as indicated by the stat vector.
*
*
*   Transform spectral coordinates; spcs2x()
*   ----------------------------------------
*   Transforms spectral world coordinates to intermediate world coordinates.
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
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of spec.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null spcprm pointer passed.
*                           2: Invalid spectral parameters.
*                           4: One or more of the spec coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Spectral transformation parameters
*   ----------------------------------
*   The spcprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following spcprm
*         structure members are set or changed: code, type, crval, restfrq,
*         restwav, or pv[].  This signals the initialization routine,
*         spcset(), to recompute intermediaries.
*
*      char type[8]
*         Four-letter spectral coordinate type, e.g "ZOPT" for
*         CTYPEk = "ZOPT-F2W".  (Declared as char[8] for alignment reasons.)
*
*      char code[4]
*         Three-letter spectral algorithm code, e.g "F2W" for
*         CTYPEk = "ZOPT-F2W".
*
*      double crval
*         Reference value (CRVALk), SI units.
*
*      double restfrq
*         Rest frequency, Hz.
*
*      double restwav
*         Rest wavelength, m.
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
*            1: CRVALX (SI units).
*            2: CDELTX (SI units).
*         The remainder are grism intermediates.
*
*      int isGrism
*         Grism coordinates?  1: vacuum, 2: air.
*
*      int (*spxx2q)(SPX_ARGS)
*      int (*spxq2s)(SPX_ARGS)
*         Pointers to the transformation functions in the two-step algorithm
*         chain X -> Q -> S in the pixel-to-spectral direction.
*
*      int (*spxs2q)(SPX_ARGS)
*      int (*spxq2x)(SPX_ARGS)
*         Pointers to the transformation functions in the two-step algorithm
*         chain S -> Q -> X in the spectral-to-pixel direction.
*
*
*   Vector length and strides
*   -------------------------
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
*   Error codes
*   -----------
*   Error messages to match the error codes returned from each function are
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
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#ifndef WCSLIB_SPC
#define WCSLIB_SPC

#include "spx.h"

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
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
   char   type[8];		/* Four-letter spectral coordinate type.    */
   char   code[4];		/* Three-letter spectral algorithm code.    */

   double crval;		/* Reference value (CRVALk), SI units.      */
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
				/*   2: CDELTX/CDELTi = dX/dw (SI units).   */
				/* The remainder are grism intermediates.   */

   int isGrism;			/* Grism coordinates?  1: vacuum, 2: air.   */

   int (*spxx2q)(SPX_ARGS);	/* Pointers to the transformation functions */
   int (*spxq2s)(SPX_ARGS);	/* in the two-step algorithm chain in the   */
				/* pixel-to-spectral direction.             */

   int (*spxs2q)(SPX_ARGS);	/* Pointers to the transformation functions */
   int (*spxq2x)(SPX_ARGS);	/* in the two-step algorithm chain in the   */
				/* spectral-to-pixel direction.             */
   int padding;			/* (Dummy inserted for alignment purposes.) */
};

#define SPCLEN (sizeof(struct spcprm)/sizeof(int))


/* Use the preprocessor to define function prototypes. */
#ifdef INI
#undef INI
#endif

#ifdef PRT
#undef PRT
#endif

#ifdef SET
#undef SET
#endif

#ifdef X2S
#undef X2S
#endif

#ifdef S2X
#undef S2X
#endif

#if __STDC__ || defined(__cplusplus)
#define INI struct spcprm *
#define PRT const struct spcprm *
#define SET struct spcprm *
#define X2S struct spcprm *, int, int, int, const double[], double[], int[]
#define S2X struct spcprm *, int, int, int, const double[], double[], int[]
#else
#define INI
#define PRT
#define SET
#define X2S
#define S2X
#endif

int spcini(INI);
int spcprt(PRT);
int spcset(SET);
int spcx2s(X2S);
int spcs2x(S2X);

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_SPC */
