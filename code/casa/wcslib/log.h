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
*   WCSLIB 4.1 - C routines that implement logarithmic coordinate systems as
*   defined by the FITS World Coordinate System (WCS) standard.  Refer to
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
*   These routines implement the part of the FITS WCS standard that deals with
*   logarithmic coordinates.  They define methods to be used for computing
*   logarithmic world coordinates from intermediate world coordinates (a
*   linear transformation of image pixel coordinates), and vice versa.
*
*   logx2s() and logs2x() implement the WCS logarithmic coordinate
*   transformations.
*
*
*   Transform to logarithmic coordinates; logx2s()
*   ----------------------------------------------
*   logx2s() transforms intermediate world coordinates to logarithmic
*   coordinates.
*
*   Given and returned:
*      crval    double   Log-coordinate reference value (CRVALia).
*
*   Given:
*      nx       int      Vector length, see below.
*      sx       int      Vector stride, see below.
*      slogc    int      Vector stride, see below.
*      x        const double[]
*                        Intermediate world coordinates, in SI units.
*
*   Returned:
*      logc     double[] Logarithmic coordinates, in SI units.
*      stat     int[]    Status return value status for each vector element:
*                           0: Success.
*                           1: Invalid value of x.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid log-coordinate reference value.
*                           3: One or more of the x coordinates were invalid,
*                              as indicated by the stat vector.
*
*
*   Transform logarithmic coordinates; logs2x()
*   -------------------------------------------
*   logs2x() transforms logarithmic world coordinates to intermediate world
*   coordinates.
*
*   Given and returned:
*      crval    double   Log-coordinate reference value (CRVALia).
*
*   Given:
*      nlogc    int      Vector length, see below.
*      slogc    int      Vector stride, see below.
*      sx       int      Vector stride, see below.
*      logc     const double[]
*                        Logarithmic coordinates, in SI units.
*
*   Returned:
*      x        double[] Intermediate world coordinates, in SI units.
*      stat     int[]    Status return value status for each vector element:
*                           0: Success.
*                           1: Invalid value of logc.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid log-coordinate reference value.
*
*
*   Vector lengths and strides
*   --------------------------
*   Vector computation in the log-coordinate code mainly speeds it by
*   amortising the function call overhead.  The vector length function
*   argument refers to the number of logarithmic coordinates in both the input
*   and output log-coordinate vectors.
*
*   The vector stride arguments allow the caller to specify that successive
*   elements of a vector are not contiguous in memory.  This applies equally
*   to vectors given to, or returned from a function.  As a simple example
*   consider the following two arrangements in memory of a log-coordinate
*   vector s[], with four elements, s1, s2, s3, and s4, together with x[] and
*   y[] vectors:
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
*   are encoded in the log_errmsg character array.
*
*
*   Argument checking
*   -----------------
*   The input log-coordinate values are only checked for values that would
*   result in floating point exceptions and the same is true for the log-
*   coordinate reference value.
*
*
*   Accuracy
*   --------
*   No warranty is given for the accuracy of these routines (refer to the
*   copyright notice above); intending users must satisfy for themselves their
*   adequacy for the intended purpose.  However, closure effectively to within
*   double precision rounding error was demonstrated by test routine tlog.c
*   which accompanies this software.
*
*===========================================================================*/

#ifndef WCSLIB_LOG
#define WCSLIB_LOG

#ifdef __cplusplus
extern "C" {
#endif


extern const char *log_errmsg[];


int logx2s(double, int, int, int, const double[], double[], int[]);
int logs2x(double, int, int, int, const double[], double[], int[]);


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_LOG */
