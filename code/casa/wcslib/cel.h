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
*   WCSLIB 3.4 - C routines that implement the FITS World Coordinate System
*   (WCS) convention.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (paper II)
*
*
*   Summary of routines
*   -------------------
*   These routines are provided as drivers for the lower level spherical
*   coordinate transformation and projection routines.  There are separate
*   driver routines for the pixel-to-world, celx2s(), and world-to-pixel,
*   cels2x(), transformations.
*
*   An initialization routine, celset(), computes intermediate values from
*   the transformation parameters but need not be called explicitly - see the
*   explanation of cel.flag below.
*
*   A service routine, celini(), is provided to initialize the celprm struct,
*   and another, celprt(), to print its contents.
*
*
*   Initialization routine for the celprm struct; celini()
*   ------------------------------------------------------
*   This service routine may be used to set the members of a celprm struct to
*   default values.
*
*   Returned:
*      cel      struct celprm*
*                        Celestial transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null celprm pointer passed.
*
*
*   Print routine for the celprm struct; celprt()
*   ---------------------------------------------
*   This service routine may be used to print the members of a celprm struct.
*
*   Given:
*      cel      const struct celprm*
*                        Celestial transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null celprm pointer passed.
*
*
*   Initialization routine; celset()
*   --------------------------------
*   Initializes a celprm data structure according to information supplied
*   within it (see "Celestial transformation parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   celx2s() and cels2x() if the "flag" structure member is anything other
*   than a predefined magic value.
*
*   Given and returned:
*      cel      struct celprm*
*                        Celestial transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null celprm pointer passed.
*                           2: Invalid projection parameters.
*                           3: Invalid coordinate transformation parameters.
*                           4: Ill-conditioned coordinate transformation
*                              parameters.
*
*
*   Pixel-to-world transformation; celx2s()
*   ---------------------------------------
*   Compute the celestial coordinates (lng,lat) of the point with projected
*   coordinates (x,y).
*
*   Given and returned:
*      cel      struct celprm*
*                        Celestial transformation parameters (see below).
*
*   Given:
*      nx,ny    int      Vector lengths, see below.
*      sxy,sll  int      Vector strides, see below.
*      x,y      const double[]
*                        Projected coordinates, "degrees".
*
*   Returned:
*      phi,     double[] Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*      lng,lat  double[] Celestial longitude and latitude of the projected
*                        point, in degrees.
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of (x,y).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null celprm pointer passed.
*                           2: Invalid projection parameters.
*                           3: Invalid coordinate transformation parameters.
*                           4: Ill-conditioned coordinate transformation
*                              parameters.
*                           5: One or more of the (x,y) coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   World-to-pixel transformation; cels2x()
*   ---------------------------------------
*   Compute (x,y) coordinates in the plane of projection from celestial
*   coordinates (lng,lat).
*
*   Given and returned:
*      cel      struct celprm*
*                        Celestial transformation parameters (see below).
*
*   Given:
*      nlng,    int
*      nlat              Vector lengths, see below.
*      sll,sxy  int      Vector strides, see below.
*      lng,lat  const double[]
*                        Celestial longitude and latitude of the projected
*                        point, in degrees.
*
*   Returned:
*      phi,     double[] Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*      x,y      double[] Projected coordinates, "degrees".
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of (lng,lat).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null celprm pointer passed.
*                           2: Invalid projection parameters.
*                           3: Invalid coordinate transformation parameters.
*                           4: Ill-conditioned coordinate transformation
*                              parameters.
*                           6: One or more of the (lng,lat) coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Celestial transformation parameters
*   -----------------------------------
*   The celprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following celprm
*         structure members are set or changed: prj.code, prj.r0, prj.pv[],
*         prj.phi0, prj.theta0, ref[4], phi0, theta0, offset.  This signals
*         the initialization routine, celset(), to recompute intermediaries.
*
*      int offset
*         If true, an offset will be applied to (x,y) to force (x,y) = (0,0)
*         at the fiducial point.
*
*      double phi0, theta0
*         The native longitude and latitude of the fiducial point, i.e. the
*         point whose celestial coordinates are given in ref[1:2].  If
*         undefined (set to a magic value by prjini()) the initialization
*         routine, celset(), will set this to a projection-specific default.
*
*      double ref[4]
*         The first pair of values should be set to the celestial longitude
*         and latitude (usually right ascension and declination) of the
*         fiducial point.  These are given by the CRVALi keywords in FITS.
*
*         The second pair of values are the native longitude and latitude of
*         the celestial pole (the latter is the same as the celestial latitude
*         of the native pole) and correspond to the FITS keywords LONPOLE and
*         LATPOLE.
*
*         LONPOLE defaults to 0 degrees if the celestial latitude of the
*         fiducial point of the projection is greater than the native
*         latitude, otherwise 180 degrees.  (This is the condition for the
*         celestial latitude to increase in the same direction as the native
*         latitude at the fiducial point.)  ref[2] may be set to 999.0 to
*         indicate that the correct default should be substituted.
*
*         In some circumstances the celestial latitude of the native pole may
*         be determined by the first three values only to within a sign and
*         LATPOLE is used to choose between the two solutions.  LATPOLE is
*         set in ref[3] and the solution closest to this value is used to
*         reset ref[3].  It is therefore legitimate, for example, to set
*         ref[3] to 999.0 to choose the more northerly solution - the default
*         if the LATPOLE card is omitted from the FITS header.  For the
*         special case where the fiducial point of the projection is at native
*         latitude zero, its celestial latitude is zero, and LONPOLE = +/- 90
*         then the celestial latitude of the native pole is not determined by
*         the first three reference values and LATPOLE specifies it
*         completely.
*
*      struct prjprm prj
*         Projection parameters described in the prologue to prj.h.
*
*   The remaining members of the celprm struct are maintained by celset() and
*   must not be modified elsewhere:
*
*      double euler[5]
*         Euler angles and associated intermediaries derived from the
*         coordinate reference values.  The first three values are the Z-, X-,
*         and Z'-Euler angles, and the remaining two are the cosine and sine
*         of the X-Euler angle.
*
*      int isolat
*         True if the spherical rotation preserves the magnitude of the
*         latitude, which occurs iff the axes of the native and celestial
*         coordinates are coincident.  It flags the opportunity to cache
*         intermediate calculations common to all elements in a vector
*         computation.
*
*
*   Vector lengths
*   --------------
*   Vector computations provide an opportunity for caching intermediate
*   calculations and this may result in a much greater speedup than simple
*   amortisation of the function call overhead.  Accordingly, the
*   transformation routines are matrix-oriented with input coordinate pairs
*   specified via pairs of vectors (one-dimensional C arrays).
*
*   The following diagram describes the situation for cels2x() with nlng = 3,
*   and nlat = 3:
*
*                 lng[0]   lng[1]   lng[2]
*                 ------   ------   ------
*      lat[0]  |  x,y[0]   x,y[1]   x,y[2]
*      lat[1]  |  x,y[3]   x,y[4]   x,y[5]
*      lat[2]  |  x,y[6]   x,y[7]   x,y[8]
*
*   In this case the world-to-pixel routine would calculate nlng * nlat = 9
*   (x,y) coordinate pairs.  It is the responsibility of the caller to ensure
*   that sufficient space has been allocated in the x[] and y[] arrays.
*
*   Vector computation will often be required where neither lng nor lat is
*   constant.  This is accomplished by setting nlat = 0 which is interpreted
*   to mean nlat = nlng but only the matrix diagonal is to be computed.  Thus,
*   for nlng = 3 and nlat = 0 only three (x,y) coordinate pairs are
*   computed:
*
*                 lng[0]   lng[1]   lng[2]
*                 ------   ------   ------
*      lat[0]  |  x,y[0]
*      lat[1]  |           x,y[1]
*      lat[2]  |                    x,y[2]
*
*   Note how this differs from nlng = 3, nlat = 1:
*
*                 lng[0]   lng[1]   lng[2]
*                 ------   ------   ------
*      lat[0]  |  x,y[0]   x,y[1]   x,y[2]
*
*   The situation for celx2s() is similar; the x-coordinate (like lng) varies
*   fastest.
*
*
*   Vector strides
*   --------------
*   The vector stride arguments allow the caller to specify that successive
*   elements of a vector are not contiguous in memory.  This applies equally
*   to vectors given to, or returned from a function.  As a simple example
*   consider the following two arrangements in memory of the elements of four
*   (x,y) coordinate pairs together with an unrelated z coordinate element:
*
*      1) x1 x2 x3 x4 y1 y2 y3 y4 z1 z2 z3 z4
*
*      2) x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4
*
*   In (1), the address of x[] is x1, its stride is 1, and length 4.
*           the address of y[] is y1, its stride is 1, and length 4.
*
*   In (2), the address of x[] is x1, its stride is 3, and length 4.
*           the address of y[] is y1, its stride is 3, and length 4.
*
*   The lng[] and lat[] arrays are assumed to have the same vector stride,
*   sll, as are the x[] and y[] arrays, sxy.  However, the intermediate phi[]
*   and theta[] arrays are each assumed to have a stride of 1.
*
*   If the vector length is 1 then the stride is ignored and may be set to 0.
*
*
*   Error codes
*   -----------
*   Error messages to match the error codes returned from each function are
*   encoded in the cel_errmsg character array.
*
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#ifndef WCSLIB_CEL
#define WCSLIB_CEL

#include "prj.h"

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif


extern const char *cel_errmsg[];
#define celini_errmsg cel_errmsg
#define celprt_errmsg cel_errmsg
#define celset_errmsg cel_errmsg
#define celx2s_errmsg cel_errmsg
#define cels2x_errmsg cel_errmsg


struct celprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* Parameters to be provided (see the prologue above).                   */
   /*-----------------------------------------------------------------------*/
   int    offset;		/* Force (x,y) = (0,0) at (phi0,theta0).    */
   double phi0, theta0;		/* Native coordinates of fiducial point.    */
   double ref[4];		/* Celestial coordinates of fiducial        */
                                /* point and native coordinates of          */
                                /* celestial pole.                          */

   struct prjprm prj;		/* Projection parameters (see prj.h).       */

   /* Information derived from the parameters supplied.                     */
   /*-----------------------------------------------------------------------*/
   double euler[5];		/* Euler angles and functions thereof.      */
   int    isolat;		/* True if |latitude| is preserved.         */
   int    padding;		/* (Dummy inserted for alignment purposes.) */
};

#if __STDC__  || defined(__cplusplus)
   int celini(struct celprm *);

   int celprt(const struct celprm *);

   int celset(struct celprm *);

   int celx2s(struct celprm *, int, int, int, int,
              const double[], const double[],
              double[], double[], double[], double[], int[]);

   int cels2x(struct celprm *, int, int, int, int,
              const double[], const double[],
              double[], double[], double[], double[], int[]);
#else
   int celini(), celprt(), celset(), celx2s(), cels2x();
#endif

#define CELLEN (sizeof(struct celprm)/sizeof(int))

#ifdef __cplusplus
}
#endif

#endif /* WCSLIB_CEL */
