/*============================================================================
*
*   WCSLIB 3.3 - an implementation of the FITS WCS convention.
*   Copyright (C) 1995-2003, Mark Calabretta
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
*   WCSLIB 3.3 - C routines that implement the spherical coordinate
*   transformations used by the FITS World Coordinate System (WCS) convention.
*   Refer to
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
*   The spherical coordinate transformations are implemented via separate
*   functions for the transformation in each direction.
*
*
*   Pixel-to-world transformation; sphx2s()
*   ---------------------------------------
*   Transform native coordinates of a projection to celestial coordinates.
*
*   Given:
*      eul      const double[5]
*                        Euler angles for the transformation:
*                          0: Celestial longitude of the native pole, in
*                             degrees.
*                          1: Celestial colatitude of the native pole, or
*                             native colatitude of the celestial pole, in
*                             degrees.
*                          2: Native longitude of the celestial pole, in
*                             degrees.
*                          3: cos(eul[1])
*                          4: sin(eul[1])
*      nphi,    int
*      ntheta            Vector lengths, see below.
*      spt,sxy  int      Vector strides, see below.
*      phi,     const double[]
*      theta             Longitude and latitude in the native coordinate
*                        system of the projection, in degrees.
*
*   Returned:
*      lng,lat  double[] Celestial longitude and latitude, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*
*
*   World-to-pixel transformation; sphs2x()
*   ---------------------------------------
*   Transform celestial coordinates to the native coordinates of a projection.
*
*   Given:
*      eul      const double[5]
*                        Euler angles for the transformation:
*                          0: Celestial longitude of the native pole, in
*                             degrees.
*                          1: Celestial colatitude of the native pole, or
*                             native colatitude of the celestial pole, in
*                             degrees.
*                          2: Native longitude of the celestial pole, in
*                             degrees.
*                          3: cos(eul[1])
*                          4: sin(eul[1])
*      nlng,    int
*      nlat              Vector lengths, see below.
*      sll,spt  int      Vector strides, see below.
*      lng,lat  const double[]
*                        Celestial longitude and latitude, in degrees.
*
*   Returned:
*      phi,     double[] Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
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
*   The following diagram describes the situation for sphs2x() with nlng = 3,
*   and nlat = 3:
*
*                    lng[0]         lng[1]         lng[2]
*                 ------------   ------------   ------------
*      lat[0]  |  phi,theta[0]   phi,theta[1]   phi,theta[2]
*      lat[1]  |  phi,theta[3]   phi,theta[4]   phi,theta[5]
*      lat[2]  |  phi,theta[6]   phi,theta[7]   phi,theta[8]
*
*   In this case the world-to-pixel routine would calculate nlng * nlat = 9
*   (phi,theta) coordinate pairs.  It is the responsibility of the caller to
*   ensure that sufficient space has been allocated in the phi[] and theta[]
*   arrays.
*
*   Vector computation will often be required where neither lng nor lat is
*   constant.  This is accomplished by setting nlat = 0 which is interpreted
*   to mean nlat = nlng but only the matrix diagonal is to be computed.  Thus,
*   for nlng = 3 and nlat = 0 only three (phi,theta) coordinate pairs are
*   computed:
*
*                    lng[0]         lng[1]         lng[2]
*                 ------------   ------------   ------------
*      lat[0]  |  phi,theta[0]
*      lat[1]  |                 phi,theta[1]
*      lat[2]  |                                phi,theta[2]
*
*   Note how this differs from nlng = 3, nlat = 1:
*
*                    lng[0]         lng[1]         lng[2]
*                 ------------   ------------   ------------
*      lat[0]  |  phi,theta[0]   phi,theta[1]   phi,theta[2]
*
*   The situation for sphx2s() is similar; the phi-coordinate (like lng)
*   varies fastest.
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
*   sll, as are the phi[] and theta[] arrays, spt.
*
*   If the vector length is 1 then the stride is ignored and may be set to 0.
*
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#ifndef WCSLIB_SPH
#define WCSLIB_SPH

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif


#if __STDC__  || defined(__cplusplus)
   int sphx2s(const double[], int, int, int, int,
              const double[], const double [],
              double[], double[]);

   int sphs2x(const double[], int, int, int, int,
              const double[], const double [],
              double[], double[]);
#else
   int sphx2s(), sphs2x();
#endif

#ifdef __cplusplus
}
#endif


/* Define macros for scalar invokation for compatibility with WCSLIB 2.x. */
#define sphrev(phi, theta, eul, lng, lat) \
        sphx2s(eul, 1, 1, 1, 1, &(phi), &(theta), lng, lat)
#define sphfwd(lng, lat, eul, phi, theta) \
        sphs2x(eul, 1, 1, 1, 1, &(lng), &(lat), phi, theta)

#endif /* WCSLIB_SPH */
