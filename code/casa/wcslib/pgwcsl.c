/*============================================================================
*
*   PGSBOX 3.2 - a non-linear coordinate axis plotter for PGPLOT.
*   Copyright (C) 1997-2003, Mark Calabretta
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
*   pgwcsl_() is an NLFUNC for PGSBOX that defines curvilinear celestial
*   coordinate systems by interfacing to WCSLIB 3.x.
*
*   Since WCSLIB 3.x is a C library, pgwcsl_() is written in C.  However, as
*   PGSBOX expects NLFUNC to be a FORTRAN subroutine, its interfaces
*   necessarily emulate those of a FORTRAN subroutine.  Hence the trailing
*   underscore in the name of the function and the pointer (reference)
*   argument list.
*
*   The wcsprm struct on which WCSLIB 3.x is based is passed as an integer
*   array of size WCSLEN at least (WCSLEN is defined in wcs.h).  While the
*   contents of this array are not interpretable in FORTRAN, it may be
*   constructed and interrogated by service routines (WCSPUT and WCSGET)
*   provided with the FORTRAN wrappers for WCSLIB 3.x.  The array is cast to
*   (struct wcsprm *) for use here and in WCSLIB.
*
*   Given:
*      opcode   int*     Transformation code:
*                            2: Compute a set of pixel coordinates which
*                               describe a path between this and the previous
*                               pair of world coordinates remembered from the
*                               last call with opcode == 1 || 2.
*                            1: Compute pixel coordinates from world
*                               coordinates.
*                            0: Initialize.
*                           -1: Compute world coordinates from pixel
*                               coordinates.
*
*      nlc      int*     Number of elements in nlcprm[] (not used).
*
*      nli      int*     Number of elements in wcs (at least WCSLEN).
*
*      nld      int*     Number of elements in nldprm (not used).
*
*      nlcprm   char[nlc]
*                        Character array (not used).
*
*   Given and/or returned:
*      wcs      int[nli] Integer array which contains the wcsprm struct (see
*                        below).
*
*      nldprm   double[nld]
*                        Double precision array (not used).
*
*      world    double[2]
*                        World coordinates.  world[0] and world[1] are the
*                        longitude and latitude, in degrees.  Given if
*                        opcode > 0, returned if opcode < 0.
*
*      pixel    double[2]
*                        Pixel coordinates.  Given if opcode < 0, returned if
*                        opcode > 0.
*
*      contrl   int*     Control flag for opcode == 2:
*                           0: Normal state
*                           1: A discontinuity has been encountered; force
*                              PGSBOX to flush its plotting buffer and call
*                              pgwcsl_() again with the same world
*                              coordinates.
*                           2: Call pgwcsl_() again with the same world
*                              coordinates.
*
*      contxt   double[20]
*                        Context elements for opcode == 2.
*
*   Returned:
*      ierr     int*     Error status
*                           0: Success.
*                           1: Invalid parameters.
*                           2: Invalid world coordinate.
*                           3: Invalid pixel coordinate.
*
*   Notes
*   -----
*    1) pgwcsl_() assumes a simple 2-D image.
*
*    2) The wcsprm struct (contained in the wcs[] array) is maintained by
*       pgwcsl_() and WCSLIB and must not be disturbed by the caller after
*       initialization with opcode == 0.
*
*    3) pgwcsl_() doesn't properly handle discontinuities between the faces
*       of the quadcube projections.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#include <math.h>

#include <wcs.h>
#include <sph.h>

void pgwcsl_(opcode, nlc, nli, nld, nlcprm, wcs, nldprm, world, pixel, contrl,
            contxt, ierr)

const int *opcode, *nlc, *nli, *nld;
const char *nlcprm;
int *wcs;
double *nldprm;
double world[2];
double pixel[2];
int *contrl;
double contxt[20];
int *ierr;

{
   int stat;
   double dp, imgcrd[2], lat, lng, ph, phi, sdummy, th, theta, wrld[2];
   struct wcsprm *wcsp;

   *ierr = 0;

   wcsp = (struct wcsprm *)wcs;

   if (*opcode == 2) {
      /* Compute pixel coordinates from world coordinates. */
      if (wcsp->lng < 0) {
         /* Simple linear coordinates. */
         if (*ierr = wcss2p(wcsp, 1, 0, world, &phi, &theta, imgcrd, pixel,
            &stat)) *ierr = 1;
         return;
      }

      wrld[wcsp->lng] = world[0];
      if (world[1] > 90.0) {
         wrld[wcsp->lat] = 90.0;
      } else if (world[1] < -90.0) {
         wrld[wcsp->lat] = -90.0;
      } else {
         wrld[wcsp->lat] = world[1];
      }

      if (*contrl == 0) {
         if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd, pixel,
            &stat)) {
            /* Translate error codes. */
            if (*ierr == 2) {
               *ierr = 1;
            } else if (*ierr == 3) {
               *ierr = 2;
            } else if (*ierr == 4) {
               *ierr = 1;
            }
            return;
         }

         if (fabs(phi-contxt[2]) > 180.0) {
            /* Hit a discontinuity at phi = +/- 180. */
            contxt[4] = pixel[0];
            contxt[5] = pixel[1];

            if (contxt[2] > phi) {
               ph = 179.9999;
               dp = (phi - contxt[2]) + 360.0;
            } else {
               ph = -179.9999;
               dp = (phi - contxt[2]) - 360.0;
            }

            /* First approximation for theta. */
            if (dp == 0.0) {
               th = contxt[3];
            } else {
               th = contxt[3] + (ph-contxt[2])*(theta-contxt[3])/dp;
            }

            /* Iterate once to refine the value of theta. */
            *ierr = sphx2s(wcsp->cel.euler, 1, 1, 1, 1, &ph, &th, &lng,
               &lat);
            if (wrld[wcsp->lng] == contxt[0]) {
               /* We are following a meridian of longitude. */
               lng = wrld[wcsp->lng];
            } else {
               /* We are following a parallel of latitude. */
               lat = wrld[wcsp->lat];
            }
            *ierr = sphs2x(wcsp->cel.euler, 1, 1, 1, 1, &lng, &lat, &sdummy,
                           &th);

            contxt[0] = wrld[wcsp->lng];
            contxt[1] = wrld[wcsp->lat];
            contxt[2] = phi;
            contxt[3] = theta;

            /* Pixel coordinates crossing into the discontinuity. */
            *ierr = sphx2s(wcsp->cel.euler, 1, 1, 1, 1, &ph, &th,
               wrld+wcsp->lng, wrld+wcsp->lat);
            if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd, pixel,
               &stat)) {
               /* Translate error codes. */
               if (*ierr == 2) {
                  *ierr = 1;
               } else if (*ierr == 3) {
                  *ierr = 2;
               } else if (*ierr == 4) {
                  *ierr = 1;
               }
               return;
            }

            /* Pixel coordinates crossing out of the discontinuity. */
            ph *= -1.0;
            *ierr = sphx2s(wcsp->cel.euler, 1, 1, 1, 1, &ph, &th,
               wrld+wcsp->lng, wrld+wcsp->lat);
            if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd,
               contxt+6, &stat)) {
               /* Translate error codes. */
               if (*ierr == 2) {
                  *ierr = 1;
               } else if (*ierr == 3) {
                  *ierr = 2;
               } else if (*ierr == 4) {
                  *ierr = 1;
               }
               return;
            }

            *contrl = 1;
         } else {
            /* Normal mode, no discontinuity. */
            contxt[0] = wrld[wcsp->lng];
            contxt[1] = wrld[wcsp->lat];
            contxt[2] = phi;
            contxt[3] = theta;
         }
      } else {
         if (*contrl == 1) {
            /* Move to the other side of the discontinuity. */
            pixel[0] = contxt[6];
            pixel[1] = contxt[7];
            *contrl = 2;
         } else {
            /* Complete the traversal. */
            pixel[0] = contxt[4];
            pixel[1] = contxt[5];
            *contrl = 0;
         }
      }

   } else if (*opcode == 1) {
      /* Compute pixel coordinates from world coordinates. */
      if (wcsp->lng < 0) {
         /* Simple linear coordinates. */
         if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd,
            pixel, &stat)) *ierr = 1;
         return;
      }

      wrld[wcsp->lng] = world[0];
      if (world[1] > 90.0) {
         wrld[wcsp->lat] = 90.0;
      } else if (world[1] < -90.0) {
         wrld[wcsp->lat] = -90.0;
      } else {
         wrld[wcsp->lat] = world[1];
      }

      if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd, pixel,
         &stat)) {
         /* Translate error codes. */
         if (*ierr == 2 || *ierr == 4) {
            *ierr = 1;
         } else if (*ierr == 3) {
            *ierr = 2;
         }
         return;
      }

      contxt[0] = wrld[wcsp->lng];
      contxt[1] = wrld[wcsp->lat];
      contxt[2] = phi;
      contxt[3] = theta;

   } else if (*opcode == 0) {
      /* Initialize. */
      if (*nli < WCSLEN) {
         *ierr = 1;
         return;
      }

      *ierr = wcsset(wcsp);

      *contrl = 0;

   } else if (*opcode == -1) {
      /* Compute world coordinates from pixel coordinates. */
      if (*ierr = wcsp2s(wcsp, 1, 0, pixel, imgcrd, &phi, &theta, wrld,
         &stat)) {
         /* Translate error codes. */
         if (*ierr == 2 || *ierr == 4) *ierr = 1;
         return;
      }

      if (wcsp->lng < 0) {
         /* Simple linear coordinates. */
         world[0] = wrld[0];
         world[1] = wrld[1];
      } else {
         world[0] = wrld[wcsp->lng];
         world[1] = wrld[wcsp->lat];

         if (phi < -180.0 || phi > 180.0) {
            /* Pixel is outside the principle range of native longitude. */
            *ierr = 3;
            return;
         }
      }

   } else {
      *ierr = 1;
   }

   return;
}
