/*============================================================================
*
*   PGSBOX 3.4 - a non-linear coordinate axis plotter for PGPLOT.
*   Copyright (C) 1997-2004, Mark Calabretta
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
   int outside, stat;
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
         outside = -2;
      } else if (world[1] < -90.0) {
         wrld[wcsp->lat] = -90.0;
         outside = -2;
      } else {
         wrld[wcsp->lat] = world[1];
         outside = 0;
      }

      if (*contrl == 0) {
         if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd, pixel,
            &stat)) {
            /* Translate error codes. */
            *ierr = stat ? 2 : 1;
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
               *ierr = stat ? 2 : 1;
               return;
            }

            /* Pixel coordinates crossing out of the discontinuity. */
            ph *= -1.0;
            *ierr = sphx2s(wcsp->cel.euler, 1, 1, 1, 1, &ph, &th,
               wrld+wcsp->lng, wrld+wcsp->lat);
            if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd,
               contxt+6, &stat)) {
               /* Translate error codes. */
               *ierr = stat ? 2 : 1;
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

      *ierr = outside;

   } else if (*opcode == 1) {
      /* Compute pixel coordinates from world coordinates. */
      if (wcsp->lng < 0) {
         /* Simple linear coordinates. */
         if (*ierr = wcss2p(wcsp, 1, 0, world, &phi, &theta, imgcrd,
            pixel, &stat)) *ierr = 1;
         return;
      }

      wrld[wcsp->lng] = world[0];
      if (world[1] > 90.0) {
         wrld[wcsp->lat] = 90.0;
         outside = -2;
      } else if (world[1] < -90.0) {
         wrld[wcsp->lat] = -90.0;
         outside = -2;
      } else {
         wrld[wcsp->lat] = world[1];
         outside = 0;
      }

      if (*ierr = wcss2p(wcsp, 1, 0, wrld, &phi, &theta, imgcrd, pixel,
         &stat)) {
         /* Translate error codes. */
         *ierr = stat ? 2 : 1;
         return;
      }

      contxt[0] = wrld[wcsp->lng];
      contxt[1] = wrld[wcsp->lat];
      contxt[2] = phi;
      contxt[3] = theta;

      *ierr = outside;

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
         *ierr = stat ? 3 : 1;
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
