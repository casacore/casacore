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
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#include <math.h>
#include <stdio.h>

#include "wcstrig.h"
#include "sph.h"
#include "cel.h"

const int CELSET = 137;

/* Map error number to error message for each function. */
const char *cel_errmsg[] = {
   0,
   "Null celprm pointer passed",
   "Invalid projection parameters",
   "Invalid coordinate transformation parameters",
   "Ill-conditioned coordinate transformation parameters",
   "One or more of the (x,y) coordinates were invalid",
   "One or more of the (lng,lat) coordinates were invalid"};

#define UNDEFINED 987654321.0e99
#define undefined(value) (value == UNDEFINED)

/*--------------------------------------------------------------------------*/

int celini(cel)

struct celprm *cel;

{
   register int k;

   if (cel == 0) return 1;

   cel->flag = 0;

   cel->offset = 0;
   cel->phi0   = UNDEFINED;
   cel->theta0 = UNDEFINED;
   cel->ref[0] =   0.0;
   cel->ref[1] =   0.0;
   cel->ref[2] = 999.0;
   cel->ref[3] = 999.0;

   for (k = 0; k < 5; cel->euler[k++] = 0.0);

   return prjini(&(cel->prj));
}

/*--------------------------------------------------------------------------*/

int celprt(cel)

const struct celprm *cel;

{
   int i;

   if (cel == 0) return 1;

   printf("       flag: %d\n",  cel->flag);
   printf("     offset: %d\n",  cel->offset);
   if (undefined(cel->phi0)) {
      printf("       phi0: UNDEFINED\n");
   } else {
      printf("       phi0: %9f\n", cel->phi0);
   }
   if (undefined(cel->theta0)) {
      printf("     theta0: UNDEFINED\n");
   } else {
      printf("     theta0: %9f\n", cel->theta0);
   }
   printf("        ref:");
   for (i = 0; i < 4; i++) {
      printf("  %- 11.4g", cel->ref[i]);
   }
   printf("\n");
   printf("        prj: (see below)\n");
   printf("      euler:");
   for (i = 0; i < 5; i++) {
      printf("  %- 11.4g", cel->euler[i]);
   }
   printf("\n");
   printf("     isolat: %d\n", cel->isolat);

   printf("\n");
   printf("   prj.*\n");
   prjprt(&(cel->prj));

   return 0;
}

/*--------------------------------------------------------------------------*/

int celset(cel)

struct celprm *cel;

{
   int status;
   const double tol = 1.0e-10;
   double clat0, cphip, cthe0, lat0, lng0, phip, slat0, sphip, sthe0;
   double latp, latp1, latp2, lngp;
   double u, v, x, y, z;
   struct prjprm *celprj;

   if (cel == 0) return 1;

   /* Initialize the projection driver routines. */
   celprj = &(cel->prj);
   if (cel->offset) {
      celprj->phi0   = cel->phi0;
      celprj->theta0 = cel->theta0;
   } else {
      /* Ensure that these are undefined - no fiducial offset. */
      celprj->phi0   = UNDEFINED;
      celprj->theta0 = UNDEFINED;
   }

   if (status = prjset(celprj)) {
      return status;
   }

   /* Defaults set by the projection routines. */
   if (undefined(cel->phi0)) {
      cel->phi0 = celprj->phi0;
   }

   if (undefined(cel->theta0)) {
      cel->theta0 = celprj->theta0;

   } else if (fabs(cel->theta0) > 90.0) {
     if (fabs(cel->theta0) > 90.0 + tol) {
        return 3;
     }

     if (cel->theta0 > 90.0) {
        cel->theta0 =  90.0;
     } else {
        cel->theta0 = -90.0;
     }
   }


   lng0 = cel->ref[0];
   lat0 = cel->ref[1];
   phip = cel->ref[2];
   latp = cel->ref[3];

   /* Set default for native longitude of the celestial pole? */
   if (phip == 999.0) {
      phip = (lat0 < cel->theta0) ? 180.0 : 0.0;
      phip += cel->phi0;

      if (phip < -180.0) {
         phip += 360.0;
      } else if (phip > 180.0) {
         phip -= 360.0;
      }

      cel->ref[2] = phip;
   }


   /* Compute celestial coordinates of the native pole. */
   if (cel->theta0 == 90.0) {
      /* Fiducial point at the native pole. */
      lngp = lng0;
      latp = lat0;

   } else {
      /* Fiducial point away from the native pole. */
      clat0 = cosd(lat0);
      slat0 = sind(lat0);
      cphip = cosd(phip - cel->phi0);
      sphip = sind(phip - cel->phi0);
      cthe0 = cosd(cel->theta0);
      sthe0 = sind(cel->theta0);

      x = cthe0*cphip;
      y = sthe0;
      z = sqrt(x*x + y*y);
      if (z == 0.0) {
         if (slat0 != 0.0) {
            return 3;
         }

         /* latp determined by LATPOLE in this case. */
         if (latp > 90.0) {
            latp = 90.0;
         } else if (latp < -90.0) {
            latp = -90.0;
         }

      } else {
         if (fabs(slat0/z) > 1.0) {
            return 3;
         }

         u = atan2d(y,x);
         v = acosd(slat0/z);

         latp1 = u + v;
         if (latp1 > 180.0) {
            latp1 -= 360.0;
         } else if (latp1 < -180.0) {
            latp1 += 360.0;
         }

         latp2 = u - v;
         if (latp2 > 180.0) {
            latp2 -= 360.0;
         } else if (latp2 < -180.0) {
            latp2 += 360.0;
         }

         if (fabs(latp-latp1) < fabs(latp-latp2)) {
            if (fabs(latp1) < 90.0+tol) {
               latp = latp1;
            } else {
               latp = latp2;
            }
         } else {
            if (fabs(latp2) < 90.0+tol) {
               latp = latp2;
            } else {
               latp = latp1;
            }
         }

         /* Account for rounding error. */
         if (fabs(latp) < 90.0+tol) {
            if (latp > 90.0) {
               latp =  90.0;
            } else if (latp < -90.0) {
               latp = -90.0;
            }
         }
      }

      z = cosd(latp)*clat0;
      if (fabs(z) < tol) {
         if (fabs(clat0) < tol) {
            /* Celestial pole at the fiducial point. */
            lngp = lng0;

         } else if (latp > 0.0) {
            /* Celestial north pole at the native pole.*/
            lngp = lng0 + phip - cel->phi0 - 180.0;

         } else {
            /* Celestial south pole at the native pole. */
            lngp = lng0 - phip + cel->phi0;
         }

      } else {
         x = (sthe0 - sind(latp)*slat0)/z;
         y =  sphip*cthe0/clat0;
         if (x == 0.0 && y == 0.0) {
            return 3;
         }
         lngp = lng0 - atan2d(y,x);
      }

      /* Make celestial longitude at the pole the same sign as at the
         fiducial point. */
      if (lng0 >= 0.0) {
         if (lngp < 0.0) {
            lngp += 360.0;
         } else if (lngp > 360.0) {
            lngp -= 360.0;
         }
      } else {
         if (lngp > 0.0) {
            lngp -= 360.0;
         } else if (lngp < -360.0) {
            lngp += 360.0;
         }
      }
   }

   /* Reset LATPOLE. */
   cel->ref[3] = latp;

   /* Set the Euler angles. */
   cel->euler[0] = lngp;
   cel->euler[1] = 90.0 - latp;
   cel->euler[2] = phip;
   cel->euler[3] = cosd(cel->euler[1]);
   cel->euler[4] = sind(cel->euler[1]);
   cel->isolat = (cel->euler[4] == 0.0);
   cel->flag = CELSET;

   /* Check for ill-conditioned parameters. */
   if (fabs(latp) > 90.0+tol) {
      return 4;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int celx2s(cel, nx, ny, sxy, sll, x, y, phi, theta, lng, lat, stat)

struct celprm *cel;
int nx, ny, sxy, sll;
const double x[], y[];
double phi[], theta[];
double lng[], lat[];
int stat[];

{
   int    nphi, status;
   struct prjprm *celprj;


   /* Initialize. */
   if (cel == 0) return 1;
   if (cel->flag != CELSET) {
      if (status = celset(cel)) return status;
   }

   /* Apply spherical deprojection. */
   celprj = &(cel->prj);
   if (status = celprj->prjx2s(celprj, nx, ny, sxy, 1, x, y, phi, theta,
                               stat)) {
      if (status == 3) {
         status = 5;
      } else {
         return status;
      }
   }

   nphi = (ny > 0) ? (nx*ny) : nx;

   /* Compute celestial coordinates. */
   sphx2s(cel->euler, nphi, 0, 1, sll, phi, theta, lng, lat);

   return status;
}

/*--------------------------------------------------------------------------*/

int cels2x(cel, nlng, nlat, sll, sxy, lng, lat, phi, theta, x, y, stat)

struct celprm *cel;
int nlng, nlat, sll, sxy;
const double lng[], lat[];
double phi[], theta[];
double x[], y[];
int stat[];

{
   int    nphi, ntheta, status;
   struct prjprm *celprj;


   /* Initialize. */
   if (cel == 0) return 1;
   if (cel->flag != CELSET) {
      if (status = celset(cel)) return status;
   }

   /* Compute native coordinates. */
   sphs2x(cel->euler, nlng, nlat, sll, 1, lng, lat, phi, theta);

   if (cel->isolat) {
      /* Constant celestial latitude -> constant native latitude. */
      nphi   = nlng;
      ntheta = nlat;
   } else {
      nphi   = (nlat > 0) ? (nlng*nlat) : nlng;
      ntheta = 0;
   }

   /* Apply the spherical projection. */
   celprj = &(cel->prj);
   if (status = celprj->prjs2x(celprj, nphi, ntheta, 1, sxy, phi, theta, x, y,
                               stat)) {
      return status == 2 ? 2 : 6;
   }

   return 0;
}
