/*============================================================================
*
*   WCSLIB 4.3 - an implementation of the FITS WCS standard.
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
*===========================================================================*/

#include <math.h>
#include "wcstrig.h"
#include "sph.h"

#define copysign(X, Y) ((Y) < 0.0 ? -fabs(X) : fabs(X))

#define tol 1.0e-5

/*--------------------------------------------------------------------------*/

int sphx2s(eul, nphi, ntheta, spt, sll, phi, theta, lng, lat)

const double eul[5];
int nphi, ntheta, spt, sll;
const double phi[], theta[];
double lng[], lat[];

{
   int mphi, mtheta, rowlen, rowoff;
   double cosphi, costhe, costhe3, costhe4, dlng, dphi, sinphi, sinthe,
          sinthe3, sinthe4, x, y, z;
   register int iphi, itheta;
   register const double *phip, *thetap;
   register double *latp, *lngp;

   if (ntheta > 0) {
      mphi   = nphi;
      mtheta = ntheta;
   } else {
      mphi   = 1;
      mtheta = 1;
      ntheta = nphi;
   }


   /* Do phi dependency. */
   phip = phi;
   rowoff = 0;
   rowlen = nphi*sll;
   for (iphi = 0; iphi < nphi; iphi++, rowoff += sll, phip += spt) {
      dphi = *phip - eul[2];

      lngp = lng + rowoff;
      for (itheta = 0; itheta < mtheta; itheta++) {
         *lngp = dphi;
         lngp += rowlen;
      }
   }


   /* Do theta dependency. */
   thetap = theta;
   lngp = lng;
   latp = lat;
   for (itheta = 0; itheta < ntheta; itheta++, thetap += spt) {
      costhe = cosd(*thetap);
      sinthe = sind(*thetap);
      costhe3 = costhe*eul[3];
      costhe4 = costhe*eul[4];
      sinthe3 = sinthe*eul[3];
      sinthe4 = sinthe*eul[4];

      for (iphi = 0; iphi < mphi; iphi++, lngp += sll, latp += sll) {
         dphi = *lngp;
         cosphi = cosd(dphi);
         sinphi = sind(dphi);

         /* Compute the celestial longitude. */
         x = sinthe4 - costhe3*cosphi;
         if (fabs(x) < tol) {
            /* Rearrange formula to reduce roundoff errors. */
            x = -cosd(*thetap+eul[1]) + costhe3*(1.0 - cosphi);
         }

         y = -costhe*sinphi;
         if (x != 0.0 || y != 0.0) {
            dlng = atan2d(y, x);
         } else {
            /* Change of origin of longitude. */
            if (eul[1] < 90.0) {
               dlng =  dphi + 180.0;
            } else {
               dlng = -dphi;
            }
         }
         *lngp = eul[0] + dlng;

         /* Normalize the celestial longitude. */
         if (eul[0] >= 0.0) {
            if (*lngp < 0.0) *lngp += 360.0;
         } else {
            if (*lngp > 0.0) *lngp -= 360.0;
         }

         if (*lngp > 360.0) {
            *lngp -= 360.0;
         } else if (*lngp < -360.0) {
            *lngp += 360.0;
         }

         /* Compute the celestial latitude. */
         if (fmod(dphi,180.0) == 0.0) {
            *latp = *thetap + cosphi*eul[1];
            if (*latp >  90.0) *latp =  180.0 - *latp;
            if (*latp < -90.0) *latp = -180.0 - *latp;
         } else {
            z = sinthe3 + costhe4*cosphi;
            if (fabs(z) > 0.99) {
               /* Use an alternative formula for greater accuracy. */
               *latp = copysign(acosd(sqrt(x*x+y*y)), z);
            } else {
               *latp = asind(z);
            }
         }
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int sphs2x(eul, nlng, nlat, sll, spt, lng, lat, phi, theta)

const double eul[5];
int nlat, nlng, sll, spt;
const double lat[], lng[];
double phi[], theta[];

{
   int mlat, mlng, rowlen, rowoff;
   double coslat, coslat3, coslat4, coslng, dlng, dphi, sinlat, sinlat3,
          sinlat4, sinlng, x, y, z;
   register int ilat, ilng;
   register const double *latp, *lngp;
   register double *phip, *thetap;

   if (nlat > 0) {
      mlng = nlng;
      mlat = nlat;
   } else {
      mlng = 1;
      mlat = 1;
      nlat = nlng;
   }


   /* Do lng dependency. */
   lngp = lng;
   rowoff = 0;
   rowlen = nlng*spt;
   for (ilng = 0; ilng < nlng; ilng++, rowoff += spt, lngp += sll) {
      dlng = *lngp - eul[0];

      phip = phi + rowoff;
      for (ilat = 0; ilat < mlat; ilat++) {
         *phip = dlng;
         phip += rowlen;
      }
   }


   /* Do lat dependency. */
   latp = lat;
   phip   = phi;
   thetap = theta;
   for (ilat = 0; ilat < nlat; ilat++, latp += sll) {
      coslat  = cosd(*latp);
      sinlat  = sind(*latp);
      coslat3 = coslat*eul[3];
      coslat4 = coslat*eul[4];
      sinlat3 = sinlat*eul[3];
      sinlat4 = sinlat*eul[4];

      for (ilng = 0; ilng < mlng; ilng++, phip += spt, thetap += spt) {
         dlng = *phip;
         coslng = cosd(dlng);
         sinlng = sind(dlng);

         /* Compute the native longitude. */
         x = sinlat4 - coslat3*coslng;
         if (fabs(x) < tol) {
            /* Rearrange formula to reduce roundoff errors. */
            x = -cosd(*latp+eul[1]) + coslat3*(1.0 - coslng);
         }

         y = -coslat*sinlng;
         if (x != 0.0 || y != 0.0) {
            dphi = atan2d(y, x);
         } else {
            /* Change of origin of longitude. */
            if (eul[1] < 90.0) {
               dphi =  dlng - 180.0;
            } else {
               dphi = -dlng;
            }
         }
         *phip = fmod(eul[2] + dphi, 360.0);

         /* Normalize the native longitude. */
         if (*phip > 180.0) {
            *phip -= 360.0;
         } else if (*phip < -180.0) {
            *phip += 360.0;
         }

         /* Compute the native latitude. */
         if (fmod(dlng,180.0) == 0.0) {
            *thetap = *latp + coslng*eul[1];
            if (*thetap >  90.0) *thetap =  180.0 - *thetap;
            if (*thetap < -90.0) *thetap = -180.0 - *thetap;
         } else {
            z = sinlat3 + coslat4*coslng;
            if (fabs(z) > 0.99) {
               /* Use an alternative formula for greater accuracy. */
               *thetap = copysign(acosd(sqrt(x*x+y*y)), z);
            } else {
               *thetap = asind(z);
            }
         }
      }
   }

   return 0;
}
