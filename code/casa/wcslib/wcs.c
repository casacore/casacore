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
*===========================================================================*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wcsmath.h"
#include "wcstrig.h"
#include "sph.h"
#include "wcs.h"

const int WCSSET = 137;

/* Maximum number of PVi_ma and PSi_ma cards. */
int NPVMAX = 64;
int NPSMAX =  8;

/* Map status return value to message. */
const char *wcs_errmsg[] = {
   0,
   "Null wcsprm pointer passed",
   "Memory allocation failed",
   "Linear transformation matrix is singular",
   "Inconsistent or unrecognized coordinate axis types",
   "Invalid parameter value",
   "Invalid coordinate transformation parameters",
   "Ill-conditioned coordinate transformation parameters",
   "One or more of the pixel coordinates were invalid",
   "One or more of the world coordinates were invalid",
   "Invalid world coordinate",
   "No solution found in the specified interval",
   "Invalid subimage specification",
   "Non-separable subimage coordinate system"};

#define signbit(X) ((X) < 0.0 ? 1 : 0)

/*--------------------------------------------------------------------------*/

int wcsnpv(int npvmax) { if (npvmax >= 0) NPVMAX = npvmax; return NPVMAX; }
int wcsnps(int npsmax) { if (npsmax >= 0) NPSMAX = npsmax; return NPSMAX; }

/*--------------------------------------------------------------------------*/

int wcsini(int alloc, int naxis, struct wcsprm *wcs)

{
   int i, j, k, status;
   double *cd;
   size_t elsize;

   if (wcs == 0) return 1;

   if (naxis <= 0) {
      return 2;
   }

   /* Initialize memory management. */
   if (wcs->flag == -1 || wcs->m_flag != WCSSET) {
      wcs->m_flag  = 0;
      wcs->m_naxis = 0;
      wcs->m_crpix = 0;
      wcs->m_pc    = 0;
      wcs->m_cdelt = 0;
      wcs->m_cunit = 0;
      wcs->m_ctype = 0;
      wcs->m_crval = 0;
      wcs->m_pv    = 0;
      wcs->m_ps    = 0;
      wcs->m_cd    = 0;
      wcs->m_crota = 0;
      wcs->m_cname = 0;
      wcs->m_crder = 0;
      wcs->m_csyer = 0;
   }

   if (wcs->flag == -1) {
      wcs->lin.flag = -1;
   }


   /* Allocate memory for arrays if required. */
   if (alloc ||
       wcs->crpix == 0 ||
       wcs->pc    == 0 ||
       wcs->cdelt == 0 ||
       wcs->cunit == 0 ||
       wcs->ctype == 0 ||
       wcs->crval == 0 ||
       (NPVMAX && wcs->pv == 0) ||
       (NPSMAX && wcs->ps == 0) ||
       wcs->cd    == 0 ||
       wcs->crota == 0 ||
       wcs->cname == 0 ||
       wcs->crder == 0 ||
       wcs->csyer == 0) {

      /* Was sufficient allocated previously? */
      if (wcs->m_flag == WCSSET &&
         (wcs->m_naxis < naxis  ||
          wcs->npvmax  < NPVMAX ||
          wcs->npsmax  < NPSMAX)) {
         /* No, free it. */
         wcsfree(wcs);
      }


      if (alloc || wcs->crpix == 0) {
         if (wcs->m_crpix) {
            /* In case the caller fiddled with it. */
            wcs->crpix = wcs->m_crpix;

         } else {
            if (!(wcs->crpix = calloc(naxis, sizeof(double)))) {
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_crpix = wcs->crpix;
         }
      }

      if (alloc || wcs->pc == 0) {
         if (wcs->m_pc) {
            /* In case the caller fiddled with it. */
            wcs->pc = wcs->m_pc;

         } else {
            if (!(wcs->pc = calloc(naxis*naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_pc    = wcs->pc;
         }
      }

      if (alloc || wcs->cdelt == 0) {
         if (wcs->m_cdelt) {
            /* In case the caller fiddled with it. */
            wcs->cdelt = wcs->m_cdelt;

         } else {
            if (!(wcs->cdelt = calloc(naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_cdelt = wcs->cdelt;
         }
      }

      if (alloc || wcs->cunit == 0) {
         if (wcs->m_cunit) {
            /* In case the caller fiddled with it. */
            wcs->cunit = wcs->m_cunit;

         } else {
            elsize = sizeof(char [72]);
            if (!(wcs->cunit = (char (*)[72])calloc(naxis, elsize))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_cunit = wcs->cunit;
         }
      }

      if (alloc || wcs->ctype == 0) {
         if (wcs->m_ctype) {
            /* In case the caller fiddled with it. */
            wcs->ctype = wcs->m_ctype;

         } else {
            elsize = sizeof(char [72]);
            if (!(wcs->ctype = (char (*)[72])calloc(naxis, elsize))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_ctype = wcs->ctype;
         }
      }

      if (alloc || wcs->crval == 0) {
         if (wcs->m_crval) {
            /* In case the caller fiddled with it. */
            wcs->crval = wcs->m_crval;

         } else {
            if (!(wcs->crval = calloc(naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_crval = wcs->crval;
         }
      }

      if (alloc || wcs->pv == 0) {
         if (wcs->m_pv) {
            /* In case the caller fiddled with it. */
            wcs->pv = wcs->m_pv;

         } else {
            if (NPVMAX) {
              elsize = sizeof(struct pvcard);
              if (!(wcs->pv = (struct pvcard *)calloc(NPVMAX, elsize))) {
                 wcsfree(wcs);
                 return 2;
              }
            } else {
              wcs->pv = (struct pvcard *)0;
            }

            wcs->npvmax  = NPVMAX;

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_pv    = wcs->pv;
         }
      }

      if (alloc || wcs->ps == 0) {
         if (wcs->m_ps) {
            /* In case the caller fiddled with it. */
            wcs->ps = wcs->m_ps;

         } else {
            if (NPSMAX) {
              elsize = sizeof(struct pscard);
              if (!(wcs->ps = (struct pscard *)calloc(NPSMAX, elsize))) {
                 wcsfree(wcs);
                 return 2;
              }
            } else {
              wcs->ps = (struct pscard *)0;
            }

            wcs->npsmax  = NPSMAX;

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_ps    = wcs->ps;
         }
      }

      if (alloc || wcs->cd == 0) {
         if (wcs->m_cd) {
            /* In case the caller fiddled with it. */
            wcs->cd = wcs->m_cd;

         } else {
            if (!(wcs->cd = calloc(naxis*naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_cd    = wcs->cd;
         }
      }

      if (alloc || wcs->crota == 0) {
         if (wcs->m_crota) {
            /* In case the caller fiddled with it. */
            wcs->crota = wcs->m_crota;

         } else {
            if (!(wcs->crota = calloc(naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_crota = wcs->crota;
         }
      }

      if (alloc || wcs->cname == 0) {
         if (wcs->m_cname) {
            /* In case the caller fiddled with it. */
            wcs->cname = wcs->m_cname;

         } else {
            elsize = sizeof(char [72]);
            if (!(wcs->cname = (char (*)[72])calloc(naxis, elsize))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_cname = wcs->cname;
         }
      }

      if (alloc || wcs->crder == 0) {
         if (wcs->m_crder) {
            /* In case the caller fiddled with it. */
            wcs->crder = wcs->m_crder;

         } else {
            if (!(wcs->crder = calloc(naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_crder = wcs->crder;
         }
      }

      if (alloc || wcs->csyer == 0) {
         if (wcs->m_csyer) {
            /* In case the caller fiddled with it. */
            wcs->csyer = wcs->m_csyer;

         } else {
            if (!(wcs->csyer = calloc(naxis, sizeof(double)))) {
               wcsfree(wcs);
               return 2;
            }

            wcs->m_flag  = WCSSET;
            wcs->m_naxis = naxis;
            wcs->m_csyer = wcs->csyer;
         }
      }
   }


   wcs->flag  = 0;
   wcs->naxis = naxis;


   /* Set defaults for the linear transformation. */
   wcs->lin.crpix  = wcs->crpix;
   wcs->lin.pc     = wcs->pc;
   wcs->lin.cdelt  = wcs->cdelt;
   wcs->lin.m_flag = 0;
   if (status = linini(0, naxis, &(wcs->lin))) {
      return status;
   }


   /* CUNITia and CTYPEia are blank by default. */
   for (i = 0; i < naxis; i++) {
      memset(wcs->cunit[i], 0, 72);
      memset(wcs->ctype[i], 0, 72);
   }


   /* CRVALia defaults to 0.0. */
   for (i = 0; i < naxis; i++) {
      wcs->crval[i] = 0.0;
   }


   /* Set defaults for the celestial transformation parameters. */
   wcs->lonpole = 999.0;
   wcs->latpole = 999.0;

   /* Set defaults for the spectral transformation parameters. */
   wcs->restfrq = 0.0;
   wcs->restwav = 0.0;

   /* Default parameter values. */
   wcs->npv = 0;
   for (k = 0; k < wcs->npvmax; k++) {
     wcs->pv[k].i = 0;
     wcs->pv[k].m = 0;
     wcs->pv[k].value = 0.0;
   }

   wcs->nps = 0;
   for (k = 0; k < wcs->npsmax; k++) {
     wcs->ps[k].i = 0;
     wcs->ps[k].m = 0;
     memset(wcs->ps[k].value, 0, 72);
   }

   /* Defaults for alternate linear transformations. */
   wcs->altlin = 0;
   cd = wcs->cd;
   for (i = 0; i < naxis; i++) {
      for (j = 0; j < naxis; j++) {
         *(cd++) = 0.0;
      }
   }
   for (i = 0; i < naxis; i++) {
      wcs->crota[i] = 0.0;
   }

   /* Defaults for auxiliary coordinate system information. */
   wcs->alt[0] = ' ';
   wcs->colnum = 0;
   memset(wcs->wcsname, 0, 72);
   for (i = 0; i < naxis; i++) {
      memset(wcs->cname[i], 0, 72);
      wcs->crder[i] = UNDEFINED;
      wcs->csyer[i] = UNDEFINED;
   }
   memset(wcs->radesys, 0, 72);
   wcs->equinox    = UNDEFINED;
   memset(wcs->specsys, 0, 72);
   memset(wcs->ssysobs, 0, 72);
   wcs->velosys    = UNDEFINED;
   memset(wcs->ssyssrc, 0, 72);
   wcs->zsource    = UNDEFINED;
   wcs->obsgeo[0]  = UNDEFINED;
   wcs->obsgeo[1]  = UNDEFINED;
   wcs->obsgeo[2]  = UNDEFINED;
   memset(wcs->dateobs, 0, 72);
   wcs->mjdobs     = UNDEFINED;
   wcs->mjdavg     = UNDEFINED;

   /* Reset derived values. */
   strcpy(wcs->lngtyp, "    ");
   strcpy(wcs->lattyp, "    ");
   wcs->lng  = -1;
   wcs->lat  = -1;
   wcs->spec = -1;
   wcs->cubeface = -1;

   celini(&(wcs->cel));
   spcini(&(wcs->spc));

   wcs->types = 0;
   wcs->tab = 0;

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcssub(
   int alloc,
   const struct wcsprm *wcssrc,
   int *nsub,
   int axes[],
   struct wcsprm *wcsdst)

{
   char *c, ctypei[16];
   int  axis, cubeface, dealloc, dummy, i, j, k, latitude, longitude,
        *map = 0, msub, naxis, npv, nps, other, spectral, status, stokes;
   const double *srcp;
   double *dstp;

   if (wcssrc == 0) return 1;

   if ((naxis = wcssrc->naxis) <= 0) {
      return 2;
   }

   if (!(map = calloc(naxis, sizeof(int)))) {
      return 2;
   }

   if (nsub == 0) {
      nsub = &dummy;
      *nsub = naxis;
   } else if (*nsub == 0) {
      *nsub = naxis;
   }

   if (dealloc = (axes == 0)) {
      /* Construct an index array. */
      if (!(axes = calloc(naxis, sizeof(int)))) {
         return 2;
      }

      for (i = 0; i < naxis; i++) {
         axes[i] = i+1;
      }
   }


   msub = 0;
   for (j = 0; j < *nsub; j++) {
      axis = axes[j];

      if (abs(axis) > 0x1000) {
         /* Subimage extraction by type. */
         k = abs(axis) & 0xFF;

         longitude = k & WCSSUB_LONGITUDE;
         latitude  = k & WCSSUB_LATITUDE;
         cubeface  = k & WCSSUB_CUBEFACE;
         spectral  = k & WCSSUB_SPECTRAL;
         stokes    = k & WCSSUB_STOKES;

         if (other = (axis < 0)) {
            longitude = !longitude;
            latitude  = !latitude;
            cubeface  = !cubeface;
            spectral  = !spectral;
            stokes    = !stokes;
         }

         for (i = 0; i < naxis; i++) {
            strncpy (ctypei, (char *)(wcssrc->ctype + i), 8);
            ctypei[8] = '\0';

            /* Find the last non-blank character. */
            c = ctypei + 8;
            while (c-- > ctypei) {
               if (*c == ' ') *c = '\0';
               if (*c != '\0') break;
            }

            if (
               strcmp(ctypei,   "RA")  == 0 ||
               strcmp(ctypei+1, "LON") == 0 ||
               strcmp(ctypei+2, "LN")  == 0 ||
               strncmp(ctypei,   "RA---", 5) == 0 ||
               strncmp(ctypei+1, "LON-", 4) == 0 ||
               strncmp(ctypei+2, "LN-", 3) == 0) {
               if (!longitude) {
                  continue;
               }

            } else if (
               strcmp(ctypei,   "DEC") == 0 ||
               strcmp(ctypei+1, "LAT") == 0 ||
               strcmp(ctypei+2, "LT")  == 0 ||
               strncmp(ctypei,   "DEC--", 5) == 0 ||
               strncmp(ctypei+1, "LAT-", 4) == 0 ||
               strncmp(ctypei+2, "LT-", 3) == 0) {
               if (!latitude) {
                  continue;
               }

            } else if (strcmp(ctypei, "CUBEFACE") == 0) {
               if (!cubeface) {
                  continue;
               }

            } else if ((
               strncmp(ctypei, "FREQ", 4) == 0 ||
               strncmp(ctypei, "ENER", 4) == 0 ||
               strncmp(ctypei, "WAVN", 4) == 0 ||
               strncmp(ctypei, "VRAD", 4) == 0 ||
               strncmp(ctypei, "WAVE", 4) == 0 ||
               strncmp(ctypei, "VOPT", 4) == 0 ||
               strncmp(ctypei, "ZOPT", 4) == 0 ||
               strncmp(ctypei, "AWAV", 4) == 0 ||
               strncmp(ctypei, "VELO", 4) == 0 ||
               strncmp(ctypei, "BETA", 4) == 0) &&
               (ctypei[4] == '\0' || ctypei[4] == '-')) {
               if (!spectral) {
                  continue;
               }

            } else if (strcmp(ctypei, "STOKES") == 0) {
               if (!stokes) {
                  continue;
               }

            } else if (!other) {
               continue;
            }

            /* This axis is wanted, but has it already been added? */
            for (k = 0; k < msub; k++) {
               if (map[k] == i+1) {
                  break;
               }
            }
            if (k == msub) map[msub++] = i+1;
         }

      } else if (0 < axis && axis <= naxis) {
         /* Check that the requested axis has not already been added. */
         for (k = 0; k < msub; k++) {
            if (map[k] == axis) {
               break;
            }
         }
         if (k == msub) map[msub++] = axis;

      } else {
         status = 12;
         goto dealloc;
      }
   }

   if ((*nsub = msub) == 0) {
      status = 0;
      goto dealloc;
   }

   for (i = 0; i < *nsub; i++) {
      axes[i] = map[i];
   }


   /* Construct the inverse axis map. */
   for (i = 0; i < naxis; i++) {
      map[i] = 0;
   }

   for (i = 0; i < *nsub; i++) {
      map[axes[i]-1] = i+1;
   }

   /* Check that the subimage coordinate system is separable. */
   if (*nsub < naxis) {
      srcp = wcssrc->pc;
      for (i = 0; i < naxis; i++) {
         for (j = 0; j < naxis; j++) {
            if (*(srcp++) == 0.0 || j == i) continue;

            if ((map[i] == 0) != (map[j] == 0)) {
               status = 13;
               goto dealloc;
            }
         }
      }
   }


   /* Initialize the destination. */
   npv = NPVMAX;
   nps = NPSMAX;

   NPVMAX = 0;
   for (k = 0; k < wcssrc->npv; k++) {
      i = wcssrc->pv[k].i;
      if (i == 0 || (i > 0 && map[i-1])) {
         NPVMAX++;
      }
   }

   NPSMAX = 0;
   for (k = 0; k < wcssrc->nps; k++) {
      i = wcssrc->ps[k].i;
      if (i > 0 && map[i-1]) {
         NPSMAX++;
      }
   }

   status = wcsini(alloc, *nsub, wcsdst);

   NPVMAX = npv;
   NPSMAX = nps;

   if (status) {
      goto dealloc;
   }


   /* Linear transformation. */
   srcp = wcssrc->crpix;
   dstp = wcsdst->crpix;
   for (j = 0; j < *nsub; j++) {
      k = axes[j] - 1;
      *(dstp++) = *(srcp+k);
   }

   srcp = wcssrc->pc;
   dstp = wcsdst->pc;
   for (i = 0; i < *nsub; i++) {
      for (j = 0; j < *nsub; j++) {
         k = (axes[i]-1)*naxis + (axes[j]-1);
         *(dstp++) = *(srcp+k);
      }
   }

   srcp = wcssrc->cdelt;
   dstp = wcsdst->cdelt;
   for (i = 0; i < *nsub; i++) {
      k = axes[i] - 1;
      *(dstp++) = *(srcp+k);
   }

   /* Coordinate units and type. */
   for (i = 0; i < *nsub; i++) {
      k = axes[i] - 1;
      strncpy(wcsdst->cunit[i], wcssrc->cunit[k], 72);
      strncpy(wcsdst->ctype[i], wcssrc->ctype[k], 72);
   }

   /* Coordinate reference value. */
   srcp = wcssrc->crval;
   dstp = wcsdst->crval;
   for (i = 0; i < *nsub; i++) {
      k = axes[i] - 1;
      *(dstp++) = *(srcp+k);
   }

   /* Celestial and spectral transformation parameters. */
   wcsdst->lonpole = wcssrc->lonpole;
   wcsdst->latpole = wcssrc->latpole;
   wcsdst->restfrq = wcssrc->restfrq;
   wcsdst->restwav = wcssrc->restwav;

   /* Parameter values. */
   npv = 0;
   for (k = 0; k < wcssrc->npv; k++) {
      i = wcssrc->pv[k].i;
      if (i == 0 || (i > 0 && (i = map[i-1]))) {
         /* i == 0 is a special code for the latitude axis. */
         wcsdst->pv[npv] = wcssrc->pv[k];
         wcsdst->pv[npv].i = i;
         npv++;
      }
   }
   wcsdst->npv = npv;

   nps = 0;
   for (k = 0; k < wcssrc->nps; k++) {
      i = wcssrc->ps[k].i;
      if (i > 0 && (i = map[i-1])) {
         wcsdst->ps[nps] = wcssrc->ps[k];
         wcsdst->ps[nps].i = i;
         nps++;
      }
   }
   wcsdst->nps = nps;

   /* Alternate linear transformations. */
   wcsdst->altlin = wcssrc->altlin;

   srcp = wcssrc->cd;
   dstp = wcsdst->cd;
   for (i = 0; i < *nsub; i++) {
      for (j = 0; j < *nsub; j++) {
         k = (axes[i]-1)*naxis + (axes[j]-1);
         *(dstp++) = *(srcp+k);
      }
   }

   srcp = wcssrc->crota;
   dstp = wcsdst->crota;
   for (i = 0; i < *nsub; i++) {
      k = axes[i] - 1;
      *(dstp++) = *(srcp+k);
   }

   /* Auxiliary coordinate system information. */
   strncpy(wcsdst->alt, wcssrc->alt, 4);
   wcsdst->colnum = wcssrc->colnum;

   strncpy(wcsdst->wcsname, wcssrc->wcsname, 72);
   for (i = 0; i < *nsub; i++) {
      k = axes[i] - 1;
      strncpy(wcsdst->cname[i], wcssrc->cname[k], 72);
      wcsdst->crder[i] = wcssrc->crder[k];
      wcsdst->csyer[i] = wcssrc->csyer[k];
   }

   strncpy(wcsdst->radesys, wcssrc->radesys, 72);
   wcsdst->equinox = wcssrc->equinox;

   strncpy(wcsdst->specsys, wcssrc->specsys, 72);
   strncpy(wcsdst->ssysobs, wcssrc->ssysobs, 72);
   wcsdst->velosys = wcssrc->velosys;
   strncpy(wcsdst->ssyssrc, wcssrc->ssyssrc, 72);
   wcsdst->zsource = wcssrc->zsource;

   wcsdst->obsgeo[0] = wcssrc->obsgeo[0];
   wcsdst->obsgeo[1] = wcssrc->obsgeo[1];
   wcsdst->obsgeo[2] = wcssrc->obsgeo[2];

   strncpy(wcsdst->dateobs, wcssrc->dateobs, 72);
   wcsdst->mjdobs = wcssrc->mjdobs;
   wcsdst->mjdavg = wcssrc->mjdavg;


dealloc:
   if (map) free(map);
   if (dealloc) {
     free(axes);
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int wcsfree(struct wcsprm *wcs)

{
   if (wcs == 0) return 1;

   if (wcs->flag == -1) {
      wcs->lin.flag = -1;

   } else {
      /* Free memory allocated by wcsini(). */
      if (wcs->m_flag == WCSSET) {
         if (wcs->crpix == wcs->m_crpix) wcs->crpix = 0;
         if (wcs->pc    == wcs->m_pc)    wcs->pc    = 0;
         if (wcs->cdelt == wcs->m_cdelt) wcs->cdelt = 0;
         if (wcs->cunit == wcs->m_cunit) wcs->cunit = 0;
         if (wcs->ctype == wcs->m_ctype) wcs->ctype = 0;
         if (wcs->crval == wcs->m_crval) wcs->crval = 0;
         if (wcs->pv    == wcs->m_pv)    wcs->pv    = 0;
         if (wcs->ps    == wcs->m_ps)    wcs->ps    = 0;
         if (wcs->cd    == wcs->m_cd)    wcs->cd    = 0;
         if (wcs->crota == wcs->m_crota) wcs->crota = 0;
         if (wcs->cname == wcs->m_cname) wcs->cname = 0;
         if (wcs->crder == wcs->m_crder) wcs->crder = 0;
         if (wcs->csyer == wcs->m_csyer) wcs->csyer = 0;

         if (wcs->m_crpix) free(wcs->m_crpix);
         if (wcs->m_pc)    free(wcs->m_pc);
         if (wcs->m_cdelt) free(wcs->m_cdelt);
         if (wcs->m_cunit) free(wcs->m_cunit);
         if (wcs->m_ctype) free(wcs->m_ctype);
         if (wcs->m_crval) free(wcs->m_crval);
         if (wcs->m_pv)    free(wcs->m_pv);
         if (wcs->m_ps)    free(wcs->m_ps);
         if (wcs->m_cd)    free(wcs->m_cd);
         if (wcs->m_crota) free(wcs->m_crota);
         if (wcs->m_cname) free(wcs->m_cname);
         if (wcs->m_crder) free(wcs->m_crder);
         if (wcs->m_csyer) free(wcs->m_csyer);
      }

      if (wcs->lin.crpix == wcs->m_crpix) wcs->lin.crpix = 0;
      if (wcs->lin.pc    == wcs->m_pc)    wcs->lin.pc    = 0;
      if (wcs->lin.cdelt == wcs->m_cdelt) wcs->lin.cdelt = 0;
   }

   wcs->m_flag  = 0;
   wcs->m_naxis = 0;
   wcs->m_crpix = 0;
   wcs->m_pc    = 0;
   wcs->m_cdelt = 0;
   wcs->m_cunit = 0;
   wcs->m_ctype = 0;
   wcs->m_crval = 0;
   wcs->m_pv    = 0;
   wcs->m_ps    = 0;
   wcs->m_cd    = 0;
   wcs->m_crota = 0;
   wcs->m_cname = 0;
   wcs->m_crder = 0;
   wcs->m_csyer = 0;

   /* Free memory allocated by wcsset(). */
   if (wcs->flag == WCSSET) {
      if (wcs->types) free(wcs->types);
      if (wcs->tab)   free(wcs->tab);
   }

   wcs->types = 0;
   wcs->tab   = 0;

   wcs->flag = 0;

   return linfree(&(wcs->lin));
}

/*--------------------------------------------------------------------------*/

int wcsprt(const struct wcsprm *wcs)

{
   int i, j, k;

   if (wcs == 0) return 1;

   if (wcs->flag != WCSSET) {
      printf("The wcsprm struct is UNINITIALIZED.\n");
      return 0;
   }

   printf("       flag: %d\n", wcs->flag);
   printf("      naxis: %d\n", wcs->naxis);
   printf("      crpix: 0x%x\n", (int)wcs->crpix);
   printf("            ");
   for (i = 0; i < wcs->naxis; i++) {
      printf("  %- 11.4g", wcs->crpix[i]);
   }
   printf("\n");

   /* Linear transformation. */
   k = 0;
   printf("         pc: 0x%x\n", (int)wcs->pc);
   for (i = 0; i < wcs->naxis; i++) {
      printf("    pc[%d][]:", i);
      for (j = 0; j < wcs->naxis; j++) {
         printf("  %- 11.4g", wcs->pc[k++]);
      }
      printf("\n");
   }

   printf("      cdelt: 0x%x\n", (int)wcs->cdelt);
   printf("            ");
   for (i = 0; i < wcs->naxis; i++) {
      printf("  %- 11.4g", wcs->cdelt[i]);
   }
   printf("\n");

   /* Coordinate units and type. */
   printf("      cunit: 0x%x\n", (int)wcs->cunit);
   for (i = 0; i < wcs->naxis; i++) {
      printf("             \"%s\"\n", wcs->cunit[i]);
   }

   printf("      ctype: 0x%x\n", (int)wcs->ctype);
   for (i = 0; i < wcs->naxis; i++) {
      printf("             \"%s\"\n", wcs->ctype[i]);
   }

   /* Coordinate reference value. */
   printf("      crval: 0x%x\n", (int)wcs->crval);
   printf("            ");
   for (i = 0; i < wcs->naxis; i++) {
      printf("  %- 11.4g", wcs->crval[i]);
   }
   printf("\n");

   /* Celestial and spectral transformation parameters. */
   printf("    lonpole: %9f\n", wcs->lonpole);
   printf("    latpole: %9f\n", wcs->latpole);
   printf("    restfrq: %f\n", wcs->restfrq);
   printf("    restwav: %f\n", wcs->restwav);

   /* Parameter values. */
   printf("        npv: %d\n",  wcs->npv);
   printf("     npvmax: %d\n",  wcs->npvmax);
   printf("         pv: 0x%x\n", (int)wcs->pv);
   for (i = 0; i < wcs->npv; i++) {
      printf("             %3d%4d  %- 11.4g\n", (wcs->pv[i]).i,
         (wcs->pv[i]).m, (wcs->pv[i]).value);
   }
   printf("        nps: %d\n",  wcs->nps);
   printf("     npsmax: %d\n",  wcs->npsmax);
   printf("         ps: 0x%x\n", (int)wcs->ps);
   for (i = 0; i < wcs->nps; i++) {
      printf("             %3d%4d  %s\n", (wcs->ps[i]).i,
         (wcs->ps[i]).m, (wcs->ps[i]).value);
   }

   /* Alternate linear transformations. */
   printf("     altlin: %d\n", wcs->altlin);

   k = 0;
   printf("         cd: 0x%x\n", (int)wcs->cd);
   if (wcs->cd) {
      for (i = 0; i < wcs->naxis; i++) {
         printf("    cd[%d][]:", i);
         for (j = 0; j < wcs->naxis; j++) {
            printf("  %- 11.4g", wcs->cd[k++]);
         }
         printf("\n");
      }
   }

   printf("      crota: 0x%x\n", (int)wcs->crota);
   if (wcs->crota) {
      printf("            ");
      for (i = 0; i < wcs->naxis; i++) {
         printf("  %- 11.4g", wcs->crota[i]);
      }
      printf("\n");
   }

   /* Auxiliary coordinate system information. */
   printf("        alt: '%c'\n", wcs->alt[0]);
   printf("     colnum: %d\n", wcs->colnum);

   if (wcs->wcsname[0] == '\0') {
      printf("    wcsname: UNDEFINED\n");
   } else {
      printf("    wcsname: \"%s\"\n", wcs->wcsname);
   }

   printf("      cname: 0x%x\n", (int)wcs->cname);
   if (wcs->cname) {
      for (i = 0; i < wcs->naxis; i++) {
         if (wcs->cname[i][0] == '\0') {
            printf("             UNDEFINED\n");
         } else {
            printf("             \"%s\"\n", wcs->cname[i]);
         }
      }
   }

   printf("      crder: 0x%x\n", (int)wcs->crder);
   if (wcs->crder) {
      printf("           ");
      for (i = 0; i < wcs->naxis; i++) {
         if (undefined(wcs->crder[i])) {
            printf("  UNDEFINED   ");
         } else {
            printf("  %- 11.4g", wcs->crder[i]);
         }
      }
      printf("\n");
   }

   printf("      csyer: 0x%x\n", (int)wcs->csyer);
   if (wcs->csyer) {
      printf("           ");
      for (i = 0; i < wcs->naxis; i++) {
         if (undefined(wcs->csyer[i])) {
            printf("  UNDEFINED   ");
         } else {
            printf("  %- 11.4g", wcs->csyer[i]);
         }
      }
      printf("\n");
   }

   if (wcs->radesys[0] == '\0') {
      printf("    radesys: UNDEFINED\n");
   } else {
      printf("    radesys: \"%s\"\n", wcs->radesys);
   }

   if (undefined(wcs->equinox)) {
      printf("    equinox: UNDEFINED\n");
   } else {
      printf("    equinox: %9f\n", wcs->equinox);
   }

   if (wcs->specsys[0] == '\0') {
      printf("    specsys: UNDEFINED\n");
   } else {
      printf("    specsys: \"%s\"\n", wcs->specsys);
   }

   if (wcs->ssysobs[0] == '\0') {
      printf("    ssysobs: UNDEFINED\n");
   } else {
      printf("    ssysobs: \"%s\"\n", wcs->ssysobs);
   }

   if (undefined(wcs->velosys)) {
      printf("    velosys: UNDEFINED\n");
   } else {
      printf("    velosys: %9f\n", wcs->velosys);
   }

   if (wcs->ssyssrc[0] == '\0') {
      printf("    ssyssrc: UNDEFINED\n");
   } else {
      printf("    ssyssrc: \"%s\"\n", wcs->ssyssrc);
   }

   if (undefined(wcs->zsource)) {
      printf("    zsource: UNDEFINED\n");
   } else {
      printf("    zsource: %9f\n", wcs->zsource);
   }

   printf("     obsgeo: ");
   for (i = 0; i < 3; i++) {
      if (undefined(wcs->obsgeo[i])) {
         printf("UNDEFINED     ");
      } else {
         printf("%- 11.4g  ", wcs->obsgeo[i]);
      }
   }
   printf("\n");

   if (wcs->dateobs[0] == '\0') {
      printf("    dateobs: UNDEFINED\n");
   } else {
      printf("    dateobs: \"%s\"\n", wcs->dateobs);
   }

   if (undefined(wcs->mjdobs)) {
      printf("     mjdobs: UNDEFINED\n");
   } else {
      printf("     mjdobs: %9f\n", wcs->mjdobs);
   }

   if (undefined(wcs->mjdavg)) {
      printf("     mjdavg: UNDEFINED\n");
   } else {
      printf("     mjdavg: %9f\n", wcs->mjdavg);
   }

   /* Derived values. */
   printf("     lngtyp: \"%s\"\n", wcs->lngtyp);
   printf("     lattyp: \"%s\"\n", wcs->lattyp);
   printf("        lng: %d\n", wcs->lng);
   printf("        lat: %d\n", wcs->lat);
   printf("       spec: %d\n", wcs->spec);
   printf("   cubeface: %d\n", wcs->cubeface);

   printf("        lin: (see below)\n");
   printf("        cel: (see below)\n");
   printf("        spc: (see below)\n");

   printf("      types: 0x%x\n           ", (int)wcs->types);
   for (i = 0; i < wcs->naxis; i++) {
      printf("%5d", wcs->types[i]);
   }
   printf("\n");

   printf("        tab: 0x%x", (int)wcs->tab);
   if (wcs->tab != 0) printf("  (see below)");
   printf("\n");

   /* Memory management. */
   printf("     m_flag: %d\n", wcs->m_flag);
   printf("    m_naxis: %d\n", wcs->m_naxis);
   printf("    m_crpix: 0x%x", (int)wcs->m_crpix);
   if (wcs->m_crpix == wcs->crpix) printf("  (= crpix)");
   printf("\n");
   printf("       m_pc: 0x%x", (int)wcs->m_pc);
   if (wcs->m_pc == wcs->pc) printf("  (= pc)");
   printf("\n");
   printf("    m_cdelt: 0x%x", (int)wcs->m_cdelt);
   if (wcs->m_cdelt == wcs->cdelt) printf("  (= cdelt)");
   printf("\n");
   printf("    m_crval: 0x%x", (int)wcs->m_crval);
   if (wcs->m_crval == wcs->crval) printf("  (= crval)");
   printf("\n");
   printf("    m_cunit: 0x%x", (int)wcs->m_cunit);
   if (wcs->m_cunit == wcs->cunit) printf("  (= cunit)");
   printf("\n");
   printf("    m_ctype: 0x%x", (int)wcs->m_ctype);
   if (wcs->m_ctype == wcs->ctype) printf("  (= ctype)");
   printf("\n");
   printf("       m_pv: 0x%x", (int)wcs->m_pv);
   if (wcs->m_pv == wcs->pv) printf("  (= pv)");
   printf("\n");
   printf("       m_ps: 0x%x", (int)wcs->m_ps);
   if (wcs->m_ps == wcs->ps) printf("  (= ps)");
   printf("\n");

   /* Linear transformation parameters. */
   printf("\n");
   printf("   lin.*\n");
   linprt(&(wcs->lin));

   /* Celestial transformation parameters. */
   printf("\n");
   printf("   cel.*\n");
   celprt(&(wcs->cel));

   /* Spectral transformation parameters. */
   printf("\n");
   printf("   spc.*\n");
   spcprt(&(wcs->spc));

   /* Tabular transformation parameters. */
   if (wcs->tab) {
      printf("\n");
      printf("   tab.*\n");
      tabprt(wcs->tab);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsset(struct wcsprm *wcs)

{
   const int  nalias = 2;
   const char aliases [2][4] = {"NCP", "GLS"};

   char ctypei[72], pcode[4], requir[9], scode[4], stype[5];
   int i, j, k, m, naxis, *ndx = 0, status, tables;
   double lambda, rho;
   double *cd, *pc;
   struct celprm *wcscel = &(wcs->cel);
   struct prjprm *wcsprj = &(wcscel->prj);
   struct spcprm *wcsspc = &(wcs->spc);


   if (wcs == 0) return 1;

   /* Parse the CTYPEia cards. */
   pcode[0] = '\0';
   requir[0] = '\0';
   wcs->lng  = -1;
   wcs->lat  = -1;
   wcs->spec = -1;
   wcs->cubeface = -1;


   naxis = wcs->naxis;
   if (wcs->types != 0) free(wcs->types);
   wcs->types = calloc(naxis, sizeof(int));

   for (i = 0; i < naxis; i++) {
      wcs->types[i] = 0;
      strncpy(ctypei, wcs->ctype[i], 72);

      /* Null fill. */
      ctypei[71] = '\0';
      for (j = 0; j < 72; j++) {
         if (ctypei[j] == '\0') {
            for (k = j+1; k < 72; k++) {
               ctypei[k] = '\0';
            }
            break;
         }
      }

      /* Ignore trailing blanks. */
      for (k = j-1; k >= 0; k--) {
         if (ctypei[k] != ' ') break;
         ctypei[k] = '\0';
      }

      /* Is CTYPEia in "4-3" form? */
      if (ctypei[4] != '-' || ctypei[8] != '\0') {
         /* No, but check for a CUBEFACE axis. */
         if (strcmp(ctypei, "CUBEFACE") == 0) {
            if (wcs->cubeface == -1) {
               wcs->cubeface = i;
            } else {
               /* Multiple CUBEFACE axes! */
               return 4;
            }
         }

         /* Linear axis. */
         continue;
      }


      /* Got an axis qualifier, is it a logarithmic or tabular axis? */
      if (strncmp(ctypei+5, "LOG", 3) == 0) {
         /* Logarithmic axis found. */
         wcs->types[i] = 100;
         break;
      } else if (strncmp(ctypei+5, "TAB", 3) == 0) {
         /* Tabular axis found. */
         wcs->types[i] = 400;
         break;
      }


      /* Is it a recognized spectral type? */
      if (!spctyp(ctypei, stype, scode, 0, 0, 0, 0, 0)) {
         if (strcmp(scode, "   ")) {
            /* Non-linear spectral axis found. */
            wcs->types[i] += 300;
            wcs->spec = i;
         }

         continue;
      }


      /* Is it a recognized celestial projection? */
      for (k = 0; k < prj_ncode; k++) {
         if (strncmp(ctypei+5, prj_codes[k], 3) == 0) break;
      }

      if (k == prj_ncode) {
         /* Not a standard projection code, maybe it's an alias. */
         for (k = 0; k < nalias; k++) {
            if (strncmp(ctypei+5, aliases[k], 3) == 0) break;
         }

         if (k == nalias) {
            /* Not a recognized algorithm code of any type. */
            return 4;
         }
      }

      /* Parse the celestial axis type. */
      wcs->types[i] = 200;
      if (strcmp(pcode, "") == 0) {
         /* The first of the two celestial axes. */
         sprintf(pcode, "%.3s", ctypei+5);

         if (strncmp(ctypei, "RA--", 4) == 0) {
            wcs->lng = i;
            strcpy(wcs->lngtyp, "RA");
            strcpy(wcs->lattyp, "DEC");
            ndx = &wcs->lat;
            sprintf(requir, "DEC--%s", pcode);
         } else if (strncmp(ctypei, "DEC-", 4) == 0) {
            wcs->lat = i;
            strcpy(wcs->lngtyp, "RA");
            strcpy(wcs->lattyp, "DEC");
            ndx = &wcs->lng;
            sprintf(requir, "RA---%s", pcode);
         } else if (strncmp(ctypei+1, "LON", 3) == 0) {
            wcs->lng = i;
            sprintf(wcs->lngtyp, "%cLON", ctypei[0]);
            sprintf(wcs->lattyp, "%cLAT", ctypei[0]);
            ndx = &wcs->lat;
            sprintf(requir, "%s-%s", wcs->lattyp, pcode);
         } else if (strncmp(ctypei+1, "LAT", 3) == 0) {
            wcs->lat = i;
            sprintf(wcs->lngtyp, "%cLON", ctypei[0]);
            sprintf(wcs->lattyp, "%cLAT", ctypei[0]);
            ndx = &wcs->lng;
            sprintf(requir, "%s-%s", wcs->lngtyp, pcode);
         } else if (strncmp(ctypei+2, "LN", 2) == 0) {
            wcs->lng = i;
            sprintf(wcs->lngtyp, "%c%cLN", ctypei[0], ctypei[1]);
            sprintf(wcs->lattyp, "%c%cLT", ctypei[0], ctypei[1]);
            ndx = &wcs->lat;
            sprintf(requir, "%s-%s", wcs->lattyp, pcode);
         } else if (strncmp(ctypei+2, "LT", 2) == 0) {
            wcs->lat = i;
            sprintf(wcs->lngtyp, "%c%cLN", ctypei[0], ctypei[1]);
            sprintf(wcs->lattyp, "%c%cLT", ctypei[0], ctypei[1]);
            ndx = &wcs->lng;
            sprintf(requir, "%s-%s", wcs->lngtyp, pcode);
         } else {
            /* Unrecognized celestial type. */
            wcs->lng = -1;
            wcs->lat = -1;
            return 4;
         }

      } else {
         /* Looking for the complementary celestial axis. */
         if (strncmp(ctypei, requir, 8) != 0) {
            /* Inconsistent projection types. */
            wcs->lng = -1;
            wcs->lat = -1;
            return 4;
         }

         *ndx = i;
         requir[0] = '\0';
      }
   }

   /* Do we have a complementary pair of celestial axes? */
   if (strcmp(requir, "")) {
      /* Unmatched celestial axis. */
      wcs->lng = -1;
      wcs->lat = -1;
      return 4;
   }


   /* Celestial projection present? */
   if (wcs->lng >= 0) {
      celini(wcscel);

      /* CRVALia, LONPOLEa, and LATPOLEa cards. */
      wcscel->ref[0] = wcs->crval[wcs->lng];
      wcscel->ref[1] = wcs->crval[wcs->lat];
      wcscel->ref[2] = wcs->lonpole;
      wcscel->ref[3] = wcs->latpole;

      /* PVi_ma cards. */
      for (k = 0; k < wcs->npv; k++) {
         i = wcs->pv[k].i - 1;
         m = wcs->pv[k].m;

         if (i == 0) {
            /* From a PROJPn card. */
            i = wcs->lat;
         }

         if (i == wcs->lat) {
            /* PVi_ma associated with latitude axis. */
            if (m < 30) {
               wcsprj->pv[m] = wcs->pv[k].value;
            }

         } else if (i == wcs->lng) {
            /* PVi_ma associated with longitude axis. */
            switch (m) {
            case 0:
               wcscel->offset = (wcs->pv[k].value != 0.0);
               break;
            case 1:
               wcscel->phi0   = wcs->pv[k].value;
               break;
            case 2:
               wcscel->theta0 = wcs->pv[k].value;
               break;
            case 3:
               /* If present, overrides the LONPOLEa card. */
               wcscel->ref[2] = wcs->pv[k].value;
               break;
            case 4:
               /* If present, overrides the LATPOLEa card. */
               wcscel->ref[3] = wcs->pv[k].value;
               break;
            default:
               return 6;
               break;
            }
         }
      }

      /* Do simple alias translations. */
      if (strncmp(pcode, "GLS", 3) == 0) {
         strcpy(wcsprj->code, "SFL");

      } else if (strcmp(pcode, "NCP") == 0) {
         /* Convert NCP to SIN. */
         if (wcscel->ref[1] == 0.0) {
            return 5;
         }

         strcpy(wcsprj->code, "SIN");
         wcsprj->pv[1] = 0.0;
         wcsprj->pv[2] = cosd(wcscel->ref[1])/sind(wcscel->ref[1]);

      } else {
         strcpy(wcsprj->code, pcode);
      }

      /* Initialize the celestial transformation routines. */
      wcsprj->r0 = 0.0;
      if (status = celset(wcscel)) {
         return status + 3;
      }
   }


   /* Spectral axis present? */
   if (wcs->spec >= 0) {
      spcini(wcsspc);

      /* CRVALia, RESTFRQa, and RESTWAVa cards. */
      strcpy(wcsspc->type, stype);
      strcpy(wcsspc->code, scode);
      wcsspc->crval = wcs->crval[wcs->spec];
      wcsspc->restfrq = wcs->restfrq;
      wcsspc->restwav = wcs->restwav;

      /* PVi_ma cards. */
      for (k = 0; k < wcs->npv; k++) {
         i = wcs->pv[k].i - 1;
         m = wcs->pv[k].m;

         if (i == wcs->spec) {
            /* PVi_ma associated with grism axis. */
            if (m < 7) {
               wcsspc->pv[m] = wcs->pv[k].value;
            }
         }
      }

      /* Initialize the spectral transformation routines. */
      if (status = spcset(wcsspc)) {
         return status + 3;
      }
   }


   /* Tabular axes present? */
   if (tables) {

   }


   /* Initialize the linear transformation. */
   wcs->altlin &= 7;
   if (wcs->altlin > 1 && !(wcs->altlin & 1)) {
      pc = wcs->pc;

      if (wcs->altlin & 2) {
         /* Copy CDi_ja to PCi_ja and reset CDELTia. */
         cd = wcs->cd;
         for (i = 0; i < naxis; i++) {
            for (j = 0; j < naxis; j++) {
               *(pc++) = *(cd++);
            }
            wcs->cdelt[i] = 1.0;
         }

      } else if (wcs->altlin & 4) {
         /* Construct PCi_ja from CROTAia. */
         if ((i = wcs->lng) >= 0 && (j = wcs->lat) >= 0) {
            rho = wcs->crota[j];

            if (wcs->cdelt[i] == 0.0) return 3;
            lambda = wcs->cdelt[j]/wcs->cdelt[i];

            *(pc + i*naxis + i) = *(pc + j*naxis + j) = cosd(rho);
            *(pc + i*naxis + j) = *(pc + j*naxis + i) = sind(rho);
            *(pc + i*naxis + j) *= -lambda;
            *(pc + j*naxis + i) /=  lambda;
         }
      }
   }

   wcs->lin.crpix  = wcs->crpix;
   wcs->lin.pc     = wcs->pc;
   wcs->lin.cdelt  = wcs->cdelt;
   if (status = linset(&(wcs->lin))) {
      return status;
   }

   wcs->flag = WCSSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsp2s(
   struct wcsprm *wcs,
   int ncoord,
   int nelem,
   const double pixcrd[],
   double imgcrd[],
   double phi[],
   double theta[],
   double world[],
   int stat[])

{
   int    face, i, iso_x, iso_y, istat, k, nx, ny, *statp, status, wcslat,
          wcslng, wcspec;
   double offset, *worldlat, *worldlng;
   register double *img, *wrl;
   struct celprm *wcscel = &(wcs->cel);
   struct prjprm *wcsprj = &(wcscel->prj);

   /* Initialize if required. */
   if (wcs == 0) return 1;
   if (wcs->flag != WCSSET) {
      if (status = wcsset(wcs)) return status;
   }

   /* Sanity check. */
   if (ncoord < 1 || (ncoord > 1 && nelem < wcs->naxis)) return 4;


   /* Apply pixel-to-world linear transformation. */
   if (status = linp2x(&(wcs->lin), ncoord, nelem, pixcrd, imgcrd)) {
      return status;
   }

   /* For convenience. */
   wcslng = wcs->lng;
   wcslat = wcs->lat;
   wcspec = wcs->spec;


   /* Convert intermediate world coordinates to world coordinates. */
   img = imgcrd;
   wrl = world;
   for (k = 0; k < ncoord; k++) {
      for (i = 0; i < wcs->naxis; i++, img++, wrl++) {
         if (i == wcslng || i == wcslat || i == wcspec) continue;
         *wrl = *img + wcs->crval[i];
      }

      /* Zero the unused world coordinate elements. */
      for (i = wcs->naxis; i < nelem; i++) {
         *(wrl++) = 0.0;
      }

      img += (nelem - wcs->naxis);
   }

   /* Convert celestial coordinates. */
   if (wcslng >= 0) {
      /* Do we have a CUBEFACE axis? */
      if (wcs->cubeface != -1) {
         /* Separation between faces. */
         if (wcsprj->r0 == 0.0) {
            offset = 90.0;
         } else {
            offset = wcsprj->r0*PI/2.0;
         }

         /* Lay out faces in a plane. */
         img = imgcrd;
         statp = stat;
         for (k = 0; k < ncoord; k++, statp++) {
            face = (int)(*(img+wcs->cubeface) + 0.5);
            if (fabs(*(img+wcs->cubeface) - face) > 1e-10) {
               *statp = 1;
               status = 8;
            } else {
               switch (face) {
               case 0:
                  *(img+wcslat) += offset;
                  break;
               case 1:
                  break;
               case 2:
                  *(img+wcslng) += offset;
                  break;
               case 3:
                  *(img+wcslng) += offset*2;
                  break;
               case 4:
                  *(img+wcslng) += offset*3;
                  break;
               case 5:
                  *(img+wcslat) -= offset;
                  break;
               default:
                  *statp = 1;
                  status = 8;
               }
            }

            img += nelem;
         }
      }


      /* Check for constant x and/or y. */
      nx = ncoord;
      ny = 0;

      if (iso_x = wcs_allEq(ncoord, nelem, imgcrd+wcslng)) {
         nx = 1;
         ny = ncoord;
      }
      if (iso_y = wcs_allEq(ncoord, nelem, imgcrd+wcslat)) {
         ny = 1;
      }

      /* Transform projection plane coordinates to celestial coordinates. */
      worldlng = world + wcslng;
      worldlat = world + wcslat;
      if (istat = celx2s(wcscel, nx, ny, nelem, nelem, imgcrd+wcslng,
                         imgcrd+wcslat, phi, theta, worldlng, worldlat,
                         stat)) {;
         if (istat == 5) {
            status = 8;
         } else {
            return istat + 3;
         }
      }

      /* If x and y were both constant, replicate values. */
      if (iso_x && iso_y) {
         wcs_setAll(ncoord, nelem, worldlng);
         wcs_setAll(ncoord, nelem, worldlat);
         wcs_setAll(ncoord, 1, phi);
         wcs_setAll(ncoord, 1, theta);
         wcs_setAli(ncoord, 1, stat);
      }
   }


   /* Convert spectral coordinates. */
   if (wcspec >= 0) {
      /* Check for constant x. */
      nx = ncoord;
      if (iso_x = wcs_allEq(ncoord, nelem, imgcrd+wcspec)) {
         nx = 1;
      }

      if (istat = spcx2s(&(wcs->spc), nx, nelem, nelem, imgcrd+wcspec,
                         world+wcspec, stat)) {
         if (istat == 3) {
            status = 8;
         } else {
            return istat + 3;
         }
      }

      /* If x was constant, replicate values. */
      if (iso_x) {
         wcs_setAll(ncoord, nelem, world+wcspec);
         wcs_setAli(ncoord, 1, stat);
      }
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int wcss2p(
   struct wcsprm* wcs,
   int ncoord,
   int nelem,
   const double world[],
   double phi[],
   double theta[],
   double imgcrd[],
   double pixcrd[],
   int stat[])

{
   int    i, isolat, isolng, isospec, istat, k, nlat, nlng, nspec, status,
          wcslat, wcslng, wcspec;
   double offset;
   register const double *wrl;
   register double *img;
   struct celprm *wcscel = &(wcs->cel);
   struct prjprm *wcsprj = &(wcscel->prj);


   /* Initialize if required. */
   status = 0;
   if (wcs == 0) return 1;
   if (wcs->flag != WCSSET) {
      if (status = wcsset(wcs)) return status;
   }

   /* Sanity check. */
   if (ncoord < 1 || (ncoord > 1 && nelem < wcs->naxis)) return 4;

   /* For convenience. */
   wcslng = wcs->lng;
   wcslat = wcs->lat;
   wcspec = wcs->spec;


   /* Convert world coordinates to intermediate world coordinates. */
   wrl = world;
   img = imgcrd;

   for (k = 0; k < ncoord; k++) {
      for (i = 0; i < wcs->naxis; i++, wrl++, img++) {
         if (i == wcslng || i == wcslat || i == wcspec) continue;
         *img = *wrl - wcs->crval[i];
      }

      /* Zero the unused intermediate world coordinate elements. */
      for (i = wcs->naxis; i < nelem; i++) {
         *(img++) = 0.0;
      }

      wrl += (nelem - wcs->naxis);
   }


   /* Celestial coordinates. */
   if (wcslng >= 0) {
      /* Check for constant lng and/or lat. */
      nlng = ncoord;
      nlat = 0;

      if (isolng = wcs_allEq(ncoord, nelem, world+wcslng)) {
         nlng = 1;
         nlat = ncoord;
      }
      if (isolat = wcs_allEq(ncoord, nelem, world+wcslat)) {
         nlat = 1;
      }

      /* Transform celestial coordinates to projection plane coordinates. */
      if (istat = cels2x(wcscel, nlng, nlat, nelem, nelem, world+wcslng,
                         world+wcslat, phi, theta, imgcrd+wcslng,
                         imgcrd+wcslat, stat)) {
         if (istat == 6) {
            status = 9;
         } else {
            return istat + 3;
         }
      }

      /* If lng and lat were both constant, replicate values. */
      if (isolng && isolat) {
         wcs_setAll(ncoord, nelem, imgcrd+wcslng);
         wcs_setAll(ncoord, nelem, imgcrd+wcslat);
         wcs_setAll(ncoord, 1, phi);
         wcs_setAll(ncoord, 1, theta);
         wcs_setAli(ncoord, 1, stat);
      }

      /* Do we have a CUBEFACE axis? */
      if (wcs->cubeface != -1) {
         /* Separation between faces. */
         if (wcsprj->r0 == 0.0) {
            offset = 90.0;
         } else {
            offset = wcsprj->r0*PI/2.0;
         }

         /* Stack faces in a cube. */
         img = imgcrd;
         for (k = 0; k < ncoord; k++) {
            if (*(img+wcslat) < -0.5*offset) {
               *(img+wcslat) += offset;
               *(img+wcs->cubeface) = 5.0;
            } else if (*(img+wcslat) > 0.5*offset) {
               *(img+wcslat) -= offset;
               *(img+wcs->cubeface) = 0.0;
            } else if (*(img+wcslng) > 2.5*offset) {
               *(img+wcslng) -= 3.0*offset;
               *(img+wcs->cubeface) = 4.0;
            } else if (*(img+wcslng) > 1.5*offset) {
               *(img+wcslng) -= 2.0*offset;
               *(img+wcs->cubeface) = 3.0;
            } else if (*(img+wcslng) > 0.5*offset) {
               *(img+wcslng) -= offset;
               *(img+wcs->cubeface) = 2.0;
            } else {
               *(img+wcs->cubeface) = 1.0;
            }

            img += nelem;
         }
      }
   }


   /* Spectral coordinates. */
   if (wcspec >= 0) {
      /* Check for constant spec. */
      nspec = ncoord;
      if (isospec = wcs_allEq(ncoord, nelem, world+wcspec)) {
         nspec = 1;
      }

      if (istat = spcs2x(&(wcs->spc), nspec, nelem, nelem, world+wcspec,
                         imgcrd+wcspec, stat)) {
         if (istat == 4) {
            status = 9;
         } else {
            return istat + 3;
         }
      }

      /* If spec was constant, replicate values. */
      if (isospec) {
         wcs_setAll(ncoord, nelem, imgcrd+wcspec);
         wcs_setAli(ncoord, 1, stat);
      }
   }


   /* Apply world-to-pixel linear transformation. */
   if (istat = linx2p(&(wcs->lin), ncoord, nelem, imgcrd, pixcrd)) {
      return istat;
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int wcsmix(
   struct wcsprm *wcs,
   int mixpix,
   int mixcel,
   const double vspan[2],
   double vstep,
   int viter,
   double world[],
   double phi[],
   double theta[],
   double imgcrd[],
   double pixcrd[])

{
   const int niter = 60;
   int    crossed, istep, iter, j, k, nstep, retry, stat[1], status;
   const double tol  = 1.0e-10;
   const double tol2 = 100.0*tol;
   double *worldlat, *worldlng;
   double lambda, span[2], step;
   double pixmix;
   double dlng, lng, lng0, lng0m, lng1, lng1m;
   double dlat, lat, lat0, lat0m, lat1, lat1m;
   double d, d0, d0m, d1, d1m, dx = 0.0;
   double dabs, dmin, lmin;
   double dphi, phi0, phi1;
   struct celprm *wcscel = &(wcs->cel);
   struct wcsprm wcs0;

   /* Initialize if required. */
   if (wcs == 0) return 1;
   if (wcs->flag != WCSSET) {
      if (status = wcsset(wcs)) return status;
   }

   worldlng = world + wcs->lng;
   worldlat = world + wcs->lat;


   /* Check vspan. */
   if (vspan[0] <= vspan[1]) {
      span[0] = vspan[0];
      span[1] = vspan[1];
   } else {
      /* Swap them. */
      span[0] = vspan[1];
      span[1] = vspan[0];
   }

   /* Check vstep. */
   step = fabs(vstep);
   if (step == 0.0) {
      step = (span[1] - span[0])/10.0;
      if (step > 1.0 || step == 0.0) step = 1.0;
   }

   /* Check viter. */
   nstep = viter;
   if (nstep < 5) {
      nstep = 5;
   } else if (nstep > 10) {
      nstep = 10;
   }

   /* Given pixel element. */
   pixmix = pixcrd[mixpix];

   /* Iterate on the step size. */
   for (istep = 0; istep <= nstep; istep++) {
      if (istep) step /= 2.0;

      /* Iterate on the sky coordinate between the specified range. */
      if (mixcel == 1) {
         /* Celestial longitude is given. */

         /* Check whether the solution interval is a crossing interval. */
         lat0 = span[0];
         *worldlat = lat0;
         if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }
         d0 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d0);
         if (dabs < tol) return 0;

         lat1 = span[1];
         *worldlat = lat1;
         if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }
         d1 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d1);
         if (dabs < tol) return 0;

         lmin = lat1;
         dmin = dabs;

         /* Check for a crossing point. */
         if (signbit(d0) != signbit(d1)) {
            crossed = 1;
            dx = d1;
         } else {
            crossed = 0;
            lat0 = span[1];
         }

         for (retry = 0; retry < 4; retry++) {
            /* Refine the solution interval. */
            while (lat0 > span[0]) {
               lat0 -= step;
               if (lat0 < span[0]) lat0 = span[0];
               *worldlat = lat0;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d0 = pixcrd[mixpix] - pixmix;

               /* Check for a solution. */
               dabs = fabs(d0);
               if (dabs < tol) return 0;

               /* Record the point of closest approach. */
               if (dabs < dmin) {
                  lmin = lat0;
                  dmin = dabs;
               }

               /* Check for a crossing point. */
               if (signbit(d0) != signbit(d1)) {
                  crossed = 2;
                  dx = d0;
                  break;
               }

               /* Advance to the next subinterval. */
               lat1 = lat0;
               d1 = d0;
            }

            if (crossed) {
               /* A crossing point was found. */
               for (iter = 0; iter < niter; iter++) {
                  /* Use regula falsi division of the interval. */
                  lambda = d0/(d0-d1);
                  if (lambda < 0.1) {
                     lambda = 0.1;
                  } else if (lambda > 0.9) {
                     lambda = 0.9;
                  }

                  dlat = lat1 - lat0;
                  lat = lat0 + lambda*dlat;
                  *worldlat = lat;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }

                  /* Check for a solution. */
                  d = pixcrd[mixpix] - pixmix;
                  dabs = fabs(d);
                  if (dabs < tol) return 0;

                  if (dlat < tol) {
                     /* An artifact of numerical imprecision. */
                     if (dabs < tol2) return 0;

                     /* Must be a discontinuity. */
                     break;
                  }

                  /* Record the point of closest approach. */
                  if (dabs < dmin) {
                     lmin = lat;
                     dmin = dabs;
                  }

                  if (signbit(d0) == signbit(d)) {
                     lat0 = lat;
                     d0 = d;
                  } else {
                     lat1 = lat;
                     d1 = d;
                  }
               }

               /* No convergence, must have been a discontinuity. */
               if (crossed == 1) lat0 = span[1];
               lat1 = lat0;
               d1 = dx;
               crossed = 0;

            } else {
               /* No crossing point; look for a tangent point. */
               if (lmin == span[0]) break;
               if (lmin == span[1]) break;

               lat = lmin;
               lat0 = lat - step;
               if (lat0 < span[0]) lat0 = span[0];
               lat1 = lat + step;
               if (lat1 > span[1]) lat1 = span[1];

               *worldlat = lat0;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d0 = fabs(pixcrd[mixpix] - pixmix);

               d  = dmin;

               *worldlat = lat1;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d1 = fabs(pixcrd[mixpix] - pixmix);

               for (iter = 0; iter < niter; iter++) {
                  lat0m = (lat0 + lat)/2.0;
                  *worldlat = lat0m;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }
                  d0m = fabs(pixcrd[mixpix] - pixmix);

                  if (d0m < tol) return 0;

                  lat1m = (lat1 + lat)/2.0;
                  *worldlat = lat1m;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }
                  d1m = fabs(pixcrd[mixpix] - pixmix);

                  if (d1m < tol) return 0;

                  if (d0m < d && d0m <= d1m) {
                     lat1 = lat;
                     d1   = d;
                     lat  = lat0m;
                     d    = d0m;
                  } else if (d1m < d) {
                     lat0 = lat;
                     d0   = d;
                     lat  = lat1m;
                     d    = d1m;
                  } else {
                     lat0 = lat0m;
                     d0   = d0m;
                     lat1 = lat1m;
                     d1   = d1m;
                  }
               }
            }
         }

      } else {
         /* Celestial latitude is given. */

         /* Check whether the solution interval is a crossing interval. */
         lng0 = span[0];
         *worldlng = lng0;
         if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }
         d0 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d0);
         if (dabs < tol) return 0;

         lng1 = span[1];
         *worldlng = lng1;
         if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }
         d1 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d1);
         if (dabs < tol) return 0;
         lmin = lng1;
         dmin = dabs;

         /* Check for a crossing point. */
         if (signbit(d0) != signbit(d1)) {
            crossed = 1;
            dx = d1;
         } else {
            crossed = 0;
            lng0 = span[1];
         }

         for (retry = 0; retry < 4; retry++) {
            /* Refine the solution interval. */
            while (lng0 > span[0]) {
               lng0 -= step;
               if (lng0 < span[0]) lng0 = span[0];
               *worldlng = lng0;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d0 = pixcrd[mixpix] - pixmix;

               /* Check for a solution. */
               dabs = fabs(d0);
               if (dabs < tol) return 0;

               /* Record the point of closest approach. */
               if (dabs < dmin) {
                  lmin = lng0;
                  dmin = dabs;
               }

               /* Check for a crossing point. */
               if (signbit(d0) != signbit(d1)) {
                  crossed = 2;
                  dx = d0;
                  break;
               }

               /* Advance to the next subinterval. */
               lng1 = lng0;
               d1 = d0;
            }

            if (crossed) {
               /* A crossing point was found. */
               for (iter = 0; iter < niter; iter++) {
                  /* Use regula falsi division of the interval. */
                  lambda = d0/(d0-d1);
                  if (lambda < 0.1) {
                     lambda = 0.1;
                  } else if (lambda > 0.9) {
                     lambda = 0.9;
                  }

                  dlng = lng1 - lng0;
                  lng = lng0 + lambda*dlng;
                  *worldlng = lng;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }

                  /* Check for a solution. */
                  d = pixcrd[mixpix] - pixmix;
                  dabs = fabs(d);
                  if (dabs < tol) return 0;

                  if (dlng < tol) {
                     /* An artifact of numerical imprecision. */
                     if (dabs < tol2) return 0;

                     /* Must be a discontinuity. */
                     break;
                  }

                  /* Record the point of closest approach. */
                  if (dabs < dmin) {
                     lmin = lng;
                     dmin = dabs;
                  }

                  if (signbit(d0) == signbit(d)) {
                     lng0 = lng;
                     d0 = d;
                  } else {
                     lng1 = lng;
                     d1 = d;
                  }
               }

               /* No convergence, must have been a discontinuity. */
               if (crossed == 1) lng0 = span[1];
               lng1 = lng0;
               d1 = dx;
               crossed = 0;

            } else {
               /* No crossing point; look for a tangent point. */
               if (lmin == span[0]) break;
               if (lmin == span[1]) break;

               lng = lmin;
               lng0 = lng - step;
               if (lng0 < span[0]) lng0 = span[0];
               lng1 = lng + step;
               if (lng1 > span[1]) lng1 = span[1];

               *worldlng = lng0;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d0 = fabs(pixcrd[mixpix] - pixmix);

               d  = dmin;

               *worldlng = lng1;
               if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                   pixcrd, stat)) {
                  return (status == 9) ? 10 : status;
               }
               d1 = fabs(pixcrd[mixpix] - pixmix);

               for (iter = 0; iter < niter; iter++) {
                  lng0m = (lng0 + lng)/2.0;
                  *worldlng = lng0m;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }
                  d0m = fabs(pixcrd[mixpix] - pixmix);

                  if (d0m < tol) return 0;

                  lng1m = (lng1 + lng)/2.0;
                  *worldlng = lng1m;
                  if (status = wcss2p(wcs, 1, 0, world, phi, theta, imgcrd,
                                      pixcrd, stat)) {
                     return (status == 9) ? 10 : status;
                  }
                  d1m = fabs(pixcrd[mixpix] - pixmix);

                  if (d1m < tol) return 0;

                  if (d0m < d && d0m <= d1m) {
                     lng1 = lng;
                     d1   = d;
                     lng  = lng0m;
                     d    = d0m;
                  } else if (d1m < d) {
                     lng0 = lng;
                     d0   = d;
                     lng  = lng1m;
                     d    = d1m;
                  } else {
                     lng0 = lng0m;
                     d0   = d0m;
                     lng1 = lng1m;
                     d1   = d1m;
                  }
               }
            }
         }
      }
   }


   /* Set cel0 to the unity transformation. */
   wcs0 = *wcs;
   wcs0.cel.euler[0] = -90.0;
   wcs0.cel.euler[1] =   0.0;
   wcs0.cel.euler[2] =  90.0;
   wcs0.cel.euler[3] =   1.0;
   wcs0.cel.euler[4] =   0.0;

   /* No convergence, check for aberrant behaviour at a native pole. */
   *theta = -90.0;
   for (j = 1; j <= 2; j++) {
      /* Could the celestial coordinate element map to a native pole? */
      *phi = 0.0;
      *theta = -*theta;
      status = sphx2s(wcscel->euler, 1, 1, 1, 1, phi, theta, &lng, &lat);

      if (mixcel == 1) {
         if (fabs(fmod(*worldlng-lng, 360.0)) > tol) continue;
         if (lat < span[0]) continue;
         if (lat > span[1]) continue;
         *worldlat = lat;
      } else {
         if (fabs(*worldlat-lat) > tol) continue;
         if (lng < span[0]) lng += 360.0;
         if (lng > span[1]) lng -= 360.0;
         if (lng < span[0]) continue;
         if (lng > span[1]) continue;
         *worldlng = lng;
      }

      /* Is there a solution for the given pixel coordinate element? */
      lng = *worldlng;
      lat = *worldlat;

      /* Feed native coordinates to wcss2p() with cel0 set to unity. */
      *worldlng = -180.0;
      *worldlat = *theta;
      if (status = wcss2p(&wcs0, 1, 0, world, phi, theta, imgcrd, pixcrd,
                          stat)) {
         return (status == 9) ? 10 : status;
      }
      d0 = pixcrd[mixpix] - pixmix;

      /* Check for a solution. */
      if (fabs(d0) < tol) {
         /* Recall saved world coordinates. */
         *worldlng = lng;
         *worldlat = lat;
         return 0;
      }

      /* Search for a crossing interval. */
      phi0 = -180.0;
      for (k = -179; k <= 180; k++) {
         phi1 = (double) k;
         *worldlng = phi1;
         if (status = wcss2p(&wcs0, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }
         d1 = pixcrd[mixpix] - pixmix;

         /* Check for a solution. */
         dabs = fabs(d1);
         if (dabs < tol) {
            /* Recall saved world coordinates. */
            *worldlng = lng;
            *worldlat = lat;
            return 0;
         }

         /* Is it a crossing interval? */
         if (signbit(d0) != signbit(d1)) break;

         phi0 = phi1;
         d0 = d1;
      }

      for (iter = 1; iter <= niter; iter++) {
         /* Use regula falsi division of the interval. */
         lambda = d0/(d0-d1);
         if (lambda < 0.1) {
            lambda = 0.1;
         } else if (lambda > 0.9) {
            lambda = 0.9;
         }

         dphi = phi1 - phi0;
         *worldlng = phi0 + lambda*dphi;
         if (status = wcss2p(&wcs0, 1, 0, world, phi, theta, imgcrd, pixcrd,
                             stat)) {
            return (status == 9) ? 10 : status;
         }

         /* Check for a solution. */
         d = pixcrd[mixpix] - pixmix;
         dabs = fabs(d);
         if (dabs < tol || (dphi < tol && dabs < tol2)) {
            /* Recall saved world coordinates. */
            *worldlng = lng;
            *worldlat = lat;
            return 0;
         }

         if (signbit(d0) == signbit(d)) {
            phi0 = *worldlng;
            d0 = d;
         } else {
            phi1 = *worldlng;
            d1 = d;
         }
      }
   }


   /* No solution. */
   return 11;
}

/*--------------------------------------------------------------------------*/

int wcssptr(
   struct wcsprm *wcs,
   int  *i,
   char ctypeS2[9])

{
   int    j, status;
   double cdeltS2, crvalS2;

   /* Initialize if required. */
   if (wcs == 0) return 1;
   if (wcs->flag != WCSSET) {
      if (status = wcsset(wcs)) return status;
   }

   if ((j = *i) < 0 && (j = wcs->spec) < 0) {
      /* Look for a linear spectral axis. */
      for (j = 0; j < wcs->naxis; j++) {
         if (!spctyp(wcs->ctype[j], 0, 0, 0, 0, 0, 0, 0)) {
            break;
         }
      }

      if (j >= wcs->naxis) {
         /* No spectral axis. */
         return 12;
      }

      *i = j;
   }

   /* Translate the spectral axis. */
   if (status = spctrn(wcs->ctype[j], wcs->crval[j], wcs->cdelt[j],
                       wcs->restfrq, wcs->restwav, ctypeS2, &crvalS2,
                       &cdeltS2)) {
      return 6;
   }

   wcs->flag = 0;
   wcs->cdelt[j] = cdeltS2;
   strcpy(wcs->ctype[j], ctypeS2);
   wcs->crval[j] = crvalS2;

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcs_allEq(int ncoord, int nelem, const double *first)

{
   double v0;
   const double *vp;

   v0 = *first;
   for (vp = first+nelem; vp < first + ncoord*nelem; vp += nelem) {
     if (*vp != v0) return 0;
   }

   return 1;
}

/*--------------------------------------------------------------------------*/

void wcs_setAll(int ncoord, int nelem, double *first)

{
   double v0, *vp;

   v0 = *first;
   for (vp = first+nelem; vp < first + ncoord*nelem; vp += nelem) {
     *vp = v0;
   }
}

/*--------------------------------------------------------------------------*/

void wcs_setAli(int ncoord, int nelem, int *first)

{
   int v0, *vp;

   v0 = *first;
   for (vp = first+nelem; vp < first + ncoord*nelem; vp += nelem) {
     *vp = v0;
   }
}
