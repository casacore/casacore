/*============================================================================
*
*   WCSLIB 3.2 - an implementation of the FITS WCS convention.
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
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "wcstrig.h"
#include "spc.h"
#include "spx.h"

/* Spectral algorithm codes. */
const int  spc_ncode = 17;
const char spc_codes[15][4] =
      {"LOG",			/* Logarithmic axis.                  */
       "F2W", "F2A", "F2V",	/* Axis linear in frequency.          */
       "W2F", "W2A", "W2V",	/* Axis linear in vacuum wavelengths. */
       "A2F", "A2W", "A2V",	/* Axis linear in air wavelength.     */
       "V2F", "V2W", "V2A",	/* Axis linear in velocity.           */
       "GRI", "GRA"};		/* Grism in vacuum or air.            */

/* Spectral algorithm types. */
#define LOG 100;		/* Logarithmic axis.                  */
#define F2S 200;		/* Axis linear in frequency.          */
#define W2S 300;		/* Axis linear in vacuum wavelengths. */
#define A2S 400;		/* Axis linear in air wavelengths.    */
#define V2S 500;		/* Axis linear in velocity.           */
#define GRI 600;		/* Grism in vacuum.                   */
#define GRA 700;		/* Grism in air.                      */

/* Spectral coordinate types. */
#define FREQ  0;		/* Frequency-like.                    */
#define AFRQ  1;		/* Frequency-like.                    */
#define ENER  2;		/* Frequency-like.                    */
#define WAVN  3;		/* Frequency-like.                    */
#define VRAD  4;		/* Frequency-like.                    */
#define WAVE 10;		/* Vacuum wavelength-like.            */
#define VOPT 11;		/* Vacuum wavelength-like.            */
#define ZOPT 12;		/* Vacuum wavelength-like.            */
#define AWAV 20;		/* Air wavelength-like.               */
#define VELO 30;		/* Velocity-like.                     */
#define BETA 31;		/* Velocity-like.                     */


/* Map error number to error message. */
const char *spc_errmsg[] = {
   0,
   "Null spcprm pointer passed",
   "Invalid spectral parameters",
   "One or more of x coordinates were invalid",
   "One or more of the spec coordinates were invalid"};


#define UNDEFINED 987654321.0e99
#define undefined(value) (value == UNDEFINED)

#define C 2.99792458e8

/*--------------------------------------------------------------------------*/

int spcini(spc)

struct spcprm *spc;

{
   register int k;

   if (spc == 0) return 1;

   spc->flag = 0;

   strcpy(spc->type, "    ");
   strcpy(spc->code, "   ");

   spc->crval = UNDEFINED;
   spc->restfrq = 0.0;
   spc->restwav = 0.0;

   for (k = 0; k < 7; k++) {
      spc->pv[k] = UNDEFINED;
   }

   spc->isGrism = 0;

   for (k = 0; k < 6; k++) {
     spc->w[k] = 0.0;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int spcprt(spc)

const struct spcprm *spc;

{
   int i;

   if (spc == 0) return 1;

   printf("       flag: %d\n", spc->flag);
   printf("       type: \"%s\"\n", spc->type);
   printf("       code: \"%s\"\n", spc->code);
   if (undefined(spc->crval)) {
      printf("      crval: UNDEFINED\n");
   } else {
      printf("      crval: %- 11.4g\n", spc->crval);
   }
   printf("    restfrq: %f\n", spc->restfrq);
   printf("    restwav: %f\n", spc->restwav);

   printf("         pv:");
   if (spc->isGrism) {
      for (i = 0; i < 5; i++) {
         if (undefined(spc->pv[i])) {
            printf("  UNDEFINED   ");
         } else {
            printf("  %- 11.4g", spc->pv[i]);
         }
      }
      printf("\n            ");
      for (i = 5; i < 7; i++) {
         if (undefined(spc->pv[i])) {
            printf("  UNDEFINED   ");
         } else {
            printf("  %- 11.4g", spc->pv[i]);
         }
      }
      printf("\n");

   } else {
      printf(" (not used)\n");
   }

   printf("          w:");
   for (i = 0; i < 3; i++) {
      printf("  %- 11.4g", spc->w[i]);
   }
   if (spc->isGrism) {
      printf("\n            ");
      for (i = 3; i < 6; i++) {
         printf("  %- 11.4g", spc->w[i]);
      }
      printf("\n");
   } else {
      printf("  (remainder unused)\n");
   }

   printf("    isGrism: %d\n", spc->isGrism);
   printf("     spxx2q: 0x%x\n", (int)spc->spxx2q);
   printf("     spxq2s: 0x%x\n", (int)spc->spxq2s);
   printf("     spxs2q: 0x%x\n", (int)spc->spxs2q);
   printf("     spxq2x: 0x%x\n", (int)spc->spxq2x);

   return 0;
}

/*--------------------------------------------------------------------------*/

int spcset(spc)

struct spcprm *spc;

{
   char   p, x;
   int    status;
   double alpha, beta_r, dn_r, epsilon, G, m, lambda_r, n_r, t, theta;
   struct spxprm spx;

   if (spc == 0) return 1;

   if (undefined(spc->crval)) {
      return 2;
   }

   spc->type[4] = '\0';
   spc->code[3] = '\0';
   spc->w[0] = 0.0;


   /* Logarithmic axes. */
   if (strcmp(spc->code, "LOG") == 0) {
      spc->flag = LOG;

      spc->w[1] = log(spc->crval);
      spc->w[2] = 1.0/spc->crval;

      spc->spxq2s = 0;
      spc->spxs2q = 0;

      spc->spxx2q = logspec;
      spc->spxq2x = speclog;

      return 0;
   }

   /* Compute all spectral parameters and their derivatives. */
   if (status = specx(spc->type, spc->crval, spc->restfrq, spc->restwav,
                      &spx)) {
      return status;
   }


   /* Set pointers-to-functions for the linear part of the transformation. */
   if (strcmp(spc->code, "GRI") == 0) {
      spc->isGrism = 1;
   } else if (strcmp(spc->code, "GRA") == 0) {
      spc->isGrism = 2;
   } else {
      spc->isGrism = 0;
   }

   if (spc->isGrism) {
      p = '*';
   } else {
      p = spc->code[2];
   }

   if (strcmp(spc->type, "FREQ") == 0) {
      /* Frequency. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'F';
      } else if (p != 'F') {
         return 2;
      }

      spc->flag = FREQ;
      spc->w[2] = 1.0;
      spc->spxq2s = 0;
      spc->spxs2q = 0;

   } else if (strcmp(spc->type, "AFRQ") == 0) {
      /* Angular frequency. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'F';
      } else if (p != 'F') {
         return 2;
      }

      spc->flag = AFRQ;
      spc->w[2] = spx.dfreqafrq;
      spc->spxq2s = freqafrq;
      spc->spxs2q = afrqfreq;

   } else if (strcmp(spc->type, "ENER") == 0) {
      /* Photon energy. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'F';
      } else if (p != 'F') {
         return 2;
      }

      spc->flag = ENER;
      spc->w[2] = spx.dfreqener;
      spc->spxq2s = freqener;
      spc->spxs2q = enerfreq;

   } else if (strcmp(spc->type, "WAVN") == 0) {
      /* Wave number. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'F';
      } else if (p != 'F') {
         return 2;
      }

      spc->flag = WAVN;
      spc->w[2] = spx.dfreqwavn;
      spc->spxq2s = freqwavn;
      spc->spxs2q = wavnfreq;

   } else if (strcmp(spc->type, "VRAD") == 0) {
      /* Radio velocity. */
      if (!spx.velotype) return 2;

      if (p == '*') {
         p = 'F';
      } else if (p != 'F') {
         return 2;
      }

      spc->flag = VRAD;
      spc->w[0] = spc->restfrq;
      spc->w[2] = spx.dfreqvrad;
      spc->spxq2s = freqvrad;
      spc->spxs2q = vradfreq;

   } else if (strcmp(spc->type, "WAVE") == 0) {
      /* Vacuum wavelengths. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'W';
      } else if (p != 'W') {
         return 2;
      }

      spc->flag = WAVE;
      spc->w[2] = 1.0;
      spc->spxq2s = 0;
      spc->spxs2q = 0;

   } else if (strcmp(spc->type, "VOPT") == 0) {
      /* Optical velocity. */
      if (!spx.velotype) return 2;

      if (p == '*') {
         p = 'W';
      } else if (p != 'W') {
         return 2;
      }

      spc->flag = VOPT;
      spc->w[0] = spc->restwav;
      spc->w[2] = spx.dwavevopt;
      spc->spxq2s = wavevopt;
      spc->spxs2q = voptwave;

   } else if (strcmp(spc->type, "ZOPT") == 0) {
      /* Redshift. */
      if (!spx.velotype) return 2;

      if (p == '*') {
         p = 'W';
      } else if (p != 'W') {
         return 2;
      }

      spc->flag = ZOPT;
      spc->w[0] = spc->restwav;
      spc->w[2] = spx.dwavezopt;
      spc->spxq2s = wavezopt;
      spc->spxs2q = zoptwave;

   } else if (strcmp(spc->type, "AWAV") == 0) {
      /* Air wavelengths. */
      if (!spx.wavetype) return 2;

      if (p == '*') {
         p = 'A';
      } else if (p != 'A') {
         return 2;
      }

      spc->flag = AWAV;
      spc->w[2] = 1.0;
      spc->spxq2s = 0;
      spc->spxs2q = 0;

   } else if (strcmp(spc->type, "VELO") == 0) {
      /* Relativistic velocity. */
      if (!spx.velotype) return 2;

      if (p == '*') {
         p = 'V';
      } else if (p != 'V') {
         return 2;
      }

      spc->flag = VELO;
      spc->w[2] = 1.0;
      spc->spxq2s = 0;
      spc->spxs2q = 0;

   } else if (strcmp(spc->type, "BETA") == 0) {
      /* Velocity ratio (v/c). */
      if (!spx.velotype) return 2;

      if (p == '*') {
         p = 'V';
      } else if (p != 'V') {
         return 2;
      }

      spc->flag = BETA;
      spc->w[2] = spx.dvelobeta;
      spc->spxq2s = velobeta;
      spc->spxs2q = betavelo;

   } else {
      /* Unrecognized coordinate type. */
      return 2;

   }


   /* Set pointers-to-functions for the non-linear part of the spectral */
   /* transformation.                                                   */
   x = spc->code[0];
   if (x == 'F') {
      /* Axis is linear in frequency. */
      if (!spx.wavetype) return 2;

      if (strcmp(spc->code, "F2W") == 0) {
         spc->w[2] *= spx.dfreqwave;
         spc->spxx2q = freqwave;
         spc->spxq2x = wavefreq;

      } else if (strcmp(spc->code, "F2A") == 0) {
         spc->w[2] *= spx.dfreqawav;
         spc->spxx2q = freqawav;
         spc->spxq2x = awavfreq;

      } else if (strcmp(spc->code, "F2V") == 0) {
         spc->w[0] = spc->restfrq;
         spc->w[2] *= spx.dfreqvelo;
         spc->spxx2q = freqvelo;
         spc->spxq2x = velofreq;

      } else {
         /* Unrecognized spectral algorithm code. */
         return 2;
      }

      spc->flag += F2S;
      spc->w[1] = spx.freq;

   } else if (x == 'W') {
      if (!spx.wavetype) return 2;

      /* Axis is linear in vacuum wavelengths. */
      if (strcmp(spc->code, "W2F") == 0) {
         spc->w[2] *= spx.dwavefreq;
         spc->spxx2q = wavefreq;
         spc->spxq2x = freqwave;

      } else if (strcmp(spc->code, "W2A") == 0) {
         spc->w[2] *= spx.dwaveawav;
         spc->spxx2q = waveawav;
         spc->spxq2x = awavwave;

      } else if (strcmp(spc->code, "W2V") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dwavevelo;
         spc->spxx2q = wavevelo;
         spc->spxq2x = velowave;

      } else {
         /* Unrecognized spectral algorithm code. */
         return 2;
      }

      spc->flag += W2S;
      spc->w[1] = spx.wave;

   } else if (x == 'A') {
      if (!spx.wavetype) return 2;

      /* Axis is linear in air wavelengths. */
      if (strcmp(spc->code, "A2F") == 0) {
         spc->w[2] *= spx.dawavfreq;
         spc->spxx2q = awavfreq;
         spc->spxq2x = freqawav;

      } else if (strcmp(spc->code, "A2W") == 0) {
         spc->w[2] *= spx.dawavwave;
         spc->spxx2q = awavwave;
         spc->spxq2x = waveawav;

      } else if (strcmp(spc->code, "A2V") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dawavvelo;
         spc->spxx2q = awavvelo;
         spc->spxq2x = veloawav;

      } else {
         /* Unrecognized spectral algorithm code. */
         return 2;
      }

      spc->flag += A2S;
      spc->w[1] = spx.awav;

   } else if (x == 'V') {
      if (!spx.velotype) return 2;

      /* Axis is linear in relativistic velocity. */
      if (strcmp(spc->code, "V2F") == 0) {
         spc->w[0] = spc->restfrq;
         spc->w[2] *= spx.dvelofreq;
         spc->spxx2q = velofreq;
         spc->spxq2x = freqvelo;

      } else if (strcmp(spc->code, "V2W") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dvelowave;
         spc->spxx2q = velowave;
         spc->spxq2x = wavevelo;

      } else if (strcmp(spc->code, "V2A") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dveloawav;
         spc->spxx2q = veloawav;
         spc->spxq2x = awavvelo;

      } else {
         /* Unrecognized spectral algorithm code. */
         return 2;
      }

      spc->flag += V2S;
      spc->w[1] = spx.velo;

   } else if (spc->isGrism) {
      /* Axis is linear in "grism parameter"; work in wavelength. */
      if (!spx.wavetype) return 2;

      if (spc->isGrism == 1) {
         /* Grism in vacuum. */
         if (p == 'F') {
            spc->w[2] *= spx.dwavefreq;
            spc->spxx2q = wavefreq;
            spc->spxq2x = freqwave;

         } else if (p == 'W') {
            spc->spxx2q = 0;
            spc->spxq2x = 0;

         } else if (p == 'A') {
            spc->w[2] *= spx.dwaveawav;
            spc->spxx2q = waveawav;
            spc->spxq2x = awavwave;

         } else if (p == 'V') {
            spc->w[0] = spc->restwav;
            spc->w[2] *= spx.dwavevelo;
            spc->spxx2q = wavevelo;
            spc->spxq2x = velowave;

         } else {
            /* Shouldn't be possible. */
            return 2;
         }

         lambda_r = spx.wave;
         spc->flag += GRI;

      } else {
         /* Grism in air. */
         if (p == 'F') {
            spc->w[2] *= spx.dawavfreq;
            spc->spxx2q = awavfreq;
            spc->spxq2x = freqawav;

         } else if (p == 'W') {
            spc->w[2] *= spx.dawavwave;
            spc->spxx2q = awavwave;
            spc->spxq2x = waveawav;

         } else if (p == 'A') {
            spc->spxx2q = 0;
            spc->spxq2x = 0;

         } else if (p == 'V') {
            spc->w[0] = spc->restwav;
            spc->w[2] *= spx.dawavvelo;
            spc->spxx2q = awavvelo;
            spc->spxq2x = veloawav;

         } else {
            /* Shouldn't be possible. */
            return 2;
         }

         lambda_r = spx.awav;
         spc->flag += GRA;
      }

      /* Set defaults. */
      if (undefined(spc->pv[0])) spc->pv[0] = 0.0;
      if (undefined(spc->pv[1])) spc->pv[1] = 0.0;
      if (undefined(spc->pv[2])) spc->pv[2] = 0.0;
      if (undefined(spc->pv[3])) spc->pv[3] = 1.0;
      if (undefined(spc->pv[4])) spc->pv[4] = 0.0;
      if (undefined(spc->pv[5])) spc->pv[5] = 0.0;
      if (undefined(spc->pv[6])) spc->pv[6] = 0.0;

      /* Compute intermediaries. */
      G       = spc->pv[0];
      m       = spc->pv[1];
      alpha   = spc->pv[2];
      n_r     = spc->pv[3];
      dn_r    = spc->pv[4];
      epsilon = spc->pv[5];
      theta   = spc->pv[6];

      t = G*m/cosd(epsilon);
      beta_r = asind(t*lambda_r - n_r*sind(alpha));

      t -= dn_r*sind(alpha);

      spc->w[1] = -tand(theta);
      spc->w[2] *= t / (cosd(beta_r)*cosd(theta)*cosd(theta));
      spc->w[3] = beta_r + theta;
      spc->w[4] = (n_r - dn_r*lambda_r)*sind(alpha);
      spc->w[5] = 1.0 / t;

   } else {
      /* Unrecognized spectral algorithm code. */
      return 2;
   }


   return 0;
}

/*--------------------------------------------------------------------------*/

int spcx2s(spc, nx, sx, sspec, x, spec, stat)

struct spcprm *spc;
int nx, sspec, sx;
const double x[];
double spec[];
int stat[];

{
   int statq2s, status = 0, statx2q;
   double beta;
   register int ix;
   register int *statp;
   register const double *xp;
   register double *specp;


   /* Initialize. */
   if (spc == 0) return 1;
   if (spc->flag == 0) {
      if (spcset(spc)) return 2;
   }

   /* Convert intermediate world coordinate x to X. */
   xp = x;
   specp = spec;
   statp = stat;
   for (ix = 0; ix < nx; ix++, xp += sx, specp += sspec) {
      *specp = spc->w[1] + (*xp)*spc->w[2];
      *(statp++) = 0;
   }

   /* If X is the grism parameter then convert it to wavelength. */
   if (spc->isGrism) {
      specp = spec;
      for (ix = 0; ix < nx; ix++, specp += sspec) {
         beta = atand(*specp) + spc->w[3];
         *specp = (sind(beta) + spc->w[4]) * spc->w[5];
      }
   }

   /* Apply the non-linear step of the algorithm chain to convert spectral */
   /* coordinate X to intermediate spectral coordinate Q.                  */
   if (spc->spxx2q != 0) {
      if (statx2q = spc->spxx2q(spc->w[0], nx, sspec, sspec, spec, spec,
                                stat)) {
         if (statx2q == 4) {
            status = 3;
         } else {
            return statx2q;
         }
      }
   }

   /* Apply the linear step of the algorithm chain to convert intermediate */
   /* spectral coordinate Q to the required spectral coordinate S.         */
   if (spc->spxq2s != 0) {
      if (statq2s = spc->spxq2s(spc->w[0], nx, sspec, sspec, spec, spec,
                                stat)) {;
         if (statq2s == 4) {
            status = 3;
         } else {
            return statq2s;
         }
      }
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int spcs2x(spc, nspec, sspec, sx, spec, x, stat)

struct spcprm *spc;
int nspec, sspec, sx;
const double spec[];
double x[];
int stat[];

{
   int statq2x, status = 0, stats2q;
   double beta, s;
   register int ispec;
   register int *statp;
   register const double *specp;
   register double *xp;


   /* Initialize. */
   if (spc == 0) return 1;
   if (spc->flag == 0) {
      if (spcset(spc)) return 2;
   }

   /* Apply the linear step of the algorithm chain to convert spectral */
   /* coordinate S to intermediate spectral coordinate Q.              */
   if (spc->spxs2q != 0) {
      if (stats2q = spc->spxs2q(spc->w[0], nspec, sspec, sx, spec, x, stat)) {
         if (stats2q == 4) {
            status = 4;
         } else {
            return stats2q;
         }
      }

   } else {
      /* Just a copy. */
      xp = x;
      specp = spec;
      statp = stat;
      for (ispec = 0; ispec < nspec; ispec++, specp += sspec, xp += sx) {
         *xp = *specp;
         *(statp++) = 0;
      }
   }


   /* Apply the non-linear step of the algorithm chain to convert  */
   /* intermediate spectral coordinate Q to spectral coordinate X. */
   if (spc->spxq2x != 0) {
      if (statq2x = spc->spxq2x(spc->w[0], nspec, sx, sx, x, x, stat)) {
         if (statq2x == 4) {
            status = 4;
         } else {
            return statq2x;
         }
      }
   }

   if (spc->isGrism) {
      /* Convert X (wavelength) to grism parameter. */
      xp = x;
      statp = stat;
      for (ispec = 0; ispec < nspec; ispec++, xp += sx, statp++) {
         if (*statp) continue;

         s = *xp/spc->w[5] - spc->w[4];
         if (fabs(s) <= 1.0) {
            beta = asind(s);
            *xp = tand(beta - spc->w[3]);
         } else {
            *statp = 1;
         }
      }
   }


   /* Convert X to intermediate world coordinate x. */
   xp = x;
   statp = stat;
   for (ispec = 0; ispec < nspec; ispec++, xp += sx) {
      if (*(statp++)) continue;

      *xp -= spc->w[1];
      *xp /= spc->w[2];
   }

   return status;
}
