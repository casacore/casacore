/*============================================================================
*
*   WCSLIB 3.7 - an implementation of the FITS WCS standard.
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
*   http://www.atnf.csiro.au/~mcalabre/index.html
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


/* Map status return value to message. */
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

int spcini(struct spcprm *spc)

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

int spcprt(const struct spcprm *spc)

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
   printf("     spxX2P: 0x%x\n", (int)spc->spxX2P);
   printf("     spxP2S: 0x%x\n", (int)spc->spxP2S);
   printf("     spxS2P: 0x%x\n", (int)spc->spxS2P);
   printf("     spxP2X: 0x%x\n", (int)spc->spxP2X);

   return 0;
}

/*--------------------------------------------------------------------------*/

int spcset(struct spcprm *spc)

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

      spc->spxP2S = 0;
      spc->spxS2P = 0;

      spc->spxX2P = logspec;
      spc->spxP2X = speclog;

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
      spc->spxP2S = 0;
      spc->spxS2P = 0;

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
      spc->spxP2S = freqafrq;
      spc->spxS2P = afrqfreq;

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
      spc->spxP2S = freqener;
      spc->spxS2P = enerfreq;

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
      spc->spxP2S = freqwavn;
      spc->spxS2P = wavnfreq;

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
      spc->spxP2S = freqvrad;
      spc->spxS2P = vradfreq;

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
      spc->spxP2S = 0;
      spc->spxS2P = 0;

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
      spc->spxP2S = wavevopt;
      spc->spxS2P = voptwave;

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
      spc->spxP2S = wavezopt;
      spc->spxS2P = zoptwave;

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
      spc->spxP2S = 0;
      spc->spxS2P = 0;

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
      spc->spxP2S = 0;
      spc->spxS2P = 0;

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
      spc->spxP2S = velobeta;
      spc->spxS2P = betavelo;

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
         spc->spxX2P = freqwave;
         spc->spxP2X = wavefreq;

      } else if (strcmp(spc->code, "F2A") == 0) {
         spc->w[2] *= spx.dfreqawav;
         spc->spxX2P = freqawav;
         spc->spxP2X = awavfreq;

      } else if (strcmp(spc->code, "F2V") == 0) {
         spc->w[0] = spc->restfrq;
         spc->w[2] *= spx.dfreqvelo;
         spc->spxX2P = freqvelo;
         spc->spxP2X = velofreq;

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
         spc->spxX2P = wavefreq;
         spc->spxP2X = freqwave;

      } else if (strcmp(spc->code, "W2A") == 0) {
         spc->w[2] *= spx.dwaveawav;
         spc->spxX2P = waveawav;
         spc->spxP2X = awavwave;

      } else if (strcmp(spc->code, "W2V") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dwavevelo;
         spc->spxX2P = wavevelo;
         spc->spxP2X = velowave;

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
         spc->spxX2P = awavfreq;
         spc->spxP2X = freqawav;

      } else if (strcmp(spc->code, "A2W") == 0) {
         spc->w[2] *= spx.dawavwave;
         spc->spxX2P = awavwave;
         spc->spxP2X = waveawav;

      } else if (strcmp(spc->code, "A2V") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dawavvelo;
         spc->spxX2P = awavvelo;
         spc->spxP2X = veloawav;

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
         spc->spxX2P = velofreq;
         spc->spxP2X = freqvelo;

      } else if (strcmp(spc->code, "V2W") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dvelowave;
         spc->spxX2P = velowave;
         spc->spxP2X = wavevelo;

      } else if (strcmp(spc->code, "V2A") == 0) {
         spc->w[0] = spc->restwav;
         spc->w[2] *= spx.dveloawav;
         spc->spxX2P = veloawav;
         spc->spxP2X = awavvelo;

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
            spc->spxX2P = wavefreq;
            spc->spxP2X = freqwave;

         } else if (p == 'W') {
            spc->spxX2P = 0;
            spc->spxP2X = 0;

         } else if (p == 'A') {
            spc->w[2] *= spx.dwaveawav;
            spc->spxX2P = waveawav;
            spc->spxP2X = awavwave;

         } else if (p == 'V') {
            spc->w[0] = spc->restwav;
            spc->w[2] *= spx.dwavevelo;
            spc->spxX2P = wavevelo;
            spc->spxP2X = velowave;

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
            spc->spxX2P = awavfreq;
            spc->spxP2X = freqawav;

         } else if (p == 'W') {
            spc->w[2] *= spx.dawavwave;
            spc->spxX2P = awavwave;
            spc->spxP2X = waveawav;

         } else if (p == 'A') {
            spc->spxX2P = 0;
            spc->spxP2X = 0;

         } else if (p == 'V') {
            spc->w[0] = spc->restwav;
            spc->w[2] *= spx.dawavvelo;
            spc->spxX2P = awavvelo;
            spc->spxP2X = veloawav;

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

int spcx2s(
   struct spcprm *spc,
   int nx,
   int sx,
   int sspec,
   const double x[],
   double spec[],
   int stat[])

{
   int statP2S, status = 0, statX2P;
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
   if (spc->spxX2P != 0) {
      if (statX2P = spc->spxX2P(spc->w[0], nx, sspec, sspec, spec, spec,
                                stat)) {
         if (statX2P == 4) {
            status = 3;
         } else {
            return statX2P;
         }
      }
   }

   /* Apply the linear step of the algorithm chain to convert intermediate */
   /* spectral coordinate Q to the required spectral coordinate S.         */
   if (spc->spxP2S != 0) {
      if (statP2S = spc->spxP2S(spc->w[0], nx, sspec, sspec, spec, spec,
                                stat)) {
         if (statP2S == 4) {
            status = 3;
         } else {
            return statP2S;
         }
      }
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int spcs2x(
   struct spcprm *spc,
   int nspec,
   int sspec,
   int sx,
   const double spec[],
   double x[],
   int stat[])

{
   int statP2X, status = 0, statS2P;
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
   if (spc->spxS2P != 0) {
      if (statS2P = spc->spxS2P(spc->w[0], nspec, sspec, sx, spec, x, stat)) {
         if (statS2P == 4) {
            status = 4;
         } else {
            return statS2P;
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
   if (spc->spxP2X != 0) {
      if (statP2X = spc->spxP2X(spc->w[0], nspec, sx, sx, x, x, stat)) {
         if (statP2X == 4) {
            status = 4;
         } else {
            return statP2X;
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

/*--------------------------------------------------------------------------*/

int spctrn(
   double restfrq,
   double restwav,
   const char ctypeS1[9],
   double crvalS1,
   double cdeltS1,
   char   ctypeS2[9],
   double *crvalS2,
   double *cdeltS2)

{
   char ptype, xtype;
   int  status;
   double cdeltP, cdeltX, crvalP, crvalX;

   if (status = spcspx(restfrq, restwav, crvalS1, cdeltS1, ctypeS1, 0, 0,
                   &xtype, &ptype, &crvalP, &cdeltP, &crvalX, &cdeltX)) {
      return status;
   }

   if (status = spcxps(restfrq, restwav, crvalX, cdeltX, ctypeS2, 0, 0,
                   &xtype, &ptype, &crvalP, &cdeltP, crvalS2, cdeltS2)) {
      return status;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int spcspx(
   double restfrq,
   double restwav,
   double crvalS,
   double cdeltS,
   const char ctypeS[9],
   char cname[32],
   char units[8],
   char *xtype,
   char *ptype,
   double *crvalP,
   double *cdeltP,
   double *crvalX,
   double *cdeltX)

{
   char type[8];
   int  restreq, status;
   struct spxprm spx;

   /* Check the spectral axis code. */
   if (spchek(ctypeS, cname, units, ptype, xtype, &restreq)) {
      return 2;
   }

   /* Do we have rest frequency and/or wavelength as required? */
   if (restreq && restfrq == 0.0 && restwav == 0.0) {
      return 2;
   }

   /* Compute all spectral parameters and their derivatives. */
   strncpy(type, ctypeS, 4);
   type[4] = '\0';
   if (status = specx(type, crvalS, restfrq, restwav, &spx)) {
      return 2;
   }


   /* Linear transformation from S to intermediate spectral coordinate P. */
   if (strncmp(ctypeS, "FREQ", 4) == 0) {
      *crvalP = spx.freq;
      *cdeltP = cdeltS;
   } else if (strncmp(ctypeS, "AFRQ", 4) == 0) {
      *crvalP = spx.freq;
      *cdeltP = cdeltS * spx.dfreqafrq;
   } else if (strncmp(ctypeS, "ENER", 4) == 0) {
      *crvalP = spx.freq;
      *cdeltP = cdeltS * spx.dfreqener;
   } else if (strncmp(ctypeS, "WAVN", 4) == 0) {
      *crvalP = spx.freq;
      *cdeltP = cdeltS * spx.dfreqwavn;
   } else if (strncmp(ctypeS, "VRAD", 4) == 0) {
      *crvalP = spx.freq;
      *cdeltP = cdeltS * spx.dfreqvrad;
   } else if (strncmp(ctypeS, "WAVE", 4) == 0) {
      *crvalP = spx.wave;
      *cdeltP = cdeltS;
   } else if (strncmp(ctypeS, "VOPT", 4) == 0) {
      *crvalP = spx.wave;
      *cdeltP = cdeltS * spx.dwavevopt;
   } else if (strncmp(ctypeS, "ZOPT", 4) == 0) {
      *crvalP = spx.wave;
      *cdeltP = cdeltS * spx.dwavezopt;
   } else if (strncmp(ctypeS, "AWAV", 4) == 0) {
      *crvalP = spx.awav;
      *cdeltP = cdeltS;
   } else if (strncmp(ctypeS, "VELO", 4) == 0) {
      *crvalP = spx.velo;
      *cdeltP = cdeltS;
   } else if (strncmp(ctypeS, "BETA", 4) == 0) {
      *crvalP = spx.velo;
      *cdeltP = cdeltS * spx.dvelobeta;
   }


   /* Non-linear transformation from P to X. */
   *cdeltX = *cdeltP;
   if (*xtype == 'F') {
      *crvalX = spx.freq;

      if (*ptype == 'W') {
         *cdeltX *= spx.dfreqwave;
      } else if (*ptype == 'A') {
         *cdeltX *= spx.dfreqawav;
      } else if (*ptype == 'V') {
         *cdeltX *= spx.dfreqvelo;
      }

   } else if (*xtype == 'W') {
      *crvalX = spx.wave;

      if (*ptype == 'F') {
         *cdeltX *= spx.dwavefreq;
      } else if (*ptype == 'A') {
         *cdeltX *= spx.dwaveawav;
      } else if (*ptype == 'V') {
         *cdeltX *= spx.dwavevelo;
      }

   } else if (*xtype == 'A') {
      *crvalX = spx.awav;

      if (*ptype == 'F') {
         *cdeltX *= spx.dawavfreq;
      } else if (*ptype == 'W') {
         *cdeltX *= spx.dawavwave;
      } else if (*ptype == 'V') {
         *cdeltX *= spx.dawavvelo;
      }

   } else if (*xtype == 'V') {
      *crvalX = spx.velo;

      if (*ptype == 'F') {
         *cdeltX *= spx.dvelofreq;
      } else if (*ptype == 'W') {
         *cdeltX *= spx.dvelowave;
      } else if (*ptype == 'A') {
         *cdeltX *= spx.dveloawav;
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int spcxps(
   double restfrq,
   double restwav,
   double crvalX,
   double cdeltX,
   const char ctypeS[9],
   char cname[32],
   char units[8],
   char *xtype,
   char *ptype,
   double *crvalP,
   double *cdeltP,
   double *crvalS,
   double *cdeltS)

{
   char type[8];
   int  restreq, status;
   struct spxprm spx;

   /* Check the spectral axis code. */
   if (spchek(ctypeS, cname, units, ptype, xtype, &restreq)) {
      return 2;
   }

   /* Do we have rest frequency and/or wavelength as required? */
   if (restreq && restfrq == 0.0 && restwav == 0.0) {
      return 2;
   }

   /* Compute all spectral parameters and their derivatives. */
   if (*xtype == 'F') {
     strcpy(type, "FREQ");
   } else if (*xtype == 'W') {
     strcpy(type, "WAVE");
   } else if (*xtype == 'A') {
     strcpy(type, "AWAV");
   } else if (*xtype == 'V') {
     strcpy(type, "VELO");
   }

   if (status = specx(type, crvalX, restfrq, restwav, &spx)) {
      return 2;
   }


   /* Non-linear transformation from X to P. */
   *cdeltP = cdeltX;
   if (*ptype == 'F') {
      *crvalP = spx.freq;

      if (*xtype == 'W') {
         *cdeltP *= spx.dfreqwave;
      } else if (*xtype == 'A') {
         *cdeltP *= spx.dfreqawav;
      } else if (*xtype == 'V') {
         *cdeltP *= spx.dfreqvelo;
      }

   } else if (*ptype == 'W') {
      *crvalP = spx.wave;

      if (*xtype == 'F') {
         *cdeltP *= spx.dwavefreq;
      } else if (*xtype == 'A') {
         *cdeltP *= spx.dwaveawav;
      } else if (*xtype == 'V') {
         *cdeltP *= spx.dwavevelo;
      }

   } else if (*ptype == 'A') {
      *crvalP = spx.awav;

      if (*xtype == 'F') {
         *cdeltP *= spx.dawavfreq;
      } else if (*xtype == 'W') {
         *cdeltP *= spx.dawavwave;
      } else if (*xtype == 'V') {
         *cdeltP *= spx.dawavvelo;
      }

   } else if (*ptype == 'V') {
      *crvalP = spx.velo;

      if (*xtype == 'F') {
         *cdeltP *= spx.dvelofreq;
      } else if (*xtype == 'W') {
         *cdeltP *= spx.dvelowave;
      } else if (*xtype == 'A') {
         *cdeltP *= spx.dveloawav;
      }
   }


   /* Linear transformation from intermediate spectral coordinate P to S. */
   if (strncmp(ctypeS, "FREQ", 4) == 0) {
      *crvalS = spx.freq;
      *cdeltS = *cdeltP;
   } else if (strncmp(ctypeS, "AFRQ", 4) == 0) {
      *crvalS = spx.afrq;
      *cdeltS = *cdeltP * spx.dafrqfreq;
   } else if (strncmp(ctypeS, "ENER", 4) == 0) {
      *crvalS = spx.ener;
      *cdeltS = *cdeltP * spx.denerfreq;
   } else if (strncmp(ctypeS, "WAVN", 4) == 0) {
      *crvalS = spx.wavn;
      *cdeltS = *cdeltP * spx.dwavnfreq;
   } else if (strncmp(ctypeS, "VRAD", 4) == 0) {
      *crvalS = spx.vrad;
      *cdeltS = *cdeltP * spx.dvradfreq;
   } else if (strncmp(ctypeS, "WAVE", 4) == 0) {
      *crvalS = spx.wave;
      *cdeltS = *cdeltP;
   } else if (strncmp(ctypeS, "VOPT", 4) == 0) {
      *crvalS = spx.vopt;
      *cdeltS = *cdeltP * spx.dvoptwave;
   } else if (strncmp(ctypeS, "ZOPT", 4) == 0) {
      *crvalS = spx.zopt;
      *cdeltS = *cdeltP * spx.dzoptwave;
   } else if (strncmp(ctypeS, "AWAV", 4) == 0) {
      *crvalS = spx.awav;
      *cdeltS = *cdeltP;
   } else if (strncmp(ctypeS, "VELO", 4) == 0) {
      *crvalS = spx.velo;
      *cdeltS = *cdeltP;
   } else if (strncmp(ctypeS, "BETA", 4) == 0) {
      *crvalS = spx.beta;
      *cdeltS = *cdeltP * spx.dbetavelo;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int spchek(
   const char ctype[9],
   char cname[32],
   char units[8],
   char *ptype,
   char *xtype,
   int  *restreq)

{
   *restreq = 0;

   /* Linear transformation from S to intermediate spectral coordinate P. */
   if (strncmp(ctype, "FREQ", 4) == 0) {
      if (cname) strcpy(cname, "Frequency");
      if (units) strcpy(units, "(Hz)");
      *ptype = 'F';
   } else if (strncmp(ctype, "AFRQ", 4) == 0) {
      if (cname) strcpy(cname, "Angular frequency");
      if (units) strcpy(units, "(deg/s)");
      *ptype = 'F';
   } else if (strncmp(ctype, "ENER", 4) == 0) {
      if (cname) strcpy(cname, "Photon energy");
      if (units) strcpy(units, "(J)");
      *ptype = 'F';
   } else if (strncmp(ctype, "WAVN", 4) == 0) {
      if (cname) strcpy(cname, "Wavenumber");
      if (units) strcpy(units, "(1/m)");
      *ptype = 'F';
   } else if (strncmp(ctype, "VRAD", 4) == 0) {
      if (cname) strcpy(cname, "Radio velocity");
      if (units) strcpy(units, "(m/s)");
      *ptype = 'F';
      *restreq = 1;
   } else if (strncmp(ctype, "WAVE", 4) == 0) {
      if (cname) strcpy(cname, "Vacuum wavelength");
      if (units) strcpy(units, "(m)");
      *ptype = 'W';
   } else if (strncmp(ctype, "VOPT", 4) == 0) {
      if (cname) strcpy(cname, "Optical velocity");
      if (units) strcpy(units, "(m/s)");
      *ptype = 'W';
      *restreq = 1;
   } else if (strncmp(ctype, "ZOPT", 4) == 0) {
      if (cname) strcpy(cname, "Redshift");
      if (units) strcpy(units, "");
      *ptype = 'W';
      *restreq = 1;
   } else if (strncmp(ctype, "AWAV", 4) == 0) {
      if (cname) strcpy(cname, "Air wavelength");
      if (units) strcpy(units, "(m)");
      *ptype = 'A';
   } else if (strncmp(ctype, "VELO", 4) == 0) {
      if (cname) strcpy(cname, "Relativistic velocity");
      if (units) strcpy(units, "(m/s)");
      *ptype = 'V';
   } else if (strncmp(ctype, "BETA", 4) == 0) {
      if (cname) strcpy(cname, "Velocity ratio (v/c)");
      if (units) strcpy(units, "");
      *ptype = 'V';
   } else {
      return 2;
   }


   /* Determine spectral type X for which the axis is linear. */
   if ((*xtype = ctype[5]) == ' ') {
      /* Validate the algorithm code. */
      if (ctype[6] != ' ' || ctype[7] != ' ') {
         return 2;
      }

      *xtype = *ptype;

   } else if (*xtype == 'G') {
      /* Validate the algorithm code. */
      if (ctype[6] != 'R') {
         return 2;
      }

      /* Grism coordinates... */
      if (ctype[7] == 'I') {
         /* ...in vacuum. */
         *xtype = 'W';
      } else if (ctype[7] == 'A') {
         /* ...in air. */
         *xtype = 'A';
      } else {
         return 2;
      }

   } else if (ctype[6] != '2' || ctype[7] != *ptype) {
      /* Validate the algorithm code. */
      return 2;
   }

   /* Non-linear transformation from P to X. */
   if (*xtype == 'F') {
      if (*ptype == 'V') {
         *restreq = 1;
      }
   } else if (*xtype == 'W') {
      if (*ptype == 'V') {
         *restreq = 1;
      }
   } else if (*xtype == 'A') {
      if (*ptype == 'V') {
         *restreq = 1;
      }
   } else if (*xtype == 'V') {
      if (*ptype == 'F') {
         *restreq = 1;
      } else if (*ptype == 'W') {
         *restreq = 1;
      } else if (*ptype == 'A') {
         *restreq = 1;
      }
   } else {
      return 2;
   }

   return 0;
}
