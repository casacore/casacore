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

#include <stdlib.h>
#include <string.h>

#include "tab.h"
#include "wcs.h"
#include "wcshdr.h"
#include "wcsutil.h"

extern const int WCSSET;

/* Map status return value to message. */
const char *wcshdr_errmsg[] = {
   "Success",
   "Null wcsprm pointer passed",
   "Memory allocation failed",
   "Invalid tabular parameters"};

/*--------------------------------------------------------------------------*/

int wcstab(struct wcsprm *wcs)

{
   char (*PSi_0a)[72] = 0x0, (*PSi_1a)[72] = 0x0, (*PSi_2a)[72] = 0x0;
   int  *PVi_1a = 0x0, *PVi_2a = 0x0, *PVi_3a = 0x0, *tabax, *tabidx = 0x0;
   int   getcrd, i, ip, itab, itabax, j, jtabax, m, naxis, ntabax, status;
   struct wtbarr *wtbp;
   struct tabprm *tabp;


   /* Free memory previously allocated by wcstab(). */
   if (wcs == 0x0) return 1;

   if (wcs->flag != -1 && wcs->m_flag == WCSSET) {
      if (wcs->wtb == wcs->m_wtb) wcs->wtb = 0x0;
      if (wcs->tab == wcs->m_tab) wcs->tab = 0x0;

      if (wcs->m_wtb) free(wcs->m_wtb);
      if (wcs->m_tab) {
         for (j = 0; j < wcs->ntab; j++) {
            tabfree(wcs->m_tab + j);
         }

         free(wcs->m_tab);
      }
   }

   wcs->ntab = 0;
   wcs->nwtb = 0;
   wcs->wtb  = 0x0;
   wcs->tab  = 0x0;


   /* Determine the number of -TAB axes. */
   naxis = wcs->naxis;
   if (!(tabax = calloc(naxis, sizeof(int)))) {
      return 2;
   }

   ntabax = 0;
   for (i = 0; i < naxis; i++) {
      /* Null fill. */
      wcsutil_null_fill(72, wcs->ctype[i]);

      if (!strcmp(wcs->ctype[i]+4, "-TAB")) {
         tabax[i] = ntabax++;
      } else {
         tabax[i] = -1;
      }
   }

   if (ntabax == 0) {
      /* No lookup tables. */
      status = 0;
      goto cleanup;
   }


   /* Collect information from the PSi_ma and PVi_ma cards. */
   if (!((PSi_0a = calloc(ntabax, sizeof(char[72]))) &&
         (PVi_1a = calloc(ntabax, sizeof(int)))      &&
         (PVi_2a = calloc(ntabax, sizeof(int)))      &&
         (PSi_1a = calloc(ntabax, sizeof(char[72]))) &&
         (PSi_2a = calloc(ntabax, sizeof(char[72]))) &&
         (PVi_3a = calloc(ntabax, sizeof(int)))      &&
         (tabidx = calloc(ntabax, sizeof(int))))) {
      status = 2;
      goto cleanup;
   }

   for (itabax = 0; itabax < ntabax; itabax++) {
      /* Remember that calloc() zeroes allocated memory. */
      PVi_1a[itabax] = 1;
      PVi_2a[itabax] = 1;
      PVi_3a[itabax] = 1;
   }

   for (ip = 0; ip < wcs->nps; ip++) {
      itabax = tabax[wcs->ps[ip].i - 1];
      if (itabax >= 0) {
         switch (wcs->ps[ip].m) {
         case 0:
            /* EXTNAME. */
            strcpy(PSi_0a[itabax], wcs->ps[ip].value);
            wcsutil_null_fill(72, PSi_0a[itabax]);
            break;
         case 1:
            /* TTYPEn for coordinate array. */
            strcpy(PSi_1a[itabax], wcs->ps[ip].value);
            wcsutil_null_fill(72, PSi_1a[itabax]);
            break;
         case 2:
            /* TTYPEn for index vector. */
            strcpy(PSi_2a[itabax], wcs->ps[ip].value);
            wcsutil_null_fill(72, PSi_2a[itabax]);
            break;
         }
      }
   }

   for (ip = 0; ip < wcs->npv; ip++) {
      itabax = tabax[wcs->pv[ip].i - 1];
      if (itabax >= 0) {
         switch (wcs->pv[ip].m) {
         case 1:
            /* EXTVER. */
            PVi_1a[itabax] = (int)(wcs->pv[ip].value + 0.5);
            break;
         case 2:
            /* EXTLEVEL. */
            PVi_2a[itabax] = (int)(wcs->pv[ip].value + 0.5);
            break;
         case 3:
            /* Table axis number. */
            PVi_3a[itabax] = (int)(wcs->pv[ip].value + 0.5);
            break;
         }
      }
   }


   /* Determine the number of independent tables. */
   for (itabax = 0; itabax < ntabax; itabax++) {
      /* These have no defaults. */
      if (!PSi_0a[itabax][0] || !PSi_1a[itabax][0]) {
         status = 3;
         goto cleanup;
      }

      tabidx[itabax] = -1;
      for (jtabax = 0; jtabax < i; jtabax++) {
         /* EXTNAME, EXTVER, EXTLEVEL, and TTYPEn for the coordinate array */
         /* must match for each axis of a multi-dimensional lookup table.  */
         if (strcmp(PSi_0a[itabax], PSi_0a[jtabax]) == 0 &&
             strcmp(PSi_1a[itabax], PSi_1a[jtabax]) == 0 &&
             PVi_1a[itabax] == PVi_1a[jtabax] &&
             PVi_2a[itabax] == PVi_2a[jtabax]) {
            tabidx[itabax] = tabidx[jtabax];
            break;
         }
      }

      if (jtabax == itabax) {
         tabidx[itabax] = wcs->ntab;
         wcs->ntab++;
      }
   }

   if (!(wcs->tab = calloc(wcs->ntab, sizeof(struct tabprm)))) {
      status = 2;
      goto cleanup;
   }
   wcs->m_tab = wcs->tab;

   /* Table dimensionality; find the largest axis number. */
   for (itabax = 0; itabax < ntabax; itabax++) {
      tabp = wcs->tab + tabidx[itabax];

      /* PVi_3a records the 1-relative table axis number. */
      if (PVi_3a[itabax] > tabp->M) {
         tabp->M = PVi_3a[itabax];
      }
   }

   for (itab = 0; itab < wcs->ntab; itab++) {
      if (status = tabini(1, wcs->tab[itab].M, 0, wcs->tab + itab)) {
         goto cleanup;
      }
   }


   /* Copy parameters into the tabprm structs. */
   for (i = 0; i < naxis; i++) {
      if ((itabax = tabax[i]) < 0) {
         /* Not a -TAB axis. */
         continue;
      }

      /* PVi_3a records the 1-relative table axis number. */
      m = PVi_3a[itabax] - 1;

      tabp = wcs->tab + tabidx[itabax];
      tabp->map[m] = i;
      tabp->crval[m] = wcs->crval[i];
   }

   /* Check for completeness. */
   for (itab = 0; itab < wcs->ntab; itab++) {
      for (m = 0; m < wcs->tab[itab].M; m++) {
         if (wcs->tab[itab].map[m] < 0) {
            status = 3;
            goto cleanup;
         }
      }
   }


   /* Set up for reading the arrays; how many arrays are there? */
   for (itabax = 0; itabax < ntabax; itabax++) {
      /* Does this -TAB axis have a non-degenerate index array? */
      if (PSi_2a[itabax][0]) {
         wcs->nwtb++;
      }
   }

   /* Add one coordinate array for each table. */
   wcs->nwtb += wcs->ntab;

   /* Allocate memory for structs to be returned. */
   if (!(wcs->wtb = calloc(wcs->nwtb, sizeof(struct wtbarr)))) {
      wcs->nwtb = 0;

      status = 2;
      goto cleanup;
   }
   wcs->m_wtb = wcs->wtb;

   /* Set pointers for the index and coordinate arrays. */
   wtbp = wcs->wtb;
   for (itab = 0; itab < wcs->ntab; itab++) {
      getcrd = 1;
      for (itabax = 0; itabax < ntabax; itabax++) {
         if (tabidx[itabax] != itab) continue;

         if (getcrd) {
            /* Coordinate array. */
            wtbp->i = itabax + 1;
            wtbp->m = PVi_3a[itabax];
            wtbp->kind = 'c';

            strcpy(wtbp->extnam, PSi_0a[itabax]);
            wtbp->extver = PVi_1a[itabax];
            wtbp->extlev = PVi_2a[itabax];
            strcpy(wtbp->ttype, PSi_1a[itabax]);
            wtbp->row    = 1L;
            wtbp->ndim   = wcs->tab[itab].M + 1;
            wtbp->dimlen = wcs->tab[itab].K;
            wtbp->arrayp = &(wcs->tab[itab].coord);

            /* Signal for tabset() to take this memory. */
            wcs->tab[itab].m_coord = (double *)0x1;

            wtbp++;
            getcrd = 0;
         }

         if (PSi_2a[itabax][0]) {
            /* Index array. */
            wtbp->i = itabax + 1;
            wtbp->m = PVi_3a[itabax];
            wtbp->kind = 'i';

            m = wtbp->m - 1;
            strcpy(wtbp->extnam, PSi_0a[itabax]);
            wtbp->extver = PVi_1a[itabax];
            wtbp->extlev = PVi_2a[itabax];
            strcpy(wtbp->ttype, PSi_2a[itabax]);
            wtbp->row    = 1L;
            wtbp->ndim   = 1;
            wtbp->dimlen = wcs->tab[itab].K + m;
            wtbp->arrayp = wcs->tab[itab].index + m;

            /* Signal for tabset() to take this memory. */
            wcs->tab[itab].m_indxs[m] = (double *)0x1;

            wtbp++;
         }
      }
   }

   status = 0;

cleanup:
   if (tabax)  free(tabax);
   if (tabidx) free(tabidx);
   if (PSi_0a) free(PSi_0a);
   if (PVi_1a) free(PVi_1a);
   if (PVi_2a) free(PVi_2a);
   if (PSi_1a) free(PSi_1a);
   if (PSi_2a) free(PSi_2a);
   if (PVi_3a) free(PVi_3a);

   if (status) {
      if (wcs->tab) free(wcs->tab);
      if (wcs->wtb) free(wcs->wtb);
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int wcsidx(int nwcs, struct wcsprm **wcs, int alts[27])

{
   int a, i;
   struct wcsprm *wcsp;

   for (a = 0; a < 27; a++) {
      alts[a] = -1;
   }

   if (wcs == 0x0) {
      return 1;
   }

   wcsp = *wcs;
   for (i = 0; i < nwcs; i++, wcsp++) {
      if (wcsp->alt[0] == ' ') {
         a = 0;
      } else {
         a = wcsp->alt[0] - 'A' + 1;
      }

      alts[a] = i;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsvfree(int *nwcs, struct wcsprm **wcs)

{
   int a, status = 0;
   struct wcsprm *wcsp;

   if (wcs == 0x0) {
      return 1;
   }

   wcsp = *wcs;
   for (a = 0; a < *nwcs; a++, wcsp++) {
      status |= wcsfree(wcsp);
   }

   free(*wcs);

   *nwcs = 0;
   *wcs = 0x0;

   return status;
}
