/*============================================================================
*
*   WCSLIB 4.0 - an implementation of the FITS WCS standard.
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

#include "wcsmath.h"
#include "tab.h"

const int TABSET = 137;

/* Map status return value to message. */
const char *tab_errmsg[] = {
   0,
   "Null tabprm pointer passed",
   "Memory allocation failed",
   "Invalid tabular parameters",
   "One or more of the x coordinates were invalid",
   "One or more of the world coordinates were invalid"};

/*--------------------------------------------------------------------------*/

int tabini(alloc, M, K, tab)

int alloc, M;
const int K[];
struct tabprm *tab;

{
   int k, l, m, N;
   double *dp;

   if (tab == 0) return 1;

   if (M <= 0 || K == 0) {
      return 3;
   }

   /* Determine the total number of elements in the coordinate array. */
   N = M;
   for (m = 0; m < M; m++) {
      N *= K[m];
   }

   if (N <= 0) {
      return 3;
   }


   /* Initialize memory management. */
   if (tab->flag == -1 || tab->m_flag != TABSET) {
      tab->m_flag  = 0;
      tab->m_M     = 0;
      tab->m_N     = 0;
      tab->m_K     = 0;
      tab->m_map   = 0;
      tab->m_crval = 0;
      tab->m_index = 0;
      tab->m_coord = 0;
   }

   /* Allocate memory for arrays if required. */
   if (alloc ||
       tab->K == 0 ||
       tab->map == 0 ||
       tab->crval == 0 ||
       tab->index == 0 ||
       tab->coord == 0) {

      /* Was sufficient allocated previously? */
      if (tab->m_flag == TABSET && (tab->m_M < M || tab->m_N < N)) {
         /* No, free it. */
         tabfree(tab);
      }

      if (alloc || tab->K == 0) {
         if (tab->m_K) {
            /* In case the caller fiddled with it. */
            tab->K = tab->m_K;

         } else {
            if (!(tab->K = calloc(M, sizeof(int)))) {
               return 2;
            }

            tab->m_flag = TABSET;
            tab->m_M = M;
            tab->m_N = N;
            tab->m_K = tab->K;
         }
      }

      if (alloc || tab->map == 0) {
         if (tab->m_map) {
            /* In case the caller fiddled with it. */
            tab->map = tab->m_map;

         } else {
            if (!(tab->map = calloc(M, sizeof(int)))) {
               return 2;
            }

            tab->m_flag = TABSET;
            tab->m_M = M;
            tab->m_N = N;
            tab->m_map = tab->map;
         }
      }

      if (alloc || tab->crval == 0) {
         if (tab->m_crval) {
            /* In case the caller fiddled with it. */
            tab->crval = tab->m_crval;

         } else {
            if (!(tab->crval = calloc(M, sizeof(double)))) {
               return 2;
            }

            tab->m_flag = TABSET;
            tab->m_M = M;
            tab->m_N = N;
            tab->m_crval = tab->crval;
         }
      }

      if (alloc || tab->index == 0) {
         if (tab->m_index) {
            /* In case the caller fiddled with it. */
            tab->index = tab->m_index;

         } else {
            if (!(tab->index = calloc(M, sizeof(double *)))) {
               return 2;
            }

            l = 0;
            for (m = 0; m < M; m++) {
               l += K[m];
            }

            if (!(tab->index[0] = calloc(l, sizeof(double)))) {
               return 2;
            }

            for (m = 0; m < M; m++) {
               if (m) {
                  tab->index[m] = tab->index[m-1] + K[m-1];
               }
            }

            tab->m_flag = TABSET;
            tab->m_M = M;
            tab->m_N = N;
            tab->m_index = tab->index;
         }
      }

      if (alloc || tab->coord == 0) {
         if (tab->m_coord) {
            /* In case the caller fiddled with it. */
            tab->coord = tab->m_coord;

         } else {
            if (!(tab->coord = calloc(N, sizeof(double)))) {
               return 2;
            }

            tab->m_flag = TABSET;
            tab->m_M = M;
            tab->m_N = N;
            tab->m_coord = tab->coord;
         }
      }
   }

   /* Free memory allocated by tabset(). */
   if (tab->flag == TABSET) {
      if (tab->sense)   free(tab->sense);
      if (tab->p0)      free(tab->p0);
      if (tab->delta)   free(tab->delta);
      if (tab->extrema) free(tab->extrema);
   }

   tab->sense   = 0;
   tab->p0      = 0;
   tab->delta   = 0;
   tab->extrema = 0;
   tab->set_M   = 0;

   tab->flag = 0;
   tab->M = M;

   /* Copy K[] and initialize map[] and crval[]. */
   for (m = 0; m < M; m++) {
      tab->K[m] = K[m];
      tab->map[m] = -1;
      tab->crval[m] = 0.0;
   }

   /* Initialize the index vectors. */
   for (m = 0; m < M; m++) {
      dp = tab->index[m];
      for (k = 0; k < K[m]; k++) {
         *(dp++) = k;
      }
   }

   /* Initialize the coordinate array. */
   for (dp = tab->coord; dp < tab->coord + N; dp++) {
      *dp = UNDEFINED;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabcpy(alloc, tabsrc, tabdst)

int alloc;
const struct tabprm *tabsrc;
struct tabprm *tabdst;

{
   int k, m, M, n, N, status;
   double *dstp, *srcp;

   if (tabsrc == 0) return 1;

   M = tabsrc->M;
   if (M <= 0) {
      return 2;
   }

   if (status = tabini(alloc, M, tabsrc->K, tabdst)) {
      return status;
   }

   N = M;
   for (m = 0; m < M; m++) {
      tabdst->map[m]   = tabsrc->map[m];
      tabdst->crval[m] = tabsrc->crval[m];
      N *= tabsrc->K[m];
   }

   for (m = 0; m < M; m++) {
      srcp = tabsrc->index[m];
      dstp = tabdst->index[m];

      for (k = 0; k < tabsrc->K[m]; k++) {
         *(dstp++) = *(srcp++);
      }
   }

   srcp = tabsrc->coord;
   dstp = tabdst->coord;
   for (n = 0; n < N; n++) {
      *(dstp++) = *(srcp++);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabfree(tab)

struct tabprm *tab;

{
   if (tab == 0) return 1;

   if (tab->flag != -1) {
      /* Free memory allocated by tabini(). */
      if (tab->m_flag == TABSET) {
         if (tab->K     == tab->m_K)     tab->K = 0;
         if (tab->map   == tab->m_map)   tab->map = 0;
         if (tab->crval == tab->m_crval) tab->crval = 0;
         if (tab->index == tab->m_index) tab->index = 0;
         if (tab->coord == tab->m_coord) tab->coord = 0;

         if (tab->m_K)     free(tab->m_K);
         if (tab->m_map)   free(tab->m_map);
         if (tab->m_crval) free(tab->m_crval);
         if (tab->m_index) {
            free(tab->m_index[0]);
            free(tab->m_index);
         }
         if (tab->m_coord) free(tab->m_coord);
      }
   }

   tab->m_flag  = 0;
   tab->m_M     = 0;
   tab->m_N     = 0;
   tab->m_K     = 0;
   tab->m_map   = 0;
   tab->m_crval = 0;
   tab->m_index = 0;
   tab->m_coord = 0;


   /* Free memory allocated by tabset(). */
   if (tab->flag == TABSET) {
      if (tab->sense)   free(tab->sense);
      if (tab->p0)      free(tab->p0);
      if (tab->delta)   free(tab->delta);
      if (tab->extrema) free(tab->extrema);
   }

   tab->sense   = 0;
   tab->p0      = 0;
   tab->delta   = 0;
   tab->extrema = 0;
   tab->set_M   = 0;

   tab->flag = 0;

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabprt(tab)

const struct tabprm *tab;

{
   int k, m, n, N;
   double *dp;

   if (tab == 0) return 1;

   if (tab->flag != TABSET) {
      printf("The tabprm struct is UNINITIALIZED.\n");
      return 0;
   }

   printf("       flag: %d\n", tab->flag);
   printf("          M: %d\n", tab->M);

   /* Array dimensions. */
   printf("          K: 0x%x\n", (int)tab->K);
   printf("            ");
   for (m = 0; m < tab->M; m++) {
      printf("%6d", tab->K[m]);
   }
   printf("\n");

   /* Map vector. */
   printf("        map: 0x%x\n", (int)tab->map);
   printf("            ");
   for (m = 0; m < tab->M; m++) {
      printf("%6d", tab->map[m]);
   }
   printf("\n");

   /* Reference index value. */
   printf("      crval: 0x%x\n", (int)tab->crval);
   printf("            ");
   for (m = 0; m < tab->M; m++) {
      printf("  %- 11.4g", tab->crval[m]);
   }
   printf("\n");

   /* Index vectors. */
   printf("      index: 0x%x\n", (int)tab->index);
   for (m = 0; m < tab->M; m++) {
      printf("   index[%d]: 0x%x", m, (int)tab->index[m]);
      for (k = 0; k < tab->K[m]; k++) {
         if (k%5 == 0) {
            printf("\n            ");
         }
         printf("  %- 11.4g", tab->index[m][k]);
      }
   }

   /* Coordinate array. */
   N = tab->M * tab->nc;

   printf("      coord: 0x%x\n", (int)tab->coord);
   printf("            ");
   dp = tab->coord;
   for (n = 0; n < N;) {
/* Should print the array index. */
      printf("             (%d)", n);
      for (m = 0; m < tab->M; m++, n++) {
         printf("  %- 11.4g", *(dp++));
      }
      printf("\n");
   }
   printf("\n");

   printf("         nc: %d\n", tab->nc);

   if (tab->sense == 0) {
      printf("      sense: (null)\n");
   } else {
      printf("      sense: 0x%x\n", (int)tab->sense);
      printf("            ");
      for (m = 0; m < tab->M; m++) {
         printf("%6d", tab->sense[m]);
      }
      printf("\n");
   }

   if (tab->p0 == 0) {
      printf("         p0: (null)\n");
   } else {
      printf("         p0: 0x%x\n", (int)tab->p0);
      printf("            ");
      for (m = 0; m < tab->M; m++) {
         printf("%6d", tab->p0[m]);
      }
      printf("\n");
   }

   if (tab->delta == 0) {
      printf("      delta: (null)\n");
   } else {
      printf("      delta: 0x%x\n", (int)tab->delta);
      printf("            ");
      for (m = 0; m < tab->M; m++) {
         printf("  %- 11.4g", tab->delta[m]);
      }
      printf("\n");
   }

   N *= 2/tab->K[0];
   printf("    extrema: 0x%x\n", (int)tab->extrema);
   printf("            ");
   dp = tab->extrema;
   for (n = 0; n < N;) {
/* Should print the array index. */
      printf("             (%d)", n);
      for (m = 0; m < tab->M; m++, n++) {
         printf("  %- 11.4g", *(dp++));
      }
      printf("\n");
   }
   printf("\n");

   /* Memory management. */
   printf("     m_flag: %d\n", tab->m_flag);
   printf("        m_M: %d\n", tab->m_M);
   printf("        m_N: %d\n", tab->m_N);
   printf("        m_K: 0x%x", (int)tab->m_K);
   if (tab->m_K == tab->K) printf("  (= K)");
   printf("\n");
   printf("      m_map: 0x%x", (int)tab->m_map);
   if (tab->m_map == tab->map) printf("  (= map)");
   printf("\n");
   printf("    m_crval: 0x%x", (int)tab->m_crval);
   if (tab->m_crval == tab->crval) printf("  (= crval)");
   printf("\n");
   printf("    m_index: 0x%x", (int)tab->m_index);
   if (tab->m_index == tab->index) printf("  (= index)");
   printf("\n");
   printf("    m_coord: 0x%x", (int)tab->m_coord);
   if (tab->m_coord == tab->coord) printf("  (= coord)");
   printf("\n");

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabset(tab)

struct tabprm *tab;

{
   int i, ic, k, *Km, m, M, ne;
   double *dcrd, *dmax, *dmin, *Psi;

   if (tab == 0) return 1;

   M = tab->M;

   /* Check the number of tabular coordinate axes. */
   if (M < 1) {
      return 3;
   }

   /* Check the axis lengths. */
   tab->nc = 1;
   for (m = 0; m < M; m++) {
      if (tab->K[m] < 1) {
         return 3;
      }

      /* Number of coordinate vectors in the coordinate array. */
      tab->nc *= tab->K[m];
   }

   /* Check that the map vector is sensible. */
   for (m = 0; m < M; m++) {
      i = tab->map[m];
      if (i < 0) {
         return 3;
      }
   }

   /* Allocate memory for work vectors. */
   if (tab->flag != TABSET || tab->set_M < M) {
      if (tab->flag == TABSET) {
         /* Free memory that may have been allocated previously. */
         if (tab->sense)   free(tab->sense);
         if (tab->p0)      free(tab->p0);
         if (tab->delta)   free(tab->delta);
         if (tab->extrema) free(tab->extrema);
      }

      /* Allocate memory for internal arrays. */
      if (!(tab->sense = calloc(M, sizeof(int)))) {
         return 2;
      }

      if (!(tab->p0 = calloc(M, sizeof(int)))) {
         free(tab->sense);
         return 2;
      }

      if (!(tab->delta = calloc(M, sizeof(double)))) {
         free(tab->sense);
         free(tab->p0);
         return 2;
      }

      ne = M * tab->nc * 2 / tab->K[0];
      if (!(tab->extrema = calloc(ne, sizeof(double)))) {
         free(tab->sense);
         free(tab->p0);
         free(tab->delta);
         return 2;
      }

      tab->set_M = M;
   }

   /* Check that the index vectors are monotonic. */
   Km = tab->K;
   for (m = 0; m < M; m++, Km++) {
      tab->sense[m] = 0;

      if (*Km > 1) {
         Psi = tab->index[m];

         for (k = 0; k < *Km-1; k++) {
            if (tab->sense[m] == 0) {
               if (Psi[k] < Psi[k+1]) {
                  /* Monotonic increasing. */
                  tab->sense[m] = 1;
               } else if (Psi[k] > Psi[k+1]) {
                  /* Monotonic decreasing. */
                  tab->sense[m] = -1;
               }
            }

            if (tab->sense[m] == 1) {
               if (Psi[k] > Psi[k+1]) {
                  /* Should be monotonic increasing. */
                  free(tab->sense);
                  free(tab->p0);
                  free(tab->delta);
                  free(tab->extrema);
                  return 3;
               }
            } else if (tab->sense[m] == -1) {
               if (Psi[k] < Psi[k+1]) {
                  /* Should be monotonic decreasing. */
                  free(tab->sense);
                  free(tab->p0);
                  free(tab->delta);
                  free(tab->extrema);
                  return 3;
               }
            }
         }

         if (tab->sense[m] == 0) {
            free(tab->sense);
            free(tab->p0);
            free(tab->delta);
            free(tab->extrema);
            return 3;
         }
      }
   }

   /* Deduce the extremal values of the coordinate elements in each row. */
   dcrd = tab->coord;
   dmin = tab->extrema;
   dmax = tab->extrema + M;
   for (ic = 0; ic < tab->nc; ic += tab->K[0]) {
      for (m = 0; m < M; m++, dcrd++) {
         *(dmax+m) = *(dmin+m) = *dcrd;
      }

      for (i = 1; i < tab->K[0]; i++) {
         for (m = 0; m < M; m++, dcrd++) {
            if (*(dmax+m) < *dcrd) *(dmax+m) = *dcrd;
            if (*(dmin+m) > *dcrd) *(dmin+m) = *dcrd;
         }
      }

      dmin += 2*M;
      dmax += 2*M;
   }

   tab->flag = TABSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabx2s(tab, ncoord, nelem, x, world, stat)

struct tabprm *tab;
int ncoord, nelem;
const double x[];
double world[];
int stat[];

{
   int i, iv, k, *Km, m, M, n, nv, offset, p0, status;
   double *coord, *Psi, psi_m, upsilon, wgt;
   register int *statp;
   register const double *xp;
   register double *wp;

   /* Initialize if required. */
   if (tab == 0) return 1;
   if (tab->flag != TABSET) {
      if (status = tabset(tab)) return status;
   }

   /* This is used a lot. */
   M = tab->M;

   status = 0;
   xp = x;
   wp = world;
   statp = stat;
   for (n = 0; n < ncoord; n++) {
      /* Determine the indexes. */
      Km  = tab->K;
      for (m = 0; m < M; m++, Km++) {
         i = tab->map[m];
         psi_m = *(xp+i) + tab->crval[i];

         Psi = tab->index[m];
         if (*Km == 1) {
            /* Index vector is degenerate. */
            if (psi_m == Psi[0]) {
               upsilon = Psi[0];
            } else {
               *statp = 1;
               status = 4;
               goto next;
            }

         } else {
            /* Interpolate in the indexing vector. */
            if (tab->sense[m] == 1) {
               /* Monotonic increasing index values. */
               if (psi_m < Psi[0] || psi_m > Psi[*Km-1]) {
                  /* Index is out of range. */
                  *statp = 1;
                  status = 4;
                  goto next;
               }

               for (k = 0; k < *Km-1; k++) {
                  if (psi_m < Psi[k]) {
                     continue;
                  }
                  if (Psi[k] == psi_m && psi_m < Psi[k+1]) {
                     break;
                  }
                  if (Psi[k] < psi_m && psi_m <= Psi[k+1]) {
                     break;
                  }
               }

            } else {
               /* Monotonic decreasing index values. */
               if (psi_m > Psi[0] || psi_m < Psi[*Km-1]) {
                  /* Index is out of range. */
                  *statp = 1;
                  status = 4;
                  goto next;
               }

               for (k = 0; k < *Km-1; k++) {
                  if (psi_m > Psi[k]) {
                     continue;
                  }
                  if (Psi[k] == psi_m && psi_m > Psi[k+1]) {
                     break;
                  }
                  if (Psi[k] > psi_m && psi_m >= Psi[k+1]) {
                     break;
                  }
               }
            }

            upsilon = k + (psi_m - Psi[k]) / (Psi[k+1] - Psi[k]);
         }

         if (upsilon < 0.0 || upsilon > (double)(*Km-1)) {
            /* Index out of range (shouldn't be possible). */
            *statp = 1;
            status = 4;
            goto next;
         }

         /* Fiducial array indices and fractional offset. */
         p0 = (int)floor(upsilon);
         tab->p0[m] = p0;
         tab->delta[m] = upsilon - p0;

         if (p0 == *Km-1 && *Km > 1) {
           tab->p0[m] -= 1;
           tab->delta[m] = 1.0;
         }
      }


      /* Now interpolate in the coordinate array; the M-dimensional linear  */
      /* interpolation algorithm is described in Sect. 3.4 of WCS Paper IV. */
      for (m = 0; m < M; m++) {
         i = tab->map[m];
         *(wp+i) = 0.0;
      }

      /* Loop over the 2^M vertices surrounding P. */
      nv = 1 << M;
      for (iv = 0; iv < nv; iv++) {
         /* Locate vertex in the coordinate array and compute its weight. */
         offset = 0;
         wgt = 1.0;
         for (m = M-1; m >= 0; m--) {
            offset *= tab->K[m];
            offset += tab->p0[m];
            if (iv & (1 << m)) {
               if (tab->K[m] > 1) offset++;
               wgt *= tab->delta[m];
            } else {
               wgt *= 1.0 - tab->delta[m];
            }
         }

         if (wgt == 0.0) continue;

         /* Add the contribution from this vertex to each element. */
         coord = tab->coord + offset*M;
         for (m = 0; m < M; m++) {
            i = tab->map[m];
            *(wp+i) += *(coord++) * wgt;
         }

         if (wgt == 1.0) break;
      }

      *statp = 0;

next:
      xp += nelem;
      wp += nelem;
      statp++;
   }

   return status;
}

/*--------------------------------------------------------------------------*/

int tabs2x(tab, ncoord, nelem, world, x, stat)

struct tabprm* tab;
int ncoord, nelem;
const double world[];
double x[];
int stat[];

{
   int tabedge(struct tabprm *);
   int tabrow(struct tabprm *, const double *);
   int tabvox(struct tabprm *, const double *, int, unsigned int *);

   int edge, i, ic, k, *Km, M, m, n, status;
   double *Psi, psi_m, upsilon;
   register int *statp;
   register const double *wp;
   register double *xp;

   /* Initialize if required. */
   if (tab == 0) return 1;
   if (tab->flag != TABSET) {
      if (status = tabset(tab)) return status;
   }

   /* This is used a lot. */
   M = tab->M;


   status = 0;
   wp = world;
   xp = x;
   statp = stat;
   for (n = 0; n < ncoord; n++) {
      /* Locate this coordinate in the coordinate array. */
      edge = 0;
      for (m = 0; m < M; m++) {
         tab->p0[m] = 0;
      }

      for (ic = 0; ic < tab->nc; ic++) {
         if (tab->p0[0] == 0) {
            /* New row, could it contain a solution? */
            if (edge || tabrow(tab, wp)) {
               /* No, skip it. */
               ic += tab->K[0] - 1;
               tab->p0[1]++;
               edge = tabedge(tab);
               continue;
            }
         }

         if (M == 1) {
            /* Deal with the one-dimensional case separately. */
            if (*wp == tab->coord[0]) {
               tab->p0[0] = 0;
               tab->delta[0] = 0.0;
               break;

            } else if (ic < tab->nc - 1) {
               if (((tab->coord[ic] <= *wp && *wp <= tab->coord[ic+1]) ||
                    (tab->coord[ic] >= *wp && *wp >= tab->coord[ic+1])) &&
                     tab->index[0][ic] != tab->index[0][ic+1]) {
                  tab->p0[0] = ic;
                  tab->delta[0] = (*wp - tab->coord[ic]) /
                                  (tab->coord[ic+1] - tab->coord[ic]);
                  break;
               }
            }

         } else {
            /* Multi-dimensional tables are much harder. */
            if (!edge && tabvox(tab, wp, 0, 0) == 0) {
               /* Found a solution. */
               break;
            }

            /* Next voxel. */
            tab->p0[0]++;
            edge = tabedge(tab);
         }
      }


      if (ic == tab->nc) {
         /* Coordinate not found. */
         *statp = 1;
         status = 5;
      } else {
         /* Determine the intermediate world coordinates. */
         Km = tab->K;
         for (m = 0; m < M; m++, Km++) {
            upsilon = tab->p0[m] + tab->delta[m];
            if (upsilon < 0.0 || upsilon > (double)(*Km-1)) {
               /* Index out of range. */
               *statp = 1;
               status = 5;

            } else {
               /* Do inverse lookup of the index vector. */
               Psi = tab->index[m];
               if (*Km == 1) {
                  /* Degenerate index vector. */
                  psi_m = Psi[0];
               } else {
                  k = (int)upsilon;
                  psi_m = Psi[k] + (upsilon - k) * (Psi[k+1] - Psi[k]);
               }

               i = tab->map[m];
               xp[i] = psi_m - tab->crval[i];
            }
         }
         *statp = 0;
      }

      wp += nelem;
      xp += nelem;
      statp++;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int tabedge(tab)

struct tabprm* tab;

{
   int edge, *Km, m;

   edge = 0;
   Km = tab->K;
   for (m = 0; m < tab->M; m++, Km++) {
      if (tab->p0[m] == *Km) {
         tab->p0[m] = 0;
         tab->p0[m+1]++;
      } else if (tab->p0[m] == *Km - 1 && *Km > 1) {
         /* Edge effects? */
         edge = 1;
      }
   }

   return edge;
}

/*--------------------------------------------------------------------------*/

int tabrow(tab, wp)

struct tabprm* tab;
const double *wp;

{
   int iv, M, m, nv, offset;
   unsigned int eq, et, gt, lt;
   const double tol = 1.0e-10;
   double *cp, w;

   M = tab->M;

   /* Could the coordinate lie somewhere along this row of voxels? */
   lt = 0;
   gt = 0;
   eq = 0;
   nv = 1 << M;
   for (iv = 0; iv < nv; iv++) {
      /* Find the extrema at this end of the row. */
      offset = 0;
      for (m = M-1; m > 0; m--) {
         offset *= tab->K[m];
         offset += tab->p0[m];
         if (iv & (1 << m)) {
            if (tab->K[m] > 1) offset++;
         }
      }

      offset *= 2;
      offset += tab->p0[0];
      if (iv & 1 && tab->K[0] > 1) offset++;

      et = 0;
      cp = tab->extrema + offset*M;
      for (m = 0; m < M; m++, cp++) {
         w = wp[tab->map[m]];
         if (fabs(*cp - w) < tol) {
            et |= (1 << m);
         } else if (*cp < w) {
            lt |= (1 << m);
         } else if (*cp > w) {
            gt |= (1 << m);
         }
      }

      if (et == nv-1) {
         /* Solution found in a corner. */
         return 0;
      }

      eq |= et;
   }

   if ((lt | eq) == nv-1 && (gt | eq) == nv-1) {
      /* A solution could lie within this row of voxels. */
      return 0;
   }

   /* No solution in this row. */
   return 1;
}

/*--------------------------------------------------------------------------*/

int tabvox(tab, wp, level, vox)

struct tabprm* tab;
const double *wp;
int level;
unsigned int *vox;

{
   int i, iv, jv, M, m, nv, offset;
   unsigned int eq, et, gt, lt, vox2[16];
   const double tol = 1.0e-10;
   double coord[16], *cp, dv, w, wgt;

   M = tab->M;

   dv = 1.0;
   for (i = 0; i < level; i++) {
      dv /= 2.0;
   }

   /* Could the coordinate lie within this voxel? */
   lt = 0;
   gt = 0;
   eq = 0;
   nv = 1 << M;
   for (iv = 0; iv < nv; iv++) {
      /* Compute the coordinates of this corner of the voxel. */
      for (m = 0; m < M; m++) {
         coord[m] = 0.0;
         tab->delta[m] = level ? dv*vox[m] : 0.0;
         if (iv & (1 << m)) {
            tab->delta[m] += dv;
         }
      }

      for (jv = 0; jv < nv; jv++) {
         offset = 0;
         wgt = 1.0;
         for (m = M-1; m >= 0; m--) {
            offset *= tab->K[m];
            offset += tab->p0[m];
            if (jv & (1 << m)) {
               if (tab->K[m] > 1) offset++;
               wgt *= tab->delta[m];
            } else {
               wgt *= 1.0 - tab->delta[m];
            }
         }

         if (wgt == 0.0) continue;

         cp = tab->coord + offset*M;
         for (m = 0; m < M; m++) {
            coord[m] += *(cp++) * wgt;
         }

         if (wgt == 1.0) break;
      }

      /* Coordinate elements are minimal or maximal in a corner. */
      et = 0;
      for (m = 0; m < M; m++) {
         w = wp[tab->map[m]];
         if (fabs(coord[m] - w) < tol) {
            et |= (1 << m);
         } else if (coord[m] < w) {
            lt |= (1 << m);
         } else if (coord[m] > w) {
            gt |= (1 << m);
         }
      }

      if (et == nv-1) {
         /* Solution found in a corner. */
         return 0;
      }

      eq |= et;
   }

   if ((lt | eq) == nv-1 && (gt | eq) == nv-1) {
      /* The coordinate could lie within this voxel, but does it? */

      if (level == 31) {
         /* Stop the recursion. */
         dv /= 2.0;
         for (m = 0; m < M; m++) {
            tab->delta[m] = dv * (2.0*vox[m] + 1.0);
         }

         return 0;
      }

      /* Subdivide and try again. */
      for (iv = 0; iv < nv; iv++) {
         for (m = 0; m < M; m++) {
            vox2[m] = level ? 2*vox[m] : 0;
            if (iv & (1 << m)) {
               vox2[m]++;
            }
         }

         if (tabvox(tab, wp, level+1, vox2) == 0) {
            return 0;
         }
      }
   }

   /* No solution in this voxel. */
   return 1;
}
