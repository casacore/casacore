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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "lin.h"

const int LINSET = 137;

/* Map status return value to message. */
const char *lin_errmsg[] = {
   0,
   "Null linprm pointer passed",
   "Memory allocation failed",
   "PCi_ja matrix is singular"};


/*--------------------------------------------------------------------------*/

int linini(alloc, naxis, lin)

int alloc, naxis;
struct linprm *lin;

{
   int i, j;
   double *pc;

   if (lin == 0) return 1;
   if (naxis <= 0) {
      return 2;
   }

   if (lin->flag == -1 || lin->m_flag != LINSET) {
      lin->m_flag  = 0;
      lin->m_naxis = 0;
      lin->m_crpix = 0;
      lin->m_pc    = 0;
      lin->m_cdelt = 0;
   }


   /* Allocate memory for arrays if required. */
   if (alloc ||
       lin->crpix == 0 ||
       lin->pc    == 0 ||
       lin->cdelt == 0) {

      /* Was sufficient allocated previously? */
      if (lin->m_flag == LINSET && lin->m_naxis < naxis) {
         /* No, free it. */
         linfree(lin);
      }

      if (alloc || lin->crpix == 0) {
         if (lin->m_crpix) {
            /* In case the caller fiddled with it. */
            lin->crpix = lin->m_crpix;

         } else {
            if (!(lin->crpix = calloc(naxis, sizeof(double)))) {
               return 2;
            }

            lin->m_flag  = LINSET;
            lin->m_naxis = naxis;
            lin->m_crpix = lin->crpix;
         }
      }

      if (alloc || lin->pc == 0) {
         if (lin->m_pc) {
            /* In case the caller fiddled with it. */
            lin->pc = lin->m_pc;

         } else {
            if (!(lin->pc = calloc(naxis*naxis, sizeof(double)))) {
               linfree(lin);
               return 2;
            }

            lin->m_flag  = LINSET;
            lin->m_naxis = naxis;
            lin->m_pc    = lin->pc;
         }
      }

      if (alloc || lin->cdelt == 0) {
         if (lin->m_cdelt) {
            /* In case the caller fiddled with it. */
            lin->cdelt = lin->m_cdelt;

         } else {
            if (!(lin->cdelt = calloc(naxis, sizeof(double)))) {
               linfree(lin);
               return 2;
            }

            lin->m_flag  = LINSET;
            lin->m_naxis = naxis;
            lin->m_cdelt = lin->cdelt;
         }
      }
   }

   /* Free memory allocated by linset(). */
   if (lin->flag == LINSET) {
      if (lin->piximg) free(lin->piximg);
      if (lin->imgpix) free(lin->imgpix);
   }

   lin->piximg = 0;
   lin->imgpix = 0;
   lin->i_naxis = 0;


   lin->flag  = 0;
   lin->naxis = naxis;


   /* CRPIXja defaults to 0.0. */
   for (j = 0; j < naxis; j++) {
      lin->crpix[j] = 0.0;
   }


   /* PCi_ja defaults to the unit matrix. */
   pc = lin->pc;
   for (i = 0; i < naxis; i++) {
      for (j = 0; j < naxis; j++) {
         if (j == i) {
            *pc = 1.0;
         } else {
            *pc = 0.0;
         }
         pc++;
      }
   }


   /* CDELTia defaults to 1.0. */
   for (i = 0; i < naxis; i++) {
      lin->cdelt[i] = 1.0;
   }


   return 0;
}

/*--------------------------------------------------------------------------*/

int lincpy(alloc, linsrc, lindst)

int alloc;
const struct linprm *linsrc;
struct linprm *lindst;

{
   int i, j, naxis, status;
   const double *srcp;
   double *dstp;

   if (linsrc == 0) return 1;

   naxis = linsrc->naxis;
   if (naxis <= 0) {
      return 2;
   }

   if (status = linini(alloc, naxis, lindst)) {
      return status;
   }

   srcp = linsrc->crpix;
   dstp = lindst->crpix;
   for (j = 0; j < naxis; j++) {
      *(dstp++) = *(srcp++);
   }

   srcp = linsrc->pc;
   dstp = lindst->pc;
   for (i = 0; i < naxis; i++) {
      for (j = 0; j < naxis; j++) {
         *(dstp++) = *(srcp++);
      }
   }

   srcp = linsrc->cdelt;
   dstp = lindst->cdelt;
   for (i = 0; i < naxis; i++) {
      *(dstp++) = *(srcp++);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int linfree(lin)

struct linprm *lin;

{
   if (lin == 0) return 1;

   if (lin->flag != -1) {
      /* Free memory allocated by linini(). */
      if (lin->m_flag == LINSET) {
         if (lin->crpix == lin->m_crpix) lin->crpix = 0;
         if (lin->pc    == lin->m_pc)    lin->pc    = 0;
         if (lin->cdelt == lin->m_cdelt) lin->cdelt = 0;

         if (lin->m_crpix) free(lin->m_crpix);
         if (lin->m_pc)    free(lin->m_pc);
         if (lin->m_cdelt) free(lin->m_cdelt);
      }
   }

   lin->m_flag  = 0;
   lin->m_naxis = 0;
   lin->m_crpix = 0;
   lin->m_pc    = 0;
   lin->m_cdelt = 0;


   /* Free memory allocated by linset(). */
   if (lin->flag == LINSET) {
      if (lin->piximg) free(lin->piximg);
      if (lin->imgpix) free(lin->imgpix);
   }

   lin->piximg = 0;
   lin->imgpix = 0;
   lin->i_naxis = 0;

   lin->flag = 0;

   return 0;
}

/*--------------------------------------------------------------------------*/

int linprt(lin)

const struct linprm *lin;

{
   int i, j, k;

   if (lin == 0) return 1;

   if (lin->flag != LINSET) {
      printf("The linprm struct is UNINITIALIZED.\n");
      return 0;
   }

   printf("       flag: %d\n", lin->flag);
   printf("      naxis: %d\n", lin->naxis);
   printf("      crpix: 0x%x\n", (int)lin->crpix);
   printf("            ");
   for (i = 0; i < lin->naxis; i++) {
      printf("  %- 11.4g", lin->crpix[i]);
   }
   printf("\n");

   k = 0;
   printf("         pc: 0x%x\n", (int)lin->pc);
   for (i = 0; i < lin->naxis; i++) {
      printf("    pc[%d][]:", i);
      for (j = 0; j < lin->naxis; j++) {
         printf("  %- 11.4g", lin->pc[k++]);
      }
      printf("\n");
   }

   printf("      cdelt: 0x%x\n", (int)lin->cdelt);
   printf("            ");
   for (i = 0; i < lin->naxis; i++) {
      printf("  %- 11.4g", lin->cdelt[i]);
   }
   printf("\n");

   printf("      unity: %d\n", lin->unity);

   if (lin->piximg == 0) {
      printf("     piximg: (null)\n");
   } else {
      k = 0;
      for (i = 0; i < lin->naxis; i++) {
         printf("piximg[%d][]:", i);
         for (j = 0; j < lin->naxis; j++) {
            printf("  %- 11.4g", lin->piximg[k++]);
         }
         printf("\n");
      }
   }

   if (lin->imgpix == 0) {
      printf("     imgpix: (null)\n");
   } else {
      k = 0;
      for (i = 0; i < lin->naxis; i++) {
         printf("imgpix[%d][]:", i);
         for (j = 0; j < lin->naxis; j++) {
            printf("  %- 11.4g", lin->imgpix[k++]);
         }
         printf("\n");
      }
   }

   printf("     m_flag: %d\n", lin->m_flag);
   printf("    m_naxis: %d\n", lin->m_naxis);
   printf("    m_crpix: 0x%x", (int)lin->m_crpix);
   if (lin->m_crpix == lin->crpix) printf("  (= crpix)");
   printf("\n");
   printf("       m_pc: 0x%x", (int)lin->m_pc);
   if (lin->m_pc == lin->pc) printf("  (= pc)");
   printf("\n");
   printf("    m_cdelt: 0x%x", (int)lin->m_cdelt);
   if (lin->m_cdelt == lin->cdelt) printf("  (= cdelt)");
   printf("\n");

   return 0;
}

/*--------------------------------------------------------------------------*/

int linset(lin)

struct linprm *lin;

{
   int i, j, n, status;
   double *pc, *piximg;

   if (lin == 0) return 1;

   n = lin->naxis;

   /* Check for a unit matrix. */
   lin->unity = 1;
   pc = lin->pc;
   for (i = 0; i < n; i++) {
      for (j = 0; j < n; j++) {
         if (j == i) {
            if (*(pc++) != 1.0) {
               lin->unity = 0;
               break;
            }
         } else {
            if (*(pc++) != 0.0) {
               lin->unity = 0;
               break;
            }
         }
      }
   }


   if (lin->unity) {
      if (lin->flag == LINSET) {
         /* Free memory that may have been allocated previously. */
         if (lin->piximg) free(lin->piximg);
         if (lin->imgpix) free(lin->imgpix);
      }

      lin->piximg = 0;
      lin->imgpix = 0;
      lin->i_naxis = 0;

   } else {
      if (lin->flag != LINSET || lin->i_naxis < n) {
         if (lin->flag == LINSET) {
            /* Free memory that may have been allocated previously. */
            if (lin->piximg) free(lin->piximg);
            if (lin->imgpix) free(lin->imgpix);
         }

         /* Allocate memory for internal arrays. */
         if (!(lin->piximg = calloc(n*n, sizeof(double)))) {
            return 2;
         }

         if (!(lin->imgpix = calloc(n*n, sizeof(double)))) {
            free(lin->piximg);
            return 2;
         }

         lin->i_naxis = n;
      }

      /* Compute the pixel-to-image transformation matrix. */
      pc     = lin->pc;
      piximg = lin->piximg;
      for (i = 0; i < n; i++) {
         for (j = 0; j < n; j++) {
            *(piximg++) = lin->cdelt[i] * (*(pc++));
         }
      }

      /* Compute the image-to-pixel transformation matrix. */
      if (status = matinv(n, lin->piximg, lin->imgpix)) {
         return status;
      }
   }


   lin->flag = LINSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int linp2x(lin, ncoord, nelem, pixcrd, imgcrd)

struct linprm *lin;
int ncoord, nelem;
const double pixcrd[];
double imgcrd[];

{
   int i, j, k, n, status;
   double temp;
   register const double *pix;
   register double *img, *piximg;


   /* Initialize. */
   if (lin == 0) return 1;
   if (lin->flag != LINSET) {
      if (status = linset(lin)) return status;
   }

   n = lin->naxis;


   /* Convert pixel coordinates to intermediate world coordinates. */
   pix = pixcrd;
   img = imgcrd;

   if (lin->unity) {
      for (k = 0; k < ncoord; k++) {
         for (i = 0; i < n; i++) {
            *(img++) = lin->cdelt[i] * (*(pix++) - lin->crpix[i]);
         }

         pix += (nelem - n);
         img += (nelem - n);
      }

   } else {
      for (k = 0; k < ncoord; k++) {
         for (i = 0; i < n; i++) {
            img[i] = 0.0;
         }

         for (j = 0; j < n; j++) {
            /* Column-wise multiplication allows this to be cached. */
            temp = *(pix++) - lin->crpix[j];

            piximg = lin->piximg + j;
            for (i = 0; i < n; i++, piximg += n) {
               img[i] += *piximg * temp;
            }
         }

         pix += (nelem - n);
         img += nelem;
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int linx2p(lin, ncoord, nelem, imgcrd, pixcrd)

struct linprm *lin;
int ncoord, nelem;
const double imgcrd[];
double pixcrd[];

{
   int i, j, k, n, status;
   register const double *img;
   register double *imgpix, *pix;


   /* Initialize. */
   if (lin == 0) return 1;
   if (lin->flag != LINSET) {
      if (status = linset(lin)) return status;
   }

   n = lin->naxis;


   /* Convert intermediate world coordinates to pixel coordinates. */
   img = imgcrd;
   pix = pixcrd;

   if (lin->unity) {
      for (k = 0; k < ncoord; k++) {
         for (j = 0; j < n; j++) {
            *(pix++) = (*(img++) / lin->cdelt[j]) + lin->crpix[j];
         }

         pix += (nelem - n);
         img += (nelem - n);
      }

   } else {
      for (k = 0; k < ncoord; k++) {
         imgpix = lin->imgpix;

         for (j = 0; j < n; j++) {
            *pix = 0.0;
            for (i = 0; i < n; i++) {
               *pix += *imgpix * img[i];
               imgpix++;
            }

            *(pix++) += lin->crpix[j];
         }

         pix += (nelem - n);
         img += nelem;
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int matinv(n, mat, inv)

int n;
const double mat[];
double inv[];

{
   register int i, ij, ik, j, k, kj, pj;
   int    itemp, *mxl, *lxm, pivot;
   double colmax, *lu, *rowmax, dtemp;


   /* Allocate memory for internal arrays. */
   if (!(mxl = calloc(n, sizeof(int)))) return 2;
   if (!(lxm = calloc(n, sizeof(int)))) {
      free(mxl);
      return 2;
   }

   if (!(rowmax = calloc(n, sizeof(double)))) {
      free(mxl);
      free(lxm);
      return 2;
   }

   if (!(lu = calloc(n*n, sizeof(double)))) {
      free(mxl);
      free(lxm);
      free(rowmax);
      return 2;
   }


   /* Initialize arrays. */
   for (i = 0, ij = 0; i < n; i++) {
      /* Vector that records row interchanges. */
      mxl[i] = i;

      rowmax[i] = 0.0;

      for (j = 0; j < n; j++, ij++) {
         dtemp = fabs(mat[ij]);
         if (dtemp > rowmax[i]) rowmax[i] = dtemp;

         lu[ij] = mat[ij];
      }

      /* A row of zeroes indicates a singular matrix. */
      if (rowmax[i] == 0.0) {
         free(mxl);
         free(lxm);
         free(rowmax);
         free(lu);
         return 3;
      }
   }


   /* Form the LU triangular factorization using scaled partial pivoting. */
   for (k = 0; k < n; k++) {
      /* Decide whether to pivot. */
      colmax = fabs(lu[k*n+k]) / rowmax[k];
      pivot = k;

      for (i = k+1; i < n; i++) {
         ik = i*n + k;
         dtemp = fabs(lu[ik]) / rowmax[i];
         if (dtemp > colmax) {
            colmax = dtemp;
            pivot = i;
         }
      }

      if (pivot > k) {
         /* We must pivot, interchange the rows of the design matrix. */
         for (j = 0, pj = pivot*n, kj = k*n; j < n; j++, pj++, kj++) {
            dtemp = lu[pj];
            lu[pj] = lu[kj];
            lu[kj] = dtemp;
         }

         /* Amend the vector of row maxima. */
         dtemp = rowmax[pivot];
         rowmax[pivot] = rowmax[k];
         rowmax[k] = dtemp;

         /* Record the interchange for later use. */
         itemp = mxl[pivot];
         mxl[pivot] = mxl[k];
         mxl[k] = itemp;
      }

      /* Gaussian elimination. */
      for (i = k+1; i < n; i++) {
         ik = i*n + k;

         /* Nothing to do if lu[ik] is zero. */
         if (lu[ik] != 0.0) {
            /* Save the scaling factor. */
            lu[ik] /= lu[k*n+k];

            /* Subtract rows. */
            for (j = k+1; j < n; j++) {
               lu[i*n+j] -= lu[ik]*lu[k*n+j];
            }
         }
      }
   }


   /* mxl[i] records which row of mat corresponds to row i of lu.  */
   /* lxm[i] records which row of lu  corresponds to row i of mat. */
   for (i = 0; i < n; i++) {
      lxm[mxl[i]] = i;
   }


   /* Determine the inverse matrix. */
   for (i = 0, ij = 0; i < n; i++) {
      for (j = 0; j < n; j++, ij++) {
         inv[ij] = 0.0;
      }
   }

   for (k = 0; k < n; k++) {
      inv[lxm[k]*n+k] = 1.0;

      /* Forward substitution. */
      for (i = lxm[k]+1; i < n; i++) {
         for (j = lxm[k]; j < i; j++) {
            inv[i*n+k] -= lu[i*n+j]*inv[j*n+k];
         }
      }

      /* Backward substitution. */
      for (i = n-1; i >= 0; i--) {
         for (j = i+1; j < n; j++) {
            inv[i*n+k] -= lu[i*n+j]*inv[j*n+k];
         }
         inv[i*n+k] /= lu[i*n+i];
      }
   }

   free(mxl);
   free(lxm);
   free(rowmax);
   free(lu);

   return 0;
}
