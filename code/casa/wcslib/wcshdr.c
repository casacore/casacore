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

#include <stdlib.h>

#include "wcshdr.h"
#include "wcs.h"

/*--------------------------------------------------------------------------*/

int wcsidx(int nwcs, struct wcsprm **wcs, int alts[27])

{
   int a, i;
   struct wcsprm *wcsp;

   for (a = 0; a < 27; a++) {
      alts[a] = -1;
   }

   if (wcs == 0) {
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

   if (wcs == 0) {
      return 1;
   }

   wcsp = *wcs;
   for (a = 0; a < *nwcs; a++, wcsp++) {
      status |= wcsfree(wcsp);
   }

   free(*wcs);

   *nwcs = 0;
   *wcs = 0;

   return status;
}
