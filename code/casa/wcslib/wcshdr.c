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

#include <stdlib.h>

#include "wcshdr.h"
#include "wcs.h"

/*--------------------------------------------------------------------------*/

int wcsvfree(nwcs, wcs)

int *nwcs;
struct wcsprm **wcs;

{
  int a, status = 0;
  struct wcsprm *wcsp;

  wcsp = *wcs;
  for (a = 0; a < *nwcs; a++, wcsp++) {
    status |= wcsfree(wcsp);
  }

  free(*wcs);

  *nwcs = 0;
  *wcs = 0;

  return status;
}
