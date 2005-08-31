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
*   $Id$
*===========================================================================*/

#include <math.h>

#include "wcsunits.h"

/* Map status return value to message. */
const char *wcsunits_errmsg[] = {
   "Success",
   "Invalid numeric multiplier",
   "Dangling binary operator",
   "Invalid symbol in INITIAL context",
   "Function in invalid context",
   "Invalid symbol in EXPON context",
   "Unbalanced bracket",
   "Unbalanced parenthesis",
   "Consecutive binary operators",
   "Internal parser error",
   "Non-conformant unit specifications",
   "Non-conformant functions",
   "Potentially unsafe translation"};


/* Unit types. */
const char *wcsunits_types[] = {
   "plane angle",
   "solid angle",
   "charge",
   "mole",
   "temperature",
   "luminous intensity",
   "mass",
   "length",
   "time",
   "beam",
   "bin",
   "bit",
   "count",
   "stellar magnitude",
   "pixel",
   "solar ratio",
   "voxel"};

const char *wcsunits_units[] = {
   "degree",
   "steradian",
   "Coulomb",
   "mole",
   "Kelvin",
   "candela",
   "kilogram",
   "metre",
   "second",
   "", "", "", "", "", "", "", ""};

int wcsunits(
  const char have[],
  const char want[],
  double *scale,
  double *offset,
  double *power)

{
  int    func1, func2, i, status;
  double scale1, scale2, units1[WCSUNITS_NTYPE], units2[WCSUNITS_NTYPE];

  if (status = wcsulex(have, &func1, &scale1, units1)) {
    return status;
  }

  if (status = wcsulex(want, &func2, &scale2, units2)) {
    return status;
  }

  /* Check conformance. */
  for (i = 0; i < WCSUNITS_NTYPE; i++) {
    if (units1[i] != units2[i]) {
      return 10;
    }
  }

  *scale  = 0.0;
  *offset = 0.0;
  *power  = 1.0;

  switch (func1) {
  case 0:
    /* No function. */
    if (func2) {
      return 11;
    }

    *scale = scale1 / scale2;
    break;

  case 1:
    /* log(). */
    if (func2 == 1) {
      /* log(). */
      *scale  = 1.0;
      *offset = log10(scale1 / scale2);

    } else if (func2 == 2) {
      /* ln(). */
      *scale  = log(10.0);
      *offset = log(scale1 / scale2);

    } else {
      return 11;
    }

    break;

  case 2:
    /* ln(). */
    if (func2 == 1) {
      /* log(). */
      *scale  = 1.0 / log(10.0);
      *offset = log(scale1 / scale2);

    } else if (func2 == 2) {
      /* ln(). */
      *scale  = 1.0;
      *offset = log(scale1 / scale2);

    } else {
      return 11;
    }

    break;

  case 3:
    /* exp(). */
    if (func2 != 3) {
      return 11;
    }

    *scale = 1.0;
    *power = scale1 / scale2;
    break;

  default:
    /* Internal parser error. */
    return 9;
  }

  return 0;
}
