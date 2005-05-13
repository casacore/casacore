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
*=============================================================================
*
*   WCSLIB 4.1 - C routines that implement the FITS World Coordinate System
*   (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061
*
*   The Flexible Image Transport System (FITS), a data format widely used in
*   astronomy for data interchange and archive, is described in
*
*      "Definition of The Flexible Image Transport System (FITS)",
*      Hanisch, R.J., Farris, A., Greisen, E.W., et al. 2001, A&A, 376, 359
*
*   which formalizes NOST 100-2.0, a document produced by the NASA/Science
*   Office of Standards and Technology, see http://fits.gsfc.nasa.gov.
*
*
*   FITS units specification conversion; wcsunits()
*   -----------------------------------------------
*   wcsunits() derives the conversion from one system of units to another.
*
*   Given:
*      have     const char []
*                        FITS units specification to convert from (null-
*                        terminated).
*
*      want     const char []
*                        FITS units specification to convert to (null-
*                        terminated).
*
*   Returned:
*      scale    double*  Convert units using (scale*value + offset)^power.
*      offset   double*  Normally offset == 0.0 except for log() or ln()
*      power    double   conversions, e.g. log(MHz) to ln(Hz).  Likewise,
*                        power == 1.0 except for exp() conversions, e.g.
*                        exp(ms) to exp(/Hz).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                         1-9: (status return from wcsulex(), see below.)
*                          10: Non-conformant unit specifications.
*                          11: Non-conformant functions.
*
*                        scale is zeroed on return if an error occurs.
*
*
*   FITS units specification parser; wcsulex()
*   ------------------------------------------
*   wcsulex() parses a FITS units specification of arbitrary complexity,
*   deriving the scale factor required to convert to canonical units -
*   basically SI with degrees and "dimensionless" additions such as byte,
*   pixel and count.
*
*   Given:
*      unitstr  const char []
*                        Null-terminated character array containing the units
*                        specification, with or without surrounding square
*                        brackets (for inline specifications); text following
*                        the closing bracket is ignored.
*
*   Returned:
*      func     int*     Special function type (see Note 4 below)
*                          0: None
*                          1: log()  ...base 10
*                          2: ln()   ...base e
*                          3: exp()
*
*      scale    double*  Scale factor for the unit specification; multiply a
*                        value expressed in the given units by this factor to
*                        convert it to canonical units.
*
*      units    double[WCSUNITS_NTYPE]
*                        A units specification is decomposed into powers of 17
*                        fundamental unit types: angle, mass, length, time,
*                        count, pixel, etc.  Preprocessor macros are defined
*                        below to dimension this vector and access its
*                        elements.
*
*                        Corresponding character strings, wcsunits_types[] and
*                        wcsunits_units[], are predefined to describe each
*                        quantity and its canonical units.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Invalid numeric multiplier.
*                           2: Dangling binary operator.
*                           3: Invalid symbol in INITIAL context.
*                           4: Function in invalid context.
*                           5: Invalid symbol in EXPON context.
*                           6: Unbalanced bracket.
*                           7: Unbalanced parenthesis.
*                           8: Consecutive binary operators.
*                           9: Internal parser error.
*
*                        scale and units[] are zeroed on return if an error
*                        occurs.
*
*
*   Notes
*   -----
*    1) The parser is permissive in accepting whitespace in all contexts in a
*       units specification where it does not create ambiguity (e.g. not
*       between a metric prefic and a basic unit string), including in strings
*       like "log (m ** 2)" which is formally disallowed.
*
*    2) "angstrom" is allowed here in addition to "Angstrom" (OGIP usage).
*       "ohm"      is allowed here in addition to "Ohm"  (OGIP usage).
*       "Byte"     is allowed here in addition to "byte" (common usage).
*
*    3) Table 6 of WCS Paper I lists eleven units for which metric prefixes
*       are allowed.  However, in this implementation only prefixes greater
*       than unity are allowed for "a" (annum), "yr" (year), "pc" (parsec),
*       "bit", and "byte", and only prefixes less than unity are allowed for
*       "mag" (stellar magnitude).
*
*       Metric prefix "P" (peta) is specifically forbidden for "a" (annum) to
*       avoid confusion with "Pa" (Pascal, not peta-annum).  Note that metric
*       prefixes are specifically disallowed for "h" (hour) and "d" (day) so
*       that "ph" (photons) cannot be interpreted as pico-hours, nor "cd"
*       (candela) as centi-days.
*
*    4) Function types log(), ln() and exp() may only occur at the start of
*       the units specification.  The scale and units[] for these refers to
*       the string inside the function "argument", e.g. to "MHz" in log(MHz)
*       for which a scale of 1e6 will be returned.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   encoded in the wcsunits_errmsg character array.
*
*===========================================================================*/

#ifndef WCSLIB_WCSUNITS
#define WCSLIB_WCSUNITS

#ifdef __cplusplus
extern "C" {
#endif


extern const char *wcsunits_errmsg[];

extern const char *wcsunits_types[];
extern const char *wcsunits_units[];

#define WCSUNITS_PLANE_ANGLE 0
#define WCSUNITS_SOLID_ANGLE 1
#define WCSUNITS_CHARGE      2
#define WCSUNITS_MOLE        3
#define WCSUNITS_TEMPERATURE 4
#define WCSUNITS_LUMINTEN    5
#define WCSUNITS_MASS        6
#define WCSUNITS_LENGTH      7
#define WCSUNITS_TIME        8
#define WCSUNITS_BEAM        9
#define WCSUNITS_BIN        10
#define WCSUNITS_BIT        11
#define WCSUNITS_COUNT      12
#define WCSUNITS_MAGNITUDE  13
#define WCSUNITS_PIXEL      14
#define WCSUNITS_SOLRATIO   15
#define WCSUNITS_VOXEL      16

#define WCSUNITS_NTYPE      17


int wcsunits(const char [], const char [], double *, double *, double *);
int wcsulex(const char [], int *, double *, double []);


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCSUNITS */
