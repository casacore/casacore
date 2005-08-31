/*============================================================================
*
*   PGSBOX 4.1 - a non-linear coordinate axis plotter for PGPLOT.
*   Copyright (C) 1997-2005, Mark Calabretta
*
*   PGSBOX is free software; you can redistribute it and/or modify it under
*   the terms of the GNU General Public License as published by the Free
*   Software Foundation; either version 2 of the License, or (at your option)
*   any later version.
*
*   PGSBOX is distributed in the hope that it will be useful, but WITHOUT ANY
*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
*   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
*   details.
*
*   You should have received a copy of the GNU General Public License along
*   with PGSBOX; if not, write to the Free Software Foundation, Inc.,
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
*   cpgsbox() and cpglbox() are C wrappers for PGSBOX and PGLBOX.  Refer to
*   the prologue of pgsbox.f for an explanation of the argument list and usage
*   notes.
*
*   The argument lists for cpgsbox()/cpglbox() differ from PGSBOX/PGLBOX in
*   the following respects:
*
*      idents   char[3][80]
*                        Fixed length character array.
*      opt      char[2]  Fixed length character array.
*      nlfunc   nlfunc_t typedef for external function defined in cpgsbox.h.
*      cache    double[][4]
*                        Array indices reversed.
*
*   Note also that the array arguments to cpgsbox()/cpglbox() are all
*   0-relative, while several of those of PGSBOX/PGLBOX are 0-relative (GRID1,
*   GRID2, and CACHE) with the remainder 1-relative.  In particular, the two-
*   dimensional CACHE array has a mixture of 0-, and 1-relative indices, and
*   the indices are reversed in C because of the differing C and FORTRAN array
*   indexing policy.  Moreover, as in PGSBOX/PGLBOX, nc is the upper array
*   index, not the array length, so the array should be dimensioned as
*   cache[nc+1][4].
*
*===========================================================================*/
#ifndef cpgsbox_h
#define cpgsbox_h

#ifdef __cplusplus
extern "C" {
#endif

typedef void nlfunc_t(int *, int *, int *, int *, char *, int *, double *,
                      double *, double *, int *, double *, int *);

void cpgsbox(
  const float blc[2],
  const float trc[2],
  char (*idents)[80],
  const char opt[2],
  int labctl,
  int labden,
  const int ci[7],
  const int gcode[2],
  double tiklen,
  int ng1,
  const double *grid1,
  int ng2,
  const double *grid2,
  int doeq,
  nlfunc_t nlfunc,
  int nlc,
  int nli,
  int nld,
  char nlcprm[],
  int nliprm[],
  double nldprm[],
  int nc,
  int *ic,
  double cache[][4],
  int *ierr);

void cpglbox(
  char (*idents)[80],
  const char opt[2],
  int labctl,
  int labden,
  const int ci[7],
  const int gcode[2],
  double tiklen,
  int ng1,
  const double *grid1,
  int ng2,
  const double *grid2,
  int doeq,
  int nc,
  int *ic,
  double cache[][4],
  int *ierr);

#ifdef __cplusplus
}
#endif

#endif
