/*============================================================================
*
*   PGSBOX 3.4 - a non-linear coordinate axis plotter for PGPLOT.
*   Copyright (C) 1997-2004, Mark Calabretta
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
*=============================================================================
*
*   pgwcsl_() is an NLFUNC for PGSBOX that defines curvilinear celestial
*   coordinate systems by interfacing to WCSLIB 3.x.
*
*   Since WCSLIB 3.x is a C library, pgwcsl_() is written in C.  However, as
*   PGSBOX expects NLFUNC to be a FORTRAN subroutine, its interfaces
*   necessarily emulate those of a FORTRAN subroutine.  Hence the trailing
*   underscore in the name of the function and the pointer (reference)
*   argument list.
*
*   The wcsprm struct on which WCSLIB 3.x is based is passed as an integer
*   array of size WCSLEN at least (WCSLEN is defined in wcs.h).  While the
*   contents of this array are not interpretable in FORTRAN, it may be
*   constructed and interrogated by service routines (WCSPUT and WCSGET)
*   provided with the FORTRAN wrappers for WCSLIB 3.x.  The array is cast to
*   (struct wcsprm *) for use here and in WCSLIB.
*
*   Given:
*      opcode   int*     Transformation code:
*                            2: Compute a set of pixel coordinates which
*                               describe a path between this and the previous
*                               pair of world coordinates remembered from the
*                               last call with opcode == 1 || 2.
*                            1: Compute pixel coordinates from world
*                               coordinates.
*                            0: Initialize.
*                           -1: Compute world coordinates from pixel
*                               coordinates.
*
*      nlc      int*     Number of elements in nlcprm[] (not used).
*
*      nli      int*     Number of elements in wcs (at least WCSLEN).
*
*      nld      int*     Number of elements in nldprm (not used).
*
*      nlcprm   char[nlc]
*                        Character array (not used).
*
*   Given and/or returned:
*      wcs      int[nli] Integer array which contains the wcsprm struct (see
*                        below).
*
*      nldprm   double[nld]
*                        Double precision array (not used).
*
*      world    double[2]
*                        World coordinates.  world[0] and world[1] are the
*                        longitude and latitude, in degrees.  Given if
*                        opcode > 0, returned if opcode < 0.
*
*      pixel    double[2]
*                        Pixel coordinates.  Given if opcode < 0, returned if
*                        opcode > 0.
*
*      contrl   int*     Control flag for opcode == 2:
*                           0: Normal state
*                           1: A discontinuity has been encountered; force
*                              PGSBOX to flush its plotting buffer and call
*                              pgwcsl_() again with the same world
*                              coordinates.
*                           2: Call pgwcsl_() again with the same world
*                              coordinates.
*
*      contxt   double[20]
*                        Context elements for opcode == 2.
*
*   Returned:
*      ierr     int*     Error status
*                           0: Success.
*                           1: Invalid parameters.
*                           2: Invalid world coordinate.
*                           3: Invalid pixel coordinate.
*
*   Notes
*   -----
*    1) pgwcsl_() assumes a simple 2-D image.
*
*    2) The wcsprm struct (contained in the wcs[] array) is maintained by
*       pgwcsl_() and WCSLIB and must not be disturbed by the caller after
*       initialization with opcode == 0.
*
*    3) pgwcsl_() doesn't properly handle discontinuities between the faces
*       of the quadcube projections.
*
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/
#ifndef pgwcsl_h
#define pgwcsl_h

#include <cpgsbox.h>

#ifdef __cplusplus
extern "C" {
#endif

nlfunc_t pgwcsl_;

#ifdef __cplusplus
}
#endif

#endif
