*-----------------------------------------------------------------------
*     ABSMAX: find the element with the largest absolute value
*-----------------------------------------------------------------------
*
*     Copyright (C) 1997,2000
*     Associated Universities, Inc. Washington DC, USA.
*
*     This library is free software; you can redistribute it and/or
*     modify it under the terms of the GNU Library General Public
*     License as published by the Free Software Foundation; either
*     version 2 of the License, or (at your option) any later version.
*
*     This library is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Library General Public License for more details.
*
*     You should have received a copy of the GNU Library General Public
*     License along with this library; if not, write to the Free
*     Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
*     MA 02139, USA.
*
*     Correspondence concerning AIPS++ should be addressed as follows:
*            Internet email: aips2-request@nrao.edu.
*            Postal address: AIPS++ Project Office
*                            National Radio Astronomy Observatory
*                            520 Edgemont Road
*                            Charlottesville, VA 22903-2475 USA
*
*     $Id$
*
*-----------------------------------------------------------------------
*
*     ABSMAX contains a set of functions for finding the element with
*     the largest absolute value in a data array, and the position
*     (offset) of this element. It is always assumed that the array is
*     one dimensional, with a specified number of elements. Standard
*     AIPS++ Arrays can, unless they have a step increment, or are a
*     sub-array, be treated as one dimensional for the purposes of
*     finding this element. Global functions defined in IPosition.h can
*     be used to convert the position returned by these functions into
*     an IPosition for indexing into the multi-dimensional AIPS++ Array.
*
*     These functions where written for the ClarkCleanModel class and
*     hence include the ability to search two dimensional arrays. Unlike
*     related function (MAXABS, HISABS, MINMAX, GETBIG), these functions
*     require that the polarization axis is the FIRST axis, and not the
*     last. 

*     The following subroutines are found here:
*     ABSMAXF : for Arrays of REAL(float) numbers 
*     ABSMAX2F: for Arrays of REAL(float) numbers with 
*               two polarizations (I & V)
*     ABSMAX4F: for Arrays of REAL(float) numbers with
*               four polarizations (I, Q, U, V)
*     It should be trivial to clone these routines for any other data
*     types, in particular double precision. 
*
*     The following comments apply to all subroutines found here:
*
*     The general form of the subroutine call is (x is replaced by
*     a character indicating the type: F for Float, D for Double, etc.),
*     and p is a number indicating the number of polarizations that are
*     required. (if p=1 it is dropped entirely)
*
*     SUBROUTINE ABSMAXpx(MAXELEM, MAXVAL, MAXPOS, ARR, NPIX);
*     Given:
*          NPIX     I     The number of pixels in the Array
*          ARR(p,NPIX)
*                   x     Data array. Note that if p.NE.1 then NPIX is
*                         NOT the total size of the ARR array
*
*     return:
*          MAXELEM(p)
*                   x     The element with the maximum value.
*          MAXVAL   x     The absolute value of this element
*          MAXPOS   I     The position of the maximum element
*
*     Notes:
*       1) It is assumed that the data array contains at least ONE
*          element (ie. NPIX.GE.1)
*
*       2) The element returned will in be an array with with p
*          elements, but its absolute value will always be a REAL number
*
*       3) The returned positions are ZERO relative. ie. the first
*          pixel in the array is at position = 0. This is for
*          convenience for the C/C++ code that will be using these
*          functions. 
*
*       4) In the 2 polarization case it is assumed that the two
*          polarizations are I & V, and that I is in the even 
*          locations in the array, and V in the odd locations.
*
*       5) In the 4 polarization case it is assumed that the four
*          polarizations are I, Q, U & V, and that they are in that
*          order in the array
*
*       6) The maximum absolute values returned if p.NE.1 are a
*          function of all the polarizations at that pixel. For the 4
*          polarization case this is the maximum eigenvalue 
*          (=ABS(I+SQRT(Q*Q+U*U+V*V))), and for the two polarisation 
*          case it is ABS(I + ABS(V))
*-----------------------------------------------------------------------

      SUBROUTINE ABSMAXF(MAXELEM, MAXVAL, MAXPOS, ARR, NPIX)

*     Return the array element with the maximum absolute value 
*     (and its position) in an array of REAL numbers

      INTEGER NPIX, MAXPOS
      REAL MAXELEM, MAXVAL, ARR(NPIX)

      INTEGER N
      REAL SAMPLE, I
*-----------------------------------------------------------------------
      I = ARR(1)
      MAXELEM = I
      MAXVAL = ABS(I)
      MAXPOS = 1
      DO 10 N = 2, NPIX
         I = ARR(N)
         SAMPLE = ABS(I)
         IF (SAMPLE.GT.MAXVAL) THEN
            MAXVAL = SAMPLE
            MAXELEM = I
            MAXPOS = N
         END IF
 10   CONTINUE
      MAXPOS = MAXPOS - 1
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSMAX2F(MAXELEM, MAXVAL, MAXPOS, ARR, NPIX)

*     Return the array element with the maximum absolute value 
*     (and its position) in an array of REAL numbers with 
*     2-Polarizations (I,V)

      INTEGER NPIX, MAXPOS
      REAL MAXELEM(2), MAXVAL, ARR(2, NPIX)

      INTEGER N
      REAL SAMPLE, I, V
*-----------------------------------------------------------------------
      I = ARR(1,1)
      V = ARR(2,1)
      MAXELEM(1) = I
      MAXELEM(2) = V
      MAXVAL = MAX(ABS(I+V), ABS(I-V))
      MAXPOS = 1
      DO 10 N = 2, NPIX
         I = ARR(1,N)
         V = ARR(2,N)
         SAMPLE = MAX(ABS(I+V), ABS(I-V))
         IF (SAMPLE.GT.MAXVAL) THEN
            MAXVAL = SAMPLE
            MAXELEM(1) = I
            MAXELEM(2) = V
            MAXPOS = N
         END IF
 10   CONTINUE
      MAXPOS = MAXPOS - 1
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSMAX4F(MAXELEM, MAXVAL, MAXPOS, ARR, NPIX)

*     Return the array element with the maximum absolute value 
*     (and its position) in an array of REAL numbers with 
*     4-Polarizations (I,Q,U,V)

      INTEGER NPIX, MAXPOS
      REAL MAXELEM(4), MAXVAL, ARR(4, NPIX)

      INTEGER N
      REAL SAMPLE, I, Q, U, V
*-----------------------------------------------------------------------
      I = ARR(1,1)
      Q = ARR(2,1)
      U = ARR(3,1)
      V = ARR(4,1)
      MAXELEM(1) = I
      MAXELEM(2) = Q
      MAXELEM(3) = U
      MAXELEM(4) = V
      MAXVAL = ABS(I+SQRT(Q*Q+U*U+V*V))
      MAXPOS = 1
      DO 10 N = 2, NPIX
         I = ARR(1,N)
         Q = ARR(2,N)
         U = ARR(3,N)
         V = ARR(4,N)
         SAMPLE = ABS(I+SQRT(Q*Q+U*U+V*V))
         IF (SAMPLE.GT.MAXVAL) THEN
            MAXVAL = SAMPLE
            MAXELEM(1) = I
            MAXELEM(2) = Q
            MAXELEM(3) = U
            MAXELEM(4) = V
            MAXPOS = N
         END IF
 10   CONTINUE
      MAXPOS = MAXPOS - 1
      RETURN
      END
