*-----------------------------------------------------------------------
*     MAXABS: find the maximum absolute value in an array
*-----------------------------------------------------------------------
*
*     Copyright (C) 1997,1998,2000
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
*     MAXABS contains a set of functions for finding the 
*     maximum absolute value in a data array. It is always assumed that 
*     the array is one dimensional, with a specified number of elements.
*     Standard AIPS++ Arrays can, unless they have a step increment
*     defined, or are a subarray, be treated as one dimensional for the 
*     purposes of finding the maximum value. 
*
*     These functions where written for the ClarkCleanModel class and
*     hence include the ability to find the maximum absolute
*     value of a two dimensional array where the second axis is the 
*     polarization axis. This requires that the slowest moving axis is
*     the polarization axis. 
*

*     There are also a set of functions for for finding the absolute
*     maximum value on pixels which are weighted by a mask. A mask is a
*     REAL array with the same number of pixels as the data array. Each
*     pixel is multiplied by the mask before its magnitude is compared
*     with the current maximum. If the mask is zero for a pixel then
*     its value can never be returned as the maximum. ie. the pixel is
*     masked. If the mask is one then then the mask has no effect on the
*     pixel. Intermediate values (between zero and one) can be used to
*     achieve a soft masking, where pixels may have their value
*     returned if there value is large enough. The returned value is
*     always the absolute value multiplied by the mask.
*      
*     The following subroutines are found here:
*     MAXABSF : for Arrays of REAL(float) numbers 
*     MAXABS2F: for Arrays of REAL(float) numbers with 
*               two polarizations (I & V)
*     MAXABS4F: for Arrays of REAL(float) numbers with
*               four polarizations (I, Q, U, V)
*     MAXABMF : for Arrays of REAL(float) numbers and masking
*     MAXABM2F: for Arrays of REAL(float) numbers with 
*               two polarizations (I & V) and masking
*     MAXABM4F: for Arrays of REAL(float) numbers with
*               four polarizations (I, Q, U, V) and masking
*     It should be trivial to clone these routines for any other data
*     types, in particular double precision. 
*
*     The following comments apply to all subroutines found here:
*
*     For the first three subroutines the general form of the subroutine
*     call is (x is replaced by a character indicating the type: F for
*     Float, D for Double, etc.), and p is a number indicating the
*     number of polarizations that are required. (if p=1 it is dropped
*     entirely)
*
*     SUBROUTINE MAXABSpx(MAXVAL, ARR, NPIX)
*     Given:
*          NPIX     I     The number of pixels in the Array
*          ARR(NPIX,p)
*                   x     Data array. Note that if p.NE.1 then NPIX is
*                         NOT the total size of the ARR array
*
*     return:
*          MAXVAL   x     The maximum absolute value in the Array
*
*     Notes:
*       1) It is assumed that the data array contains at least ONE
*          element (ie. NPIX.GE.1)
*
*       2) In the 2 polarization case it is assumed that the two
*          polarizations are I & V, and that I is in the first half of
*          the array, and V in the second half.
*
*       3) In the 4 polarization case it is assumed that the four
*          polarizations are I, Q, U & V, and that they are in that
*          order in the array
*
*       4) The minimum and maximum absolute values returned if p.NE.1 are a
*          function of all the polarizations at that pixel. For the 4
*          polarization case this is the maximum eigenvalue 
*          (=ABS(I+SQRT(Q*Q+U*U+V*V))), and for the two polarisation 
*          case it is MAX(ABS(I+V), ABS(I-V))
*
*     For the last three subroutines the general form of the subroutine
*     call is (x is replaced by a character indicating the type: F for
*     Float, D for Double, etc.), and p is a number indicating the
*     number of polarizations that are required. (if p=1 it is dropped
*     entirely)
*
*     SUBROUTINE MAXABMpx(MAXVAL, ARR, MASK, NPIX)
*     Given:
*          NPIX     I     The number of pixels in the Array
*          ARR(NPIX,p)
*                   x     Data array. Note that if p.NE.1 then NPIX is
*                         NOT the total size of the ARR array
*          MASK(NPIX)
*                   x     MASK array.
*
*     return:
*          MAXVAL   x     The maximum absolute value in the Array
*                         multiplied by the mask value
*
*     Notes:
*       1) See all the notes (1-4) for the ABSMAXpx routines
*
*       2) The mask array is a different size (ie. number of bytes)
*          to the data array if p.NE.1
*-----------------------------------------------------------------------

      SUBROUTINE MAXABSF(MAXVAL, ARR, NPIX)

*     Find the maximum absolute value in an array of REAL numbers

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX)

      INTEGER N
      REAL I
*-----------------------------------------------------------------------
      I = ARR(1)
      MAXVAL = ABS(I)
      DO 10 N = 2, NPIX
         I = ARR(N)
         MAXVAL = MAX(MAXVAL, ABS(I))
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE MAXABS2F(MAXVAL, ARR, NPIX)

*     Find the maximum absolute value in an array of REAL numbers with 
*     2-Polarizations (I,V)

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX, 2)
      REAL THISVAL

      INTEGER N
      REAL I, V
*-----------------------------------------------------------------------
      I = ARR(1,1)
      V = ARR(1,2)
      MAXVAL = MAX(ABS(I+V), ABS(I-V))
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         V = ARR(N,2)
         THISVAL = MAX(ABS(I+V), ABS(I-V))
         MAXVAL = MAX(MAXVAL, THISVAL)
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE MAXABS4F(MAXVAL, ARR, NPIX)

*     Find the maximum absolute value in an array of REAL numbers 
*     with 4-Polarizations (I,Q,U,V)

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX, 4)

      INTEGER N
      REAL I, Q, U, V
*-----------------------------------------------------------------------
      I = ARR(1,1)
      Q = ARR(1,2)
      U = ARR(1,3)
      V = ARR(1,4)
      MAXVAL = ABS(I+SQRT(Q*Q+U*U+V*V))
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         Q = ARR(N,2)
         U = ARR(N,3)
         V = ARR(N,4)
         MAXVAL = MAX(MAXVAL, ABS(I+SQRT(Q*Q+U*U+V*V)))
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE MAXABMF(MAXVAL, ARR, MASK, NPIX)

*     Find the maximum absolute value in an array of REAL numbers
*     weighted by a mask.

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX), MASK(NPIX)

      INTEGER N
      REAL I
*-----------------------------------------------------------------------
      I = ARR(1)
      MAXVAL = MASK(1) * ABS(I)
      DO 10 N = 2, NPIX
         I = ARR(N)
         MAXVAL = MAX(MAXVAL, MASK(N) * ABS(I))
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE MAXABM2F(MAXVAL, ARR, MASK, NPIX)

*     Find the maximum absolute value in an array of REAL numbers with
*     2-Polarizations (I,V) weighted by a mask.

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX, 2), MASK(NPIX)

      INTEGER N
      REAL I, V, THISVAL
*-----------------------------------------------------------------------
      I = ARR(1,1)
      V = ARR(1,2)
      MAXVAL = MASK(1) * MAX(ABS(I+V), ABS(I-V))
      DO 10 N = 1, NPIX
         I = ARR(N,1)
         V = ARR(N,2)
         THISVAL = MASK(N) * MAX(ABS(I+V), ABS(I-V)) 
         MAXVAL = MAX(MAXVAL, THISVAL)
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE MAXABM4F(MAXVAL, ARR, MASK, NPIX)

*     Find the maximum absolute value in an array of REAL numbers 
*     with 4-Polarizations (I,Q,U,V) weighted by a mask.

      INTEGER NPIX
      REAL MAXVAL, ARR(NPIX, 4), MASK(NPIX)

      INTEGER N
      REAL I, Q, U, V
*-----------------------------------------------------------------------
      I = ARR(1,1)
      Q = ARR(1,2)
      U = ARR(1,3)
      V = ARR(1,4)
      MAXVAL = MASK(1) * ABS(I+SQRT(Q*Q+U*U+V*V))
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         Q = ARR(N,2)
         U = ARR(N,3)
         V = ARR(N,4)
         MAXVAL = MAX(MAXVAL, MASK(N) * ABS(I+SQRT(Q*Q+U*U+V*V)))
 10   CONTINUE
      RETURN
      END
