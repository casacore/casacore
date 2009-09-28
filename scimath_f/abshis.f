*-----------------------------------------------------------------------
*     ABSHIS: calculate an histogram of absolute values.
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
*     ABSHIS contains a set of functions which produce a histogram of
*     the absolute values in a data array. It is always assumed that the
*     array is one dimensional, with a specified number of elements. 
*     Standard AIPS++ Arrays can, unless they have a step increment
*     defined, be treated as one dimensional for the purposes of finding
*     the minimum and maximum value.
*
*     These functions also return the limits of the returned histogram,
*     ie. minimum and maximum absolute values in the data array.
* 
*     These functions where written for the ClarkCleanModel class and
*     hence include the ability to find the minimum and maximum of a
*     two dimensional array where the second axis is the polarization
*     axis.  This requires that the slowest moving axis is
*     the polarization axis. 

*     The following subroutines are found here:
*     ABSHISF : for Arrays of REAL(float) numbers 
*     ABSHIS2F: for Arrays of REAL(float) numbers with 
*               two polarizations (I & V)
*     ABSHIS4F: for Arrays of REAL(float) numbers with
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
*     SUBROUTINE ABSHISpx(HIST, MINVAL, MAXVAL, NBINS, ARR, NPIX)
*     Given:
*          NPIX     I     The number of pixels in the data array (ARR)
*          NBINS    I     The number of bins in the histogram (HIST)
*          ARR(NPIX,p)
*                   x     Data array. Note that if p.NE.1 then NPIX is
*                         NOT the total size of the ARR array
*          HIST(NBINS)
*                   I     An array to hold the histogram.
*
*     return:
*          MINVAL   x     The minimum absolute value in the Array
*          MAXVAL   x     The maximum absolute value in the Array
*          HIST(NBINS)
*                   I     The resultant histogram produced from the data.
*
*     Notes:
*       1) The histogram needs to be initialised as it will NOT be
*          initialised to zero in these routines.
*
*       2) If the data array has all pixels with the same value (like
*          all zero) then the histogram will have all the pixels in the
*          topmost bin.
*
*       3) This routine calculates the minimum and maximum absolute
*          values and returns them also.
*
*       4) In the 2 polarization case it is assumed that the two
*          polarizations are I & V, and that I is in the first half of
*          the array, and V in the second half.
*
*       5) In the 4 polarization case it is assumed that the four
*          polarizations are I, Q, U & V, and that they are in that
*          order in the array
*
*       6) The minimum and maximum absolute values returned if p.NE.1 are a
*          function of all the polarizations at that pixel. For the 4
*          polarization case this is the maximum eigenvalue 
*          (=ABS(I+SQRT(Q*Q+U*U+V*V))), and for the 
*          two polarisation case it is ABS(I + ABS(V))
*-----------------------------------------------------------------------

      SUBROUTINE ABSHISF(HIST, MINVAL, MAXVAL, NBINS, ARR, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers.

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX)

      INTEGER N, BIN
      REAL SAMPLE
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      MINVAL = ABS(ARR(1))
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         SAMPLE = ABS(ARR(N))
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            SAMPLE = ABS(ARR(N))
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSHIS2F(HIST, MINVAL, MAXVAL, NBINS, ARR, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers, assuming the array has two polarizations (I & V)

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX,2)

      INTEGER N, BIN
      REAL SAMPLE, I, V
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      I = ARR(1,1)
      V = ARR(1,2)
      MINVAL = MAX(ABS(I+V), ABS(I-V))
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         V = ARR(N,2)
         SAMPLE = MAX(ABS(I+V), ABS(I-V))
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            I = ARR(N,1)
            V = ARR(N,2)
            SAMPLE = MAX(ABS(I+V), ABS(I-V))
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSHIS4F(HIST, MINVAL, MAXVAL, NBINS, ARR, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers, assuming the array has four polarizations (I, Q, U, V)

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX,4)

      INTEGER N, BIN
      REAL SAMPLE, I, Q, U, V
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      I = ARR(1,1)
      Q = ARR(1,2)
      U = ARR(1,3)
      V = ARR(1,4)
      MINVAL = ABS(I+SQRT(Q*Q+U*U+V*V))
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         Q = ARR(N,2)
         U = ARR(N,3)
         V = ARR(N,4)
         SAMPLE = ABS(I+SQRT(Q*Q+U*U+V*V))
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            I = ARR(N,1)
            Q = ARR(N,2)
            U = ARR(N,3)
            V = ARR(N,4)
            SAMPLE = ABS(I+SQRT(Q*Q+U*U+V*V))
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSHIMF(HIST, MINVAL, MAXVAL, NBINS, ARR, MASK, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers weighted by the mask.

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX), MASK(NPIX)

      INTEGER N, BIN
      REAL SAMPLE
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      MINVAL = ABS(ARR(1)) * MASK(1)
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         SAMPLE = ABS(ARR(N)) * MASK(N)
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            SAMPLE = ABS(ARR(N)) * MASK(N)
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSHIM2F(HIST, MINVAL, MAXVAL, NBINS, ARR, MASK, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers, assuming the array has two polarizations (I & V) &
*     is weighted by a mask.

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX,2), MASK(NPIX)

      INTEGER N, BIN
      REAL SAMPLE, I, V
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      I = ARR(1,1)
      V = ARR(1,2)
      MINVAL = ABS(I + ABS(V)) * MASK(1)
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         V = ARR(N,2)
         SAMPLE = ABS(I + ABS(V)) * MASK(N)
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            I = ARR(N,1)
            V = ARR(N,2)
            SAMPLE = ABS(I + ABS(V)) * MASK(N)
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ABSHIM4F(HIST, MINVAL, MAXVAL, NBINS, ARR, MASK, NPIX)

*     Calculate a histogram of the absolute values of an array of REAL 
*     numbers, assuming the array has four polarizations (I, Q, U, V) & 
*     is weighted by a mask.

      INTEGER NPIX, NBINS
      INTEGER HIST(0:NBINS-1)
      REAL MAXVAL, MINVAL, ARR(NPIX,4), MASK(NPIX)

      INTEGER N, BIN
      REAL SAMPLE, I, Q, U, V
*-----------------------------------------------------------------------
*
* Find the minimum and maximum absolute values
*
      I = ARR(1,1)
      Q = ARR(1,2)
      U = ARR(1,3)
      V = ARR(1,4)
      MINVAL = ABS(I+SQRT(Q*Q+U*U+V*V)) * MASK(1)
      MAXVAL = MINVAL
      DO 10 N = 2, NPIX
         I = ARR(N,1)
         Q = ARR(N,2)
         U = ARR(N,3)
         V = ARR(N,4)
         SAMPLE = ABS(I+SQRT(Q*Q+U*U+V*V)) * MASK(N)
         MAXVAL = MAX(MAXVAL, SAMPLE)
         MINVAL = MIN(MINVAL, SAMPLE)
 10   CONTINUE
*
* Now create the histogram
* 
      IF (MINVAL.NE.MAXVAL) THEN
         SCALE = REAL(NBINS)/(MAXVAL-MINVAL)
         DO 20 N = 1, NPIX
            I = ARR(N,1)
            Q = ARR(N,2)
            U = ARR(N,3)
            V = ARR(N,4)
            SAMPLE = ABS(I+SQRT(Q*Q+U*U+V*V)) * MASK(N)
            BIN = INT((SAMPLE-MINVAL)*SCALE)
            IF (BIN.EQ.NBINS) THEN
               HIST(NBINS-1) = HIST(NBINS-1) + 1
            ELSE
               HIST(BIN) = HIST(BIN) + 1
            END IF
 20      CONTINUE
      ELSE
         HIST(NBINS-1) = NPIX
      END IF
      RETURN
      END
