*-----------------------------------------------------------------------
*     GETBIG: find the pixels with the biggest absolute value
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
*     GETBIG contains a set of functions which extract a list of
*     components (defined as a pixel with specified amplitude and
*     position) from a specified array. Only components with an absolute
*     value greater than a specified fluxlimit are extracted.
*
*     This specialised function is used by the Clark clean algorithm 
*     (in the ClarkCleanModel class), and contains variants for
*     data with 1, 2 or 4 polarisations. 
*
*     It is assumed that the array from with the components to be
*     extracted has two spatial dimensions and that the third axis is
*     the polarization axis. 
*
*     The position of all the components in the list is zero relative,
*     for the convienience of the C/C++ functions which call this one.
*
*     If there are too many components to extract this function will
*     only extract enough components to fill the specified list. But it
*     will continue scanning the data array and return how many
*     components there are in total that meet the criteria. This would
*     allow the calling routine to resize the list to the required size
*     and call the this function again.
*
*     If there are fewer components in the list then the number of
*     components extracted will be returned. 
*
*     The following subroutines are found here:
*     GETBIGF : for Arrays of REAL numbers with 
*               one polarisation (I)
*     GETBIG2F: for Arrays of REAL numbers with
*               two polarisations (I & V)
*     GETBIG4F: for Arrays of REAL numbers with
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
*     SUBROUTINE GETBIGpx(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, ARR, NX, NY)
*     Given:
*          FLUXLIM  x     Extract only pixels with an
*                         absolute value greater then this fluxlimit
*          MAXPIX   I     The maximum number of pixels to extract
*          NX, NY   I     Size of the array to extract the componets from
*          ARR(NX,NY,p)
*                   x     The array wich the components are extracted from
*
*     return:
*          PIXVAL(p,NPIX)
*                   x     The amplitudes of the pixels extracted
*          PIXPOS(2,NPIX)
*                   I     The positions of the pixels extracted
*          MAXPIX   I     The number of pixels which meet the 
*                         fluxlimit criteria
*
*     Notes:
*
*       1) In the 2 and 4 polarization case a transpose is effectively
*          done as the input arrat as polarization as the slowest moving
*          axis but the returned pixel values have polarization as the
*          fastest moving axis. This is because the the length of the
*          returned list is not known beforehand, wheras the number of
*          polarizations is, and it ensures that the returned pixel
*          value list is contigious in memory, and that different
*          polarizations are near each other, which should improve the
*          number of hits in the processor data cache.
*
*       2) The minimum and maximum absolute values used if p.NE.1 is a
*          function of all the polarizations at that pixel. For the 4
*          polarization case this is the maximum eigenvalue 
*          (=ABS(I+SQRT(Q*Q+U*U+V*V))), and for the 
*          two polarisation case it is ABS(I + ABS(V))
*
*       3) The returned value of MAXPIX is positive if not all the
*          components could be extracted and negative if there where
*          not enough to fill the list
*-----------------------------------------------------------------------

      SUBROUTINE GETBIGF(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, ARR, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit.

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(MAXPIX), ARR(0:NX-1, 0:NY-1)

      INTEGER NPIX, IX, IY
      REAL I
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            I = ARR(IX, IY)
            IF (ABS(I).GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(NPIX) = I 
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE GETBIG2F(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, ARR, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit in an array
*     with two polarizations (I & V)

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(2, MAXPIX), ARR(0:NX-1, 0:NY-1, 2)

      INTEGER NPIX, IX, IY
      REAL I, V
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            I = ARR(IX, IY, 1)
            V = ARR(IX, IY, 2)
            IF ( (MAX(ABS(I+V), ABS(I-V))) .GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(1, NPIX) = I
                  PIXVAL(2, NPIX) = V
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE GETBIG4F(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, ARR, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit in an array
*     with four polarizations (I, Q, U & V)

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(4, MAXPIX), ARR(0:NX-1, 0:NY-1, 4)

      INTEGER NPIX, IX, IY
      REAL I, Q, U, V
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            I = ARR(IX, IY, 1)
            Q = ARR(IX, IY, 2)
            U = ARR(IX, IY, 3)
            V = ARR(IX, IY, 4)
            IF (ABS(I+SQRT(Q*Q+U*U+V*V)).GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(1, NPIX) = I
                  PIXVAL(2, NPIX) = Q
                  PIXVAL(3, NPIX) = U
                  PIXVAL(4, NPIX) = V
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE GETBIMF(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, 
     *     ARR, MASK, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit after weighting
*     the pixels by a mask.

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(MAXPIX)
      REAL ARR(0:NX-1, 0:NY-1), MASK(0:NX-1, 0:NY-1)

      INTEGER NPIX, IX, IY
      REAL I
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            I = ARR(IX, IY) * MASK(IX, IY)
            IF (ABS(I).GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(NPIX) = I 
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE GETBIM2F(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, 
     *     ARR, MASK, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit in an array with
*     two polarizations (I & V) after weighting the pixels by a mask.

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(2, MAXPIX)
      REAL ARR(0:NX-1, 0:NY-1, 2), MASK(0:NX-1, 0:NY-1)

      INTEGER NPIX, IX, IY
      REAL I, V, M
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            M = MASK(IX, IY)
            I = ARR(IX, IY, 1) * M
            V = ARR(IX, IY, 2) * M
            IF (ABS(I + ABS(V)).GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(1, NPIX) = I
                  PIXVAL(2, NPIX) = V
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE GETBIM4F(PIXVAL, PIXPOS, MAXPIX, FLUXLIM, 
     *     ARR, MASK, NX, NY)

*     Returns the value and position of all pixels which have an
*     absolute value greater than a specified fluxlimit in an array with
*     four polarizations (I, Q, U & V) after weighting the pixels by a
*     mask.

      INTEGER NX, NY, MAXPIX, PIXPOS(2, MAXPIX)
      REAL FLUXLIM, PIXVAL(4, MAXPIX)
      REAL ARR(0:NX-1, 0:NY-1, 4), MASK(0:NX-1, 0:NY-1)

      INTEGER NPIX, IX, IY
      REAL I, Q, U, V, M
*-----------------------------------------------------------------------
      NPIX = 0
      DO 10 IX = 0, NX-1
         DO 20, IY = 0, NY-1
            M = MASK(IX, IY)
            I = ARR(IX, IY, 1) * M
            Q = ARR(IX, IY, 2) * M
            U = ARR(IX, IY, 3) * M
            V = ARR(IX, IY, 4) * M
            IF (ABS(I+SQRT(Q*Q+U*U+V*V)).GE.FLUXLIM) THEN
               NPIX = NPIX + 1
               IF (NPIX.LE.MAXPIX) THEN
                  PIXVAL(1, NPIX) = I
                  PIXVAL(2, NPIX) = Q
                  PIXVAL(3, NPIX) = U
                  PIXVAL(4, NPIX) = V
                  PIXPOS(1, NPIX) = IX
                  PIXPOS(2, NPIX) = IY
               END IF
            END IF
 20      CONTINUE
 10   CONTINUE
      MAXPIX = NPIX - MAXPIX
      RETURN
      END
