*-----------------------------------------------------------------------
*     SUBCOM: Subtract a component from a list of pixels
*-----------------------------------------------------------------------
*
*     Copyright (C) 1997
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
*     SUBCOM contains a set of functions for subtracting a component
*     (defined as a pixel with specified amplitude and position), from
*     a list of components after convolving the component with a two
*     dimensional point spread function.
*
*     This is very specialised function that is at the core of the
*     Clark clean algorithm and is used by the ClarkCleanModel class.
*
*     It contains a number of functions for subtracting components with
*     1, 2 or 4 polarizations. While there is no intrinsic difference
*     between these three functions separate functions where written to
*     avoid an otherwise small inner loop over all the polarizations.
*     I hope this is more efficient.
*
*     The following subroutines are found here:
*     SUBCOMF : for one polarisation and REAL arrays
*     SUBCOM2F: for two polarisations (I & V) and REAL arrays
*     SUBCOM4F: for four polarisations (I, Q, U & V) and REAL arrays
*
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
*     SUBROUTINE SUBCOMpx(PIXVAL, PIXPOS, NPIX, MAXPIX, MAXPOS, PSF, NX, NY)
*          
*     Given:
*          NPIX     I     The number of components in the list
*          PIXVAL(p,NPIX)
*                   x     The amplitude of each component in the list
*          PIXPOS(2,NPIX)
*                   I     The position of each component in the list
*          MAXVAL(p)
*                   x     The amplitude of the maximum pixel
*          MAXPOS(2)
*                   I     The position of the maximum pixel
*          NX, NY   I     The size of the two dimensional point spread function
*          PSF(NX,NY)
*                   x     Data array. Note that if p.NE.1 then NPIX is
*                         NOT the total size of the ARR array
*
*     return:
*          PIXVAL(p,NPIX)
*                   x     The amplitude of each component in the list
*                         after the maximum has been subtracted
*
*     Notes:
*       1) It is assumed that the "origin" of the psf is at (NX,NY)/2 + 1
*          At this point the psf should have a value of one. For a 
*          128 by 128 psf this will be at 65, 65 and for a 5 by 5 psf
*          it will be at 3, 3 (where the first element is at 1,1). This
*          is relatively simple to change.
*
*       2) A series of nested IF's is used rather than a single 
*          IF (expr1).AND.(expr2).AND ... for effeciency
*          It means that the as soon as one expression returns false
*          the component will bypassed.
*
*-----------------------------------------------------------------------

      SUBROUTINE SUBCOMF(PIXVAL, PIXPOS, NPIX, MAXPIX, MAXPOS, 
     *     PSF, NX, NY)

*     Subtract a component (after convolving with the psf) from a list
*     of pixels in an array of REAL numbers

      INTEGER NPIX, NX, NY, PIXPOS(2, NPIX), MAXPOS(2)
      REAL MAXPIX, PIXVAL(NPIX), PSF(NX, NY)

      INTEGER N, POSX, POSY, IX, IY
      REAL PSFVAL
*-----------------------------------------------------------------------
      POSX = NX/2 + 1 - MAXPOS(1)
      POSY = NY/2 + 1 - MAXPOS(2)
      DO 10 N = 1, NPIX
         IX = PIXPOS(1, N) + POSX
         IF (IX.GE.1) THEN
            IF (IX.LE.NX) THEN
               IY = PIXPOS(2, N) + POSY
               IF (IY.GE.1) THEN
                  IF (IY.LE.NY) THEN
                     PSFVAL = PSF(IX,IY)
                     PIXVAL(N) = PIXVAL(N) - MAXPIX*PSFVAL
                  END IF
               END IF
            END IF
         END IF
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE SUBCOM2F(PIXVAL, PIXPOS, NPIX, MAXPIX, MAXPOS, 
     *     PSF, NX, NY)

*     Subtract a component (after convolving with the psf) from a list
*     of pixels in an array of REAL numbers with 2-Polarizations (I & V)

      INTEGER NPIX, NX, NY, PIXPOS(2, NPIX), MAXPOS(2)
      REAL MAXPIX(2), PIXVAL(2, NPIX), PSF(NX, NY)

      INTEGER N, POSX, POSY, IX, IY
      REAL PSFVAL, IMAX, VMAX
*-----------------------------------------------------------------------
      POSX = NX/2 + 1 - MAXPOS(1)
      POSY = NY/2 + 1 - MAXPOS(2)
      IMAX = MAXPIX(1)
      VMAX = MAXPIX(2)
      DO 10 N = 1, NPIX
         IX = PIXPOS(1, N) + POSX
         IF (IX.GE.1) THEN
            IF (IX.LE.NX) THEN
               IY = PIXPOS(2, N) + POSY
               IF (IY.GE.1) THEN
                  IF (IY.LE.NY) THEN
                     PSFVAL = PSF(IX,IY)
                     PIXVAL(1, N) = PIXVAL(1, N) - IMAX*PSFVAL
                     PIXVAL(2, N) = PIXVAL(2, N) - VMAX*PSFVAL
                  END IF
               END IF
            END IF
         END IF
 10   CONTINUE
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE SUBCOM4F(PIXVAL, PIXPOS, NPIX, MAXPIX, MAXPOS, 
     *     PSF, NX, NY)

*     Subtract a component (after convolving with the psf) from a list
*     of pixels in an array of REAL numbers with 4-Polarizations 
*     (I, Q, U & V)

      INTEGER NPIX, NX, NY, PIXPOS(2, NPIX), MAXPOS(2)
      REAL MAXPIX(4), PIXVAL(4, NPIX), PSF(NX, NY)

      INTEGER N, POSX, POSY, IX, IY
      REAL PSFVAL, IMAX, QMAX, UMAX, VMAX
*-----------------------------------------------------------------------
      POSX = NX/2 + 1 - MAXPOS(1)
      POSY = NY/2 + 1 - MAXPOS(2)
      IMAX = MAXPIX(1)
      QMAX = MAXPIX(2)
      UMAX = MAXPIX(3)
      VMAX = MAXPIX(4)
      DO 10 N = 1, NPIX
         IX = PIXPOS(1, N) + POSX
         IF (IX.GE.1) THEN
            IF (IX.LE.NX) THEN
               IY = PIXPOS(2, N) + POSY
               IF (IY.GE.1) THEN
                  IF (IY.LE.NY) THEN
                     PSFVAL = PSF(IX,IY)
                     PIXVAL(1, N) = PIXVAL(1, N) - IMAX*PSFVAL
                     PIXVAL(2, N) = PIXVAL(2, N) - QMAX*PSFVAL
                     PIXVAL(3, N) = PIXVAL(3, N) - UMAX*PSFVAL
                     PIXVAL(4, N) = PIXVAL(4, N) - VMAX*PSFVAL
                  END IF
               END IF
            END IF
         END IF
 10   CONTINUE
      RETURN
      END
