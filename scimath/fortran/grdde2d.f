*=======================================================================
*     Copyright (C) 1999
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
*-----------------------------------------------------------------------
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C $Id$
C
      SUBROUTINE GRDDE2D (VIS, WT, UVW, NVIS, SCALE, OFFSET, 
     1   ORIGIN, GVIS, NAXIS1, NAXIS2, INTFNU, INTFNV, SUPP, OSAMP,
     $     SHIFT)
C
CD De-Grid two dimensional complex data. There must be no points within the 
C support size of the edge of the grid. 
C The scaling factors should be set so that SCALE(1)*U + OFFSET(1) converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, OFFSET(1), OFFSET(2) should nearly always be zero, while
C UORIGIN = 1, ORIGIN(2) = NV/2.
C
C	VIS	CMPLX(*)	Output	Un-gridded data
C	WT	REAL(*)		Output	Weights
C	UVW	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	SCALE	REAL(2)		input	Scaling factor to get to pixels
C	OFFSET	REAL(2)		input	Offset to get to pixels
C	ORIGIN	INT(2)		input	Origin of u axis
C	GVIS	CMPLX(*)	input	Gridded data
C	NAxis	INT(2,2)	input	Size of gridded plane
C	INTFNU	REAL(*)		input	Interpolation function
C	INTFNV	REAL(*)		input	Interpolation function
C	SUPP	INT(2,2)	input	Support of interpolation function
C	OSAMP	INT(2,2)	input	Over-sampling factor
C	SHIFT	REAL(3,3)	input	Shift matrix 
C
C Audit trail:
C
C	First version.  This has been tailored to match the requirements
C       of AIPS++ FFTTool.
C       Also, expects the convolution functions to be un-normalized 
C       (conv. functions generated in SDE are normalized to unit area).
C				S.Bhatnagar Sept. 4 1996
C
C------------------------------------------------------------------------
C
C
      INTEGER	NVIS, NAXIS1, NAXIS2, SUPP(2), OSAMP(2)
      INTEGER	ORIGIN(2)
      COMPLEX 	VIS(*), GVIS(NAXIS1,NAXIS2)
      REAL	WT(*)
      REAL	UVW(3,*), SCALE(2), OFFSET(2), INTFNU(*)
      REAL	INTFNV(*)
      DOUBLE PRECISION	SHIFT(3,3)
C
      CHARACTER*(*)	ROUTINE
      DOUBLE PRECISION  TWOPI, STOR
      PARAMETER	(ROUTINE = 'GRDDE2D', TWOPI=2*3.14159265358979323844,
     $     STOR=TWOPI/(360.0*3600.0))
C
      LOGICAL   DOSHIFT
      INTEGER 	IVIS, NBAD, CFOFFSET
      INTEGER	OFFU, UINT, UCEN, DELU, DELUI
      INTEGER	OFFV, VINT, VCEN, DELV, DELVI
      COMPLEX	ROT
      REAL	UCELL, VCELL, UVWT, SUMWT
      DOUBLE PRECISION	UG, VG, WG
      DOUBLE PRECISION	PHASE, ULOCAL, VLOCAL, WLOCAL
C
C==========================================================================
C
      CFOFFSET = (SUPP(1)+1)*OSAMP(1) + 1
C
      NBAD = 0
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Start of loop elements to be de-gridded
C
      DO 10 IVIS = 1, NVIS
         VIS(IVIS)=CMPLX(0.0,0.0)
         IF (WT(IVIS).LE.0.0) GO TO 10
         SUMWT = 0.0
         WT(IVIS) = - WT(IVIS)
         UG = - DBLE(UVW(1,IVIS))
         VG = - DBLE(UVW(2,IVIS))
         WG = - DBLE(UVW(3,IVIS))
         if (DOSHIFT) then
            ULOCAL = SHIFT(1,1) * UG + SHIFT(2,1) * VG +
     1           SHIFT(3,1) * WG
            VLOCAL = SHIFT(1,2) * UG + SHIFT(2,2) * VG +
     1           SHIFT(3,2) * WG
            WLOCAL = SHIFT(1,3) * UG + SHIFT(2,3) * VG +
     1           SHIFT(3,3) * WG
            PHASE = TWOPI * (WLOCAL - WG)
            ROT = CMPLX(COS(PHASE), SIN(PHASE))
            UCELL = SCALE(1) * ULOCAL + OFFSET(1)
            VCELL = SCALE(2) * VLOCAL + OFFSET(2)
         else
            UCELL = SCALE(1) * UG + OFFSET(1)
            VCELL = SCALE(2) * VG + OFFSET(2)
         endif
C
         IF ((NINT(UCELL)+SUPP(1)).GT.NAXIS1) GO TO 10
         IF ((NINT(UCELL)-SUPP(1)).LT.1)        GO TO 10
         IF ((NINT(VCELL)+SUPP(2)).GT.NAXIS2) GO TO 10
         IF ((NINT(VCELL)-SUPP(2)).LT.1)        GO TO 10
C
         DELUI = NINT((OSAMP(1)*(FLOAT(NINT(UCELL))-UCELL)))
         DELVI = NINT((OSAMP(2)*(FLOAT(NINT(VCELL))-VCELL)))
         UCEN = NINT(UCELL) - ORIGIN(1)
         VCEN = NINT(VCELL) - ORIGIN(2)
C
C No danger of axis problems: simple loop
C
         DO 140 OFFV = - SUPP(2), SUPP(2)
            DELV  = DELVI + OSAMP(2)*OFFV + CFOFFSET
            VINT = ORIGIN(2) + VCEN + OFFV
            DO 150 OFFU = - SUPP(1), SUPP(1)
               DELU  = DELUI + OSAMP(1)*OFFU + CFOFFSET
               UINT = ORIGIN(1) + UCEN + OFFU
               UVWT = INTFNU(DELU) * INTFNV(DELV) 
               VIS(IVIS) = VIS(IVIS) + GVIS(UINT, VINT)* UVWT
               SUMWT = SUMWT + UVWT
 150        CONTINUE
 140     CONTINUE
C
C Were there any good data for this point?
C
         IF (SUMWT.GT.0.0) THEN
            VIS(IVIS) = VIS(IVIS)/SUMWT
            WT(IVIS) = ABS(WT(IVIS))
         ELSE
            NBAD = NBAD + 1
            VIS(IVIS) = 0.0
         END IF
C
C Now finally phase rotate
C
         if (DOSHIFT) VIS(IVIS) = VIS(IVIS) * ROT
C
  10  CONTINUE
C
 999  CONTINUE
      END
