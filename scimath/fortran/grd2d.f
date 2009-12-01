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
      SUBROUTINE GRD2D (VIS, WT, UVW, NVIS, SCALE, OFFSET, 
     1     ORIGIN, GVIS, PSF, NAXIS1, NAXIS2, GRIDFNU, GRIDFNV, 
     1     SUPP, OSAMP, SHIFT, SUMWT)
C
CD Grid two dimensional complex data.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C   The phase center of the data can be adjusted as it is gridded using
C the shift matrix.
C
C	VIS	CMPLX(*)	input	Non-gridded data
C	WT	REAL(*)		input	Weights
C	UVW	REAL(3,*)	input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	SCALE	REAL(2)		input	Scaling factor to get to pixels
C	OFFSET	REAL(2)		input	Offset to get to pixels
C	ORIGIN	INT(2)		input	Origin of u axis
C	GVIS	CMPLX(*)	output	Gridded data
C	PSF	LOG		input	TRUE for PSF
C	NAXIS	INT(2)		input	Size of gridded plane
C	GRIDFNU	REAL(*)		input	Gridding function
C	GRIDFNV	REAL(*)		input	Gridding function
C	SUPP	INT(2)		input	Support of gridding function
C	SAMPU	INT(2)		input	Over-sampling factor
C	SHIFT	REAL(3,3)	input	Shift matrix
C	SUMWT	REAL		output	Sum of weights
C
C Audit trail:
C     First version.  This has been tailored to match the requirements 
C     of AIPS++ FFTTool.
C     Also, expects the convolution functions to be un-normalized 
C     (conv. functions generated in SDE are normalized to unit area).
C				S.Bhatnagar 27 Aug. 1996
C
C------------------------------------------------------------------------
C
C
      INTEGER	NVIS, NAXIS1, NAXIS2, SUPP(2), OSAMP(2)
      INTEGER	ORIGIN(2)
      COMPLEX 	VIS(*), GVIS(NAXIS1, NAXIS2)
      LOGICAL	PSF
      REAL	WT(*)
      REAL	UVW(3,*), SCALE(2), OFFSET(2)
      REAL      GRIDFNU(*),GRIDFNV(*)
      DOUBLE PRECISION	SHIFT(3,*)
      REAL	SUMWT
C
      CHARACTER*(*)	ROUTINE
      DOUBLE PRECISION  TWOPI
      PARAMETER	(ROUTINE = 'GRD2D', TWOPI=3.14159265358979323844*2)
C
      LOGICAL	DOSHIFT
      INTEGER 	IVIS, CFOFFSET
      INTEGER	OFFU, UGRID, DELU, DELUI, UCEN
      INTEGER	OFFV, VGRID, DELV, DELVI, VCEN
      COMPLEX	FVIS
      DOUBLE PRECISION	UG, VG, WG
      DOUBLE PRECISION	PHASE, ULOCAL, VLOCAL, WLOCAL
      DOUBLE PRECISION 	UCELL, VCELL, UVWT, LSUMWT
C==========================================================================
C
      CFOFFSET = (SUPP(1)+1)*OSAMP(1) + 1
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Start of loop elements to be gridded
C
      LSUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
C
C Shift to new phase center if required.
C
         UG = - DBLE(UVW(1,IVIS))
         VG = - DBLE(UVW(2,IVIS))
         WG = - DBLE(UVW(3,IVIS))
         IF (DOSHIFT) THEN
            ULOCAL = SHIFT(1,1) * UG + SHIFT(2,1) * VG +
     1         SHIFT(3,1) * WG
            VLOCAL = SHIFT(1,2) * UG + SHIFT(2,2) * VG +
     1         SHIFT(3,2) * WG
            WLOCAL = SHIFT(1,3) * UG + SHIFT(2,3) * VG +
     1         SHIFT(3,3) * WG
            PHASE = TWOPI * (WLOCAL - WG)
            FVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
            UCELL = SCALE(1) * ULOCAL + OFFSET(1) 
            VCELL = SCALE(2) * VLOCAL + OFFSET(2)
         ELSE
            FVIS = VIS(IVIS)
            UCELL = SCALE(1) * UG + OFFSET(1)
            VCELL = SCALE(2) * VG + OFFSET(2)
         END IF
C
C Accumulate sum of weights
C
         SUMWT = SUMWT + WT(IVIS)
C
C Find offsets within convolution function and center point of
C gridded point. At his point we offset the grid by SUPP(1) so
C that we don't have to worry about edge effects on the v-axis.
C
         DELUI = NINT(OSAMP(1)*(FLOAT(NINT(UCELL))-UCELL)) + CFOFFSET
         DELVI = NINT(OSAMP(2)*(FLOAT(NINT(VCELL))-VCELL)) + CFOFFSET
         UCEN = NINT(UCELL)
C + SUPP(1)
         VCEN = NINT(VCELL)
         if (UCEN+SUPP(1) .GT. NAXIS1) goto 10
         if (UCEN-SUPP(1) .LT. 1)        goto 10
         if (VCEN+SUPP(2) .GT. NAXIS2) goto 10
         if (VCEN-SUPP(2) .LT. 1)        goto 10
C
C There is no problem with running into an axis so just plunge right
C in. 
C ********************************************************************
C This loop produces different answers on the sparc and on the
C IBM6000. UCEN, VCEN, DELUI, DELVI are the same as are GRIDFNU, GRIDFNV
C The difference is that one pixel (?) is misplaced by two pixels in
C v. I don't know which is correct. This same error also afflicts
C GRDH23D. TJC March 3 1991
C
         DO 140 OFFV = - SUPP(2), SUPP(2)
            DELV  = DELVI + OSAMP(2)*OFFV
            VGRID = VCEN + OFFV
            DO 150 OFFU = - SUPP(1), SUPP(1)
               DELU  = DELUI + OSAMP(1)*OFFU
               UGRID = UCEN + OFFU
               UVWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV)
               GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + UVWT * FVIS
               LSUMWT = LSUMWT + UVWT
 150        CONTINUE
 140     CONTINUE
C
C ********************************************************************
C
  10  CONTINUE
C
 999  CONTINUE
      END
