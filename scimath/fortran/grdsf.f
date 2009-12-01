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
      SUBROUTINE GRDSF (NU, VAL)
C
CD Find Spheroidal function with M = 6, alpha = 1 using the rational
C approximations discussed by Fred Schwab in 'Indirect Imaging'.
C This routine was checked against Fred's SPHFN routine, and agreed
C to about the 7th significant digit.
C The gridding function is (1-NU**2)*GRDSF(NU) where NU is the distance
C to the edge. The grid correction function is just 1/GRDSF(NU) where NU
C is now the distance to the edge of the image.
C
C------------------------------------------------------------------------
C
      DOUBLE PRECISION	NU, VAL
C
      DOUBLE PRECISION	TOP, BOT, DELNUSQ, NUEND
      INTEGER	K, PART
      INTEGER	NP, NQ
      PARAMETER	(NP = 4)
      PARAMETER	(NQ = 2)
      DOUBLE PRECISION	P(0:NP,2), Q(0:NQ,2)
      DATA	P	/8.203343D-2, -3.644705D-1, 6.278660D-1,
     1			-5.335581D-1, 2.312756D-1,
     2			4.028559D-3, -3.697768D-2, 1.021332D-1,
     3			-1.201436D-1, 6.412774D-2/
      DATA	Q	/1.0000000D0, 8.212018D-1, 2.078043D-1,
     1			1.0000000D0, 9.599102D-1, 2.918724D-1/
C=======================================================================
      VAL = 0.0
C
      IF ((NU.GE.0.0).AND.(NU.LT.0.75)) THEN
         PART = 1
         NUEND = 0.75D0
      ELSEIF ((NU.GE.0.75).AND.(NU.LE.1.00)) THEN
         PART = 2
         NUEND = 1.00D0
      ELSE 
         VAL = 0.0
         GO TO 999
      END IF
C
      TOP = P(0,PART)
      DELNUSQ = NU**2 - NUEND**2
      DO 10 K = 1, NP
         TOP = TOP + P(K,PART) * DELNUSQ ** K
  10  CONTINUE
      BOT = Q(0,PART)
      DO 20 K = 1, NQ
         BOT = BOT + Q(K,PART) * DELNUSQ ** K
  20  CONTINUE
      IF (BOT.NE.0.0) THEN
         VAL = TOP/BOT
      ELSE
         VAL = 0.0
      END IF
C
 999  CONTINUE
      END 
