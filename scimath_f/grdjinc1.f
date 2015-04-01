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
*     $Id: grdsf.f 17791 2004-08-25 02:28:46Z cvsmgr $
*-----------------------------------------------------------------------
      SUBROUTINE GRDJINC1 (C, VAL, NORM, OUT)
C
C The jinc function for the first order Bessel function, which is 
C written as,
C 
C    OUT = J_1( PI * VAL / C ) / ( PI * VAL / C )
C
C where J_1 is the Bessel function of the first kind with order of 
C 1, which will be calculated by the subroutine BESSELJ1 defined 
C below.
C
C The name, jinc, may be an analogue of sinc function.
C
C If the parameter NORM is 1, then returned value will be normalized  
C such that OUT[0] = 1.0. Otherwise, the value will not be scaled 
C (In this case, OUT[0] = 0.5).
C
C------------------------------------------------------------------------
C
      DOUBLE PRECISION C, VAL
      INTEGER NORM
      DOUBLE PRECISION OUT
C
      DOUBLE PRECISION X
      PARAMETER (PI = 3.1415926535897931D0)
C=======================================================================
      IF (VAL .EQ. 0.0) THEN
         OUT = 0.5D0
      ELSE
         X = PI * VAL / C
         CALL BESSELJ1(X, OUT)
         OUT = OUT / X
      ENDIF

      IF (NORM .EQ. 1) THEN
         OUT = OUT / 0.5D0
      ENDIF

      END


C
C Bessel function of the first kind with order of 1, J_1(x)
C Approximate formula is taken from Numerical Recipe
C
      SUBROUTINE BESSELJ1( VAL, OUT )

      DOUBLE PRECISION VAL, OUT
C
      DOUBLE PRECISION AX, X, Y, Z, ANS1, ANS2
C=======================================================================
      OUT = 0.0

      AX = DABS(VAL)
      IF (AX .LT. 8.0D0) THEN
         Y = VAL * VAL
         ANS1 = VAL * (72362614232.0D0 + Y * (-7895059235.0D0 
     $        + Y * (242396853.1D0 + Y * (-2972611.439D0 
     $        + Y * (15704.48260D0 + Y * (-30.16036606D0))))))
         ANS2 = 144725228442.0D0 + Y * (2300535178.0D0 
     $        + Y * (18583304.74D0 + Y * (99447.43394D0 
     $        + Y * (376.9991397D0 + Y * 1.0D0))))
         OUT = ANS1 / ANS2
      ELSE 
         Z = 8.0D0 / AX
         Y = Z * Z
         X = AX - 2.356194491D0
         ANS1 = 1.0D0 + Y * (0.183105D-2 + Y * (-0.3516396496D-4
     $        + Y * (0.2457520174D-5 + Y * (-0.240337019D-6))))
         ANS2 = 0.04687499995D0 + Y * (-0.2002690873D-3
     $        + Y * (0.8449199096D-5 + Y * (-0.88228987D-6 
     $        + Y * (0.105787412D-6))))
         OUT = DSQRT(0.636619772D0 / AX) 
     $        * (DCOS(X) * ANS1 - Z * DSIN(X) * ANS2)

         IF (VAL .LT. 0.0D0) THEN
            OUT = - OUT
         ENDIF
      ENDIF
      END 
