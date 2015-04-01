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
      SUBROUTINE GRDGAUSS (HWHM, VAL, OUT)
C
C     Gaussian function with a radius of half-maximum, HWHM, which 
C     is written as,
C
C        OUT = exp( -ln(2.0)*(VAL/RHM)**2 )
C
C------------------------------------------------------------------------
C
      DOUBLE PRECISION	HWHM, VAL, OUT
C
      DOUBLE PRECISION :: LN2 = 0.69314718055994529D0
C=======================================================================
      OUT = DEXP( - LN2 * (VAL/HWHM) * (VAL/HWHM) )
      END 
