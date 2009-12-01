*-----------------------------------------------------------------------
*     ADDGRD: Add values with weights to an array.
*-----------------------------------------------------------------------
*
*     Copyright (C) 1995
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
*     ADDGRD contains a set of nearly identical subroutines that are
*     used by specific instances of the templated class GridTool<T,S>
*     when gridding real data.  The execution speed of GridTool is
*     greatly enhanced by these subroutines.  Each specific type of
*     T in GridTool<T,S> requires a separate version of ADDGRD (the
*     S type is not used by the real gridding versions of GridTool).
*
*     The following subroutines are found here:
*     ADGRDF : for Arrays of Float.
*     ADGRDD : for Arrays of Double
*     It should be trivial to clone these routines for any other type.
*
*     The following comments apply to all subroutines found here:
*
*     The general form of the subroutine call is (x is replaced by
*     a character indicating the type: F for Float, D for Double, etc.).
*
*     SUBROUTINE ADGRDx(NVALS, START, INCR, CELLWT, DATA, GRID, WEIGHT)
*     Given:
*          NVALS    I     The number of points to be gridded.
*          START    I     The starting location in GRID and WEIGHT.
*          INCR     I     The increment to use between successive
*                         locations in GRID and WEIGHT.
*          CELLWT   x     A global weight.  Applied to all data values.
*          DATA(2*NVALS)
*                   x     Data array (includes data and associated
*                         weights, see note 1 below).
*
*     Given and returned:
*          GRID(START+INCR*(NVALS-1))
*                   x    The array holding the gridded data.
*          WEIGHT(START+INCR*(NVALS-1))
*                   x    The array holding the sum of the weights.
*
*     Notes:
*       1) DATA consists of 2*NVALS values with the values to be added
*          to the grid at the odd numbered locations of DATA (1,3,5...)
*          and the associated weights at the even numbered locations of
*          DATA (2,4,6...).
*
*       2) The total weight given to each data point is the global
*          weight, CELLWT, multiplied by the weight for that data point
*          (from the associated even number of DATA).  A weight less
*          than zero implies that the associated data value is NOT to be
*          added to GRID (this is consistent with GridTool usage).
*
*       3) Each data value is multipled by the total weight and added
*          to the existing value of GRID.  The total weight is added
*          to the existing value of WEIGHT.  The addition of values to
*          GRID and WEIGHT starts at location START and continues at
*          each INCR (increment) location after START until all NVALS
*          values have been used.
*
*       4) Keep in mind that the first element in a fortran array is
*          element number 1.
*-----------------------------------------------------------------------

      SUBROUTINE ADGRDF(NVALS, START, INCR, CELLWT, DATA, GRID, WEIGHT)

*     Add values to an array of Float

      INTEGER INCR, J, K, NVALS, OFFSET, START
      REAL    CELLWT, DATA(2*NVALS), GRID(START+INCR*(NVALS-1)), TOTWT,
     *        WEIGHT(START+INCR*(NVALS-1))
*-----------------------------------------------------------------------
      OFFSET = 0
      DO 10 J = 2, 2*NVALS, 2
          IF (DATA(J).GE.0.0) THEN
              TOTWT = CELLWT * DATA(J)
              K = START + OFFSET
              GRID(K) = GRID(K) + DATA(J-1)*TOTWT
              WEIGHT(K) = WEIGHT(K) + TOTWT
          END IF
          OFFSET = OFFSET + INCR
 10   CONTINUE
 
      RETURN
      END
*-----------------------------------------------------------------------

      SUBROUTINE ADGRDD(NVALS, START, INCR, CELLWT, DATA, GRID, WEIGHT)

*     Add values to an array of Double

      INTEGER INCR, J, K, NVALS, OFFSET, START
      DOUBLE PRECISION CELLWT, DATA(2*NVALS), 
     *        GRID(START+INCR*(NVALS-1)), TOTWT, 
     *        WEIGHT(START+INCR*(NVALS-1))
*-----------------------------------------------------------------------
      OFFSET = 0
      DO 10 J = 2, 2*NVALS, 2
          IF (DATA(J).GE.0.0) THEN
              TOTWT = CELLWT * DATA(J)
              K = START + OFFSET
              GRID(K) = GRID(K) + DATA(J-1)*TOTWT
              WEIGHT(K) = WEIGHT(K) + TOTWT
          END IF
          OFFSET = OFFSET + INCR
 10   CONTINUE
 
      RETURN
      END
