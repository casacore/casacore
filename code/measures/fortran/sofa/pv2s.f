      SUBROUTINE iau_PV2S ( PV, THETA, PHI, R, TD, PD, RD )
*+
*  - - - - - - - - -
*   i a u _ P V 2 S
*  - - - - - - - - -
*
*  Convert position/velocity from Cartesian to spherical coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     PV       d(3,2)    pv-vector
*
*  Returned:
*     THETA    d         longitude angle (radians)
*     PHI      d         latitude angle (radians)
*     R        d         radial distance
*     TD       d         rate of change of THETA
*     PD       d         rate of change of PHI
*     RD       d         rate of change of R
*
*  Notes:
*
*  1) If the position part of PV is null, THETA, PHI, TD and PD
*     are indeterminate.  This is handled by extrapolating the
*     position through unit time by using the velocity part of
*     PV.  This moves the origin without changing the direction
*     of the velocity component.  If the position and velocity
*     components of PV are both null, zeroes are returned for all
*     six results.
*
*  2) If the position is a pole, THETA, TD and PD are indeterminate.
*     In such cases zeroes are returned for THETA, TD and PD.
*
*  This revision:  2001 January 4
*
*  Copyright (C) 2003 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION PV(3,2), THETA, PHI, R, TD, PD, RD

      DOUBLE PRECISION X, Y, Z, XD, YD, ZD, RXY2, RXY, R2,
     :                 RTRUE, RW, XYP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Components of position/velocity vector.
      X =  PV(1,1)
      Y =  PV(2,1)
      Z =  PV(3,1)
      XD = PV(1,2)
      YD = PV(2,2)
      ZD = PV(3,2)

*  Component of R in XY plane squared.
      RXY2 = X*X + Y*Y

*  Modulus squared.
      R2 = RXY2 + Z*Z

*  Modulus.
      RTRUE = SQRT(R2)

*  If null vector, move the origin along the direction of movement.
      RW = RTRUE
      IF ( RTRUE .EQ. 0D0 ) THEN
         X = XD
         Y = YD
         Z = ZD
         RXY2 = X*X + Y*Y
         R2 = RXY2 + Z*Z
         RW = SQRT(R2)
      END IF

*  Position and velocity in spherical coordinates.
      RXY = SQRT(RXY2)
      XYP = X*XD + Y*YD
      IF ( RXY2 .NE. 0D0 ) THEN
         THETA = ATAN2(Y,X)
         PHI = ATAN2(Z,RXY)
         TD = ( X*YD - Y*XD ) / RXY2
         PD = ( ZD*RXY2 - Z*XYP ) / ( R2*RXY )
      ELSE
         THETA = 0D0
         IF ( Z.NE.0D0 ) THEN
            PHI = ATAN2(Z,RXY)
         ELSE
            PHI = 0D0
         END IF
         TD = 0D0
         PD = 0D0
      END IF
      R = RTRUE
      IF ( RW.NE.0D0 ) THEN
         RD = ( XYP + Z*ZD ) / RW
      ELSE
         RD = 0D0
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2003
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END
