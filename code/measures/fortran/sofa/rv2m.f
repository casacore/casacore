      SUBROUTINE iau_RV2M ( W, R )
*+
*  - - - - - - - - -
*   i a u _ R V 2 M
*  - - - - - - - - -
*
*  Form the r-matrix corresponding to a given r-vector.
*
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     W        d(3)      rotation vector (Note 1)
*
*  Returned:
*     R        d(3,3)    rotation matrix
*
*  Notes:
*
*  1) A rotation matrix describes a rotation about some arbitrary axis.
*     The axis is called the Euler axis, and the angle through which
*     the reference frame rotates is called the Euler angle.  The
*     "rotation vector" supplied to this routine has the same direction
*     as the Euler axis, and its magnitude is the Euler angle in
*     radians.
*
*  2) If W is null, the unit matrix is returned.
*
*  3) The reference frame rotates clockwise as seen looking along
*     the rotation vector from the origin.
*
*  This revision:  2003 January 20
*
*  Copyright (C) 2003 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION W(3), R(3,3)

      DOUBLE PRECISION X, Y, Z, PHI, S, C, F

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Euler angle (magnitude of rotation vector) and functions.
      X = W(1)
      Y = W(2)
      Z = W(3)
      PHI = SQRT(X*X + Y*Y + Z*Z)
      S = SIN(PHI)
      C = COS(PHI)
      F = 1D0 - C

*  Euler axis (direction of rotation vector), perhaps null.
      IF ( PHI .NE. 0D0 ) THEN
         X = X / PHI
         Y = Y / PHI
         Z = Z / PHI
      END IF

*  Form the rotation matrix.
      R(1,1) = X*X*F + C
      R(1,2) = X*Y*F + Z*S
      R(1,3) = X*Z*F - Y*S
      R(2,1) = Y*X*F - Z*S
      R(2,2) = Y*Y*F + C
      R(2,3) = Y*Z*F + X*S
      R(3,1) = Z*X*F + Y*S
      R(3,2) = Z*Y*F - X*S
      R(3,3) = Z*Z*F + C

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
