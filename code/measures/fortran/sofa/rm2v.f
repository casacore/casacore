      SUBROUTINE iau_RM2V ( R, W )
*+
*  - - - - - - - - -
*   i a u _ R M 2 V
*  - - - - - - - - -
*
*  Express an r-matrix as an r-vector.
*
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     R        d(3,3)    rotation matrix
*
*  Returned:
*     W        d(3)      rotation vector (Note 1)
*
*  Notes:
*
*  1) A rotation matrix describes a rotation about some arbitrary axis.
*     The axis is called the Euler axis, and the angle through which
*     the reference frame rotates is called the Euler angle.  The
*     "rotator vector" returned by this routine has the same direction
*     as the Euler axis, and its magnitude is the Euler angle in
*     radians.  (The magnitude and direction can be separated by means
*     of the routine iau_PN.)
*
*  2) If R is null, so is the result.  If R is not a rotation matrix
*     the result is undefined.  R must be proper (i.e. have a positive
*     determinant) and real orthogonal (inverse = transpose).
*
*  3) The reference frame rotates clockwise as seen looking along
*     the rotation vector from the origin.
*
*  This revision:  2000 December 15
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R(3,3), W(3)

      DOUBLE PRECISION X, Y, Z, S2, C2, PHI, F

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      X = R(2,3) - R(3,2)
      Y = R(3,1) - R(1,3)
      Z = R(1,2) - R(2,1)
      S2 = SQRT(X*X + Y*Y + Z*Z)
      IF ( S2 .NE. 0D0 ) THEN
         C2 = R(1,1) + R(2,2) + R(3,3) - 1D0
         PHI = ATAN2(S2,C2)
         F = PHI / S2
         W(1) = X * F
         W(2) = Y * F
         W(3) = Z * F
      ELSE
         W(1) = 0D0
         W(2) = 0D0
         W(3) = 0D0
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2005
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
