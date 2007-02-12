      SUBROUTINE iau_PAP ( A, B, THETA )
*+
*  - - - - - - - -
*   i a u _ P A P
*  - - - - - - - -
*
*  Position-angle from two p-vectors.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A        d(3)      direction of reference point
*     B        d(3)      direction of point whose PA is required
*
*  Returned:
*     THETA    d         position angle of B with respect to A (radians)
*
*  Notes:
*
*  1) The result is the position angle, in radians, of direction B with
*     respect to direction A.  It is in the range -pi to +pi.  The sense
*     is such that if B is a small distance "north" of A the position
*     angle is approximately zero, and if B is a small distance "east" of
*     A the position angle is approximately +pi/2.
*
*  2) A and B need not be unit vectors.
*
*  3) Zero is returned if the two directions are the same or if either
*     vector is null.
*
*  4) If A is at a pole, the result is ill-defined.
*
*  Called:
*     iau_PN       separate p-vector into modulus and direction
*     iau_PM       modulus of p-vector
*     iau_PXP      vector product of two p-vectors
*     iau_PMP      p-vector minus p-vector
*     iau_PDP      scalar product of two p-vectors
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A(3), B(3), THETA

      DOUBLE PRECISION AM, AU(3), BM, ST, CT, XA, YA, ZA, ETA(3),
     :                 XI(3), A2B(3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Modulus and direction of the A vector.
      CALL iau_PN ( A, AM, AU )

*  Modulus of the B vector.
      CALL iau_PM ( B, BM )

*  Deal with the case of a null vector.
      IF ( AM.EQ.0D0 .OR. BM.EQ.0D0 ) THEN
         ST = 0D0
         CT = 1D0
      ELSE

*     The "north" axis tangential from A (arbitrary length).
         XA = A(1)
         YA = A(2)
         ZA = A(3)
         ETA(1) = - XA * ZA
         ETA(2) = - YA * ZA
         ETA(3) = XA*XA + YA*YA

*     The "east" axis tangential from A (same length).
         CALL iau_PXP ( ETA, AU, XI )

*     The vector from A to B.
         CALL iau_PMP ( B, A, A2B )

*     Resolve into components along the north and east axes.
         CALL iau_PDP ( A2B, XI, ST )
         CALL iau_PDP ( A2B, ETA, CT )

*     Deal with degenerate cases.
         IF ( ST.EQ.0D0 .AND. CT.EQ.0D0 ) CT = 1D0

      END IF

*  Position angle.
      THETA = ATAN2(ST,CT)

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
