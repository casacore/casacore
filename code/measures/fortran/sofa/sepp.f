      SUBROUTINE iau_SEPP ( A, B, S )
*+
*  - - - - - - - - -
*   i a u _ S E P P
*  - - - - - - - - -
*
*  Angular separation between two p-vectors.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A        d(3)      first p-vector (not necessarily unit length)
*     B        d(3)      second p-vector (not necessarily unit length)
*
*  Returned:
*     S        d         angular separation (radians, always positive)
*
*  Notes:
*
*  1) If either vector is null, a zero result is returned.
*
*  2) The angular separation is most simply formulated in terms of
*     scalar product.  However, this gives poor accuracy for angles
*     near zero and pi.  The present algorithm uses both cross product
*     and dot product, to deliver full accuracy whatever the size of
*     the angle.
*
*  Called:
*     iau_PXP      vector product of two p-vectors
*     iau_PM       modulus of p-vector
*     iau_PDP      scalar product of the two p-vectors
*
*  This revision:  2005 August 26
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A(3), B(3), S

      DOUBLE PRECISION AXB(3), SS, CS

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Sine of the angle between the vectors, multiplied by the two moduli.
      CALL iau_PXP ( A, B, AXB )
      CALL iau_PM ( AXB, SS )

*  Cosine of the angle, multiplied by the two moduli.
      CALL iau_PDP ( A, B, CS )

*  The angle.
      IF ( SS.NE.0D0 .OR. CS.NE.0D0 ) THEN
         S = ATAN2(SS,CS)
      ELSE
         S = 0D0
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
