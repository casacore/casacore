      SUBROUTINE iau_PVXPV ( A, B, AXB )
*+
*  - - - - - - - - - -
*   i a u _ P V X P V
*  - - - - - - - - - -
*
*  Outer (=vector=cross) product of two pv-vectors.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A        d(3,2)      first pv-vector
*     B        d(3,2)      second pv-vector
*
*  Returned:
*     AXB      d(3,2)      A x B
*
*  Note:
*
*     If the position and velocity components of the two pv-vectors are
*     ( Ap, Av ) and ( Bp, Bv ), the result, A x B, is the pair of
*     vectors ( Ap x Bp, Ap x Bv + Av x Bp ).  The two vectors are the
*     cross-product of the two p-vectors and its derivative.
*
*  Called:
*     iau_CPV      copy pv-vector
*     iau_PXP      outer product of two p-vectors
*     iau_PPP      p-vector addition
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A(3,2), B(3,2), AXB(3,2)

      DOUBLE PRECISION WA(3,2), WB(3,2), AXBD(3), ADXB(3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Make copies of the inputs.
      CALL iau_CPV ( A, WA )
      CALL iau_CPV ( B, WB )

*  A x B = position part of result.
      CALL iau_PXP ( WA(1,1), WB(1,1), AXB(1,1) )

*  A x Bdot + Adot x B = velocity part of result.
      CALL iau_PXP ( WA(1,1), WB(1,2), AXBD )
      CALL iau_PXP ( WA(1,2), WB(1,1), ADXB )
      CALL iau_PPP ( AXBD, ADXB, AXB(1,2) )

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
