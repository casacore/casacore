      SUBROUTINE iau_C2TCEO ( RC2I, ERA, RPOM, RC2T )
*+
*  - - - - - - - - - - -
*   i a u _ C 2 T C E O
*  - - - - - - - - - - -
*
*  Assemble the celestial to terrestrial matrix from CEO-based
*  components (the celestial-to-intermediate matrix, the Earth Rotation
*  Angle and the polar motion matrix).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     RC2I     d(3,3)    celestial-to-intermediate matrix
*     ERA        d       Earth Rotation Angle
*     RPOM     d(3,3)    polar-motion matrix
*
*  Returned:
*     RC2T     d(3,3)    celestial-to-terrestrial matrix
*
*  Notes:
*
*  1) This routine constructs the rotation matrix that transforms
*     vectors in the celestial system into vectors in the terrestrial
*     system.  It does so starting from precomputed components, namely
*     the matrix which rotates from celestial coordinates to the
*     intermediate frame, the Earth Rotation Angle and the polar motion
*     matrix.  One use of the present routine is when generating a
*     series of celestial-to-terrestrial matrices where only the Earth
*     Rotation Angle changes, avoiding the considerable overhead of
*     recomputing the precession-nutation more often than necessary to
*     achieve given accuracy objectives.
*
*  2) The relationship between the arguments is as follows:
*
*        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003).
*
*  Called:
*     iau_CR       copy r-matrix
*     iau_RZ       rotate around Z-axis
*     iau_RXR      r-matrix product
*
*  Reference:
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*  This revision:  2005 August 24
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION RC2I(3,3), ERA, RPOM(3,3), RC2T(3,3)

      DOUBLE PRECISION R(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Construct the matrix.
      CALL iau_CR ( RC2I, R )
      CALL iau_RZ ( ERA, R )
      CALL iau_RXR ( RPOM, R, RC2T )

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
