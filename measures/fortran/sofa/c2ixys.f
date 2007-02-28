      SUBROUTINE iau_C2IXYS ( X, Y, S, RC2I )
*+
*  - - - - - - - - - - -
*   i a u _ C 2 I X Y S
*  - - - - - - - - - - -
*
*  Form the celestial to intermediate-frame-of-date matrix given the CIP
*  X,Y and the quantity s.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     X,Y        d       Celestial Intermediate Pole (Note 1)
*     S          d       the quantity s (Note 2)
*
*  Returned:
*     RC2I     d(3,3)    celestial-to-intermediate matrix (Note 3)
*
*  Notes:
*
*  1) The Celestial Intermediate Pole coordinates are the x,y components
*     of the unit vector in the Geocentric Celestial Reference System.
*
*  2) The quantity s (in radians) positions the Celestial Ephemeris
*     Origin on the equator of the CIP.
*
*  3) The matrix RC2I is the first stage in the transformation from
*     celestial to terrestrial coordinates:
*
*        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003), ERA is the Earth
*     Rotation Angle and RPOM is the polar motion matrix.
*
*  Called:
*     iau_IR       initialize r-matrix to identity
*     iau_RZ       rotate around Z-axis
*     iau_RY       rotate around Y-axis
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

      DOUBLE PRECISION X, Y, S, RC2I(3,3)

      DOUBLE PRECISION R2, E, D

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Obtain the spherical angles E and d.
      R2 = X*X+Y*Y
      IF ( R2.NE.0D0 ) THEN
         E = ATAN2 ( Y, X )
      ELSE
         E = 0D0
      END IF
      D = ATAN ( SQRT ( R2 / (1D0-R2) ) )

*  Form the matrix.
      CALL iau_IR ( RC2I )
      CALL iau_RZ ( E, RC2I )
      CALL iau_RY ( D, RC2I )
      CALL iau_RZ ( -(E+S), RC2I )

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
