      SUBROUTINE iau_C2T00A ( TTA, TTB, UTA, UTB, XP, YP, RC2T )
*+
*  - - - - - - - - - - -
*   i a u _ C 2 T 0 0 A
*  - - - - - - - - - - -
*
*  Form the celestial to terrestrial matrix given the date, the UT1 and
*  the polar motion, using the IAU 2000A nutation model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     TTA,TTB    d       TT as a 2-part Julian Date (Note 1)
*     UTA,UTB    d       UT1 as a 2-part Julian Date (Note 1)
*     XP,YP      d       coordinates of the pole (radians, Note 2)
*
*  Returned:
*     RC2T     d(3,3)    celestial-to-terrestrial matrix (Note 3)
*
*  Notes:
*
*  1) The TT and UT1 dates TTA+TTB and UTA+UTB are Julian Dates,
*     apportioned in any convenient way between the arguments UTA and
*     UTB.  For example, JD(UT1)=2450123.7 could be expressed in any of
*     these ways, among others:
*
*             UTA            UTB
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution is
*     acceptable.  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  In the case of UTA,UTB, the
*     date & time method is best matched to the Earth Rotation Angle
*     algorithm used:  maximum accuracy (or, at least, minimum noise) is
*     delivered when the UTA argument is for 0hrs UT1 on the day in
*     question and the UTB argument lies in the range 0 to 1, or vice
*     versa.
*
*  2) XP and YP are the "coordinates of the pole", in radians, which
*     position the Celestial Intermediate Pole in the International
*     Terrestrial Reference System (see IERS Conventions 2003).  In a
*     geocentric right-handed triad u,v,w, where the w-axis points at
*     the north geographic pole, the v-axis points towards the origin
*     of longitudes and the u axis completes the system, XP = +u and
*     YP = -v.
*
*  3) The matrix RC2T transforms from celestial to terrestrial
*     coordinates:
*
*        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003), RC2I is the
*     celestial-to-intermediate matrix, ERA is the Earth Rotation Angle
*     and RPOM is the polar motion matrix.
*
*  4) A faster, but slightly less accurate result (about 1 mas), can be
*     obtained by using instead the iau_C2T00B routine.  n.b. The
*     argument list for the latter omits SP.
*
*  Called:
*     iau_C2I00A   celestial-to-intermediate matrix, IAU 2000A
*     iau_ERA00    Earth Rotation Angle, IAU 2000
*     iau_SP00     the quantity s'
*     iau_POM00    polar motion matrix
*     iau_C2TCEO   construct CEO-based celestial-to-terrestrial matrix
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

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)

      DOUBLE PRECISION RC2I(3,3), ERA, SP, RPOM(3,3)

      DOUBLE PRECISION iau_ERA00, iau_SP00

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Form the celestial-to-intermediate matrix for this TT (IAU 2000A).
      CALL iau_C2I00A ( TTA, TTB, RC2I )

*  Predict the Earth Rotation Angle for this UT1.
      ERA = iau_ERA00 ( UTA, UTB )

*  Estimate s'.
      SP = iau_SP00 ( TTA, TTB )

*  Form the polar motion matrix.
      CALL iau_POM00 ( XP, YP, SP, RPOM )

*  Combine to form the celestial-to-terrestrial matrix.
      CALL iau_C2TCEO ( RC2I, ERA, RPOM, RC2T )

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
