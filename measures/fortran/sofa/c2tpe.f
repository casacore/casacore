      SUBROUTINE iau_C2TPE ( TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP,
     :                       RC2T )
*+
*  - - - - - - - - - -
*   i a u _ C 2 T P E
*  - - - - - - - - - -
*
*  Form the celestial to terrestrial matrix given the date, the UT1, the
*  nutation and the polar motion.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     TTA,TTB     d       TT as a 2-part Julian Date (Note 1)
*     UTA,UTB     d       UT1 as a 2-part Julian Date (Note 1)
*     DPSI,DEPS   d       nutation (Note 2)
*     XP,YP       d       coordinates of the pole (radians, Note 3)
*
*  Returned:
*     RC2T      d(3,3)    celestial-to-terrestrial matrix (Note 4)
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
*  2) The caller is responsible for providing the nutation components;
*     they are in longitude and obliquity, in radians and are with
*     respect to the equinox and ecliptic of date.  For high-accuracy
*     applications, free core nutation should be included as well as
*     any other relevant corrections to the position of the CIP.
*
*  3) XP and YP are the "coordinates of the pole", in radians, which
*     position the Celestial Intermediate Pole in the International
*     Terrestrial Reference System (see IERS Conventions 2003).  In a
*     geocentric right-handed triad u,v,w, where the w-axis points at
*     the north geographic pole, the v-axis points towards the origin
*     of longitudes and the u axis completes the system, XP = +u and
*     YP = -v.
*
*  4) The matrix RC2T transforms from celestial to terrestrial
*     coordinates:
*
*        [TRS]  =  RPOM * R_3(GST) * RBPN * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003), RBPN is the
*     bias-precession-nutation matrix, GST is the Greenwich (apparent)
*     Sidereal Time and RPOM is the polar motion matrix.
*
*  Called:
*     iau_PN00     bias/precession/nutation results
*     iau_GMST00   Greenwich Mean Sidereal Time, IAU 2000
*     iau_SP00     the quantity s'
*     iau_EE00     equation of the equinoxes, IAU 2000
*     iau_POM00    polar motion matrix
*     iau_C2TEQX   form equinox-based celestial-to-terrestrial matrix
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

      DOUBLE PRECISION TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T(3,3)

      DOUBLE PRECISION EPSA, RB(3,3), RP(3,3), RBP(3,3), RN(3,3),
     :                 RBPN(3,3), GMST, EE, SP, RPOM(3,3)

      DOUBLE PRECISION iau_GMST00, iau_EE00, iau_SP00

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


*  Form the celestial-to-true matrix for this TT.
      CALL iau_PN00 ( TTA, TTB, DPSI, DEPS,
     :                EPSA, RB, RP, RBP, RN, RBPN )

*  Predict the Greenwich Mean Sidereal Time for this UT1 and TT.
      GMST = iau_GMST00 ( UTA, UTB, TTA, TTB )

*  Predict the equation of the equinoxes given TT and nutation.
      EE = iau_EE00 ( TTA, TTB, EPSA, DPSI)

*  Estimate s'.
      SP = iau_SP00 ( TTA, TTB)

*  Form the polar motion matrix.
      CALL iau_POM00 ( XP, YP, SP, RPOM )

*  Combine to form the celestial-to-terrestrial matrix.
      CALL iau_C2TEQX ( RBPN, GMST+EE, RPOM, RC2T )

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
