      SUBROUTINE iau_PN00 ( DATE1, DATE2, DPSI, DEPS,
     :                      EPSA, RB, RP, RBP, RN, RBPN )
*+
*  - - - - - - - - -
*   i a u _ P N 0 0
*  - - - - - - - - -
*
*  Precession-nutation, IAU 2000 model; a multi-purpose routine,
*  supporting classical, equinox-based, use directly and CEO-based
*  use indirectly.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DATE1,DATE2   d       TT as a 2-part Julian Date (Note 1)
*     DPSI,DEPS     d       nutation (Note 2)
*
*  Returned:
*     EPSA          d       mean obliquity (Note 3)
*     RB          d(3,3)    frame bias matrix (Note 4)
*     RP          d(3,3)    precession matrix (Note 5)
*     RBP         d(3,3)    bias-precession matrix (Note 6)
*     RN          d(3,3)    nutation matrix (Note 7)
*     RBPN        d(3,3)    GCRS-to-true matrix (Note 8)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The caller is responsible for providing the nutation components;
*     they are in longitude and obliquity, in radians and are with
*     respect to the equinox and ecliptic of date.  For high-accuracy
*     applications, free core nutation should be included as well as
*     any other relevant corrections to the position of the CIP.
*
*  3) The returned mean obliquity is consistent with the IAU 2000
*     precession-nutation models.
*
*  4) The matrix RB transforms vectors from GCRS to mean J2000 by
*     applying frame bias.
*
*  5) The matrix RP transforms vectors from mean J2000 to mean of date
*     by applying precession.
*
*  6) The matrix RBP transforms vectors from GCRS to mean of date by
*     applying frame bias then precession.  It is the product RP x RB.
*
*  7) The matrix RN transforms vectors from mean of date to true of date
*     by applying the nutation (luni-solar + planetary).
*
*  8) The matrix RBPN transforms vectors from GCRS to true of date
*     (CIP/equinox).  It is the product RN x RBP, applying frame bias,
*     precession and nutation in that order.
*
*  Called:
*     iau_PR00     IAU 2000 precession adjustments
*     iau_OBL80    mean obliquity, IAU 1980
*     iau_BP00     frame bias and precession matrices
*     iau_NUMAT    form nutation matrix
*     iau_RXR      r-matrix product
*
*  Reference:
*
*     Capitaine, N., Chapront, J., Lambert, S. and Wallace, P.,
*     "Expressions for the Celestial Intermediate Pole and Celestial
*     Ephemeris Origin consistent with the IAU 2000A precession-nutation
*     model", Astronomy & Astrophysics, 400, 1145-1154 (2003)
*
*  This revision:  2005 August 24
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, DPSI, DEPS,
     :                 EPSA, RB(3,3), RP(3,3), RBP(3,3),
     :                 RN(3,3), RBPN(3,3)

      DOUBLE PRECISION DPSIPR, DEPSPR

      DOUBLE PRECISION iau_OBL80

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  IAU 2000 precession-rate adjustments.
      CALL iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )

*  Mean obliquity, consistent with IAU 2000 precession-nutation.
      EPSA = iau_OBL80 ( DATE1, DATE2 ) + DEPSPR

*  Frame bias and precession matrices and their product.
      CALL iau_BP00 ( DATE1, DATE2, RB, RP, RBP )

*  Nutation matrix.
      CALL iau_NUMAT ( EPSA, DPSI, DEPS, RN )

*  Bias-precession-nutation matrix (classical).
      CALL iau_RXR ( RN, RBP, RBPN )

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
