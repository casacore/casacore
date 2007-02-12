      SUBROUTINE iau_PREC76 ( EP01, EP02, EP11, EP12, ZETA, Z, THETA )
*+
*  - - - - - - - - - - -
*   i a u _ P R E C 7 6
*  - - - - - - - - - - -
*
*  IAU 1976 precession model.
*
*  This routine forms the three Euler angles which implement general
*  precession between two epochs, using the IAU 1976 model (as for
*  the FK5 catalog).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     EP01,EP02   d      TDB starting epoch (Note 1)
*     EP11,EP12   d      TDB ending epoch (Note 1)
*
*  Returned:
*     ZETA        d      1st rotation: radians clockwise around z
*     Z           d      3rd rotation: radians clockwise around z
*     THETA       d      2nd rotation: radians counterclockwise around y
*
*  Notes:
*
*  1) The epochs EP01+EP02 and EP11+EP12 are Julian Dates, apportioned
*     in any convenient way between the arguments EPn1 and EPn2.  For
*     example, JD(TDB)=2450123.7 could be expressed in any of these
*     ways, among others:
*
*             EPn1          EPn2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in cases
*     where the loss of several decimal digits of resolution is
*     acceptable.  The J2000 method is best matched to the way the
*     argument is handled internally and will deliver the optimum
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*     The two epochs may be expressed using different methods, but at
*     the risk of losing some resolution.
*
*  2) The accumulated precession angles zeta, z, theta are expressed
*     through canonical polynomials which are valid only for a limited
*     time span.  In addition, the IAU 1976 precession rate is known to
*     be imperfect.  The absolute accuracy of the present formulation is
*     better than 0.1 arcsec from 1960AD to 2040AD, better than 1 arcsec
*     from 1640AD to 2360AD, and remains below 3 arcsec for the whole of
*     the period 500BC to 3000AD.  The errors exceed 10 arcsec outside
*     the range 1200BC to 3900AD, exceed 100 arcsec outside 4200BC to
*     5600AD and exceed 1000 arcsec 1000 arcsec outside 6800BC to
*     8200AD.
*
*  3) The three angles are returned in the conventional order, which
*     is not the same as the order of the corresponding Euler rotations.
*     The precession matrix is R_3(-z) x R_2(+theta) x R_3(-zeta).
*
*  Reference:
*
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*      equations (6) & (7), p283.
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION EP01, EP02, EP11, EP12, ZETA, Z, THETA

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T0, T, TAS2R, W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and beginning epoch (JC).
      T0 = ( ( EP01-DJ0 ) + EP02 ) / DJC

*  Interval over which precession required (JC).
      T = ( ( EP11-EP01 ) + ( EP12-EP02 ) ) / DJC

*  Euler angles.
      TAS2R = T * DAS2R
      W = 2306.2181D0 + (
     :       1.39656D0
     :     - 0.000139D0 * T0 ) * T0

      ZETA = ( W + ( ( 0.30188D0
     :               - 0.000344D0 * T0 )
     :               + 0.017998D0 * T ) * T ) * TAS2R

      Z = ( W + ( ( 1.09468D0
     :            + 0.000066D0 * T0 )
     :            + 0.018203D0 * T ) * T ) * TAS2R

      THETA = ( ( 2004.3109D0 + (
     :             - 0.85330D0
     :             - 0.000217D0 * T0 ) * T0 ) + ( (
     :             - 0.42665D0
     :             - 0.000217D0 * T0 )
     :             - 0.041833D0 * T ) * T ) * TAS2R

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
