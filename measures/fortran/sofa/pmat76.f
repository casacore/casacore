      SUBROUTINE iau_PMAT76 ( EPOCH1, EPOCH2, RMATP )
*+
*  - - - - - - - - - - -
*   i a u _ P M A T 7 6
*  - - - - - - - - - - -
*
*  Precession matrix from J2000 to a specified date, IAU 1976 model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     EPOCH1,EPOCH2  d       ending epoch, TDB (Note 1)
*
*  Returned:
*     RMATP          d(3,3)  precession matrix, J2000 -> EPOCH1+EPOCH2
*
*  Notes:
*
*  1) The ending epoch EPOCH1+EPOCH2 is a Julian Date, apportioned
*     in any convenient way between the arguments EPOCH1 and EPOCH2.
*     For example, JD(TDB)=2450123.7 could be expressed in any of
*     these ways, among others:
*
*            EPOCH1        EPOCH2
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
*  2) The matrix operates in the sense V(date) = RMATP * V(J2000),
*     where the p-vector V(J2000) is with respect to the mean
*     equatorial triad of epoch J2000 and the p-vector V(date)
*     is with respect to the mean equatorial triad of the given
*     epoch.
*
*  3) Though the matrix method itself is rigorous, the precession
*     angles are expressed through canonical polynomials which are
*     valid only for a limited time span.  In addition, the IAU 1976
*     precession rate is known to be imperfect.  The absolute accuracy
*     of the present formulation is better than 0.1 arcsec from
*     1960AD to 2040AD, better than 1 arcsec from 1640AD to 2360AD,
*     and remains below 3 arcsec for the whole of the period
*     500BC to 3000AD.  The errors exceed 10 arcsec outside the
*     range 1200BC to 3900AD, exceed 100 arcsec outside 4200BC to
*     5600AD and exceed 1000 arcsec outside 6800BC to 8200AD.
*
*  Called:
*     iau_PREC76  accumulated precession angles, IAU 1976
*     iau_IR      initialize r-matrix to identity
*     iau_RZ      rotate around Z-axis
*     iau_RY      rotate around Y-axis
*     iau_CR      copy r-matrix
*
*  References:
*
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*      equations (6) & (7), p283.
*
*     Kaplan,G.H., 1981. USNO circular no. 163, pA2.
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION EPOCH1, EPOCH2, RMATP(3,3)

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

      DOUBLE PRECISION ZETA, Z, THETA, WMAT(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Precession Euler angles, J2000 to specified epoch.
      CALL iau_PREC76 ( DJ0, 0D0, EPOCH1, EPOCH2, ZETA, Z, THETA )

*  Form the rotation matrix.
      CALL iau_IR ( WMAT )
      CALL iau_RZ ( -ZETA, WMAT )
      CALL iau_RY ( THETA, WMAT )
      CALL iau_RZ ( -Z, WMAT )
      CALL iau_CR ( WMAT, RMATP )

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
