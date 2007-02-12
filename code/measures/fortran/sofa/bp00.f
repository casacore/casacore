      SUBROUTINE iau_BP00 ( DATE1, DATE2, RB, RP, RBP )
*+
*  - - - - - - - - -
*   i a u _ B P 0 0
*  - - - - - - - - -
*
*  Frame bias and precession, IAU 2000.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2    d       TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     RB           d(3,3)    frame bias matrix (Note 2)
*     RP           d(3,3)    precession matrix (Note 3)
*     RBP          d(3,3)    bias-precession matrix (Note 4)
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
*  2) The matrix RB transforms vectors from GCRS to mean J2000 by
*     applying frame bias.
*
*  3) The matrix RP transforms vectors from mean J2000 to mean of date
*     by applying precession.
*
*  4) The matrix RBP transforms vectors from GCRS to mean of date by
*     applying frame bias then precession.  It is the product RP x RB.
*
*  Called:
*     iau_BI00     IAU 2000 frame bias components
*     iau_PR00     IAU 2000 precession adjustments
*     iau_IR       initialize r-matrix to identity
*     iau_RX       rotate around X-axis
*     iau_RY       rotate around Y-axis
*     iau_RZ       rotate around Z-axis
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

      DOUBLE PRECISION DATE1, DATE2, RB(3,3), RP(3,3), RBP(3,3)

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

*  J2000 obliquity (Lieske et al. 1977)
      DOUBLE PRECISION EPS0
      PARAMETER ( EPS0 = 84381.448D0 * DAS2R )

      DOUBLE PRECISION T, DPSIBI, DEPSBI, DRA0,
     :                 PSIA77, OMA77, CHIA, DPSIPR, DEPSPR, PSIA, OMA

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

*  Frame bias.
      CALL iau_BI00 ( DPSIBI, DEPSBI, DRA0 )

*  Precession angles (Lieske et al. 1977)
      PSIA77 =        ( 5038.7784D0 +
     :                (   -1.07259D0 +
     :                (   -0.001147D0 ) * T ) * T ) * T * DAS2R
      OMA77  = EPS0 + (
     :                (    0.05127D0 +
     :                (   -0.007726D0 ) * T ) * T ) * T * DAS2R
      CHIA   =        (   10.5526D0 +
     :                (   -2.38064D0 +
     :                (   -0.001125D0 ) * T ) * T ) * T * DAS2R

*  Apply IAU 2000 precession corrections.
      CALL iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )
      PSIA = PSIA77 + DPSIPR
      OMA  = OMA77  + DEPSPR

*  Frame bias matrix: GCRS to J2000.
      CALL iau_IR ( RB )
      CALL iau_RZ ( DRA0, RB )
      CALL iau_RY ( DPSIBI*SIN(EPS0), RB )
      CALL iau_RX ( -DEPSBI, RB )

*  Precession matrix: J2000 to mean of date.
      CALL iau_IR ( RP )
      CALL iau_RX ( EPS0, RP )
      CALL iau_RZ ( -PSIA, RP )
      CALL iau_RX ( -OMA, RP )
      CALL iau_RZ ( CHIA, RP )

*  Bias-precession matrix: GCRS to mean of date.
      CALL iau_RXR ( RP, RB, RBP )

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
