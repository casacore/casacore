      DOUBLE PRECISION FUNCTION iau_GST00A ( UTA, UTB, TTA, TTB )
*+
*  - - - - - - - - - - -
*   i a u _ G S T 0 0 A
*  - - - - - - - - - - -
*
*  Greenwich Apparent Sidereal Time (consistent with IAU 2000
*  resolutions).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     UTA, UTB     d      UT1 as a 2-part Julian Date (Notes 1,2)
*     TTA, TTB     d      TT as a 2-part Julian Date (Notes 1,2)
*
*  The result is the Greenwich Apparent Sidereal Time (radians), in the
*  range 0 to 2pi.
*
*  Notes:
*
*  1) The UT1 and TT dates UTA+UTB and TTA+TTB respectively, are both
*     Julian Dates, apportioned in any convenient way between the
*     argument pairs.  For example, JD=2450123.7 could be expressed in
*     any of these ways, among others:
*
*            Part A         Part B
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable (in the case of UT;  the TT is not at all critical
*     in this respect).  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  For UT, the date & time
*     method is best matched to the algorithm that is used by the Earth
*     Rotation Angle routine, called internally:  maximum accuracy (or,
*     at least, minimum noise) is delivered when the UTA argument is for
*     0hrs UT1 on the day in question and the UTB argument lies in the
*     range 0 to 1, or vice versa.
*
*  2) Both UT1 and TT are required, UT1 to predict the Earth rotation
*     and TT to predict the effects of precession-nutation.  If UT1 is
*     used for both purposes, errors of order 100 microarcseconds
*     result.
*
*  3) This GAST is compatible with the IAU 2000 resolutions and must be
*     used only in conjunction with other IAU 2000 compatible components
*     such as precession-nutation.
*
*  4) The algorithm is from Capitaine et al. (2003) and IERS Conventions
*     2003.
*
*  Called:
*     iau_GMST00     Greenwich Mean Sidereal Time, IAU 2000
*     iau_EE00A      equation of the equinoxes, IAU 2000A
*     iau_ANP        normalize angle into range 0 to 2pi
*
*  References:
*
*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
*     implement the IAU 2000 definition of UT1", Astronomy &
*     Astrophysics, 406, 1135-1149 (2003)
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

      DOUBLE PRECISION UTA, UTB, TTA, TTB

      DOUBLE PRECISION iau_ANP, iau_GMST00, iau_EE00A

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      iau_GST00A = iau_ANP ( iau_GMST00 ( UTA,UTB, TTA,TTB ) +
     :                       iau_EE00A ( TTA,TTB ) )

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
