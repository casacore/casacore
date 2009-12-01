      DOUBLE PRECISION FUNCTION iau_GST00B ( UTA, UTB )
*+
*  - - - - - - - - - - -
*   i a u _ G S T 0 0 B
*  - - - - - - - - - - -
*
*  Greenwich Apparent Sidereal Time (consistent with IAU 2000
*  resolutions but using the truncated nutation model IAU 2000B).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     UTA, UTB     d      UT1 as a 2-part Julian Date (Notes 1,2)
*
*  The result is the Greenwich Apparent Sidereal Time (radians), in the
*  range 0 to 2pi.
*
*  Notes:
*
*  1) The UT1 date UTA+UTB is a Julian Date, apportioned in any
*     convenient way between the argument pair.  For example,
*     JD=2450123.7 could be expressed in any of these ways, among
*     others:
*
*             UTA            UTB
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in cases
*     where the loss of several decimal digits of resolution is
*     acceptable.  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  For UT, the date & time
*     method is best matched to the algorithm that is used by the Earth
*     Rotation Angle routine, called internally:  maximum accuracy (or,
*     at least, minimum noise) is delivered when the UTA argument is for
*     0hrs UT1 on the day in question and the UTB argument lies in the
*     range 0 to 1, or vice versa.
*
*  2) The result is compatible with the IAU 2000 resolutions, except
*     that accuracy has been compromised for the sake of speed and
*     convenience in two respects:
*
*     . UT is used instead of TDB (or TT) to compute the precession
*       component of GMST and the equation of the equinoxes.  This
*       results in errors of order 0.1 mas at present.
*
*     . The IAU 2000B abridged nutation model (McCarthy & Luzum, 2001)
*       is used, introducing errors of up to 1 mas.
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
*     iau_EE00B      equation of the equinoxes, IAU 2000B
*     iau_ANP        normalize angle into range 0 to 2pi
*
*  References:
*
*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
*     implement the IAU 2000 definition of UT1", Astronomy &
*     Astrophysics, 406, 1135-1149 (2003)
*
*     McCarthy, D.D. & Luzum, B.J., "An abridged model of the
*     precession-nutation of the celestial pole", Celestial Mechanics &
*     Dynamical Astronomy, 85, 37-49 (2003)
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

      DOUBLE PRECISION UTA, UTB

      DOUBLE PRECISION iau_ANP, iau_GMST00, iau_EE00B

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      iau_GST00B = iau_ANP ( iau_GMST00 ( UTA,UTB, UTA,UTB ) +
     :                       iau_EE00B ( UTA,UTB ) )

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
