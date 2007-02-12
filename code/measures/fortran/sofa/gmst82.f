      DOUBLE PRECISION FUNCTION iau_GMST82 ( DJ1, DJ2 )
*+
*  - - - - - - - - - - -
*   i a u _ G M S T 8 2
*  - - - - - - - - - - -
*
*  Universal Time to Greenwich Mean Sidereal Time (IAU 1982 model).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DJ1, DJ2     d      UT1 Julian Date (see note)
*
*  The result is the Greenwich Mean Sidereal Time (radians), in the
*  range 0 to 2pi.
*
*  Notes:
*
*  1) The UT1 epoch DJ1+DJ2 is a Julian Date, apportioned in any
*     convenient way between the arguments DJ1 and DJ2.  For example,
*     JD(UT1)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  The date & time method is
*     best matched to the algorithm used:  maximum accuracy (or, at
*     least, minimum noise) is delivered when the DJ1 argument is for
*     0hrs UT1 on the day in question and the DJ2 argument lies in the
*     range 0 to 1, or vice versa.
*
*  2) The algorithm is based on the IAU 1982 expression.  This is always
*     described as giving the GMST at 0 hours UT1.  In fact, it gives the
*     difference between the GMST and the UT, the steady 4-minutes-per-day
*     drawing-ahead of ST with respect to UT.  When whole days are ignored,
*     the expression happens to equal the GMST at 0 hours UT1 each day.
*
*  3) In this routine, the entire UT1 (the sum of the two arguments DJ1
*     and DJ2) is used directly as the argument for the standard formula,
*     the constant term of which is adjusted by 12 hours to take account
*     of the noon phasing of Julian Date.  The UT1 is then added, but
*     omitting whole days to conserve accuracy.
*
*  Called:
*     iau_ANP        normalize angle into range 0 to 2pi
*
*  References:
*
*     Transactions of the International Astronomical Union,
*     XVIII B, 67 (1983).
*
*     Aoki et al., Astron. Astrophys. 105, 359-361 (1982).
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2

      DOUBLE PRECISION DS2R
      PARAMETER ( DS2R = 7.272205216643039903848712D-5 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Seconds per day, days per Julian century
      DOUBLE PRECISION DAYSEC, CENDAY
      PARAMETER ( DAYSEC = 86400D0, CENDAY = 36525D0 )

*  Coefficients of IAU 1982 GMST-UT1 model
      DOUBLE PRECISION A, B, C, D
      PARAMETER ( A = 24110.54841D0 - DAYSEC/2D0,
     :            B = 8640184.812866D0,
     :            C = 0.093104D0,
     :            D = -6.2D-6 )

*  Note: the first constant, A, has to be adjusted by 12 hours because
*  the UT1 is supplied as a Julian date, which begins at noon.

      DOUBLE PRECISION D1, D2, T, F

      DOUBLE PRECISION iau_ANP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Julian centuries since fundamental epoch.
      IF ( DJ1 .LT. DJ2 ) THEN
         D1 = DJ1
         D2 = DJ2
      ELSE
         D1 = DJ2
         D2 = DJ1
      END IF
      T = ( D1 + ( D2-DJ0 ) ) / CENDAY

*  Fractional part of JD(UT1), in seconds.
      F = DAYSEC * ( MOD(D1,1D0) + MOD(D2,1D0) )

*  GMST at this UT1.
      iau_GMST82 = iau_ANP ( DS2R * ( (A+(B+(C+D*T)*T)*T) + F ) )

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
