      SUBROUTINE iau_A2AF ( NDP, ANGLE, SIGN, IDMSF )
*+
*  - - - - - - - - -
*   i a u _ A 2 A F
*  - - - - - - - - -
*
*  Decompose radians into degrees, arcminutes, arcseconds, fraction.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     NDP       i        resolution (Note 1)
*     ANGLE     d        angle in radians
*
*  Returned:
*     SIGN      c        '+' or '-'
*     IDMSF     i(4)     degrees, arcminutes, arcseconds, fraction
*
*  Called:
*     iau_D2TF     decompose days to hms
*
*  Notes:
*
*  1) NDP is interpreted as follows:
*
*     NDP         resolution
*      :      ...0000 00 00
*     -7         1000 00 00
*     -6          100 00 00
*     -5           10 00 00
*     -4            1 00 00
*     -3            0 10 00
*     -2            0 01 00
*     -1            0 00 10
*      0            0 00 01
*      1            0 00 00.1
*      2            0 00 00.01
*      3            0 00 00.001
*      :            0 00 00.000...
*
*  2) The largest positive useful value for NDP is determined by the
*     size of ANGLE, the format of DOUBLE PRECISION floating-point
*     numbers on the target platform, and the risk of overflowing
*     IDMSF(4).  On a typical platform, for ANGLE up to 2pi, the
*     available floating-point precision might correspond to NDP=12.
*     However, the practical limit is typically NDP=9, set by the
*     capacity of a 32-bit IDMSF(4).
*
*  3) The absolute value of ANGLE may exceed 2pi.  In cases where it
*     does not, it is up to the caller to test for and handle the
*     case where ANGLE is very nearly 2pi and rounds up to 360 degrees,
*     by testing for IHMSF(1)=360 and setting IHMSF(1-4) to zero.
*
*  This revision:  2005 August 26
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IDMSF(4)

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER ( F = 15D0/D2PI )

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Scale then use days to h,m,s routine.
      CALL iau_D2TF ( NDP, ANGLE*F, SIGN, IDMSF )

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
