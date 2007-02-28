      DOUBLE PRECISION FUNCTION iau_EPJ ( DJ1, DJ2 )
*+
*  - - - - - - - -
*   i a u _ E P J
*  - - - - - - - -
*
*  Julian Date to Julian Epoch.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DJ1,DJ2   d         Julian Date (see note)
*
*  The result is the Julian Epoch.
*
*  Note:
*
*     The Julian Date is supplied in two pieces, in the usual SOFA
*     manner, which is designed to preserve time resolution.  The
*     Julian Date is available as a single number by adding DJ1 and
*     DJ2.  The maximum resolution is achieved if DJ1 is 2451545D0
*     (J2000).
*
*  Reference:
*
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian year
      DOUBLE PRECISION DJY
      PARAMETER ( DJY = 365.25D0 )

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      iau_EPJ = 2000D0 + ( ( DJ1-DJ0 ) + DJ2 ) / DJY

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
