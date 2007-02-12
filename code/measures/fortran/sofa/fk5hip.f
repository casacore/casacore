      SUBROUTINE iau_FK5HIP ( R5H, S5H )
*+
*  - - - - - - - - - - -
*   i a u _ F K 5 H I P
*  - - - - - - - - - - -
*
*  FK5 to Hipparcos rotation and spin.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Returned:
*     R5H     d(3,3)   r-matrix: FK5 rotation wrt Hipparcos (Note 2)
*     S5H     d(3)     r-vector: FK5 spin wrt Hipparcos (Note 3)
*
*  Notes:
*
*  1) This routine models the FK5 to Hipparcos transformation as a
*     pure rotation and spin;  zonal errors in the FK5 catalogue are
*     not taken into account.
*
*  2) The r-matrix R5H operates in the sense:
*
*           P_Hipparcos = R5H x P_FK5
*
*     where P_FK5 is a p-vector in the FK5 frame, and P_Hipparcos is
*     the equivalent Hipparcos p-vector.
*
*  3) The r-vector S5H represents the time derivative of the FK5 to
*     Hipparcos rotation.  The units are radians per year (Julian,
*     TDB).
*
*  Called:
*     iau_RV2M    r-vector to r-matrix
*
*  Reference:
*
*     F.Mignard & M.Froeschle, Astron. Astrophys. 354, 732-739 (2000).
*
*  This revision:  2001 August 1
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R5H(3,3), S5H(3)

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  FK5 wrt Hipparcos orientation and spin (radians, radians/year)
      DOUBLE PRECISION EPX, EPY, EPZ
      DOUBLE PRECISION OMX, OMY, OMZ

      PARAMETER ( EPX = -19.9D-3 * DAS2R,
     :            EPY =  -9.1D-3 * DAS2R,
     :            EPZ = +22.9D-3 * DAS2R )

      PARAMETER ( OMX = -0.30D-3 * DAS2R,
     :            OMY = +0.60D-3 * DAS2R,
     :            OMZ = +0.70D-3 * DAS2R )

      DOUBLE PRECISION V(3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  FK5 to Hipparcos orientation expressed as an r-vector.
      V(1) = EPX
      V(2) = EPY
      V(3) = EPZ

*  Re-express as an r-matrix.
      CALL iau_RV2M ( V, R5H )

*  Hipparcos wrt FK5 spin expressed as an r-vector.
      S5H(1) = OMX
      S5H(2) = OMY
      S5H(3) = OMZ

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
