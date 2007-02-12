      SUBROUTINE iau_FK52H ( R5, D5, DR5, DD5, PX5, RV5,
     :                       RH, DH, DRH, DDH, PXH, RVH )
*+
*  - - - - - - - - - -
*   i a u _ F K 5 2 H
*  - - - - - - - - - -
*
*  Transform FK5 (J2000) star data into the Hipparcos frame.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given (all FK5, equinox J2000, epoch J2000):
*     R5        d      RA (radians)
*     D5        d      Dec (radians)
*     DR5       d      proper motion in RA (dRA/dt, rad/Jyear)
*     DD5       d      proper motion in Dec (dDec/dt, rad/Jyear)
*     PX5       d      parallax (arcsec)
*     RV5       d      radial velocity (positive = receding)
*
*  Returned (all Hipparcos, epoch J2000):
*     RH        d      RA (radians)
*     DH        d      Dec (radians)
*     DRH       d      proper motion in RA (dRA/dt, rad/Jyear)
*     DDH       d      proper motion in Dec (dDec/dt, rad/Jyear)
*     PXH       d      parallax (arcsec)
*     RVH       d      radial velocity (positive = receding)
*
*  Notes:
*
*  1) This routine transforms FK5 star positions and proper motions
*     into the frame of the Hipparcos catalogue.
*
*  2) The proper motions in RA are dRA/dt rather than
*     cos(Dec)*dRA/dt, and are per year rather than per century.
*
*  3) The FK5 to Hipparcos transformation is modeled as a pure
*     rotation and spin;  zonal errors in the FK5 catalogue are
*     not taken into account.
*
*  4) See also iau_H2FK5, iau_FK5HZ, iau_HFK5Z.
*
*  Called:
*     iau_STARPV  star catalog data to pv-vector
*     iau_FK5HIP  FK5 to Hipparcos rotation and spin
*     iau_RXP     product of r-matrix and p-vector
*     iau_PXP     outer (=vector=cross) product of two p-vectors
*     iau_PPP     p-vector plus p-vector
*     iau_PVSTAR  pv-vector to star catalog data
*
*  Reference:
*
*     F.Mignard & M.Froeschle, Astron. Astrophys. 354, 732-739 (2000).
*
*  This revision:  2005 August 26
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R5, D5, DR5, DD5, PX5, RV5,
     :                 RH, DH, DRH, DDH, PXH, RVH

      DOUBLE PRECISION PV5(3,2), R5H(3,3), S5H(3), WXP(3), VV(3),
     :                 PVH(3,2)
      INTEGER J

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  FK5 barycentric position/velocity pv-vector (normalized).
      CALL iau_STARPV ( R5, D5, DR5, DD5, PX5, RV5, PV5, J )

*  FK5 to Hipparcos orientation matrix and spin vector.
      CALL iau_FK5HIP ( R5H, S5H )

*  Orient the FK5 position into the Hipparcos frame.
      CALL iau_RXP ( R5H, PV5(1,1), PVH(1,1) )

*  Apply spin to the position giving an extra space motion component.
      CALL iau_PXP ( PV5(1,1), S5H, WXP )

*  Add this component to the FK5 space motion.
      CALL iau_PPP ( WXP, PV5(1,2), VV )

*  Orient the FK5 space motion into the Hipparcos frame.
      CALL iau_RXP ( R5H, VV, PVH(1,2) )

*  Hipparcos pv-vector to spherical.
      CALL iau_PVSTAR ( PVH, RH, DH, DRH, DDH, PXH, RVH, J )

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
