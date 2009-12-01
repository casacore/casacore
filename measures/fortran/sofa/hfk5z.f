      SUBROUTINE iau_HFK5Z ( RH, DH, EPOCH1, EPOCH2, R5, D5, DR5, DD5 )
*+
*  - - - - - - - - - -
*   i a u _ H F K 5 Z
*  - - - - - - - - - -
*
*  Transform a Hipparcos star position into FK5 J2000, assuming
*  zero Hipparcos proper motion.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     RH              d      Hipparcos RA (radians)
*     DH              d      Hipparcos Dec (radians)
*     EPOCH1,EPOCH2   d      TDB epoch (Note 1)
*
*  Returned (all FK5, equinox J2000, epoch EPOCH1+EPOCH2):
*     R5              d      RA (radians)
*     D5              d      Dec (radians)
*     DR5             d      FK5 RA proper motion (rad/year, Note 4)
*     DD5             d      Dec proper motion (rad/year, Note 4)
*
*  Notes:
*
*  1) The epoch EPOCH1+EPOCH2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TDB)=2450123.7 could be expressed in any of these ways,
*     among others:
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
*  2) The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*
*  3) The FK5 to Hipparcos transformation is modeled as a pure
*     rotation and spin;  zonal errors in the FK5 catalogue are
*     not taken into account.
*
*  4) It was the intention that Hipparcos should be a close
*     approximation to an inertial frame, so that distant objects
*     have zero proper motion;  such objects have (in general)
*     non-zero proper motion in FK5, and this routine returns those
*     fictitious proper motions.
*
*  5) The position returned by this routine is in the FK5 J2000
*     reference frame but at Julian epoch EPOCH1+EPOCH2.
*
*  6) See also iau_FK52H, iau_H2FK5, iau_FK5ZHZ.
*
*  Called:
*     iau_S2C     spherical to unit vector
*     iau_FK5HIP  FK5 rotation and spin wrt to Hipparcos
*     iau_RXP     product of r-matrix and p-vector
*     iau_SXP     product of scalar and p-vector
*     iau_RXR     r-matrix multiply
*     iau_TRXP    product of transpose of r-matrix and p-vector
*     iau_PXP     vector product of two p-vectors
*     iau_PV2S    pv-vector to spherical
*     iau_ANP     normalize radians to range 0 to 2pi
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

      DOUBLE PRECISION RH, DH, EPOCH1, EPOCH2, R5, D5, DR5, DD5

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian year
      DOUBLE PRECISION DJY
      PARAMETER ( DJY = 365.25D0 )

      DOUBLE PRECISION T, PH(3), R5H(3,3), S5H(3), SH(3), VST(3),
     :                 RST(3,3), R5HT(3,3), PV5E(3,2), VV(3),
     :                 W, R, V

      DOUBLE PRECISION iau_ANP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Time interval from fundamental epoch J2000.0 to given epoch (JY).
      T = ( ( EPOCH1-DJ0 ) + EPOCH2 ) / DJY

*  Hipparcos barycentric position vector (normalized).
      CALL iau_S2C ( RH, DH, PH )

*  FK5 to Hipparcos orientation matrix and spin vector.
      CALL iau_FK5HIP ( R5H, S5H )

*  Rotate the spin into the Hipparcos frame.
      CALL iau_RXP ( R5H, S5H, SH )

*  Accumulated Hipparcos wrt FK5 spin over that interval.
      CALL iau_SXP ( T, S5H, VST )

*  Express the accumulated spin as a rotation matrix.
      CALL iau_RV2M ( VST, RST )

*  Rotation matrix:  accumulated spin, then FK5 to Hipparcos.
      CALL iau_RXR ( R5H, RST, R5HT )

*  De-orient & de-spin the Hipparcos position into FK5 J2000.
      CALL iau_TRXP ( R5HT, PH, PV5E )

*  Apply spin to the position giving a space motion.
      CALL iau_PXP ( SH, PH, VV )

*  De-orient & de-spin the Hipparcos space motion into FK5 J2000.
      CALL iau_TRXP ( R5HT, VV, PV5E(1,2) )

*  FK5 position/velocity pv-vector to spherical.
      CALL iau_PV2S ( PV5E, W, D5, R, DR5, DD5, V )
      R5 = iau_ANP ( W )

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
