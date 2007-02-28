      SUBROUTINE iau_STARPV ( RA, DEC, PMR, PMD, PX, RV, PV, J )
*+
*  - - - - - - - - - - -
*   i a u _ S T A R P V
*  - - - - - - - - - - -
*
*  Convert star catalog coordinates to position+velocity vector.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given (Note 1):
*     RA       d         right ascension (radians)
*     DEC      d         declination (radians)
*     PMR      d         RA proper motion (radians/year)
*     PMD      d         Dec proper motion (radians/year)
*     PX       d         parallax (arcseconds)
*     RV       d         radial velocity (km/s, positive = receding)
*
*  Returned (Note 2):
*     PV       d(3,2)    pv-vector (AU, AU/day)
*     J        i         status:
*                           0 = no warnings
*                           1 = distance overridden (Note 6)
*                           2 = excessive velocity (Note 7)
*                           4 = solution didn't converge (Note 8)
*                        else = binary logical OR of the above
*
*  Notes:
*
*  1) The star data accepted by this routine are "observables" for an
*     imaginary observer at the solar-system barycenter.  Proper motion
*     and radial velocity are, strictly, in terms of barycentric
*     coordinate time, TCB.  For most practical applications, it is
*     permissible to neglect the distinction between TCB and ordinary
*     "proper" time on Earth (TT/TAI).  The result will, as a rule, be
*     limited by the intrinsic accuracy of the proper-motion and radial-
*     velocity data;  moreover, the pv-vector is likely to be merely an
*     intermediate result, so that a change of time unit would cancel
*     out overall.
*
*     In accordance with normal star-catalog conventions, the object's
*     right ascension and declination are freed from the effects of
*     secular aberration.  The frame, which is aligned to the catalog
*     equator and equinox, is Lorentzian and centered on the SSB.
*
*  2) The resulting position and velocity pv-vector is with respect to
*     the same frame and, like the catalog coordinates, is freed from
*     the effects of secular aberration.  Should the "coordinate
*     direction", where the object was located at the catalog epoch, be
*     required, it may be obtained by calculating the magnitude of the
*     position vector PV(1-3,1) dividing by the speed of light in AU/day
*     to give the light-time, and then multiplying the space velocity
*     PV(1-3,2) by this light-time and adding the result to PV(1-3,1).
*
*     Summarizing, the pv-vector returned is for most stars almost
*     identical to the result of applying the standard geometrical
*     "space motion" transformation.  The differences, which are the
*     subject of the Stumpff paper referenced below, are:
*
*     (i) In stars with significant radial velocity and proper motion,
*     the constantly changing light-time distorts the apparent proper
*     motion.  Note that this is a classical, not a relativistic,
*     effect.
*
*     (ii) The transformation complies with special relativity.
*
*  3) Care is needed with units.  The star coordinates are in radians
*     and the proper motions in radians per Julian year, but the
*     parallax is in arcseconds; the radial velocity is in km/s, but
*     the pv-vector result is in AU and AU/day.
*
*  4) The RA proper motion is in terms of coordinate angle, not true
*     angle.  If the catalog uses arcseconds for both RA and Dec proper
*     motions, the RA proper motion will need to be divided by cos(Dec)
*     before use.
*
*  5) Straight-line motion at constant speed, in the inertial frame,
*     is assumed.
*
*  6) An extremely small (or zero or negative) parallax is interpreted
*     to mean that the object is on the "celestial sphere", the radius
*     of which is an arbitrary (large) value (see the constant PXMIN).
*     When the distance is overridden in this way, the status, initially
*     zero, has 1 added to it.
*
*  7) If the space velocity is a significant fraction of c (see the
*     constant VMAX), it is arbitrarily set to zero.  When this action
*     occurs, 2 is added to the status.
*
*  8) The relativistic adjustment involves an iterative calculation.
*     If the process fails to converge within a set number (IMAX) of
*     iterations, 4 is added to the status.
*
*  9) The inverse transformation is performed by the routine iau_PVSTAR.
*
*  Called:
*     iau_S2PV     spherical coordinates to pv-vector
*     iau_PM       modulus of p-vector
*     iau_ZP       zero a p-vector
*     iau_PN       normalize p-vector returning modulus
*     iau_PDP      dot product of two p-vectors
*     iau_SXP      multiply p-vector by scalar
*     iau_PMP      p-vector minus p-vector
*     iau_PPP      p-vector plus p-vector
*
*  Reference:
*
*     Stumpff, P., Astron.Astrophys. 144, 232-240 (1985).
*
*  This revision:  2005 August 26
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION RA, DEC, PMR, PMD, PX, RV, PV(3,2)
      INTEGER J

*  Smallest allowed parallax
      DOUBLE PRECISION PXMIN
      PARAMETER ( PXMIN = 1D-7 )

*  Largest allowed speed (fraction of c)
      DOUBLE PRECISION VMAX
      PARAMETER ( VMAX = 0.5D0 )

*  Julian years to days
      DOUBLE PRECISION Y2D
      PARAMETER ( Y2D = 365.25D0 )

*  Radians to arc seconds
      DOUBLE PRECISION DR2AS
      PARAMETER ( DR2AS = 206264.8062470963551564734D0 )

*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

*  AU (meters)
      DOUBLE PRECISION AUM
      PARAMETER ( AUM = 149597870D3 )

*  Speed of light (AU per day)
      DOUBLE PRECISION C
      PARAMETER ( C = D2S/499.004782D0 )

*  Maximum number of iterations for relativistic solution
      INTEGER I,IMAX
      PARAMETER ( IMAX = 100 )

      INTEGER IWARN
      DOUBLE PRECISION W, R, RD, RAD, DECD, V, X(3), USR(3), UST(3),
     :                 VSR, VST, BETST, BETSR, BETT, BETR, OD, ODEL,
     :                 DD, DDEL, ODD, ODDEL, D, DEL, UR(3), UT(3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Distance (AU).
      IF ( PX.GE.PXMIN ) THEN
         W = PX
         IWARN = 0
      ELSE
         W = PXMIN
         IWARN = 1
      END IF
      R = DR2AS / W

*  Radial velocity (AU/day).
      RD = D2S * RV * 1D3 / AUM

*  Proper motion (radian/day).
      RAD = PMR / Y2D
      DECD = PMD / Y2D

*  To pv-vector (AU,AU/day).
      CALL iau_S2PV ( RA, DEC, R, RAD, DECD, RD, PV )

*  If excessive velocity, arbitrarily set it to zero.
      CALL iau_PM ( PV(1,2), V )
      IF ( V/C .GT. VMAX ) THEN
         CALL iau_ZP ( PV(1,2) )
         IWARN = IWARN + 2
      END IF

*  Isolate the radial component of the velocity (AU/day).
      CALL iau_PN ( PV(1,1), W, X )
      CALL iau_PDP ( X, PV(1,2), VSR )
      CALL iau_SXP ( VSR, X, USR )

*  Isolate the transverse component of the velocity (AU/day).
      CALL iau_PMP ( PV(1,2), USR, UST )
      CALL iau_PM ( UST, VST )

*  Special-relativity dimensionless parameters.
      BETSR = VSR / C
      BETST = VST / C

*  Determine the inertial-to-observed relativistic correction terms.
      BETT = BETST
      BETR = BETSR
      DO 1 I=1,IMAX
         D = 1D0 + BETR
         DEL = SQRT(1D0 - BETR*BETR - BETT*BETT) - 1D0
         BETR = D*BETSR + DEL
         BETT = D*BETST
         IF ( I .GT. 1 ) THEN
            DD = ABS(D-OD)
            DDEL = ABS(DEL-ODEL)
            IF ( I.GT.2 .AND.
     :           DD.EQ.ODD .AND.
     :           DDEL.EQ.ODDEL ) GO TO 2
            IF ( I .GE. IMAX ) IWARN = IWARN + 4
            ODD = DD
            ODDEL = DDEL
         END IF
         OD = D
         ODEL = DEL
 1    CONTINUE
 2    CONTINUE

*  Replace observed radial velocity with inertial value.
      IF ( BETSR .NE. 0D0 ) THEN
         W = D + DEL/BETSR
      ELSE
         W = 1D0
      END IF
      CALL iau_SXP ( W, USR, UR )

*  Replace observed tangential velocity with inertial value.
      CALL iau_SXP ( D, UST, UT )

*  Combine the two to obtain the inertial space velocity.
      CALL iau_PPP ( UR, UT, PV(1,2) )

*  Return the status.
      J = IWARN

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
