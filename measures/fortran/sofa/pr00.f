      SUBROUTINE iau_PR00 ( DATE1, DATE2, DPSIPR, DEPSPR )
*+
*  - - - - - - - - -
*   i a u _ P R 0 0
*  - - - - - - - - -
*
*  Precession-rate part of the IAU 2000 precession-nutation models
*  (part of MHB2000).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2    d   TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     DPSIPR,DEPSPR  d   precession corrections (Notes 2,3)
*
*  Notes
*
*  1) The T date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others
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
*  2) The precession adjustments are expressed as "nutation components",
*     corrections in longitude and obliquity with respect to the J2000
*     equinox and ecliptic.
*
*  3) Although the precession adjustments are stated to be with respect
*     to Lieske et al. (1977), the MHB2000 model does not specify which
*     set of Euler angles are to be used and how the adjustments are to
*     be applied.  The most literal and straightforward procedure is to
*     adopt the 4-rotation epsilon_0, psi_A, omega_A, xi_A option, and
*     to add DPSIPR to psi_A and DEPSPR to both omega_A and eps_A
*     (Wallace 2002).
*
*  4) This is an implementation of one aspect of the IAU 2000A nutation
*     model, formally adopted by the IAU General Assembly in 2000,
*     namely MHB2000 (Mathews et al. 2002).
*
*  References
*
*     Lieske, J.H., Lederle, T., Fricke, W. & Morando, B., "Expressions
*     for the precession quantities based upon the IAU (1976) System of
*     Astronomical Constants", Astron.Astrophys., 58, 1-16 (1977)
*
*     Mathews, P.M., Herring, T.A., Buffet, B.A., "Modeling of nutation
*     and precession   New nutation series for nonrigid Earth and
*     insights into the Earth's interior", J.Geophys.Res., 107, B4,
*     2002.  The MHB2000 code itself was obtained on 9th September 2002
*     from ftp://maia.usno.navy.mil/conv2000/chapter5/IAU2000A.
*
*     Wallace, P.T., "Software for Implementing the IAU 2000
*     Resolutions", in IERS Workshop 5.1 (2002)
*
*  This revision:  2005 August 24
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, DPSIPR, DEPSPR

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

*  ------------------------------------
*  Precession and obliquity corrections (radians per century)
*  ------------------------------------

      DOUBLE PRECISION PRECOR, OBLCOR
      PARAMETER ( PRECOR = -0.29965D0 * DAS2R,
     :            OBLCOR = -0.02524D0 * DAS2R )

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

*  Precession rate contributions with respect to IAU 1976/80.
      DPSIPR = PRECOR * T
      DEPSPR = OBLCOR * T

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
