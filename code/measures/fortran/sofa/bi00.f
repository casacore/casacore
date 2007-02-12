      SUBROUTINE iau_BI00 ( DPSIBI, DEPSBI, DRA )
*+
*  - - - - - - - - -
*   i a u _ B I 0 0
*  - - - - - - - - -
*
*  Frame bias components of IAU 2000 precession-nutation models (part of
*  MHB2000 with additions).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Returned:
*     DPSIBI,DEPSBI  d   longitude and obliquity corrections
*     DRA            d   the ICRS RA of the J2000 mean equinox
*
*  Notes
*
*  1) The frame bias corrections in longitude and obliquity (radians)
*     are required in order to correct for the offset between the GCRS
*     pole and the mean J2000 pole.  They define, with respect to the
*     GCRS frame, a J2000 mean pole that is consistent with the rest of
*     the IAU 2000A precession-nutation model.
*
*  2) In addition to the displacement of the pole, the complete
*     description of the frame bias requires also an offset in right
*     ascension.  This is not part of the IAU 2000A model, and is from
*     Chapront et al. (2002).  It is returned in radians.
*
*  3) This is a supplemented implementation of one aspect of the IAU
*     2000A nutation model, formally adopted by the IAU General Assembly
*     in 2000, namely MHB2000 (Mathews et al. 2002).
*
*  References:
*
*     Chapront, J., Chapront-Touze, M. & Francou, G., Astron.Astrophys.,
*     387, 700, 2002.
*
*     Mathews, P.M., Herring, T.A., Buffet, B.A., "Modeling of nutation
*     and precession   New nutation series for nonrigid Earth and
*     insights into the Earth's interior", J.Geophys.Res., 107, B4,
*     2002.  The MHB2000 code itself was obtained on 9th September 2002
*     from ftp://maia.usno.navy.mil/conv2000/chapter5/IAU2000A.
*
*  This revision:  2005 August 24
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DPSIBI, DEPSBI, DRA

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  The frame bias corrections in longitude and obliquity
      DOUBLE PRECISION DPBIAS, DEBIAS
      PARAMETER ( DPBIAS = -0.041775D0 * DAS2R,
     :            DEBIAS = -0.0068192D0 * DAS2R )

*  The ICRS RA of the J2000 equinox (Chapront et al., 2002)
      DOUBLE PRECISION DRA0
      PARAMETER ( DRA0 = -0.0146D0 * DAS2R )

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Return the results (which are fixed).
      DPSIBI = DPBIAS
      DEPSBI = DEBIAS
      DRA = DRA0

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
