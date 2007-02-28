      SUBROUTINE iau_JDCALF ( NDP, DJ1, DJ2, IYMDF, J )
*+
*  - - - - - - - - - - -
*   i a u _ J D C A L F
*  - - - - - - - - - - -
*
*  Julian Date to Gregorian Calendar, expressed in a form convenient
*  for formatting messages:  rounded to a specified precision, and with
*  the fields stored in a single array.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     NDP         i     number of decimal places of days in fraction
*     DJ1,DJ2     d     DJ1+DJ2 = Julian Date (Note 1)
*
*  Returned:
*     IYMDF       i(4)  year, month, day, fraction in Gregorian
*                       calendar
*     J           i     status:
*                          -1 = date out of range
*                           0 = OK
*                          +1 = NDP not 0-9 (interpreted as 0)
*
*  Notes:
*
*  1) The Julian Date is apportioned in any convenient way between
*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
*     be expressed in any of these ways, among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*  2) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  3) Refer to the routine iau_JD2CAL.
*
*  4) NDP should be 4 or less if internal overflows are to be
*     avoided on machines which use 16-bit integers.
*
*  Called:
*     iau_JD2CAL  JD to Gregorian calendar
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*  This revision:  2003 January 14
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION DJ1, DJ2
      INTEGER IYMDF(4), J

      INTEGER JS
      DOUBLE PRECISION DENOM, D1, D2, F1, F2, F

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Denominator of fraction (e.g. 100 for 2 decimal places).
      IF ( NDP.GE.0 .AND. NDP.LE.9 ) THEN
         DENOM = DBLE(10**NDP)
      ELSE
         J = 1
         DENOM = 1D0
      END IF

*  Copy the date, big then small, and realign to midnight.
      IF ( DJ1 .GE. DJ2 ) THEN
         D1 = DJ1
         D2 = DJ2
      ELSE
         D1 = DJ2
         D2 = DJ1
      END IF
      D2 = D2 - 0.5D0

*  Separate days and fractions.
      F1 = MOD(D1,1D0)
      F2 = MOD(D2,1D0)
      D1 = ANINT(D1-F1)
      D2 = ANINT(D2-F2)

*  Round the total fraction to the specified number of places.
      F = ANINT(( F1+F2 ) * DENOM) / DENOM

*  Re-assemble the rounded date and re-align to noon.
      D2 = D2 + F + 0.5D0

*  Convert to Gregorian Calendar.
      CALL iau_JD2CAL ( D1, D2, IYMDF(1), IYMDF(2), IYMDF(3), F, JS )
      IF ( JS .EQ. 0 ) THEN
         IYMDF(4) = NINT(F*DENOM)
      ELSE
         J = JS
      END IF

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
