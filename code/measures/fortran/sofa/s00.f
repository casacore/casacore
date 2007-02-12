      DOUBLE PRECISION FUNCTION iau_S00 ( DATE1, DATE2, X, Y )
*+
*  - - - - - - - -
*   i a u _ S 0 0
*  - - - - - - - -
*
*  The quantity s, positioning the Celestial Ephemeris Origin on the
*  equator of the Celestial Intermediate Pole, given the CIP's X,Y
*  coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2    d      TT as a 2-part Julian Date (Note 1)
*     X,Y            d      CIP coordinates (Note 3)
*
*  Returned:
*     iau_S00        d      the quantity s in radians (Note 2)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others:
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
*  2) The quantity s is the difference between the right ascensions
*     of the same point in two systems.  The two systems are the GCRS
*     and the CIP,CEO, and the point is the ascending node of the
*     CIP equator.  The quantity s remains a small fraction of
*     1 arcsecond throughout 1900-2100.
*
*  3) The series used to compute s is in fact for s+XY/2, where X and Y
*     are the x and y components of the CIP unit vector;  this series is
*     more compact than a direct series for s would be.  This routine
*     requires X,Y to be supplied by the caller, who is responsible for
*     providing values that are consistent with the supplied date.
*
*  Called:
*     iau_ANPM      normalize angle into range +/- pi
*
*  References:
*
*     Capitaine, N., Chapront, J., Lambert, S. and Wallace, P.,
*     "Expressions for the Celestial Intermediate Pole and Celestial
*     Ephemeris Origin consistent with the IAU 2000A precession-nutation
*     model", Astronomy & Astrophysics, 400, 1145-1154 (2003)
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*  This revision:  2005 August 24
*
*  Copyright (C) 2005 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, X, Y

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

*  Time since J2000, in Julian centuries
      DOUBLE PRECISION T

*  Miscellaneous
      INTEGER I, J
      DOUBLE PRECISION A, S0, S1, S2, S3, S4, S5
      DOUBLE PRECISION iau_ANPM

*  Fundamental arguments
      DOUBLE PRECISION FA(14)

*  ---------------------
*  The series for s+XY/2
*  ---------------------

*  Number of terms in the series
      INTEGER NSP, NS0, NS1, NS2, NS3, NS4
      PARAMETER ( NSP=6, NS0=  33, NS1=  3, NS2=25, NS3=4, NS4=1 )

*  Polynomial coefficients
      DOUBLE PRECISION SP ( NSP )

*  Coefficients of l,l',F,D,Om,LMe,LVe,LE,LMa,LJu,LSa,LU,LN,pA
      INTEGER KS0 ( 14, NS0 ),
     :        KS1 ( 14, NS1 ),
     :        KS2 ( 14, NS2 ),
     :        KS3 ( 14, NS3 ),
     :        KS4 ( 14, NS4 )

*  Sine and cosine coefficients
      DOUBLE PRECISION SS0 ( 2, NS0 ),
     :                 SS1 ( 2, NS1 ),
     :                 SS2 ( 2, NS2 ),
     :                 SS3 ( 2, NS3 ),
     :                 SS4 ( 2, NS4 )

*  Polynomial coefficients
      DATA SP /    94    D-6,
     :           3808.35 D-6,
     :           -119.94 D-6,
     :         -72574.09 D-6,
     :             27.70 D-6,
     :             15.61 D-6 /

*  Argument coefficients for t^0
      DATA ( ( KS0(I,J), I=1,14), J =    1,   10 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KS0(I,J), I=1,14), J =   11,   20 ) /
     :  1,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  4, -4,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  1, -1,  1,  0, -8, 12,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KS0(I,J), I=1,14), J =   21,   30 ) /
     :  0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1, -2,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1, -2,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  0,  0,  8,-13,  0,  0,  0,  0,  0, -1,
     :  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  2,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0, -2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  4, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KS0(I,J), I=1,14), J =   31,  NS0 ) /
     :  0,  0,  2, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0, -2,  0, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

*  Argument coefficients for t^1
      DATA ( ( KS1(I,J), I=1,14), J =    1,  NS1 ) /
     :  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

*  Argument coefficients for t^2
      DATA ( ( KS2(I,J), I=1,14), J =    1,   10 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1, -2,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KS2(I,J), I=1,14), J =   11,   20 ) /
     :  1,  0,  0, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0, -2,  0, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0, -2, -2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  2,  0,  0, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  2,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KS2(I,J), I=1,14), J =   21,  NS2 ) /
     :  0,  0,  2,  2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  2,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

*  Argument coefficients for t^3
      DATA ( ( KS3(I,J), I=1,14), J =    1,  NS3 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

*  Argument coefficients for t^4
      DATA ( ( KS4(I,J), I=1,14), J =    1,  NS4 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /

*  Sine and cosine coefficients for t^0
      DATA ( ( SS0(I,J), I=1,2), J =    1,   10 ) /
     :            -2640.73D-6,          +0.39D-6,
     :              -63.53D-6,          +0.02D-6,
     :              -11.75D-6,          -0.01D-6,
     :              -11.21D-6,          -0.01D-6,
     :               +4.57D-6,          +0.00D-6,
     :               -2.02D-6,          +0.00D-6,
     :               -1.98D-6,          +0.00D-6,
     :               +1.72D-6,          +0.00D-6,
     :               +1.41D-6,          +0.01D-6,
     :               +1.26D-6,          +0.01D-6 /
      DATA ( ( SS0(I,J), I=1,2), J =   11,   20 ) /
     :               +0.63D-6,          +0.00D-6,
     :               +0.63D-6,          +0.00D-6,
     :               -0.46D-6,          +0.00D-6,
     :               -0.45D-6,          +0.00D-6,
     :               -0.36D-6,          +0.00D-6,
     :               +0.24D-6,          +0.12D-6,
     :               -0.32D-6,          +0.00D-6,
     :               -0.28D-6,          +0.00D-6,
     :               -0.27D-6,          +0.00D-6,
     :               -0.26D-6,          +0.00D-6 /
      DATA ( ( SS0(I,J), I=1,2), J =   21,   30 ) /
     :               +0.21D-6,          +0.00D-6,
     :               -0.19D-6,          +0.00D-6,
     :               -0.18D-6,          +0.00D-6,
     :               +0.10D-6,          -0.05D-6,
     :               -0.15D-6,          +0.00D-6,
     :               +0.14D-6,          +0.00D-6,
     :               +0.14D-6,          +0.00D-6,
     :               -0.14D-6,          +0.00D-6,
     :               -0.14D-6,          +0.00D-6,
     :               -0.13D-6,          +0.00D-6 /
      DATA ( ( SS0(I,J), I=1,2), J =   31,  NS0 ) /
     :               +0.11D-6,          +0.00D-6,
     :               -0.11D-6,          +0.00D-6,
     :               -0.11D-6,          +0.00D-6 /

*  Sine and cosine coefficients for t^1
      DATA ( ( SS1(I,J), I=1,2), J =    1,  NS1 ) /
     :               -0.07D-6,          +3.57D-6,
     :               +1.71D-6,          -0.03D-6,
     :               +0.00D-6,          +0.48D-6 /

*  Sine and cosine coefficients for t^2
      DATA ( ( SS2(I,J), I=1,2), J =    1,   10 ) /
     :             +743.53D-6,          -0.17D-6,
     :              +56.91D-6,          +0.06D-6,
     :               +9.84D-6,          -0.01D-6,
     :               -8.85D-6,          +0.01D-6,
     :               -6.38D-6,          -0.05D-6,
     :               -3.07D-6,          +0.00D-6,
     :               +2.23D-6,          +0.00D-6,
     :               +1.67D-6,          +0.00D-6,
     :               +1.30D-6,          +0.00D-6,
     :               +0.93D-6,          +0.00D-6 /
      DATA ( ( SS2(I,J), I=1,2), J =   11,   20 ) /
     :               +0.68D-6,          +0.00D-6,
     :               -0.55D-6,          +0.00D-6,
     :               +0.53D-6,          +0.00D-6,
     :               -0.27D-6,          +0.00D-6,
     :               -0.27D-6,          +0.00D-6,
     :               -0.26D-6,          +0.00D-6,
     :               -0.25D-6,          +0.00D-6,
     :               +0.22D-6,          +0.00D-6,
     :               -0.21D-6,          +0.00D-6,
     :               +0.20D-6,          +0.00D-6 /
      DATA ( ( SS2(I,J), I=1,2), J =   21,  NS2 ) /
     :               +0.17D-6,          +0.00D-6,
     :               +0.13D-6,          +0.00D-6,
     :               -0.13D-6,          +0.00D-6,
     :               -0.12D-6,          +0.00D-6,
     :               -0.11D-6,          +0.00D-6 /

*  Sine and cosine coefficients for t^3
      DATA ( ( SS3(I,J), I=1,2), J =    1,  NS3 ) /
     :               +0.30D-6,         -23.51D-6,
     :               -0.03D-6,          -1.39D-6,
     :               -0.01D-6,          -0.24D-6,
     :               +0.00D-6,          +0.22D-6 /

*  Sine and cosine coefficients for t^4
      DATA ( ( SS4(I,J), I=1,2), J =    1,  NS4 ) /
     :               -0.26D-6,          -0.01D-6 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

*  Fundamental Arguments (from IERS Conventions 2003)

*  Mean Anomaly of the Moon.
      FA(1) = iau_ANPM ( ( 485868.249036D0 +
     :                   ( 715923.2178D0 +
     :                   (     31.8792D0 +
     :                   (      0.051635D0 +
     :                   (     -0.00024470D0 )
     :                   * T ) * T ) * T ) * T ) * DAS2R
     :                   + MOD ( 1325D0*T, 1D0 ) * D2PI )

*  Mean Anomaly of the Sun.
      FA(2) = iau_ANPM ( ( 1287104.793048D0 +
     :                   ( 1292581.0481D0 +
     :                   (      -0.5532D0 +
     :                   (      +0.000136D0 +
     :                   (      -0.00001149D0 )
     :                   * T ) * T ) * T ) * T ) * DAS2R
     :                   + MOD ( 99D0*T, 1D0 ) * D2PI )

*  Mean Longitude of the Moon minus Mean Longitude of the Ascending
*  Node of the Moon.
      FA(3) = iau_ANPM ( (  335779.526232D0 +
     :                   (  295262.8478D0 +
     :                   (     -12.7512D0 +
     :                   (      -0.001037D0 +
     :                   (       0.00000417D0 )
     :                   * T ) * T ) * T ) * T ) * DAS2R
     :                   + MOD ( 1342D0*T, 1D0 ) * D2PI )

*  Mean Elongation of the Moon from the Sun.
      FA(4) = iau_ANPM ( ( 1072260.703692D0 +
     :                   ( 1105601.2090D0 +
     :                   (      -6.3706D0 +
     :                   (       0.006593D0 +
     :                   (      -0.00003169D0 )
     :                   * T ) * T ) * T ) * T ) * DAS2R
     :                   + MOD ( 1236D0*T, 1D0 ) * D2PI )

*  Mean Longitude of the Ascending Node of the Moon.
      FA(5) = iau_ANPM ( (  450160.398036D0 +
     :                   ( -482890.5431D0 +
     :                   (       7.4722D0 +
     :                   (       0.007702D0 +
     :                   (      -0.00005939D0 )
     :                   * T ) * T ) * T ) * T ) * DAS2R
     :                   + MOD ( -5D0*T, 1D0 ) * D2PI )

      FA( 6) = iau_ANPM ( 4.402608842D0 + 2608.7903141574D0 * T )
      FA( 7) = iau_ANPM ( 3.176146697D0 + 1021.3285546211D0 * T )
      FA( 8) = iau_ANPM ( 1.753470314D0 +  628.3075849991D0 * T )
      FA( 9) = iau_ANPM ( 6.203480913D0 +  334.0612426700D0 * T )
      FA(10) = iau_ANPM ( 0.599546497D0 +   52.9690962641D0 * T )
      FA(11) = iau_ANPM ( 0.874016757D0 +   21.3299104960D0 * T )
      FA(12) = iau_ANPM ( 5.481293872D0 +    7.4781598567D0 * T )
      FA(13) = iau_ANPM ( 5.311886287D0 +    3.8133035638D0 * T )
      FA(14) =          ( 0.024381750D0 +    0.00000538691D0 * T ) * T

*  Evaluate S.
      S0 = SP(1)
      S1 = SP(2)
      S2 = SP(3)
      S3 = SP(4)
      S4 = SP(5)
      S5 = SP(6)

      DO 2 I = NS0,1,-1
         A = 0D0
         DO 1 J=1,14
            A = A + DBLE(KS0(J,I))*FA(J)
 1       CONTINUE
         S0 = S0 + ( SS0(1,I)*SIN(A) + SS0(2,I)*COS(A) )
 2    CONTINUE

      DO 4 I = NS1,1,-1
         A = 0D0
         DO 3 J=1,14
            A = A + DBLE(KS1(J,I))*FA(J)
 3       CONTINUE
         S1 = S1 + ( SS1(1,I)*SIN(A) + SS1(2,I)*COS(A) )
 4    CONTINUE

      DO 6 I = NS2,1,-1
         A = 0D0
         DO 5 J=1,14
            A = A + DBLE(KS2(J,I))*FA(J)
 5       CONTINUE
         S2 = S2 + ( SS2(1,I)*SIN(A) + SS2(2,I)*COS(A) )
 6    CONTINUE

      DO 8 I = NS3,1,-1
         A = 0D0
         DO 7 J=1,14
            A = A + DBLE(KS3(J,I))*FA(J)
 7       CONTINUE
         S3 = S3 + ( SS3(1,I)*SIN(A) + SS3(2,I)*COS(A) )
 8    CONTINUE

      DO 10 I = NS4,1,-1
         A = 0D0
         DO 9 J=1,14
            A = A + DBLE(KS4(J,I))*FA(J)
 9       CONTINUE
         S4 = S4 + ( SS4(1,I)*SIN(A) + SS4(2,I)*COS(A) )
 10   CONTINUE

      iau_S00 = ( S0 +
     :          ( S1 +
     :          ( S2 +
     :          ( S3 +
     :          ( S4 +
     :            S5 * T ) * T ) * T ) * T ) * T ) * DAS2R - X*Y/2D0

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
