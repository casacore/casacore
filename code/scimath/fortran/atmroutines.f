*     Copyright (C) 1999,2001
*     Associated Universities, Inc. Washington DC, USA.
*
*     This library is free software; you can redistribute it and/or
*     modify it under the terms of the GNU Library General Public
*     License as published by the Free Software Foundation; either
*     version 2 of the License, or (at your option) any later version.
*
*     This library is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Library General Public License for more details.
*
*     You should have received a copy of the GNU Library General Public
*     License along with this library; if not, write to the Free
*     Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
*     MA 02139, USA.
*
*     Correspondence concerning AIPS++ should be addressed as follows:
*            Internet email: aips2-request@@nrao.edu.
*            Postal address: AIPS++ Project Office
*                            National Radio Astronomy Observatory
*                            520 Edgemont Road
*                            Charlottesville, VA 22903-2475 USA
*
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      SUBROUTINE ASE45(P,T,D,HA)
C J CERNICHARO
C
C	ATMOSFERA U.S. 1962 MES DE ENERO 45 GRADOS DE LATITUD NORTE
C	HA ES LA ALTURA A LA QUE SE QUIEREN CALCULAR LA PRESION,
C	(MILIBARES),LA TEMPERATURA (K) Y LA DENSIDAD (GR/M**3)
C	HA DEBE ESTAR EN KM
C
C	P === PRESION
C	T === TEMPERATURA
C	D === DENSIDAD
C
      REAL P,T,D,HA
      REAL PR(91),TE(91),DEN(91)
      INTEGER I0,I1,I2,J
      REAL X1,X2,X3,Y1,Y2,Y3,A,B,C
      DATA TE/
     $272.150,268.650,265.154,261.654,255.668,249.674,243.686,237.698,
     $231.710,225.728,219.746,219.159,218.661,218.163,217.665,217.167,
     $216.670,216.172,215.675,215.178,215.150,215.150,215.150,215.150,
     $215.150,215.150,215.150,215.150,215.852,216.644,217.437,218.230,
     $219.022,221.723,224.789,227.855,230.921,233.987,237.049,240.112,
     $243.175,246.235,249.294,252.354,255.414,258.470,261.527,264.580,
     $265.650,265.650,265.650,265.650,265.650,264.526,262.560,260.594,
     $258.628,256.664,254.698,252.736,250.772,248.810,246.848,244.886,
     $242.926,240.932,238.874,236.818,234.762,232.706,230.650,228.596,
     $226.543,224.491,222.439,220.387,218.336,216.286,214.236,212.187,
     $210.150,210.150,210.150,210.150,210.150,210.150,210.150,210.150,
     $210.150,210.150,210.150/
      DATA PR/
     $.101800E04,.897340E03,.789746E03,.693761E03,.608132E03,.531322E03,
     $.462749E03,.401636E03,.347333E03,.299257E03,.256837E03,.219907E03,
     $.188249E03,.161092E03,.137803E03,.117839E03,.100747E03,.861032E02,
     $.735727E02,.628431E02,.536674E02,.458314E02,.391458E02,.334355E02,
     $.285581E02,.243961E02,.208407E02,.178034E02,.152146E02,.130097E02,
     $.111307E02,.952848E01,.816277E01,.700062E01,.601741E01,.518290E01,
     $.447305E01,.386792E01,.335147E01,.290933E01,.253005E01,.220437E01,
     $.192389E01,.168188E01,.147270E01,.129175E01,.113478E01,.998513,
     $.879632,.774975,.682770,.601612,.530101,.467019,.411144,.361607,
     $.317729,.278938,.244604,.214337,.187596,.164042,.143293,.125033,
     $.108996,.949100E-01,.825474E-01,.717187E-01,.622342E-01,
     $.539367E-01,.466861E-01,.403641E-01,.348523E-01,.3006E-01,
     $.258872E-01,.223E-01,.191221E-01,.164020E-01,.140484E-01,
     $.120146E-01,.102596E-01,.875578E-02,.747237E-02,.637812E-02,
     $.544411E-02,.464687E-02,.396639E-02,.338610E-02,.289071E-02,
     $.246780E-02,.210710E-02/
      DATA DEN/
     $.130098E04,.116211E04,.103651E04,.922974E03,.828225E03,
     $.741093E03,.661404E03,.588557E03,.522172E03,.461856E03,
     $.407161E03,.349556E03,.299915E03,.257234E03,.220550E03,
     $.189031E03,.161984E03,.138758E03,.118838E03,.101741E03,
     $.868975E02,.742096E02,.633844E02,.541483E02,.462409E02,
     $.395018E02,.337449E02,.288270E02,.245552E02,.209198E02,
     $.178331E02,.152106E02,.129834E02,.109993E02,.932552E01,
     $.792416E01,.674806E01,.575870E01,.492532E01,.422101E01,
     $.362450E01,.311870E01,.268847E01,.232179E01,.200867E01,
     $.174103E01,.151159E01,.131472E01,.115353E01,.101629E01,
     $.895370,.788942,.695164,.615041,.545511,.483404,.427977,
     $.378600,.334562,.295439,.260605,.229681,.202224,.177869,
     $.156306,.137232,.120385,.105501,.923507E-01,.807449E-01,
     $.705135E-01,.615126E-01,.535945E-01,.466437E-01,.405427E-01,
     $.351939E-01,.305104E-01,.264185E-01,.228440E-01,.197255E-01,
     $.170075E-01,.145146E-01,.123870E-01,.105731E-01,.902476E-02,
     $.770318E-02,.657513E-02,.561318E-02,.479197E-02,.409090E-02,
     $.349296E-02/
C
C	INTERPOLACION PARABOLICA ENTRE TRES PUNTOS
C
C	CALL POLI2
C
      I1=HA+1
      I0=I1-1
      I2=I1+1
      IF(I0.GT.0)GO TO 10
      I1=I1+1
      I2=I2+1
      I0=I0+1
10    X1=I0-1
      X2=I1-1
      X3=I2-1
      DO 3 J=1,3
         GO TO (5,6,7)J
5        Y1=PR(I0)
         Y2=PR(I1)
         Y3=PR(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         P=A+B*HA+C*HA**2
         GO TO 3
6        Y1=TE(I0)
         Y2=TE(I1)
         Y3=TE(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         T=A+B*HA+C*HA*HA
         GO TO 3
7        Y1=DEN(I0)
         Y2=DEN(I1)
         Y3=DEN(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         D=A+B*HA+C*HA**2
3     CONTINUE
      RETURN
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      SUBROUTINE ASJ45(P,T,D,HA)
c J CERNICHARO
C
C	ATMOSFERA STANDARD U.S. 1962 MES DE JULIO 45 GRADOS LATITUD NORTE
C	HA === ALTURA EN KM DONDE SE DESEA CALCULAR P,T,D
C	P  === PRESION EN MILIBARES
C	D  === DENSIDAD EN GR/M**3
C	T  === TEMPERATURA EN K
C
      REAL P,T,D,HA
      REAL PR(91),TE(91),DEN(91)
      INTEGER I0,I1,I2,J
      REAL X1,X2,X3,Y1,Y2,Y3,A,B,C
*
      DATA TE/294.15,289.65,285.154,279.156,273.168,267.174,261.186,
     $254.702,248.215,241.734,235.254,228.773,222.299,215.825,
     $215.650,215.650,215.650,215.650,216.789,217.982,219.174,
     $220.367,221.559,222.750,223.942,225.132,226.323,227.513,
     $229.492,231.573,233.654,235.735,237.814,240.225,242.697,
     $245.170,247.642,250.115,252.585,255.055,257.525,259.992,
     $262.460,264.927,267.395,269.860,272.325,274.787,275.650,
     $275.650,275.650,275.650,275.650,274.245,271.787,269.330,
     $266.872,264.417,261.960,259.507,257.052,254.600,252.147,
     $248.931,244.521,240.111,235.701,231.295,226.890,222.484,
     $218.079,213.678,209.277,204.880,200.484,196.087,191.691,
     $187.299,182.907,178.515,174.150,174.150,174.150,174.150,
     $174.150,174.150,174.150,174.150,174.150,174.150,174.150/
      DATA PR
     $/.101350E+04,.902198E+03,.801594E+03,.710433E+03,.628063E+03,
     $.553615E+03,.486632E+03,.426404E+03,.372351E+03,.324024E+03,
     $.280904E+03,.242550E+03,.208581E+03,.178571E+03,.152506E+03,
     $.130245E+03,.111251E+03,.950271E+02,.812140E+02,.694686E+02,
     $.594726E+02,.509578E+02,.437053E+02,.375159E+02,.322292E+02,
     $.277141E+02,.238504E+02,.205416E+02,.177123E+02,.152931E+02,
     $.132216E+02,.114455E+02,.992199E+01,.861267E+01,.748802E+01,
     $.651947E+01,.568409E+01,.496251E+01,.433891E+01,.379863E+01,
     $.332989E+01,.292305E+01,.256908E+01,.226070E+01,.199169E+01,
     $.175697E+01,.155167E+01,.137207E+01,.121431E+01,.107476E+01,
     $.951246,.842033,.745358,.659666,.583315,.515225,.454565,
     $.400635,.352641,.310106,.272332,.238893,.209295,.183112,
     $.159876,.139244,.120964,.104821,.905822E-01,.780538E-01,
     $.670582E-01,.574425E-01,.490475E-01,.417458E-01,.354072E-01,
     $.299215E-01,.251894E-01,.211250E-01,.176426E-01,.146698E-01,
     $.121421E-01,.100283E-01,.828248E-02,.684195E-02,.565195E-02,
     $.466893E-02,.358689E-02,.318670E-02,.263297E-02,.217545E-02,
     $.179779E-02/
      DATA DEN/
     $.119194E04,.107953E04,.975726E03,.884585E03,.799782E03,
     $.721113E03,.648694E03,.582972E03,.522462E03,.466877E03,
     $.415928E03,.369346E03,.326869E03,.288235E03,.246363E03,
     $.210402E03,.179719E03,.153510E03,.130507E03,.111022E03,
     $.945290E02,.805568E02,.687200E02,.586725E02,.501363E02,
     $.428845E02,.367119E02,.314533E02,.268872E02,.230062E02,
     $.197129E02,.169141E02,.145345E02,.124899E02,.107483E02,
     $.926367E01,.799602E01,.691194E01,.598427E01,.518837E01,
     $.450452E01,.391664E01,.340998E01,.297272E01,.259482E01,
     $.226811E01,.198495E01,.173947E01,.153465E01,.135829E01,
     $.120219E01,.106417E01,.941988,.837960,.747673,.666424,.593377,
     $.527834,.468961,.416292,.369075,.326876,.289163,.256257,
     $.227775,.202024,.178786,.157877,.139080,.122217,.107121,
     $.936510E-01,.816457E-01,.709824E-01,.615248E-01,.531584E-01,
     $.457778E-01,.392916E-01,.336024E-01,.286279E-01,.242889E-01,
     $.200605E-01,.165682E-01,.136866E-01,.113061E-01,.933969E-02,
     $.717528E-02,.637464E-02,.526696E-02,.435175E-02,.359628E-02/
C
C	INTERPOLACION PARABOLICA ENTRE TRES PUNTOS
C
C	CALL POLI2
C
      I1=HA+1
      I0=I1-1
      I2=I1+1
      IF(I0.GT.0)GO TO 10
      I1=I1+1
      I2=I2+1
      I0=I0+1
10    X1=I0-1
      X2=I1-1
      X3=I2-1
      DO 3 J=1,3
         GO TO (5,6,7)J
5        Y1=PR(I0)
         Y2=PR(I1)
         Y3=PR(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         P=A+B*HA+C*HA**2
         GO TO 3
6        Y1=TE(I0)
         Y2=TE(I1)
         Y3=TE(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         T=A+B*HA+C*HA*HA
         GO TO 3
7        Y1=DEN(I0)
         Y2=DEN(I1)
         Y3=DEN(I2)
         CALL POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
         D=A+B*HA+C*HA**2
3     CONTINUE
      RETURN
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
C+HPUX
c$OPTIMIZE LEVEL1
C-HPUX
      SUBROUTINE ATMATMOSP(T0,P0,H0)
C----------------------------------------------------------------------
C Compute an atmospheric model, interpolated between standard atmospheres
C of winter and summer (subroutines ase45 and asj 45), to fit with temperature
C t0 (k) and pressure p0 (mbar) at altitude h0 (km).
C 15 layers are used.
C The transmission of the model atmosphere can then be computed by calling
C entry point transm.
C
C input 	t0	R	temperature	(K)
C		po	R	pressure	(mbar)
C		h0	R	altitude	(km)
C----------------------------------------------------------------------
      INTEGER MP
      PARAMETER (MP=80)
      REAL  T(MP), H(MP), P(MP), R(MP), RR(MP),
     $PE, TE, DE, H0, T0, P0, PJ, TJ, DJ, APE, ATE, APJ, ATJ, P1,
     $T1, D1, HEIGHT, R1, WATER, AIRMASS, TAUW, TAUOX, TAUT, TEMI,
     $FREQ, TAG, TOX, TAGU, TOXI, TATM
      INTEGER NP, J, IER
      REAL PATH
      REAL DPATH, Z, PR_AG
      REAL*8 N_INDEX, C_SNELL
*
      SAVE T,H,P,R,NP
*
* average summer and winter model atmospheres according to
* given values of temperature and pression
      CALL ASE45(PE,TE,DE,H0)
      CALL ASJ45(PJ,TJ,DJ,H0)
      APE = (P0-PJ)/(PE-PJ)
      APJ = (P0-PE)/(PJ-PE)
      ATE = (T0-TJ)/(TE-TJ)
      ATJ = (T0-TE)/(TJ-TE)
*
* set layers
      DO J=1,6
         H(J) = .5E5
      ENDDO
      DO J=7,12
         H(J) = 2.E5
      ENDDO
      DO J=13,15
         H(J) = 15.E5
      ENDDO
      NP = 15
*
* Set t,p, and r (H2O for 1mm precipitable content) profiles
      HEIGHT = H0
      P1 = P0
      T1 = T0
      R1 = .5
      DO J = 1, NP
         HEIGHT = HEIGHT + H(J)/100000.  ! in km.
         P(J) = P1
         T(J) = T1
         R(J) = R1
         CALL ASE45(PE,TE,DE,HEIGHT)
         CALL ASJ45(PJ,TJ,DJ,HEIGHT)
         P1 = APE*PE+APJ*PJ
         T1 = ATE*TE+ATJ*TJ
         D1 = DE*(1+(P1-PE)/PE-(T1-TE)/TE)
         R1 = .5*EXP(-.5*(HEIGHT-H0))
         IF(HEIGHT.GT.15.) R1 = R1 + D1*2E-6
         P(J) = (P(J) + P1)/2.
         T(J) = (T(J) + T1)/2.
         R(J) = (R(J) + R1)/2.
      ENDDO
      RETURN
*
      ENTRY ATMTRANSM(WATER,AIRMASS,FREQ,TEMI,TATM,TAUOX,TAUW,TAUT
     $,IER)
C----------------------------------------------------------------------
C Compute atmospheric emission and  absorption.
C
C Input:
C 	water 	R	H2O precipitable content(mm)
C	airmass R	Number of air masses
C	freq 	R	Frequency		(GHz)
C
C Output:
C	temi	R	atmosph emission	(K)
C	tatm   	R	mean temperature	(K)
C	tauox	R	Oxygen optical depth  AT ZENITH	(nepers)
C       tauw   	R	Water  optical depth  AT ZENITH	(nepers)
C       taut   	R	Total  optical depth  AT ZENITH	(nepers)
C	IER	I	Error code
C----------------------------------------------------------------------
      DO J = 1, NP
         RR(J) = R(J) * WATER
      ENDDO
      IER = 0
      CALL KVATM(NP,P,T,RR,H,TAUW,TAUOX,FREQ,TEMI,TATM,TAG,TAGU,TOX,
     $TOXI,0,0,TAUT,AIRMASS,IER)
      TAUOX = TAUOX / AIRMASS            ! RL 14 MAR 86
      TAUW = TAUW / AIRMASS              !
      TAUT = TAUT / AIRMASS              !
      RETURN
*
      ENTRY ATMPATH(WATER,AIRMASS,FREQ,PATH,IER)
C----------------------------------------------------------------------
C       integrated optical pathlength of atmosphere
C
C	np .... numero de capas
C	h  .... espesor de las capas     (cm)
C	p  .... presion (milibares)
C	t  .... temperatura (k)
C	rho ... cantidad de vapor de agua (gr/m**3)
C----------------------------------------------------------------------
C-----------------------------------------------------------------------
* MB: zenith distance angle from airmass (parallel layers):
      Z = ACOS( 1. / AIRMASS)
      C_SNELL = -1
      PATH    = 0.
*
      DO J=1,NP
*
* partial pressure of water vapor. Rspec = Rgas/M_H2O = 8314/18.02 = 461.4
* Conversion from pascal->mbar 1e-2, g->kg 1e-3:
         PR_AG = 4.614E-03  * T(J) * R(J) * WATER
         CALL EXCESS_PATH (FREQ, P(J), PR_AG, T(J), H(J),
     $   Z, DPATH, C_SNELL, N_INDEX)
*         IF (J .EQ. 1) DI = Z - ASIN(SIN(Z) / N_INDEX)
         PATH = PATH + DPATH
      ENDDO

C      WRITE(*,*)'FORTRAN ERR=', IER

      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      SUBROUTINE EXCESS_PATH (F_GHZ, P_ATM, P_VAP, T, DH,
     $Z, PATH, C_SNELL, N_INDEX)
C--------------------------------------------------------------------------
*
* Calculation of the excess path length (path length difference between
* vacuum and atmospheric propagation) for a parallel medium of
* constant refraction index.
* Source for the path length formula: Thompson, Moran, Swenson (1986),
*                     Interferometry and Synthesis in Radio Astronomy, p 407
*
* Input:  real*4 f_ghz   frequency in GHz
*                p_atm   total atmospherical pressure in millibar
*                p_vap   partial pressure of water vapor in millibar
*                t       temperature in kelvin
*                z       Zenith distance angle (radians)
* Output: real*8 c_snell : Constant of Snell's law
*                n_index : refraction index
*         real*4 path    : path length difference in units of dh.
*
* Author: 25-Jan-1994  Michael Bremer, IRAM
C-------------------------------------------------------------------------
      REAL REFRACT_TOTAL, F_GHZ, P_ATM, P_VAP, T, DH, Z, PATH, REFR
      REAL*8 SIN_Z, COS_Z,  C_SNELL, N_INDEX
*----------------------------------------------
*
      REFR    = REFRACT_TOTAL( F_GHZ, P_ATM, P_VAP, T)
*
* Apply the definition of refractivity to get the refraction index :
*
      N_INDEX = 1.0D+00 + REFR * 1.0D-06
*
* c_snell stays constant along the line of sight (Snell's law) and
* is calculated if the given value is .lt. 0. This should make life
* easier when dealing with multiple layers.
*
      IF (C_SNELL .LT. 0) C_SNELL = SIN( Z ) * N_INDEX
      SIN_Z   = C_SNELL / N_INDEX
      COS_Z   = SQRT(1.D+00 - SIN_Z * SIN_Z)
*
      PATH = REFR * 1.0D-06 * DH / COS_Z
*
      END
*<FF>
      FUNCTION REFRACT_TOTAL (F_GHZ, P_ATM, P_VAP, T )
C-------------------------------------------------------------------
C
C Calculation of the total atmospheric refractivity (dry component and
C water vapor), taking into account the dependences from
C frequency, pressure, temperature
C Source of formulae: Hill and Cliffort (1981), Radio Science 16, pp. 77-82
C                and  Thompson, Moran, Swenson (1986),
C                     Interferometry and Synthesis in Radio Astronomy, p 407
C
C Input: real*4 f_ghz  frequency in GHz
C               p_atm  total atmospherical pressure in millibar
C               p_vap  partial pressure of water vapor in millibar
C               t      temperature in kelvin
C
C Author: 25-Jan-1994  Michael Bremer, IRAM
C
C-------------------------------------------------------------------
      REAL REFRACT_TOTAL, F_GHZ, P_ATM, P_VAP, T
      REAL REF_DRY, REF_VAP,  SC, REFRACT_VAPOR
*
*--------------------------------------------
*
* sc = scaling factor for the wavelenght dependent part of the wet refration
* (normal conditions 300K, 1013mbar, 80% humidity -> partial pressure of
*  water vapor 28.2mbar ):
*
      SC      = (P_VAP / 28.2) * (300./T)**2
      REF_DRY = 77.493 * P_ATM / T
      REF_VAP = - 12.8 * P_VAP / T + REFRACT_VAPOR( F_GHZ ) * SC
      REFRACT_TOTAL = REF_DRY + REF_VAP
      END
*<FF>
      FUNCTION REFRACT_VAPOR (F_GHZ)
C-----------------------------------------------------------------------
C
C Function to calculate the refractivity of water vapor 0-480 GHz, under
C conditions T=300K, P=1atm, 80% rel. humidity (i.e. partial pressure of
C water vapor 28.2 mbar).
C
C Source: Hill and Clifford (1981), Radio Science 16, curve p. 80
C Method of digitalisation: zoomed copy to a transparency,
C                           points read by cursor.
C                           Approx. errors: F +-1.5 GHz, R +-0.1
C
C Author: 24-Jan-1993 Michael Bremer, IRAM
C------------------------------------------------------------------------
      REAL REFRACT_VAPOR, F_GHZ
      INTEGER NPOINT, I
      PARAMETER (NPOINT = 53)
      REAL FREQ(NPOINT), REFR(NPOINT), U
*
      DATA FREQ /   0.00 ,  18.00 ,  22.53 ,  47.95 ,
     $59.41 ,  79.04 ,  98.67 , 115.85 ,
     $133.03 , 152.66 , 167.39 , 181.70 ,
     $183.57 , 185.66 , 188.11 , 195.47 ,
     $215.65 , 235.29 , 250.83 , 271.28 ,
     $288.28 , 305.46 , 315.27 , 321.64 ,
     $323.50 , 327.18 , 328.82 , 336.18 ,
     $348.45 , 358.27 , 366.45 , 371.35 ,
     $374.63 , 377.90 , 379.53 , 378.72 ,
     $381.35 , 382.99 , 387.90 , 393.44 ,
     $405.71 , 427.26 , 431.07 , 437.80 ,
     $441.07 , 446.16 , 448.80 , 449.62 ,
     $456.98 , 469.07 , 472.97 , 476.43 ,
     $480.01 /
      DATA REFR / 115.64 , 115.68 , 115.59 , 115.69 ,
     $115.76 , 115.88 , 116.07 , 116.25 ,
     $116.50 , 116.81 , 117.24 , 118.08 ,
     $117.80 , 116.25 , 116.38 , 117.00 ,
     $117.65 , 118.21 , 118.71 , 119.39 ,
     $120.07 , 120.87 , 121.53 , 122.26 ,
     $122.45 , 121.33 , 121.33 , 122.20 ,
     $123.38 , 124.37 , 125.61 , 126.91 ,
     $128.27 , 129.85 , 125.73 , 121.89 ,
     $119.29 , 119.29 , 121.89 , 123.30 ,
     $125.70 , 129.18 , 129.87 , 132.29 ,
     $132.42 , 134.90 , 129.51 , 125.30 ,
     $129.07 , 133.47 , 134.59 , 134.24 ,
     $134.96 /
* -------------------------------
*
* negative frequencies are NOT accepted (not even in jest):
*
      IF (F_GHZ .LT. 0) THEN
         WRITE(6,*) 'E-ATM,  Error from refract_vapor: frequency < 0'
         STOP 'Negative frequency'
      ENDIF
*
* Find the frequency interval (i-1,i) of the input frequency:
      I = 2
*
10    CONTINUE
      IF (FREQ(I) .GT. F_GHZ) GOTO 20
      I = I + 1
*
      IF (I .LE. NPOINT) GOTO 10
*
*     Print an error message, if the frequency range has been checked and the
*     requested frequency lies beyond, and give the last data range value:
*      PRINT *,'Error from refract_vapor: ',F_GHZ,' outside 0-480 GHz.'
*
      REFRACT_VAPOR = REFR(NPOINT)
      RETURN
*
20    CONTINUE
*
* Perform linear interpolation between the interval borders:
*
      U = (F_GHZ - FREQ(I-1)) / (FREQ(I) - FREQ(I-1))
      REFRACT_VAPOR = REFR(I-1) + (REFR(I) - REFR(I-1)) * U
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      FUNCTION FLIN(V,VL,DV)
C----------------------------------------------------------------------
C J Cernicharo, Model atmosphere.
C
C	FORMA CINETICA DEL PERFIL
C	V... FRECUENCIA
C	VL.. FRECUENCIA DE LA LINEA
C	DV.. ANCHURA DE LA LINEA
C----------------------------------------------------------------------
      REAL FLIN,V,VL,DV,PI,V2
      DATA PI/3.141592654/
*
      FLIN=4.*V*VL*DV/PI
      V2=V*V
      FLIN=FLIN/(4.*V2*DV*DV+(VL*VL-V2)**2)
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      FUNCTION FVVW(V,VL,DV)
C----------------------------------------------------------------------
C	PERFIL DE VAN VLECK & WEISSKOPF
C
C J CERNICHARO
C----------------------------------------------------------------------
      REAL FVVW,V,DV,VL,PI,DV2,A1,A2
      DATA PI/3.141592654/
*
      FVVW=DV*V/VL/PI
      DV2=DV*DV
      A1=DV2+(V-VL)**2
      A2=DV2+(V+VL)**2
      FVVW=FVVW*(1./A1+1./A2)
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      FUNCTION KH2O(RHO,T,P,V,IL)
C----------------------------------------------------------------------
C	COEFICIENTE DE ABSORCION DEL VAPOR DE AGUA ATMOSFERICO
C	T ... ES LA TEMPERATURA (K)
C	P ... ES LA PRESION EN MILIBARES
C	RHO . ES LA CONCENTRACION DE VAPOR DE AGUA EN GR/M**3
C	V ... ES LA FRECUENCIA EN GHZ
C	KH2O . COEFICIENTE DE ABSORCION EN CM-1
C	IL=0   PERFIL CINETICO
C	IL=1   PERFIL DE VAN VLECK & WEISSKPOF
C
C J.Cernicharo
C----------------------------------------------------------------------
      REAL RHO,T,P,V
      INTEGER IL
      REAL FRE(19),GL(19),FLM(19),EL(19),DV0(19),DVLM(19),
     $X(19),B1(9),B2(9),B3(9),FDEB(9)
      REAL KH2O,SUM,TA,TKK,TK,SOM,DV,RD,RD0,TT,GG,PI,FV
      INTEGER L,J
*
      REAL FVVW,FLIN
C
C	FRE ... FRECUENCIAS DE LAS TRANSICIONES DEL VAPOR DE AGUA (GHZ)
C
      DATA FRE/22.23507985,183.3100906,321.225644,325.152919,380.197372,
     $390.14,437.346667,439.150812,443.018295,448.001075,
     $470.888947,474.689127,488.491133,556.936002,620.700807,
     $752.033227,916.62,970.31,987.94/
C
C	GL ... DEGENERACION DE LOS NIVELES
C
      DATA GL/3.,1.,3.,1.,3.,1.,1.,3.,3.,3.,1.,1.,1.,3.,3.,
     $1.,1.,1.,1./
C
C	FLM ... CUADRADO DEL ELEMENTO DE MATRIZ LM
C
      DATA FLM/.057,.102,.089,.091,.123,.068,.088,.0101,.088,
     $.132,.102,.118,.036,1.5,.122,2.073,.161,.262,.7557/
C
C	EM ... ENERGIAS EN CM-1 DEL NIVEL SUPERIOR EN LA TRANS L=>M
C	EL ...     "     "  "    "    "   INFERIOR  "  "   "     "
C
C	DATA EM/447.3,142.27,1293.8,326.62,224.84,1538.31,1059.63,
C	1 756.76,1059.90,300.37/
      DATA EL/446.56,136.16,1283.02,315.78,212.16,1525.31,
     $1045.03,742.11,1045.11,285.42,742.074,488.135,586.48,
     $23.794,488.108,70.091,285.217,383.837,37.137/
      DATA DV0/2.85,2.68,2.3,3.03,3.19,2.11,1.5,1.94,1.51,
     $2.47,1.89,2.07,2.58,3.33,2.28,3.13,2.59,2.48,3.09/
      DATA DVLM/13.68,14.49,12.04,15.21,15.84,11.42,7.94,10.44,
     $8.13,14.24,10.56,11.95,14.77,14.66,12.78,13.93,14.06,14.16,
     $15.20/
C
C	X  ... EXPONENTE DE LA TEMPERATURA
C
      DATA X/.626,.649,.42,.619,.63,.33,.29,.36,.332,.51,
     $.380,.38,.57,.645,.6,.69,.676,.56,.66/
      DATA FDEB/68.052,503.56,504.46,658.34,841.01,859.81,899.38,
     $903.28,906.21/
      DATA B1/1.8E-3,3.5E-3,1.2E-3,4.6E-2,1.2E-3,1.5E-3,9.1E-3,
     $6.4E-3,1.79E-2/
      DATA B2/8.75,6.69,6.69,7.76,8.11,7.99,7.84,8.35,5.04/
      DATA B3/2.8E-3,1.27E-3,1.3E-3,3.28E-3,1.7E-3,2.7E-3,3E-3,
     $2.8E-3,2.04E-3/
      DATA PI/3.141592654/,TK/.69503096/
      KH2O=1.44*RHO*V/SQRT(T**3)
      SUM=0.
      TA=300./T
      TKK=TK*T
      DO 1 L=1,19
C	IF(V.LE.FRE(L)+200..AND.V.GE.FRE(L)-200.)GO TO 5
C	GO TO 1
5        SOM=GL(L)*FLM(L)*EXP(-EL(L)/TKK)*(1.-EXP(-FRE(L)/TKK/29.97925))
         DV=DV0(L)*P/1013./((T/300.)**X(L))
         DV=DV*(1.+(4.6E-03*RHO*T/P)*(DVLM(L)/DV0(L)-1.))
         IF(IL.EQ.0)FV=FLIN(V,FRE(L),DV)
         IF(IL.EQ.1)FV=FVVW(V,FRE(L),DV)
         SUM=SUM+SOM*FV
1     CONTINUE
      KH2O=KH2O*SUM
C
C	TERMINO CORRECTOR EMPIRICO (POSIBLE CONTRIBUCION DE LOS DIMEROS
C				    DE H2O)
C
      KH2O=KH2O+1.08E-11*RHO*V*V*P/1000.*(TA)**2.1
C
C	RAYAS DEBILES
C
      RD=1.937E-9*V*RHO*T
      RD0=0.
      DO 10 J=1,9
C	IF(V.LE.FDEB(J)+100..AND.V.GE.FDEB(J)-100.)GO TO 15
C	GO TO 10
15       TT=B2(J)*(1.-TA)
         TT=B1(J)*EXP(TT)*(TA)**3.5
         GG=TA**0.6*P*B3(J)
         IF(IL.EQ.0)FV=FLIN(V,FDEB(J),GG)
         IF(IL.EQ.1)FV=FVVW(V,FDEB(J),GG)
         RD0=RD0+TT*FV
10    CONTINUE
      RD=RD*RD0
      KH2O=KH2O+RD
      RETURN
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      FUNCTION KO2(T,P,V,IL)
C----------------------------------------------------------------------
C	OPACIDAD DE LA ATMOSFERA DEBIDA AL OXIGENO (O2)
C	T .... ES LA TEMPERATURA (K)
C	P .... ES LA PRESION EN MILIBARES
C	V .... ES LA FRECUENCIA A LA CUAL SE DESEA CALCULAR KO2
C	IL ... =0  PERFIL CINETICO
C	IL ... =1  PERFIL DE VAN VLECK & WEISSKOPF
C	KO2 .. OPACIDAD EN CM-1
C
C	ANCHURA DE LAS RAYAS DE REBER... BUENA APROXIMACION EN LAS ALAS
C
C J.Cernicharo
C----------------------------------------------------------------------
      REAL KO2,T,P,V
      INTEGER IL,L,J
      REAL FMEN(20),FMAS(20),RN(20),FDEB(6),B1(6),B2(6),B3(6)
*
      REAL TA,V2,SUM,E0,DV1,DV,DV2,A1,A2,A3,E,PI,RD,RD0,GG,B,RR
      REAL FLIN,FVVW
C
C	FMEN ... FRECUENCIAS EN GHZ  N-
C
      DATA FMEN/118.750343,62.486255,60.306044,59.164215,
     $58.323885,57.612488,56.968180,56.363393,55.783819,
     $55.221372,54.671145,54.1302,53.5959,53.0669,52.5424,
     $52.0214,51.50302,50.9873,50.4736,49.9618/
C
C	FMAS ... FRECUENCIAS EN GHZ N+
C
      DATA FMAS/56.264766,58.446580,59.590978,60.434776,
     $61.15057,61.800169,62.411223,62.997991,63.56852,
     $64.127777,64.678914,65.22412,65.764744,66.30206,
     $66.83677,67.36951,67.90073,68.4308,68.9601,69.4887/
C
C	N    ... NUMERO CUANTICO DE ROTACION
C
      DATA RN/1.,3.,5.,7.,9.,11.,13.,15.,17.,19.,21.,23.,
     $25.,27.,29.,31.,33.,35.,37.,39./
C
C	RAYAS CON DN=2
C
      DATA FDEB/368.499,424.7638,487.25,715.3944,773.841,834.147/
      DATA B1/6.79E-6,6.43E-05,2.39E-5,9.79E-6,5.71E-5,1.83E-5/
      DATA B2/.202,.0112,.0112,.0891,.0798,.0798/
      DATA B3/15.6E-4,14.7E-4,14.7E-4,14.4E-4,14E-4,14E-4/
      DATA PI/3.141592654/
      KO2=1.44E-05*P*V/T/T/T
      TA=300./T
      V2=V**2
      SUM=0.
      E0=2.07/T
      DV1=1.41E-03*P*300./T
      DV=DV1
      IF(DV1.GT.0.0527)DV=DV/3.+0.03513
      DV2=DV*DV
      DO 1 L=1,20
         A1=(RN(L)**2+RN(L)+1.)*(2.*RN(L)+1.)/RN(L)/(RN(L)+1.)
         E=E0*RN(L)*(RN(L)+1.)
         A1=A1*2.*V*DV/PI/(V2+DV2)
         A2=RN(L)*(2.*RN(L)+3.)/(RN(L)+1.)
         IF(IL.EQ.0)A2=A2*FLIN(V,FMAS(L),DV)*FMAS(L)
         IF(IL.EQ.1)A2=A2*FVVW(V,FMAS(L),DV)*FMAS(L)
         A3=(RN(L)+1.)*(2.*RN(L)-1.)/RN(L)
         B=DV
         IF(L.EQ.1)B=DV1
         IF(IL.EQ.0.)A3=A3*FMEN(L)*FLIN(V,FMEN(L),B)
         IF(IL.EQ.1.)A3=A3*FMEN(L)*FVVW(V,FMEN(L),B)
1     SUM=SUM+(A1+A2+A3)*EXP(-E)
      KO2=SUM*KO2
C
C	RAYAS CON DN=2
C
      RD=P*TA**3*4.193E-07*V
      RD0=0.
      DO 10 J=1,6
C	IF(V.LE.FDEB(J)+200..AND.V.GE.FDEB(J)-200.)GO TO 15
C	GO TO 10
15       RR=B1(J)*EXP(B2(J)*(1.-TA))
         GG=B3(J)*P*TA**.9
         IF(IL.EQ.0)RR=RR*FLIN(V,FDEB(J),GG)
         IF(IL.EQ.1)RR=RR*FVVW(V,FDEB(J),GG)
         RD0=RD0+RR
10    CONTINUE
      RD=RD*RD0
      KO2=KO2+RD
      RETURN
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      SUBROUTINE KVATM(NP,P,T,RHO,H,AGU,OXI,V,TEMI,TATM,TAG,TAGU,TOX,
     $TOXI,ILAG,ILOX,KVAT,AMA,IER)
C----------------------------------------------------------------------
C	opacidad de la atmosfera a la frecuencia 'v' debida al
C	vapor de agua y al oxigeno.
C
C	np .... numero de capas
C	h  .... espesor de las capas     (cm)
C	p  .... presion (milibares)
C	t  .... temperatura (k)
C	rho ... cantidad de vapor de agua (gr/m**3)
C	temi .. emisividad total de la atmosfera     (k)
C	kvat .. opacidad total (nepers)
C	tatm .. temperatura media de la atmosfera   (k)
C	agu ... opacidad debida al vapor de agua (nepers)
C	oxi ... opacidad debida al oxigeno       (   "  )
C	tag ... emisividad debida al vapor de agua (atmosfera sin oxigeno)
C	tox ... idem para el oxigeno (sin vapor de agua)
C	tagu .. temperatura media de la capa de vapor de agua (k)
C	toxi .. temperatura media de la capa de oxigeno (k)
C
C J.Cernicharo
C----------------------------------------------------------------------
      INTEGER NP,ILAG,ILOX,IER
      REAL AGU,OXI,V,TEMI,TATM,TAG,TAGU,TOX,TOXI,KVAT,AMA
      REAL H(*),P(*),T(*),RHO(*)
*
      INTEGER J
      REAL R,PR,TEM,DH,OX,AG,KV
      REAL KH2O, KO2
*
      TEMI=0.
      KV=0.
      TAG=0.
      TOX=0.
      AGU=0.
      OXI=0.
      DO 1 J=1,NP
         R=RHO(J)
         PR=P(J)
         TEM=T(J)
         DH=H(J)
         AG=KH2O(R,TEM,PR,V,ILAG)*DH*AMA
         OX=KO2(TEM,PR,V,ILOX)*DH*AMA
         TAG=TAG+TEM*EXP(-AGU)*(1.-EXP(-AG))
         AGU=AGU+AG
         TOX=TOX+TEM*EXP(-OXI)*(1.-EXP(-OX))
         OXI=OXI+OX
         TEMI=TEMI+TEM*EXP(-KV)*(1.-EXP(-AG-OX))
1     KV=AGU+OXI
      KVAT=KV
      IF ( KV.LE.1.E-10 ) THEN
         IER = 1
      ELSEIF ( OXI.LE.1.E-20) THEN
         IER = 2
      ELSEIF ( AGU.LE.1.E-20 ) THEN
         IER = 3
      ELSE
         TATM=TEMI/(1.-EXP(-KV))
         TAGU=TAG/(1.-EXP(-AGU))
         TOXI=TOX/(1.-EXP(-OXI))
         IER = 0
      ENDIF
      END
* Last processed by NICE on 05-Jul-2001 14:40:00
* Customized for :  IEEE, LINUX, UNIX, MOTIF, F77
      SUBROUTINE POLI2(X1,X2,X3,Y1,Y2,Y3,A,B,C)
C----------------------------------------------------------------------
C	ESTA SUBRUTINA CALCULA LOS COEFICIENTES A,B,C DEL POLINOMIO DE
C	SEGUNDO GRADO A+BX+CX**2, QUE PASA POR LOS PUNTOS (X1,Y1),
C	(X2,Y2),(X3,Y3)
C J.Cernicharo
C----------------------------------------------------------------------
      REAL X1,X2,X3,Y1,Y2,Y3,A,B,C
*
      C=(Y3-Y2)*(X2-X1)-(Y2-Y1)*(X3-X2)
      B=(X2-X1)*(X3*X3-X2*X2)-(X2*X2-X1*X1)*(X3-X2)
      C=C/B
      B=(Y2-Y1)-C*(X2*X2-X1*X1)
      B=B/(X2-X1)
      A=Y1-C*X1*X1-B*X1
      END
