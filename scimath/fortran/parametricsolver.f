*=======================================================================
*     Copyright (C) 1999,2001,2002,2003
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
*            Internet email: aips2-request@nrao.edu.
*            Postal address: AIPS++ Project Office
*                            National Radio Astronomy Observatory
*                            520 Edgemont Road
*                            Charlottesville, VA 22903-2475 USA
*
*     $Id$
*-------------------------------------------------------------------------
C
C Routines for polynomial and spline fitting, using the global
C baseline data to get antenna based coefficients
C Written by  R.Lucas for IRAM gildas.
C

      SUBROUTINE POLYANT(IY, M, NBAS, IANT, JANT, IREF,
     $     KPLUS1, NANT, X, Y, W,
     $     WK1, WK2, WK3, SS, C) 
C------------------------------------------------------------------------
C     polyant computes a weighted least-squares polynimial approximation
C     to the antenna amplitudes or phases
c     for an arbitrary set of baseline data points.
c parameters:
C     iy             I   Input    1 for log(amplitude), 2 for phase
c     m              I   Input    the number of data points for each baseline
c     nbas           I   Input    the number of baselines
c     iant(nbas)     I   Input    start antenna for each baseline
c     jant(nbas)     I   Input    end  antenna for each baseline
c     iref           i   Input    Reference antenna for phases
c     kplus1         I   Input    degree of polynomials + 1
c     nant           I   Input    the number of antennas
c     x(m)           R8  Input    the data abscissae
c     y(m,nbas)      R8  Input    the data values
c     w(m,mbas)      R8  Input    weights
c     wk1(kplus1)    R8  Output   work space
c     wk2(kplus1**2*nant**2)
c                    R8  Output   work space
c     wk3(kplus1*nant)
c                    R8  Output   work space
c     ss(nbas)       R8  Output   rms of fit for each baseline
c     c(nant,kplus1) R8  Output   the polynomial coefficients
c
C------------------------------------------------------------------------
* Dummy
      INTEGER M, KPLUS1, NANT, NBAS, IANT(NBAS), JANT(NBAS), IY, IREF
      REAL*8 C(NANT,KPLUS1), W(M,NBAS),
     $WK2(KPLUS1*NANT,KPLUS1*NANT), WK3(KPLUS1*NANT),
     $WK1(KPLUS1), X(M), Y(M,NBAS), SS(NBAS), NORM, TOL 
      LOGICAL ERROR
*
      PARAMETER (TOL=1D-14)
c-------------------------------------------------------
* PI is defined with more digits than necessary to avoid losing
* the last few bits in the decimal to binary conversion
      REAL*8 PI
      PARAMETER (PI=3.14159265358979323846D0)
      REAL*4 PIS
      PARAMETER (PIS=3.141592653)
* Relative precision of REAL*4
      REAL*4 EPSR4
      PARAMETER (EPSR4=1E-7)
      REAL*4 MAXR4
      PARAMETER (MAXR4=1E38)
* Maximum acceptable integer
      INTEGER MAX_INTEG
      PARAMETER (MAX_INTEG=2147483647)
C-------------------------------------------------

* Local
      INTEGER ZANT, I, IA, IB, NANTM1, J,
     $JA, IC, IL, KN, KP, K, L, ITER, INFO
      REAL*8  WI, XI, WN, WW, WWW, TEST, YI, XCAP, X1, XN, D, WSS(4000)
C------------------------------------------------------------------------
C Code
c
c Check that the weights are positive.
c


      ERROR=.FALSE.

      DO  I=1,M
         DO IB = 1, NBAS
            IF (W(I,IB).LT.0.0D0) THEN
C               CALL MESSAGE(6,4,'POLYANT','Weights not positive')
               write(*,*)'POLYANT: Weights not positive'
               ERROR = .TRUE.
               RETURN
            ENDIF
         ENDDO
      ENDDO
      X1 = X(1)
      XN = X(M)
      D = XN-X1
c
* Amplitude case is simple...
c
      IF (IY.EQ.1) THEN
         DO I=1,NANT*KPLUS1
            DO L=1,NANT*KPLUS1
               WK2(L,I) = 0.0D0
            ENDDO
            WK3(I) = 0.0D0
         ENDDO
         DO I=1, M
            XI = X(I)
            XCAP = ((XI-X1)-(XN-XI))/D
* compute the chebychev polynomials at point xi.
            CALL CHEB (KPLUS1, XCAP, WK1, ERROR)
            IF (ERROR) GOTO 999
*
c store the upper-triangular part of the normal equations in wk2
c and the right-hand side in wk3.
c
            DO K=1, KPLUS1
               WN = WK1(K)
               IL = (K-1)*NANT
               DO KP = 1, KPLUS1
                  WW = WN*WK1(KP)
                  IC = (KP-1)*NANT
                  DO IB=1,NBAS
                     WI = W(I,IB)
                     IF (WI.GT.0) THEN
                        IA = IANT(IB)
                        JA = JANT(IB)
                        WWW = WI*WW
                        WK2(IL+IA,IC+IA) = WK2(IL+IA,IC+IA)+WWW
                        WK2(IL+IA,IC+JA) = WK2(IL+IA,IC+JA)+WWW
                        WK2(IL+JA,IC+IA) = WK2(IL+JA,IC+IA)+WWW
                        WK2(IL+JA,IC+JA) = WK2(IL+JA,IC+JA)+WWW
                     ENDIF
                  ENDDO
               ENDDO
               DO IB=1, NBAS
                  IA = IANT(IB)
                  JA = JANT(IB)
                  WI = W(I,IB)*WN*Y(I,IB)
*                  wi = w(i,ib)*wn*xcap*(ja+ia) !**!
                  WK3(IL+IA) = WK3(IL+IA) + WI
                  WK3(IL+JA) = WK3(IL+JA) + WI
               ENDDO
            ENDDO
         ENDDO
C
C Solve the system of normal equations by first computing the Cholesky
c factorization
*
         CALL MTH_DPOTRF ('POLYANT',
     $   'U',KPLUS1*NANT,WK2,KPLUS1*NANT,INFO)


         IF (ERROR) GOTO 999
         CALL MTH_DPOTRS ('POLYANT',
     $   'U',KPLUS1*NANT,1,WK2,KPLUS1*NANT,
     $   WK3,KPLUS1*NANT,INFO)

         IF (ERROR) GOTO 999
         DO J=1,KPLUS1
            DO I=1,NANT
               C(I,J) = WK3(I+(J-1)*NANT)
            ENDDO
         ENDDO
*
* Phase is more complicated ...
      ELSEIF (IY.EQ.2) THEN
         NANTM1 = NANT-1
         DO I=1,NANT
            DO J=1,KPLUS1
               C(I,J) = 0.0D0
            ENDDO
         ENDDO
*
* start iterating
         NORM = 1E10
         ITER = 0
         DO WHILE (NORM.GT.TOL .AND. ITER.LT.100)
            ITER = ITER + 1
            DO I=1,NANTM1*KPLUS1
               DO L=1,NANTM1*KPLUS1
                  WK2(L,I) = 0.0D0
               ENDDO
               WK3(I) = 0.0D0
            ENDDO
            DO I=1, M
               XI = X(I)
               XCAP = ((XI-X1)-(XN-XI))/D
* compute the chebychev polynomials at point xi.
               CALL CHEB (KPLUS1, XCAP, WK1, ERROR)
               IF (ERROR) GOTO 999
*
               DO K=1, KPLUS1
                  WN = WK1(K)
                  IL = (K-1)*NANTM1
                  DO KP = 1, KPLUS1
                     WW = WN*WK1(KP)
                     IC = (KP-1)*NANTM1
                     DO IB=1,NBAS
                        WI = W(I,IB)
                        IF (WI.GT.0) THEN


                           IA = ZANT(IANT(IB),IREF)
                           JA = ZANT(JANT(IB),IREF)
                           WWW = WI*WW
                           IF (IA.NE.0) THEN
                              WK2(IL+IA,IC+IA) = WK2(IL+IA,IC+IA)+WWW
                           ENDIF
                           IF (JA.NE.0) THEN
                              WK2(IL+JA,IC+JA) = WK2(IL+JA,IC+JA)+WWW
                           ENDIF
                           IF (IA.NE.0 .AND. JA.NE.0) THEN
                              WK2(IL+JA,IC+IA) = WK2(IL+JA,IC+IA)-WWW
                              WK2(IL+IA,IC+JA) = WK2(IL+IA,IC+JA)-WWW
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
                  DO IB=1, NBAS
                     IF (W(I,IB).GT.0) THEN
                        YI = Y(I,IB)
                        IA = IANT(IB)
                        JA = JANT(IB)
                        DO KP=1, KPLUS1
                           YI = YI+(C(IA,KP)-C(JA,KP))*WK1(KP)
                        ENDDO
*                  yi = xcap*(ja-ia) !**!
                     ELSE
                        YI = 0
                     ENDIF
*                     if (norm.lt.1e9) then
                     YI = SIN(YI)
*                     endif

                     IA = ZANT(IANT(IB),IREF)
                     JA = ZANT(JANT(IB),IREF)
                     WI = W(I,IB)*WN*YI
                     IF (IA.NE.0) THEN
                        WK3(IA+IL) = WK3(IA+IL) - WI
                     ENDIF
                     IF (JA.NE.0) THEN
                        WK3(JA+IL) = WK3(JA+IL) + WI
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO

C Solve the system of normal equations by first computing the Cholesky
c factorization
            CALL MTH_DPOTRF('POLYANT',
     $      'U',KPLUS1*NANTM1,WK2,KPLUS1*NANT,INFO)
            IF (ERROR) GOTO 999
            CALL MTH_DPOTRS ('POLYANT',
     $      'U',KPLUS1*NANTM1,1,WK2,KPLUS1*NANT,
     $      WK3,KPLUS1*NANTM1,INFO)
            IF (ERROR) GOTO 999

*
*     Add the result to c:
            NORM = 0
            DO J=1,KPLUS1
               DO IA=1,NANT
                  I = ZANT(IA,IREF)
                  IF (I.NE.0) THEN
                     WW = WK3(I+(J-1)*NANTM1)
                     C(IA,J) = C(IA,J)+WW
                     NORM = NORM+WW**2
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF

c

    
c loop over data points to get rms
      DO IB=1, NBAS
         SS(IB) = 0
         WSS(IB) = 0
      ENDDO


      DO I = 1, M
         XI = X(I)
         XCAP = ((XI-X1)-(XN-XI))/D
         CALL CHEB (KPLUS1, XCAP, WK1, ERROR)
         IF (ERROR) GOTO 999
         DO IB=1,NBAS
            IF (W(I,IB).GT.0) THEN
               IA = IANT(IB)
               JA = JANT(IB)
               TEST = 0
               DO KN = 1, KPLUS1
                  WN = WK1(KN)
                  IF (IY.EQ.1) THEN
                     TEST = TEST+(C(IA,KN)+C(JA,KN))*WN
                  ELSE
                     TEST = TEST+(-C(IA,KN)+C(JA,KN))*WN
                  ENDIF
               ENDDO
               TEST = Y(I,IB)-TEST
               IF (IY.EQ.2) TEST = MOD(TEST+11*PI,2*PI)-PI
               SS(IB) = SS(IB)+W(I,IB)*TEST**2
               WSS(IB) = WSS(IB)+W(I,IB)
            ENDIF
         ENDDO
      ENDDO

      DO IB=1, NBAS
         IF (WSS(IB).NE.0) THEN
            SS(IB) = SQRT(SS(IB)/WSS(IB))
         ELSE
            SS(IB) = 0
         ENDIF
      ENDDO

      RETURN
999   ERROR = .TRUE.
      RETURN
      END
*<FF>
      SUBROUTINE CHEB(NPLUS1, XCAP, P, ERROR)
C
C  Compute nplus1 Che+bishev Polynomials at x = xcap
C
      LOGICAL ERROR
      INTEGER NPLUS1
      REAL*8 XCAP, P(NPLUS1)
*
      REAL*8 BK, BKP1, BKP2, DK, ETA, FACTOR
      INTEGER N, K
*
c
c     eta  is the smallest positive number such that
c     the computed value of  1.0 + eta  exceeds unity.
c          with NAG,  ETA = X02AAF(1.0D0)
      DATA ETA/1.110223024625156D-16/
*
      ERROR=.FALSE.

      IF (NPLUS1.LT.1) THEN
         WRITE(*,*) 'F-CHEB, nplus1.lt.1'
         ERROR = .TRUE.
         RETURN
      ENDIF
      IF (DABS(XCAP).GT.1.0D0+4.0D0*ETA) THEN
         WRITE(*,*) 'F-CHEB, abs(xcap).gt.1'
      ENDIF
      P(1) = 0.5D0
      IF (NPLUS1.LE.1) RETURN
      N = NPLUS1 - 1
      K = N + 2
      IF (XCAP.LT.-0.5D0) THEN
c
c Gentleman*s modified recurrence.
         FACTOR = 2.0D0*(1.0D0+XCAP)
         DK = -1.0D0
         BK = 0.0D0
         DO  K=1,N
            DK = - DK + FACTOR*BK
            BK = DK - BK
            P(K+1) = - DK + 0.5D0*FACTOR*BK
         ENDDO
      ELSEIF (XCAP.LE.0.5D0) THEN
c
c Clenshaw*s original recurrence.
c
         FACTOR = 2.0D0*XCAP
         BKP1 = 0.0D0
         BKP2 = -1.0D0
         DO K=1,N
            BK = - BKP2 + FACTOR*BKP1
            P(K+1) = - BKP1 + 0.5D0*FACTOR*BK
            BKP2 = BKP1
            BKP1 = BK
         ENDDO
      ELSE
c
c Reinsch*s modified recurrence.
c
         FACTOR = 2.0D0*(1.0D0-XCAP)
         DK = 1.0D0
         BK = 0.0D0
         DO K=1,N
            DK = DK - FACTOR*BK
            BK = BK + DK
            P(K+1) = DK - 0.5D0*FACTOR*BK
         ENDDO
      ENDIF
      P(1) = 0.5D0
      RETURN
      END
* Linear Algebra: use LAPACK routines
*<FF>
      SUBROUTINE MTH_DPOTRF (NAME, UPLO, N, A, LDA, INFO)
      CHARACTER*(*)      NAME, UPLO
      INTEGER            INFO, LDA, N
      REAL*8             A( LDA, * )
      LOGICAL ERROR
*
*  Purpose
*  =======
*
*  DPOTRF computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.
*
*  The factorization has the form
*     A = U**T * U,  if UPLO = 'U', or
*     A = L  * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*
*  This is the block version of the algorithm, calling Level 3 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite, and the factorization could not be
*                completed.
*
*
* Call LAPACK routine


      CALL DPOTRF  (UPLO, N, A, LDA, INFO )
C      CALL MTH_FAIL(NAME,'MTH_DPOTRF',INFO,ERROR)
      END


      SUBROUTINE MTH_DPOTRS (NAME,
     $UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      CHARACTER*(*)      UPLO, NAME
      INTEGER            INFO, LDA, LDB, N, NRHS
      REAL*8             A( LDA, * ), B( LDB, * )
      LOGICAL ERROR
*
*  Purpose
*  =======
*
*  DPOTRS solves a system of linear equations A*X = B with a symmetric
*  positive definite matrix A using the Cholesky factorization
*  A = U**T*U or A = L*L**T computed by DPOTRF.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The triangular factor U or L from the Cholesky factorization
*          A = U**T*U or A = L*L**T, as computed by DPOTRF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
* Call LAPACK routine
      CALL DPOTRS  (UPLO, N, NRHS, A, LDA, B, LDB, INFO )
C      CALL MTH_FAIL(NAME,'MTH_DPOTRF',INFO,ERROR)
      END


      FUNCTION ZANT(I,R)
      INTEGER I, R, ZANT
      IF (I.EQ.R) THEN
         ZANT = 0
      ELSEIF (I.GT.R) THEN
         ZANT = I-1
      ELSE
         ZANT = I
      ENDIF
      RETURN
      END

      SUBROUTINE SPLINANT(IY, M, NBAS, IANT, JANT, IREF,
     $NCAP7, NANT, X, Y, W,
     $K, WK1, WK2, WK3, SS, C)
C------------------------------------------------------------------------
C     splinant computes a weighted least-squares approximation
c     to an arbitrary set of data points, either amplitude or phase.
c     with knots prescribed by the user.
c parameters:
C     iy            I   Input    1 for log(amplitude), 2 for phase
c     m             I   Input    the number of data points for each baseline
c     nbas          I   Input    the number of baselines
c     iant(nbas)    I   Input    start antenna for each baseline
c     jant(nbas)    I   Input    end  antenna for each baseline
c     iref          i   Input    Reference antenna for phases
c     ncap7         I   Input    number of knots for splines (see e02baf)
c     nant          I   Input    the number of antennas
c     X(m)          R8  Input    the data abscissae
c     Y(m,nbas)     R8  Input    the data values
c     W(m,mbas)     R8  Input    weights
c     k(ncap7)      R8  Input    knots for the splines (inners + 4 at each end)
c     wk1(4,m)    R8  Output   wk space
c     wk2(4*nant,ncap7*nant)
c                   R8  Output   work space
c     wk3(ncap7*nant)
c                   R8  Output   work space
c     ss(nbas)      R8  Output   rms of fit for each baseline
c     c(nant,ncap7) R8  Output   the spline coefficients (ncap3 values)
c
C------------------------------------------------------------------------
* Dummy
      INTEGER M, NCAP7, NANT, NBAS, IANT(NBAS), JANT(NBAS), IY, IREF
      REAL*8 C(NANT,NCAP7), K(NCAP7), W(M,NBAS), WK1(4,M),
     $WK2(4*NANT,NCAP7*NANT), WK3(NCAP7*NANT),
     $X(M), Y(M,NBAS), SS(NBAS),
     $NORM, TOL
      LOGICAL ERROR
      PARAMETER (TOL=1E-14)
      INTEGER ZANT
c-------------------------------------------------------
      REAL*8 PI
      PARAMETER (PI=3.14159265358979323846D0)
      REAL*4 PIS
      PARAMETER (PIS=3.141592653)
* Relative precision of REAL*4
      REAL*4 EPSR4
      PARAMETER (EPSR4=1E-7)
      REAL*4 MAXR4
      PARAMETER (MAXR4=1E38)
* Maximum acceptable integer
      INTEGER MAX_INTEG
      PARAMETER (MAX_INTEG=2147483647)
C-------------------------------------------------
* Local
      INTEGER I, IA, IB, JJ, NANTM1, ITER, JA, IBD, ICOL, NBD,
     $KN, KP, J, JOLD,  L, NCAP3, NCAP, NCAPM1
      REAL*8  D4, D5, D6, D7,  D8, D9,  E2, E3, E4, E5, K1, K2, K3,
     $K4, K5, K6, N1, N2, N3, WI, XI, WN, WW, WWW, TEST, YI
C------------------------------------------------------------------------
C Code
c
      ERROR=.FALSE.

      CALL  SPLINE_CHECK(M, NCAP7, X, K, WK1, ERROR)
      IF (ERROR) RETURN
c
c Check that the weights are (strictly) positive.
c

      DO  I=1,M
         DO IB = 1, NBAS
            IF (W(I,IB).LT.0.0D0) THEN
               WRITE(*,*)'SPLINANT','Weights not positive'
               ERROR = .TRUE.
               RETURN
            ENDIF
         ENDDO
      ENDDO
      NCAP = NCAP7 - 7
      NCAPM1 = NCAP - 1
      NCAP3 = NCAP + 3
      NBD = 4*NANT
      NANTM1 = NANT-1
c
c First loop on data abscissae to compute the spline values in wk1
c
      J = 0
      JOLD = 0
      DO I = 1, M
c
c for the data point  (x(i), y(i,ib))  determine an interval
c k(j + 3) .le. x .lt. k(j + 4)  containing  x(i).  (in the
c case  j + 4 .eq. ncap  the second equality is relaxed to
c include equality).
c
         XI = X(I)
         DO WHILE (XI.GE.K(J+4) .AND. J.LE.NCAPM1)
            J = J + 1
         ENDDO
         IF (J.NE.JOLD) THEN
c
c set certain constants relating to the interval
c k(j + 3) .le. x .le. k(j + 4)
c (i.e. the jth non vanishing interval)
c
            K1 = K(J+1)
            K2 = K(J+2)
            K3 = K(J+3)
            K4 = K(J+4)
            K5 = K(J+5)
            K6 = K(J+6)
            D4 = 1.0D0/(K4-K1)
            D5 = 1.0D0/(K5-K2)
            D6 = 1.0D0/(K6-K3)
            D7 = 1.0D0/(K4-K2)
            D8 = 1.0D0/(K5-K3)
            D9 = 1.0D0/(K4-K3)
            JOLD = J
         ENDIF
c
c compute and store in  wk1(l,i) (l = 1, 2, 3, 4)  the values
c of the four normalized cubic b-splines which are non-zero at
c x=x(i), i.e. the splines of indexes j, j+1, j+2, j+3.
c
         E5 = K5 - XI
         E4 = K4 - XI
         E3 = XI - K3
         E2 = XI - K2
         N1 = E4*D9*D7
         N2 = E3*D9*D8
         N3 = E3*N2*D6
         N2 = (E2*N1+E5*N2)*D5
         N1 = E4*N1*D4
         WK1(4,I) = E3*N3
         WK1(3,I) = E2*N2 + (K6-XI)*N3
         WK1(2,I) = (XI-K1)*N1 + E5*N2
         WK1(1,I) = E4*N1
      ENDDO
c
* Amplitude case is simple...
c
      IF (IY.EQ.1) THEN
         NBD = 4*NANT
         DO I=1,NANT*NCAP3
            DO L=1,NBD
               WK2(L,I) = 0.0D0
            ENDDO
            WK3(I) = 0.0D0
         ENDDO
         J = 0
         DO I=1, M
            XI = X(I)
            DO WHILE (XI.GE.K(J+4) .AND. J.LE.NCAPM1)
               J = J + 1
            ENDDO
*
c store the upper-triangular part of the normal equations in wk2
*
*            write(*,*) i,j,(wk1(kn,i),kn=1,4)
            DO KN=0, 3
               WN = WK1(KN+1,I)
               DO KP = KN, 3
                  ICOL = (J+KP-1)*NANT
                  IBD = NBD-(KP-KN)*NANT
                  WW = WN*WK1(KP+1,I)
                  DO IB=1,NBAS
                     WI = W(I,IB)
                     IF (WI.GT.0) THEN
                        IA = IANT(IB)
                        JA = JANT(IB)
                        WWW = WI*WW
                        WK2(IBD,ICOL+IA) = WK2(IBD,ICOL+IA)+WWW
                        WK2(IBD,ICOL+JA) = WK2(IBD,ICOL+JA)+WWW
                        WK2(IBD+IA-JA,ICOL+JA) =
     $                  WK2(IBD+IA-JA,ICOL+JA) + WWW
                        IF (KP.GT.KN) THEN
                           WK2(IBD+JA-IA,ICOL+IA) =
     $                     WK2(IBD+JA-IA,ICOL+IA) + WWW
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
               DO IB=1, NBAS
                  IA = IANT(IB)
                  JA = JANT(IB)
                  WI = W(I,IB)*WN*Y(I,IB)
                  JJ = (J+KN-1)*NANT
                  WK3(IA+JJ) = WK3(IA+JJ) + WI
                  WK3(JA+JJ) = WK3(JA+JJ) + WI
               ENDDO
            ENDDO
         ENDDO
C
C Solve the system of normal equations by first computing the Cholesky
c factorization
         CALL MTH_DPBTRF ('SPLINANT',
     $   'U',NCAP3*NANT,NBD-1,WK2,4*NANT,ERROR)
         IF (ERROR) RETURN
         CALL MTH_DPBTRS ('SPLINANT',
     $   'U',NCAP3*NANT,NBD-1,1,WK2,4*NANT,
     $   WK3,NCAP3*NANT,ERROR)
         IF (ERROR) RETURN
         DO J=1,NCAP3
            DO I=1,NANT
               C(I,J) = WK3(I+(J-1)*NANT)
            ENDDO
         ENDDO
* Phase is more complicated ...
      ELSEIF (IY.EQ.2) THEN
         NBD = 4*NANTM1
         DO I=1,NANT
            DO J=1,NCAP3
               C(I,J) = 0.0D0
            ENDDO
         ENDDO
*     start iterating
         NORM = 1E10
         ITER = 0
         DO WHILE (NORM.GT.TOL .AND. ITER.LT.100)
            ITER = ITER+1
            DO I=1,NANTM1*NCAP3
               DO L=1,NBD
                  WK2(L,I) = 0.0D0
               ENDDO
               WK3(I) = 0.0D0
            ENDDO
            J = 0
            I = 0
            XI = X(1)
            DO I=1, M
               XI = X(I)
               DO WHILE (XI.GE.K(J+4) .AND. J.LE.NCAPM1)
                  J = J + 1
               ENDDO
*
c store the upper-triangular part of the normal equations in wk2
*
               DO KN=0, 3
                  WN = WK1(KN+1,I)
                  DO KP = KN, 3
                     ICOL = (J+KP-1)*NANTM1
                     IBD = NBD-(KP-KN)*NANTM1
                     WW = WN*WK1(KP+1,I)
                     DO IB=1,NBAS
                        WI = W(I,IB)
                        IF (WI.GT.0) THEN
                           WWW = WI*WW
                           IA = ZANT(IANT(IB),IREF)
                           JA = ZANT(JANT(IB),IREF)
                           IF (IA.NE.0) THEN
                              WK2(IBD,ICOL+IA) = WK2(IBD,ICOL+IA)+WWW
                           ENDIF
                           IF (JA.NE.0) THEN
                              WK2(IBD,ICOL+JA) = WK2(IBD,ICOL+JA)+WWW
                           ENDIF
                           IF (IA.NE.0 .AND. JA.NE.0) THEN
                              WK2(IBD+IA-JA,ICOL+JA) =
     $                        WK2(IBD+IA-JA,ICOL+JA) - WWW
                              IF (KP.GT.KN) THEN
                                 WK2(IBD+JA-IA,ICOL+IA) =
     $                           WK2(IBD+JA-IA,ICOL+IA) - WWW
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
               DO IB=1, NBAS
                  IF (W(I,IB).GT.0) THEN
                     IA = IANT(IB)
                     JA = JANT(IB)
                     YI = Y(I,IB)
                     IF (J.LE.NCAP3) THEN
                        DO KN = 0, 3
                           WN = WK1(KN+1,I)
                           IF (J+KN.LE.NCAP3) THEN
                              YI = YI+C(IA,J+KN)*WN
                              YI = YI-C(JA,J+KN)*WN
                           ENDIF
                        ENDDO
                     ELSE
                        YI = 0
                     ENDIF
                     IF (NORM.LT.1E9) THEN
                        YI = SIN(YI)
                     ENDIF
                     IA = ZANT(IANT(IB),IREF)
                     JA = ZANT(JANT(IB),IREF)
                     DO KN = 0, 3
                        WN = WK1(KN+1,I)
                        WI = W(I,IB)*WN*YI
                        JJ = (J+KN-1)*NANTM1
                        IF (IA.NE.0) THEN
                           WK3(IA+JJ) = WK3(IA+JJ) - WI
                        ENDIF
                        IF (JA.NE.0) THEN
                           WK3(JA+JJ) = WK3(JA+JJ) + WI
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
C
C Solve the system of normal equations by first computing the Cholesky
c factorization
            CALL MTH_DPBTRF ('SPLINANT',
     $      'U',NCAP3*NANTM1,NBD-1,WK2,4*NANT,ERROR)
            IF (ERROR) RETURN
            CALL MTH_DPBTRS ('SPLINANT',
     $      'U',NCAP3*NANTM1,NBD-1,1,WK2,4*NANT,
     $      WK3,NCAP3*NANT,ERROR)
            IF (ERROR) RETURN
*
* Add the result to c:
            NORM = 0
            DO J=1,NCAP3
               DO IA=1,NANT
                  I = ZANT(IA,IREF)
                  IF (I.NE.0) THEN
                     WW = WK3(I+(J-1)*NANTM1)
                     C(IA,J) = C(IA,J)+WW
                     NORM = NORM+WW**2
                  ENDIF
               ENDDO
            ENDDO

         ENDDO

      ENDIF
c
c loop over data points to get rms
      DO I=1, NBAS
         SS(I) = 0
         WK2(I,1) = 0
      ENDDO
      J = 0
      DO I = 1, M
         XI = X(I)
         DO WHILE (XI.GE.K(J+4) .AND. J.LE.NCAPM1)
            J = J + 1
         ENDDO
         DO IB=1,NBAS
            IF (W(I,IB).GT.0) THEN
               IA = IANT(IB)
               JA = JANT(IB)
               TEST = 0
               DO KN = 0, 3
                  WN = WK1(KN+1,I)
                  IF (IY.EQ.1) THEN
                     TEST = TEST+(C(IA,J+KN)+C(JA,J+KN))*WN
                  ELSE
                     TEST = TEST+(-C(IA,J+KN)+C(JA,J+KN))*WN
                  ENDIF
               ENDDO
               TEST = Y(I,IB)-TEST
               TEST = MOD(TEST+11*PI,2*PI)-PI
               SS(IB) = SS(IB)+W(I,IB)*TEST**2
               WK2(IB,1) = WK2(IB,1)+W(I,IB)
            ENDIF
         ENDDO
      ENDDO
      DO IB=1, NBAS
         IF (WK2(IB,1).GT.0) THEN
            SS(IB) = SQRT(SS(IB)/WK2(IB,1))
         ELSE
            SS(IB) = 0
         ENDIF
      ENDDO
*
      RETURN
      END
*

      SUBROUTINE SPLINE_CHECK(M, NCAP7, X, K, WK, ERROR)
C------------------------------------------------------------------------
C     splinant computes a weighted least-squares approximation
c     to an arbitrary set of data points by a cubic spline
c     with knots prescribed by the user.
C------------------------------------------------------------------------
* Dummy
      INTEGER M, NCAP7
      REAL*8 K(NCAP7), WK(M), X(M)
      LOGICAL ERROR
* Local
      INTEGER I, J, L, NCAP3, NCAP, NCAPM1, R
      REAL*8 K0, K4
C------------------------------------------------------------------------
C Code
c
c check that the values of  m  and  ncap7  are reasonable
      IF (NCAP7.LT.8 .OR. M.LT.NCAP7-4) GO TO 991
      NCAP = NCAP7 - 7
      NCAPM1 = NCAP - 1
      NCAP3 = NCAP + 3
c
c In order to define the full b-spline basis, augment the
c prescribed interior knots by knots of multiplicity four
c at each end of the data range.
c
      DO  J=1,4
         I = NCAP3 + J
         K(J) = X(1)
         K(I) = X(M)
      ENDDO
c
c test the validity of the data.
c
c check that the knots are ordered and are interior
c to the data interval.
c
      IF (K(5).LE.X(1) .OR. K(NCAP3).GE.X(M)) THEN
         WRITE(*,*)'SPLINE_CHECK',' Knots outside range'
         ERROR = .TRUE.
         RETURN
      ELSE
         DO  J=4,NCAP3
            IF (K(J).GT.K(J+1)) THEN
               WRITE(*,*)'SPLINE_CHECK','Knots non increasing'
               ERROR = .TRUE.
               RETURN
            ENDIF
         ENDDO
      ENDIF
c
c check that the data abscissae are ordered, then form the
c array  wk  from the array  x.  the array  wk  contains
c the set of distinct data abscissae.
c
      WK(1) = X(1)
      J = 2
      DO I=2,M
         IF (X(I).LT.WK(J-1)) THEN
            write(*,*)'SPLINE_CHECK',
     $      'Data abscissae not ordered'
            ERROR = .TRUE.
            RETURN
         ELSEIF (X(I).GT.WK(J-1)) THEN
            WK(J) = X(I)
            J = J + 1
         ENDIF
      ENDDO
      R = J - 1
c
c check that there are sufficient distinct data abscissae for
c the prescribed number of knots.
c
      IF (R.LT.NCAP3) GOTO 991
c
c check the first  s  and the last  s  Schoenberg-Whitney
c conditions ( s = min(ncap - 1, 4) ).
c
      DO J=1,4
         IF (J.GE.NCAP) RETURN
         I = NCAP3 - J + 1
         L = R - J + 1
         IF (WK(J).GE.K(J+4) .OR. K(I).GE.WK(L)) GOTO 991
      ENDDO
c
c check all the remaining schoenberg-whitney conditions.
c
      IF (NCAP.GT.5) THEN
         R = R - 4
         I = 3
         DO J= 5, NCAPM1
            K0 = K(J+4)
            K4 = K(J)
            DO WHILE (WK(I).LE.K4)
               I = I + 1
            ENDDO
            IF (I.GT.R .OR. WK(I).GE.K0) GOTO 991
         ENDDO
      ENDIF
      RETURN
991   WRITE(*,*)'SPLINE_CHECK',
     $'Too many knots'
      WRITE(*,*) 'abscissae: ',(WK(I),I=1,R)
      WRITE(*,*) 'knots: ',(K(I),I=1,NCAP7)
      ERROR = .TRUE.
      RETURN
      END



      SUBROUTINE MTH_DPBTRF (NAME, UPLO, N, KD, AB, LDAB, ERROR)
      CHARACTER*(*)      UPLO, NAME
      INTEGER            INFO, KD, LDAB, N
      REAL*8             AB( LDAB, * )
      LOGICAL ERROR
*
*  Purpose
*  =======
*
*  DPBTRF computes the Cholesky factorization of a real symmetric
*  positive definite band matrix A.
*
*  The factorization has the form
*     A = U**T * U,  if UPLO = 'U', or
*     A = L  * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, if INFO = 0, the triangular factor U or L from the
*          Cholesky factorization A = U**T*U or A = L*L**T of the band
*          matrix A, in the same storage format as A.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite, and the factorization could not be
*                completed.
*
*  Further Details
*  ===============
*
*  The band storage scheme is illustrated by the following example, when
*  N = 6, KD = 2, and UPLO = 'U':
*
*  On entry:                       On exit:
*
*      *    *   a13  a24  a35  a46      *    *   u13  u24  u35  u46
*      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
*     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
*
*  Similarly, if UPLO = 'L' the format of A is as follows:
*
*  On entry:                       On exit:
*
*     a11  a22  a33  a44  a55  a66     l11  l22  l33  l44  l55  l66
*     a21  a32  a43  a54  a65   *      l21  l32  l43  l54  l65   *
*     a31  a42  a53  a64   *    *      l31  l42  l53  l64   *    *
*
*  Array elements marked * are not used by the routine.
*
* Call LAPACK routine
      CALL DPBTRF  (UPLO, N, KD, AB, LDAB, INFO )
      if(INFO .lt. 0) then
         write(*,*) 'DPBTRF NOT SUCCESSFUL; INFO', INFO
      endif
      if(INFO .gt. 0) then
         write(*,*) 'DPBTRF ;problem leading minor ', INFO
      endif


C      CALL MTH_FAIL(NAME,'MTH_DPBTRF',INFO,ERROR)
      END

      SUBROUTINE MTH_DPBTRS (NAME,
     $UPLO, N, KD, NRHS, AB, LDAB, B, LDB, ERROR)
      CHARACTER*(*)      UPLO, NAME
      INTEGER            INFO, KD, LDAB, LDB, N, NRHS
      REAL*8             AB( LDAB, * ), B( LDB, * )
      LOGICAL ERROR
*
*  Purpose
*  =======
*
*  DPBTRS solves a system of linear equations A*X = B with a symmetric
*  positive definite band matrix A using the Cholesky factorization
*  A = U**T*U or A = L*L**T computed by DPBTRF.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangular factor stored in AB;
*          = 'L':  Lower triangular factor stored in AB.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The triangular factor U or L from the Cholesky factorization
*          A = U**T*U or A = L*L**T of the band matrix A, stored in the
*          first KD+1 rows of the array.  The j-th column of U or L is
*          stored in the j-th column of the array AB as follows:
*          if UPLO ='U', AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO ='L', AB(1+i-j,j)    = L(i,j) for j<=i<=min(n,j+kd).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
* Call LAPACK routine
      CALL DPBTRS (UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
C      CALL MTH_FAIL(NAME,'MTH_DPBTRS',INFO,ERROR)
      if (INFO .lt. 0) then
         write(*,*) 'DPBTRS NOT SUCCESSFUL; INFO', INFO
      endif

      END

      SUBROUTINE GETBSPL(NCAP7, K, C, X, S, IFAIL)
*
* Evaluates a cubic spline from its B-spline representation.
*
* Uses DE BOOR*s method of convex combinations.
*
      INTEGER NCAP7, IFAIL, J, J1, L
      REAL*8 K(NCAP7), C(NCAP7), X, S, C1, C2, C3, E2, E3, E4,
     $E5,K1, K2, K3, K4, K5, K6
*
* Check enough data
      IF (NCAP7.LT.8) THEN
         IFAIL = 2
         RETURN
      ENDIF
* Check in boundary
      IF (X.LT.K(4) .OR. X.GT.K(NCAP7-3)) THEN
         IFAIL = 1
         S = 0.0D0
         RETURN
      ENDIF
*
* Determine  J  such that  K(J + 3) .LE. X .LE. K(J + 4).
      J1 = 0
      J = NCAP7 - 7
      DO WHILE (J-J1.GT.1)
         L = (J1+J)/2
         IF (X.GE.K(L+4)) THEN
            J1 = L
         ELSE
            J = L
         ENDIF
      ENDDO
*
* Use the method of convex combinations to compute  S(X).
      K1 = K(J+1)
      K2 = K(J+2)
      K3 = K(J+3)
      K4 = K(J+4)
      K5 = K(J+5)
      K6 = K(J+6)
      E2 = X - K2
      E3 = X - K3
      E4 = K4 - X
      E5 = K5 - X
      C2 = C(J+1)
      C3 = C(J+2)
      C1 = ((X-K1)*C2+E4*C(J))/(K4-K1)
      C2 = (E2*C3+E5*C2)/(K5-K2)
      C3 = (E3*C(J+3)+(K6-X)*C3)/(K6-K3)
      C1 = (E2*C2+E4*C1)/(K4-K2)
      C2 = (E3*C3+E5*C2)/(K5-K3)
      S = (E3*C2+E4*C1)/(K4-K3)
      IFAIL = 0
      END

      SUBROUTINE AMPLIANT(ANT1,ANT2,NANT,NBAS,BD,WBD,AD,WAD,ERROR, 
     $WK2,WK3)
      INTEGER ERROR
      INTEGER NANT, NBAS
      INTEGER ANT1(NBAS), ANT2(NBAS)
      REAL*8 BD(NBAS), WBD(NBAS), AD(NANT), WAD(NANT), WB, YI, WW
      REAL*8 WK2(NANT, NANT), WK3(NBAS)
      INTEGER IB, IA, JA, NANTM1

      NANTM1=NANT-1
*
      DO IA=1, NANT
         DO JA=1, NANT
            WK2(IA,JA) = 0
         ENDDO
         WK3(IA) = 0
         AD(IA) = 0
      ENDDO
      DO IB = 1, NBAS
         WB = WBD(IB)
         IF (WB.GT.0) THEN
            IA = ANT1(IB)
            JA = ANT2(IB)
            YI = BD(IB) - (AD(JA)+AD(IA))
            WK3(IA) = WK3(IA) + WB*YI
            WK3(JA) = WK3(JA) + WB*YI
            WK2(IA,IA) = WK2(IA,IA) + WB
            WK2(JA,JA) = WK2(JA,JA) + WB
            WK2(IA,JA) = WK2(IA,JA) + WB
            WK2(JA,IA) = WK2(JA,IA) + WB
         ENDIF
      ENDDO
      CALL MTH_DPOTRF ('AMPLI_ANT','U',NANT,WK2,NANT,ERROR)
      IF (ERROR.GT.0 .OR. ERROR.LT.0)THEN
         WRITE(*,*)'AMPLIANT: DPOTRF RETURNS ', ERROR
      ENDIF
      CALL MTH_DPOTRS ('AMPLI_ANT',
     $'U',NANT,1,WK2,NANT,WK3,NANT,ERROR)
      IF (ERROR.GT.0 .OR. ERROR.LT.0) THEN
         WRITE(*,*)'AMPLIANT: DPOTRF RETURNS ', ERROR
      ENDIF
* Add the result to ad:
      DO IA=1,NANT
         WW = WK3(IA)
         AD(IA) = AD(IA) + WW
      ENDDO
      RETURN
      END


      SUBROUTINE PHASEANT(ANT1,ANT2,NANT,NBAS,BD,WBD,AD,WAD,ERROR, WK2,
     $ WK3)
      INTEGER ERROR
      INTEGER NANT, NBAS
      INTEGER ANT1(NBAS), ANT2(NBAS)
      REAL*8 BD(NBAS), WBD(NBAS), AD(NANT), WAD(NANT), WB, YI, WW
      REAL*8 WK2(NANT, NANT), WK3(NBAS), NORM
      INTEGER IB, IA, IR, NANTM1, JA, I, ZANT
*
      NORM = 1E10
      NANTM1 = NANT - 1
      IR = 1
      DO IA=1, NANT
         AD(IA) = 0
      ENDDO
      DO WHILE (NORM.GT.1E-10)
         DO IA=1, NANT
            DO JA=1, NANT
               WK2(IA,JA) = 0
            ENDDO
            WK3(IA) = 0
         ENDDO
         DO IB = 1, NBAS
            WB = WBD(IB)
            IF (WB.GT.0) THEN
               IA = ANT1(IB)
               JA = ANT2(IB)
               YI = SIN(BD(IB) - (AD(JA)-AD(IA)))
               IA = ZANT(IA,IR)
               JA = ZANT(JA,IR)
               IF (IA.NE.0) THEN
                  WK2(IA,IA) = WK2(IA,IA) + WB
                  WK3(IA) = WK3(IA) - WB*YI
               ENDIF
               IF (JA.NE.0) THEN
                  WK2(JA,JA) = WK2(JA,JA) + WB
                  WK3(JA) = WK3(JA) + WB*YI
               ENDIF
               IF (IA.NE.0 .AND. JA.NE.0) THEN
                  WK2(IA,JA) = WK2(IA,JA) - WB
                  WK2(JA,IA) = WK2(JA,IA) - WB
               ENDIF
            ENDIF
         ENDDO
         CALL MTH_DPOTRF ('PHASE_ANT','U',NANTM1,WK2,NANT,ERROR)
         IF (ERROR .NE. 0) THEN
            WRITE(*,*) 'PHASEANT ERROR IN DPOTRF ',ERROR 
         ENDIF   
         CALL MTH_DPOTRS ('PHASE_ANT',
     $   'U',NANTM1,1,WK2,NANT,WK3,NANTM1,ERROR)
         IF (ERROR .NE. 0) THEN
            WRITE(*,*) 'PHASEANT ERROR IN DPOTRS ',ERROR
         ENDIF
* Add the result to ad:
         NORM = 0
         DO IA=1,NANT
            I = ZANT(IA,IR)
            IF (I.NE.0) THEN
               WW = WK3(I)
               AD(IA) = AD(IA) + WW
               NORM = NORM + WW**2
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE ANTGAIN (Z,W, IANT, JANT, ZANT,WANT, NANT, NBAS,
     $                    REF_ANT)
C------------------------------------------------------------------------
C CLIC
C     Derive antenna "gains" from baseline visibilities
C Arguments:
C     Z(NBAS)    COMPLEX   Visibility
C     W(NBAS)    REAL      Weight
C     IANT(NBAS) INTEGER   antenna1 for baseline
C     JANT(NBAS) INTEGER   antenna2 for baseline
C     ZANT(NANT) COMPLEX   Complex antenna gain
C     WANT(NANT) REAL      Weight
C     NANT       INTEGER   number of antennas
C     NBAS       INTEGER   number of baselines
C     REF_ANT    INTEGER   reference antenna

C------------------------------------------------------------------------
* Dummy variables:
      REAL*4 W(2016), WANT(64)
      COMPLEX Z(2016), ZANT(64), CMPL2
      INTEGER REF_ANT, IANT(2016), JANT(2016)
* Local variables:
      REAL*4 PHA(64), AMP(64), AA, FAZ, WA, ADD, AJI, AKI, AJK,
     $PHA0(64), C(2016)
      INTEGER IB, IA, J_I, K_I, J_K, JA, KA, BASE, IREF, ITRY, I
      LOGICAL RETRO, REFOK
      PARAMETER (RETRO=.FALSE.)
*------------------------------------------------------------------------
* Code:
*
* Solve for phases, using retroprojection algorithm:
*
      REFOK = .FALSE.
      IREF = REF_ANT
      ITRY = 1
      DO WHILE (.NOT.REFOK .AND. ITRY.LE.NANT)
         DO IA=1, NANT
            IF (IA.LT.IREF) THEN
               IB = BASE(IA,IREF)
               REFOK = REFOK .OR. W(IB).GT.0
            ELSEIF (IA.GT.IREF) THEN
               IB = BASE(IREF,IA)
               REFOK = REFOK .OR. W(IB).GT.0
            ENDIF
         ENDDO
         IF (.NOT.REFOK) THEN
            ITRY = ITRY+1
            IREF = MOD(IREF,NANT)+1
         ENDIF
      ENDDO
      IF (.NOT.REFOK) THEN
         DO I=1, NANT
            PHA(I) = 0
         ENDDO
      ELSE
*
*
         PHA0(IREF) = 0.
         DO IA=1, NANT
            IF (IA.LT.IREF) THEN
               IB = BASE(IA,IREF)
               IF (W(IB).GT.0) PHA0(IA) = -FAZ(Z(IB))
            ELSEIF (IA.GT.IREF) THEN
               IB = BASE(IREF,IA)
               IF (W(IB).GT.0) PHA0(IA) = FAZ(Z(IB))
            ENDIF
         ENDDO
         DO IA = 1, NANT
            ADD = 0
            DO JA = 1, NANT
               IF (JA.LT.IA) THEN
                  IB = BASE(JA,IA)
                  IF (W(IB).GT.0) THEN
                     ADD = ADD + FAZ(Z(IB)) - PHA0(IA) + PHA0(JA)
                  ENDIF
               ELSEIF (JA.GT.IA) THEN
                  IB = BASE(IA,JA)
                  IF (W(IB).GT.0) THEN
                     ADD = ADD - FAZ(Z(IB)) - PHA0(IA) + PHA0(JA)
                  ENDIF
               ENDIF
            ENDDO
            ADD = MOD(ADD+31D0*PI,2D0*PI)-PI
            PHA(IA) = PHA0(IA)+ADD/NANT
         ENDDO
      ENDIF
*
* solve for amplitudes, using retroprojection algorithm:
c (to be checked again)
* note:
* to compute this way the weights must be provided by antenna,
* not by baseline. (RL 2002-02-27)
      IF (RETRO) THEN
         DO IA = 1, NANT
            WANT(IA) = 0
            AMP(IA) = 0
            DO IB=1, NBAS
               IF (W(IB).GT.0) THEN
                  IF (IANT(IB).EQ.IA .OR. JANT(IB).EQ.IA) THEN
                     AMP(IA) = AMP(IA)+LOG(ABS(Z(IB)))*WA
                     WANT(IA) = WANT(IA)+WA
                  ELSE
                     AMP(IA) = AMP(IA)-LOG(ABS(Z(IB)))*WA/(NANT-2)
                     WANT(IA) = WANT(IA)-WA/(NANT-2)
                  ENDIF
               ENDIF
            ENDDO
            IF (WANT(IA).GT.0) THEN
               AMP(IA) = AMP(IA)/WANT(IA)
            ELSE
               AMP(IA) = 0.
            ENDIF
            IF (R_NANT.GT.1) THEN
               AMP(IA) = AMP(IA)/(NANT-1)
               WANT(IA) = WANT(IA)/(NANT-1)
            ENDIF
            ZANT(IA) = EXP(CMPLX(AMP(IA),PHA(IA)))
c            if (want(ia).gt.0) WANT(IA) = EXP(WANT(IA))
         ENDDO
      ELSE
*
* Solve for amplitudes
         DO IA = 1, NANT
            WANT(IA) = 0
            AMP(IA) = 0
            IF (NANT.GT.2) THEN
               DO JA = 1, NANT
                  IF (JA.NE.IA .AND. JA.LT.NANT) THEN
                     DO  KA = JA+1, NANT
                        IF (KA.NE.IA) THEN
                           J_I = BASE(MIN(JA,IA),MAX(JA,IA))
                           K_I = BASE(MIN(KA,IA),MAX(KA,IA))
                           J_K = BASE(JA,KA)
                           IF (Z(J_K).NE.0 .AND. Z(J_I).NE.0 .AND.
     $                     Z(K_I).NE.0 .AND. W(J_K).NE.0 .AND.
     $                     W(J_I).NE.0 .AND. W(K_I).NE.0) THEN
                              AJI = ABS(Z(J_I))
                              AKI = ABS(Z(K_I))
                              AJK = ABS(Z(J_K))
                              IF (AJI.LT.1E15 .AND. AKI.LT.1E15
     $                        .AND. AJK.LT.1E15) THEN
                                 AA = AJI*AKI/AJK
                                 WA = 1./W(J_I)/ABS(Z(J_I))**2
     $                           +1./W(K_I)/ABS(Z(K_I))**2
     $                           +1./W(J_K)/ABS(Z(J_K))**2
                                 WA = 1/AA**2/WA
                                 AMP(IA) = AMP(IA) + AA*WA
                                 WANT(IA) = WANT(IA) + WA
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO
               IF (WANT(IA).NE.0) AMP(IA) = AMP(IA) / WANT(IA)
            ENDIF
*
* if previous algorithm did not work, take first valid baseline
* containing IA. This will work if there is only one operational baseline ...
            IF (WANT(IA).LE.0) THEN
               DO IB=1, NBAS
                  IF (W(IB).GT.0) THEN
                     AMP(IA) = ABS(Z(IB))
                     WANT(IA) = W(IB)
                  ENDIF
               ENDDO
            ENDIF
            ZANT(IA) = AMP(IA) * EXP(CMPLX(0.,PHA(IA)))
c
c            ZANT(IA) =CMPL2(AMP(IA), PHA(IA))
c            if (amp(ia).GT.BLANK4-D_BLANK4) want(ia) = 0
         ENDDO
      ENDIF
      RETURN
      END

      FUNCTION BASE(I,J)
C----------------------------------------------------------------------
C Returns the number of baseline I,J (not oriented)
C----------------------------------------------------------------------
* Global variables:
* Dummy variables:
      INTEGER BASE,I,J
* Local variables:
*------------------------------------------------------------------------
* Code:
      IF (I.LT.J) THEN
         BASE = (J-1)*(J-2)/2 + I
      ELSE
         BASE = (I-1)*(I-2)/2 + J
      ENDIF
      END


      FUNCTION FAZ(Z)
C------------------------------------------------------------------------
C 	Compute the phase of a complex number Z
C------------------------------------------------------------------------

* Dummy variables:
      COMPLEX Z
      REAL FAZ
      COMPLEX BLANKC
      PARAMETER (BLANKC=(1.23456E34,1.23456E34))
      REAL*4 BLANK4
      PARAMETER (BLANK4=1.23456E34)
*------------------------------------------------------------------------
* Code:
      IF (Z.NE.0 .AND. Z.NE.BLANKC) THEN
         FAZ = ATAN2(AIMAG(Z),REAL(Z))
      ELSE
         FAZ = BLANK4
      ENDIF
      RETURN
      END
