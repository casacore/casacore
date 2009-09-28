*=======================================================================
C     Correspondence concerning AIPS++ should be addressed as follows:
C            Internet email: aips2-request@nrao.edu.
C            Postal address: AIPS++ Project Office
C                            National Radio Astronomy Observatory
C                            520 Edgemont Road
C                            Charlottesville, VA 22903-2475 USA
C
C     $Id$
C
C downloaded from http://www.netlib.org/fftpack/ on Nov 1997
*-----------------------------------------------------------------------
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C                       FFTPACK
C 
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C                   version 4  april 1985
C 
C      a package of fortran subprograms for the fast fourier
C       transform of periodic and other symmetric sequences
C 
C                          by
C 
C                   paul n swarztrauber
C 
C   national center for atmospheric research  boulder,colorado 80307
C 
C    which is sponsored by the national science foundation
C 
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C 
C this package consists of programs which perform fast fourier
C transforms for both complex and real periodic sequences and
C certain other symmetric sequences that are listed below.
C 
C 1.   rffti     initialize  rfftf and rfftb
C 2.   rfftf     forward transform of a real periodic sequence
C 3.   rfftb     backward transform of a real coefficient array
C 
C 4.   ezffti    initialize ezfftf and ezfftb
C 5.   ezfftf    a simplified real periodic forward transform
C 6.   ezfftb    a simplified real periodic backward transform
C 
C 7.   sinti     initialize sint
C 8.   sint      sine transform of a real odd sequence
C 
C 9.   costi     initialize cost
C 10.  cost      cosine transform of a real even sequence
C 
C 11.  sinqi     initialize sinqf and sinqb
C 12.  sinqf     forward sine transform with odd wave numbers
C 13.  sinqb     unnormalized inverse of sinqf
C 
C 14.  cosqi     initialize cosqf and cosqb
C 15.  cosqf     forward cosine transform with odd wave numbers
C 16.  cosqb     unnormalized inverse of cosqf
C 
C 17.  cffti     initialize cfftf and cfftb
C 18.  cfftf     forward transform of a complex periodic sequence
C 19.  cfftb     unnormalized inverse of cfftf
C
C ******************************************************************
C 
C subroutine rffti(n,wsave)
C 
C ******************************************************************
C 
C subroutine rffti initializes the array wsave which is used in
C both rfftf and rfftb. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed.
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 2*n+15.
C         the same work array can be used for both rfftf and rfftb
C         as long as n remains unchanged. different wsave arrays
C         are required for different values of n. the contents of
C         wsave must not be changed between calls of rfftf or rfftb.
C
      SUBROUTINE RFFTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      IF (N .EQ. 1) RETURN
      CALL RFFTI1 (N,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTI1 (N,WA,IFAC)
      DIMENSION       WA(*)      ,IFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
      NL = N
      NF = 0
      J = 0
 101  J = J+1
      IF (J-4) 102,102,103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
 105  NF = NF+1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         IFAC(IB+2) = IFAC(IB+1)
 106  CONTINUE
      IFAC(3) = 2
 107  IF (NL .NE. 1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      IS = 0
      NFM1 = NF-1
      L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 110 K1=1,NFM1
         IP = IFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IPM = IP-1
         DO 109 J=1,IPM
            LD = LD+L1
            I = IS
            ARGLD = FLOAT(LD)*ARGH
            FI = 0.
            DO 108 II=3,IDO,2
               I = I+2
               FI = FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
 108        CONTINUE
            IS = IS+IDO
 109     CONTINUE
         L1 = L2
 110  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine rfftf(n,r,wsave)
C 
C ******************************************************************
C 
C subroutine rfftf computes the fourier coefficients of a real
C perodic sequence (fourier analysis). the transform is defined
C below at output parameter r.
C 
C input parameters
C 
C n       the length of the array r to be transformed.  the method
C         is most efficient when n is a product of small primes.
C         n may change so long as different work arrays are provided
C 
C r       a real array of length n which contains the sequence
C         to be transformed
C 
C wsave   a work array which must be dimensioned at least 2*n+15.
C         in the program that calls rfftf. the wsave array must be
C         initialized by calling subroutine rffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by rfftf and rfftb.
C 
C 
C output parameters
C 
C r       r(1) = the sum from i=1 to i=n of r(i)
C 
C         if n is even set l =n/2   , if n is odd set l = (n+1)/2
C 
C           then for k = 2,...,l
C 
C              r(2*k-2) = the sum from i = 1 to i = n of
C 
C                   r(i)*cos((k-1)*(i-1)*2*pi/n)
C 
C              r(2*k-1) = the sum from i = 1 to i = n of
C 
C                  -r(i)*sin((k-1)*(i-1)*2*pi/n)
C 
C         if n is even
C 
C              r(n) = the sum from i = 1 to i = n of
C 
C                   (-1)**(i-1)*r(i)
C 
C  *****  note
C              this transform is unnormalized since a call of rfftf
C              followed by a call of rfftb will multiply the input
C              sequence by n.
C 
C wsave   contains results which must not be destroyed between
C         calls of rfftf or rfftb.
C
      SUBROUTINE RFFTF (N,R,WSAVE)
      DIMENSION       R(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      CALL RFFTF1 (N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTF1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,IFAC(*)
      NF = IFAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1=1,NF
         KH = NF-K1
         IP = IFAC(KH+3)
         L1 = L2/IP
         IDO = N/L2
         IDL1 = IDO*L1
         IW = IW-(IP-1)*IDO
         NA = 1-NA
         IF (IP .NE. 4) GO TO 102
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
         CALL RADF4 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
 101     CALL RADF4 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
 102     IF (IP .NE. 2) GO TO 104
         IF (NA .NE. 0) GO TO 103
         CALL RADF2 (IDO,L1,C,CH,WA(IW))
         GO TO 110
 103     CALL RADF2 (IDO,L1,CH,C,WA(IW))
         GO TO 110
 104     IF (IP .NE. 3) GO TO 106
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 105
         CALL RADF3 (IDO,L1,C,CH,WA(IW),WA(IX2))
         GO TO 110
 105     CALL RADF3 (IDO,L1,CH,C,WA(IW),WA(IX2))
         GO TO 110
 106     IF (IP .NE. 5) GO TO 108
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 107
         CALL RADF5 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 110
 107     CALL RADF5 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 110
 108     IF (IDO .EQ. 1) NA = 1-NA
         IF (NA .NE. 0) GO TO 109
         CALL RADFG (IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         NA = 1
         GO TO 110
 109     CALL RADFG (IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
         NA = 0
 110     L2 = L1
 111  CONTINUE
      IF (NA .EQ. 1) RETURN
      DO 112 I=1,N
         C(I) = CH(I)
 112  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CH(IDO,2,L1)           ,CC(IDO,L1,2)           ,
     1     WA1(*)
      DO 101 K=1,L1
         CH(1,1,K) = CC(1,K,1)+CC(1,K,2)
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,2)
 101  CONTINUE
      IF (IDO-2) 107,105,102
 102  IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CH(I,1,K) = CC(I,K,1)+TI2
            CH(IC,2,K) = TI2-CC(I,K,1)
            CH(I-1,1,K) = CC(I-1,K,1)+TR2
            CH(IC-1,2,K) = CC(I-1,K,1)-TR2
 103     CONTINUE
 104  CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
 105  DO 106 K=1,L1
         CH(1,2,K) = -CC(IDO,K,2)
         CH(IDO,1,K) = CC(IDO,K,1)
 106  CONTINUE
 107  RETURN
      END
C
      SUBROUTINE RADF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CH(IDO,3,L1)           ,CC(IDO,L1,3)           ,
     1     WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         CR2 = CC(1,K,2)+CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2
         CH(1,3,K) = TAUI*(CC(1,K,3)-CC(1,K,2))
         CH(IDO,2,K) = CC(1,K,1)+TAUR*CR2
 101  CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR2 = DR2+DR3
            CI2 = DI2+DI3
            CH(I-1,1,K) = CC(I-1,K,1)+CR2
            CH(I,1,K) = CC(I,K,1)+CI2
            TR2 = CC(I-1,K,1)+TAUR*CR2
            TI2 = CC(I,K,1)+TAUR*CI2
            TR3 = TAUI*(DI2-DI3)
            TI3 = TAUI*(DR3-DR2)
            CH(I-1,3,K) = TR2+TR3
            CH(IC-1,2,K) = TR2-TR3
            CH(I,3,K) = TI2+TI3
            CH(IC,2,K) = TI3-TI2
 102     CONTINUE
 103  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,L1,4)           ,CH(IDO,4,L1)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)
      DATA HSQT2 /.7071067811865475/
      DO 101 K=1,L1
         TR1 = CC(1,K,2)+CC(1,K,4)
         TR2 = CC(1,K,1)+CC(1,K,3)
         CH(1,1,K) = TR1+TR2
         CH(IDO,4,K) = TR2-TR1
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,3)
         CH(1,3,K) = CC(1,K,4)-CC(1,K,2)
 101  CONTINUE
      IF (IDO-2) 107,105,102
 102  IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            CI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            CR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            CI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            CR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            CI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            TR1 = CR2+CR4
            TR4 = CR4-CR2
            TI1 = CI2+CI4
            TI4 = CI2-CI4
            TI2 = CC(I,K,1)+CI3
            TI3 = CC(I,K,1)-CI3
            TR2 = CC(I-1,K,1)+CR3
            TR3 = CC(I-1,K,1)-CR3
            CH(I-1,1,K) = TR1+TR2
            CH(IC-1,4,K) = TR2-TR1
            CH(I,1,K) = TI1+TI2
            CH(IC,4,K) = TI1-TI2
            CH(I-1,3,K) = TI4+TR3
            CH(IC-1,2,K) = TR3-TI4
            CH(I,3,K) = TR4+TI3
            CH(IC,2,K) = TR4-TI3
 103     CONTINUE
 104  CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
 105  CONTINUE
      DO 106 K=1,L1
         TI1 = -HSQT2*(CC(IDO,K,2)+CC(IDO,K,4))
         TR1 = HSQT2*(CC(IDO,K,2)-CC(IDO,K,4))
         CH(IDO,1,K) = TR1+CC(IDO,K,1)
         CH(IDO,3,K) = CC(IDO,K,1)-TR1
         CH(1,2,K) = TI1-CC(IDO,K,3)
         CH(1,4,K) = TI1+CC(IDO,K,3)
 106  CONTINUE
 107  RETURN
      END
C
      SUBROUTINE RADF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,L1,5)           ,CH(IDO,5,L1)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1     -.809016994374947,.587785252292473/
      DO 101 K=1,L1
         CR2 = CC(1,K,5)+CC(1,K,2)
         CI5 = CC(1,K,5)-CC(1,K,2)
         CR3 = CC(1,K,4)+CC(1,K,3)
         CI4 = CC(1,K,4)-CC(1,K,3)
         CH(1,1,K) = CC(1,K,1)+CR2+CR3
         CH(IDO,2,K) = CC(1,K,1)+TR11*CR2+TR12*CR3
         CH(1,3,K) = TI11*CI5+TI12*CI4
         CH(IDO,4,K) = CC(1,K,1)+TR12*CR2+TR11*CR3
         CH(1,5,K) = TI12*CI5-TI11*CI4
 101  CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)
            DR4 = WA3(I-2)*CC(I-1,K,4)+WA3(I-1)*CC(I,K,4)
            DI4 = WA3(I-2)*CC(I,K,4)-WA3(I-1)*CC(I-1,K,4)
            DR5 = WA4(I-2)*CC(I-1,K,5)+WA4(I-1)*CC(I,K,5)
            DI5 = WA4(I-2)*CC(I,K,5)-WA4(I-1)*CC(I-1,K,5)
            CR2 = DR2+DR5
            CI5 = DR5-DR2
            CR5 = DI2-DI5
            CI2 = DI2+DI5
            CR3 = DR3+DR4
            CI4 = DR4-DR3
            CR4 = DI3-DI4
            CI3 = DI3+DI4
            CH(I-1,1,K) = CC(I-1,K,1)+CR2+CR3
            CH(I,1,K) = CC(I,K,1)+CI2+CI3
            TR2 = CC(I-1,K,1)+TR11*CR2+TR12*CR3
            TI2 = CC(I,K,1)+TR11*CI2+TR12*CI3
            TR3 = CC(I-1,K,1)+TR12*CR2+TR11*CR3
            TI3 = CC(I,K,1)+TR12*CI2+TR11*CI3
            TR5 = TI11*CR5+TI12*CR4
            TI5 = TI11*CI5+TI12*CI4
            TR4 = TI12*CR5-TI11*CR4
            TI4 = TI12*CI5-TI11*CI4
            CH(I-1,3,K) = TR2+TR5
            CH(IC-1,2,K) = TR2-TR5
            CH(I,3,K) = TI2+TI5
            CH(IC,2,K) = TI5-TI2
            CH(I-1,5,K) = TR3+TR4
            CH(IC-1,4,K) = TR3-TR4
            CH(I,5,K) = TI3+TI4
            CH(IC,4,K) = TI4-TI3
 102     CONTINUE
 103  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADFG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1     C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2     CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IF (IDO .EQ. 1) GO TO 119
      DO 101 IK=1,IDL1
         CH2(IK,1) = C2(IK,1)
 101  CONTINUE
      DO 103 J=2,IP
         DO 102 K=1,L1
            CH(1,K,J) = C1(1,K,J)
 102     CONTINUE
 103  CONTINUE
      IF (NBD .GT. L1) GO TO 107
      IS = -IDO
      DO 106 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 105 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 104 K=1,L1
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
 104        CONTINUE
 105     CONTINUE
 106  CONTINUE
      GO TO 111
 107  IS = -IDO
      DO 110 J=2,IP
         IS = IS+IDO
         DO 109 K=1,L1
            IDIJ = IS
            DO 108 I=3,IDO,2
               IDIJ = IDIJ+2
               CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J)+WA(IDIJ)*C1(I,K,J)
               CH(I,K,J) = WA(IDIJ-1)*C1(I,K,J)-WA(IDIJ)*C1(I-1,K,J)
 108        CONTINUE
 109     CONTINUE
 110  CONTINUE
 111  IF (NBD .LT. L1) GO TO 115
      DO 114 J=2,IPPH
         JC = IPP2-J
         DO 113 K=1,L1
            DO 112 I=3,IDO,2
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
 112        CONTINUE
 113     CONTINUE
 114  CONTINUE
      GO TO 121
 115  DO 118 J=2,IPPH
         JC = IPP2-J
         DO 117 I=3,IDO,2
            DO 116 K=1,L1
               C1(I-1,K,J) = CH(I-1,K,J)+CH(I-1,K,JC)
               C1(I-1,K,JC) = CH(I,K,J)-CH(I,K,JC)
               C1(I,K,J) = CH(I,K,J)+CH(I,K,JC)
               C1(I,K,JC) = CH(I-1,K,JC)-CH(I-1,K,J)
 116        CONTINUE
 117     CONTINUE
 118  CONTINUE
      GO TO 121
 119  DO 120 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
 120  CONTINUE
 121  DO 123 J=2,IPPH
         JC = IPP2-J
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)+CH(1,K,JC)
            C1(1,K,JC) = CH(1,K,JC)-CH(1,K,J)
 122     CONTINUE
 123  CONTINUE
C
      AR1 = 1.
      AI1 = 0.
      DO 127 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 124 IK=1,IDL1
            CH2(IK,L) = C2(IK,1)+AR1*C2(IK,2)
            CH2(IK,LC) = AI1*C2(IK,IP)
 124     CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 126 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 125 IK=1,IDL1
               CH2(IK,L) = CH2(IK,L)+AR2*C2(IK,J)
               CH2(IK,LC) = CH2(IK,LC)+AI2*C2(IK,JC)
 125        CONTINUE
 126     CONTINUE
 127  CONTINUE
      DO 129 J=2,IPPH
         DO 128 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+C2(IK,J)
 128     CONTINUE
 129  CONTINUE
C
      IF (IDO .LT. L1) GO TO 132
      DO 131 K=1,L1
         DO 130 I=1,IDO
            CC(I,1,K) = CH(I,K,1)
 130     CONTINUE
 131  CONTINUE
      GO TO 135
 132  DO 134 I=1,IDO
         DO 133 K=1,L1
            CC(I,1,K) = CH(I,K,1)
 133     CONTINUE
 134  CONTINUE
 135  DO 137 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 136 K=1,L1
            CC(IDO,J2-2,K) = CH(1,K,J)
            CC(1,J2-1,K) = CH(1,K,JC)
 136     CONTINUE
 137  CONTINUE
      IF (IDO .EQ. 1) RETURN
      IF (NBD .LT. L1) GO TO 141
      DO 140 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 139 K=1,L1
            DO 138 I=3,IDO,2
               IC = IDP2-I
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
 138        CONTINUE
 139     CONTINUE
 140  CONTINUE
      RETURN
 141  DO 144 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 143 I=3,IDO,2
            IC = IDP2-I
            DO 142 K=1,L1
               CC(I-1,J2-1,K) = CH(I-1,K,J)+CH(I-1,K,JC)
               CC(IC-1,J2-2,K) = CH(I-1,K,J)-CH(I-1,K,JC)
               CC(I,J2-1,K) = CH(I,K,J)+CH(I,K,JC)
               CC(IC,J2-2,K) = CH(I,K,JC)-CH(I,K,J)
 142        CONTINUE
 143     CONTINUE
 144  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine rfftb(n,r,wsave)
C 
C ******************************************************************
C 
C subroutine rfftb computes the real perodic sequence from its
C fourier coefficients (fourier synthesis). the transform is defined
C below at output parameter r.
C 
C input parameters
C 
C n       the length of the array r to be transformed.  the method
C         is most efficient when n is a product of small primes.
C         n may change so long as different work arrays are provided
C 
C r       a real array of length n which contains the sequence
C         to be transformed
C 
C wsave   a work array which must be dimensioned at least 2*n+15.
C         in the program that calls rfftb. the wsave array must be
C         initialized by calling subroutine rffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by rfftf and rfftb.
C 
C 
C output parameters
C 
C r       for n even and for i = 1,...,n
C 
C              r(i) = r(1)+(-1)**(i-1)*r(n)
C 
C                   plus the sum from k=2 to k=n/2 of
C 
C                    2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
C 
C                   -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
C 
C         for n odd and for i = 1,...,n
C 
C              r(i) = r(1) plus the sum from k=2 to k=(n+1)/2 of
C 
C                   2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
C 
C                  -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
C 
C  *****  note
C              this transform is unnormalized since a call of rfftf
C              followed by a call of rfftb will multiply the input
C              sequence by n.
C 
C wsave   contains results which must not be destroyed between
C         calls of rfftb or rfftf.
C
      SUBROUTINE RFFTB (N,R,WSAVE)
      DIMENSION       R(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      CALL RFFTB1 (N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTB1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,IFAC(*)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDL1 = IDO*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
         CALL RADB4 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
 101     CALL RADB4 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
 102     NA = 1-NA
         GO TO 115
 103     IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL RADB2 (IDO,L1,C,CH,WA(IW))
         GO TO 105
 104     CALL RADB2 (IDO,L1,CH,C,WA(IW))
 105     NA = 1-NA
         GO TO 115
 106     IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 107
         CALL RADB3 (IDO,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
 107     CALL RADB3 (IDO,L1,CH,C,WA(IW),WA(IX2))
 108     NA = 1-NA
         GO TO 115
 109     IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 110
         CALL RADB5 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
 110     CALL RADB5 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
 111     NA = 1-NA
         GO TO 115
 112     IF (NA .NE. 0) GO TO 113
         CALL RADBG (IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
 113     CALL RADBG (IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
 114     IF (IDO .EQ. 1) NA = 1-NA
 115     L1 = L2
         IW = IW+(IP-1)*IDO
 116  CONTINUE
      IF (NA .EQ. 0) RETURN
      DO 117 I=1,N
         C(I) = CH(I)
 117  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1     WA1(*)
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(IDO,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(IDO,2,K)
 101  CONTINUE
      IF (IDO-2) 107,105,102
 102  IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CH(I-1,K,1) = CC(I-1,1,K)+CC(IC-1,2,K)
            TR2 = CC(I-1,1,K)-CC(IC-1,2,K)
            CH(I,K,1) = CC(I,1,K)-CC(IC,2,K)
            TI2 = CC(I,1,K)+CC(IC,2,K)
            CH(I-1,K,2) = WA1(I-2)*TR2-WA1(I-1)*TI2
            CH(I,K,2) = WA1(I-2)*TI2+WA1(I-1)*TR2
 103     CONTINUE
 104  CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
 105  DO 106 K=1,L1
         CH(IDO,K,1) = CC(IDO,1,K)+CC(IDO,1,K)
         CH(IDO,K,2) = -(CC(1,2,K)+CC(1,2,K))
 106  CONTINUE
 107  RETURN
      END
C
      SUBROUTINE RADB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1     WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      DO 101 K=1,L1
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         CI3 = TAUI*(CC(1,3,K)+CC(1,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
 101  CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,3,K)-CC(IC,2,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,3,K)-CC(IC-1,2,K))
            CI3 = TAUI*(CC(I,3,K)+CC(IC,2,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
 102     CONTINUE
 103  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)
      DATA SQRT2 /1.414213562373095/
      DO 101 K=1,L1
         TR1 = CC(1,1,K)-CC(IDO,4,K)
         TR2 = CC(1,1,K)+CC(IDO,4,K)
         TR3 = CC(IDO,2,K)+CC(IDO,2,K)
         TR4 = CC(1,3,K)+CC(1,3,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,2) = TR1-TR4
         CH(1,K,3) = TR2-TR3
         CH(1,K,4) = TR1+TR4
 101  CONTINUE
      IF (IDO-2) 107,105,102
 102  IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            TI1 = CC(I,1,K)+CC(IC,4,K)
            TI2 = CC(I,1,K)-CC(IC,4,K)
            TI3 = CC(I,3,K)-CC(IC,2,K)
            TR4 = CC(I,3,K)+CC(IC,2,K)
            TR1 = CC(I-1,1,K)-CC(IC-1,4,K)
            TR2 = CC(I-1,1,K)+CC(IC-1,4,K)
            TI4 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR3 = CC(I-1,3,K)+CC(IC-1,2,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1-TR4
            CR4 = TR1+TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2
            CH(I,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2
            CH(I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3
            CH(I,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3
            CH(I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4
            CH(I,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4
 103     CONTINUE
 104  CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
 105  CONTINUE
      DO 106 K=1,L1
         TI1 = CC(1,2,K)+CC(1,4,K)
         TI2 = CC(1,4,K)-CC(1,2,K)
         TR1 = CC(IDO,1,K)-CC(IDO,3,K)
         TR2 = CC(IDO,1,K)+CC(IDO,3,K)
         CH(IDO,K,1) = TR2+TR2
         CH(IDO,K,2) = SQRT2*(TR1-TI1)
         CH(IDO,K,3) = TI2+TI2
         CH(IDO,K,4) = -SQRT2*(TR1+TI1)
 106  CONTINUE
 107  RETURN
      END
C
      SUBROUTINE RADB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1     -.809016994374947,.587785252292473/
      DO 101 K=1,L1
         TI5 = CC(1,3,K)+CC(1,3,K)
         TI4 = CC(1,5,K)+CC(1,5,K)
         TR2 = CC(IDO,2,K)+CC(IDO,2,K)
         TR3 = CC(IDO,4,K)+CC(IDO,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI5 = TI11*TI5+TI12*TI4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(1,K,5) = CR2+CI5
 101  CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            TI5 = CC(I,3,K)+CC(IC,2,K)
            TI2 = CC(I,3,K)-CC(IC,2,K)
            TI4 = CC(I,5,K)+CC(IC,4,K)
            TI3 = CC(I,5,K)-CC(IC,4,K)
            TR5 = CC(I-1,3,K)-CC(IC-1,2,K)
            TR2 = CC(I-1,3,K)+CC(IC-1,2,K)
            TR4 = CC(I-1,5,K)-CC(IC-1,4,K)
            TR3 = CC(I-1,5,K)+CC(IC-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
            CH(I,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
            CH(I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
            CH(I,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
            CH(I-1,K,4) = WA3(I-2)*DR4-WA3(I-1)*DI4
            CH(I,K,4) = WA3(I-2)*DI4+WA3(I-1)*DR4
            CH(I-1,K,5) = WA4(I-2)*DR5-WA4(I-1)*DI5
            CH(I,K,5) = WA4(I-2)*DI5+WA4(I-1)*DR5
 102     CONTINUE
 103  CONTINUE
      RETURN
      END
C
      SUBROUTINE RADBG (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1     C1(IDO,L1,IP)          ,C2(IDL1,IP),
     2     CH2(IDL1,IP)           ,WA(*)
      DATA TPI/6.28318530717959/
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IF (IDO .LT. L1) GO TO 103
      DO 102 K=1,L1
         DO 101 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
 101     CONTINUE
 102  CONTINUE
      GO TO 106
 103  DO 105 I=1,IDO
         DO 104 K=1,L1
            CH(I,K,1) = CC(I,1,K)
 104     CONTINUE
 105  CONTINUE
 106  DO 108 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 107 K=1,L1
            CH(1,K,J) = CC(IDO,J2-2,K)+CC(IDO,J2-2,K)
            CH(1,K,JC) = CC(1,J2-1,K)+CC(1,J2-1,K)
 107     CONTINUE
 108  CONTINUE
      IF (IDO .EQ. 1) GO TO 116
      IF (NBD .LT. L1) GO TO 112
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 110 K=1,L1
            DO 109 I=3,IDO,2
               IC = IDP2-I
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
 109        CONTINUE
 110     CONTINUE
 111  CONTINUE
      GO TO 116
 112  DO 115 J=2,IPPH
         JC = IPP2-J
         DO 114 I=3,IDO,2
            IC = IDP2-I
            DO 113 K=1,L1
               CH(I-1,K,J) = CC(I-1,2*J-1,K)+CC(IC-1,2*J-2,K)
               CH(I-1,K,JC) = CC(I-1,2*J-1,K)-CC(IC-1,2*J-2,K)
               CH(I,K,J) = CC(I,2*J-1,K)-CC(IC,2*J-2,K)
               CH(I,K,JC) = CC(I,2*J-1,K)+CC(IC,2*J-2,K)
 113        CONTINUE
 114     CONTINUE
 115  CONTINUE
 116  AR1 = 1.
      AI1 = 0.
      DO 120 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 117 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+AR1*CH2(IK,2)
            C2(IK,LC) = AI1*CH2(IK,IP)
 117     CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
         DO 119 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2 = DC2*AI2+DS2*AR2
            AR2 = AR2H
            DO 118 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+AR2*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+AI2*CH2(IK,JC)
 118        CONTINUE
 119     CONTINUE
 120  CONTINUE
      DO 122 J=2,IPPH
         DO 121 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
 121     CONTINUE
 122  CONTINUE
      DO 124 J=2,IPPH
         JC = IPP2-J
         DO 123 K=1,L1
            CH(1,K,J) = C1(1,K,J)-C1(1,K,JC)
            CH(1,K,JC) = C1(1,K,J)+C1(1,K,JC)
 123     CONTINUE
 124  CONTINUE
      IF (IDO .EQ. 1) GO TO 132
      IF (NBD .LT. L1) GO TO 128
      DO 127 J=2,IPPH
         JC = IPP2-J
         DO 126 K=1,L1
            DO 125 I=3,IDO,2
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
 125        CONTINUE
 126     CONTINUE
 127  CONTINUE
      GO TO 132
 128  DO 131 J=2,IPPH
         JC = IPP2-J
         DO 130 I=3,IDO,2
            DO 129 K=1,L1
               CH(I-1,K,J) = C1(I-1,K,J)-C1(I,K,JC)
               CH(I-1,K,JC) = C1(I-1,K,J)+C1(I,K,JC)
               CH(I,K,J) = C1(I,K,J)+C1(I-1,K,JC)
               CH(I,K,JC) = C1(I,K,J)-C1(I-1,K,JC)
 129        CONTINUE
 130     CONTINUE
 131  CONTINUE
 132  CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 133 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
 133  CONTINUE
      DO 135 J=2,IP
         DO 134 K=1,L1
            C1(1,K,J) = CH(1,K,J)
 134     CONTINUE
 135  CONTINUE
      IF (NBD .GT. L1) GO TO 139
      IS = -IDO
      DO 138 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 137 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 136 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
 136        CONTINUE
 137     CONTINUE
 138  CONTINUE
      GO TO 143
 139  IS = -IDO
      DO 142 J=2,IP
         IS = IS+IDO
         DO 141 K=1,L1
            IDIJ = IS
            DO 140 I=3,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
 140        CONTINUE
 141     CONTINUE
 142  CONTINUE
 143  RETURN
      END
C
C ******************************************************************
C 
C subroutine ezffti(n,wsave)
C 
C ******************************************************************
C 
C subroutine ezffti initializes the array wsave which is used in
C both ezfftf and ezfftb. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed.
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         the same work array can be used for both ezfftf and ezfftb
C         as long as n remains unchanged. different wsave arrays
C         are required for different values of n.
C
      SUBROUTINE EZFFTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      IF (N .EQ. 1) RETURN
      CALL EZFFT1 (N,WSAVE(2*N+1),WSAVE(3*N+1))
      RETURN
      END
C
      SUBROUTINE EZFFT1 (N,WA,IFAC)
      DIMENSION       WA(*)      ,IFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
     1     ,TPI/6.28318530717959/
      NL = N
      NF = 0
      J = 0
 101  J = J+1
      IF (J-4) 102,102,103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
 105  NF = NF+1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         IFAC(IB+2) = IFAC(IB+1)
 106  CONTINUE
      IFAC(3) = 2
 107  IF (NL .NE. 1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      ARGH = TPI/FLOAT(N)
      IS = 0
      NFM1 = NF-1
      L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 111 K1=1,NFM1
         IP = IFAC(K1+2)
         L2 = L1*IP
         IDO = N/L2
         IPM = IP-1
         ARG1 = FLOAT(L1)*ARGH
         CH1 = 1.
         SH1 = 0.
         DCH1 = COS(ARG1)
         DSH1 = SIN(ARG1)
         DO 110 J=1,IPM
            CH1H = DCH1*CH1-DSH1*SH1
            SH1 = DCH1*SH1+DSH1*CH1
            CH1 = CH1H
            I = IS+2
            WA(I-1) = CH1
            WA(I) = SH1
            IF (IDO .LT. 5) GO TO 109
            DO 108 II=5,IDO,2
               I = I+2
               WA(I-1) = CH1*WA(I-3)-SH1*WA(I-2)
               WA(I) = CH1*WA(I-2)+SH1*WA(I-3)
 108        CONTINUE
 109        IS = IS+IDO
 110     CONTINUE
         L1 = L2
 111  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine ezfftf(n,r,azero,a,b,wsave)
C 
C ******************************************************************
C 
C subroutine ezfftf computes the fourier coefficients of a real
C perodic sequence (fourier analysis). the transform is defined
C below at output parameters azero,a and b. ezfftf is a simplified
C but slower version of rfftf.
C 
C input parameters
C 
C n       the length of the array r to be transformed.  the method
C         is must efficient when n is the product of small primes.
C 
C r       a real array of length n which contains the sequence
C         to be transformed. r is not destroyed.
C 
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         in the program that calls ezfftf. the wsave array must be
C         initialized by calling subroutine ezffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by ezfftf and ezfftb.
C 
C output parameters
C 
C azero   the sum from i=1 to i=n of r(i)/n
C 
C a,b     for n even b(n/2)=0. and a(n/2) is the sum from i=1 to
C         i=n of (-1)**(i-1)*r(i)/n
C 
C         for n even define kmax=n/2-1
C         for n odd  define kmax=(n-1)/2
C 
C         then for  k=1,...,kmax
C 
C              a(k) equals the sum from i=1 to i=n of
C 
C                   2./n*r(i)*cos(k*(i-1)*2*pi/n)
C 
C              b(k) equals the sum from i=1 to i=n of
C 
C                   2./n*r(i)*sin(k*(i-1)*2*pi/n)
C
      SUBROUTINE EZFFTF (N,R,AZERO,A,B,WSAVE)
C
C                       VERSION 3  JUNE 1979
C
      DIMENSION       R(*)       ,A(*)       ,B(*)       ,WSAVE(*)
      IF (N-2) 101,102,103
 101  AZERO = R(1)
      RETURN
 102  AZERO = .5*(R(1)+R(2))
      A(1) = .5*(R(1)-R(2))
      RETURN
 103  DO 104 I=1,N
         WSAVE(I) = R(I)
 104  CONTINUE
      CALL RFFTF (N,WSAVE,WSAVE(N+1))
      CF = 2./FLOAT(N)
      CFM = -CF
      AZERO = .5*CF*WSAVE(1)
      NS2 = (N+1)/2
      NS2M = NS2-1
      DO 105 I=1,NS2M
         A(I) = CF*WSAVE(2*I)
         B(I) = CFM*WSAVE(2*I+1)
 105  CONTINUE
      IF (MOD(N,2) .EQ. 1) RETURN
      A(NS2) = .5*CF*WSAVE(N)
      B(NS2) = 0.
      RETURN
      END
C
C ******************************************************************
C 
C subroutine ezfftb(n,r,azero,a,b,wsave)
C 
C ******************************************************************
C 
C subroutine ezfftb computes a real perodic sequence from its
C fourier coefficients (fourier synthesis). the transform is
C defined below at output parameter r. ezfftb is a simplified
C but slower version of rfftb.
C 
C input parameters
C 
C n       the length of the output array r.  the method is most
C         efficient when n is the product of small primes.
C 
C azero   the constant fourier coefficient
C 
C a,b     arrays which contain the remaining fourier coefficients
C         these arrays are not destroyed.
C 
C         the length of these arrays depends on whether n is even or
C         odd.
C 
C         if n is even n/2    locations are required
C         if n is odd (n-1)/2 locations are required
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         in the program that calls ezfftb. the wsave array must be
C         initialized by calling subroutine ezffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by ezfftf and ezfftb.
C 
C 
C output parameters
C 
C r       if n is even define kmax=n/2
C         if n is odd  define kmax=(n-1)/2
C 
C         then for i=1,...,n
C 
C              r(i)=azero plus the sum from k=1 to k=kmax of
C 
C              a(k)*cos(k*(i-1)*2*pi/n)+b(k)*sin(k*(i-1)*2*pi/n)
C 
C ********************* complex notation **************************
C 
C         for j=1,...,n
C 
C         r(j) equals the sum from k=-kmax to k=kmax of
C 
C              c(k)*exp(i*k*(j-1)*2*pi/n)
C 
C         where
C 
C              c(k) = .5*cmplx(a(k),-b(k))   for k=1,...,kmax
C 
C              c(-k) = conjg(c(k))
C 
C              c(0) = azero
C 
C                   and i=sqrt(-1)
C 
C *************** amplitude - phase notation ***********************
C 
C         for i=1,...,n
C 
C         r(i) equals azero plus the sum from k=1 to k=kmax of
C 
C              alpha(k)*cos(k*(i-1)*2*pi/n+beta(k))
C 
C         where
C 
C              alpha(k) = sqrt(a(k)*a(k)+b(k)*b(k))
C 
C              cos(beta(k))=a(k)/alpha(k)
C 
C              sin(beta(k))=-b(k)/alpha(k)
C
      SUBROUTINE EZFFTB (N,R,AZERO,A,B,WSAVE)
      DIMENSION       R(*)       ,A(*)       ,B(*)       ,WSAVE(*)
      IF (N-2) 101,102,103
 101  R(1) = AZERO
      RETURN
 102  R(1) = AZERO+A(1)
      R(2) = AZERO-A(1)
      RETURN
 103  NS2 = (N-1)/2
      DO 104 I=1,NS2
         R(2*I) = .5*A(I)
         R(2*I+1) = -.5*B(I)
 104  CONTINUE
      R(1) = AZERO
      IF (MOD(N,2) .EQ. 0) R(N) = A(NS2+1)
      CALL RFFTB (N,R,WSAVE(N+1))
      RETURN
      END
C
C ******************************************************************
C 
C subroutine sinti(n,wsave)
C 
C ******************************************************************
C 
C subroutine sinti initializes the array wsave which is used in
C subroutine sint. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed.  the method
C         is most efficient when n+1 is a product of small primes.
C 
C output parameter
C 
C wsave   a work array with at least int(2.5*n+15) locations.
C         different wsave arrays are required for different values
C         of n. the contents of wsave must not be changed between
C         calls of sint.
C
      SUBROUTINE SINTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      DATA PI /3.14159265358979/
      IF (N .LE. 1) RETURN
      NS2 = N/2
      NP1 = N+1
      DT = PI/FLOAT(NP1)
      DO 101 K=1,NS2
         WSAVE(K) = 2.*SIN(K*DT)
 101  CONTINUE
      CALL RFFTI (NP1,WSAVE(NS2+1))
      RETURN
      END
C
C ******************************************************************
C 
C subroutine sint(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine sint computes the discrete fourier sine transform
C of an odd sequence x(i). the transform is defined below at
C output parameter x.
C 
C sint is the unnormalized inverse of itself since a call of sint
C followed by another call of sint will multiply the input sequence
C x by 2*(n+1).
C 
C the array wsave which is used by subroutine sint must be
C initialized by calling subroutine sinti(n,wsave).
C 
C input parameters
C 
C n       the length of the sequence to be transformed.  the method
C         is most efficient when n+1 is the product of small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C 
C wsave   a work array with dimension at least int(2.5*n+15)
C         in the program that calls sint. the wsave array must be
C         initialized by calling subroutine sinti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C              x(i)= the sum from k=1 to k=n
C 
C                   2*x(k)*sin(k*i*pi/(n+1))
C 
C              a call of sint followed by another call of
C              sint will multiply the sequence x by 2*(n+1).
C              hence sint is the unnormalized inverse
C              of itself.
C 
C wsave   contains initialization calculations which must not be
C         destroyed between calls of sint.
C
      SUBROUTINE SINT (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      NP1 = N+1
      IW1 = N/2+1
      IW2 = IW1+NP1
      IW3 = IW2+NP1
      CALL SINT1(N,X,WSAVE,WSAVE(IW1),WSAVE(IW2),WSAVE(IW3))
      RETURN
      END
C
      SUBROUTINE SINT1(N,WAR,WAS,XH,X,IFAC)
      DIMENSION WAR(*),WAS(*),X(*),XH(*),IFAC(*)
      DATA SQRT3 /1.73205080756888/
      DO 100 I=1,N
         XH(I) = WAR(I)
         WAR(I) = X(I)
 100  CONTINUE
      IF (N-2) 101,102,103
 101  XH(1) = XH(1)+XH(1)
      GO TO 106
 102  XHOLD = SQRT3*(XH(1)+XH(2))
      XH(2) = SQRT3*(XH(1)-XH(2))
      XH(1) = XHOLD
      GO TO 106
 103  NP1 = N+1
      NS2 = N/2
      X(1) = 0.
      DO 104 K=1,NS2
         KC = NP1-K
         T1 = XH(K)-XH(KC)
         T2 = WAS(K)*(XH(K)+XH(KC))
         X(K+1) = T1+T2
         X(KC+1) = T2-T1
 104  CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) X(NS2+2) = 4.*XH(NS2+1)
      CALL RFFTF1 (NP1,X,XH,WAR,IFAC)
      XH(1) = .5*X(1)
      DO 105 I=3,N,2
         XH(I-1) = -X(I)
         XH(I) = XH(I-2)+X(I-1)
 105  CONTINUE
      IF (MODN .NE. 0) GO TO 106
      XH(N) = -X(N+1)
 106  DO 107 I=1,N
         X(I) = WAR(I)
         WAR(I) = XH(I)
 107  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine costi(n,wsave)
C 
C ******************************************************************
C 
C subroutine costi initializes the array wsave which is used in
C subroutine cost. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed.  the method
C         is most efficient when n-1 is a product of small primes.
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         different wsave arrays are required for different values
C         of n. the contents of wsave must not be changed between
C         calls of cost.
C
      SUBROUTINE COSTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      DATA PI /3.14159265358979/
      IF (N .LE. 3) RETURN
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      DT = PI/FLOAT(NM1)
      FK = 0.
      DO 101 K=2,NS2
         KC = NP1-K
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
         WSAVE(KC) = 2.*COS(FK*DT)
 101  CONTINUE
      CALL RFFTI (NM1,WSAVE(N+1))
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cost(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine cost computes the discrete fourier cosine transform
C of an even sequence x(i). the transform is defined below at output
C parameter x.
C 
C cost is the unnormalized inverse of itself since a call of cost
C followed by another call of cost will multiply the input sequence
C x by 2*(n-1). the transform is defined below at output parameter x
C 
C the array wsave which is used by subroutine cost must be
C initialized by calling subroutine costi(n,wsave).
C 
C input parameters
C 
C n       the length of the sequence x. n must be greater than 1.
C         the method is most efficient when n-1 is a product of
C         small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C wsave   a work array which must be dimensioned at least 3*n+15
C         in the program that calls cost. the wsave array must be
C         initialized by calling subroutine costi(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C             x(i) = x(1)+(-1)**(i-1)*x(n)
C 
C              + the sum from k=2 to k=n-1
C 
C                  2*x(k)*cos((k-1)*(i-1)*pi/(n-1))
C 
C              a call of cost followed by another call of
C              cost will multiply the sequence x by 2*(n-1)
C              hence cost is the unnormalized inverse
C              of itself.
C 
C wsave   contains initialization calculations which must not be
C         destroyed between calls of cost.
C
      SUBROUTINE COST (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N-2) 106,101,102
 101  X1H = X(1)+X(2)
      X(2) = X(1)-X(2)
      X(1) = X1H
      RETURN
 102  IF (N .GT. 3) GO TO 103
      X1P3 = X(1)+X(3)
      TX2 = X(2)+X(2)
      X(2) = X(1)-X(3)
      X(1) = X1P3+TX2
      X(3) = X1P3-TX2
      RETURN
 103  C1 = X(1)-X(N)
      X(1) = X(1)+X(N)
      DO 104 K=2,NS2
         KC = NP1-K
         T1 = X(K)+X(KC)
         T2 = X(K)-X(KC)
         C1 = C1+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(K) = T1-T2
         X(KC) = T1+T2
 104  CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) X(NS2+1) = X(NS2+1)+X(NS2+1)
      CALL RFFTF (NM1,X,WSAVE(N+1))
      XIM2 = X(2)
      X(2) = C1
      DO 105 I=4,N,2
         XI = X(I)
         X(I) = X(I-2)-X(I-1)
         X(I-1) = XIM2
         XIM2 = XI
 105  CONTINUE
      IF (MODN .NE. 0) X(N) = XIM2
 106  RETURN
      END
C
C ******************************************************************
C 
C subroutine sinqi(n,wsave)
C 
C ******************************************************************
C 
C subroutine sinqi initializes the array wsave which is used in
C both sinqf and sinqb. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed. the method
C         is most efficient when n is a product of small primes.
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         the same work array can be used for both sinqf and sinqb
C         as long as n remains unchanged. different wsave arrays
C         are required for different values of n. the contents of
C         wsave must not be changed between calls of sinqf or sinqb.
C
      SUBROUTINE SINQI (N,WSAVE)
      DIMENSION       WSAVE(*)
      CALL COSQI (N,WSAVE)
      RETURN
      END
C
C ******************************************************************
C 
C subroutine sinqf(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine sinqf computes the fast fourier transform of quarter
C wave data. that is , sinqf computes the coefficients in a sine
C series representation with only odd wave numbers. the transform
C is defined below at output parameter x.
C 
C sinqb is the unnormalized inverse of sinqf since a call of sinqf
C followed by a call of sinqb will multiply the input sequence x
C by 4*n.
C 
C the array wsave which is used by subroutine sinqf must be
C initialized by calling subroutine sinqi(n,wsave).
C 
C 
C input parameters
C 
C n       the length of the array x to be transformed.  the method
C         is most efficient when n is a product of small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         in the program that calls sinqf. the wsave array must be
C         initialized by calling subroutine sinqi(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C              x(i) = (-1)**(i-1)*x(n)
C 
C                 + the sum from k=1 to k=n-1 of
C 
C                 2*x(k)*sin((2*i-1)*k*pi/(2*n))
C 
C              a call of sinqf followed by a call of
C              sinqb will multiply the sequence x by 4*n.
C              therefore sinqb is the unnormalized inverse
C              of sinqf.
C 
C wsave   contains initialization calculations which must not
C         be destroyed between calls of sinqf or sinqb.
C
      SUBROUTINE SINQF (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      NS2 = N/2
      DO 101 K=1,NS2
         KC = N-K
         XHOLD = X(K)
         X(K) = X(KC+1)
         X(KC+1) = XHOLD
 101  CONTINUE
      CALL COSQF (N,X,WSAVE)
      DO 102 K=2,N,2
         X(K) = -X(K)
 102  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine sinqb(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine sinqb computes the fast fourier transform of quarter
C wave data. that is , sinqb computes a sequence from its
C representation in terms of a sine series with odd wave numbers.
C the transform is defined below at output parameter x.
C 
C sinqf is the unnormalized inverse of sinqb since a call of sinqb
C followed by a call of sinqf will multiply the input sequence x
C by 4*n.
C 
C the array wsave which is used by subroutine sinqb must be
C initialized by calling subroutine sinqi(n,wsave).
C 
C 
C input parameters
C 
C n       the length of the array x to be transformed.  the method
C         is most efficient when n is a product of small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         in the program that calls sinqb. the wsave array must be
C         initialized by calling subroutine sinqi(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C              x(i)= the sum from k=1 to k=n of
C 
C                4*x(k)*sin((2k-1)*i*pi/(2*n))
C 
C              a call of sinqb followed by a call of
C              sinqf will multiply the sequence x by 4*n.
C              therefore sinqf is the unnormalized inverse
C              of sinqb.
C 
C wsave   contains initialization calculations which must not
C         be destroyed between calls of sinqb or sinqf.
C
      SUBROUTINE SINQB (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      IF (N .GT. 1) GO TO 101
      X(1) = 4.*X(1)
      RETURN
 101  NS2 = N/2
      DO 102 K=2,N,2
         X(K) = -X(K)
 102  CONTINUE
      CALL COSQB (N,X,WSAVE)
      DO 103 K=1,NS2
         KC = N-K
         XHOLD = X(K)
         X(K) = X(KC+1)
         X(KC+1) = XHOLD
 103  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cosqi(n,wsave)
C 
C ******************************************************************
C 
C subroutine cosqi initializes the array wsave which is used in
C both cosqf and cosqb. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the array to be transformed.  the method
C         is most efficient when n is a product of small primes.
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 3*n+15.
C         the same work array can be used for both cosqf and cosqb
C         as long as n remains unchanged. different wsave arrays
C         are required for different values of n. the contents of
C         wsave must not be changed between calls of cosqf or cosqb.
C
      SUBROUTINE COSQI (N,WSAVE)
      DIMENSION       WSAVE(*)
      DATA PIH /1.57079632679491/
      DT = PIH/FLOAT(N)
      FK = 0.
      DO 101 K=1,N
         FK = FK+1.
         WSAVE(K) = COS(FK*DT)
 101  CONTINUE
      CALL RFFTI (N,WSAVE(N+1))
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cosqf(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine cosqf computes the fast fourier transform of quarter
C wave data. that is , cosqf computes the coefficients in a cosine
C series representation with only odd wave numbers. the transform
C is defined below at output parameter x
C 
C cosqf is the unnormalized inverse of cosqb since a call of cosqf
C followed by a call of cosqb will multiply the input sequence x
C by 4*n.
C 
C the array wsave which is used by subroutine cosqf must be
C initialized by calling subroutine cosqi(n,wsave).
C 
C 
C input parameters
C 
C n       the length of the array x to be transformed.  the method
C         is most efficient when n is a product of small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C wsave   a work array which must be dimensioned at least 3*n+15
C         in the program that calls cosqf. the wsave array must be
C         initialized by calling subroutine cosqi(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C              x(i) = x(1) plus the sum from k=2 to k=n of
C 
C                 2*x(k)*cos((2*i-1)*(k-1)*pi/(2*n))
C 
C              a call of cosqf followed by a call of
C              cosqb will multiply the sequence x by 4*n.
C              therefore cosqb is the unnormalized inverse
C              of cosqf.
C 
C wsave   contains initialization calculations which must not
C         be destroyed between calls of cosqf or cosqb.
C
      SUBROUTINE COSQF (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      DATA SQRT2 /1.4142135623731/
      IF (N-2) 102,101,103
 101  TSQX = SQRT2*X(2)
      X(2) = X(1)-TSQX
      X(1) = X(1)+TSQX
 102  RETURN
 103  CALL COSQF1 (N,X,WSAVE,WSAVE(N+1))
      RETURN
      END
C
      SUBROUTINE COSQF1 (N,X,W,XH)
      DIMENSION       X(*)       ,W(*)       ,XH(*)
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 K=2,NS2
         KC = NP2-K
         XH(K) = X(K)+X(KC)
         XH(KC) = X(K)-X(KC)
 101  CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) XH(NS2+1) = X(NS2+1)+X(NS2+1)
      DO 102 K=2,NS2
         KC = NP2-K
         X(K) = W(K-1)*XH(KC)+W(KC-1)*XH(K)
         X(KC) = W(K-1)*XH(K)-W(KC-1)*XH(KC)
 102  CONTINUE
      IF (MODN .EQ. 0) X(NS2+1) = W(NS2)*XH(NS2+1)
      CALL RFFTF (N,X,XH)
      DO 103 I=3,N,2
         XIM1 = X(I-1)-X(I)
         X(I) = X(I-1)+X(I)
         X(I-1) = XIM1
 103  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cosqb(n,x,wsave)
C 
C ******************************************************************
C 
C subroutine cosqb computes the fast fourier transform of quarter
C wave data. that is , cosqb computes a sequence from its
C representation in terms of a cosine series with odd wave numbers.
C the transform is defined below at output parameter x.
C 
C cosqb is the unnormalized inverse of cosqf since a call of cosqb
C followed by a call of cosqf will multiply the input sequence x
C by 4*n.
C 
C the array wsave which is used by subroutine cosqb must be
C initialized by calling subroutine cosqi(n,wsave).
C 
C 
C input parameters
C 
C n       the length of the array x to be transformed.  the method
C         is most efficient when n is a product of small primes.
C 
C x       an array which contains the sequence to be transformed
C 
C wsave   a work array that must be dimensioned at least 3*n+15
C         in the program that calls cosqb. the wsave array must be
C         initialized by calling subroutine cosqi(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C 
C output parameters
C 
C x       for i=1,...,n
C 
C              x(i)= the sum from k=1 to k=n of
C 
C                4*x(k)*cos((2*k-1)*(i-1)*pi/(2*n))
C 
C              a call of cosqb followed by a call of
C              cosqf will multiply the sequence x by 4*n.
C              therefore cosqf is the unnormalized inverse
C              of cosqb.
C 
C wsave   contains initialization calculations which must not
C         be destroyed between calls of cosqb or cosqf.
C
      SUBROUTINE COSQB (N,X,WSAVE)
      DIMENSION       X(*)       ,WSAVE(*)
      DATA TSQRT2 /2.82842712474619/
      IF (N-2) 101,102,103
 101  X(1) = 4.*X(1)
      RETURN
 102  X1 = 4.*(X(1)+X(2))
      X(2) = TSQRT2*(X(1)-X(2))
      X(1) = X1
      RETURN
 103  CALL COSQB1 (N,X,WSAVE,WSAVE(N+1))
      RETURN
      END
C
      SUBROUTINE COSQB1 (N,X,W,XH)
      DIMENSION       X(*)       ,W(*)       ,XH(*)
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 I=3,N,2
         XIM1 = X(I-1)+X(I)
         X(I) = X(I)-X(I-1)
         X(I-1) = XIM1
 101  CONTINUE
      X(1) = X(1)+X(1)
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) X(N) = X(N)+X(N)
      CALL RFFTB (N,X,XH)
      DO 102 K=2,NS2
         KC = NP2-K
         XH(K) = W(K-1)*X(KC)+W(KC-1)*X(K)
         XH(KC) = W(K-1)*X(K)-W(KC-1)*X(KC)
 102  CONTINUE
      IF (MODN .EQ. 0) X(NS2+1) = W(NS2)*(X(NS2+1)+X(NS2+1))
      DO 103 K=2,NS2
         KC = NP2-K
         X(K) = XH(K)+XH(KC)
         X(KC) = XH(K)-XH(KC)
 103  CONTINUE
      X(1) = X(1)+X(1)
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cffti(n,wsave)
C 
C ******************************************************************
C 
C subroutine cffti initializes the array wsave which is used in
C both cfftf and cfftb. the prime factorization of n together with
C a tabulation of the trigonometric functions are computed and
C stored in wsave.
C 
C input parameter
C 
C n       the length of the sequence to be transformed
C 
C output parameter
C 
C wsave   a work array which must be dimensioned at least 4*n+15
C         the same work array can be used for both cfftf and cfftb
C         as long as n remains unchanged. different wsave arrays
C         are required for different values of n. the contents of
C         wsave must not be changed between calls of cfftf or cfftb.
C
      SUBROUTINE CFFTI (N,WSAVE)
      DIMENSION       WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL CFFTI1 (N,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
C
      SUBROUTINE CFFTI1 (N,WA,IFAC)
      DIMENSION       WA(*)      ,IFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/3,4,2,5/
      NL = N
      NF = 0
      J = 0
 101  J = J+1
      IF (J-4) 102,102,103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
 105  NF = NF+1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         IFAC(IB+2) = IFAC(IB+1)
 106  CONTINUE
      IFAC(3) = 2
 107  IF (NL .NE. 1) GO TO 104
      IFAC(1) = N
      IFAC(2) = NF
      TPI = 6.28318530717959
      ARGH = TPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 110 K1=1,NF
         IP = IFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IDOT = IDO+IDO+2
         IPM = IP-1
         DO 109 J=1,IPM
            I1 = I
            WA(I-1) = 1.
            WA(I) = 0.
            LD = LD+L1
            FI = 0.
            ARGLD = FLOAT(LD)*ARGH
            DO 108 II=4,IDOT,2
               I = I+2
               FI = FI+1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
 108        CONTINUE
            IF (IP .LE. 5) GO TO 109
            WA(I1-1) = WA(I-1)
            WA(I1) = WA(I)
 109     CONTINUE
         L1 = L2
 110  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cfftf(n,c,wsave)
C 
C ******************************************************************
C 
C subroutine cfftf computes the forward complex discrete fourier
C transform (the fourier analysis). equivalently , cfftf computes
C the fourier coefficients of a complex periodic sequence.
C the transform is defined below at output parameter c.
C 
C the transform is not normalized. to obtain a normalized transform
C the output must be divided by n. otherwise a call of cfftf
C followed by a call of cfftb will multiply the sequence by n.
C 
C the array wsave which is used by subroutine cfftf must be
C initialized by calling subroutine cffti(n,wsave).
C 
C input parameters
C 
C 
C n      the length of the complex sequence c. the method is
C        more efficient when n is the product of small primes. n
C 
C c      a complex array of length n which contains the sequence
C 
C wsave   a real work array which must be dimensioned at least 4n+15
C         in the program that calls cfftf. the wsave array must be
C         initialized by calling subroutine cffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by cfftf and cfftb.
C 
C output parameters
C 
C c      for j=1,...,n
C 
C            c(j)=the sum from k=1,...,n of
C 
C                  c(k)*exp(-i*(j-1)*(k-1)*2*pi/n)
C 
C                        where i=sqrt(-1)
C 
C wsave   contains initialization calculations which must not be
C         destroyed between calls of subroutine cfftf or cfftb
C
      SUBROUTINE CFFTF (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL CFFTF1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
C
      SUBROUTINE CFFTF1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,IFAC(*)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PASSF4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
 101     CALL PASSF4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
 102     NA = 1-NA
         GO TO 115
 103     IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PASSF2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
 104     CALL PASSF2 (IDOT,L1,CH,C,WA(IW))
 105     NA = 1-NA
         GO TO 115
 106     IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PASSF3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
 107     CALL PASSF3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
 108     NA = 1-NA
         GO TO 115
 109     IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PASSF5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
 110     CALL PASSF5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
 111     NA = 1-NA
         GO TO 115
 112     IF (NA .NE. 0) GO TO 113
         CALL PASSF (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
 113     CALL PASSF (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
 114     IF (NAC .NE. 0) NA = 1-NA
 115     L1 = L2
         IW = IW+(IP-1)*IDOT
 116  CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
 117  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSF (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1     C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2     CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
 101        CONTINUE
 102     CONTINUE
 103  CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
 104     CONTINUE
 105  CONTINUE
      GO TO 112
 106  DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
 107        CONTINUE
 108     CONTINUE
 109  CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
 110     CONTINUE
 111  CONTINUE
 112  IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = -WA(IDL)*CH2(IK,IP)
 113     CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)-WAI*CH2(IK,JC)
 114        CONTINUE
 115     CONTINUE
 116  CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
 117     CONTINUE
 118  CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
 119     CONTINUE
 120  CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
 121  CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
 122     CONTINUE
 123  CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
 124        CONTINUE
 125     CONTINUE
 126  CONTINUE
      RETURN
 127  IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)+WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)-WA(IDIJ)*CH(I-1,K,J)
 128        CONTINUE
 129     CONTINUE
 130  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSF2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1     WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2-WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2+WA1(I)*TI2
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSF3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1     WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,-.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSF4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,2,K)-CC(2,4,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,4,K)-CC(1,2,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,2,K)-CC(I,4,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,4,K)-CC(I-1,2,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2+WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2-WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3+WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3-WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4+WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4-WA3(I)*CR4
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSF5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,-.951056516295154,
     1     -.809016994374947,-.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2+WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2-WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3+WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3-WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4+WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4-WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5+WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5-WA4(I)*DR5
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
C ******************************************************************
C 
C subroutine cfftb(n,c,wsave)
C 
C ******************************************************************
C 
C subroutine cfftb computes the backward complex discrete fourier
C transform (the fourier synthesis). equivalently , cfftb computes
C a complex periodic sequence from its fourier coefficients.
C the transform is defined below at output parameter c.
C 
C a call of cfftf followed by a call of cfftb will multiply the
C sequence by n.
C 
C the array wsave which is used by subroutine cfftb must be
C initialized by calling subroutine cffti(n,wsave).
C 
C input parameters
C 
C 
C n      the length of the complex sequence c. the method is
C        more efficient when n is the product of small primes.
C 
C c      a complex array of length n which contains the sequence
C 
C wsave   a real work array which must be dimensioned at least 4n+15
C         in the program that calls cfftb. the wsave array must be
C         initialized by calling subroutine cffti(n,wsave) and a
C         different wsave array must be used for each different
C         value of n. this initialization does not have to be
C         repeated so long as n remains unchanged thus subsequent
C         transforms can be obtained faster than the first.
C         the same wsave array can be used by cfftf and cfftb.
C 
C output parameters
C 
C c      for j=1,...,n
C 
C            c(j)=the sum from k=1,...,n of
C 
C                  c(k)*exp(i*(j-1)*(k-1)*2*pi/n)
C 
C                        where i=sqrt(-1)
C 
C wsave   contains initialization calculations which must not be
C         destroyed between calls of subroutine cfftf or cfftb
C
      SUBROUTINE CFFTB (N,C,WSAVE)
      DIMENSION       C(*)       ,WSAVE(*)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL CFFTB1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
C
      SUBROUTINE CFFTB1 (N,C,CH,WA,IFAC)
      DIMENSION       CH(*)      ,C(*)       ,WA(*)      ,IFAC(*)
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO+IDO
         IDL1 = IDOT*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IF (NA .NE. 0) GO TO 101
         CALL PASSB4 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
 101     CALL PASSB4 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
 102     NA = 1-NA
         GO TO 115
 103     IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
         CALL PASSB2 (IDOT,L1,C,CH,WA(IW))
         GO TO 105
 104     CALL PASSB2 (IDOT,L1,CH,C,WA(IW))
 105     NA = 1-NA
         GO TO 115
 106     IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDOT
         IF (NA .NE. 0) GO TO 107
         CALL PASSB3 (IDOT,L1,C,CH,WA(IW),WA(IX2))
         GO TO 108
 107     CALL PASSB3 (IDOT,L1,CH,C,WA(IW),WA(IX2))
 108     NA = 1-NA
         GO TO 115
 109     IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDOT
         IX3 = IX2+IDOT
         IX4 = IX3+IDOT
         IF (NA .NE. 0) GO TO 110
         CALL PASSB5 (IDOT,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
         GO TO 111
 110     CALL PASSB5 (IDOT,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
 111     NA = 1-NA
         GO TO 115
 112     IF (NA .NE. 0) GO TO 113
         CALL PASSB (NAC,IDOT,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
         GO TO 114
 113     CALL PASSB (NAC,IDOT,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
 114     IF (NAC .NE. 0) NA = 1-NA
 115     L1 = L2
         IW = IW+(IP-1)*IDOT
 116  CONTINUE
      IF (NA .EQ. 0) RETURN
      N2 = N+N
      DO 117 I=1,N2
         C(I) = CH(I)
 117  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSB (NAC,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
      DIMENSION       CH(IDO,L1,IP)          ,CC(IDO,IP,L1)          ,
     1     C1(IDO,L1,IP)          ,WA(*)      ,C2(IDL1,IP),
     2     CH2(IDL1,IP)
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP+2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF (IDO .LT. L1) GO TO 106
      DO 103 J=2,IPPH
         JC = IPP2-J
         DO 102 K=1,L1
            DO 101 I=1,IDO
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
 101        CONTINUE
 102     CONTINUE
 103  CONTINUE
      DO 105 K=1,L1
         DO 104 I=1,IDO
            CH(I,K,1) = CC(I,1,K)
 104     CONTINUE
 105  CONTINUE
      GO TO 112
 106  DO 109 J=2,IPPH
         JC = IPP2-J
         DO 108 I=1,IDO
            DO 107 K=1,L1
               CH(I,K,J) = CC(I,J,K)+CC(I,JC,K)
               CH(I,K,JC) = CC(I,J,K)-CC(I,JC,K)
 107        CONTINUE
 108     CONTINUE
 109  CONTINUE
      DO 111 I=1,IDO
         DO 110 K=1,L1
            CH(I,K,1) = CC(I,1,K)
 110     CONTINUE
 111  CONTINUE
 112  IDL = 2-IDO
      INC = 0
      DO 116 L=2,IPPH
         LC = IPP2-L
         IDL = IDL+IDO
         DO 113 IK=1,IDL1
            C2(IK,L) = CH2(IK,1)+WA(IDL-1)*CH2(IK,2)
            C2(IK,LC) = WA(IDL)*CH2(IK,IP)
 113     CONTINUE
         IDLJ = IDL
         INC = INC+IDO
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = IDLJ+INC
            IF (IDLJ .GT. IDP) IDLJ = IDLJ-IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 114 IK=1,IDL1
               C2(IK,L) = C2(IK,L)+WAR*CH2(IK,J)
               C2(IK,LC) = C2(IK,LC)+WAI*CH2(IK,JC)
 114        CONTINUE
 115     CONTINUE
 116  CONTINUE
      DO 118 J=2,IPPH
         DO 117 IK=1,IDL1
            CH2(IK,1) = CH2(IK,1)+CH2(IK,J)
 117     CONTINUE
 118  CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 IK=2,IDL1,2
            CH2(IK-1,J) = C2(IK-1,J)-C2(IK,JC)
            CH2(IK-1,JC) = C2(IK-1,J)+C2(IK,JC)
            CH2(IK,J) = C2(IK,J)+C2(IK-1,JC)
            CH2(IK,JC) = C2(IK,J)-C2(IK-1,JC)
 119     CONTINUE
 120  CONTINUE
      NAC = 1
      IF (IDO .EQ. 2) RETURN
      NAC = 0
      DO 121 IK=1,IDL1
         C2(IK,1) = CH2(IK,1)
 121  CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            C1(1,K,J) = CH(1,K,J)
            C1(2,K,J) = CH(2,K,J)
 122     CONTINUE
 123  CONTINUE
      IF (IDOT .GT. L1) GO TO 127
      IDIJ = 0
      DO 126 J=2,IP
         IDIJ = IDIJ+2
         DO 125 I=4,IDO,2
            IDIJ = IDIJ+2
            DO 124 K=1,L1
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
 124        CONTINUE
 125     CONTINUE
 126  CONTINUE
      RETURN
 127  IDJ = 2-IDO
      DO 130 J=2,IP
         IDJ = IDJ+IDO
         DO 129 K=1,L1
            IDIJ = IDJ
            DO 128 I=4,IDO,2
               IDIJ = IDIJ+2
               C1(I-1,K,J) = WA(IDIJ-1)*CH(I-1,K,J)-WA(IDIJ)*CH(I,K,J)
               C1(I,K,J) = WA(IDIJ-1)*CH(I,K,J)+WA(IDIJ)*CH(I-1,K,J)
 128        CONTINUE
 129     CONTINUE
 130  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSB2 (IDO,L1,CC,CH,WA1)
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,
     1     WA1(*)
      IF (IDO .GT. 2) GO TO 102
      DO 101 K=1,L1
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)
            TI2 = CC(I,1,K)-CC(I,2,K)
            CH(I,K,2) = WA1(I-1)*TI2+WA1(I)*TR2
            CH(I-1,K,2) = WA1(I-1)*TR2-WA1(I)*TI2
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSB3 (IDO,L1,CC,CH,WA1,WA2)
      DIMENSION       CC(IDO,3,L1)           ,CH(IDO,L1,3)           ,
     1     WA1(*)     ,WA2(*)
      DATA TAUR,TAUI /-.5,.866025403784439/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,2,K)+CC(1,3,K)
         CR2 = CC(1,1,K)+TAUR*TR2
         CH(1,K,1) = CC(1,1,K)+TR2
         TI2 = CC(2,2,K)+CC(2,3,K)
         CI2 = CC(2,1,K)+TAUR*TI2
         CH(2,K,1) = CC(2,1,K)+TI2
         CR3 = TAUI*(CC(1,2,K)-CC(1,3,K))
         CI3 = TAUI*(CC(2,2,K)-CC(2,3,K))
         CH(1,K,2) = CR2-CI3
         CH(1,K,3) = CR2+CI3
         CH(2,K,2) = CI2+CR3
         CH(2,K,3) = CI2-CR3
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TR2 = CC(I-1,2,K)+CC(I-1,3,K)
            CR2 = CC(I-1,1,K)+TAUR*TR2
            CH(I-1,K,1) = CC(I-1,1,K)+TR2
            TI2 = CC(I,2,K)+CC(I,3,K)
            CI2 = CC(I,1,K)+TAUR*TI2
            CH(I,K,1) = CC(I,1,K)+TI2
            CR3 = TAUI*(CC(I-1,2,K)-CC(I-1,3,K))
            CI3 = TAUI*(CC(I,2,K)-CC(I,3,K))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSB4 (IDO,L1,CC,CH,WA1,WA2,WA3)
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,1,K)-CC(2,3,K)
         TI2 = CC(2,1,K)+CC(2,3,K)
         TR4 = CC(2,4,K)-CC(2,2,K)
         TI3 = CC(2,2,K)+CC(2,4,K)
         TR1 = CC(1,1,K)-CC(1,3,K)
         TR2 = CC(1,1,K)+CC(1,3,K)
         TI4 = CC(1,2,K)-CC(1,4,K)
         TR3 = CC(1,2,K)+CC(1,4,K)
         CH(1,K,1) = TR2+TR3
         CH(1,K,3) = TR2-TR3
         CH(2,K,1) = TI2+TI3
         CH(2,K,3) = TI2-TI3
         CH(1,K,2) = TR1+TR4
         CH(1,K,4) = TR1-TR4
         CH(2,K,2) = TI1+TI4
         CH(2,K,4) = TI1-TI4
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI1 = CC(I,1,K)-CC(I,3,K)
            TI2 = CC(I,1,K)+CC(I,3,K)
            TI3 = CC(I,2,K)+CC(I,4,K)
            TR4 = CC(I,4,K)-CC(I,2,K)
            TR1 = CC(I-1,1,K)-CC(I-1,3,K)
            TR2 = CC(I-1,1,K)+CC(I-1,3,K)
            TI4 = CC(I-1,2,K)-CC(I-1,4,K)
            TR3 = CC(I-1,2,K)+CC(I-1,4,K)
            CH(I-1,K,1) = TR2+TR3
            CR3 = TR2-TR3
            CH(I,K,1) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(I-1,K,2) = WA1(I-1)*CR2-WA1(I)*CI2
            CH(I,K,2) = WA1(I-1)*CI2+WA1(I)*CR2
            CH(I-1,K,3) = WA2(I-1)*CR3-WA2(I)*CI3
            CH(I,K,3) = WA2(I-1)*CI3+WA2(I)*CR3
            CH(I-1,K,4) = WA3(I-1)*CR4-WA3(I)*CI4
            CH(I,K,4) = WA3(I-1)*CI4+WA3(I)*CR4
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
C
      SUBROUTINE PASSB5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
      DIMENSION       CC(IDO,5,L1)           ,CH(IDO,L1,5)           ,
     1     WA1(*)     ,WA2(*)     ,WA3(*)     ,WA4(*)
      DATA TR11,TI11,TR12,TI12 /.309016994374947,.951056516295154,
     1     -.809016994374947,.587785252292473/
      IF (IDO .NE. 2) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,2,K)-CC(2,5,K)
         TI2 = CC(2,2,K)+CC(2,5,K)
         TI4 = CC(2,3,K)-CC(2,4,K)
         TI3 = CC(2,3,K)+CC(2,4,K)
         TR5 = CC(1,2,K)-CC(1,5,K)
         TR2 = CC(1,2,K)+CC(1,5,K)
         TR4 = CC(1,3,K)-CC(1,4,K)
         TR3 = CC(1,3,K)+CC(1,4,K)
         CH(1,K,1) = CC(1,1,K)+TR2+TR3
         CH(2,K,1) = CC(2,1,K)+TI2+TI3
         CR2 = CC(1,1,K)+TR11*TR2+TR12*TR3
         CI2 = CC(2,1,K)+TR11*TI2+TR12*TI3
         CR3 = CC(1,1,K)+TR12*TR2+TR11*TR3
         CI3 = CC(2,1,K)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2) = CR2-CI5
         CH(1,K,5) = CR2+CI5
         CH(2,K,2) = CI2+CR5
         CH(2,K,3) = CI3+CR4
         CH(1,K,3) = CR3-CI4
         CH(1,K,4) = CR3+CI4
         CH(2,K,4) = CI3-CR4
         CH(2,K,5) = CI2-CR5
 101  CONTINUE
      RETURN
 102  DO 104 K=1,L1
         DO 103 I=2,IDO,2
            TI5 = CC(I,2,K)-CC(I,5,K)
            TI2 = CC(I,2,K)+CC(I,5,K)
            TI4 = CC(I,3,K)-CC(I,4,K)
            TI3 = CC(I,3,K)+CC(I,4,K)
            TR5 = CC(I-1,2,K)-CC(I-1,5,K)
            TR2 = CC(I-1,2,K)+CC(I-1,5,K)
            TR4 = CC(I-1,3,K)-CC(I-1,4,K)
            TR3 = CC(I-1,3,K)+CC(I-1,4,K)
            CH(I-1,K,1) = CC(I-1,1,K)+TR2+TR3
            CH(I,K,1) = CC(I,1,K)+TI2+TI3
            CR2 = CC(I-1,1,K)+TR11*TR2+TR12*TR3
            CI2 = CC(I,1,K)+TR11*TI2+TR12*TI3
            CR3 = CC(I-1,1,K)+TR12*TR2+TR11*TR3
            CI3 = CC(I,1,K)+TR12*TI2+TR11*TI3
            CR5 = TI11*TR5+TI12*TR4
            CI5 = TI11*TI5+TI12*TI4
            CR4 = TI12*TR5-TI11*TR4
            CI4 = TI12*TI5-TI11*TI4
            DR3 = CR3-CI4
            DR4 = CR3+CI4
            DI3 = CI3+CR4
            DI4 = CI3-CR4
            DR5 = CR2+CI5
            DR2 = CR2-CI5
            DI5 = CI2-CR5
            DI2 = CI2+CR5
            CH(I-1,K,2) = WA1(I-1)*DR2-WA1(I)*DI2
            CH(I,K,2) = WA1(I-1)*DI2+WA1(I)*DR2
            CH(I-1,K,3) = WA2(I-1)*DR3-WA2(I)*DI3
            CH(I,K,3) = WA2(I-1)*DI3+WA2(I)*DR3
            CH(I-1,K,4) = WA3(I-1)*DR4-WA3(I)*DI4
            CH(I,K,4) = WA3(I-1)*DI4+WA3(I)*DR4
            CH(I-1,K,5) = WA4(I-1)*DR5-WA4(I)*DI5
            CH(I,K,5) = WA4(I-1)*DI5+WA4(I)*DR5
 103     CONTINUE
 104  CONTINUE
      RETURN
      END
