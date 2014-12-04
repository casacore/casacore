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
      CALL RFFTI1_OLD (N,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTI1_OLD (N,WA,IFAC)
      DIMENSION       WA(*)      ,IFAC(*)    ,NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
      NL = N
      NF = 0
      J = 0
 101  J = J+1
C      IF (J-4) 102,102,103
      IF (J-4 .GT. 0) GOTO 103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
C      IF (NR) 101,105,101
      IF (NR .NE. 0) GOTO 101
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
      CALL RFFTF1_OLD (N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTF1_OLD (N,C,CH,WA,IFAC)
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
C      IF (IDO-2) 107,105,102
      IF (IDO-2 .LT. 0) GOTO 107
      IF (IDO-2 .EQ. 0) GOTO 105

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
C      IF (IDO-2) 107,105,102
      IF (IDO-2 .LT. 0) GOTO 107
      IF (IDO-2 .EQ. 0) GOTO 105
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
      CALL RFFTB1_OLD (N,R,WSAVE,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
C
      SUBROUTINE RFFTB1_OLD (N,C,CH,WA,IFAC)
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
C      IF (IDO-2) 107,105,102
      IF (IDO-2 .LT. 0) GOTO 107
      IF (IDO-2 .EQ. 0) GOTO 105
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
C      IF (IDO-2) 107,105,102
      IF (IDO-2 .LT. 0) GOTO 107
      IF (IDO-2 .EQ. 0) GOTO 105
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
C      IF (J-4) 102,102,103
      IF (J-4 .GT. 0) GOTO 103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
C      IF (NR) 101,105,101
      IF (NR .NE. 0) GOTO 101
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
C      IF (N-2) 101,102,103
      IF (N-2 .EQ. 0) GOTO 102
      IF (N-2 .GT. 0) GOTO 103
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
C      IF (N-2) 101,102,103
      IF (N-2 .EQ. 0) GOTO 102
      IF (N-2 .GT. 0) GOTO 103
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
C      IF (N-2) 101,102,103
      IF (N-2 .EQ. 0) GOTO 102
      IF (N-2 .GT. 0) GOTO 103
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
      CALL RFFTF1_OLD (NP1,X,XH,WAR,IFAC)
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
C      IF (N-2) 106,101,102
      IF (N-2 .LT. 0) GOTO 106
      IF (N-2 .GT. 0) GOTO 102
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
C      IF (N-2) 102,101,103
      IF (N-2 .LT. 0) GOTO 102
      IF (N-2 .GT. 0) GOTO 103
 101  TSQX = SQRT2*X(2)
      X(2) = X(1)-TSQX
      X(1) = X(1)+TSQX
 102  RETURN
 103  CALL COSQF1_OLD (N,X,WSAVE,WSAVE(N+1))
      RETURN
      END
C
      SUBROUTINE COSQF1_OLD (N,X,W,XH)
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
C      IF (N-2) 101,102,103
      IF (N-2 .EQ. 0) GOTO 102
      IF (N-2 .GT. 0) GOTO 103
 101  X(1) = 4.*X(1)
      RETURN
 102  X1 = 4.*(X(1)+X(2))
      X(2) = TSQRT2*(X(1)-X(2))
      X(1) = X1
      RETURN
 103  CALL COSQB1_OLD (N,X,WSAVE,WSAVE(N+1))
      RETURN
      END
C
      SUBROUTINE COSQB1_OLD (N,X,W,XH)
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
C      IF (J-4) 102,102,103
      IF (J-4 .GT. 0) GOTO 103
 102  NTRY = NTRYH(J)
      GO TO 104
 103  NTRY = NTRY+2
 104  NQ = NL/NTRY
      NR = NL-NTRY*NQ
C      IF (NR) 101,105,101
      IF (NR .NE. 0) GOTO 101
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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F2KB (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,2),CH(IN2,L1,2,IDO),WA(IDO,1,2)
C
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         CHOLD1 = CC(1,K,1,1)+CC(1,K,1,2)
         CC(1,K,1,2) = CC(1,K,1,1)-CC(1,K,1,2)
         CC(1,K,1,1) = CHOLD1
         CHOLD2 = CC(2,K,1,1)+CC(2,K,1,2)
         CC(2,K,1,2) = CC(2,K,1,1)-CC(2,K,1,2)
         CC(2,K,1,1) = CHOLD2
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         CH(1,K,1,1) = CC(1,K,1,1)+CC(1,K,1,2)
         CH(1,K,2,1) = CC(1,K,1,1)-CC(1,K,1,2)
         CH(2,K,1,1) = CC(2,K,1,1)+CC(2,K,1,2)
         CH(2,K,2,1) = CC(2,K,1,1)-CC(2,K,1,2)
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
            CH(1,K,1,I) = CC(1,K,I,1)+CC(1,K,I,2)
            TR2 = CC(1,K,I,1)-CC(1,K,I,2)
            CH(2,K,1,I) = CC(2,K,I,1)+CC(2,K,I,2)
            TI2 = CC(2,K,I,1)-CC(2,K,I,2)
            CH(2,K,2,I) = WA(I,1,1)*TI2+WA(I,1,2)*TR2
            CH(1,K,2,I) = WA(I,1,1)*TR2-WA(I,1,2)*TI2
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F2KF (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,2),CH(IN2,L1,2,IDO),WA(IDO,1,2)
C
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(2*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         CHOLD1 = SN*(CC(1,K,1,1)+CC(1,K,1,2))
         CC(1,K,1,2) = SN*(CC(1,K,1,1)-CC(1,K,1,2))
         CC(1,K,1,1) = CHOLD1
         CHOLD2 = SN*(CC(2,K,1,1)+CC(2,K,1,2))
         CC(2,K,1,2) = SN*(CC(2,K,1,1)-CC(2,K,1,2))
         CC(2,K,1,1) = CHOLD2
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         CH(1,K,1,1) = SN*(CC(1,K,1,1)+CC(1,K,1,2))
         CH(1,K,2,1) = SN*(CC(1,K,1,1)-CC(1,K,1,2))
         CH(2,K,1,1) = SN*(CC(2,K,1,1)+CC(2,K,1,2))
         CH(2,K,2,1) = SN*(CC(2,K,1,1)-CC(2,K,1,2))
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         CH(1,K,1,1) = CC(1,K,1,1)+CC(1,K,1,2)
         CH(1,K,2,1) = CC(1,K,1,1)-CC(1,K,1,2)
         CH(2,K,1,1) = CC(2,K,1,1)+CC(2,K,1,2)
         CH(2,K,2,1) = CC(2,K,1,1)-CC(2,K,1,2)
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
            CH(1,K,1,I) = CC(1,K,I,1)+CC(1,K,I,2)
            TR2 = CC(1,K,I,1)-CC(1,K,I,2)
            CH(2,K,1,I) = CC(2,K,I,1)+CC(2,K,I,2)
            TI2 = CC(2,K,I,1)-CC(2,K,I,2)
            CH(2,K,2,I) = WA(I,1,1)*TI2-WA(I,1,2)*TR2
            CH(1,K,2,I) = WA(I,1,1)*TR2+WA(I,1,2)*TI2
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F3KB (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,3),CH(IN2,L1,3,IDO),WA(IDO,2,2)
      DATA TAUR,TAUI /-.5,.866025403784439/
C
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CC(1,K,1,1) = CC(1,K,1,1)+TR2
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CC(2,K,1,1) = CC(2,K,1,1)+TI2
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CC(1,K,1,2) = CR2-CI3
         CC(1,K,1,3) = CR2+CI3
         CC(2,K,1,2) = CI2+CR3
         CC(2,K,1,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CH(1,K,1,1) = CC(1,K,1,1)+TR2
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CH(2,K,1,1) = CC(2,K,1,1)+TI2
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CH(1,K,2,1) = CR2-CI3
         CH(1,K,3,1) = CR2+CI3
         CH(2,K,2,1) = CI2+CR3
         CH(2,K,3,1) = CI2-CR3
  103 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
        DO 104 K=1,L1
            TR2 = CC(1,K,I,2)+CC(1,K,I,3)
            CR2 = CC(1,K,I,1)+TAUR*TR2
            CH(1,K,1,I) = CC(1,K,I,1)+TR2
            TI2 = CC(2,K,I,2)+CC(2,K,I,3)
            CI2 = CC(2,K,I,1)+TAUR*TI2
            CH(2,K,1,I) = CC(2,K,I,1)+TI2
            CR3 = TAUI*(CC(1,K,I,2)-CC(1,K,I,3))
            CI3 = TAUI*(CC(2,K,I,2)-CC(2,K,I,3))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2
            CH(1,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2
            CH(2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3
            CH(1,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F3KF (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,3),CH(IN2,L1,3,IDO),WA(IDO,2,2)
      DATA TAUR,TAUI /-.5,-.866025403784439/
C
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(3*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CC(1,K,1,1) = SN*(CC(1,K,1,1)+TR2)
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CC(2,K,1,1) = SN*(CC(2,K,1,1)+TI2)
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CC(1,K,1,2) = SN*(CR2-CI3)
         CC(1,K,1,3) = SN*(CR2+CI3)
         CC(2,K,1,2) = SN*(CI2+CR3)
         CC(2,K,1,3) = SN*(CI2-CR3)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CH(1,K,1,1) = SN*(CC(1,K,1,1)+TR2)
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CH(2,K,1,1) = SN*(CC(2,K,1,1)+TI2)
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CH(1,K,2,1) = SN*(CR2-CI3)
         CH(1,K,3,1) = SN*(CR2+CI3)
         CH(2,K,2,1) = SN*(CI2+CR3)
         CH(2,K,3,1) = SN*(CI2-CR3)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TR2 = CC(1,K,1,2)+CC(1,K,1,3)
         CR2 = CC(1,K,1,1)+TAUR*TR2
         CH(1,K,1,1) = CC(1,K,1,1)+TR2
         TI2 = CC(2,K,1,2)+CC(2,K,1,3)
         CI2 = CC(2,K,1,1)+TAUR*TI2
         CH(2,K,1,1) = CC(2,K,1,1)+TI2
         CR3 = TAUI*(CC(1,K,1,2)-CC(1,K,1,3))
         CI3 = TAUI*(CC(2,K,1,2)-CC(2,K,1,3))
         CH(1,K,2,1) = CR2-CI3
         CH(1,K,3,1) = CR2+CI3
         CH(2,K,2,1) = CI2+CR3
         CH(2,K,3,1) = CI2-CR3
  103 CONTINUE
      DO 105 I=2,IDO
        DO 104 K=1,L1
            TR2 = CC(1,K,I,2)+CC(1,K,I,3)
            CR2 = CC(1,K,I,1)+TAUR*TR2
            CH(1,K,1,I) = CC(1,K,I,1)+TR2
            TI2 = CC(2,K,I,2)+CC(2,K,I,3)
            CI2 = CC(2,K,I,1)+TAUR*TI2
            CH(2,K,1,I) = CC(2,K,I,1)+TI2
            CR3 = TAUI*(CC(1,K,I,2)-CC(1,K,I,3))
            CI3 = TAUI*(CC(2,K,I,2)-CC(2,K,I,3))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(2,K,2,I) = WA(I,1,1)*DI2-WA(I,1,2)*DR2
            CH(1,K,2,I) = WA(I,1,1)*DR2+WA(I,1,2)*DI2
            CH(2,K,3,I) = WA(I,2,1)*DI3-WA(I,2,2)*DR3
            CH(1,K,3,I) = WA(I,2,1)*DR3+WA(I,2,2)*DI3
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F4KB (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL CC(IN1,L1,IDO,4),CH(IN2,L1,4,IDO),WA(IDO,3,2)
C
C FFTPACK 5.1 auxiliary routine
C
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         TI1 = CC(2,K,1,1)-CC(2,K,1,3)
         TI2 = CC(2,K,1,1)+CC(2,K,1,3)
         TR4 = CC(2,K,1,4)-CC(2,K,1,2)
         TI3 = CC(2,K,1,2)+CC(2,K,1,4)
         TR1 = CC(1,K,1,1)-CC(1,K,1,3)
         TR2 = CC(1,K,1,1)+CC(1,K,1,3)
         TI4 = CC(1,K,1,2)-CC(1,K,1,4)
         TR3 = CC(1,K,1,2)+CC(1,K,1,4)
         CC(1,K,1,1) = TR2+TR3
         CC(1,K,1,3) = TR2-TR3
         CC(2,K,1,1) = TI2+TI3
         CC(2,K,1,3) = TI2-TI3
         CC(1,K,1,2) = TR1+TR4
         CC(1,K,1,4) = TR1-TR4
         CC(2,K,1,2) = TI1+TI4
         CC(2,K,1,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TI1 = CC(2,K,1,1)-CC(2,K,1,3)
         TI2 = CC(2,K,1,1)+CC(2,K,1,3)
         TR4 = CC(2,K,1,4)-CC(2,K,1,2)
         TI3 = CC(2,K,1,2)+CC(2,K,1,4)
         TR1 = CC(1,K,1,1)-CC(1,K,1,3)
         TR2 = CC(1,K,1,1)+CC(1,K,1,3)
         TI4 = CC(1,K,1,2)-CC(1,K,1,4)
         TR3 = CC(1,K,1,2)+CC(1,K,1,4)
         CH(1,K,1,1) = TR2+TR3
         CH(1,K,3,1) = TR2-TR3
         CH(2,K,1,1) = TI2+TI3
         CH(2,K,3,1) = TI2-TI3
         CH(1,K,2,1) = TR1+TR4
         CH(1,K,4,1) = TR1-TR4
         CH(2,K,2,1) = TI1+TI4
         CH(2,K,4,1) = TI1-TI4
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
            TI1 = CC(2,K,I,1)-CC(2,K,I,3)
            TI2 = CC(2,K,I,1)+CC(2,K,I,3)
            TI3 = CC(2,K,I,2)+CC(2,K,I,4)
            TR4 = CC(2,K,I,4)-CC(2,K,I,2)
            TR1 = CC(1,K,I,1)-CC(1,K,I,3)
            TR2 = CC(1,K,I,1)+CC(1,K,I,3)
            TI4 = CC(1,K,I,2)-CC(1,K,I,4)
            TR3 = CC(1,K,I,2)+CC(1,K,I,4)
            CH(1,K,1,I) = TR2+TR3
            CR3 = TR2-TR3
            CH(2,K,1,I) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(1,K,2,I) = WA(I,1,1)*CR2-WA(I,1,2)*CI2
            CH(2,K,2,I) = WA(I,1,1)*CI2+WA(I,1,2)*CR2
            CH(1,K,3,I) = WA(I,2,1)*CR3-WA(I,2,2)*CI3
            CH(2,K,3,I) = WA(I,2,1)*CI3+WA(I,2,2)*CR3
            CH(1,K,4,I) = WA(I,3,1)*CR4-WA(I,3,2)*CI4
            CH(2,K,4,I) = WA(I,3,1)*CI4+WA(I,3,2)*CR4
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F4KF (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL CC(IN1,L1,IDO,4),CH(IN2,L1,4,IDO),WA(IDO,3,2)
C
C FFTPACK 5.1 auxiliary routine
C
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(4*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         TI1 = CC(2,K,1,1)-CC(2,K,1,3)
         TI2 = CC(2,K,1,1)+CC(2,K,1,3)
         TR4 = CC(2,K,1,2)-CC(2,K,1,4)
         TI3 = CC(2,K,1,2)+CC(2,K,1,4)
         TR1 = CC(1,K,1,1)-CC(1,K,1,3)
         TR2 = CC(1,K,1,1)+CC(1,K,1,3)
         TI4 = CC(1,K,1,4)-CC(1,K,1,2)
         TR3 = CC(1,K,1,2)+CC(1,K,1,4)
         CC(1,K,1,1) = SN*(TR2+TR3)
         CC(1,K,1,3) = SN*(TR2-TR3)
         CC(2,K,1,1) = SN*(TI2+TI3)
         CC(2,K,1,3) = SN*(TI2-TI3)
         CC(1,K,1,2) = SN*(TR1+TR4)
         CC(1,K,1,4) = SN*(TR1-TR4)
         CC(2,K,1,2) = SN*(TI1+TI4)
         CC(2,K,1,4) = SN*(TI1-TI4)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         TI1 = CC(2,K,1,1)-CC(2,K,1,3)
         TI2 = CC(2,K,1,1)+CC(2,K,1,3)
         TR4 = CC(2,K,1,2)-CC(2,K,1,4)
         TI3 = CC(2,K,1,2)+CC(2,K,1,4)
         TR1 = CC(1,K,1,1)-CC(1,K,1,3)
         TR2 = CC(1,K,1,1)+CC(1,K,1,3)
         TI4 = CC(1,K,1,4)-CC(1,K,1,2)
         TR3 = CC(1,K,1,2)+CC(1,K,1,4)
         CH(1,K,1,1) = SN*(TR2+TR3)
         CH(1,K,3,1) = SN*(TR2-TR3)
         CH(2,K,1,1) = SN*(TI2+TI3)
         CH(2,K,3,1) = SN*(TI2-TI3)
         CH(1,K,2,1) = SN*(TR1+TR4)
         CH(1,K,4,1) = SN*(TR1-TR4)
         CH(2,K,2,1) = SN*(TI1+TI4)
         CH(2,K,4,1) = SN*(TI1-TI4)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TI1 = CC(2,K,1,1)-CC(2,K,1,3)
         TI2 = CC(2,K,1,1)+CC(2,K,1,3)
         TR4 = CC(2,K,1,2)-CC(2,K,1,4)
         TI3 = CC(2,K,1,2)+CC(2,K,1,4)
         TR1 = CC(1,K,1,1)-CC(1,K,1,3)
         TR2 = CC(1,K,1,1)+CC(1,K,1,3)
         TI4 = CC(1,K,1,4)-CC(1,K,1,2)
         TR3 = CC(1,K,1,2)+CC(1,K,1,4)
         CH(1,K,1,1) = TR2+TR3
         CH(1,K,3,1) = TR2-TR3
         CH(2,K,1,1) = TI2+TI3
         CH(2,K,3,1) = TI2-TI3
         CH(1,K,2,1) = TR1+TR4
         CH(1,K,4,1) = TR1-TR4
         CH(2,K,2,1) = TI1+TI4
         CH(2,K,4,1) = TI1-TI4
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
            TI1 = CC(2,K,I,1)-CC(2,K,I,3)
            TI2 = CC(2,K,I,1)+CC(2,K,I,3)
            TI3 = CC(2,K,I,2)+CC(2,K,I,4)
            TR4 = CC(2,K,I,2)-CC(2,K,I,4)
            TR1 = CC(1,K,I,1)-CC(1,K,I,3)
            TR2 = CC(1,K,I,1)+CC(1,K,I,3)
            TI4 = CC(1,K,I,4)-CC(1,K,I,2)
            TR3 = CC(1,K,I,2)+CC(1,K,I,4)
            CH(1,K,1,I) = TR2+TR3
            CR3 = TR2-TR3
            CH(2,K,1,I) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(1,K,2,I) = WA(I,1,1)*CR2+WA(I,1,2)*CI2
            CH(2,K,2,I) = WA(I,1,1)*CI2-WA(I,1,2)*CR2
            CH(1,K,3,I) = WA(I,2,1)*CR3+WA(I,2,2)*CI3
            CH(2,K,3,I) = WA(I,2,1)*CI3-WA(I,2,2)*CR3
            CH(1,K,4,I) = WA(I,3,1)*CR4+WA(I,3,2)*CI4
            CH(2,K,4,I) = WA(I,3,1)*CI4-WA(I,3,2)*CR4
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F5KB (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,5),CH(IN2,L1,5,IDO),WA(IDO,4,2)
      DATA TR11,TI11,TR12,TI12 /.3090169943749474,.9510565162951536,
     1-.8090169943749474,.5877852522924731/
C
C FFTPACK 5.1 auxiliary routine
C
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         TI5 = CC(2,K,1,2)-CC(2,K,1,5)
         TI2 = CC(2,K,1,2)+CC(2,K,1,5)
         TI4 = CC(2,K,1,3)-CC(2,K,1,4)
         TI3 = CC(2,K,1,3)+CC(2,K,1,4)
         TR5 = CC(1,K,1,2)-CC(1,K,1,5)
         TR2 = CC(1,K,1,2)+CC(1,K,1,5)
         TR4 = CC(1,K,1,3)-CC(1,K,1,4)
         TR3 = CC(1,K,1,3)+CC(1,K,1,4)
         CHOLD1 = CC(1,K,1,1)+TR2+TR3
         CHOLD2 = CC(2,K,1,1)+TI2+TI3
         CR2 = CC(1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,K,1,1)+TR12*TI2+TR11*TI3
         CC(1,K,1,1) = CHOLD1
         CC(2,K,1,1) = CHOLD2
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CC(1,K,1,2) = CR2-CI5
         CC(1,K,1,5) = CR2+CI5
         CC(2,K,1,2) = CI2+CR5
         CC(2,K,1,3) = CI3+CR4
         CC(1,K,1,3) = CR3-CI4
         CC(1,K,1,4) = CR3+CI4
         CC(2,K,1,4) = CI3-CR4
         CC(2,K,1,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TI5 = CC(2,K,1,2)-CC(2,K,1,5)
         TI2 = CC(2,K,1,2)+CC(2,K,1,5)
         TI4 = CC(2,K,1,3)-CC(2,K,1,4)
         TI3 = CC(2,K,1,3)+CC(2,K,1,4)
         TR5 = CC(1,K,1,2)-CC(1,K,1,5)
         TR2 = CC(1,K,1,2)+CC(1,K,1,5)
         TR4 = CC(1,K,1,3)-CC(1,K,1,4)
         TR3 = CC(1,K,1,3)+CC(1,K,1,4)
         CH(1,K,1,1) = CC(1,K,1,1)+TR2+TR3
         CH(2,K,1,1) = CC(2,K,1,1)+TI2+TI3
         CR2 = CC(1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2,1) = CR2-CI5
         CH(1,K,5,1) = CR2+CI5
         CH(2,K,2,1) = CI2+CR5
         CH(2,K,3,1) = CI3+CR4
         CH(1,K,3,1) = CR3-CI4
         CH(1,K,4,1) = CR3+CI4
         CH(2,K,4,1) = CI3-CR4
         CH(2,K,5,1) = CI2-CR5
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
            TI5 = CC(2,K,I,2)-CC(2,K,I,5)
            TI2 = CC(2,K,I,2)+CC(2,K,I,5)
            TI4 = CC(2,K,I,3)-CC(2,K,I,4)
            TI3 = CC(2,K,I,3)+CC(2,K,I,4)
            TR5 = CC(1,K,I,2)-CC(1,K,I,5)
            TR2 = CC(1,K,I,2)+CC(1,K,I,5)
            TR4 = CC(1,K,I,3)-CC(1,K,I,4)
            TR3 = CC(1,K,I,3)+CC(1,K,I,4)
            CH(1,K,1,I) = CC(1,K,I,1)+TR2+TR3
            CH(2,K,1,I) = CC(2,K,I,1)+TI2+TI3
            CR2 = CC(1,K,I,1)+TR11*TR2+TR12*TR3
            CI2 = CC(2,K,I,1)+TR11*TI2+TR12*TI3
            CR3 = CC(1,K,I,1)+TR12*TR2+TR11*TR3
            CI3 = CC(2,K,I,1)+TR12*TI2+TR11*TI3
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
            CH(1,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2
            CH(2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2
            CH(1,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3
            CH(2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3
            CH(1,K,4,I) = WA(I,3,1)*DR4-WA(I,3,2)*DI4
            CH(2,K,4,I) = WA(I,3,1)*DI4+WA(I,3,2)*DR4
            CH(1,K,5,I) = WA(I,4,1)*DR5-WA(I,4,2)*DI5
            CH(2,K,5,I) = WA(I,4,1)*DI5+WA(I,4,2)*DR5
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1F5KF (IDO,L1,NA,CC,IN1,CH,IN2,WA)
      REAL  CC(IN1,L1,IDO,5),CH(IN2,L1,5,IDO),WA(IDO,4,2)
      DATA TR11,TI11,TR12,TI12 /.3090169943749474,-.9510565162951536,
     1-.8090169943749474,-.5877852522924731/
C
C FFTPACK 5.1 auxiliary routine
C
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(5*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         TI5 = CC(2,K,1,2)-CC(2,K,1,5)
         TI2 = CC(2,K,1,2)+CC(2,K,1,5)
         TI4 = CC(2,K,1,3)-CC(2,K,1,4)
         TI3 = CC(2,K,1,3)+CC(2,K,1,4)
         TR5 = CC(1,K,1,2)-CC(1,K,1,5)
         TR2 = CC(1,K,1,2)+CC(1,K,1,5)
         TR4 = CC(1,K,1,3)-CC(1,K,1,4)
         TR3 = CC(1,K,1,3)+CC(1,K,1,4)
         CHOLD1 = SN*(CC(1,K,1,1)+TR2+TR3)
         CHOLD2 = SN*(CC(2,K,1,1)+TI2+TI3)
         CR2 = CC(1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,K,1,1)+TR12*TI2+TR11*TI3
         CC(1,K,1,1) = CHOLD1
         CC(2,K,1,1) = CHOLD2
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CC(1,K,1,2) = SN*(CR2-CI5)
         CC(1,K,1,5) = SN*(CR2+CI5)
         CC(2,K,1,2) = SN*(CI2+CR5)
         CC(2,K,1,3) = SN*(CI3+CR4)
         CC(1,K,1,3) = SN*(CR3-CI4)
         CC(1,K,1,4) = SN*(CR3+CI4)
         CC(2,K,1,4) = SN*(CI3-CR4)
         CC(2,K,1,5) = SN*(CI2-CR5)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         TI5 = CC(2,K,1,2)-CC(2,K,1,5)
         TI2 = CC(2,K,1,2)+CC(2,K,1,5)
         TI4 = CC(2,K,1,3)-CC(2,K,1,4)
         TI3 = CC(2,K,1,3)+CC(2,K,1,4)
         TR5 = CC(1,K,1,2)-CC(1,K,1,5)
         TR2 = CC(1,K,1,2)+CC(1,K,1,5)
         TR4 = CC(1,K,1,3)-CC(1,K,1,4)
         TR3 = CC(1,K,1,3)+CC(1,K,1,4)
         CH(1,K,1,1) = SN*(CC(1,K,1,1)+TR2+TR3)
         CH(2,K,1,1) = SN*(CC(2,K,1,1)+TI2+TI3)
         CR2 = CC(1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2,1) = SN*(CR2-CI5)
         CH(1,K,5,1) = SN*(CR2+CI5)
         CH(2,K,2,1) = SN*(CI2+CR5)
         CH(2,K,3,1) = SN*(CI3+CR4)
         CH(1,K,3,1) = SN*(CR3-CI4)
         CH(1,K,4,1) = SN*(CR3+CI4)
         CH(2,K,4,1) = SN*(CI3-CR4)
         CH(2,K,5,1) = SN*(CI2-CR5)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         TI5 = CC(2,K,1,2)-CC(2,K,1,5)
         TI2 = CC(2,K,1,2)+CC(2,K,1,5)
         TI4 = CC(2,K,1,3)-CC(2,K,1,4)
         TI3 = CC(2,K,1,3)+CC(2,K,1,4)
         TR5 = CC(1,K,1,2)-CC(1,K,1,5)
         TR2 = CC(1,K,1,2)+CC(1,K,1,5)
         TR4 = CC(1,K,1,3)-CC(1,K,1,4)
         TR3 = CC(1,K,1,3)+CC(1,K,1,4)
         CH(1,K,1,1) = CC(1,K,1,1)+TR2+TR3
         CH(2,K,1,1) = CC(2,K,1,1)+TI2+TI3
         CR2 = CC(1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,K,2,1) = CR2-CI5
         CH(1,K,5,1) = CR2+CI5
         CH(2,K,2,1) = CI2+CR5
         CH(2,K,3,1) = CI3+CR4
         CH(1,K,3,1) = CR3-CI4
         CH(1,K,4,1) = CR3+CI4
         CH(2,K,4,1) = CI3-CR4
         CH(2,K,5,1) = CI2-CR5
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
            TI5 = CC(2,K,I,2)-CC(2,K,I,5)
            TI2 = CC(2,K,I,2)+CC(2,K,I,5)
            TI4 = CC(2,K,I,3)-CC(2,K,I,4)
            TI3 = CC(2,K,I,3)+CC(2,K,I,4)
            TR5 = CC(1,K,I,2)-CC(1,K,I,5)
            TR2 = CC(1,K,I,2)+CC(1,K,I,5)
            TR4 = CC(1,K,I,3)-CC(1,K,I,4)
            TR3 = CC(1,K,I,3)+CC(1,K,I,4)
            CH(1,K,1,I) = CC(1,K,I,1)+TR2+TR3
            CH(2,K,1,I) = CC(2,K,I,1)+TI2+TI3
            CR2 = CC(1,K,I,1)+TR11*TR2+TR12*TR3
            CI2 = CC(2,K,I,1)+TR11*TI2+TR12*TI3
            CR3 = CC(1,K,I,1)+TR12*TR2+TR11*TR3
            CI3 = CC(2,K,I,1)+TR12*TI2+TR11*TI3
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
            CH(1,K,2,I) = WA(I,1,1)*DR2+WA(I,1,2)*DI2
            CH(2,K,2,I) = WA(I,1,1)*DI2-WA(I,1,2)*DR2
            CH(1,K,3,I) = WA(I,2,1)*DR3+WA(I,2,2)*DI3
            CH(2,K,3,I) = WA(I,2,1)*DI3-WA(I,2,2)*DR3
            CH(1,K,4,I) = WA(I,3,1)*DR4+WA(I,3,2)*DI4
            CH(2,K,4,I) = WA(I,3,1)*DI4-WA(I,3,2)*DR4
            CH(1,K,5,I) = WA(I,4,1)*DR5+WA(I,4,2)*DI5
            CH(2,K,5,I) = WA(I,4,1)*DI5-WA(I,4,2)*DR5
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1FGKB (IDO,IP,L1,LID,NA,CC,CC1,IN1,
     1                                      CH,CH1,IN2,WA)
      REAL       CH(IN2,L1,IDO,IP) ,CC(IN1,L1,IP,IDO),
     1                CC1(IN1,LID,IP)    ,CH1(IN2,LID,IP)  ,
     2                WA(IDO,IP-1,2)
C
C FFTPACK 5.1 auxiliary routine
C
      IPP2 = IP+2
      IPPH = (IP+1)/2
      DO 110 KI=1,LID
         CH1(1,KI,1) = CC1(1,KI,1)
         CH1(2,KI,1) = CC1(2,KI,1)
  110 CONTINUE
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 112 KI=1,LID
            CH1(1,KI,J) =  CC1(1,KI,J)+CC1(1,KI,JC)
            CH1(1,KI,JC) = CC1(1,KI,J)-CC1(1,KI,JC)
            CH1(2,KI,J) =  CC1(2,KI,J)+CC1(2,KI,JC)
            CH1(2,KI,JC) = CC1(2,KI,J)-CC1(2,KI,JC)
  112    CONTINUE
  111 CONTINUE
      DO 118 J=2,IPPH
         DO 117 KI=1,LID
            CC1(1,KI,1) = CC1(1,KI,1)+CH1(1,KI,J)
            CC1(2,KI,1) = CC1(2,KI,1)+CH1(2,KI,J)
  117    CONTINUE
  118 CONTINUE
      DO 116 L=2,IPPH
         LC = IPP2-L
         DO 113 KI=1,LID
            CC1(1,KI,L) = CH1(1,KI,1)+WA(1,L-1,1)*CH1(1,KI,2)
            CC1(1,KI,LC) = WA(1,L-1,2)*CH1(1,KI,IP)
            CC1(2,KI,L) = CH1(2,KI,1)+WA(1,L-1,1)*CH1(2,KI,2)
            CC1(2,KI,LC) = WA(1,L-1,2)*CH1(2,KI,IP)
  113    CONTINUE
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = MOD((L-1)*(J-1),IP)
            WAR = WA(1,IDLJ,1)
            WAI = WA(1,IDLJ,2)
            DO 114 KI=1,LID
               CC1(1,KI,L) = CC1(1,KI,L)+WAR*CH1(1,KI,J)
               CC1(1,KI,LC) = CC1(1,KI,LC)+WAI*CH1(1,KI,JC)
               CC1(2,KI,L) = CC1(2,KI,L)+WAR*CH1(2,KI,J)
               CC1(2,KI,LC) = CC1(2,KI,LC)+WAI*CH1(2,KI,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      IF(IDO.GT.1 .OR. NA.EQ.1) GO TO 136
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 KI=1,LID
            CHOLD1 = CC1(1,KI,J)-CC1(2,KI,JC)
            CHOLD2 = CC1(1,KI,J)+CC1(2,KI,JC)
            CC1(1,KI,J) = CHOLD1
            CC1(2,KI,JC) = CC1(2,KI,J)-CC1(1,KI,JC)
            CC1(2,KI,J) = CC1(2,KI,J)+CC1(1,KI,JC)
            CC1(1,KI,JC) = CHOLD2
  119    CONTINUE
  120 CONTINUE
      RETURN
  136 DO 137 KI=1,LID
         CH1(1,KI,1) = CC1(1,KI,1)
         CH1(2,KI,1) = CC1(2,KI,1)
  137 CONTINUE
      DO 135 J=2,IPPH
         JC = IPP2-J
         DO 134 KI=1,LID
            CH1(1,KI,J) = CC1(1,KI,J)-CC1(2,KI,JC)
            CH1(1,KI,JC) = CC1(1,KI,J)+CC1(2,KI,JC)
            CH1(2,KI,JC) = CC1(2,KI,J)-CC1(1,KI,JC)
            CH1(2,KI,J) = CC1(2,KI,J)+CC1(1,KI,JC)
  134    CONTINUE
  135 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 131 I=1,IDO
         DO 130 K=1,L1
            CC(1,K,1,I) = CH(1,K,I,1)
            CC(2,K,1,I) = CH(2,K,I,1)
  130    CONTINUE
  131 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            CC(1,K,J,1) = CH(1,K,1,J)
            CC(2,K,J,1) = CH(2,K,1,J)
  122    CONTINUE
  123 CONTINUE
      DO 126 J=2,IP
         DO 125 I=2,IDO
            DO 124 K=1,L1
               CC(1,K,J,I) = WA(I,J-1,1)*CH(1,K,I,J)
     1                      -WA(I,J-1,2)*CH(2,K,I,J)
               CC(2,K,J,I) = WA(I,J-1,1)*CH(2,K,I,J)
     1                      +WA(I,J-1,2)*CH(1,K,I,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1FGKF (IDO,IP,L1,LID,NA,CC,CC1,IN1,
     1                                      CH,CH1,IN2,WA)
      REAL       CH(IN2,L1,IDO,IP) ,CC(IN1,L1,IP,IDO),
     1                CC1(IN1,LID,IP)    ,CH1(IN2,LID,IP)  ,
     2                WA(IDO,IP-1,2)
C
C FFTPACK 5.1 auxiliary routine
C
      IPP2 = IP+2
      IPPH = (IP+1)/2
      DO 110 KI=1,LID
         CH1(1,KI,1) = CC1(1,KI,1)
         CH1(2,KI,1) = CC1(2,KI,1)
  110 CONTINUE
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 112 KI=1,LID
            CH1(1,KI,J) =  CC1(1,KI,J)+CC1(1,KI,JC)
            CH1(1,KI,JC) = CC1(1,KI,J)-CC1(1,KI,JC)
            CH1(2,KI,J) =  CC1(2,KI,J)+CC1(2,KI,JC)
            CH1(2,KI,JC) = CC1(2,KI,J)-CC1(2,KI,JC)
  112    CONTINUE
  111 CONTINUE
      DO 118 J=2,IPPH
         DO 117 KI=1,LID
            CC1(1,KI,1) = CC1(1,KI,1)+CH1(1,KI,J)
            CC1(2,KI,1) = CC1(2,KI,1)+CH1(2,KI,J)
  117    CONTINUE
  118 CONTINUE
      DO 116 L=2,IPPH
         LC = IPP2-L
         DO 113 KI=1,LID
            CC1(1,KI,L) = CH1(1,KI,1)+WA(1,L-1,1)*CH1(1,KI,2)
            CC1(1,KI,LC) = -WA(1,L-1,2)*CH1(1,KI,IP)
            CC1(2,KI,L) = CH1(2,KI,1)+WA(1,L-1,1)*CH1(2,KI,2)
            CC1(2,KI,LC) = -WA(1,L-1,2)*CH1(2,KI,IP)
  113    CONTINUE
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = MOD((L-1)*(J-1),IP)
            WAR = WA(1,IDLJ,1)
            WAI = -WA(1,IDLJ,2)
            DO 114 KI=1,LID
               CC1(1,KI,L) = CC1(1,KI,L)+WAR*CH1(1,KI,J)
               CC1(1,KI,LC) = CC1(1,KI,LC)+WAI*CH1(1,KI,JC)
               CC1(2,KI,L) = CC1(2,KI,L)+WAR*CH1(2,KI,J)
               CC1(2,KI,LC) = CC1(2,KI,LC)+WAI*CH1(2,KI,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      IF (IDO .GT. 1) GO TO 136
      SN = 1./REAL(IP*L1)
      IF (NA .EQ. 1) GO TO 146
      DO 149 KI=1,LID
         CC1(1,KI,1) = SN*CC1(1,KI,1)
         CC1(2,KI,1) = SN*CC1(2,KI,1)
  149 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 KI=1,LID
            CHOLD1 = SN*(CC1(1,KI,J)-CC1(2,KI,JC))
            CHOLD2 = SN*(CC1(1,KI,J)+CC1(2,KI,JC))
            CC1(1,KI,J) = CHOLD1
            CC1(2,KI,JC) = SN*(CC1(2,KI,J)-CC1(1,KI,JC))
            CC1(2,KI,J) = SN*(CC1(2,KI,J)+CC1(1,KI,JC))
            CC1(1,KI,JC) = CHOLD2
  119    CONTINUE
  120 CONTINUE
      RETURN
  146 DO 147 KI=1,LID
         CH1(1,KI,1) = SN*CC1(1,KI,1)
         CH1(2,KI,1) = SN*CC1(2,KI,1)
  147 CONTINUE
      DO 145 J=2,IPPH
         JC = IPP2-J
         DO 144 KI=1,LID
            CH1(1,KI,J) = SN*(CC1(1,KI,J)-CC1(2,KI,JC))
            CH1(2,KI,J) = SN*(CC1(2,KI,J)+CC1(1,KI,JC))
            CH1(1,KI,JC) = SN*(CC1(1,KI,J)+CC1(2,KI,JC))
            CH1(2,KI,JC) = SN*(CC1(2,KI,J)-CC1(1,KI,JC))
  144    CONTINUE
  145 CONTINUE
      RETURN
  136 DO 137 KI=1,LID
         CH1(1,KI,1) = CC1(1,KI,1)
         CH1(2,KI,1) = CC1(2,KI,1)
  137 CONTINUE
      DO 135 J=2,IPPH
         JC = IPP2-J
         DO 134 KI=1,LID
            CH1(1,KI,J) = CC1(1,KI,J)-CC1(2,KI,JC)
            CH1(2,KI,J) = CC1(2,KI,J)+CC1(1,KI,JC)
            CH1(1,KI,JC) = CC1(1,KI,J)+CC1(2,KI,JC)
            CH1(2,KI,JC) = CC1(2,KI,J)-CC1(1,KI,JC)
  134    CONTINUE
  135 CONTINUE
      DO 131 I=1,IDO
         DO 130 K=1,L1
            CC(1,K,1,I) = CH(1,K,I,1)
            CC(2,K,1,I) = CH(2,K,I,1)
  130    CONTINUE
  131 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
            CC(1,K,J,1) = CH(1,K,1,J)
            CC(2,K,J,1) = CH(2,K,1,J)
  122    CONTINUE
  123 CONTINUE
      DO 126 J=2,IP
         DO 125 I=2,IDO
            DO 124 K=1,L1
               CC(1,K,J,I) = WA(I,J-1,1)*CH(1,K,I,J)
     1                      +WA(I,J-1,2)*CH(2,K,I,J)
               CC(2,K,J,I) = WA(I,J-1,1)*CH(2,K,I,J)
     1                      -WA(I,J-1,2)*CH(1,K,I,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1FM1B (N,INC,C,CH,WA,FNF,FAC)
      COMPLEX       C(*)
      REAL       CH(*),     WA(*),     FAC(*)
C
C FFTPACK 5.1 auxiliary routine
C
      INC2 = INC+INC
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1=1,NF
         IP = FAC(K1)
         L2 = IP*L1
         IDO = N/L2
         LID = L1*IDO
         NBR = 1+NA+2*MIN(IP-2,4)
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR
   52    CALL C1F2KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   62    CALL C1F2KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   53    CALL C1F3KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   63    CALL C1F3KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   54    CALL C1F4KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   64    CALL C1F4KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   55    CALL C1F5KB (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   65    CALL C1F5KB (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   56    CALL C1FGKB (IDO,IP,L1,LID,NA,C,C,INC2,CH,CH,2,
     1     WA(IW))
         GO TO 120
   66    CALL C1FGKB (IDO,IP,L1,LID,NA,CH,CH,2,C,C,
     1     INC2,WA(IW))
  120    L1 = L2
         IW = IW+(IP-1)*(IDO+IDO)
         IF(IP .LE. 5) NA = 1-NA
  125 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE C1FM1F (N,INC,C,CH,WA,FNF,FAC)
      COMPLEX       C(*)
      REAL       CH(*),     WA(*),      FAC(*)
C
C FFTPACK 5.1 auxiliary routine
C
      INC2 = INC+INC
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1=1,NF
         IP = FAC(K1)
         L2 = IP*L1
         IDO = N/L2
         LID = L1*IDO
         NBR = 1+NA+2*MIN(IP-2,4)
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR
   52    CALL C1F2KF (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   62    CALL C1F2KF (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   53    CALL C1F3KF (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   63    CALL C1F3KF (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   54    CALL C1F4KF (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   64    CALL C1F4KF (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   55    CALL C1F5KF (IDO,L1,NA,C,INC2,CH,2,WA(IW))
         GO TO 120
   65    CALL C1F5KF (IDO,L1,NA,CH,2,C,INC2,WA(IW))
         GO TO 120
   56    CALL C1FGKF (IDO,IP,L1,LID,NA,C,C,INC2,CH,CH,
     1     2,WA(IW))
         GO TO 120
   66    CALL C1FGKF (IDO,IP,L1,LID,NA,CH,CH,2,C,C,
     1     INC2,WA(IW))
  120    L1 = L2
         IW = IW+(IP-1)*(IDO+IDO)
         IF(IP .LE. 5) NA = 1-NA
  125 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT1B (N, INC, C, LENC, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  N, INC, LENC, LENSAV, LENWRK, IER
      COMPLEX       C(LENC)
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK)
C
      IER = 0
C
      IF (LENC .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('CFFT1B ', 4)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFT1B ', 6)
      ELSEIF (LENWRK .LT. 2*N) THEN
        IER = 3
        CALL XERFFT ('CFFT1B ', 8)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL C1FM1B (N,INC,C,WORK,WSAVE,WSAVE(IW1),
     1                           WSAVE(IW1+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT1F (N, INC, C, LENC, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  N, INC, LENC, LENSAV, LENWRK, IER
      COMPLEX  C(LENC)
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK)
C
      IER = 0
C
      IF (LENC .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('CFFT1F ', 4)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFT1F ', 6)
      ELSEIF (LENWRK .LT. 2*N) THEN
        IER = 3
        CALL XERFFT ('CFFT1F ', 8)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL C1FM1F (N,INC,C,WORK,WSAVE,WSAVE(IW1),
     1                           WSAVE(IW1+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFTMI ', 3)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL MCFTI1 (N,WSAVE,WSAVE(IW1),WSAVE(IW1+1))
C
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT2B (LDIM, L, M, C, WSAVE, LENSAV,
     1                     WORK, LENWRK, IER)
      INTEGER L, M, LDIM, LENSAV, LENWRK, IER
      COMPLEX C(LDIM,M)
      REAL WSAVE(LENSAV), WORK(LENWRK)
C
C Initialize error return
C
      IER = 0
C
      IF (L .GT. LDIM) THEN
        IER = 5
        CALL XERFFT ('CFFT2B', -2)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*L + INT(LOG(REAL(L))/LOG(2.)) + 
     1                    2*M + INT(LOG(REAL(M))/LOG(2.)) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2B', 6)
        GO TO 100
      ELSEIF (LENWRK .LT. 2*L*M) THEN
        IER = 3
        CALL XERFFT ('CFFT2B', 8)
        GO TO 100
      ENDIF
C
C Transform X lines of C array
      IW = 2*L+INT(LOG(REAL(L))/LOG(2.)) + 3
      CALL CFFTMB(L, 1, M, LDIM, C, (L-1) + LDIM*(M-1) +1,
     1     WSAVE(IW), 2*M + INT(LOG(REAL(M))/LOG(2.)) + 4, 
     2     WORK, 2*L*M, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2B',-5)
        GO TO 100
      ENDIF
C
C Transform Y lines of C array
      IW = 1
      CALL CFFTMB (M, LDIM, L, 1, C, (M-1)*LDIM + L,
     1     WSAVE(IW), 2*L + INT(LOG(REAL(L))/LOG(2.)) + 4, 
     2     WORK, 2*M*L, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2B',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT2F (LDIM, L, M, C, WSAVE, LENSAV,
     1                     WORK, LENWRK, IER)
      INTEGER L, M, LDIM, LENSAV, LENWRK, IER
      COMPLEX C(LDIM,M)
      REAL WSAVE(LENSAV), WORK(LENWRK)
C
C Initialize error return
C
      IER = 0
C
      IF (L .GT. LDIM) THEN
        IER = 5
        CALL XERFFT ('CFFT2F', -2)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*L + INT(LOG(REAL(L))/LOG(2.)) + 
     1                    2*M + INT(LOG(REAL(M))/LOG(2.)) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2F', 6)
        GO TO 100
      ELSEIF (LENWRK .LT. 2*L*M) THEN
        IER = 3
        CALL XERFFT ('CFFT2F', 8)
        GO TO 100
      ENDIF
C
C Transform X lines of C array
      IW = 2*L+INT(LOG(REAL(L))/LOG(2.)) + 3
      CALL CFFTMF(L, 1, M, LDIM, C, (L-1) + LDIM*(M-1) +1,
     1     WSAVE(IW), 2*M + INT(LOG(REAL(M))/LOG(2.)) + 4, 
     2     WORK, 2*L*M, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2F',-5)
        GO TO 100
      ENDIF
C
C Transform Y lines of C array
      IW = 1
      CALL CFFTMF (M, LDIM, L, 1, C, (M-1)*LDIM + L,
     1     WSAVE(IW), 2*L + INT(LOG(REAL(L))/LOG(2.)) + 4, 
     2     WORK, 2*M*L, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2F',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, IER
      REAL WSAVE(LENSAV)
C
C Initialize error return
C
      IER = 0
C
      IF (LENSAV .LT. 2*L + INT(LOG(REAL(L))/LOG(2.)) + 
     1                    2*M + INT(LOG(REAL(M))/LOG(2.)) +8) THEN
        IER = 2
        CALL XERFFT ('CFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL CFFTMI (L, WSAVE(1), 2*L + INT(LOG(REAL(L))/LOG(2.)) + 4, 
     1 IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(2*L+INT(LOG(REAL(L))/LOG(2.)) + 3), 
     1            2*M + INT(LOG(REAL(M))/LOG(2.)) + 4, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('CFFT2I',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFTMB (LOT, JUMP, N, INC, C, LENC, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  LOT, JUMP, N, INC, LENC, LENSAV, LENWRK, IER
      COMPLEX       C(LENC)
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK)
      LOGICAL XERCON
C
      IER = 0
C
      IF (LENC .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('CFFTMB ', 6)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFTMB ', 8)
      ELSEIF (LENWRK .LT. 2*LOT*N) THEN
        IER = 3
        CALL XERFFT ('CFFTMB ', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('CFFTMB ', -1)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL CMFM1B (LOT,JUMP,N,INC,C,WORK,WSAVE,WSAVE(IW1),
     1                           WSAVE(IW1+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFTMF (LOT, JUMP, N, INC, C, LENC, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  LOT, JUMP, N, INC, LENC, LENSAV, LENWRK, IER
      COMPLEX  C(LENC)
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK)
      LOGICAL  XERCON
C
      IER = 0
C
      IF (LENC .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('CFFTMF ', 6)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFTMF ', 8)
      ELSEIF (LENWRK .LT. 2*LOT*N) THEN
        IER = 3
        CALL XERFFT ('CFFTMF ', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('CFFTMF ', -1)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL CMFM1F (LOT,JUMP,N,INC,C,WORK,WSAVE,WSAVE(IW1),
     1                           WSAVE(IW1+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFFTMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) + 4) THEN
        IER = 2
        CALL XERFFT ('CFFTMI ', 3)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      IW1 = N+N+1
      CALL MCFTI1 (N,WSAVE,WSAVE(IW1),WSAVE(IW1+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF2KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,2),CH(2,IN2,L1,2,IDO),WA(IDO,1,2)
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         CHOLD1 = CC(1,M1,K,1,1)+CC(1,M1,K,1,2)
         CC(1,M1,K,1,2) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2)
         CC(1,M1,K,1,1) = CHOLD1
         CHOLD2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,2)
         CC(2,M1,K,1,2) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2)
         CC(2,M1,K,1,1) = CHOLD2
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+CC(1,M1,K,1,2)
         CH(1,M2,K,2,1) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2)
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+CC(2,M1,K,1,2)
         CH(2,M2,K,2,1) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2)
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+CC(1,M1,K,I,2)
            TR2 = CC(1,M1,K,I,1)-CC(1,M1,K,I,2)
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+CC(2,M1,K,I,2)
            TI2 = CC(2,M1,K,I,1)-CC(2,M1,K,I,2)
            CH(2,M2,K,2,I) = WA(I,1,1)*TI2+WA(I,1,2)*TR2
            CH(1,M2,K,2,I) = WA(I,1,1)*TR2-WA(I,1,2)*TI2
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF2KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,2),CH(2,IN2,L1,2,IDO),WA(IDO,1,2)
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(2*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         CHOLD1 = SN*(CC(1,M1,K,1,1)+CC(1,M1,K,1,2))
         CC(1,M1,K,1,2) = SN*(CC(1,M1,K,1,1)-CC(1,M1,K,1,2))
         CC(1,M1,K,1,1) = CHOLD1
         CHOLD2 = SN*(CC(2,M1,K,1,1)+CC(2,M1,K,1,2))
         CC(2,M1,K,1,2) = SN*(CC(2,M1,K,1,1)-CC(2,M1,K,1,2))
         CC(2,M1,K,1,1) = CHOLD2
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         M2 = M2S
         DO 107 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(1,M2,K,1,1) = SN*(CC(1,M1,K,1,1)+CC(1,M1,K,1,2))
         CH(1,M2,K,2,1) = SN*(CC(1,M1,K,1,1)-CC(1,M1,K,1,2))
         CH(2,M2,K,1,1) = SN*(CC(2,M1,K,1,1)+CC(2,M1,K,1,2))
         CH(2,M2,K,2,1) = SN*(CC(2,M1,K,1,1)-CC(2,M1,K,1,2))
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+CC(1,M1,K,1,2)
         CH(1,M2,K,2,1) = CC(1,M1,K,1,1)-CC(1,M1,K,1,2)
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+CC(2,M1,K,1,2)
         CH(2,M2,K,2,1) = CC(2,M1,K,1,1)-CC(2,M1,K,1,2)
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+CC(1,M1,K,I,2)
            TR2 = CC(1,M1,K,I,1)-CC(1,M1,K,I,2)
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+CC(2,M1,K,I,2)
            TI2 = CC(2,M1,K,I,1)-CC(2,M1,K,I,2)
            CH(2,M2,K,2,I) = WA(I,1,1)*TI2-WA(I,1,2)*TR2
            CH(1,M2,K,2,I) = WA(I,1,1)*TR2+WA(I,1,2)*TI2
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF3KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,3),CH(2,IN2,L1,3,IDO),WA(IDO,2,2)
      DATA TAUR,TAUI /-.5,.866025403784439/
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3)
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2
         CC(1,M1,K,1,1) = CC(1,M1,K,1,1)+TR2
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3)
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2
         CC(2,M1,K,1,1) = CC(2,M1,K,1,1)+TI2
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3))
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3))
         CC(1,M1,K,1,2) = CR2-CI3
         CC(1,M1,K,1,3) = CR2+CI3
         CC(2,M1,K,1,2) = CI2+CR3
         CC(2,M1,K,1,3) = CI2-CR3
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3)
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3)
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3))
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3))
         CH(1,M2,K,2,1) = CR2-CI3
         CH(1,M2,K,3,1) = CR2+CI3
         CH(2,M2,K,2,1) = CI2+CR3
         CH(2,M2,K,3,1) = CI2-CR3
  103 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
        DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,3)
            CR2 = CC(1,M1,K,I,1)+TAUR*TR2
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,3)
            CI2 = CC(2,M1,K,I,1)+TAUR*TI2
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2
            CR3 = TAUI*(CC(1,M1,K,I,2)-CC(1,M1,K,I,3))
            CI3 = TAUI*(CC(2,M1,K,I,2)-CC(2,M1,K,I,3))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF3KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,3),CH(2,IN2,L1,3,IDO),WA(IDO,2,2)
      DATA TAUR,TAUI /-.5,-.866025403784439/
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(3*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3)
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2
         CC(1,M1,K,1,1) = SN*(CC(1,M1,K,1,1)+TR2)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3)
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2
         CC(2,M1,K,1,1) = SN*(CC(2,M1,K,1,1)+TI2)
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3))
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3))
         CC(1,M1,K,1,2) = SN*(CR2-CI3)
         CC(1,M1,K,1,3) = SN*(CR2+CI3)
         CC(2,M1,K,1,2) = SN*(CI2+CR3)
         CC(2,M1,K,1,3) = SN*(CI2-CR3)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         M2 = M2S
         DO 107 M1=1,M1D,IM1
         M2 = M2+IM2
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3)
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2
         CH(1,M2,K,1,1) = SN*(CC(1,M1,K,1,1)+TR2)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3)
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2
         CH(2,M2,K,1,1) = SN*(CC(2,M1,K,1,1)+TI2)
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3))
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3))
         CH(1,M2,K,2,1) = SN*(CR2-CI3)
         CH(1,M2,K,3,1) = SN*(CR2+CI3)
         CH(2,M2,K,2,1) = SN*(CI2+CR3)
         CH(2,M2,K,3,1) = SN*(CI2-CR3)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,3)
         CR2 = CC(1,M1,K,1,1)+TAUR*TR2
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,3)
         CI2 = CC(2,M1,K,1,1)+TAUR*TI2
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2
         CR3 = TAUI*(CC(1,M1,K,1,2)-CC(1,M1,K,1,3))
         CI3 = TAUI*(CC(2,M1,K,1,2)-CC(2,M1,K,1,3))
         CH(1,M2,K,2,1) = CR2-CI3
         CH(1,M2,K,3,1) = CR2+CI3
         CH(2,M2,K,2,1) = CI2+CR3
         CH(2,M2,K,3,1) = CI2-CR3
  103 CONTINUE
      DO 105 I=2,IDO
        DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,3)
            CR2 = CC(1,M1,K,I,1)+TAUR*TR2
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,3)
            CI2 = CC(2,M1,K,I,1)+TAUR*TI2
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2
            CR3 = TAUI*(CC(1,M1,K,I,2)-CC(1,M1,K,I,3))
            CI3 = TAUI*(CC(2,M1,K,I,2)-CC(2,M1,K,I,3))
            DR2 = CR2-CI3
            DR3 = CR2+CI3
            DI2 = CI2+CR3
            DI3 = CI2-CR3
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2-WA(I,1,2)*DR2
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2+WA(I,1,2)*DI2
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3-WA(I,2,2)*DR3
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3+WA(I,2,2)*DI3
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF4KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL CC(2,IN1,L1,IDO,4),CH(2,IN2,L1,4,IDO),WA(IDO,3,2)
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3)
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3)
         TR4 = CC(2,M1,K,1,4)-CC(2,M1,K,1,2)
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4)
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3)
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3)
         TI4 = CC(1,M1,K,1,2)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4)
         CC(1,M1,K,1,1) = TR2+TR3
         CC(1,M1,K,1,3) = TR2-TR3
         CC(2,M1,K,1,1) = TI2+TI3
         CC(2,M1,K,1,3) = TI2-TI3
         CC(1,M1,K,1,2) = TR1+TR4
         CC(1,M1,K,1,4) = TR1-TR4
         CC(2,M1,K,1,2) = TI1+TI4
         CC(2,M1,K,1,4) = TI1-TI4
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3)
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3)
         TR4 = CC(2,M1,K,1,4)-CC(2,M1,K,1,2)
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4)
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3)
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3)
         TI4 = CC(1,M1,K,1,2)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = TR2+TR3
         CH(1,M2,K,3,1) = TR2-TR3
         CH(2,M2,K,1,1) = TI2+TI3
         CH(2,M2,K,3,1) = TI2-TI3
         CH(1,M2,K,2,1) = TR1+TR4
         CH(1,M2,K,4,1) = TR1-TR4
         CH(2,M2,K,2,1) = TI1+TI4
         CH(2,M2,K,4,1) = TI1-TI4
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TI1 = CC(2,M1,K,I,1)-CC(2,M1,K,I,3)
            TI2 = CC(2,M1,K,I,1)+CC(2,M1,K,I,3)
            TI3 = CC(2,M1,K,I,2)+CC(2,M1,K,I,4)
            TR4 = CC(2,M1,K,I,4)-CC(2,M1,K,I,2)
            TR1 = CC(1,M1,K,I,1)-CC(1,M1,K,I,3)
            TR2 = CC(1,M1,K,I,1)+CC(1,M1,K,I,3)
            TI4 = CC(1,M1,K,I,2)-CC(1,M1,K,I,4)
            TR3 = CC(1,M1,K,I,2)+CC(1,M1,K,I,4)
            CH(1,M2,K,1,I) = TR2+TR3
            CR3 = TR2-TR3
            CH(2,M2,K,1,I) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(1,M2,K,2,I) = WA(I,1,1)*CR2-WA(I,1,2)*CI2
            CH(2,M2,K,2,I) = WA(I,1,1)*CI2+WA(I,1,2)*CR2
            CH(1,M2,K,3,I) = WA(I,2,1)*CR3-WA(I,2,2)*CI3
            CH(2,M2,K,3,I) = WA(I,2,1)*CI3+WA(I,2,2)*CR3
            CH(1,M2,K,4,I) = WA(I,3,1)*CR4-WA(I,3,2)*CI4
            CH(2,M2,K,4,I) = WA(I,3,1)*CI4+WA(I,3,2)*CR4
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF4KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL CC(2,IN1,L1,IDO,4),CH(2,IN2,L1,4,IDO),WA(IDO,3,2)
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(4*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3)
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3)
         TR4 = CC(2,M1,K,1,2)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4)
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3)
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3)
         TI4 = CC(1,M1,K,1,4)-CC(1,M1,K,1,2)
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4)
         CC(1,M1,K,1,1) = SN*(TR2+TR3)
         CC(1,M1,K,1,3) = SN*(TR2-TR3)
         CC(2,M1,K,1,1) = SN*(TI2+TI3)
         CC(2,M1,K,1,3) = SN*(TI2-TI3)
         CC(1,M1,K,1,2) = SN*(TR1+TR4)
         CC(1,M1,K,1,4) = SN*(TR1-TR4)
         CC(2,M1,K,1,2) = SN*(TI1+TI4)
         CC(2,M1,K,1,4) = SN*(TI1-TI4)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         M2 = M2S
         DO 107 M1=1,M1D,IM1
         M2 = M2+IM2
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3)
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3)
         TR4 = CC(2,M1,K,1,2)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4)
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3)
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3)
         TI4 = CC(1,M1,K,1,4)-CC(1,M1,K,1,2)
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = SN*(TR2+TR3)
         CH(1,M2,K,3,1) = SN*(TR2-TR3)
         CH(2,M2,K,1,1) = SN*(TI2+TI3)
         CH(2,M2,K,3,1) = SN*(TI2-TI3)
         CH(1,M2,K,2,1) = SN*(TR1+TR4)
         CH(1,M2,K,4,1) = SN*(TR1-TR4)
         CH(2,M2,K,2,1) = SN*(TI1+TI4)
         CH(2,M2,K,4,1) = SN*(TI1-TI4)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TI1 = CC(2,M1,K,1,1)-CC(2,M1,K,1,3)
         TI2 = CC(2,M1,K,1,1)+CC(2,M1,K,1,3)
         TR4 = CC(2,M1,K,1,2)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,2)+CC(2,M1,K,1,4)
         TR1 = CC(1,M1,K,1,1)-CC(1,M1,K,1,3)
         TR2 = CC(1,M1,K,1,1)+CC(1,M1,K,1,3)
         TI4 = CC(1,M1,K,1,4)-CC(1,M1,K,1,2)
         TR3 = CC(1,M1,K,1,2)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = TR2+TR3
         CH(1,M2,K,3,1) = TR2-TR3
         CH(2,M2,K,1,1) = TI2+TI3
         CH(2,M2,K,3,1) = TI2-TI3
         CH(1,M2,K,2,1) = TR1+TR4
         CH(1,M2,K,4,1) = TR1-TR4
         CH(2,M2,K,2,1) = TI1+TI4
         CH(2,M2,K,4,1) = TI1-TI4
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TI1 = CC(2,M1,K,I,1)-CC(2,M1,K,I,3)
            TI2 = CC(2,M1,K,I,1)+CC(2,M1,K,I,3)
            TI3 = CC(2,M1,K,I,2)+CC(2,M1,K,I,4)
            TR4 = CC(2,M1,K,I,2)-CC(2,M1,K,I,4)
            TR1 = CC(1,M1,K,I,1)-CC(1,M1,K,I,3)
            TR2 = CC(1,M1,K,I,1)+CC(1,M1,K,I,3)
            TI4 = CC(1,M1,K,I,4)-CC(1,M1,K,I,2)
            TR3 = CC(1,M1,K,I,2)+CC(1,M1,K,I,4)
            CH(1,M2,K,1,I) = TR2+TR3
            CR3 = TR2-TR3
            CH(2,M2,K,1,I) = TI2+TI3
            CI3 = TI2-TI3
            CR2 = TR1+TR4
            CR4 = TR1-TR4
            CI2 = TI1+TI4
            CI4 = TI1-TI4
            CH(1,M2,K,2,I) = WA(I,1,1)*CR2+WA(I,1,2)*CI2
            CH(2,M2,K,2,I) = WA(I,1,1)*CI2-WA(I,1,2)*CR2
            CH(1,M2,K,3,I) = WA(I,2,1)*CR3+WA(I,2,2)*CI3
            CH(2,M2,K,3,I) = WA(I,2,1)*CI3-WA(I,2,2)*CR3
            CH(1,M2,K,4,I) = WA(I,3,1)*CR4+WA(I,3,2)*CI4
            CH(2,M2,K,4,I) = WA(I,3,1)*CI4-WA(I,3,2)*CR4
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMF5KB (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,5),CH(2,IN2,L1,5,IDO),WA(IDO,4,2)
      DATA TR11,TI11,TR12,TI12 /.3090169943749474,.9510565162951536,
     1-.8090169943749474,.5877852522924731/
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO.GT.1 .OR. NA.EQ.1) GO TO 102
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5)
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4)
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5)
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5)
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4)
         CHOLD1 = CC(1,M1,K,1,1)+TR2+TR3
         CHOLD2 = CC(2,M1,K,1,1)+TI2+TI3
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3
         CC(1,M1,K,1,1) = CHOLD1
         CC(2,M1,K,1,1) = CHOLD2
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CC(1,M1,K,1,2) = CR2-CI5
         CC(1,M1,K,1,5) = CR2+CI5
         CC(2,M1,K,1,2) = CI2+CR5
         CC(2,M1,K,1,3) = CI3+CR4
         CC(1,M1,K,1,3) = CR3-CI4
         CC(1,M1,K,1,4) = CR3+CI4
         CC(2,M1,K,1,4) = CI3-CR4
         CC(2,M1,K,1,5) = CI2-CR5
  101 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5)
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4)
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5)
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5)
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2+TR3
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2+TI3
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,M2,K,2,1) = CR2-CI5
         CH(1,M2,K,5,1) = CR2+CI5
         CH(2,M2,K,2,1) = CI2+CR5
         CH(2,M2,K,3,1) = CI3+CR4
         CH(1,M2,K,3,1) = CR3-CI4
         CH(1,M2,K,4,1) = CR3+CI4
         CH(2,M2,K,4,1) = CI3-CR4
         CH(2,M2,K,5,1) = CI2-CR5
  103 CONTINUE
      IF(IDO .EQ. 1) RETURN
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TI5 = CC(2,M1,K,I,2)-CC(2,M1,K,I,5)
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,5)
            TI4 = CC(2,M1,K,I,3)-CC(2,M1,K,I,4)
            TI3 = CC(2,M1,K,I,3)+CC(2,M1,K,I,4)
            TR5 = CC(1,M1,K,I,2)-CC(1,M1,K,I,5)
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,5)
            TR4 = CC(1,M1,K,I,3)-CC(1,M1,K,I,4)
            TR3 = CC(1,M1,K,I,3)+CC(1,M1,K,I,4)
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2+TR3
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2+TI3
            CR2 = CC(1,M1,K,I,1)+TR11*TR2+TR12*TR3
            CI2 = CC(2,M1,K,I,1)+TR11*TI2+TR12*TI3
            CR3 = CC(1,M1,K,I,1)+TR12*TR2+TR11*TR3
            CI3 = CC(2,M1,K,I,1)+TR12*TI2+TR11*TI3
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
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2-WA(I,1,2)*DI2
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2+WA(I,1,2)*DR2
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3-WA(I,2,2)*DI3
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3+WA(I,2,2)*DR3
            CH(1,M2,K,4,I) = WA(I,3,1)*DR4-WA(I,3,2)*DI4
            CH(2,M2,K,4,I) = WA(I,3,1)*DI4+WA(I,3,2)*DR4
            CH(1,M2,K,5,I) = WA(I,4,1)*DR5-WA(I,4,2)*DI5
            CH(2,M2,K,5,I) = WA(I,4,1)*DI5+WA(I,4,2)*DR5
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CMF5KF (LOT,IDO,L1,NA,CC,IM1,IN1,CH,IM2,IN2,WA)
      REAL  CC(2,IN1,L1,IDO,5),CH(2,IN2,L1,5,IDO),WA(IDO,4,2)
      DATA TR11,TI11,TR12,TI12 /.3090169943749474,-.9510565162951536,
     1-.8090169943749474,-.5877852522924731/
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IF (IDO .GT. 1) GO TO 102
      SN = 1./REAL(5*L1)
      IF (NA .EQ. 1) GO TO 106
      DO 101 K=1,L1
         DO 101 M1=1,M1D,IM1
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5)
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4)
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5)
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5)
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4)
         CHOLD1 = SN*(CC(1,M1,K,1,1)+TR2+TR3)
         CHOLD2 = SN*(CC(2,M1,K,1,1)+TI2+TI3)
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3
         CC(1,M1,K,1,1) = CHOLD1
         CC(2,M1,K,1,1) = CHOLD2
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CC(1,M1,K,1,2) = SN*(CR2-CI5)
         CC(1,M1,K,1,5) = SN*(CR2+CI5)
         CC(2,M1,K,1,2) = SN*(CI2+CR5)
         CC(2,M1,K,1,3) = SN*(CI3+CR4)
         CC(1,M1,K,1,3) = SN*(CR3-CI4)
         CC(1,M1,K,1,4) = SN*(CR3+CI4)
         CC(2,M1,K,1,4) = SN*(CI3-CR4)
         CC(2,M1,K,1,5) = SN*(CI2-CR5)
  101 CONTINUE
      RETURN
  106 DO 107 K=1,L1
         M2 = M2S
         DO 107 M1=1,M1D,IM1
         M2 = M2+IM2
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5)
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4)
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5)
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5)
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = SN*(CC(1,M1,K,1,1)+TR2+TR3)
         CH(2,M2,K,1,1) = SN*(CC(2,M1,K,1,1)+TI2+TI3)
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,M2,K,2,1) = SN*(CR2-CI5)
         CH(1,M2,K,5,1) = SN*(CR2+CI5)
         CH(2,M2,K,2,1) = SN*(CI2+CR5)
         CH(2,M2,K,3,1) = SN*(CI3+CR4)
         CH(1,M2,K,3,1) = SN*(CR3-CI4)
         CH(1,M2,K,4,1) = SN*(CR3+CI4)
         CH(2,M2,K,4,1) = SN*(CI3-CR4)
         CH(2,M2,K,5,1) = SN*(CI2-CR5)
  107 CONTINUE
      RETURN
  102 DO 103 K=1,L1
         M2 = M2S
         DO 103 M1=1,M1D,IM1
         M2 = M2+IM2
         TI5 = CC(2,M1,K,1,2)-CC(2,M1,K,1,5)
         TI2 = CC(2,M1,K,1,2)+CC(2,M1,K,1,5)
         TI4 = CC(2,M1,K,1,3)-CC(2,M1,K,1,4)
         TI3 = CC(2,M1,K,1,3)+CC(2,M1,K,1,4)
         TR5 = CC(1,M1,K,1,2)-CC(1,M1,K,1,5)
         TR2 = CC(1,M1,K,1,2)+CC(1,M1,K,1,5)
         TR4 = CC(1,M1,K,1,3)-CC(1,M1,K,1,4)
         TR3 = CC(1,M1,K,1,3)+CC(1,M1,K,1,4)
         CH(1,M2,K,1,1) = CC(1,M1,K,1,1)+TR2+TR3
         CH(2,M2,K,1,1) = CC(2,M1,K,1,1)+TI2+TI3
         CR2 = CC(1,M1,K,1,1)+TR11*TR2+TR12*TR3
         CI2 = CC(2,M1,K,1,1)+TR11*TI2+TR12*TI3
         CR3 = CC(1,M1,K,1,1)+TR12*TR2+TR11*TR3
         CI3 = CC(2,M1,K,1,1)+TR12*TI2+TR11*TI3
         CR5 = TI11*TR5+TI12*TR4
         CI5 = TI11*TI5+TI12*TI4
         CR4 = TI12*TR5-TI11*TR4
         CI4 = TI12*TI5-TI11*TI4
         CH(1,M2,K,2,1) = CR2-CI5
         CH(1,M2,K,5,1) = CR2+CI5
         CH(2,M2,K,2,1) = CI2+CR5
         CH(2,M2,K,3,1) = CI3+CR4
         CH(1,M2,K,3,1) = CR3-CI4
         CH(1,M2,K,4,1) = CR3+CI4
         CH(2,M2,K,4,1) = CI3-CR4
         CH(2,M2,K,5,1) = CI2-CR5
  103 CONTINUE
      DO 105 I=2,IDO
         DO 104 K=1,L1
         M2 = M2S
         DO 104 M1=1,M1D,IM1
         M2 = M2+IM2
            TI5 = CC(2,M1,K,I,2)-CC(2,M1,K,I,5)
            TI2 = CC(2,M1,K,I,2)+CC(2,M1,K,I,5)
            TI4 = CC(2,M1,K,I,3)-CC(2,M1,K,I,4)
            TI3 = CC(2,M1,K,I,3)+CC(2,M1,K,I,4)
            TR5 = CC(1,M1,K,I,2)-CC(1,M1,K,I,5)
            TR2 = CC(1,M1,K,I,2)+CC(1,M1,K,I,5)
            TR4 = CC(1,M1,K,I,3)-CC(1,M1,K,I,4)
            TR3 = CC(1,M1,K,I,3)+CC(1,M1,K,I,4)
            CH(1,M2,K,1,I) = CC(1,M1,K,I,1)+TR2+TR3
            CH(2,M2,K,1,I) = CC(2,M1,K,I,1)+TI2+TI3
            CR2 = CC(1,M1,K,I,1)+TR11*TR2+TR12*TR3
            CI2 = CC(2,M1,K,I,1)+TR11*TI2+TR12*TI3
            CR3 = CC(1,M1,K,I,1)+TR12*TR2+TR11*TR3
            CI3 = CC(2,M1,K,I,1)+TR12*TI2+TR11*TI3
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
            CH(1,M2,K,2,I) = WA(I,1,1)*DR2+WA(I,1,2)*DI2
            CH(2,M2,K,2,I) = WA(I,1,1)*DI2-WA(I,1,2)*DR2
            CH(1,M2,K,3,I) = WA(I,2,1)*DR3+WA(I,2,2)*DI3
            CH(2,M2,K,3,I) = WA(I,2,1)*DI3-WA(I,2,2)*DR3
            CH(1,M2,K,4,I) = WA(I,3,1)*DR4+WA(I,3,2)*DI4
            CH(2,M2,K,4,I) = WA(I,3,1)*DI4-WA(I,3,2)*DR4
            CH(1,M2,K,5,I) = WA(I,4,1)*DR5+WA(I,4,2)*DI5
            CH(2,M2,K,5,I) = WA(I,4,1)*DI5-WA(I,4,2)*DR5
  104    CONTINUE
  105 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMFGKB (LOT,IDO,IP,L1,LID,NA,CC,CC1,IM1,IN1,
     1                                      CH,CH1,IM2,IN2,WA)
      REAL       CH(2,IN2,L1,IDO,IP) ,CC(2,IN1,L1,IP,IDO),
     1                CC1(2,IN1,LID,IP)    ,CH1(2,IN2,LID,IP)  ,
     2                WA(IDO,IP-1,2)
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IPP2 = IP+2
      IPPH = (IP+1)/2
      DO 110 KI=1,LID
         M2 = M2S
         DO 110 M1=1,M1D,IM1
         M2 = M2+IM2
         CH1(1,M2,KI,1) = CC1(1,M1,KI,1)
         CH1(2,M2,KI,1) = CC1(2,M1,KI,1)
  110 CONTINUE
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 112 KI=1,LID
         M2 = M2S
         DO 112 M1=1,M1D,IM1
         M2 = M2+IM2
            CH1(1,M2,KI,J) =  CC1(1,M1,KI,J)+CC1(1,M1,KI,JC)
            CH1(1,M2,KI,JC) = CC1(1,M1,KI,J)-CC1(1,M1,KI,JC)
            CH1(2,M2,KI,J) =  CC1(2,M1,KI,J)+CC1(2,M1,KI,JC)
            CH1(2,M2,KI,JC) = CC1(2,M1,KI,J)-CC1(2,M1,KI,JC)
  112    CONTINUE
  111 CONTINUE
      DO 118 J=2,IPPH
         DO 117 KI=1,LID
         M2 = M2S
         DO 117 M1=1,M1D,IM1
         M2 = M2+IM2
            CC1(1,M1,KI,1) = CC1(1,M1,KI,1)+CH1(1,M2,KI,J)
            CC1(2,M1,KI,1) = CC1(2,M1,KI,1)+CH1(2,M2,KI,J)
  117    CONTINUE
  118 CONTINUE
      DO 116 L=2,IPPH
         LC = IPP2-L
         DO 113 KI=1,LID
         M2 = M2S
         DO 113 M1=1,M1D,IM1
         M2 = M2+IM2
            CC1(1,M1,KI,L) = CH1(1,M2,KI,1)+WA(1,L-1,1)*CH1(1,M2,KI,2)
            CC1(1,M1,KI,LC) = WA(1,L-1,2)*CH1(1,M2,KI,IP)
            CC1(2,M1,KI,L) = CH1(2,M2,KI,1)+WA(1,L-1,1)*CH1(2,M2,KI,2)
            CC1(2,M1,KI,LC) = WA(1,L-1,2)*CH1(2,M2,KI,IP)
  113    CONTINUE
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = MOD((L-1)*(J-1),IP)
            WAR = WA(1,IDLJ,1)
            WAI = WA(1,IDLJ,2)
            DO 114 KI=1,LID
               M2 = M2S
               DO 114 M1=1,M1D,IM1
               M2 = M2+IM2
               CC1(1,M1,KI,L) = CC1(1,M1,KI,L)+WAR*CH1(1,M2,KI,J)
               CC1(1,M1,KI,LC) = CC1(1,M1,KI,LC)+WAI*CH1(1,M2,KI,JC)
               CC1(2,M1,KI,L) = CC1(2,M1,KI,L)+WAR*CH1(2,M2,KI,J)
               CC1(2,M1,KI,LC) = CC1(2,M1,KI,LC)+WAI*CH1(2,M2,KI,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      IF(IDO.GT.1 .OR. NA.EQ.1) GO TO 136
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 KI=1,LID
         DO 119 M1=1,M1D,IM1
            CHOLD1 = CC1(1,M1,KI,J)-CC1(2,M1,KI,JC)
            CHOLD2 = CC1(1,M1,KI,J)+CC1(2,M1,KI,JC)
            CC1(1,M1,KI,J) = CHOLD1
            CC1(2,M1,KI,JC) = CC1(2,M1,KI,J)-CC1(1,M1,KI,JC)
            CC1(2,M1,KI,J) = CC1(2,M1,KI,J)+CC1(1,M1,KI,JC)
            CC1(1,M1,KI,JC) = CHOLD2
  119    CONTINUE
  120 CONTINUE
      RETURN
  136 DO 137 KI=1,LID
         M2 = M2S
         DO 137 M1=1,M1D,IM1
         M2 = M2+IM2
         CH1(1,M2,KI,1) = CC1(1,M1,KI,1)
         CH1(2,M2,KI,1) = CC1(2,M1,KI,1)
  137 CONTINUE
      DO 135 J=2,IPPH
         JC = IPP2-J
         DO 134 KI=1,LID
         M2 = M2S
         DO 134 M1=1,M1D,IM1
         M2 = M2+IM2
            CH1(1,M2,KI,J) = CC1(1,M1,KI,J)-CC1(2,M1,KI,JC)
            CH1(1,M2,KI,JC) = CC1(1,M1,KI,J)+CC1(2,M1,KI,JC)
            CH1(2,M2,KI,JC) = CC1(2,M1,KI,J)-CC1(1,M1,KI,JC)
            CH1(2,M2,KI,J) = CC1(2,M1,KI,J)+CC1(1,M1,KI,JC)
  134    CONTINUE
  135 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 131 I=1,IDO
         DO 130 K=1,L1
         M2 = M2S
         DO 130 M1=1,M1D,IM1
         M2 = M2+IM2
            CC(1,M1,K,1,I) = CH(1,M2,K,I,1)
            CC(2,M1,K,1,I) = CH(2,M2,K,I,1)
  130    CONTINUE
  131 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
         M2 = M2S
         DO 122 M1=1,M1D,IM1
         M2 = M2+IM2
            CC(1,M1,K,J,1) = CH(1,M2,K,1,J)
            CC(2,M1,K,J,1) = CH(2,M2,K,1,J)
  122    CONTINUE
  123 CONTINUE
      DO 126 J=2,IP
         DO 125 I=2,IDO
            DO 124 K=1,L1
               M2 = M2S
               DO 124 M1=1,M1D,IM1
               M2 = M2+IM2
               CC(1,M1,K,J,I) = WA(I,J-1,1)*CH(1,M2,K,I,J)
     1                      -WA(I,J-1,2)*CH(2,M2,K,I,J)
               CC(2,M1,K,J,I) = WA(I,J-1,1)*CH(2,M2,K,I,J)
     1                      +WA(I,J-1,2)*CH(1,M2,K,I,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMFGKF (LOT,IDO,IP,L1,LID,NA,CC,CC1,IM1,IN1,
     1                                      CH,CH1,IM2,IN2,WA)
      REAL       CH(2,IN2,L1,IDO,IP) ,CC(2,IN1,L1,IP,IDO),
     1                CC1(2,IN1,LID,IP)    ,CH1(2,IN2,LID,IP)  ,
     2                WA(IDO,IP-1,2)
C
C FFTPACK 5.0 auxiliary routine
C
      M1D = (LOT-1)*IM1+1
      M2S = 1-IM2
      IPP2 = IP+2
      IPPH = (IP+1)/2
      DO 110 KI=1,LID
         M2 = M2S
         DO 110 M1=1,M1D,IM1
         M2 = M2+IM2
         CH1(1,M2,KI,1) = CC1(1,M1,KI,1)
         CH1(2,M2,KI,1) = CC1(2,M1,KI,1)
  110 CONTINUE
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 112 KI=1,LID
         M2 = M2S
         DO 112 M1=1,M1D,IM1
         M2 = M2+IM2
            CH1(1,M2,KI,J) =  CC1(1,M1,KI,J)+CC1(1,M1,KI,JC)
            CH1(1,M2,KI,JC) = CC1(1,M1,KI,J)-CC1(1,M1,KI,JC)
            CH1(2,M2,KI,J) =  CC1(2,M1,KI,J)+CC1(2,M1,KI,JC)
            CH1(2,M2,KI,JC) = CC1(2,M1,KI,J)-CC1(2,M1,KI,JC)
  112    CONTINUE
  111 CONTINUE
      DO 118 J=2,IPPH
         DO 117 KI=1,LID
         M2 = M2S
         DO 117 M1=1,M1D,IM1
         M2 = M2+IM2
            CC1(1,M1,KI,1) = CC1(1,M1,KI,1)+CH1(1,M2,KI,J)
            CC1(2,M1,KI,1) = CC1(2,M1,KI,1)+CH1(2,M2,KI,J)
  117    CONTINUE
  118 CONTINUE
      DO 116 L=2,IPPH
         LC = IPP2-L
         DO 113 KI=1,LID
         M2 = M2S
         DO 113 M1=1,M1D,IM1
         M2 = M2+IM2
            CC1(1,M1,KI,L) = CH1(1,M2,KI,1)+WA(1,L-1,1)*CH1(1,M2,KI,2)
            CC1(1,M1,KI,LC) = -WA(1,L-1,2)*CH1(1,M2,KI,IP)
            CC1(2,M1,KI,L) = CH1(2,M2,KI,1)+WA(1,L-1,1)*CH1(2,M2,KI,2)
            CC1(2,M1,KI,LC) = -WA(1,L-1,2)*CH1(2,M2,KI,IP)
  113    CONTINUE
         DO 115 J=3,IPPH
            JC = IPP2-J
            IDLJ = MOD((L-1)*(J-1),IP)
            WAR = WA(1,IDLJ,1)
            WAI = -WA(1,IDLJ,2)
            DO 114 KI=1,LID
               M2 = M2S
               DO 114 M1=1,M1D,IM1
               M2 = M2+IM2
               CC1(1,M1,KI,L) = CC1(1,M1,KI,L)+WAR*CH1(1,M2,KI,J)
               CC1(1,M1,KI,LC) = CC1(1,M1,KI,LC)+WAI*CH1(1,M2,KI,JC)
               CC1(2,M1,KI,L) = CC1(2,M1,KI,L)+WAR*CH1(2,M2,KI,J)
               CC1(2,M1,KI,LC) = CC1(2,M1,KI,LC)+WAI*CH1(2,M2,KI,JC)
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
      IF (IDO .GT. 1) GO TO 136
      SN = 1./REAL(IP*L1)
      IF (NA .EQ. 1) GO TO 146
      DO 149 KI=1,LID
         M2 = M2S
         DO 149 M1=1,M1D,IM1
         M2 = M2+IM2
         CC1(1,M1,KI,1) = SN*CC1(1,M1,KI,1)
         CC1(2,M1,KI,1) = SN*CC1(2,M1,KI,1)
  149 CONTINUE
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 KI=1,LID
         DO 119 M1=1,M1D,IM1
            CHOLD1 = SN*(CC1(1,M1,KI,J)-CC1(2,M1,KI,JC))
            CHOLD2 = SN*(CC1(1,M1,KI,J)+CC1(2,M1,KI,JC))
            CC1(1,M1,KI,J) = CHOLD1
            CC1(2,M1,KI,JC) = SN*(CC1(2,M1,KI,J)-CC1(1,M1,KI,JC))
            CC1(2,M1,KI,J) = SN*(CC1(2,M1,KI,J)+CC1(1,M1,KI,JC))
            CC1(1,M1,KI,JC) = CHOLD2
  119    CONTINUE
  120 CONTINUE
      RETURN
  146 DO 147 KI=1,LID
         M2 = M2S
         DO 147 M1=1,M1D,IM1
         M2 = M2+IM2
         CH1(1,M2,KI,1) = SN*CC1(1,M1,KI,1)
         CH1(2,M2,KI,1) = SN*CC1(2,M1,KI,1)
  147 CONTINUE
      DO 145 J=2,IPPH
         JC = IPP2-J
         DO 144 KI=1,LID
         M2 = M2S
         DO 144 M1=1,M1D,IM1
         M2 = M2+IM2
            CH1(1,M2,KI,J) = SN*(CC1(1,M1,KI,J)-CC1(2,M1,KI,JC))
            CH1(2,M2,KI,J) = SN*(CC1(2,M1,KI,J)+CC1(1,M1,KI,JC))
            CH1(1,M2,KI,JC) = SN*(CC1(1,M1,KI,J)+CC1(2,M1,KI,JC))
            CH1(2,M2,KI,JC) = SN*(CC1(2,M1,KI,J)-CC1(1,M1,KI,JC))
  144    CONTINUE
  145 CONTINUE
      RETURN
  136 DO 137 KI=1,LID
         M2 = M2S
         DO 137 M1=1,M1D,IM1
         M2 = M2+IM2
         CH1(1,M2,KI,1) = CC1(1,M1,KI,1)
         CH1(2,M2,KI,1) = CC1(2,M1,KI,1)
  137 CONTINUE
      DO 135 J=2,IPPH
         JC = IPP2-J
         DO 134 KI=1,LID
         M2 = M2S
         DO 134 M1=1,M1D,IM1
         M2 = M2+IM2
            CH1(1,M2,KI,J) = CC1(1,M1,KI,J)-CC1(2,M1,KI,JC)
            CH1(2,M2,KI,J) = CC1(2,M1,KI,J)+CC1(1,M1,KI,JC)
            CH1(1,M2,KI,JC) = CC1(1,M1,KI,J)+CC1(2,M1,KI,JC)
            CH1(2,M2,KI,JC) = CC1(2,M1,KI,J)-CC1(1,M1,KI,JC)
  134    CONTINUE
  135 CONTINUE
      DO 131 I=1,IDO
         DO 130 K=1,L1
         M2 = M2S
         DO 130 M1=1,M1D,IM1
         M2 = M2+IM2
            CC(1,M1,K,1,I) = CH(1,M2,K,I,1)
            CC(2,M1,K,1,I) = CH(2,M2,K,I,1)
  130    CONTINUE
  131 CONTINUE
      DO 123 J=2,IP
         DO 122 K=1,L1
         M2 = M2S
         DO 122 M1=1,M1D,IM1
         M2 = M2+IM2
            CC(1,M1,K,J,1) = CH(1,M2,K,1,J)
            CC(2,M1,K,J,1) = CH(2,M2,K,1,J)
  122    CONTINUE
  123 CONTINUE
      DO 126 J=2,IP
         DO 125 I=2,IDO
            DO 124 K=1,L1
               M2 = M2S
               DO 124 M1=1,M1D,IM1
               M2 = M2+IM2
               CC(1,M1,K,J,I) = WA(I,J-1,1)*CH(1,M2,K,I,J)
     1                      +WA(I,J-1,2)*CH(2,M2,K,I,J)
               CC(2,M1,K,J,I) = WA(I,J-1,1)*CH(2,M2,K,I,J)
     1                      -WA(I,J-1,2)*CH(1,M2,K,I,J)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMFM1B (LOT,JUMP,N,INC,C,CH,WA,FNF,FAC)
      COMPLEX       C(*)
      REAL       CH(*),     WA(*),     FAC(*)
C
C FFTPACK 5.0 auxiliary routine
C
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1=1,NF
         IP = FAC(K1)
         L2 = IP*L1
         IDO = N/L2
         LID = L1*IDO
         NBR = 1+NA+2*MIN(IP-2,4)
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR
   52    CALL CMF2KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   62    CALL CMF2KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   53    CALL CMF3KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   63    CALL CMF3KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   54    CALL CMF4KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   64    CALL CMF4KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   55    CALL CMF5KB (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   65    CALL CMF5KB (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   56    CALL CMFGKB (LOT,IDO,IP,L1,LID,NA,C,C,JUMP,INC,CH,CH,1,
     1     LOT,WA(IW))
         GO TO 120
   66    CALL CMFGKB (LOT,IDO,IP,L1,LID,NA,CH,CH,1,LOT,C,C,
     1     JUMP,INC,WA(IW))
  120    L1 = L2
         IW = IW+(IP-1)*(IDO+IDO)
         IF(IP .LE. 5) NA = 1-NA
  125 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CMFM1F (LOT,JUMP,N,INC,C,CH,WA,FNF,FAC)
      COMPLEX       C(*)
      REAL       CH(*),     WA(*),      FAC(*)
C
C FFTPACK 5.0 auxiliary routine
C
      NF = FNF
      NA = 0
      L1 = 1
      IW = 1
      DO 125 K1=1,NF
         IP = FAC(K1)
         L2 = IP*L1
         IDO = N/L2
         LID = L1*IDO
         NBR = 1+NA+2*MIN(IP-2,4)
         GO TO (52,62,53,63,54,64,55,65,56,66),NBR
   52    CALL CMF2KF (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   62    CALL CMF2KF (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   53    CALL CMF3KF (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   63    CALL CMF3KF (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   54    CALL CMF4KF (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   64    CALL CMF4KF (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   55    CALL CMF5KF (LOT,IDO,L1,NA,C,JUMP,INC,CH,1,LOT,WA(IW))
         GO TO 120
   65    CALL CMF5KF (LOT,IDO,L1,NA,CH,1,LOT,C,JUMP,INC,WA(IW))
         GO TO 120
   56    CALL CMFGKF (LOT,IDO,IP,L1,LID,NA,C,C,JUMP,INC,CH,CH,
     1     1,LOT,WA(IW))
         GO TO 120
   66    CALL CMFGKF (LOT,IDO,IP,L1,LID,NA,CH,CH,1,LOT,C,C,
     1     JUMP,INC,WA(IW))
  120    L1 = L2
         IW = IW+(IP-1)*(IDO+IDO)
         IF(IP .LE. 5) NA = 1-NA
  125 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQ1B (N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSQ1B', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQ1B', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('COSQ1B', 10)
        GO TO 300
      ENDIF
C
      IF (N-2) 300,102,103
 102  SSQRT2 = 1./SQRT(2.)
      X1 = X(1,1)+X(1,2)
      X(1,2) = SSQRT2*(X(1,1)-X(1,2))
      X(1,1) = X1
      RETURN
  103 CALL COSQB1 (N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQ1B',-5)
      ENDIF
C
  300 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQ1F (N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSQ1F', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQ1F', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('COSQ1F', 10)
        GO TO 300
      ENDIF
C
      IF (N-2) 102,101,103
  101 SSQRT2 = 1./SQRT(2.)
      TSQX = SSQRT2*X(1,2)
      X(1,2) = .5*X(1,1)-TSQX
      X(1,1) = .5*X(1,1)+TSQX
  102 RETURN
  103 CALL COSQF1 (N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQ1F',-5)
      ENDIF
C
  300 CONTINUE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQ1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQ1I', 3)
        GO TO 300
      ENDIF
C
      PIH = 2.*ATAN(1.)
      DT = PIH/FLOAT(N)
      FK = 0.
      DO 101 K=1,N
         FK = FK+1.
         WSAVE(K) = COS(FK*DT)
  101 CONTINUE
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) +4
      CALL RFFT1I (N, WSAVE(N+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQ1I',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQB1 (N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)     ,WSAVE(*)     ,WORK(*)
      IER = 0
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 I=3,N,2
         XIM1 = X(1,I-1)+X(1,I)
         X(1,I) = .5*(X(1,I-1)-X(1,I))
         X(1,I-1) = .5*XIM1
  101 CONTINUE
      X(1,1) = .5*X(1,1)
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 302
      X(1,N) = .5*X(1,N)
  302 LENX = INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = N
C
      CALL RFFT1B(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQB1',-5)
        GO TO 400
      ENDIF
C
      DO 102 K=2,NS2
         KC = NP2-K
         WORK(K) = WSAVE(K-1)*X(1,KC)+WSAVE(KC-1)*X(1,K)
         WORK(KC) = WSAVE(K-1)*X(1,K)-WSAVE(KC-1)*X(1,KC)
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 305
      X(1,NS2+1) = WSAVE(NS2)*(X(1,NS2+1)+X(1,NS2+1))
  305 DO 103 K=2,NS2
         KC = NP2-K
         X(1,K) = WORK(K)+WORK(KC)
         X(1,KC) = WORK(K)-WORK(KC)
  103 CONTINUE
      X(1,1) = X(1,1)+X(1,1)
  400 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQF1 (N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)      ,WSAVE(*)      ,WORK(*)
      IER = 0
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 K=2,NS2
         KC = NP2-K
         WORK(K)  = X(1,K)+X(1,KC)
         WORK(KC) = X(1,K)-X(1,KC)
  101 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 301
      WORK(NS2+1) = X(1,NS2+1)+X(1,NS2+1)
  301 DO 102 K=2,NS2
         KC = NP2-K
         X(1,K)  = WSAVE(K-1)*WORK(KC)+WSAVE(KC-1)*WORK(K)
         X(1,KC) = WSAVE(K-1)*WORK(K) -WSAVE(KC-1)*WORK(KC)
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 303
      X(1,NS2+1) = WSAVE(NS2)*WORK(NS2+1)
  303 LENX = INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = N
C
      CALL RFFT1F(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQF1',-5)
        GO TO 400
      ENDIF
C
      DO 103 I=3,N,2
         XIM1 = .5*(X(1,I-1)+X(1,I))
         X(1,I) = .5*(X(1,I-1)-X(1,I))
         X(1,I-1) = XIM1
  103 CONTINUE
  400 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQMB (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSQMB', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQMB', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('COSQMB', 10)
        GO TO 300
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('COSQMB', -1)
        GO TO 300
      ENDIF
C
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 101,102,103
 101  DO 201 M=1,LJ,JUMP
      X(M,1) = X(M,1)
 201  CONTINUE
      RETURN
 102  SSQRT2 = 1./SQRT(2.)
      DO 202 M=1,LJ,JUMP
      X1 = X(M,1)+X(M,2)
      X(M,2) = SSQRT2*(X(M,1)-X(M,2))
      X(M,1) = X1
 202  CONTINUE
      RETURN
  103 CALL MCSQB1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQMB',-5)
      ENDIF
C
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSQMF', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQMF', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('COSQMF', 10)
        GO TO 300
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('COSQMF', -1)
        GO TO 300
      ENDIF
C
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 102,101,103
  101 SSQRT2 = 1./SQRT(2.)
      DO 201 M=1,LJ,JUMP
      TSQX = SSQRT2*X(M,2)
      X(M,2) = .5*X(M,1)-TSQX
      X(M,1) = .5*X(M,1)+TSQX
  201 CONTINUE
  102 RETURN
  103 CALL MCSQF1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQMF',-5)
      ENDIF
C
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSQMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSQMI', 3)
        GO TO 300
      ENDIF
C
      PIH = 2.*ATAN(1.)
      DT = PIH/FLOAT(N)
      FK = 0.
      DO 101 K=1,N
         FK = FK+1.
         WSAVE(K) = COS(FK*DT)
  101 CONTINUE
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) +4
      CALL RFFTMI (N, WSAVE(N+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSQMI',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COST1B ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COST1B', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COST1B', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. N-1) THEN
        IER = 3
        CALL XERFFT ('COST1B', 10)
        GO TO 100
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL COSTB1 (N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COST1B',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COST1F ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COST1F', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COST1F', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. N-1) THEN
        IER = 3
        CALL XERFFT ('COST1F', 10)
        GO TO 100
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL COSTF1(N,INC,X,WSAVE,WORK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COST1F',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COST1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COST1I', 3)
        GO TO 300
      ENDIF
C
      IF (N .LE. 3) RETURN
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      PI = 4.*ATAN(1.)
      DT = PI/FLOAT(NM1)
      FK = 0.
      DO 101 K=2,NS2
         KC = NP1-K
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
         WSAVE(KC) = 2.*COS(FK*DT)
  101 CONTINUE
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) +4
      CALL RFFT1I (NM1, WSAVE(N+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COST1I',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTB1(N,INC,X,WSAVE,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)
      DOUBLE PRECISION           DSUM
      IER = 0
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N-2) 106,101,102
  101 X1H = X(1,1)+X(1,2)
      X(1,2) = X(1,1)-X(1,2)
      X(1,1) = X1H
      RETURN
  102 IF (N .GT. 3) GO TO 103
      X1P3 = X(1,1)+X(1,3)
      X2 = X(1,2)
      X(1,2) = X(1,1)-X(1,3)
      X(1,1) = X1P3+X2
      X(1,3) = X1P3-X2
      RETURN
  103 X(1,1) = X(1,1)+X(1,1)
      X(1,N) = X(1,N)+X(1,N)
      DSUM = X(1,1)-X(1,N)
      X(1,1) = X(1,1)+X(1,N)
      DO 104 K=2,NS2
         KC = NP1-K
         T1 = X(1,K)+X(1,KC)
         T2 = X(1,K)-X(1,KC)
         DSUM = DSUM+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(1,K) = T1-T2
         X(1,KC) = T1+T2
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      X(1,NS2+1) = X(1,NS2+1)+X(1,NS2+1)
  124 LENX = INC*(NM1-1)  + 1
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) + 4
      LNWK = NM1
C
      CALL RFFT1F(NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,
     1            LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTB1',-5)
        RETURN
      ENDIF
C
      FNM1S2 = FLOAT(NM1)/2.
      DSUM = .5*DSUM
      X(1,1) = FNM1S2*X(1,1)
      IF(MOD(NM1,2) .NE. 0) GO TO 30
      X(1,NM1) = X(1,NM1)+X(1,NM1)
   30 FNM1S4 = FLOAT(NM1)/4.
      DO 105 I=3,N,2
         XI = FNM1S4*X(1,I)
         X(1,I) = FNM1S4*X(1,I-1)
         X(1,I-1) = DSUM
         DSUM = DSUM+XI
  105 CONTINUE
      IF (MODN .NE. 0) RETURN
         X(1,N) = DSUM
  106 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTF1(N,INC,X,WSAVE,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)
      DOUBLE PRECISION           DSUM
      IER = 0
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      IF (N-2) 200,101,102
  101 X1H = X(1,1)+X(1,2)
      X(1,2) = .5*(X(1,1)-X(1,2))
      X(1,1) = .5*X1H
      GO TO 200
  102 IF (N .GT. 3) GO TO 103
      X1P3 = X(1,1)+X(1,3)
      TX2 = X(1,2)+X(1,2)
      X(1,2) = .5*(X(1,1)-X(1,3))
      X(1,1) = .25*(X1P3+TX2)
      X(1,3) = .25*(X1P3-TX2)
      GO TO 200
  103 DSUM = X(1,1)-X(1,N)
      X(1,1) = X(1,1)+X(1,N)
      DO 104 K=2,NS2
         KC = NP1-K
         T1 = X(1,K)+X(1,KC)
         T2 = X(1,K)-X(1,KC)
         DSUM = DSUM+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(1,K) = T1-T2
         X(1,KC) = T1+T2
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      X(1,NS2+1) = X(1,NS2+1)+X(1,NS2+1)
  124 LENX = INC*(NM1-1)  + 1
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) + 4
      LNWK = NM1
C
      CALL RFFT1F(NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,
     1            LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTF1',-5)
        GO TO 200
      ENDIF
C
      SNM1 = 1./FLOAT(NM1)
      DSUM = SNM1*DSUM
      IF(MOD(NM1,2) .NE. 0) GO TO 30
      X(1,NM1) = X(1,NM1)+X(1,NM1)
   30 DO 105 I=3,N,2
         XI = .5*X(1,I)
         X(1,I) = .5*X(1,I-1)
         X(1,I-1) = DSUM
         DSUM = DSUM+XI
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 117
      X(1,N) = DSUM
  117 X(1,1) = .5*X(1,1)
      X(1,N) = .5*X(1,N)
  200 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTMB (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSTMB', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSTMB', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. LOT*(N+1)) THEN
        IER = 3
        CALL XERFFT ('COSTMB', 10)
        GO TO 100
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('COSTMB', -1)
        GO TO 100
      ENDIF
C
      IW1 = LOT+LOT+1
      CALL MCSTB1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTMB',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('COSTMF', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSTMF', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. LOT*(N+1)) THEN
        IER = 3
        CALL XERFFT ('COSTMF', 10)
        GO TO 100
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('COSTMF', -1)
        GO TO 100
      ENDIF
C
      IW1 = LOT+LOT+1
      CALL MCSTF1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTMF',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE COSTMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('COSTMI', 3)
        GO TO 300
      ENDIF
C
      IF (N .LE. 3) RETURN
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      PI = 4.*ATAN(1.)
      DT = PI/FLOAT(NM1)
      FK = 0.
      DO 101 K=2,NS2
         KC = NP1-K
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
         WSAVE(KC) = 2.*COS(FK*DT)
  101 CONTINUE
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) +4
      CALL RFFTMI (NM1, WSAVE(N+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('COSTMI',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE FACTOR (N,NF,FAC)
      REAL FAC(*)
      INTEGER NTRYH(4)
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
C
      NL = N
      NF = 0
      J = 0
  101 J = J+1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY+2
  104 NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF+1
      FAC(NF) = NTRY
      NL = NQ
      IF (NL .NE. 1) GO TO 104
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCFTI1 (N,WA,FNF,FAC)
      REAL            WA(*),FAC(*) 
C
      CALL FACTOR (N,NF,FAC)
      FNF = NF
      IW = 1
      L1 = 1
      DO 110 K1=1,NF
         IP = FAC(K1)
         L2 = L1*IP
         IDO = N/L2
         CALL TABLES (IDO,IP,WA(IW))
         IW = IW+(IP-1)*(IDO+IDO)
         L1 = L2
  110 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCSQB1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)     ,WSAVE(*)     ,WORK(LOT,*)
      IER = 0
      LJ = (LOT-1)*JUMP+1
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 I=3,N,2
         DO 201 M=1,LJ,JUMP
         XIM1 = X(M,I-1)+X(M,I)
         X(M,I) = .5*(X(M,I-1)-X(M,I))
         X(M,I-1) = .5*XIM1
 201     CONTINUE
  101 CONTINUE
      DO 301 M=1,LJ,JUMP
      X(M,1) = .5*X(M,1)
 301  CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 302
      DO 303 M=1,LJ,JUMP
      X(M,N) = .5*X(M,N)
 303  CONTINUE
 302  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = LOT*N
C
      CALL RFFTMB(LOT,JUMP,N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSQB1',-5)
        GO TO 400
      ENDIF
C
      DO 102 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 202 M=1,LJ,JUMP
         M1 = M1 + 1
         WORK(M1,K) = WSAVE(K-1)*X(M,KC)+WSAVE(KC-1)*X(M,K)
         WORK(M1,KC) = WSAVE(K-1)*X(M,K)-WSAVE(KC-1)*X(M,KC)
 202     CONTINUE
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 305
      DO 304 M=1,LJ,JUMP
         X(M,NS2+1) = WSAVE(NS2)*(X(M,NS2+1)+X(M,NS2+1))
 304     CONTINUE
 305  DO 103 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 203 M=1,LJ,JUMP
            M1 = M1 + 1
            X(M,K) = WORK(M1,K)+WORK(M1,KC)
            X(M,KC) = WORK(M1,K)-WORK(M1,KC)
 203     CONTINUE
  103 CONTINUE
      DO 104 M=1,LJ,JUMP
      X(M,1) = X(M,1)+X(M,1)
 104  CONTINUE
  400 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCSQF1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER)
      DIMENSION       X(INC,*)      ,WSAVE(*)      ,WORK(LOT,*)
      IER = 0
      LJ = (LOT-1)*JUMP+1
      NS2 = (N+1)/2
      NP2 = N+2
      DO 101 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 201 M=1,LJ,JUMP
         M1 = M1 + 1
         WORK(M1,K)  = X(M,K)+X(M,KC)
         WORK(M1,KC) = X(M,K)-X(M,KC)
 201     CONTINUE
  101 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) GO TO 301
         M1 = 0
         DO 202 M=1,LJ,JUMP
         M1 = M1 + 1
         WORK(M1,NS2+1) = X(M,NS2+1)+X(M,NS2+1)
 202     CONTINUE
 301     DO 102 K=2,NS2
         KC = NP2-K
         M1 = 0
         DO 302 M=1,LJ,JUMP
         M1 = M1 + 1
         X(M,K)  = WSAVE(K-1)*WORK(M1,KC)+WSAVE(KC-1)*WORK(M1,K)
         X(M,KC) = WSAVE(K-1)*WORK(M1,K) -WSAVE(KC-1)*WORK(M1,KC)
 302     CONTINUE
  102 CONTINUE
      IF (MODN .NE. 0) GO TO 303
      M1 = 0
      DO 304 M=1,LJ,JUMP
         M1 = M1 + 1
         X(M,NS2+1) = WSAVE(NS2)*WORK(M1,NS2+1)
 304  CONTINUE
 303  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(N-1)  + 1
      LNSV = N + INT(LOG(REAL(N))/LOG(2.)) + 4
      LNWK = LOT*N
C
      CALL RFFTMF(LOT,JUMP,N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSQF1',-5)
        GO TO 400
      ENDIF
C
      DO 103 I=3,N,2
         DO 203 M=1,LJ,JUMP
            XIM1 = .5*(X(M,I-1)+X(M,I))
            X(M,I) = .5*(X(M,I-1)-X(M,I))
            X(M,I-1) = XIM1
 203     CONTINUE
  103 CONTINUE
  400 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCSTB1(LOT,JUMP,N,INC,X,WSAVE,DSUM,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)
      DOUBLE PRECISION           DSUM(*)
      IER = 0
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 106,101,102
  101 DO 111 M=1,LJ,JUMP
         X1H = X(M,1)+X(M,2)
         X(M,2) = X(M,1)-X(M,2)
         X(M,1) = X1H
  111 CONTINUE
      RETURN
  102 IF (N .GT. 3) GO TO 103
      DO 112 M=1,LJ,JUMP
         X1P3 = X(M,1)+X(M,3)
         X2 = X(M,2)
         X(M,2) = X(M,1)-X(M,3)
         X(M,1) = X1P3+X2
         X(M,3) = X1P3-X2
  112 CONTINUE
      RETURN
 103  DO 118 M=1,LJ,JUMP
      X(M,1) = X(M,1)+X(M,1)
      X(M,N) = X(M,N)+X(M,N)
 118  CONTINUE
      M1 = 0
      DO 113 M=1,LJ,JUMP
         M1 = M1+1
         DSUM(M1) = X(M,1)-X(M,N)
         X(M,1) = X(M,1)+X(M,N)
  113 CONTINUE
      DO 104 K=2,NS2
         M1 = 0
         DO 114 M=1,LJ,JUMP
         M1 = M1+1
         KC = NP1-K
         T1 = X(M,K)+X(M,KC)
         T2 = X(M,K)-X(M,KC)
         DSUM(M1) = DSUM(M1)+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(M,K) = T1-T2
         X(M,KC) = T1+T2
  114    CONTINUE
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
         DO 123 M=1,LJ,JUMP
         X(M,NS2+1) = X(M,NS2+1)+X(M,NS2+1)
  123    CONTINUE
 124  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(NM1-1)  + 1
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) + 4
      LNWK = LOT*NM1
C
      CALL RFFTMF(LOT,JUMP,NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,
     1            LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSTB1',-5)
        GO TO 106
      ENDIF
C
      FNM1S2 = FLOAT(NM1)/2.
      M1 = 0
      DO 10 M=1,LJ,JUMP
      M1 = M1+1
      DSUM(M1) = .5*DSUM(M1)
      X(M,1) = FNM1S2*X(M,1)
   10 CONTINUE
      IF(MOD(NM1,2) .NE. 0) GO TO 30
      DO 20 M=1,LJ,JUMP
      X(M,NM1) = X(M,NM1)+X(M,NM1)
   20 CONTINUE
 30   FNM1S4 = FLOAT(NM1)/4.
      DO 105 I=3,N,2
         M1 = 0
         DO 115 M=1,LJ,JUMP
            M1 = M1+1
            XI = FNM1S4*X(M,I)
            X(M,I) = FNM1S4*X(M,I-1)
            X(M,I-1) = DSUM(M1)
            DSUM(M1) = DSUM(M1)+XI
  115 CONTINUE
  105 CONTINUE
      IF (MODN .NE. 0) RETURN
      M1 = 0
      DO 116 M=1,LJ,JUMP
         M1 = M1+1
         X(M,N) = DSUM(M1)
  116 CONTINUE
  106 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCSTF1(LOT,JUMP,N,INC,X,WSAVE,DSUM,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)
      DOUBLE PRECISION           DSUM(*)
      IER = 0
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 200,101,102
  101 DO 111 M=1,LJ,JUMP
         X1H = X(M,1)+X(M,2)
         X(M,2) = .5*(X(M,1)-X(M,2))
         X(M,1) = .5*X1H
  111 CONTINUE
      GO TO 200
  102 IF (N .GT. 3) GO TO 103
      DO 112 M=1,LJ,JUMP
         X1P3 = X(M,1)+X(M,3)
         TX2 = X(M,2)+X(M,2)
         X(M,2) = .5*(X(M,1)-X(M,3))
         X(M,1) = .25*(X1P3+TX2)
         X(M,3) = .25*(X1P3-TX2)
  112 CONTINUE
      GO TO 200
  103 M1 = 0
      DO 113 M=1,LJ,JUMP
         M1 = M1+1
         DSUM(M1) = X(M,1)-X(M,N)
         X(M,1) = X(M,1)+X(M,N)
  113 CONTINUE
      DO 104 K=2,NS2
         M1 = 0
         DO 114 M=1,LJ,JUMP
         M1 = M1+1
         KC = NP1-K
         T1 = X(M,K)+X(M,KC)
         T2 = X(M,K)-X(M,KC)
         DSUM(M1) = DSUM(M1)+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(M,K) = T1-T2
         X(M,KC) = T1+T2
  114    CONTINUE
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
         DO 123 M=1,LJ,JUMP
         X(M,NS2+1) = X(M,NS2+1)+X(M,NS2+1)
  123    CONTINUE
 124  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(NM1-1)  + 1
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) + 4
      LNWK = LOT*NM1
C
      CALL RFFTMF(LOT,JUMP,NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,
     1            LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSTF1',-5)
        GO TO 200
      ENDIF
C
      SNM1 = 1./FLOAT(NM1)
      DO 10 M=1,LOT
      DSUM(M) = SNM1*DSUM(M)
   10 CONTINUE
      IF(MOD(NM1,2) .NE. 0) GO TO 30
      DO 20 M=1,LJ,JUMP
      X(M,NM1) = X(M,NM1)+X(M,NM1)
   20 CONTINUE
 30   DO 105 I=3,N,2
         M1 = 0
         DO 115 M=1,LJ,JUMP
            M1 = M1+1
            XI = .5*X(M,I)
            X(M,I) = .5*X(M,I-1)
            X(M,I-1) = DSUM(M1)
            DSUM(M1) = DSUM(M1)+XI
  115 CONTINUE
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 117
      M1 = 0
      DO 116 M=1,LJ,JUMP
         M1 = M1+1
         X(M,N) = DSUM(M1)
  116 CONTINUE
 117  DO 118 M=1,LJ,JUMP
      X(M,1) = .5*X(M,1)
      X(M,N) = .5*X(M,N)
 118  CONTINUE
C
 200  CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADB2 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1)
      REAL       CC(IN1,IDO,2,L1), CH(IN2,IDO,L1,2), WA1(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      DO 101 K=1,L1
          M2 = M2S
          DO 1001 M1=1,M1D,IM1
          M2 = M2+IM2
         CH(M2,1,K,1) = CC(M1,1,1,K)+CC(M1,IDO,2,K)
         CH(M2,1,K,2) = CC(M1,1,1,K)-CC(M1,IDO,2,K)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
               M2 = M2S
               DO 1002 M1=1,M1D,IM1
               M2 = M2+IM2
        CH(M2,I-1,K,1) = CC(M1,I-1,1,K)+CC(M1,IC-1,2,K)
        CH(M2,I,K,1) = CC(M1,I,1,K)-CC(M1,IC,2,K)
        CH(M2,I-1,K,2) = WA1(I-2)*(CC(M1,I-1,1,K)-CC(M1,IC-1,2,K))
     1  -WA1(I-1)*(CC(M1,I,1,K)+CC(M1,IC,2,K))
        CH(M2,I,K,2) = WA1(I-2)*(CC(M1,I,1,K)+CC(M1,IC,2,K))+WA1(I-1)
     1  *(CC(M1,I-1,1,K)-CC(M1,IC-1,2,K))
 1002          CONTINUE
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
          M2 = M2S
          DO 1003 M1=1,M1D,IM1
          M2 = M2+IM2
         CH(M2,IDO,K,1) = CC(M1,IDO,1,K)+CC(M1,IDO,1,K)
         CH(M2,IDO,K,2) = -(CC(M1,1,2,K)+CC(M1,1,2,K))
 1003     CONTINUE
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADB3 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1,WA2)
      REAL       CC(IN1,IDO,3,L1)    ,CH(IN2,IDO,L1,3),
     1           WA1(IDO)   ,WA2(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      ARG=2.*4.*ATAN(1.0)/3.
      TAUR=COS(ARG)
      TAUI=SIN(ARG)
      DO 101 K=1,L1
          M2 = M2S
          DO 1001 M1=1,M1D,IM1
          M2 = M2+IM2
         CH(M2,1,K,1) = CC(M1,1,1,K)+2.*CC(M1,IDO,2,K)
         CH(M2,1,K,2) = CC(M1,1,1,K)+(2.*TAUR)*CC(M1,IDO,2,K)
     1   -(2.*TAUI)*CC(M1,1,3,K)
         CH(M2,1,K,3) = CC(M1,1,1,K)+(2.*TAUR)*CC(M1,IDO,2,K)
     1   +2.*TAUI*CC(M1,1,3,K)
 1001     CONTINUE
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
               M2 = M2S
               DO 1002 M1=1,M1D,IM1
               M2 = M2+IM2
        CH(M2,I-1,K,1) = CC(M1,I-1,1,K)+(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
        CH(M2,I,K,1) = CC(M1,I,1,K)+(CC(M1,I,3,K)-CC(M1,IC,2,K))
        CH(M2,I-1,K,2) = WA1(I-2)*
     1 ((CC(M1,I-1,1,K)+TAUR*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K)))-
     * (TAUI*(CC(M1,I,3,K)+CC(M1,IC,2,K))))
     2                   -WA1(I-1)*
     3 ((CC(M1,I,1,K)+TAUR*(CC(M1,I,3,K)-CC(M1,IC,2,K)))+
     * (TAUI*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))))
            CH(M2,I,K,2) = WA1(I-2)*
     4 ((CC(M1,I,1,K)+TAUR*(CC(M1,I,3,K)-CC(M1,IC,2,K)))+
     8 (TAUI*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))))
     5                  +WA1(I-1)*
     6 ((CC(M1,I-1,1,K)+TAUR*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K)))-
     8 (TAUI*(CC(M1,I,3,K)+CC(M1,IC,2,K))))
              CH(M2,I-1,K,3) = WA2(I-2)*
     7 ((CC(M1,I-1,1,K)+TAUR*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K)))+
     8 (TAUI*(CC(M1,I,3,K)+CC(M1,IC,2,K))))
     8   -WA2(I-1)*
     9 ((CC(M1,I,1,K)+TAUR*(CC(M1,I,3,K)-CC(M1,IC,2,K)))-
     8 (TAUI*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))))
            CH(M2,I,K,3) = WA2(I-2)*
     1 ((CC(M1,I,1,K)+TAUR*(CC(M1,I,3,K)-CC(M1,IC,2,K)))-
     8 (TAUI*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))))
     2                 +WA2(I-1)*
     3 ((CC(M1,I-1,1,K)+TAUR*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K)))+
     8 (TAUI*(CC(M1,I,3,K)+CC(M1,IC,2,K))))
 1002          CONTINUE
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADB4 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1,WA2,WA3)
      REAL       CC(IN1,IDO,4,L1)  ,CH(IN2,IDO,L1,4)    ,
     1           WA1(IDO)  ,        WA2(IDO)  ,       WA3(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      SQRT2=SQRT(2.)
      DO 101 K=1,L1
          M2 = M2S
          DO 1001 M1=1,M1D,IM1
          M2 = M2+IM2
         CH(M2,1,K,3) = (CC(M1,1,1,K)+CC(M1,IDO,4,K))
     1   -(CC(M1,IDO,2,K)+CC(M1,IDO,2,K))
         CH(M2,1,K,1) = (CC(M1,1,1,K)+CC(M1,IDO,4,K))
     1   +(CC(M1,IDO,2,K)+CC(M1,IDO,2,K))
         CH(M2,1,K,4) = (CC(M1,1,1,K)-CC(M1,IDO,4,K))
     1   +(CC(M1,1,3,K)+CC(M1,1,3,K))
         CH(M2,1,K,2) = (CC(M1,1,1,K)-CC(M1,IDO,4,K))
     1   -(CC(M1,1,3,K)+CC(M1,1,3,K))
 1001     CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
               M2 = M2S
               DO 1002 M1=1,M1D,IM1
               M2 = M2+IM2
        CH(M2,I-1,K,1) = (CC(M1,I-1,1,K)+CC(M1,IC-1,4,K))
     1  +(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
        CH(M2,I,K,1) = (CC(M1,I,1,K)-CC(M1,IC,4,K))
     1  +(CC(M1,I,3,K)-CC(M1,IC,2,K))
        CH(M2,I-1,K,2)=WA1(I-2)*((CC(M1,I-1,1,K)-CC(M1,IC-1,4,K))
     1  -(CC(M1,I,3,K)+CC(M1,IC,2,K)))-WA1(I-1)
     1  *((CC(M1,I,1,K)+CC(M1,IC,4,K))+(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K)))
        CH(M2,I,K,2)=WA1(I-2)*((CC(M1,I,1,K)+CC(M1,IC,4,K))
     1  +(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K)))+WA1(I-1)
     1  *((CC(M1,I-1,1,K)-CC(M1,IC-1,4,K))-(CC(M1,I,3,K)+CC(M1,IC,2,K)))
        CH(M2,I-1,K,3)=WA2(I-2)*((CC(M1,I-1,1,K)+CC(M1,IC-1,4,K))
     1  -(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K)))-WA2(I-1)
     1  *((CC(M1,I,1,K)-CC(M1,IC,4,K))-(CC(M1,I,3,K)-CC(M1,IC,2,K)))
        CH(M2,I,K,3)=WA2(I-2)*((CC(M1,I,1,K)-CC(M1,IC,4,K))
     1  -(CC(M1,I,3,K)-CC(M1,IC,2,K)))+WA2(I-1)
     1  *((CC(M1,I-1,1,K)+CC(M1,IC-1,4,K))-(CC(M1,I-1,3,K)
     1  +CC(M1,IC-1,2,K)))
        CH(M2,I-1,K,4)=WA3(I-2)*((CC(M1,I-1,1,K)-CC(M1,IC-1,4,K))
     1  +(CC(M1,I,3,K)+CC(M1,IC,2,K)))-WA3(I-1)
     1 *((CC(M1,I,1,K)+CC(M1,IC,4,K))-(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K)))
        CH(M2,I,K,4)=WA3(I-2)*((CC(M1,I,1,K)+CC(M1,IC,4,K))
     1  -(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K)))+WA3(I-1)
     1  *((CC(M1,I-1,1,K)-CC(M1,IC-1,4,K))+(CC(M1,I,3,K)+CC(M1,IC,2,K)))
 1002          CONTINUE
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
               M2 = M2S
               DO 1003 M1=1,M1D,IM1
               M2 = M2+IM2
         CH(M2,IDO,K,1) = (CC(M1,IDO,1,K)+CC(M1,IDO,3,K))
     1   +(CC(M1,IDO,1,K)+CC(M1,IDO,3,K))
         CH(M2,IDO,K,2) = SQRT2*((CC(M1,IDO,1,K)-CC(M1,IDO,3,K))
     1   -(CC(M1,1,2,K)+CC(M1,1,4,K)))
         CH(M2,IDO,K,3) = (CC(M1,1,4,K)-CC(M1,1,2,K))
     1   +(CC(M1,1,4,K)-CC(M1,1,2,K))
         CH(M2,IDO,K,4) = -SQRT2*((CC(M1,IDO,1,K)-CC(M1,IDO,3,K))
     1   +(CC(M1,1,2,K)+CC(M1,1,4,K)))
 1003          CONTINUE
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADB5 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,
     1       WA1,WA2,WA3,WA4)
      REAL   CC(IN1,IDO,5,L1)    ,CH(IN2,IDO,L1,5),
     1       WA1(IDO)     ,WA2(IDO)     ,WA3(IDO)     ,WA4(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      ARG=2.*4.*ATAN(1.0)/5.
      TR11=COS(ARG)
      TI11=SIN(ARG)
      TR12=COS(2.*ARG)
      TI12=SIN(2.*ARG)
      DO 101 K=1,L1
      M2 = M2S
      DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,K,1) = CC(M1,1,1,K)+2.*CC(M1,IDO,2,K)+2.*CC(M1,IDO,4,K)
         CH(M2,1,K,2) = (CC(M1,1,1,K)+TR11*2.*CC(M1,IDO,2,K)
     1   +TR12*2.*CC(M1,IDO,4,K))-(TI11*2.*CC(M1,1,3,K)
     1   +TI12*2.*CC(M1,1,5,K))
         CH(M2,1,K,3) = (CC(M1,1,1,K)+TR12*2.*CC(M1,IDO,2,K)
     1   +TR11*2.*CC(M1,IDO,4,K))-(TI12*2.*CC(M1,1,3,K)
     1   -TI11*2.*CC(M1,1,5,K))
         CH(M2,1,K,4) = (CC(M1,1,1,K)+TR12*2.*CC(M1,IDO,2,K)
     1   +TR11*2.*CC(M1,IDO,4,K))+(TI12*2.*CC(M1,1,3,K)
     1   -TI11*2.*CC(M1,1,5,K))
         CH(M2,1,K,5) = (CC(M1,1,1,K)+TR11*2.*CC(M1,IDO,2,K)
     1   +TR12*2.*CC(M1,IDO,4,K))+(TI11*2.*CC(M1,1,3,K)
     1   +TI12*2.*CC(M1,1,5,K))
 1001          CONTINUE
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
      DO 1002 M1=1,M1D,IM1
        M2 = M2+IM2
        CH(M2,I-1,K,1) = CC(M1,I-1,1,K)+(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K))
        CH(M2,I,K,1) = CC(M1,I,1,K)+(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +(CC(M1,I,5,K)-CC(M1,IC,4,K))
        CH(M2,I-1,K,2) = WA1(I-2)*((CC(M1,I-1,1,K)+TR11*
     1  (CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))+TR12
     1  *(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA1(I-1)*((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))+(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,2) = WA1(I-2)*((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)
     1  -CC(M1,IC,2,K))+TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI11*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))+TI12
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))+WA1(I-1)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)
     1  +CC(M1,IC-1,2,K))+TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))
     1  -(TI11*(CC(M1,I,3,K)+CC(M1,IC,2,K))+TI12
     1  *(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,3) = WA2(I-2)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1 -WA2(I-1)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,3) = WA2(I-2)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  +(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA2(I-1)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))-(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,4) = WA3(I-2)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA3(I-1)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  -(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,4) = WA3(I-2)
     1 *((CC(M1,I,1,K)+TR12*(CC(M1,I,3,K)-
     1  CC(M1,IC,2,K))+TR11*(CC(M1,I,5,K)-CC(M1,IC,4,K)))
     1  -(TI12*(CC(M1,I-1,3,K)-CC(M1,IC-1,2,K))-TI11
     1  *(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA3(I-1)
     1  *((CC(M1,I-1,1,K)+TR12*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR11*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI12*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))-TI11*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
        CH(M2,I-1,K,5) = WA4(I-2)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
     1  -WA4(I-1)
     1  *((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))-(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
        CH(M2,I,K,5) = WA4(I-2)
     1  *((CC(M1,I,1,K)+TR11*(CC(M1,I,3,K)-CC(M1,IC,2,K))
     1  +TR12*(CC(M1,I,5,K)-CC(M1,IC,4,K)))-(TI11*(CC(M1,I-1,3,K)
     1  -CC(M1,IC-1,2,K))+TI12*(CC(M1,I-1,5,K)-CC(M1,IC-1,4,K))))
     1  +WA4(I-1)
     1  *((CC(M1,I-1,1,K)+TR11*(CC(M1,I-1,3,K)+CC(M1,IC-1,2,K))
     1  +TR12*(CC(M1,I-1,5,K)+CC(M1,IC-1,4,K)))+(TI11*(CC(M1,I,3,K)
     1  +CC(M1,IC,2,K))+TI12*(CC(M1,I,5,K)+CC(M1,IC,4,K))))
 1002      CONTINUE
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADBG (M,IDO,IP,L1,IDL1,CC,C1,C2,IM1,IN1,
     1          CH,CH2,IM2,IN2,WA)
      REAL      CH(IN2,IDO,L1,IP)    ,CC(IN1,IDO,IP,L1) ,
     1          C1(IN1,IDO,L1,IP)    ,C2(IN1,IDL1,IP),
     2          CH2(IN2,IDL1,IP)     ,WA(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      TPI=2.*4.*ATAN(1.0)
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
            M2 = M2S
            DO 1001 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I,K,1) = CC(M1,I,1,K)
 1001       CONTINUE
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 I=1,IDO
         DO 104 K=1,L1
            M2 = M2S
            DO 1004 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I,K,1) = CC(M1,I,1,K)
 1004       CONTINUE
  104    CONTINUE
  105 CONTINUE
  106 DO 108 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 107 K=1,L1
            M2 = M2S
            DO 1007 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,1,K,J) = CC(M1,IDO,J2-2,K)+CC(M1,IDO,J2-2,K)
            CH(M2,1,K,JC) = CC(M1,1,J2-1,K)+CC(M1,1,J2-1,K)
 1007       CONTINUE
  107    CONTINUE
  108 CONTINUE
      IF (IDO .EQ. 1) GO TO 116
      IF (NBD .LT. L1) GO TO 112
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 110 K=1,L1
            DO 109 I=3,IDO,2
               IC = IDP2-I
               M2 = M2S
               DO 1009 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = CC(M1,I-1,2*J-1,K)+CC(M1,IC-1,2*J-2,K)
               CH(M2,I-1,K,JC) = CC(M1,I-1,2*J-1,K)-CC(M1,IC-1,2*J-2,K)
               CH(M2,I,K,J) = CC(M1,I,2*J-1,K)-CC(M1,IC,2*J-2,K)
               CH(M2,I,K,JC) = CC(M1,I,2*J-1,K)+CC(M1,IC,2*J-2,K)
 1009          CONTINUE
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      GO TO 116
  112 DO 115 J=2,IPPH
         JC = IPP2-J
         DO 114 I=3,IDO,2
            IC = IDP2-I
            DO 113 K=1,L1
               M2 = M2S
               DO 1013 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = CC(M1,I-1,2*J-1,K)+CC(M1,IC-1,2*J-2,K)
               CH(M2,I-1,K,JC) = CC(M1,I-1,2*J-1,K)-CC(M1,IC-1,2*J-2,K)
               CH(M2,I,K,J) = CC(M1,I,2*J-1,K)-CC(M1,IC,2*J-2,K)
               CH(M2,I,K,JC) = CC(M1,I,2*J-1,K)+CC(M1,IC,2*J-2,K)
 1013          CONTINUE
  113       CONTINUE
  114    CONTINUE
  115 CONTINUE
  116 AR1 = 1.
      AI1 = 0.
      DO 120 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 117 IK=1,IDL1
            M2 = M2S
            DO 1017 M1=1,M1D,IM1
            M2 = M2+IM2
            C2(M1,IK,L) = CH2(M2,IK,1)+AR1*CH2(M2,IK,2)
            C2(M1,IK,LC) = AI1*CH2(M2,IK,IP)
 1017       CONTINUE
  117    CONTINUE
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
               M2 = M2S
               DO 1018 M1=1,M1D,IM1
               M2 = M2+IM2
               C2(M1,IK,L) = C2(M1,IK,L)+AR2*CH2(M2,IK,J)
               C2(M1,IK,LC) = C2(M1,IK,LC)+AI2*CH2(M2,IK,JC)
 1018          CONTINUE
  118       CONTINUE
  119    CONTINUE
  120 CONTINUE
      DO 122 J=2,IPPH
         DO 121 IK=1,IDL1
            M2 = M2S
            DO 1021 M1=1,M1D,IM1
            M2 = M2+IM2
            CH2(M2,IK,1) = CH2(M2,IK,1)+CH2(M2,IK,J)
 1021       CONTINUE
  121    CONTINUE
  122 CONTINUE
      DO 124 J=2,IPPH
         JC = IPP2-J
         DO 123 K=1,L1
            M2 = M2S
            DO 1023 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,1,K,J) = C1(M1,1,K,J)-C1(M1,1,K,JC)
            CH(M2,1,K,JC) = C1(M1,1,K,J)+C1(M1,1,K,JC)
 1023       CONTINUE
  123    CONTINUE
  124 CONTINUE
      IF (IDO .EQ. 1) GO TO 132
      IF (NBD .LT. L1) GO TO 128
      DO 127 J=2,IPPH
         JC = IPP2-J
         DO 126 K=1,L1
            DO 125 I=3,IDO,2
               M2 = M2S
               DO 1025 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = C1(M1,I-1,K,J)-C1(M1,I,K,JC)
               CH(M2,I-1,K,JC) = C1(M1,I-1,K,J)+C1(M1,I,K,JC)
               CH(M2,I,K,J) = C1(M1,I,K,J)+C1(M1,I-1,K,JC)
               CH(M2,I,K,JC) = C1(M1,I,K,J)-C1(M1,I-1,K,JC)
 1025          CONTINUE
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      GO TO 132
  128 DO 131 J=2,IPPH
         JC = IPP2-J
         DO 130 I=3,IDO,2
            DO 129 K=1,L1
               M2 = M2S
               DO 1029 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = C1(M1,I-1,K,J)-C1(M1,I,K,JC)
               CH(M2,I-1,K,JC) = C1(M1,I-1,K,J)+C1(M1,I,K,JC)
               CH(M2,I,K,J) = C1(M1,I,K,J)+C1(M1,I-1,K,JC)
               CH(M2,I,K,JC) = C1(M1,I,K,J)-C1(M1,I-1,K,JC)
 1029          CONTINUE
  129       CONTINUE
  130    CONTINUE
  131 CONTINUE
  132 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 133 IK=1,IDL1
         M2 = M2S
         DO 1033 M1=1,M1D,IM1
         M2 = M2+IM2
         C2(M1,IK,1) = CH2(M2,IK,1)
 1033    CONTINUE
  133 CONTINUE
      DO 135 J=2,IP
         DO 134 K=1,L1
            M2 = M2S
            DO 1034 M1=1,M1D,IM1
            M2 = M2+IM2
            C1(M1,1,K,J) = CH(M2,1,K,J)
 1034       CONTINUE
  134    CONTINUE
  135 CONTINUE
      IF (NBD .GT. L1) GO TO 139
      IS = -IDO
      DO 138 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 137 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 136 K=1,L1
               M2 = M2S
               DO 1036 M1=1,M1D,IM1
               M2 = M2+IM2
               C1(M1,I-1,K,J) = WA(IDIJ-1)*CH(M2,I-1,K,J)-WA(IDIJ)*
     1          CH(M2,I,K,J)
               C1(M1,I,K,J) = WA(IDIJ-1)*CH(M2,I,K,J)+WA(IDIJ)*
     1          CH(M2,I-1,K,J)
 1036          CONTINUE
  136       CONTINUE
  137    CONTINUE
  138 CONTINUE
      GO TO 143
  139 IS = -IDO
      DO 142 J=2,IP
         IS = IS+IDO
         DO 141 K=1,L1
            IDIJ = IS
            DO 140 I=3,IDO,2
               IDIJ = IDIJ+2
               M2 = M2S
               DO 1040 M1=1,M1D,IM1
               M2 = M2+IM2
               C1(M1,I-1,K,J) = WA(IDIJ-1)*CH(M2,I-1,K,J)-WA(IDIJ)*
     1          CH(M2,I,K,J)
               C1(M1,I,K,J) = WA(IDIJ-1)*CH(M2,I,K,J)+WA(IDIJ)*
     1          CH(M2,I-1,K,J)
 1040          CONTINUE
  140       CONTINUE
  141    CONTINUE
  142 CONTINUE
  143 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADF2 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1)
      REAL       CH(IN2,IDO,2,L1) ,CC(IN1,IDO,L1,2) , WA1(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      DO 101 K=1,L1
         M2 = M2S
         DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,1,K) = CC(M1,1,K,1)+CC(M1,1,K,2)
         CH(M2,IDO,2,K) = CC(M1,1,K,1)-CC(M1,1,K,2)
 1001    CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
            DO 1003 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I,1,K) = CC(M1,I,K,1)+(WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))
            CH(M2,IC,2,K) = (WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*
     1       CC(M1,I-1,K,2))-CC(M1,I,K,1)
            CH(M2,I-1,1,K) = CC(M1,I-1,K,1)+(WA1(I-2)*CC(M1,I-1,K,2)+
     1       WA1(I-1)*CC(M1,I,K,2))
            CH(M2,IC-1,2,K) = CC(M1,I-1,K,1)-(WA1(I-2)*CC(M1,I-1,K,2)+
     1       WA1(I-1)*CC(M1,I,K,2))
 1003       CONTINUE
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         M2 = M2S
         DO 1006 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,2,K) = -CC(M1,IDO,K,2)
         CH(M2,IDO,1,K) = CC(M1,IDO,K,1)
 1006    CONTINUE
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADF3 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1,WA2)
      REAL       CH(IN2,IDO,3,L1)  ,CC(IN1,IDO,L1,3)     ,
     1                WA1(IDO)     ,WA2(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      ARG=2.*4.*ATAN(1.0)/3.
      TAUR=COS(ARG)
      TAUI=SIN(ARG)
      DO 101 K=1,L1
         M2 = M2S
         DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,1,K) = CC(M1,1,K,1)+(CC(M1,1,K,2)+CC(M1,1,K,3))
         CH(M2,1,3,K) = TAUI*(CC(M1,1,K,3)-CC(M1,1,K,2))
         CH(M2,IDO,2,K) = CC(M1,1,K,1)+TAUR*
     1      (CC(M1,1,K,2)+CC(M1,1,K,3))
 1001    CONTINUE
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
            DO 1002 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I-1,1,K) = CC(M1,I-1,K,1)+((WA1(I-2)*CC(M1,I-1,K,2)+
     1       WA1(I-1)*CC(M1,I,K,2))+(WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3)))
            CH(M2,I,1,K) = CC(M1,I,K,1)+((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3)))
            CH(M2,I-1,3,K) = (CC(M1,I-1,K,1)+TAUR*((WA1(I-2)*
     1       CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2))+(WA2(I-2)*
     1       CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3))))+(TAUI*((WA1(I-2)*
     1       CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2))-(WA2(I-2)*
     1       CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3))))
            CH(M2,IC-1,2,K) = (CC(M1,I-1,K,1)+TAUR*((WA1(I-2)*
     1       CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2))+(WA2(I-2)*
     1       CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3))))-(TAUI*((WA1(I-2)*
     1       CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2))-(WA2(I-2)*
     1       CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3))))
            CH(M2,I,3,K) = (CC(M1,I,K,1)+TAUR*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))))+(TAUI*((WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2))))
            CH(M2,IC,2,K) = (TAUI*((WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2))))-(CC(M1,I,K,1)+TAUR*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))))
 1002       CONTINUE
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADF4 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,WA1,WA2,WA3)
      REAL       CC(IN1,IDO,L1,4)   ,CH(IN2,IDO,4,L1)     ,
     1           WA1(IDO)           ,WA2(IDO)     ,WA3(IDO)
C
      HSQT2=SQRT(2.)/2.
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      DO 101 K=1,L1
         M2 = M2S
         DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,1,K) = (CC(M1,1,K,2)+CC(M1,1,K,4))
     1      +(CC(M1,1,K,1)+CC(M1,1,K,3))
         CH(M2,IDO,4,K) = (CC(M1,1,K,1)+CC(M1,1,K,3))
     1      -(CC(M1,1,K,2)+CC(M1,1,K,4))
         CH(M2,IDO,2,K) = CC(M1,1,K,1)-CC(M1,1,K,3)
         CH(M2,1,3,K) = CC(M1,1,K,4)-CC(M1,1,K,2)
 1001    CONTINUE
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
            DO 1003 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I-1,1,K) = ((WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2))+(WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4)))+(CC(M1,I-1,K,1)+(WA2(I-2)*CC(M1,I-1,K,3)+
     1       WA2(I-1)*CC(M1,I,K,3)))
            CH(M2,IC-1,4,K) = (CC(M1,I-1,K,1)+(WA2(I-2)*CC(M1,I-1,K,3)+
     1       WA2(I-1)*CC(M1,I,K,3)))-((WA1(I-2)*CC(M1,I-1,K,2)+
     1       WA1(I-1)*CC(M1,I,K,2))+(WA3(I-2)*CC(M1,I-1,K,4)+
     1       WA3(I-1)*CC(M1,I,K,4)))
            CH(M2,I,1,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*
     1       CC(M1,I-1,K,2))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4)))+(CC(M1,I,K,1)+(WA2(I-2)*CC(M1,I,K,3)-
     1       WA2(I-1)*CC(M1,I-1,K,3)))
            CH(M2,IC,4,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*
     1       CC(M1,I-1,K,2))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4)))-(CC(M1,I,K,1)+(WA2(I-2)*CC(M1,I,K,3)-
     1       WA2(I-1)*CC(M1,I-1,K,3)))
            CH(M2,I-1,3,K) = ((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*
     1       CC(M1,I-1,K,2))-(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4)))+(CC(M1,I-1,K,1)-(WA2(I-2)*CC(M1,I-1,K,3)+
     1       WA2(I-1)*CC(M1,I,K,3)))
            CH(M2,IC-1,2,K) = (CC(M1,I-1,K,1)-(WA2(I-2)*CC(M1,I-1,K,3)+
     1       WA2(I-1)*CC(M1,I,K,3)))-((WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*
     1       CC(M1,I-1,K,2))-(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4)))
            CH(M2,I,3,K) = ((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))+(CC(M1,I,K,1)-(WA2(I-2)*CC(M1,I,K,3)-
     1       WA2(I-1)*CC(M1,I-1,K,3)))
            CH(M2,IC,2,K) = ((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))-(CC(M1,I,K,1)-(WA2(I-2)*CC(M1,I,K,3)-
     1       WA2(I-1)*CC(M1,I-1,K,3)))
 1003       CONTINUE
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         M2 = M2S
         DO 1006 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,IDO,1,K) = (HSQT2*(CC(M1,IDO,K,2)-CC(M1,IDO,K,4)))+
     1       CC(M1,IDO,K,1)
            CH(M2,IDO,3,K) = CC(M1,IDO,K,1)-(HSQT2*(CC(M1,IDO,K,2)-
     1       CC(M1,IDO,K,4)))
            CH(M2,1,2,K) = (-HSQT2*(CC(M1,IDO,K,2)+CC(M1,IDO,K,4)))-
     1       CC(M1,IDO,K,3)
            CH(M2,1,4,K) = (-HSQT2*(CC(M1,IDO,K,2)+CC(M1,IDO,K,4)))+
     1       CC(M1,IDO,K,3)
 1006    CONTINUE
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADF5 (M,IDO,L1,CC,IM1,IN1,CH,IM2,IN2,
     1                   WA1,WA2,WA3,WA4)
      REAL       CC(IN1,IDO,L1,5)    ,CH(IN2,IDO,5,L1)     ,
     1           WA1(IDO)     ,WA2(IDO)     ,WA3(IDO)     ,WA4(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      ARG=2.*4.*ATAN(1.0)/5.
      TR11=COS(ARG)
      TI11=SIN(ARG)
      TR12=COS(2.*ARG)
      TI12=SIN(2.*ARG)
      DO 101 K=1,L1
         M2 = M2S
         DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH(M2,1,1,K) = CC(M1,1,K,1)+(CC(M1,1,K,5)+CC(M1,1,K,2))+
     1    (CC(M1,1,K,4)+CC(M1,1,K,3))
         CH(M2,IDO,2,K) = CC(M1,1,K,1)+TR11*(CC(M1,1,K,5)+CC(M1,1,K,2))+
     1    TR12*(CC(M1,1,K,4)+CC(M1,1,K,3))
         CH(M2,1,3,K) = TI11*(CC(M1,1,K,5)-CC(M1,1,K,2))+TI12*
     1    (CC(M1,1,K,4)-CC(M1,1,K,3))
         CH(M2,IDO,4,K) = CC(M1,1,K,1)+TR12*(CC(M1,1,K,5)+CC(M1,1,K,2))+
     1    TR11*(CC(M1,1,K,4)+CC(M1,1,K,3))
         CH(M2,1,5,K) = TI12*(CC(M1,1,K,5)-CC(M1,1,K,2))-TI11*
     1    (CC(M1,1,K,4)-CC(M1,1,K,3))
 1001    CONTINUE
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            M2 = M2S
            DO 1002 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,I-1,1,K) = CC(M1,I-1,K,1)+((WA1(I-2)*CC(M1,I-1,K,2)+
     1       WA1(I-1)*CC(M1,I,K,2))+(WA4(I-2)*CC(M1,I-1,K,5)+WA4(I-1)*
     1       CC(M1,I,K,5)))+((WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))+(WA3(I-2)*CC(M1,I-1,K,4)+
     1       WA3(I-1)*CC(M1,I,K,4)))
            CH(M2,I,1,K) = CC(M1,I,K,1)+((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*
     1       CC(M1,I-1,K,5)))+((WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4)))
            CH(M2,I-1,3,K) = CC(M1,I-1,K,1)+TR11*
     1      ( WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2)
     1       +WA4(I-2)*CC(M1,I-1,K,5)+WA4(I-1)*CC(M1,I,K,5))+TR12*
     1      ( WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3)
     1       +WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*CC(M1,I,K,4))+TI11*
     1      ( WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2)
     1       -(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*CC(M1,I-1,K,5)))+TI12*
     1      ( WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3)
     1       -(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*CC(M1,I-1,K,4)))
            CH(M2,IC-1,2,K) = CC(M1,I-1,K,1)+TR11*
     1      ( WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2)
     1       +WA4(I-2)*CC(M1,I-1,K,5)+WA4(I-1)*CC(M1,I,K,5))+TR12*
     1     ( WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3)
     1      +WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*CC(M1,I,K,4))-(TI11*
     1      ( WA1(I-2)*CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2)
     1       -(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*CC(M1,I-1,K,5)))+TI12*
     1      ( WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3)
     1       -(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*CC(M1,I-1,K,4))))
            CH(M2,I,3,K) = (CC(M1,I,K,1)+TR11*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*
     1       CC(M1,I-1,K,5)))+TR12*((WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4))))+(TI11*((WA4(I-2)*CC(M1,I-1,K,5)+
     1       WA4(I-1)*CC(M1,I,K,5))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))+TI12*((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))))
            CH(M2,IC,2,K) = (TI11*((WA4(I-2)*CC(M1,I-1,K,5)+WA4(I-1)*
     1       CC(M1,I,K,5))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))+TI12*((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))))-(CC(M1,I,K,1)+TR11*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*
     1       CC(M1,I-1,K,5)))+TR12*((WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4))))
            CH(M2,I-1,5,K) = (CC(M1,I-1,K,1)+TR12*((WA1(I-2)*
     1       CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2))+(WA4(I-2)*
     1       CC(M1,I-1,K,5)+WA4(I-1)*CC(M1,I,K,5)))+TR11*((WA2(I-2)*
     1       CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3))+(WA3(I-2)*
     1       CC(M1,I-1,K,4)+WA3(I-1)*CC(M1,I,K,4))))+(TI12*((WA1(I-2)*
     1       CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2))-(WA4(I-2)*
     1       CC(M1,I,K,5)-WA4(I-1)*CC(M1,I-1,K,5)))-TI11*((WA2(I-2)*
     1       CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3))-(WA3(I-2)*
     1       CC(M1,I,K,4)-WA3(I-1)*CC(M1,I-1,K,4))))
            CH(M2,IC-1,4,K) = (CC(M1,I-1,K,1)+TR12*((WA1(I-2)*
     1       CC(M1,I-1,K,2)+WA1(I-1)*CC(M1,I,K,2))+(WA4(I-2)*
     1       CC(M1,I-1,K,5)+WA4(I-1)*CC(M1,I,K,5)))+TR11*((WA2(I-2)*
     1       CC(M1,I-1,K,3)+WA2(I-1)*CC(M1,I,K,3))+(WA3(I-2)*
     1       CC(M1,I-1,K,4)+WA3(I-1)*CC(M1,I,K,4))))-(TI12*((WA1(I-2)*
     1       CC(M1,I,K,2)-WA1(I-1)*CC(M1,I-1,K,2))-(WA4(I-2)*
     1       CC(M1,I,K,5)-WA4(I-1)*CC(M1,I-1,K,5)))-TI11*((WA2(I-2)*
     1       CC(M1,I,K,3)-WA2(I-1)*CC(M1,I-1,K,3))-(WA3(I-2)*
     1       CC(M1,I,K,4)-WA3(I-1)*CC(M1,I-1,K,4))))
            CH(M2,I,5,K) = (CC(M1,I,K,1)+TR12*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*
     1       CC(M1,I-1,K,5)))+TR11*((WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4))))+(TI12*((WA4(I-2)*CC(M1,I-1,K,5)+
     1       WA4(I-1)*CC(M1,I,K,5))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))-TI11*((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))))
            CH(M2,IC,4,K) = (TI12*((WA4(I-2)*CC(M1,I-1,K,5)+WA4(I-1)*
     1       CC(M1,I,K,5))-(WA1(I-2)*CC(M1,I-1,K,2)+WA1(I-1)*
     1       CC(M1,I,K,2)))-TI11*((WA3(I-2)*CC(M1,I-1,K,4)+WA3(I-1)*
     1       CC(M1,I,K,4))-(WA2(I-2)*CC(M1,I-1,K,3)+WA2(I-1)*
     1       CC(M1,I,K,3))))-(CC(M1,I,K,1)+TR12*((WA1(I-2)*CC(M1,I,K,2)-
     1       WA1(I-1)*CC(M1,I-1,K,2))+(WA4(I-2)*CC(M1,I,K,5)-WA4(I-1)*
     1       CC(M1,I-1,K,5)))+TR11*((WA2(I-2)*CC(M1,I,K,3)-WA2(I-1)*
     1       CC(M1,I-1,K,3))+(WA3(I-2)*CC(M1,I,K,4)-WA3(I-1)*
     1       CC(M1,I-1,K,4))))
 1002       CONTINUE
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRADFG (M,IDO,IP,L1,IDL1,CC,C1,C2,IM1,IN1,
     1              CH,CH2,IM2,IN2,WA)
      REAL          CH(IN2,IDO,L1,IP)   ,CC(IN1,IDO,IP,L1),
     1              C1(IN1,IDO,L1,IP)   ,C2(IN1,IDL1,IP),
     2              CH2(IN2,IDL1,IP)    ,WA(IDO)
C
      M1D = (M-1)*IM1+1
      M2S = 1-IM2
      TPI=2.*4.*ATAN(1.0)
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IF (IDO .EQ. 1) GO TO 119
      DO 101 IK=1,IDL1
         M2 = M2S
         DO 1001 M1=1,M1D,IM1
         M2 = M2+IM2
         CH2(M2,IK,1) = C2(M1,IK,1)
 1001    CONTINUE
  101 CONTINUE
      DO 103 J=2,IP
         DO 102 K=1,L1
            M2 = M2S
            DO 1002 M1=1,M1D,IM1
            M2 = M2+IM2
            CH(M2,1,K,J) = C1(M1,1,K,J)
 1002       CONTINUE
  102    CONTINUE
  103 CONTINUE
      IF (NBD .GT. L1) GO TO 107
      IS = -IDO
      DO 106 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 105 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 104 K=1,L1
               M2 = M2S
               DO 1004 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = WA(IDIJ-1)*C1(M1,I-1,K,J)+WA(IDIJ)
     1           *C1(M1,I,K,J)
               CH(M2,I,K,J) = WA(IDIJ-1)*C1(M1,I,K,J)-WA(IDIJ)
     1           *C1(M1,I-1,K,J)
 1004          CONTINUE
  104       CONTINUE
  105    CONTINUE
  106 CONTINUE
      GO TO 111
  107 IS = -IDO
      DO 110 J=2,IP
         IS = IS+IDO
         DO 109 K=1,L1
            IDIJ = IS
            DO 108 I=3,IDO,2
               IDIJ = IDIJ+2
               M2 = M2S
               DO 1008 M1=1,M1D,IM1
               M2 = M2+IM2
               CH(M2,I-1,K,J) = WA(IDIJ-1)*C1(M1,I-1,K,J)+WA(IDIJ)
     1           *C1(M1,I,K,J)
               CH(M2,I,K,J) = WA(IDIJ-1)*C1(M1,I,K,J)-WA(IDIJ)
     1           *C1(M1,I-1,K,J)
 1008          CONTINUE
  108       CONTINUE
  109    CONTINUE
  110 CONTINUE
  111 IF (NBD .LT. L1) GO TO 115
      DO 114 J=2,IPPH
         JC = IPP2-J
         DO 113 K=1,L1
            DO 112 I=3,IDO,2
               M2 = M2S
               DO 1012 M1=1,M1D,IM1
               M2 = M2+IM2
               C1(M1,I-1,K,J) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC)
               C1(M1,I-1,K,JC) = CH(M2,I,K,J)-CH(M2,I,K,JC)
               C1(M1,I,K,J) = CH(M2,I,K,J)+CH(M2,I,K,JC)
               C1(M1,I,K,JC) = CH(M2,I-1,K,JC)-CH(M2,I-1,K,J)
 1012          CONTINUE
  112       CONTINUE
  113    CONTINUE
  114 CONTINUE
      GO TO 121
  115 DO 118 J=2,IPPH
         JC = IPP2-J
         DO 117 I=3,IDO,2
            DO 116 K=1,L1
               M2 = M2S
               DO 1016 M1=1,M1D,IM1
               M2 = M2+IM2
               C1(M1,I-1,K,J) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC)
               C1(M1,I-1,K,JC) = CH(M2,I,K,J)-CH(M2,I,K,JC)
               C1(M1,I,K,J) = CH(M2,I,K,J)+CH(M2,I,K,JC)
               C1(M1,I,K,JC) = CH(M2,I-1,K,JC)-CH(M2,I-1,K,J)
 1016          CONTINUE
  116       CONTINUE
  117    CONTINUE
  118 CONTINUE
      GO TO 121
  119 DO 120 IK=1,IDL1
         M2 = M2S
         DO 1020 M1=1,M1D,IM1
         M2 = M2+IM2
         C2(M1,IK,1) = CH2(M2,IK,1)
 1020    CONTINUE
  120 CONTINUE
  121 DO 123 J=2,IPPH
         JC = IPP2-J
         DO 122 K=1,L1
            M2 = M2S
            DO 1022 M1=1,M1D,IM1
            M2 = M2+IM2
            C1(M1,1,K,J) = CH(M2,1,K,J)+CH(M2,1,K,JC)
            C1(M1,1,K,JC) = CH(M2,1,K,JC)-CH(M2,1,K,J)
 1022       CONTINUE
  122    CONTINUE
  123 CONTINUE
C
      AR1 = 1.
      AI1 = 0.
      DO 127 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 124 IK=1,IDL1
            M2 = M2S
            DO 1024 M1=1,M1D,IM1
            M2 = M2+IM2
            CH2(M2,IK,L) = C2(M1,IK,1)+AR1*C2(M1,IK,2)
            CH2(M2,IK,LC) = AI1*C2(M1,IK,IP)
 1024       CONTINUE
  124    CONTINUE
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
               M2 = M2S
               DO 1025 M1=1,M1D,IM1
               M2 = M2+IM2
               CH2(M2,IK,L) = CH2(M2,IK,L)+AR2*C2(M1,IK,J)
               CH2(M2,IK,LC) = CH2(M2,IK,LC)+AI2*C2(M1,IK,JC)
 1025          CONTINUE
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      DO 129 J=2,IPPH
         DO 128 IK=1,IDL1
            M2 = M2S
            DO 1028 M1=1,M1D,IM1
            M2 = M2+IM2
            CH2(M2,IK,1) = CH2(M2,IK,1)+C2(M1,IK,J)
 1028       CONTINUE
  128    CONTINUE
  129 CONTINUE
C
      IF (IDO .LT. L1) GO TO 132
      DO 131 K=1,L1
         DO 130 I=1,IDO
            M2 = M2S
            DO 1030 M1=1,M1D,IM1
            M2 = M2+IM2
            CC(M1,I,1,K) = CH(M2,I,K,1)
 1030       CONTINUE
  130    CONTINUE
  131 CONTINUE
      GO TO 135
  132 DO 134 I=1,IDO
         DO 133 K=1,L1
            M2 = M2S
            DO 1033 M1=1,M1D,IM1
            M2 = M2+IM2
            CC(M1,I,1,K) = CH(M2,I,K,1)
 1033       CONTINUE
  133    CONTINUE
  134 CONTINUE
  135 DO 137 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 136 K=1,L1
            M2 = M2S
            DO 1036 M1=1,M1D,IM1
            M2 = M2+IM2
            CC(M1,IDO,J2-2,K) = CH(M2,1,K,J)
            CC(M1,1,J2-1,K) = CH(M2,1,K,JC)
 1036       CONTINUE
  136    CONTINUE
  137 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IF (NBD .LT. L1) GO TO 141
      DO 140 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 139 K=1,L1
            DO 138 I=3,IDO,2
               IC = IDP2-I
               M2 = M2S
               DO 1038 M1=1,M1D,IM1
               M2 = M2+IM2
               CC(M1,I-1,J2-1,K) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC)
               CC(M1,IC-1,J2-2,K) = CH(M2,I-1,K,J)-CH(M2,I-1,K,JC)
               CC(M1,I,J2-1,K) = CH(M2,I,K,J)+CH(M2,I,K,JC)
               CC(M1,IC,J2-2,K) = CH(M2,I,K,JC)-CH(M2,I,K,J)
 1038          CONTINUE
  138       CONTINUE
  139    CONTINUE
  140 CONTINUE
      RETURN
  141 DO 144 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 143 I=3,IDO,2
            IC = IDP2-I
            DO 142 K=1,L1
               M2 = M2S
               DO 1042 M1=1,M1D,IM1
               M2 = M2+IM2
               CC(M1,I-1,J2-1,K) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC)
               CC(M1,IC-1,J2-2,K) = CH(M2,I-1,K,J)-CH(M2,I-1,K,JC)
               CC(M1,I,J2-1,K) = CH(M2,I,K,J)+CH(M2,I,K,JC)
               CC(M1,IC,J2-2,K) = CH(M2,I,K,JC)-CH(M2,I,K,J)
 1042          CONTINUE
  142       CONTINUE
  143    CONTINUE
  144 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRFTB1 (M,IM,N,IN,C,CH,WA,FAC)
      REAL       CH(M,*), C(IN,*), WA(N) ,FAC(15)
C
      NF = FAC(2)
      NA = 0
      DO 10 K1=1,NF
      IP = FAC(K1+2)
      NA = 1-NA      
      IF(IP .LE. 5) GO TO 10
      IF(K1 .EQ. NF) GO TO 10
      NA = 1-NA
   10 CONTINUE 
      HALF = .5
      HALFM = -.5
      MODN = MOD(N,2)
      NL = N-2
      IF(MODN .NE. 0) NL = N-1
      IF (NA .EQ. 0) GO TO 120
      M2 = 1-IM
      DO 117 I=1,M
      M2 = M2+IM
      CH(I,1) = C(M2,1)
      CH(I,N) = C(M2,N)
  117 CONTINUE
      DO 118 J=2,NL,2
      M2 = 1-IM
      DO 118 I=1,M
         M2 = M2+IM
	 CH(I,J) = HALF*C(M2,J)
	 CH(I,J+1) = HALFM*C(M2,J+1)
  118 CONTINUE
      GO TO 124
  120 CONTINUE
      DO 122 J=2,NL,2
      M2 = 1-IM
      DO 122 I=1,M
         M2 = M2+IM
	 C(M2,J) = HALF*C(M2,J)
	 C(M2,J+1) = HALFM*C(M2,J+1)
  122 CONTINUE
  124 L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = FAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDL1 = IDO*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
         CALL MRADB4 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL MRADB4 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
	 CALL MRADB2 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW))
         GO TO 105
  104    CALL MRADB2 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 107
	 CALL MRADB3 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2))
         GO TO 108
  107    CALL MRADB3 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 110
         CALL MRADB5 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2),
     1                  WA(IX3),WA(IX4))
         GO TO 111
  110    CALL MRADB5 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2),
     1                  WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
	 CALL MRADBG (M,IDO,IP,L1,IDL1,C,C,C,IM,IN,CH,CH,1,
     1                            M,WA(IW))
         GO TO 114
  113    CALL MRADBG (M,IDO,IP,L1,IDL1,CH,CH,CH,1,M,C,C,IM,
     1                           IN,WA(IW))
  114    IF (IDO .EQ. 1) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDO
  116 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRFTF1 (M,IM,N,IN,C,CH,WA,FAC)
      REAL       CH(M,*) ,C(IN,*)  ,WA(N)   ,FAC(15)
C
      NF = FAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1=1,NF
         KH = NF-K1
         IP = FAC(KH+3)
         L1 = L2/IP
         IDO = N/L2
         IDL1 = IDO*L1
         IW = IW-(IP-1)*IDO
         NA = 1-NA
         IF (IP .NE. 4) GO TO 102
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
	 CALL MRADF4 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
  101    CALL MRADF4 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
  102    IF (IP .NE. 2) GO TO 104
         IF (NA .NE. 0) GO TO 103
	 CALL MRADF2 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW))
         GO TO 110
  103    CALL MRADF2 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW))
         GO TO 110
  104    IF (IP .NE. 3) GO TO 106
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 105
	 CALL MRADF3 (M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2))
         GO TO 110
  105    CALL MRADF3 (M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2))
         GO TO 110
  106    IF (IP .NE. 5) GO TO 108
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 107
         CALL MRADF5(M,IDO,L1,C,IM,IN,CH,1,M,WA(IW),WA(IX2),
     1                      WA(IX3),WA(IX4))
         GO TO 110
  107    CALL MRADF5(M,IDO,L1,CH,1,M,C,IM,IN,WA(IW),WA(IX2),
     1                      WA(IX3),WA(IX4))
         GO TO 110
  108    IF (IDO .EQ. 1) NA = 1-NA
         IF (NA .NE. 0) GO TO 109
	 CALL MRADFG (M,IDO,IP,L1,IDL1,C,C,C,IM,IN,CH,CH,1,M,WA(IW))
         NA = 1
         GO TO 110
  109    CALL MRADFG (M,IDO,IP,L1,IDL1,CH,CH,CH,1,M,C,C,IM,IN,WA(IW))
         NA = 0
  110    L2 = L1
  111 CONTINUE
      SN = 1./N
      TSN = 2./N
      TSNM = -TSN
      MODN = MOD(N,2)
      NL = N-2
      IF(MODN .NE. 0) NL = N-1
      IF (NA .NE. 0) GO TO 120
      M2 = 1-IM
      DO 117 I=1,M
         M2 = M2+IM
         C(M2,1) = SN*CH(I,1)
  117 CONTINUE
      DO 118 J=2,NL,2
      M2 = 1-IM
      DO 118 I=1,M
         M2 = M2+IM
	 C(M2,J) = TSN*CH(I,J)
	 C(M2,J+1) = TSNM*CH(I,J+1)
  118 CONTINUE
      IF(MODN .NE. 0) RETURN
      M2 = 1-IM
      DO 119 I=1,M
         M2 = M2+IM
         C(M2,N) = SN*CH(I,N)
  119 CONTINUE
      RETURN
  120 M2 = 1-IM
      DO 121 I=1,M
         M2 = M2+IM
         C(M2,1) = SN*C(M2,1)
  121 CONTINUE
      DO 122 J=2,NL,2
      M2 = 1-IM
      DO 122 I=1,M
         M2 = M2+IM
	 C(M2,J) = TSN*C(M2,J)
	 C(M2,J+1) = TSNM*C(M2,J+1)
  122 CONTINUE
      IF(MODN .NE. 0) RETURN
      M2 = 1-IM
      DO 123 I=1,M
         M2 = M2+IM
         C(M2,N) = SN*C(M2,N)
  123 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MRFTI1 (N,WA,FAC)
      REAL       WA(N)      ,FAC(15)
      INTEGER    NTRYH(4)
      DOUBLE PRECISION TPI,ARGH,ARGLD,ARG
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
C
      NL = N
      NF = 0
      J = 0
  101 J = J+1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY+2
  104 NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF+1
      FAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         FAC(IB+2) = FAC(IB+1)
  106 CONTINUE
      FAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
      FAC(1) = N
      FAC(2) = NF
      TPI = 8.D0*DATAN(1.D0)
      ARGH = TPI/FLOAT(N)
      IS = 0
      NFM1 = NF-1
      L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 110 K1=1,NFM1
         IP = FAC(K1+2)
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
	       WA(I-1) = DCOS(ARG)
	       WA(I) = DSIN(ARG)
  108       CONTINUE
            IS = IS+IDO
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MSNTB1(LOT,JUMP,N,INC,X,WSAVE,DSUM,XH,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(LOT,*)
      DOUBLE PRECISION           DSUM(*)
      IER = 0
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 200,102,103
 102  SRT3S2 = SQRT(3.)/2.
      DO 112 M=1,LJ,JUMP
         XHOLD = SRT3S2*(X(M,1)+X(M,2))
         X(M,2) = SRT3S2*(X(M,1)-X(M,2))
         X(M,1) = XHOLD
  112 CONTINUE
      GO TO 200
  103 NP1 = N+1
      NS2 = N/2
      DO 104 K=1,NS2
         KC = NP1-K
         M1 = 0
         DO 114 M=1,LJ,JUMP
         M1 = M1+1
         T1 = X(M,K)-X(M,KC)
         T2 = WSAVE(K)*(X(M,K)+X(M,KC))
         XH(M1,K+1) = T1+T2
         XH(M1,KC+1) = T2-T1
  114    CONTINUE
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      M1 = 0
      DO 123 M=1,LJ,JUMP
         M1 = M1+1
         XH(M1,NS2+2) = 4.*X(M,NS2+1)
  123 CONTINUE
  124 DO 127 M=1,LOT
         XH(M,1) = 0.
  127 CONTINUE 
      LNXH = LOT-1 + LOT*(NP1-1) + 1
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) + 4
      LNWK = LOT*NP1
C
      CALL RFFTMF(LOT,1,NP1,LOT,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,
     1            LNWK,IER1)     
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MSNTB1',-5)
        GO TO 200
      ENDIF
C
      IF(MOD(NP1,2) .NE. 0) GO TO 30
      DO 20 M=1,LOT
      XH(M,NP1) = XH(M,NP1)+XH(M,NP1)
   20 CONTINUE
 30   FNP1S4 = FLOAT(NP1)/4.
      M1 = 0
      DO 125 M=1,LJ,JUMP
         M1 = M1+1
         X(M,1) = FNP1S4*XH(M1,1)
         DSUM(M1) = X(M,1)
  125 CONTINUE
      DO 105 I=3,N,2
         M1 = 0
         DO 115 M=1,LJ,JUMP
            M1 = M1+1
            X(M,I-1) = FNP1S4*XH(M1,I)
            DSUM(M1) = DSUM(M1)+FNP1S4*XH(M1,I-1)
            X(M,I) = DSUM(M1)
  115    CONTINUE
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 200
      M1 = 0
      DO 116 M=1,LJ,JUMP
         M1 = M1+1
         X(M,N) = FNP1S4*XH(M1,N+1)
  116 CONTINUE
C
  200 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MSNTF1(LOT,JUMP,N,INC,X,WSAVE,DSUM,XH,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(LOT,*)
      DOUBLE PRECISION           DSUM(*)
      IER = 0
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 101,102,103
 102  SSQRT3 = 1./SQRT(3.)
      DO 112 M=1,LJ,JUMP
         XHOLD = SSQRT3*(X(M,1)+X(M,2))
         X(M,2) = SSQRT3*(X(M,1)-X(M,2))
         X(M,1) = XHOLD
  112 CONTINUE
  101  GO TO 200
  103 NP1 = N+1
      NS2 = N/2
      DO 104 K=1,NS2
         KC = NP1-K
         M1 = 0
         DO 114 M=1,LJ,JUMP
         M1 = M1 + 1
         T1 = X(M,K)-X(M,KC)
         T2 = WSAVE(K)*(X(M,K)+X(M,KC))
         XH(M1,K+1) = T1+T2
         XH(M1,KC+1) = T2-T1
  114    CONTINUE
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      M1 = 0
      DO 123 M=1,LJ,JUMP
         M1 = M1 + 1
         XH(M1,NS2+2) = 4.*X(M,NS2+1)
  123 CONTINUE
  124 DO 127 M=1,LOT
         XH(M,1) = 0.
  127 CONTINUE 
      LNXH = LOT-1 + LOT*(NP1-1) + 1
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) + 4
      LNWK = LOT*NP1
C
      CALL RFFTMF(LOT,1,NP1,LOT,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,
     1            LNWK,IER1)     
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MSNTF1',-5)
        GO TO 200
      ENDIF
C
      IF(MOD(NP1,2) .NE. 0) GO TO 30
      DO 20 M=1,LOT
      XH(M,NP1) = XH(M,NP1)+XH(M,NP1)
   20 CONTINUE
   30 SFNP1 = 1./FLOAT(NP1)
      M1 = 0
      DO 125 M=1,LJ,JUMP
         M1 = M1+1
         X(M,1) = .5*XH(M1,1)
         DSUM(M1) = X(M,1)
  125 CONTINUE
      DO 105 I=3,N,2
         M1 = 0
         DO 115 M=1,LJ,JUMP
            M1 = M1+1
            X(M,I-1) = .5*XH(M1,I)
            DSUM(M1) = DSUM(M1)+.5*XH(M1,I-1)
            X(M,I) = DSUM(M1)
  115    CONTINUE
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 200
      M1 = 0
      DO 116 M=1,LJ,JUMP
         M1 = M1+1
         X(M,N) = .5*XH(M1,N+1)
  116 CONTINUE
  200 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F2KB (IDO,L1,CC,IN1,CH,IN2,WA1)
      REAL       CC(IN1,IDO,2,L1), CH(IN2,IDO,L1,2), WA1(IDO)
C
      DO 101 K=1,L1
         CH(1,1,K,1) = CC(1,1,1,K)+CC(1,IDO,2,K)
         CH(1,1,K,2) = CC(1,1,1,K)-CC(1,IDO,2,K)
 101  CONTINUE
      IF (IDO-2) 107,105,102
 102  IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            
            CH(1,I-1,K,1) = CC(1,I-1,1,K)+CC(1,IC-1,2,K)
            CH(1,I,K,1) = CC(1,I,1,K)-CC(1,IC,2,K)
            
            CH(1,I-1,K,2) = WA1(I-2)*(CC(1,I-1,1,K)-CC(1,IC-1,2,K))
     1           -WA1(I-1)*(CC(1,I,1,K)+CC(1,IC,2,K))
            CH(1,I,K,2) = WA1(I-2)*(CC(1,I,1,K)+CC(1,IC,2,K))+WA1(I-1)
     1           *(CC(1,I-1,1,K)-CC(1,IC-1,2,K))

 103     CONTINUE
 104  CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
 105  DO 106 K=1,L1
         CH(1,IDO,K,1) = CC(1,IDO,1,K)+CC(1,IDO,1,K)
         CH(1,IDO,K,2) = -(CC(1,1,2,K)+CC(1,1,2,K))
 106  CONTINUE
 107  RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F2KF (IDO,L1,CC,IN1,CH,IN2,WA1)
      REAL       CH(IN2,IDO,2,L1) ,CC(IN1,IDO,L1,2) , WA1(IDO)
C
      DO 101 K=1,L1
         CH(1,1,1,K) = CC(1,1,K,1)+CC(1,1,K,2)
         CH(1,IDO,2,K) = CC(1,1,K,1)-CC(1,1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CH(1,I,1,K) = CC(1,I,K,1)+(WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))
            CH(1,IC,2,K) = (WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*
     1       CC(1,I-1,K,2))-CC(1,I,K,1)
            CH(1,I-1,1,K) = CC(1,I-1,K,1)+(WA1(I-2)*CC(1,I-1,K,2)+
     1       WA1(I-1)*CC(1,I,K,2))
            CH(1,IC-1,2,K) = CC(1,I-1,K,1)-(WA1(I-2)*CC(1,I-1,K,2)+
     1       WA1(I-1)*CC(1,I,K,2))
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 DO 106 K=1,L1
         CH(1,1,2,K) = -CC(1,IDO,K,2)
         CH(1,IDO,1,K) = CC(1,IDO,K,1)
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F3KB (IDO,L1,CC,IN1,CH,IN2,WA1,WA2)
      REAL       CC(IN1,IDO,3,L1)    ,CH(IN2,IDO,L1,3),
     1           WA1(IDO)   ,WA2(IDO)
C
      ARG=2.*4.*ATAN(1.0)/3.
      TAUR=COS(ARG)
      TAUI=SIN(ARG)
      DO 101 K=1,L1
         CH(1,1,K,1) = CC(1,1,1,K)+2.*CC(1,IDO,2,K)
         CH(1,1,K,2) = CC(1,1,1,K)+(2.*TAUR)*CC(1,IDO,2,K)
     1   -(2.*TAUI)*CC(1,1,3,K)
         CH(1,1,K,3) = CC(1,1,1,K)+(2.*TAUR)*CC(1,IDO,2,K)
     1   +2.*TAUI*CC(1,1,3,K)
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
        CH(1,I-1,K,1) = CC(1,I-1,1,K)+(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
        CH(1,I,K,1) = CC(1,I,1,K)+(CC(1,I,3,K)-CC(1,IC,2,K))
        CH(1,I-1,K,2) = WA1(I-2)*
     1 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))-
     * (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
     2                   -WA1(I-1)*
     3 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))+
     * (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
            CH(1,I,K,2) = WA1(I-2)*
     4 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))+
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
     5                  +WA1(I-1)*
     6 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))-
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
              CH(1,I-1,K,3) = WA2(I-2)*
     7 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))+
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
     8   -WA2(I-1)*
     9 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))-
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
            CH(1,I,K,3) = WA2(I-2)*
     1 ((CC(1,I,1,K)+TAUR*(CC(1,I,3,K)-CC(1,IC,2,K)))-
     8 (TAUI*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))))
     2                 +WA2(I-1)*
     3 ((CC(1,I-1,1,K)+TAUR*(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))+
     8 (TAUI*(CC(1,I,3,K)+CC(1,IC,2,K))))
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F3KF (IDO,L1,CC,IN1,CH,IN2,WA1,WA2)
      REAL       CH(IN2,IDO,3,L1)  ,CC(IN1,IDO,L1,3)     ,
     1                WA1(IDO)     ,WA2(IDO)
C
      ARG=2.*4.*ATAN(1.0)/3.
      TAUR=COS(ARG)
      TAUI=SIN(ARG)
      DO 101 K=1,L1
         CH(1,1,1,K) = CC(1,1,K,1)+(CC(1,1,K,2)+CC(1,1,K,3))
         CH(1,1,3,K) = TAUI*(CC(1,1,K,3)-CC(1,1,K,2))
         CH(1,IDO,2,K) = CC(1,1,K,1)+TAUR*
     1      (CC(1,1,K,2)+CC(1,1,K,3))
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            CH(1,I-1,1,K) = CC(1,I-1,K,1)+((WA1(I-2)*CC(1,I-1,K,2)+
     1       WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3)))
            CH(1,I,1,K) = CC(1,I,K,1)+((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3)))
            CH(1,I-1,3,K) = (CC(1,I-1,K,1)+TAUR*((WA1(I-2)*
     1       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*
     1       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))+(TAUI*((WA1(I-2)*
     1       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA2(I-2)*
     1       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))))
            CH(1,IC-1,2,K) = (CC(1,I-1,K,1)+TAUR*((WA1(I-2)*
     1       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA2(I-2)*
     1       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))))-(TAUI*((WA1(I-2)*
     1       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA2(I-2)*
     1       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))))
            CH(1,I,3,K) = (CC(1,I,K,1)+TAUR*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))))+(TAUI*((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2))))
            CH(1,IC,2,K) = (TAUI*((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2))))-(CC(1,I,K,1)+TAUR*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))))
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F4KB (IDO,L1,CC,IN1,CH,IN2,WA1,WA2,WA3)
      REAL       CC(IN1,IDO,4,L1)  ,CH(IN2,IDO,L1,4)    ,
     1           WA1(IDO)  ,        WA2(IDO)  ,       WA3(IDO)
C
      SQRT2=SQRT(2.)
      DO 101 K=1,L1
         CH(1,1,K,3) = (CC(1,1,1,K)+CC(1,IDO,4,K))
     1   -(CC(1,IDO,2,K)+CC(1,IDO,2,K))
         CH(1,1,K,1) = (CC(1,1,1,K)+CC(1,IDO,4,K))
     1   +(CC(1,IDO,2,K)+CC(1,IDO,2,K))
         CH(1,1,K,4) = (CC(1,1,1,K)-CC(1,IDO,4,K))
     1   +(CC(1,1,3,K)+CC(1,1,3,K))
         CH(1,1,K,2) = (CC(1,1,1,K)-CC(1,IDO,4,K))
     1   -(CC(1,1,3,K)+CC(1,1,3,K))
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
        CH(1,I-1,K,1) = (CC(1,I-1,1,K)+CC(1,IC-1,4,K))
     1  +(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
        CH(1,I,K,1) = (CC(1,I,1,K)-CC(1,IC,4,K))
     1  +(CC(1,I,3,K)-CC(1,IC,2,K))
        CH(1,I-1,K,2)=WA1(I-2)*((CC(1,I-1,1,K)-CC(1,IC-1,4,K))
     1  -(CC(1,I,3,K)+CC(1,IC,2,K)))-WA1(I-1)
     1  *((CC(1,I,1,K)+CC(1,IC,4,K))+(CC(1,I-1,3,K)-CC(1,IC-1,2,K)))
        CH(1,I,K,2)=WA1(I-2)*((CC(1,I,1,K)+CC(1,IC,4,K))
     1  +(CC(1,I-1,3,K)-CC(1,IC-1,2,K)))+WA1(I-1)
     1  *((CC(1,I-1,1,K)-CC(1,IC-1,4,K))-(CC(1,I,3,K)+CC(1,IC,2,K)))
        CH(1,I-1,K,3)=WA2(I-2)*((CC(1,I-1,1,K)+CC(1,IC-1,4,K))
     1  -(CC(1,I-1,3,K)+CC(1,IC-1,2,K)))-WA2(I-1)
     1  *((CC(1,I,1,K)-CC(1,IC,4,K))-(CC(1,I,3,K)-CC(1,IC,2,K)))
        CH(1,I,K,3)=WA2(I-2)*((CC(1,I,1,K)-CC(1,IC,4,K))
     1  -(CC(1,I,3,K)-CC(1,IC,2,K)))+WA2(I-1)
     1  *((CC(1,I-1,1,K)+CC(1,IC-1,4,K))-(CC(1,I-1,3,K)
     1  +CC(1,IC-1,2,K)))
        CH(1,I-1,K,4)=WA3(I-2)*((CC(1,I-1,1,K)-CC(1,IC-1,4,K))
     1  +(CC(1,I,3,K)+CC(1,IC,2,K)))-WA3(I-1)
     1 *((CC(1,I,1,K)+CC(1,IC,4,K))-(CC(1,I-1,3,K)-CC(1,IC-1,2,K)))
        CH(1,I,K,4)=WA3(I-2)*((CC(1,I,1,K)+CC(1,IC,4,K))
     1  -(CC(1,I-1,3,K)-CC(1,IC-1,2,K)))+WA3(I-1)
     1  *((CC(1,I-1,1,K)-CC(1,IC-1,4,K))+(CC(1,I,3,K)+CC(1,IC,2,K)))
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
         CH(1,IDO,K,1) = (CC(1,IDO,1,K)+CC(1,IDO,3,K))
     1   +(CC(1,IDO,1,K)+CC(1,IDO,3,K))
         CH(1,IDO,K,2) = SQRT2*((CC(1,IDO,1,K)-CC(1,IDO,3,K))
     1   -(CC(1,1,2,K)+CC(1,1,4,K)))
         CH(1,IDO,K,3) = (CC(1,1,4,K)-CC(1,1,2,K))
     1   +(CC(1,1,4,K)-CC(1,1,2,K))
         CH(1,IDO,K,4) = -SQRT2*((CC(1,IDO,1,K)-CC(1,IDO,3,K))
     1   +(CC(1,1,2,K)+CC(1,1,4,K)))
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F4KF (IDO,L1,CC,IN1,CH,IN2,WA1,WA2,WA3)
      REAL       CC(IN1,IDO,L1,4)   ,CH(IN2,IDO,4,L1)     ,
     1           WA1(IDO)           ,WA2(IDO)     ,WA3(IDO)
C
      HSQT2=SQRT(2.)/2.
      DO 101 K=1,L1
         CH(1,1,1,K) = (CC(1,1,K,2)+CC(1,1,K,4))
     1      +(CC(1,1,K,1)+CC(1,1,K,3))
         CH(1,IDO,4,K) = (CC(1,1,K,1)+CC(1,1,K,3))
     1      -(CC(1,1,K,2)+CC(1,1,K,4))
         CH(1,IDO,2,K) = CC(1,1,K,1)-CC(1,1,K,3)
         CH(1,1,3,K) = CC(1,1,K,4)-CC(1,1,K,2)
  101 CONTINUE
      IF (IDO-2) 107,105,102
  102 IDP2 = IDO+2
      DO 104 K=1,L1
         DO 103 I=3,IDO,2
            IC = IDP2-I
            CH(1,I-1,1,K) = ((WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2))+(WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4)))+(CC(1,I-1,K,1)+(WA2(I-2)*CC(1,I-1,K,3)+
     1       WA2(I-1)*CC(1,I,K,3)))
            CH(1,IC-1,4,K) = (CC(1,I-1,K,1)+(WA2(I-2)*CC(1,I-1,K,3)+
     1       WA2(I-1)*CC(1,I,K,3)))-((WA1(I-2)*CC(1,I-1,K,2)+
     1       WA1(I-1)*CC(1,I,K,2))+(WA3(I-2)*CC(1,I-1,K,4)+
     1       WA3(I-1)*CC(1,I,K,4)))
            CH(1,I,1,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*
     1       CC(1,I-1,K,2))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4)))+(CC(1,I,K,1)+(WA2(I-2)*CC(1,I,K,3)-
     1       WA2(I-1)*CC(1,I-1,K,3)))
            CH(1,IC,4,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*
     1       CC(1,I-1,K,2))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4)))-(CC(1,I,K,1)+(WA2(I-2)*CC(1,I,K,3)-
     1       WA2(I-1)*CC(1,I-1,K,3)))
            CH(1,I-1,3,K) = ((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*
     1       CC(1,I-1,K,2))-(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4)))+(CC(1,I-1,K,1)-(WA2(I-2)*CC(1,I-1,K,3)+
     1       WA2(I-1)*CC(1,I,K,3)))
            CH(1,IC-1,2,K) = (CC(1,I-1,K,1)-(WA2(I-2)*CC(1,I-1,K,3)+
     1       WA2(I-1)*CC(1,I,K,3)))-((WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*
     1       CC(1,I-1,K,2))-(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4)))
            CH(1,I,3,K) = ((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))+(CC(1,I,K,1)-(WA2(I-2)*CC(1,I,K,3)-
     1       WA2(I-1)*CC(1,I-1,K,3)))
            CH(1,IC,2,K) = ((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))-(CC(1,I,K,1)-(WA2(I-2)*CC(1,I,K,3)-
     1       WA2(I-1)*CC(1,I-1,K,3)))
  103    CONTINUE
  104 CONTINUE
      IF (MOD(IDO,2) .EQ. 1) RETURN
  105 CONTINUE
      DO 106 K=1,L1
            CH(1,IDO,1,K) = (HSQT2*(CC(1,IDO,K,2)-CC(1,IDO,K,4)))+
     1       CC(1,IDO,K,1)
            CH(1,IDO,3,K) = CC(1,IDO,K,1)-(HSQT2*(CC(1,IDO,K,2)-
     1       CC(1,IDO,K,4)))
            CH(1,1,2,K) = (-HSQT2*(CC(1,IDO,K,2)+CC(1,IDO,K,4)))-
     1       CC(1,IDO,K,3)
            CH(1,1,4,K) = (-HSQT2*(CC(1,IDO,K,2)+CC(1,IDO,K,4)))+
     1       CC(1,IDO,K,3)
  106 CONTINUE
  107 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F5KB (IDO,L1,CC,IN1,CH,IN2,
     1       WA1,WA2,WA3,WA4)
      REAL   CC(IN1,IDO,5,L1)    ,CH(IN2,IDO,L1,5),
     1       WA1(IDO)     ,WA2(IDO)     ,WA3(IDO)     ,WA4(IDO)
C
      ARG=2.*4.*ATAN(1.0)/5.
      TR11=COS(ARG)
      TI11=SIN(ARG)
      TR12=COS(2.*ARG)
      TI12=SIN(2.*ARG)
      DO 101 K=1,L1
         CH(1,1,K,1) = CC(1,1,1,K)+2.*CC(1,IDO,2,K)+2.*CC(1,IDO,4,K)
         CH(1,1,K,2) = (CC(1,1,1,K)+TR11*2.*CC(1,IDO,2,K)
     1   +TR12*2.*CC(1,IDO,4,K))-(TI11*2.*CC(1,1,3,K)
     1   +TI12*2.*CC(1,1,5,K))
         CH(1,1,K,3) = (CC(1,1,1,K)+TR12*2.*CC(1,IDO,2,K)
     1   +TR11*2.*CC(1,IDO,4,K))-(TI12*2.*CC(1,1,3,K)
     1   -TI11*2.*CC(1,1,5,K))
         CH(1,1,K,4) = (CC(1,1,1,K)+TR12*2.*CC(1,IDO,2,K)
     1   +TR11*2.*CC(1,IDO,4,K))+(TI12*2.*CC(1,1,3,K)
     1   -TI11*2.*CC(1,1,5,K))
         CH(1,1,K,5) = (CC(1,1,1,K)+TR11*2.*CC(1,IDO,2,K)
     1   +TR12*2.*CC(1,IDO,4,K))+(TI11*2.*CC(1,1,3,K)
     1   +TI12*2.*CC(1,1,5,K))
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
        CH(1,I-1,K,1) = CC(1,I-1,1,K)+(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +(CC(1,I-1,5,K)+CC(1,IC-1,4,K))
        CH(1,I,K,1) = CC(1,I,1,K)+(CC(1,I,3,K)-CC(1,IC,2,K))
     1  +(CC(1,I,5,K)-CC(1,IC,4,K))
        CH(1,I-1,K,2) = WA1(I-2)*((CC(1,I-1,1,K)+TR11*
     1  (CC(1,I-1,3,K)+CC(1,IC-1,2,K))+TR12
     1  *(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))-(TI11*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))+TI12*(CC(1,I,5,K)+CC(1,IC,4,K))))
     1  -WA1(I-1)*((CC(1,I,1,K)+TR11*(CC(1,I,3,K)-CC(1,IC,2,K))
     1  +TR12*(CC(1,I,5,K)-CC(1,IC,4,K)))+(TI11*(CC(1,I-1,3,K)
     1  -CC(1,IC-1,2,K))+TI12*(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
        CH(1,I,K,2) = WA1(I-2)*((CC(1,I,1,K)+TR11*(CC(1,I,3,K)
     1  -CC(1,IC,2,K))+TR12*(CC(1,I,5,K)-CC(1,IC,4,K)))
     1  +(TI11*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))+TI12
     1  *(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))+WA1(I-1)
     1  *((CC(1,I-1,1,K)+TR11*(CC(1,I-1,3,K)
     1  +CC(1,IC-1,2,K))+TR12*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))
     1  -(TI11*(CC(1,I,3,K)+CC(1,IC,2,K))+TI12
     1  *(CC(1,I,5,K)+CC(1,IC,4,K))))
        CH(1,I-1,K,3) = WA2(I-2)
     1  *((CC(1,I-1,1,K)+TR12*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR11*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))-(TI12*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))-TI11*(CC(1,I,5,K)+CC(1,IC,4,K))))
     1 -WA2(I-1)
     1 *((CC(1,I,1,K)+TR12*(CC(1,I,3,K)-
     1  CC(1,IC,2,K))+TR11*(CC(1,I,5,K)-CC(1,IC,4,K)))
     1  +(TI12*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))-TI11
     1  *(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
        CH(1,I,K,3) = WA2(I-2)
     1 *((CC(1,I,1,K)+TR12*(CC(1,I,3,K)-
     1  CC(1,IC,2,K))+TR11*(CC(1,I,5,K)-CC(1,IC,4,K)))
     1  +(TI12*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))-TI11
     1  *(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
     1  +WA2(I-1)
     1  *((CC(1,I-1,1,K)+TR12*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR11*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))-(TI12*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))-TI11*(CC(1,I,5,K)+CC(1,IC,4,K))))
        CH(1,I-1,K,4) = WA3(I-2)
     1  *((CC(1,I-1,1,K)+TR12*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR11*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))+(TI12*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))-TI11*(CC(1,I,5,K)+CC(1,IC,4,K))))
     1  -WA3(I-1)
     1 *((CC(1,I,1,K)+TR12*(CC(1,I,3,K)-
     1  CC(1,IC,2,K))+TR11*(CC(1,I,5,K)-CC(1,IC,4,K)))
     1  -(TI12*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))-TI11
     1  *(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
        CH(1,I,K,4) = WA3(I-2)
     1 *((CC(1,I,1,K)+TR12*(CC(1,I,3,K)-
     1  CC(1,IC,2,K))+TR11*(CC(1,I,5,K)-CC(1,IC,4,K)))
     1  -(TI12*(CC(1,I-1,3,K)-CC(1,IC-1,2,K))-TI11
     1  *(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
     1  +WA3(I-1)
     1  *((CC(1,I-1,1,K)+TR12*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR11*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))+(TI12*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))-TI11*(CC(1,I,5,K)+CC(1,IC,4,K))))
        CH(1,I-1,K,5) = WA4(I-2)
     1  *((CC(1,I-1,1,K)+TR11*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR12*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))+(TI11*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))+TI12*(CC(1,I,5,K)+CC(1,IC,4,K))))
     1  -WA4(I-1)
     1  *((CC(1,I,1,K)+TR11*(CC(1,I,3,K)-CC(1,IC,2,K))
     1  +TR12*(CC(1,I,5,K)-CC(1,IC,4,K)))-(TI11*(CC(1,I-1,3,K)
     1  -CC(1,IC-1,2,K))+TI12*(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
        CH(1,I,K,5) = WA4(I-2)
     1  *((CC(1,I,1,K)+TR11*(CC(1,I,3,K)-CC(1,IC,2,K))
     1  +TR12*(CC(1,I,5,K)-CC(1,IC,4,K)))-(TI11*(CC(1,I-1,3,K)
     1  -CC(1,IC-1,2,K))+TI12*(CC(1,I-1,5,K)-CC(1,IC-1,4,K))))
     1  +WA4(I-1)
     1  *((CC(1,I-1,1,K)+TR11*(CC(1,I-1,3,K)+CC(1,IC-1,2,K))
     1  +TR12*(CC(1,I-1,5,K)+CC(1,IC-1,4,K)))+(TI11*(CC(1,I,3,K)
     1  +CC(1,IC,2,K))+TI12*(CC(1,I,5,K)+CC(1,IC,4,K))))
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1F5KF (IDO,L1,CC,IN1,CH,IN2,
     1                   WA1,WA2,WA3,WA4)
      REAL       CC(IN1,IDO,L1,5)    ,CH(IN2,IDO,5,L1)     ,
     1           WA1(IDO)     ,WA2(IDO)     ,WA3(IDO)     ,WA4(IDO)
C
      ARG=2.*4.*ATAN(1.0)/5.
      TR11=COS(ARG)
      TI11=SIN(ARG)
      TR12=COS(2.*ARG)
      TI12=SIN(2.*ARG)
      DO 101 K=1,L1
         CH(1,1,1,K) = CC(1,1,K,1)+(CC(1,1,K,5)+CC(1,1,K,2))+
     1    (CC(1,1,K,4)+CC(1,1,K,3))
         CH(1,IDO,2,K) = CC(1,1,K,1)+TR11*(CC(1,1,K,5)+CC(1,1,K,2))+
     1    TR12*(CC(1,1,K,4)+CC(1,1,K,3))
         CH(1,1,3,K) = TI11*(CC(1,1,K,5)-CC(1,1,K,2))+TI12*
     1    (CC(1,1,K,4)-CC(1,1,K,3))
         CH(1,IDO,4,K) = CC(1,1,K,1)+TR12*(CC(1,1,K,5)+CC(1,1,K,2))+
     1    TR11*(CC(1,1,K,4)+CC(1,1,K,3))
         CH(1,1,5,K) = TI12*(CC(1,1,K,5)-CC(1,1,K,2))-TI11*
     1    (CC(1,1,K,4)-CC(1,1,K,3))
  101 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IDP2 = IDO+2
      DO 103 K=1,L1
         DO 102 I=3,IDO,2
            IC = IDP2-I
            CH(1,I-1,1,K) = CC(1,I-1,K,1)+((WA1(I-2)*CC(1,I-1,K,2)+
     1       WA1(I-1)*CC(1,I,K,2))+(WA4(I-2)*CC(1,I-1,K,5)+WA4(I-1)*
     1       CC(1,I,K,5)))+((WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))+(WA3(I-2)*CC(1,I-1,K,4)+
     1       WA3(I-1)*CC(1,I,K,4)))
            CH(1,I,1,K) = CC(1,I,K,1)+((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*
     1       CC(1,I-1,K,5)))+((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4)))
            CH(1,I-1,3,K) = CC(1,I-1,K,1)+TR11*
     1      ( WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2)
     1       +WA4(I-2)*CC(1,I-1,K,5)+WA4(I-1)*CC(1,I,K,5))+TR12*
     1      ( WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3)
     1       +WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,4))+TI11*
     1      ( WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2)
     1       -(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))+TI12*
     1      ( WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3)
     1       -(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,4)))
            CH(1,IC-1,2,K) = CC(1,I-1,K,1)+TR11*
     1      ( WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2)
     1       +WA4(I-2)*CC(1,I-1,K,5)+WA4(I-1)*CC(1,I,K,5))+TR12*
     1     ( WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3)
     1      +WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,4))-(TI11*
     1      ( WA1(I-2)*CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2)
     1       -(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))+TI12*
     1      ( WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3)
     1       -(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,4))))
            CH(1,I,3,K) = (CC(1,I,K,1)+TR11*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*
     1       CC(1,I-1,K,5)))+TR12*((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4))))+(TI11*((WA4(I-2)*CC(1,I-1,K,5)+
     1       WA4(I-1)*CC(1,I,K,5))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))+TI12*((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))))
            CH(1,IC,2,K) = (TI11*((WA4(I-2)*CC(1,I-1,K,5)+WA4(I-1)*
     1       CC(1,I,K,5))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))+TI12*((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))))-(CC(1,I,K,1)+TR11*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*
     1       CC(1,I-1,K,5)))+TR12*((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4))))
            CH(1,I-1,5,K) = (CC(1,I-1,K,1)+TR12*((WA1(I-2)*
     1       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA4(I-2)*
     1       CC(1,I-1,K,5)+WA4(I-1)*CC(1,I,K,5)))+TR11*((WA2(I-2)*
     1       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))+(WA3(I-2)*
     1       CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,4))))+(TI12*((WA1(I-2)*
     1       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA4(I-2)*
     1       CC(1,I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))-TI11*((WA2(I-2)*
     1       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))-(WA3(I-2)*
     1       CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,4))))
            CH(1,IC-1,4,K) = (CC(1,I-1,K,1)+TR12*((WA1(I-2)*
     1       CC(1,I-1,K,2)+WA1(I-1)*CC(1,I,K,2))+(WA4(I-2)*
     1       CC(1,I-1,K,5)+WA4(I-1)*CC(1,I,K,5)))+TR11*((WA2(I-2)*
     1       CC(1,I-1,K,3)+WA2(I-1)*CC(1,I,K,3))+(WA3(I-2)*
     1       CC(1,I-1,K,4)+WA3(I-1)*CC(1,I,K,4))))-(TI12*((WA1(I-2)*
     1       CC(1,I,K,2)-WA1(I-1)*CC(1,I-1,K,2))-(WA4(I-2)*
     1       CC(1,I,K,5)-WA4(I-1)*CC(1,I-1,K,5)))-TI11*((WA2(I-2)*
     1       CC(1,I,K,3)-WA2(I-1)*CC(1,I-1,K,3))-(WA3(I-2)*
     1       CC(1,I,K,4)-WA3(I-1)*CC(1,I-1,K,4))))
            CH(1,I,5,K) = (CC(1,I,K,1)+TR12*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*
     1       CC(1,I-1,K,5)))+TR11*((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4))))+(TI12*((WA4(I-2)*CC(1,I-1,K,5)+
     1       WA4(I-1)*CC(1,I,K,5))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))-TI11*((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))))
            CH(1,IC,4,K) = (TI12*((WA4(I-2)*CC(1,I-1,K,5)+WA4(I-1)*
     1       CC(1,I,K,5))-(WA1(I-2)*CC(1,I-1,K,2)+WA1(I-1)*
     1       CC(1,I,K,2)))-TI11*((WA3(I-2)*CC(1,I-1,K,4)+WA3(I-1)*
     1       CC(1,I,K,4))-(WA2(I-2)*CC(1,I-1,K,3)+WA2(I-1)*
     1       CC(1,I,K,3))))-(CC(1,I,K,1)+TR12*((WA1(I-2)*CC(1,I,K,2)-
     1       WA1(I-1)*CC(1,I-1,K,2))+(WA4(I-2)*CC(1,I,K,5)-WA4(I-1)*
     1       CC(1,I-1,K,5)))+TR11*((WA2(I-2)*CC(1,I,K,3)-WA2(I-1)*
     1       CC(1,I-1,K,3))+(WA3(I-2)*CC(1,I,K,4)-WA3(I-1)*
     1       CC(1,I-1,K,4))))
  102    CONTINUE
  103 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1FGKB (IDO,IP,L1,IDL1,CC,C1,C2,IN1,
     1          CH,CH2,IN2,WA)
      REAL      CH(IN2,IDO,L1,IP)    ,CC(IN1,IDO,IP,L1) ,
     1          C1(IN1,IDO,L1,IP)    ,C2(IN1,IDL1,IP),
     2          CH2(IN2,IDL1,IP)     ,WA(IDO)
C
      TPI=2.*4.*ATAN(1.0)
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
            CH(1,I,K,1) = CC(1,I,1,K)
  101    CONTINUE
  102 CONTINUE
      GO TO 106
  103 DO 105 I=1,IDO
         DO 104 K=1,L1
            CH(1,I,K,1) = CC(1,I,1,K)
  104    CONTINUE
  105 CONTINUE
  106 DO 108 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 107 K=1,L1
            CH(1,1,K,J) = CC(1,IDO,J2-2,K)+CC(1,IDO,J2-2,K)
            CH(1,1,K,JC) = CC(1,1,J2-1,K)+CC(1,1,J2-1,K)
 1007       CONTINUE
  107    CONTINUE
  108 CONTINUE
      IF (IDO .EQ. 1) GO TO 116
      IF (NBD .LT. L1) GO TO 112
      DO 111 J=2,IPPH
         JC = IPP2-J
         DO 110 K=1,L1
            DO 109 I=3,IDO,2
               IC = IDP2-I
               CH(1,I-1,K,J) = CC(1,I-1,2*J-1,K)+CC(1,IC-1,2*J-2,K)
               CH(1,I-1,K,JC) = CC(1,I-1,2*J-1,K)-CC(1,IC-1,2*J-2,K)
               CH(1,I,K,J) = CC(1,I,2*J-1,K)-CC(1,IC,2*J-2,K)
               CH(1,I,K,JC) = CC(1,I,2*J-1,K)+CC(1,IC,2*J-2,K)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      GO TO 116
  112 DO 115 J=2,IPPH
         JC = IPP2-J
         DO 114 I=3,IDO,2
            IC = IDP2-I
            DO 113 K=1,L1
               CH(1,I-1,K,J) = CC(1,I-1,2*J-1,K)+CC(1,IC-1,2*J-2,K)
               CH(1,I-1,K,JC) = CC(1,I-1,2*J-1,K)-CC(1,IC-1,2*J-2,K)
               CH(1,I,K,J) = CC(1,I,2*J-1,K)-CC(1,IC,2*J-2,K)
               CH(1,I,K,JC) = CC(1,I,2*J-1,K)+CC(1,IC,2*J-2,K)
  113       CONTINUE
  114    CONTINUE
  115 CONTINUE
  116 AR1 = 1.
      AI1 = 0.
      DO 120 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 117 IK=1,IDL1
            C2(1,IK,L) = CH2(1,IK,1)+AR1*CH2(1,IK,2)
            C2(1,IK,LC) = AI1*CH2(1,IK,IP)
  117    CONTINUE
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
               C2(1,IK,L) = C2(1,IK,L)+AR2*CH2(1,IK,J)
               C2(1,IK,LC) = C2(1,IK,LC)+AI2*CH2(1,IK,JC)
  118       CONTINUE
  119    CONTINUE
  120 CONTINUE
      DO 122 J=2,IPPH
         DO 121 IK=1,IDL1
            CH2(1,IK,1) = CH2(1,IK,1)+CH2(1,IK,J)
  121    CONTINUE
  122 CONTINUE
      DO 124 J=2,IPPH
         JC = IPP2-J
         DO 123 K=1,L1
            CH(1,1,K,J) = C1(1,1,K,J)-C1(1,1,K,JC)
            CH(1,1,K,JC) = C1(1,1,K,J)+C1(1,1,K,JC)
  123    CONTINUE
  124 CONTINUE
      IF (IDO .EQ. 1) GO TO 132
      IF (NBD .LT. L1) GO TO 128
      DO 127 J=2,IPPH
         JC = IPP2-J
         DO 126 K=1,L1
            DO 125 I=3,IDO,2
               CH(1,I-1,K,J) = C1(1,I-1,K,J)-C1(1,I,K,JC)
               CH(1,I-1,K,JC) = C1(1,I-1,K,J)+C1(1,I,K,JC)
               CH(1,I,K,J) = C1(1,I,K,J)+C1(1,I-1,K,JC)
               CH(1,I,K,JC) = C1(1,I,K,J)-C1(1,I-1,K,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      GO TO 132
  128 DO 131 J=2,IPPH
         JC = IPP2-J
         DO 130 I=3,IDO,2
            DO 129 K=1,L1
               CH(1,I-1,K,J) = C1(1,I-1,K,J)-C1(1,I,K,JC)
               CH(1,I-1,K,JC) = C1(1,I-1,K,J)+C1(1,I,K,JC)
               CH(1,I,K,J) = C1(1,I,K,J)+C1(1,I-1,K,JC)
               CH(1,I,K,JC) = C1(1,I,K,J)-C1(1,I-1,K,JC)
  129       CONTINUE
  130    CONTINUE
  131 CONTINUE
  132 CONTINUE
      IF (IDO .EQ. 1) RETURN
      DO 133 IK=1,IDL1
         C2(1,IK,1) = CH2(1,IK,1)
  133 CONTINUE
      DO 135 J=2,IP
         DO 134 K=1,L1
            C1(1,1,K,J) = CH(1,1,K,J)
  134    CONTINUE
  135 CONTINUE
      IF (NBD .GT. L1) GO TO 139
      IS = -IDO
      DO 138 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 137 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 136 K=1,L1
               C1(1,I-1,K,J) = WA(IDIJ-1)*CH(1,I-1,K,J)-WA(IDIJ)*
     1          CH(1,I,K,J)
               C1(1,I,K,J) = WA(IDIJ-1)*CH(1,I,K,J)+WA(IDIJ)*
     1          CH(1,I-1,K,J)
  136       CONTINUE
  137    CONTINUE
  138 CONTINUE
      GO TO 143
  139 IS = -IDO
      DO 142 J=2,IP
         IS = IS+IDO
         DO 141 K=1,L1
            IDIJ = IS
            DO 140 I=3,IDO,2
               IDIJ = IDIJ+2
               C1(1,I-1,K,J) = WA(IDIJ-1)*CH(1,I-1,K,J)-WA(IDIJ)*
     1          CH(1,I,K,J)
               C1(1,I,K,J) = WA(IDIJ-1)*CH(1,I,K,J)+WA(IDIJ)*
     1          CH(1,I-1,K,J)
  140       CONTINUE
  141    CONTINUE
  142 CONTINUE
  143 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE R1FGKF (IDO,IP,L1,IDL1,CC,C1,C2,IN1,
     1              CH,CH2,IN2,WA)
      REAL          CH(IN2,IDO,L1,IP)   ,CC(IN1,IDO,IP,L1),
     1              C1(IN1,IDO,L1,IP)   ,C2(IN1,IDL1,IP),
     2              CH2(IN2,IDL1,IP)    ,WA(IDO)
C
      TPI=2.*4.*ATAN(1.0)
      ARG = TPI/FLOAT(IP)
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
      NBD = (IDO-1)/2
      IF (IDO .EQ. 1) GO TO 119
      DO 101 IK=1,IDL1
         CH2(1,IK,1) = C2(1,IK,1)
  101 CONTINUE
      DO 103 J=2,IP
         DO 102 K=1,L1
            CH(1,1,K,J) = C1(1,1,K,J)
  102    CONTINUE
  103 CONTINUE
      IF (NBD .GT. L1) GO TO 107
      IS = -IDO
      DO 106 J=2,IP
         IS = IS+IDO
         IDIJ = IS
         DO 105 I=3,IDO,2
            IDIJ = IDIJ+2
            DO 104 K=1,L1
               CH(1,I-1,K,J) = WA(IDIJ-1)*C1(1,I-1,K,J)+WA(IDIJ)
     1           *C1(1,I,K,J)
               CH(1,I,K,J) = WA(IDIJ-1)*C1(1,I,K,J)-WA(IDIJ)
     1           *C1(1,I-1,K,J)
  104       CONTINUE
  105    CONTINUE
  106 CONTINUE
      GO TO 111
  107 IS = -IDO
      DO 110 J=2,IP
         IS = IS+IDO
         DO 109 K=1,L1
            IDIJ = IS
            DO 108 I=3,IDO,2
               IDIJ = IDIJ+2
               CH(1,I-1,K,J) = WA(IDIJ-1)*C1(1,I-1,K,J)+WA(IDIJ)
     1           *C1(1,I,K,J)
               CH(1,I,K,J) = WA(IDIJ-1)*C1(1,I,K,J)-WA(IDIJ)
     1           *C1(1,I-1,K,J)
  108       CONTINUE
  109    CONTINUE
  110 CONTINUE
  111 IF (NBD .LT. L1) GO TO 115
      DO 114 J=2,IPPH
         JC = IPP2-J
         DO 113 K=1,L1
            DO 112 I=3,IDO,2
               C1(1,I-1,K,J) = CH(1,I-1,K,J)+CH(1,I-1,K,JC)
               C1(1,I-1,K,JC) = CH(1,I,K,J)-CH(1,I,K,JC)
               C1(1,I,K,J) = CH(1,I,K,J)+CH(1,I,K,JC)
               C1(1,I,K,JC) = CH(1,I-1,K,JC)-CH(1,I-1,K,J)
  112       CONTINUE
  113    CONTINUE
  114 CONTINUE
      GO TO 121
  115 DO 118 J=2,IPPH
         JC = IPP2-J
         DO 117 I=3,IDO,2
            DO 116 K=1,L1
               C1(1,I-1,K,J) = CH(1,I-1,K,J)+CH(1,I-1,K,JC)
               C1(1,I-1,K,JC) = CH(1,I,K,J)-CH(1,I,K,JC)
               C1(1,I,K,J) = CH(1,I,K,J)+CH(1,I,K,JC)
               C1(1,I,K,JC) = CH(1,I-1,K,JC)-CH(1,I-1,K,J)
  116       CONTINUE
  117    CONTINUE
  118 CONTINUE
      GO TO 121
  119 DO 120 IK=1,IDL1
         C2(1,IK,1) = CH2(1,IK,1)
  120 CONTINUE
  121 DO 123 J=2,IPPH
         JC = IPP2-J
         DO 122 K=1,L1
            C1(1,1,K,J) = CH(1,1,K,J)+CH(1,1,K,JC)
            C1(1,1,K,JC) = CH(1,1,K,JC)-CH(1,1,K,J)
  122    CONTINUE
  123 CONTINUE
C
      AR1 = 1.
      AI1 = 0.
      DO 127 L=2,IPPH
         LC = IPP2-L
         AR1H = DCP*AR1-DSP*AI1
         AI1 = DCP*AI1+DSP*AR1
         AR1 = AR1H
         DO 124 IK=1,IDL1
            CH2(1,IK,L) = C2(1,IK,1)+AR1*C2(1,IK,2)
            CH2(1,IK,LC) = AI1*C2(1,IK,IP)
  124    CONTINUE
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
               CH2(1,IK,L) = CH2(1,IK,L)+AR2*C2(1,IK,J)
               CH2(1,IK,LC) = CH2(1,IK,LC)+AI2*C2(1,IK,JC)
  125       CONTINUE
  126    CONTINUE
  127 CONTINUE
      DO 129 J=2,IPPH
         DO 128 IK=1,IDL1
            CH2(1,IK,1) = CH2(1,IK,1)+C2(1,IK,J)
  128    CONTINUE
  129 CONTINUE
C
      IF (IDO .LT. L1) GO TO 132
      DO 131 K=1,L1
         DO 130 I=1,IDO
            CC(1,I,1,K) = CH(1,I,K,1)
  130    CONTINUE
  131 CONTINUE
      GO TO 135
  132 DO 134 I=1,IDO
         DO 133 K=1,L1
            CC(1,I,1,K) = CH(1,I,K,1)
  133    CONTINUE
  134 CONTINUE
  135 DO 137 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 136 K=1,L1
            CC(1,IDO,J2-2,K) = CH(1,1,K,J)
            CC(1,1,J2-1,K) = CH(1,1,K,JC)
  136    CONTINUE
  137 CONTINUE
      IF (IDO .EQ. 1) RETURN
      IF (NBD .LT. L1) GO TO 141
      DO 140 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 139 K=1,L1
            DO 138 I=3,IDO,2
               IC = IDP2-I
               CC(1,I-1,J2-1,K) = CH(1,I-1,K,J)+CH(1,I-1,K,JC)
               CC(1,IC-1,J2-2,K) = CH(1,I-1,K,J)-CH(1,I-1,K,JC)
               CC(1,I,J2-1,K) = CH(1,I,K,J)+CH(1,I,K,JC)
               CC(1,IC,J2-2,K) = CH(1,I,K,JC)-CH(1,I,K,J)
  138       CONTINUE
  139    CONTINUE
  140 CONTINUE
      RETURN
  141 DO 144 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 143 I=3,IDO,2
            IC = IDP2-I
            DO 142 K=1,L1
               CC(1,I-1,J2-1,K) = CH(1,I-1,K,J)+CH(1,I-1,K,JC)
               CC(1,IC-1,J2-2,K) = CH(1,I-1,K,J)-CH(1,I-1,K,JC)
               CC(1,I,J2-1,K) = CH(1,I,K,J)+CH(1,I,K,JC)
               CC(1,IC,J2-2,K) = CH(1,I,K,JC)-CH(1,I,K,J)
  142       CONTINUE
  143    CONTINUE
  144 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1
C
C   AUTHORS:  PAUL N. SWARZTRAUBER AND RICHARD A. VALENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine r2w(ldr,ldw,l,m,r,w)
      dimension r(ldr,*),w(ldw,*)
      do j=1,m
      do i=1,l
      w(i,j) = r( i,j)
      end do
      end do
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT1B ( N, INC, R, LENR, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  N, INC, LENR, LENSAV, LENWRK, IER
      REAL     R(LENR), WSAVE(LENSAV)     ,WORK(LENWRK)
C
      IER = 0
C
      IF (LENR .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('RFFT1B ', 6)
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFT1B ', 8)
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('RFFT1B ', 10)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL RFFTB1 (N,INC,R,WORK,WSAVE,WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT1F ( N, INC, R, LENR, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  N, INC, LENR, LENSAV, LENWRK, IER
      REAL     R(LENR), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENR .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('RFFT1F ', 6)
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFT1F ', 8)
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('RFFT1F ', 10)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL RFFTF1 (N,INC,R,WORK,WSAVE,WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFT1I ( N, WSAVE, LENSAV, IER )
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFT1I ', 3)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL RFFTI1 (N,WSAVE(1),WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1
C
C   AUTHORS:  PAUL N. SWARZTRAUBER AND RICHARD A. VALENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE RFFT2B (LDIM, L, M, R, WSAVE, LENSAV, WORK,
     1  LENWRK, IER)
      INTEGER LDIM, L, M, LENSAV, LENWRK, IER
      REAL    R(LDIM,M), WSAVE(LENSAV), WORK(LENWRK)
      INTEGER LDX
C
C
C INITIALIZE IER
C
      IER = 0
C
C VERIFY LENSAV
C
      LWSAV =   L+INT(LOG(REAL(L))/LOG(2.))+4
      MWSAV =   2*M+INT(LOG(REAL(M))/LOG(2.))+4
      MMSAV =   M+INT(LOG(REAL(M))/LOG(2.))+4
      MODL = MOD(L,2)
      MODM = MOD(M,2)
C
      IF (LENSAV .LT. LWSAV+MWSAV+MMSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2F', 6)
        GO TO 100
      ENDIF
C
C VERIFY LENWRK
C
      IF (LENWRK .LT. (L+1)*M) THEN
        IER = 3
        CALL XERFFT ('RFFT2F', 8)
        GO TO 100
      ENDIF
C
C VERIFY LDIM IS AS BIG AS L
C
      IF (LDIM .LT. L) THEN
        IER = 5
        CALL XERFFT ('RFFT2F', -6)
        GO TO 100
      ENDIF
C
C TRANSFORM SECOND DIMENSION OF ARRAY
C
      DO J=2,2*((M+1)/2)-1
      R(1,J) = R(1,J)+R(1,J)
      END DO
      DO J=3,M,2
      R(1,J) = -R(1,J)
      END DO
      CALL RFFTMB(1,1,M,LDIM,R,M*LDIM,
     1     WSAVE(LWSAV+MWSAV+1),MMSAV,WORK,LENWRK,IER1)
      LDH = INT((L+1)/2)
      IF(LDH.GT.1) THEN
      LDW = LDH+LDH
C
C     R AND WORK ARE SWITCHED BECAUSE THE THE FIRST DIMENSION
C     OF THE INPUT TO COMPLEX CFFTMF MUST BE EVEN.
C
      CALL R2W(LDIM,LDW,L,M,R,WORK)
      CALL CFFTMB(LDH-1,1,M,LDH,WORK(2),LDH*M,
     1     WSAVE(LWSAV+1),MWSAV,R,L*M, IER1)
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2B',-5)
         GO TO 100
      END IF
      CALL W2R(LDIM,LDW,L,M,R,WORK)
      END IF
C
      IF(MODL.EQ.0) THEN
      DO J=2,2*((M+1)/2)-1
      R(L,J) = R(L,J)+R(L,J)
      END DO
      DO J=3,M,2
      R(L,J) = -R(L,J)
      END DO
      CALL RFFTMB(1,1,M,LDIM,R(L,1),M*LDIM,
     1     WSAVE(LWSAV+MWSAV+1),MMSAV,WORK,LENWRK,IER1)
      END IF
C
C     PRINT*, 'BACKWARD TRANSFORM IN THE J DIRECTION'
C     DO I=1,L
C       PRINT*, (R(I,J),J=1,M)
C     END DO
C
C TRANSFORM FIRST DIMENSION OF ARRAY
C
      LDX = 2*INT((L+1)/2)-1
      DO I=2,LDX
      DO J=1,M
      R(I,J) = R(I,J)+R(I,J)
      END DO
      END DO
      DO J=1,M
      DO I=3,LDX,2
      R(I,J) = -R(I,J)
      END DO
      END DO
      CALL RFFTMB(M,LDIM,L,1,R,M*LDIM,WSAVE(1),
     .     L+INT(LOG(REAL(L))/LOG(2.))+4,WORK,LENWRK,IER1)
C

C
C     PRINT*, 'BACKWARD TRANSFORM IN THE I DIRECTION'
C     DO I=1,L
C       PRINT*, (R(I,J),J=1,M)
C     END DO
C
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
C
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
C
  100 CONTINUE
C
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1
C
C   AUTHORS:  PAUL N. SWARZTRAUBER AND RICHARD A. VALENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE RFFT2F (LDIM, L, M, R, WSAVE, LENSAV, WORK,
     1  LENWRK, IER)
      INTEGER LDIM, L, M, LENSAV, LENWRK, IER, IDX, MODL, MODM,
     1                      IDH, IDW
      REAL    R(LDIM,M), WSAVE(LENSAV), WORK(LENWRK)
C
C
C INITIALIZE IER
C
      IER = 0
C
C VERIFY LENSAV
C
      LWSAV =   L+INT(LOG(REAL(L))/LOG(2.))+4
      MWSAV =   2*M+INT(LOG(REAL(M))/LOG(2.))+4
      MMSAV =   M+INT(LOG(REAL(M))/LOG(2.))+4
C
      IF (LENSAV .LT. LWSAV+MWSAV+MMSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2F', 6)
        GO TO 100
      ENDIF
C
C VERIFY LENWRK
C
      IF (LENWRK .LT. (L+1)*M) THEN
        IER = 3
        CALL XERFFT ('RFFT2F', 8)
        GO TO 100
      ENDIF
C
C VERIFY LDIM IS AS BIG AS L
C
      IF (LDIM .LT. L) THEN
        IER = 5
        CALL XERFFT ('RFFT2F', -6)
        GO TO 100
      ENDIF
C
C TRANSFORM FIRST DIMENSION OF ARRAY
C
      CALL RFFTMF(M,LDIM,L,1,R,M*LDIM,WSAVE(1),
     .     L+INT(LOG(REAL(L))/LOG(2.))+4,WORK,LENWRK,IER1)
C
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
C
      LDX = 2*INT((L+1)/2)-1
      DO I=2,LDX
      DO J=1,M
      R(I,J) = .5*R(I,J)
      END DO
      END DO
      DO J=1,M
      DO I=3,LDX,2
      R(I,J) = -R(I,J)
      END DO
      END DO
C
C     PRINT*, 'FORWARD TRANSFORM IN THE I DIRECTION'
C     DO I=1,L
C       PRINT*, (R(I,J),J=1,M)
C     END DO
C
C RESHUFFLE TO ADD IN NYQUIST IMAGINARY COMPONENTS
C
      MODL = MOD(L,2)
      MODM = MOD(M,2)
C
C TRANSFORM SECOND DIMENSION OF ARRAY
C
      CALL RFFTMF(1,1,M,LDIM,R,M*LDIM,
     1     WSAVE(LWSAV+MWSAV+1),MMSAV,WORK,LENWRK,IER1)
      DO J=2,2*((M+1)/2)-1
      R(1,J) = .5*R(1,J)
      END DO
      DO J=3,M,2
      R(1,J) = -R(1,J)
      END DO
      LDH = INT((L+1)/2)
      IF(LDH.GT.1) THEN
      LDW = LDH+LDH
C
C     R AND WORK ARE SWITCHED BECAUSE THE THE FIRST DIMENSION
C     OF THE INPUT TO COMPLEX CFFTMF MUST BE EVEN.
C
      CALL R2W(LDIM,LDW,L,M,R,WORK)
      CALL CFFTMF(LDH-1,1,M,LDH,WORK(2),LDH*M,
     1     WSAVE(LWSAV+1),MWSAV,R,L*M, IER1)
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
      CALL W2R(LDIM,LDW,L,M,R,WORK)
      END IF
C
      IF(MODL.EQ.0) THEN
      CALL RFFTMF(1,1,M,LDIM,R(L,1),M*LDIM,
     1     WSAVE(LWSAV+MWSAV+1),MMSAV,WORK,LENWRK,IER1)
      DO J=2,2*((M+1)/2)-1
      R(L,J) = .5*R(L,J)
      END DO
      DO J=3,M,2
      R(L,J) = -R(L,J)
      END DO
      END IF
C
C     PRINT*, 'FORWARD TRANSFORM IN THE J DIRECTION'
C     DO I=1,L
C       PRINT*, (R(I,J),J=1,M)
C     END DO
C
      IF(IER1.NE.0) THEN
         IER=20
         CALL XERFFT('RFFT2F',-5)
         GO TO 100
      ENDIF
C
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1
C
C   AUTHORS:  PAUL N. SWARZTRAUBER AND RICHARD A. VALENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE RFFT2I (L, M, WSAVE, LENSAV, IER)
      INTEGER L, M, LENSAV, IER
      INTEGER LWSAV,MWSAV,MMSAV
      REAL WSAVE(LENSAV)
C
C INITIALIZE IER
C
      IER = 0
C
C VERIFY LENSAV
C
      LWSAV =   L+INT(LOG(REAL(L))/LOG(2.))+4
      MWSAV =   2*M+INT(LOG(REAL(M))/LOG(2.))+4
      MMSAV =   M+INT(LOG(REAL(M))/LOG(2.))+4
      IF (LENSAV .LT. LWSAV+MWSAV+MMSAV) THEN
        IER = 2
        CALL XERFFT ('RFFT2I', 4)
        GO TO 100
      ENDIF
C
      CALL RFFTMI (L, WSAVE(1), LWSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      ENDIF
      CALL CFFTMI (M, WSAVE(LWSAV+1),MWSAV,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      ENDIF
C
      CALL RFFTMI (M,WSAVE(LWSAV+MWSAV+1),MMSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('RFFT2I',-5)
        GO TO 100
      END IF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTB1 (N,IN,C,CH,WA,FAC)
      REAL       CH(*), C(IN,*), WA(N) ,FAC(15)
C
      NF = FAC(2)
      NA = 0
      DO 10 K1=1,NF
      IP = FAC(K1+2)
      NA = 1-NA      
      IF(IP .LE. 5) GO TO 10
      IF(K1 .EQ. NF) GO TO 10
      NA = 1-NA
   10 CONTINUE 
      HALF = .5
      HALFM = -.5
      MODN = MOD(N,2)
      NL = N-2
      IF(MODN .NE. 0) NL = N-1
      IF (NA .EQ. 0) GO TO 120
      CH(1) = C(1,1)
      CH(N) = C(1,N)
      DO 118 J=2,NL,2
	 CH(J) = HALF*C(1,J)
	 CH(J+1) = HALFM*C(1,J+1)
  118 CONTINUE
      GO TO 124
  120 DO 122 J=2,NL,2
	 C(1,J) = HALF*C(1,J)
	 C(1,J+1) = HALFM*C(1,J+1)
  122 CONTINUE
  124 L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = FAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDL1 = IDO*L1
         IF (IP .NE. 4) GO TO 103
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
         CALL R1F4KB (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),WA(IX3))
         GO TO 102
  101    CALL R1F4KB (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),WA(IX3))
  102    NA = 1-NA
         GO TO 115
  103    IF (IP .NE. 2) GO TO 106
         IF (NA .NE. 0) GO TO 104
	 CALL R1F2KB (IDO,L1,C,IN,CH,1,WA(IW))
         GO TO 105
  104    CALL R1F2KB (IDO,L1,CH,1,C,IN,WA(IW))
  105    NA = 1-NA
         GO TO 115
  106    IF (IP .NE. 3) GO TO 109
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 107
	 CALL R1F3KB (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2))
         GO TO 108
  107    CALL R1F3KB (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2))
  108    NA = 1-NA
         GO TO 115
  109    IF (IP .NE. 5) GO TO 112
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 110
         CALL R1F5KB (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),
     1                  WA(IX3),WA(IX4))
         GO TO 111
  110    CALL R1F5KB (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),
     1                  WA(IX3),WA(IX4))
  111    NA = 1-NA
         GO TO 115
  112    IF (NA .NE. 0) GO TO 113
	 CALL R1FGKB (IDO,IP,L1,IDL1,C,C,C,IN,CH,CH,1,WA(IW))
         GO TO 114
  113    CALL R1FGKB (IDO,IP,L1,IDL1,CH,CH,CH,1,C,C,IN,WA(IW))
  114    IF (IDO .EQ. 1) NA = 1-NA
  115    L1 = L2
         IW = IW+(IP-1)*IDO
  116 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTF1 (N,IN,C,CH,WA,FAC)
      REAL       CH(*) ,C(IN,*)  ,WA(N)   ,FAC(15)
C
      NF = FAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1=1,NF
         KH = NF-K1
         IP = FAC(KH+3)
         L1 = L2/IP
         IDO = N/L2
         IDL1 = IDO*L1
         IW = IW-(IP-1)*IDO
         NA = 1-NA
         IF (IP .NE. 4) GO TO 102
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IF (NA .NE. 0) GO TO 101
	 CALL R1F4KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
  101    CALL R1F4KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),WA(IX3))
         GO TO 110
  102    IF (IP .NE. 2) GO TO 104
         IF (NA .NE. 0) GO TO 103
	 CALL R1F2KF (IDO,L1,C,IN,CH,1,WA(IW))
         GO TO 110
  103    CALL R1F2KF (IDO,L1,CH,1,C,IN,WA(IW))
         GO TO 110
  104    IF (IP .NE. 3) GO TO 106
         IX2 = IW+IDO
         IF (NA .NE. 0) GO TO 105
	 CALL R1F3KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2))
         GO TO 110
  105    CALL R1F3KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2))
         GO TO 110
  106    IF (IP .NE. 5) GO TO 108
         IX2 = IW+IDO
         IX3 = IX2+IDO
         IX4 = IX3+IDO
         IF (NA .NE. 0) GO TO 107
         CALL R1F5KF (IDO,L1,C,IN,CH,1,WA(IW),WA(IX2),
     1                      WA(IX3),WA(IX4))
         GO TO 110
  107    CALL R1F5KF (IDO,L1,CH,1,C,IN,WA(IW),WA(IX2),
     1                      WA(IX3),WA(IX4))
         GO TO 110
  108    IF (IDO .EQ. 1) NA = 1-NA
         IF (NA .NE. 0) GO TO 109
	 CALL R1FGKF (IDO,IP,L1,IDL1,C,C,C,IN,CH,CH,1,WA(IW))
         NA = 1
         GO TO 110
  109    CALL R1FGKF (IDO,IP,L1,IDL1,CH,CH,CH,1,C,C,IN,WA(IW))
         NA = 0
  110    L2 = L1
  111 CONTINUE
      SN = 1./N
      TSN = 2./N
      TSNM = -TSN
      MODN = MOD(N,2)
      NL = N-2
      IF(MODN .NE. 0) NL = N-1
      IF (NA .NE. 0) GO TO 120
      C(1,1) = SN*CH(1)
      DO 118 J=2,NL,2
	 C(1,J) = TSN*CH(J)
	 C(1,J+1) = TSNM*CH(J+1)
  118 CONTINUE
      IF(MODN .NE. 0) RETURN
      C(1,N) = SN*CH(N)
      RETURN
  120 C(1,1) = SN*C(1,1)
      DO 122 J=2,NL,2
	 C(1,J) = TSN*C(1,J)
	 C(1,J+1) = TSNM*C(1,J+1)
  122 CONTINUE
      IF(MODN .NE. 0) RETURN
      C(1,N) = SN*C(1,N)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTI1 (N,WA,FAC)
      REAL       WA(N)      ,FAC(15)
      INTEGER    NTRYH(4)
      DOUBLE PRECISION TPI,ARGH,ARGLD,ARG
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4)/4,2,3,5/
C
      NL = N
      NF = 0
      J = 0
  101 J = J+1
      IF (J-4) 102,102,103
  102 NTRY = NTRYH(J)
      GO TO 104
  103 NTRY = NTRY+2
  104 NQ = NL/NTRY
      NR = NL-NTRY*NQ
      IF (NR) 101,105,101
  105 NF = NF+1
      FAC(NF+2) = NTRY
      NL = NQ
      IF (NTRY .NE. 2) GO TO 107
      IF (NF .EQ. 1) GO TO 107
      DO 106 I=2,NF
         IB = NF-I+2
         FAC(IB+2) = FAC(IB+1)
  106 CONTINUE
      FAC(3) = 2
  107 IF (NL .NE. 1) GO TO 104
      FAC(1) = N
      FAC(2) = NF
      TPI = 8.D0*DATAN(1.D0)
      ARGH = TPI/FLOAT(N)
      IS = 0
      NFM1 = NF-1
      L1 = 1
      IF (NFM1 .EQ. 0) RETURN
      DO 110 K1=1,NFM1
         IP = FAC(K1+2)
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
	       WA(I-1) = DCOS(ARG)
	       WA(I) = DSIN(ARG)
  108       CONTINUE
            IS = IS+IDO
  109    CONTINUE
         L1 = L2
  110 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTMB (LOT, JUMP, N, INC, R, LENR, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  LOT, JUMP, N, INC, LENR, LENSAV, LENWRK, IER
      REAL     R(LENR), WSAVE(LENSAV)     ,WORK(LENWRK)
      LOGICAL  XERCON
C
      IER = 0
C
      IF (LENR .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('RFFTMB ', 6)
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFTMB ', 8)
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('RFFTMB ', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('RFFTMB ', -1)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL MRFTB1 (LOT,JUMP,N,INC,R,WORK,WSAVE,WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTMF (LOT, JUMP, N, INC, R, LENR, WSAVE, LENSAV,
     1                  WORK, LENWRK, IER)
      INTEGER  LOT, JUMP, N, INC, LENR, LENSAV, LENWRK, IER
      REAL     R(LENR), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL  XERCON
C
      IER = 0
C
      IF (LENR .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('RFFTMF ', 6)
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFTMF ', 8)
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('RFFTMF ', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('RFFTMF ', -1)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL MRFTF1 (LOT,JUMP,N,INC,R,WORK,WSAVE,WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RFFTMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('RFFTMI ', 3)
      ENDIF
C
      IF (N .EQ. 1) RETURN
C
      CALL MRFTI1 (N,WSAVE(1),WSAVE(N+1))
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQ1B ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINQ1B', 6)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQ1B', 8)
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('SINQ1B', 10)
      ENDIF
C
      IF (N .GT. 1) GO TO 101
C     X(1,1) = 4.*X(1,1) line disabled by Dick Valent 08/26/2010
      RETURN
  101 NS2 = N/2
      DO 102 K=2,N,2
         X(1,K) = -X(1,K)
  102 CONTINUE
      CALL COSQ1B (N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQ1B',-5)
        GO TO 300
      ENDIF
      DO 103 K=1,NS2
         KC = N-K
         XHOLD = X(1,K)
         X(1,K) = X(1,KC+1)
         X(1,KC+1) = XHOLD
  103 CONTINUE
  300 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQ1F ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINQ1F', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQ1F', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. N) THEN
        IER = 3
        CALL XERFFT ('SINQ1F', 10)
        GO TO 300
      ENDIF
C
      IF (N .EQ. 1) RETURN
      NS2 = N/2
      DO 101 K=1,NS2
         KC = N-K
         XHOLD = X(1,K)
         X(1,K) = X(1,KC+1)
         X(1,KC+1) = XHOLD
  101 CONTINUE
      CALL COSQ1F (N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQ1F',-5)
        GO TO 300
      ENDIF
      DO 102 K=2,N,2
         X(1,K) = -X(1,K)
  102 CONTINUE
  300 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQ1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQ1I', 3)
        GO TO 300
      ENDIF
C
      CALL COSQ1I (N, WSAVE, LENSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQ1I',-5)
      ENDIF
  300 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQMB (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINQMB', 6)
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQMB', 8)
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('SINQMB', 10)
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('SINQMB', -1)
      ENDIF
C
      LJ = (LOT-1)*JUMP+1
      IF (N .GT. 1) GO TO 101
      DO 201 M=1,LJ,JUMP
         X(M,1) = 4.*X(M,1)
 201  CONTINUE
      RETURN
  101 NS2 = N/2
      DO 102 K=2,N,2
         DO 202 M=1,LJ,JUMP
         X(M,K) = -X(M,K)
 202     CONTINUE
  102 CONTINUE
      CALL COSQMB (LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQMB',-5)
        GO TO 300
      ENDIF
      DO 103 K=1,NS2
         KC = N-K
         DO 203 M=1,LJ,JUMP
         XHOLD = X(M,K)
         X(M,K) = X(M,KC+1)
         X(M,KC+1) = XHOLD
 203     CONTINUE
  103 CONTINUE
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINQMF', 6)
        GO TO 300
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQMF', 8)
        GO TO 300
      ELSEIF (LENWRK .LT. LOT*N) THEN
        IER = 3
        CALL XERFFT ('SINQMF', 10)
        GO TO 300
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('SINQMF', -1)
        GO TO 300
      ENDIF
C
      IF (N .EQ. 1) RETURN
      NS2 = N/2
      LJ = (LOT-1)*JUMP+1
      DO 101 K=1,NS2
         KC = N-K
         DO 201 M=1,LJ,JUMP
         XHOLD = X(M,K)
         X(M,K) = X(M,KC+1)
         X(M,KC+1) = XHOLD
 201     CONTINUE
  101 CONTINUE
      CALL COSQMF (LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQMF',-5)
        GO TO 300
      ENDIF
      DO 102 K=2,N,2
         DO 202 M=1,LJ,JUMP
         X(M,K) = -X(M,K)
 202     CONTINUE
  102 CONTINUE
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINQMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINQMI', 3)
        GO TO 300
      ENDIF
C
      CALL COSQMI (N, WSAVE, LENSAV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINQMI',-5)
      ENDIF
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINT1B ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
C
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINT1B', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINT1B', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. (2*N+2)) THEN
        IER = 3
        CALL XERFFT ('SINT1B', 10)
        GO TO 100
      ENDIF
C
      CALL SINTB1(N,INC,X,WSAVE,WORK,WORK(N+2),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINT1B',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINT1F ( N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
C
      IER = 0
      IF (LENX .LT. INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINT1F', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINT1F', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. (2*N+2)) THEN
        IER = 3
        CALL XERFFT ('SINT1F', 10)
        GO TO 100
      ENDIF
C
      CALL SINTF1(N,INC,X,WSAVE,WORK,WORK(N+2),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINT1F',-5)
      ENDIF
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINT1I (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINT1I', 3)
        GO TO 300
      ENDIF
C
      PI = 4.*ATAN(1.)
      IF (N .LE. 1) RETURN
      NS2 = N/2
      NP1 = N+1
      DT = PI/FLOAT(NP1)
      DO 101 K=1,NS2
         WSAVE(K) = 2.*SIN(K*DT)
  101 CONTINUE
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) +4
      CALL RFFT1I (NP1, WSAVE(NS2+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINT1I',-5)
      ENDIF
C
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINTB1(N,INC,X,WSAVE,XH,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(*)
      DOUBLE PRECISION           DSUM
      IER = 0
      IF (N-2) 200,102,103
  102 SRT3S2 = SQRT(3.)/2.
      XHOLD = SRT3S2*(X(1,1)+X(1,2))
      X(1,2) = SRT3S2*(X(1,1)-X(1,2))
      X(1,1) = XHOLD
      GO TO 200
  103 NP1 = N+1
      NS2 = N/2
      DO 104 K=1,NS2
         KC = NP1-K
         T1 = X(1,K)-X(1,KC)
         T2 = WSAVE(K)*(X(1,K)+X(1,KC))
         XH(K+1) = T1+T2
         XH(KC+1) = T2-T1
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      XH(NS2+2) = 4.*X(1,NS2+1)
  124 XH(1) = 0.
      LNXH = NP1
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) + 4
      LNWK = NP1
C
      CALL RFFT1F(NP1,1,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,LNWK,IER1)     
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTB1',-5)
        GO TO 200
      ENDIF
C
      IF(MOD(NP1,2) .NE. 0) GO TO 30
      XH(NP1) = XH(NP1)+XH(NP1)
 30   FNP1S4 = FLOAT(NP1)/4.
         X(1,1) = FNP1S4*XH(1)
         DSUM = X(1,1)
      DO 105 I=3,N,2
            X(1,I-1) = FNP1S4*XH(I)
            DSUM = DSUM+FNP1S4*XH(I-1)
            X(1,I) = DSUM
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 200
         X(1,N) = FNP1S4*XH(N+1)
C
  200 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINTF1(N,INC,X,WSAVE,XH,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(*)
      DOUBLE PRECISION           DSUM
      IER = 0
      IF (N-2) 200,102,103
  102 SSQRT3 = 1./SQRT(3.)
      XHOLD = SSQRT3*(X(1,1)+X(1,2))
      X(1,2) = SSQRT3*(X(1,1)-X(1,2))
      X(1,1) = XHOLD
      GO TO 200
  103 NP1 = N+1
      NS2 = N/2
      DO 104 K=1,NS2
         KC = NP1-K
         T1 = X(1,K)-X(1,KC)
         T2 = WSAVE(K)*(X(1,K)+X(1,KC))
         XH(K+1) = T1+T2
         XH(KC+1) = T2-T1
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
      XH(NS2+2) = 4.*X(1,NS2+1)
  124 XH(1) = 0.
      LNXH = NP1
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) + 4
      LNWK = NP1
C
      CALL RFFT1F(NP1,1,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,
     1            LNWK,IER1)     
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTF1',-5)
        GO TO 200
      ENDIF
C
      IF(MOD(NP1,2) .NE. 0) GO TO 30
      XH(NP1) = XH(NP1)+XH(NP1)
   30 SFNP1 = 1./FLOAT(NP1)
         X(1,1) = .5*XH(1)
         DSUM = X(1,1)
      DO 105 I=3,N,2
            X(1,I-1) = .5*XH(I)
            DSUM = DSUM+.5*XH(I-1)
            X(1,I) = DSUM
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 200
      X(1,N) = .5*XH(N+1)
  200 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINTMB (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINTMB', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINTMB', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. LOT*(2*N+4)) THEN
        IER = 3
        CALL XERFFT ('SINTMB', 10)
        GO TO 100
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('SINTMB', -1)
        GO TO 100
      ENDIF
C
      IW1 = LOT+LOT+1
      IW2 = IW1+LOT*(N+1)
      CALL MSNTB1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),WORK(IW2),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTMB',-5)
      ENDIF
C
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINTMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV, 
     1                   WORK, LENWRK, IER)
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK)
      LOGICAL    XERCON
C
      IER = 0
C
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN
        IER = 1
        CALL XERFFT ('SINTMF', 6)
        GO TO 100
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINTMF', 8)
        GO TO 100
      ELSEIF (LENWRK .LT. LOT*(2*N+4)) THEN
        IER = 3
        CALL XERFFT ('SINTMF', 10)
        GO TO 100
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN
        IER = 4
        CALL XERFFT ('SINTMF', -1)
        GO TO 100
      ENDIF
C
      IW1 = LOT+LOT+1
      IW2 = IW1+LOT*(N+1)
      CALL MSNTF1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),WORK(IW2),IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTMF',-5)
      ENDIF
  100 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SINTMI (N, WSAVE, LENSAV, IER)
      INTEGER    N, LENSAV, IER
      REAL       WSAVE(LENSAV)
C
      IER = 0
C
      IF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))/LOG(2.)) +4) THEN
        IER = 2
        CALL XERFFT ('SINTMI', 3)
        GO TO 300
      ENDIF
C
      PI = 4.*ATAN(1.)
      IF (N .LE. 1) RETURN
      NS2 = N/2
      NP1 = N+1
      DT = PI/FLOAT(NP1)
      DO 101 K=1,NS2
         WSAVE(K) = 2.*SIN(K*DT)
  101 CONTINUE
      LNSV = NP1 + INT(LOG(REAL(NP1))/LOG(2.)) +4
      CALL RFFTMI (NP1, WSAVE(NS2+1), LNSV, IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('SINTMI',-5)
      ENDIF
C
  300 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE TABLES (IDO,IP,WA)
      REAL  WA(IDO,IP-1,2)
C
      TPI = 8.*ATAN(1.)
      ARGZ = TPI/REAL(IP)
      ARG1 = TPI/REAL(IDO*IP)
      DO 110 J=2,IP
         ARG2 = REAL(J-1)*ARG1
         DO 100 I=1,IDO
            ARG3 = REAL(I-1)*ARG2 
            WA(I,J-1,1) = COS(ARG3)
            WA(I,J-1,2) = SIN(ARG3)
  100    CONTINUE
         IF (IP .LE. 5) GO TO 110
         ARG4 = REAL(J-1)*ARGZ
         WA(1,J-1,1) = COS(ARG4)
         WA(1,J-1,2) = SIN(ARG4)
  110 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1
C
C   AUTHORS:  PAUL N. SWARZTRAUBER AND RICHARD A. VALENT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine w2r(ldr,ldw,l,m,r,w)
      dimension r(ldr,*),w(ldw,*)
      do j=1,m
      do i=1,l
      r(i,j) = w( i,j)
      end do
      end do
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      LOGICAL FUNCTION XERCON (INC,JUMP,N,LOT)
      INTEGER INC, JUMP, N, LOT
      INTEGER I, J, JNEW, LCM
C
C     Definition: positive integers INC, JUMP, N and LOT are consistent 
C                                                            ----------
C     if I1*INC + J1*JUMP = I2*INC + J2*JUMP for I1,I2 < N and J1,J2 
C     < LOT implies I1=I2 and J1=J2.
C
C     For multiple FFTs to execute correctly, input parameters INC, 
C     JUMP, N and LOT must be consistent ... otherwise at least one 
C     array element mistakenly is transformed more than once.
C
C     XERCON = .TRUE. if and only if INC, JUMP, N and LOT are 
C     consistent.
C
C     ------------------------------------------------------------------
C
C     Compute I = greatest common divisor (INC, JUMP)
C
      I = INC
      J = JUMP
   10 CONTINUE
      IF (J .NE. 0) THEN
        JNEW = MOD(I,J)
        I    = J
        J    = JNEW
        GO TO 10
      ENDIF
C
C Compute LCM = least common multiple (INC, JUMP)
C
      LCM = (INC*JUMP)/I
C
C Check consistency of INC, JUMP, N, LOT
C
      IF (LCM .LE. (N-1)*INC .AND. LCM .LE. (LOT-1)*JUMP) THEN
        XERCON = .FALSE.
      ELSE
        XERCON = .TRUE.
      ENDIF
C
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.1 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE XERFFT( SRNAME, INFO)
C
C     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
C
C     ..
C
C  Purpose
C  =======
C
C  XERFFT  is an error handler for library FFTPACK version 5.1 routines.
C  It is called by an FFTPACK 5.1 routine if an input parameter has an
C  invalid value.  A message is printed and execution stops.
C
C  Installers may consider modifying the STOP statement in order to
C  call system-specific exception-handling facilities.
C
C  Arguments
C  =========
C
C  SRNAME  (input) CHARACTER*6
C          The name of the routine which called XERFFT.
C
C  INFO    (input) INTEGER
C          When a single  invalid parameter in the parameter list of
C          the calling routine has been detected, INFO is the position
C          of that parameter.  In the case when an illegal combination
C          of LOT, JUMP, N, and INC has been detected, the calling
C          subprogram calls XERFFT with INFO = -1.
C
C =====================================================================
C
C     .. Executable Statements ..
C
      IF (INFO .GE. 1) THEN
        WRITE( *, '(A,A,A,I3,A)') ' ** On entry to ', SRNAME,
     1    ' parameter number ', INFO, ' had an illegal value'
      ELSEIF (INFO .EQ. -1) THEN
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,
     1    ' parameters LOT, JUMP, N and INC are inconsistent'
      ELSEIF (INFO .EQ. -2) THEN
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,
     1    ' parameter L is greater than LDIM'
      ELSEIF (INFO .EQ. -3) THEN
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,
     1    ' parameter M is greater than MDIM'
      ELSEIF (INFO .EQ. -5) THEN
        WRITE( *, '(A,A,A,A)') ' ** Within ', SRNAME,
     1    ' input error returned by lower level routine'
      ELSEIF (INFO .EQ. -6) THEN
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,
     1    ' parameter LDIM is less than 2*(L/2+1)'
      ENDIF
C
      STOP
C
C     End of XERFFT
C
      END
