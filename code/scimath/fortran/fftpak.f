C 
C                          FFTPACK
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
C certian other symmetric sequences that are listed below.
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
C
C 	---R.C. Singleton multi-dimensional
C          FFT routines. Modified and added by Philippe Lachlan 
C          McLean, 7 April 1995.
C
C          The following two functions perform multidimensional 
C          complex Fast Fourier Transforms. Real and imaginary
C          components are passed as separate arrays a and b,
C          and the magnitude of isn can specify the indexing skip
C          between successive elements in the arrays. See the 
C          documention at the subroutine definition.
C          Parameters at,bk,ck,sk,
C          np, and nfac are work arrays.
C          
C     
C      subroutine mfft(a,b,ntot,n,nspan,isn,at,bt,ck,sk,np,nfac)
C      subroutine mdfft(a,b,ntot,n,nspan,isn,at,bt,ck,sk,np,nfac)
C          ( double precision version of mfft )
C
C 
C ******************************************************************
C 
C subroutine rffti(n,wsave)
C 
C   ****************************************************************
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
C 
C 

C
C These subroutines are from the FFTPAK collection
C by Paul N. Swarztrauber
C
      subroutine cffti (n,wsave)

      real wsave(*)
      integer n
      integer iw1, iw2

      if (n .eq. 1) return
      iw1 = n+n+1
      iw2 = iw1+n+n
      call cffti1 (n,wsave(iw1),wsave(iw2))
      return
      end

      subroutine cffti1 (n,wa,ifac)

      real wa(*)
      integer n, ifac(*)

      integer ntryh(4), nl, nf, ntry, nq, nr, i, j, ib, l1, l2, ld, k1 
      integer ip, ido, idot, ipm, i1, ii
      real tpi, argh, argld, fi, arg
      data ntryh(1),ntryh(2),ntryh(3),ntryh(4)/3,4,2,5/

      nl = n
      nf = 0
      j = 0
  101 j = j+1
      if (j-4) 102,102,103
  102 ntry = ntryh(j)
      go to 104
  103 ntry = ntry+2
  104 nq = nl/ntry
      nr = nl-ntry*nq
      if (nr) 101,105,101
  105 nf = nf+1
      ifac(nf+2) = ntry
      nl = nq
      if (ntry .ne. 2) go to 107
      if (nf .eq. 1) go to 107
      do 106 i=2,nf
         ib = nf-i+2
         ifac(ib+2) = ifac(ib+1)
  106 continue
      ifac(3) = 2
  107 if (nl .ne. 1) go to 104
      ifac(1) = n
      ifac(2) = nf
      tpi = 6.28318530717959
      argh = tpi/float(n)
      i = 2
      l1 = 1
      do 110 k1=1,nf
         ip = ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         idot = ido+ido+2
         ipm = ip-1
         do 109 j=1,ipm
            i1 = i
            wa(i-1) = 1.
            wa(i) = 0.
            ld = ld+l1
            fi = 0.
            argld = float(ld)*argh
            do 108 ii=4,idot,2
               i = i+2
               fi = fi+1.
               arg = fi*argld
               wa(i-1) = cos(arg)
               wa(i) = sin(arg)
  108       continue
            if (ip .le. 5) go to 109
            wa(i1-1) = wa(i-1)
            wa(i1) = wa(i)
  109    continue
         l1 = l2
  110 continue
      return
      end

      subroutine cfftf (n,c,wsave)

      integer n
      real c(*), wsave(*)

      integer iw1, iw2, iw3
      logical zerochk

      if (n .eq. 1) return
      iw3 = n+n
      if (zerochk(iw3,c)) return
      iw1 = n+n+1
      iw2 = iw1+n+n
      call cfftf1 (n,c,wsave,wsave(iw1),wsave(iw2))
      return
      end

      subroutine cfftf1 (n,c,ch,wa,ifac)

      integer n, ifac(*)
      real ch(*), c(*), wa(*)

      integer nf, na, l1, l2, iw, k1, ip, ido, idot, idl1, ix2, ix3, ix4
      integer nac, n2, i

      nf = ifac(2)
      na = 0
      l1 = 1
      iw = 1
      do 116 k1=1,nf
         ip = ifac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idot = ido+ido
         idl1 = idot*l1
         if (ip .ne. 4) go to 103
         ix2 = iw+idot
         ix3 = ix2+idot
         if (na .ne. 0) go to 101
         call passf4 (idot,l1,c,ch,wa(iw),wa(ix2),wa(ix3))
         go to 102
  101    call passf4 (idot,l1,ch,c,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na
         go to 115
  103    if (ip .ne. 2) go to 106
         if (na .ne. 0) go to 104
         call passf2 (idot,l1,c,ch,wa(iw))
         go to 105
  104    call passf2 (idot,l1,ch,c,wa(iw))
  105    na = 1-na
         go to 115
  106    if (ip .ne. 3) go to 109
         ix2 = iw+idot
         if (na .ne. 0) go to 107
         call passf3 (idot,l1,c,ch,wa(iw),wa(ix2))
         go to 108
  107    call passf3 (idot,l1,ch,c,wa(iw),wa(ix2))
  108    na = 1-na
         go to 115
  109    if (ip .ne. 5) go to 112
         ix2 = iw+idot
         ix3 = ix2+idot
         ix4 = ix3+idot
         if (na .ne. 0) go to 110
         call passf5 (idot,l1,c,ch,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111
  110    call passf5 (idot,l1,ch,c,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na
         go to 115
  112    if (na .ne. 0) go to 113
         call passf (nac,idot,ip,l1,idl1,c,c,c,ch,ch,wa(iw))
         go to 114
  113    call passf (nac,idot,ip,l1,idl1,ch,ch,ch,c,c,wa(iw))
  114    if (nac .ne. 0) na = 1-na
  115    l1 = l2
         iw = iw+(ip-1)*idot
  116 continue
      if (na .eq. 0) return
      n2 = n+n
      do 117 i=1,n2
         c(i) = ch(i)
  117 continue
      return
      end
      
      subroutine cfftb (n,c,wsave)

      real c(*), wsave(*)
      integer n

      integer iw1, iw2, iw3
      logical zerochk

      if (n .eq. 1) return
      iw3 = n+n
      if (zerochk(iw3,c)) return
      iw1 = n+n+1
      iw2 = iw1+n+n
      call cfftb1 (n,c,wsave,wsave(iw1),wsave(iw2))
      return
      end

      subroutine cfftb1 (n,c,ch,wa,ifac)

      integer n, ifac(*)
      real ch(*), c(*), wa(*)

      integer nf, na, l1, l2, iw, k1, ip, nac, n2, i, ido, idot, idl1
      integer ix2, ix3, ix4

      nf = ifac(2)
      na = 0
      l1 = 1
      iw = 1
      do 116 k1=1,nf
         ip = ifac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idot = ido+ido
         idl1 = idot*l1
         if (ip .ne. 4) go to 103
         ix2 = iw+idot
         ix3 = ix2+idot
         if (na .ne. 0) go to 101
         call passb4 (idot,l1,c,ch,wa(iw),wa(ix2),wa(ix3))
         go to 102
  101    call passb4 (idot,l1,ch,c,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na
         go to 115
  103    if (ip .ne. 2) go to 106
         if (na .ne. 0) go to 104
         call passb2 (idot,l1,c,ch,wa(iw))
         go to 105
  104    call passb2 (idot,l1,ch,c,wa(iw))
  105    na = 1-na
         go to 115
  106    if (ip .ne. 3) go to 109
         ix2 = iw+idot
         if (na .ne. 0) go to 107
         call passb3 (idot,l1,c,ch,wa(iw),wa(ix2))
         go to 108
  107    call passb3 (idot,l1,ch,c,wa(iw),wa(ix2))
  108    na = 1-na
         go to 115
  109    if (ip .ne. 5) go to 112
         ix2 = iw+idot
         ix3 = ix2+idot
         ix4 = ix3+idot
         if (na .ne. 0) go to 110
         call passb5 (idot,l1,c,ch,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111
  110    call passb5 (idot,l1,ch,c,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na
         go to 115
  112    if (na .ne. 0) go to 113
         call passb (nac,idot,ip,l1,idl1,c,c,c,ch,ch,wa(iw))
         go to 114
  113    call passb (nac,idot,ip,l1,idl1,ch,ch,ch,c,c,wa(iw))
  114    if (nac .ne. 0) na = 1-na
  115    l1 = l2
         iw = iw+(ip-1)*idot
  116 continue
      if (na .eq. 0) return
      n2 = n+n
      do 117 i=1,n2
         c(i) = ch(i)
  117 continue
      return
      end

      subroutine passf (nac,ido,ip,l1,idl1,cc,c1,c2,ch,ch2,wa)

      integer nac, ido, ip, l1, idl1
      real	      ch(ido,l1,ip)          ,cc(ido,ip,l1)          ,
     1                c1(ido,l1,ip)          ,wa(*)      ,c2(idl1,ip),
     2                ch2(idl1,ip)

      integer idot, nt, ipp2, ipph, idp, j, jc, i, k, idl, inc, l, lc
      integer ik, idlj, idij, idj
      real war, wai

      idot = ido/2
      nt = ip*idl1
      ipp2 = ip+2
      ipph = (ip+1)/2
      idp = ip*ido
c
      if (ido .lt. l1) go to 106
      do 103 j=2,ipph
         jc = ipp2-j
         do 102 k=1,l1
            do 101 i=1,ido
               ch(i,k,j) = cc(i,j,k)+cc(i,jc,k)
               ch(i,k,jc) = cc(i,j,k)-cc(i,jc,k)
  101       continue
  102    continue
  103 continue
      do 105 k=1,l1
         do 104 i=1,ido
            ch(i,k,1) = cc(i,1,k)
  104    continue
  105 continue
      go to 112
  106 do 109 j=2,ipph
         jc = ipp2-j
         do 108 i=1,ido
            do 107 k=1,l1
               ch(i,k,j) = cc(i,j,k)+cc(i,jc,k)
               ch(i,k,jc) = cc(i,j,k)-cc(i,jc,k)
  107       continue
  108    continue
  109 continue
      do 111 i=1,ido
         do 110 k=1,l1
            ch(i,k,1) = cc(i,1,k)
  110    continue
  111 continue
  112 idl = 2-ido
      inc = 0
      do 116 l=2,ipph
         lc = ipp2-l
         idl = idl+ido
         do 113 ik=1,idl1
            c2(ik,l) = ch2(ik,1)+wa(idl-1)*ch2(ik,2)
            c2(ik,lc) = -wa(idl)*ch2(ik,ip)
  113    continue
         idlj = idl
         inc = inc+ido
         do 115 j=3,ipph
            jc = ipp2-j
            idlj = idlj+inc
            if (idlj .gt. idp) idlj = idlj-idp
            war = wa(idlj-1)
            wai = wa(idlj)
            do 114 ik=1,idl1
               c2(ik,l) = c2(ik,l)+war*ch2(ik,j)
               c2(ik,lc) = c2(ik,lc)-wai*ch2(ik,jc)
  114       continue
  115    continue
  116 continue
      do 118 j=2,ipph
         do 117 ik=1,idl1
            ch2(ik,1) = ch2(ik,1)+ch2(ik,j)
  117    continue
  118 continue
      do 120 j=2,ipph
         jc = ipp2-j
         do 119 ik=2,idl1,2
            ch2(ik-1,j) = c2(ik-1,j)-c2(ik,jc)
            ch2(ik-1,jc) = c2(ik-1,j)+c2(ik,jc)
            ch2(ik,j) = c2(ik,j)+c2(ik-1,jc)
            ch2(ik,jc) = c2(ik,j)-c2(ik-1,jc)
  119    continue
  120 continue
      nac = 1
      if (ido .eq. 2) return
      nac = 0
      do 121 ik=1,idl1
         c2(ik,1) = ch2(ik,1)
  121 continue
      do 123 j=2,ip
         do 122 k=1,l1
            c1(1,k,j) = ch(1,k,j)
            c1(2,k,j) = ch(2,k,j)
  122    continue
  123 continue
      if (idot .gt. l1) go to 127
      idij = 0
      do 126 j=2,ip
         idij = idij+2
         do 125 i=4,ido,2
            idij = idij+2
            do 124 k=1,l1
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)+wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)-wa(idij)*ch(i-1,k,j)
  124       continue
  125    continue
  126 continue
      return
  127 idj = 2-ido
      do 130 j=2,ip
         idj = idj+ido
         do 129 k=1,l1
            idij = idj
            do 128 i=4,ido,2
               idij = idij+2
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)+wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)-wa(idij)*ch(i-1,k,j)
  128       continue
  129    continue
  130 continue
      return
      end

      subroutine passf2 (ido,l1,cc,ch,wa1)

      integer ido, l1
      real	       cc(ido,2,l1)           ,ch(ido,l1,2)           ,
     1                wa1(*)

      integer i, k
      real tr2, ti2

      if (ido .gt. 2) go to 102
      do 101 k=1,l1
         ch(1,k,1) = cc(1,1,k)+cc(1,2,k)
         ch(1,k,2) = cc(1,1,k)-cc(1,2,k)
         ch(2,k,1) = cc(2,1,k)+cc(2,2,k)
         ch(2,k,2) = cc(2,1,k)-cc(2,2,k)
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ch(i-1,k,1) = cc(i-1,1,k)+cc(i-1,2,k)
            tr2 = cc(i-1,1,k)-cc(i-1,2,k)
            ch(i,k,1) = cc(i,1,k)+cc(i,2,k)
            ti2 = cc(i,1,k)-cc(i,2,k)
            ch(i,k,2) = wa1(i-1)*ti2-wa1(i)*tr2
            ch(i-1,k,2) = wa1(i-1)*tr2+wa1(i)*ti2
  103    continue
  104 continue
      return
      end

      subroutine passf3 (ido,l1,cc,ch,wa1,wa2)

      integer ido, l1
      real	       cc(ido,3,l1)           ,ch(ido,l1,3)           ,
     1                wa1(*)     ,wa2(*)

      integer i, k
      real tr2, ti2, cr2, ci2, taur, taui, cr3, ci3, dr2, di2, dr3, di3

      data taur,taui /-.5,-.866025403784439/

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         tr2 = cc(1,2,k)+cc(1,3,k)
         cr2 = cc(1,1,k)+taur*tr2
         ch(1,k,1) = cc(1,1,k)+tr2
         ti2 = cc(2,2,k)+cc(2,3,k)
         ci2 = cc(2,1,k)+taur*ti2
         ch(2,k,1) = cc(2,1,k)+ti2
         cr3 = taui*(cc(1,2,k)-cc(1,3,k))
         ci3 = taui*(cc(2,2,k)-cc(2,3,k))
         ch(1,k,2) = cr2-ci3
         ch(1,k,3) = cr2+ci3
         ch(2,k,2) = ci2+cr3
         ch(2,k,3) = ci2-cr3
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            tr2 = cc(i-1,2,k)+cc(i-1,3,k)
            cr2 = cc(i-1,1,k)+taur*tr2
            ch(i-1,k,1) = cc(i-1,1,k)+tr2
            ti2 = cc(i,2,k)+cc(i,3,k)
            ci2 = cc(i,1,k)+taur*ti2
            ch(i,k,1) = cc(i,1,k)+ti2
            cr3 = taui*(cc(i-1,2,k)-cc(i-1,3,k))
            ci3 = taui*(cc(i,2,k)-cc(i,3,k))
            dr2 = cr2-ci3
            dr3 = cr2+ci3
            di2 = ci2+cr3
            di3 = ci2-cr3
            ch(i,k,2) = wa1(i-1)*di2-wa1(i)*dr2
            ch(i-1,k,2) = wa1(i-1)*dr2+wa1(i)*di2
            ch(i,k,3) = wa2(i-1)*di3-wa2(i)*dr3
            ch(i-1,k,3) = wa2(i-1)*dr3+wa2(i)*di3
  103    continue
  104 continue
      return
      end

      subroutine passf4 (ido,l1,cc,ch,wa1,wa2,wa3)

      integer ido, l1
      real	       cc(ido,4,l1)           ,ch(ido,l1,4)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)

      integer i, k
      real tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4
      real cr2, ci2, cr3, ci3, cr4, ci4

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         ti1 = cc(2,1,k)-cc(2,3,k)
         ti2 = cc(2,1,k)+cc(2,3,k)
         tr4 = cc(2,2,k)-cc(2,4,k)
         ti3 = cc(2,2,k)+cc(2,4,k)
         tr1 = cc(1,1,k)-cc(1,3,k)
         tr2 = cc(1,1,k)+cc(1,3,k)
         ti4 = cc(1,4,k)-cc(1,2,k)
         tr3 = cc(1,2,k)+cc(1,4,k)
         ch(1,k,1) = tr2+tr3
         ch(1,k,3) = tr2-tr3
         ch(2,k,1) = ti2+ti3
         ch(2,k,3) = ti2-ti3
         ch(1,k,2) = tr1+tr4
         ch(1,k,4) = tr1-tr4
         ch(2,k,2) = ti1+ti4
         ch(2,k,4) = ti1-ti4
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ti1 = cc(i,1,k)-cc(i,3,k)
            ti2 = cc(i,1,k)+cc(i,3,k)
            ti3 = cc(i,2,k)+cc(i,4,k)
            tr4 = cc(i,2,k)-cc(i,4,k)
            tr1 = cc(i-1,1,k)-cc(i-1,3,k)
            tr2 = cc(i-1,1,k)+cc(i-1,3,k)
            ti4 = cc(i-1,4,k)-cc(i-1,2,k)
            tr3 = cc(i-1,2,k)+cc(i-1,4,k)
            ch(i-1,k,1) = tr2+tr3
            cr3 = tr2-tr3
            ch(i,k,1) = ti2+ti3
            ci3 = ti2-ti3
            cr2 = tr1+tr4
            cr4 = tr1-tr4
            ci2 = ti1+ti4
            ci4 = ti1-ti4
            ch(i-1,k,2) = wa1(i-1)*cr2+wa1(i)*ci2
            ch(i,k,2) = wa1(i-1)*ci2-wa1(i)*cr2
            ch(i-1,k,3) = wa2(i-1)*cr3+wa2(i)*ci3
            ch(i,k,3) = wa2(i-1)*ci3-wa2(i)*cr3
            ch(i-1,k,4) = wa3(i-1)*cr4+wa3(i)*ci4
            ch(i,k,4) = wa3(i-1)*ci4-wa3(i)*cr4
  103    continue
  104 continue
      return
      end

      subroutine passf5 (ido,l1,cc,ch,wa1,wa2,wa3,wa4)

      integer ido, l1
      real	      cc(ido,5,l1)           ,ch(ido,l1,5)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)     ,wa4(*)

      integer i, k
      real cr2, ci2, cr3, ci3, cr4, ci4, cr5, ci5
      real dr2, di2, dr3, di3, dr4, di4, dr5, di5
      real tr2, ti2, tr3, ti3, tr4, ti4, tr5, ti5
      real tr11, ti11, tr12, ti12

      data tr11,ti11,tr12,ti12 /.309016994374947,-.951056516295154,
     1-.809016994374947,-.587785252292473/

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         ti5 = cc(2,2,k)-cc(2,5,k)
         ti2 = cc(2,2,k)+cc(2,5,k)
         ti4 = cc(2,3,k)-cc(2,4,k)
         ti3 = cc(2,3,k)+cc(2,4,k)
         tr5 = cc(1,2,k)-cc(1,5,k)
         tr2 = cc(1,2,k)+cc(1,5,k)
         tr4 = cc(1,3,k)-cc(1,4,k)
         tr3 = cc(1,3,k)+cc(1,4,k)
         ch(1,k,1) = cc(1,1,k)+tr2+tr3
         ch(2,k,1) = cc(2,1,k)+ti2+ti3
         cr2 = cc(1,1,k)+tr11*tr2+tr12*tr3
         ci2 = cc(2,1,k)+tr11*ti2+tr12*ti3
         cr3 = cc(1,1,k)+tr12*tr2+tr11*tr3
         ci3 = cc(2,1,k)+tr12*ti2+tr11*ti3
         cr5 = ti11*tr5+ti12*tr4
         ci5 = ti11*ti5+ti12*ti4
         cr4 = ti12*tr5-ti11*tr4
         ci4 = ti12*ti5-ti11*ti4
         ch(1,k,2) = cr2-ci5
         ch(1,k,5) = cr2+ci5
         ch(2,k,2) = ci2+cr5
         ch(2,k,3) = ci3+cr4
         ch(1,k,3) = cr3-ci4
         ch(1,k,4) = cr3+ci4
         ch(2,k,4) = ci3-cr4
         ch(2,k,5) = ci2-cr5
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ti5 = cc(i,2,k)-cc(i,5,k)
            ti2 = cc(i,2,k)+cc(i,5,k)
            ti4 = cc(i,3,k)-cc(i,4,k)
            ti3 = cc(i,3,k)+cc(i,4,k)
            tr5 = cc(i-1,2,k)-cc(i-1,5,k)
            tr2 = cc(i-1,2,k)+cc(i-1,5,k)
            tr4 = cc(i-1,3,k)-cc(i-1,4,k)
            tr3 = cc(i-1,3,k)+cc(i-1,4,k)
            ch(i-1,k,1) = cc(i-1,1,k)+tr2+tr3
            ch(i,k,1) = cc(i,1,k)+ti2+ti3
            cr2 = cc(i-1,1,k)+tr11*tr2+tr12*tr3
            ci2 = cc(i,1,k)+tr11*ti2+tr12*ti3
            cr3 = cc(i-1,1,k)+tr12*tr2+tr11*tr3
            ci3 = cc(i,1,k)+tr12*ti2+tr11*ti3
            cr5 = ti11*tr5+ti12*tr4
            ci5 = ti11*ti5+ti12*ti4
            cr4 = ti12*tr5-ti11*tr4
            ci4 = ti12*ti5-ti11*ti4
            dr3 = cr3-ci4
            dr4 = cr3+ci4
            di3 = ci3+cr4
            di4 = ci3-cr4
            dr5 = cr2+ci5
            dr2 = cr2-ci5
            di5 = ci2-cr5
            di2 = ci2+cr5
            ch(i-1,k,2) = wa1(i-1)*dr2+wa1(i)*di2
            ch(i,k,2) = wa1(i-1)*di2-wa1(i)*dr2
            ch(i-1,k,3) = wa2(i-1)*dr3+wa2(i)*di3
            ch(i,k,3) = wa2(i-1)*di3-wa2(i)*dr3
            ch(i-1,k,4) = wa3(i-1)*dr4+wa3(i)*di4
            ch(i,k,4) = wa3(i-1)*di4-wa3(i)*dr4
            ch(i-1,k,5) = wa4(i-1)*dr5+wa4(i)*di5
            ch(i,k,5) = wa4(i-1)*di5-wa4(i)*dr5
  103    continue
  104 continue
      return
      end

      subroutine passb (nac,ido,ip,l1,idl1,cc,c1,c2,ch,ch2,wa)

      integer nac, ido, ip, l1, idl1
      real 	      ch(ido,l1,ip)          ,cc(ido,ip,l1)          ,
     1                c1(ido,l1,ip)          ,wa(*)      ,c2(idl1,ip),
     2                ch2(idl1,ip)

      integer idot, nt, ipp2, ipph, idp, j, jc, k, i, idl, inc, l, lc
      integer ik, idlj, idij, idj
      real war, wai

      idot = ido/2
      nt = ip*idl1
      ipp2 = ip+2
      ipph = (ip+1)/2
      idp = ip*ido
c
      if (ido .lt. l1) go to 106
      do 103 j=2,ipph
         jc = ipp2-j
         do 102 k=1,l1
            do 101 i=1,ido
               ch(i,k,j) = cc(i,j,k)+cc(i,jc,k)
               ch(i,k,jc) = cc(i,j,k)-cc(i,jc,k)
  101       continue
  102    continue
  103 continue
      do 105 k=1,l1
         do 104 i=1,ido
            ch(i,k,1) = cc(i,1,k)
  104    continue
  105 continue
      go to 112
  106 do 109 j=2,ipph
         jc = ipp2-j
         do 108 i=1,ido
            do 107 k=1,l1
               ch(i,k,j) = cc(i,j,k)+cc(i,jc,k)
               ch(i,k,jc) = cc(i,j,k)-cc(i,jc,k)
  107       continue
  108    continue
  109 continue
      do 111 i=1,ido
         do 110 k=1,l1
            ch(i,k,1) = cc(i,1,k)
  110    continue
  111 continue
  112 idl = 2-ido
      inc = 0
      do 116 l=2,ipph
         lc = ipp2-l
         idl = idl+ido
         do 113 ik=1,idl1
            c2(ik,l) = ch2(ik,1)+wa(idl-1)*ch2(ik,2)
            c2(ik,lc) = wa(idl)*ch2(ik,ip)
  113    continue
         idlj = idl
         inc = inc+ido
         do 115 j=3,ipph
            jc = ipp2-j
            idlj = idlj+inc
            if (idlj .gt. idp) idlj = idlj-idp
            war = wa(idlj-1)
            wai = wa(idlj)
            do 114 ik=1,idl1
               c2(ik,l) = c2(ik,l)+war*ch2(ik,j)
               c2(ik,lc) = c2(ik,lc)+wai*ch2(ik,jc)
  114       continue
  115    continue
  116 continue
      do 118 j=2,ipph
         do 117 ik=1,idl1
            ch2(ik,1) = ch2(ik,1)+ch2(ik,j)
  117    continue
  118 continue
      do 120 j=2,ipph
         jc = ipp2-j
         do 119 ik=2,idl1,2
            ch2(ik-1,j) = c2(ik-1,j)-c2(ik,jc)
            ch2(ik-1,jc) = c2(ik-1,j)+c2(ik,jc)
            ch2(ik,j) = c2(ik,j)+c2(ik-1,jc)
            ch2(ik,jc) = c2(ik,j)-c2(ik-1,jc)
  119    continue
  120 continue
      nac = 1
      if (ido .eq. 2) return
      nac = 0
      do 121 ik=1,idl1
         c2(ik,1) = ch2(ik,1)
  121 continue
      do 123 j=2,ip
         do 122 k=1,l1
            c1(1,k,j) = ch(1,k,j)
            c1(2,k,j) = ch(2,k,j)
  122    continue
  123 continue
      if (idot .gt. l1) go to 127
      idij = 0
      do 126 j=2,ip
         idij = idij+2
         do 125 i=4,ido,2
            idij = idij+2
            do 124 k=1,l1
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
  124       continue
  125    continue
  126 continue
      return
  127 idj = 2-ido
      do 130 j=2,ip
         idj = idj+ido
         do 129 k=1,l1
            idij = idj
            do 128 i=4,ido,2
               idij = idij+2
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
  128       continue
  129    continue
  130 continue
      return
      end

      subroutine passb2 (ido,l1,cc,ch,wa1)

      integer ido, l1
      real		cc(ido,2,l1)           ,ch(ido,l1,2)           ,
     1                wa1(*)

      integer k, i
      real tr2, ti2

      if (ido .gt. 2) go to 102
      do 101 k=1,l1
         ch(1,k,1) = cc(1,1,k)+cc(1,2,k)
         ch(1,k,2) = cc(1,1,k)-cc(1,2,k)
         ch(2,k,1) = cc(2,1,k)+cc(2,2,k)
         ch(2,k,2) = cc(2,1,k)-cc(2,2,k)
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ch(i-1,k,1) = cc(i-1,1,k)+cc(i-1,2,k)
            tr2 = cc(i-1,1,k)-cc(i-1,2,k)
            ch(i,k,1) = cc(i,1,k)+cc(i,2,k)
            ti2 = cc(i,1,k)-cc(i,2,k)
            ch(i,k,2) = wa1(i-1)*ti2+wa1(i)*tr2
            ch(i-1,k,2) = wa1(i-1)*tr2-wa1(i)*ti2
  103    continue
  104 continue
      return
      end

      subroutine passb3 (ido,l1,cc,ch,wa1,wa2)

      integer ido, l1
      real		cc(ido,3,l1)           ,ch(ido,l1,3)           ,
     1                wa1(*)     ,wa2(*)

      integer k, i
      real tr2, ti2, cr2, ci2, cr3, ci3, dr2, di2, dr3, di3, taur, taui

      data taur,taui /-.5,.866025403784439/

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         tr2 = cc(1,2,k)+cc(1,3,k)
         cr2 = cc(1,1,k)+taur*tr2
         ch(1,k,1) = cc(1,1,k)+tr2
         ti2 = cc(2,2,k)+cc(2,3,k)
         ci2 = cc(2,1,k)+taur*ti2
         ch(2,k,1) = cc(2,1,k)+ti2
         cr3 = taui*(cc(1,2,k)-cc(1,3,k))
         ci3 = taui*(cc(2,2,k)-cc(2,3,k))
         ch(1,k,2) = cr2-ci3
         ch(1,k,3) = cr2+ci3
         ch(2,k,2) = ci2+cr3
         ch(2,k,3) = ci2-cr3
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            tr2 = cc(i-1,2,k)+cc(i-1,3,k)
            cr2 = cc(i-1,1,k)+taur*tr2
            ch(i-1,k,1) = cc(i-1,1,k)+tr2
            ti2 = cc(i,2,k)+cc(i,3,k)
            ci2 = cc(i,1,k)+taur*ti2
            ch(i,k,1) = cc(i,1,k)+ti2
            cr3 = taui*(cc(i-1,2,k)-cc(i-1,3,k))
            ci3 = taui*(cc(i,2,k)-cc(i,3,k))
            dr2 = cr2-ci3
            dr3 = cr2+ci3
            di2 = ci2+cr3
            di3 = ci2-cr3
            ch(i,k,2) = wa1(i-1)*di2+wa1(i)*dr2
            ch(i-1,k,2) = wa1(i-1)*dr2-wa1(i)*di2
            ch(i,k,3) = wa2(i-1)*di3+wa2(i)*dr3
            ch(i-1,k,3) = wa2(i-1)*dr3-wa2(i)*di3
  103    continue
  104 continue
      return
      end

      subroutine passb4 (ido,l1,cc,ch,wa1,wa2,wa3)

      integer ido, l1
      real 		cc(ido,4,l1)           ,ch(ido,l1,4)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)
                 
      integer k, i
      real cr2, ci2, cr3, ci3, cr4, ci4, tr1, ti1, tr2, ti2, tr3, ti3
      real tr4, ti4

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         ti1 = cc(2,1,k)-cc(2,3,k)
         ti2 = cc(2,1,k)+cc(2,3,k)
         tr4 = cc(2,4,k)-cc(2,2,k)
         ti3 = cc(2,2,k)+cc(2,4,k)
         tr1 = cc(1,1,k)-cc(1,3,k)
         tr2 = cc(1,1,k)+cc(1,3,k)
         ti4 = cc(1,2,k)-cc(1,4,k)
         tr3 = cc(1,2,k)+cc(1,4,k)
         ch(1,k,1) = tr2+tr3
         ch(1,k,3) = tr2-tr3
         ch(2,k,1) = ti2+ti3
         ch(2,k,3) = ti2-ti3
         ch(1,k,2) = tr1+tr4
         ch(1,k,4) = tr1-tr4
         ch(2,k,2) = ti1+ti4
         ch(2,k,4) = ti1-ti4
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ti1 = cc(i,1,k)-cc(i,3,k)
            ti2 = cc(i,1,k)+cc(i,3,k)
            ti3 = cc(i,2,k)+cc(i,4,k)
            tr4 = cc(i,4,k)-cc(i,2,k)
            tr1 = cc(i-1,1,k)-cc(i-1,3,k)
            tr2 = cc(i-1,1,k)+cc(i-1,3,k)
            ti4 = cc(i-1,2,k)-cc(i-1,4,k)
            tr3 = cc(i-1,2,k)+cc(i-1,4,k)
            ch(i-1,k,1) = tr2+tr3
            cr3 = tr2-tr3
            ch(i,k,1) = ti2+ti3
            ci3 = ti2-ti3
            cr2 = tr1+tr4
            cr4 = tr1-tr4
            ci2 = ti1+ti4
            ci4 = ti1-ti4
            ch(i-1,k,2) = wa1(i-1)*cr2-wa1(i)*ci2
            ch(i,k,2) = wa1(i-1)*ci2+wa1(i)*cr2
            ch(i-1,k,3) = wa2(i-1)*cr3-wa2(i)*ci3
            ch(i,k,3) = wa2(i-1)*ci3+wa2(i)*cr3
            ch(i-1,k,4) = wa3(i-1)*cr4-wa3(i)*ci4
            ch(i,k,4) = wa3(i-1)*ci4+wa3(i)*cr4
  103    continue
  104 continue
      return
      end

      subroutine passb5 (ido,l1,cc,ch,wa1,wa2,wa3,wa4)

      integer ido, l1
      real 		cc(ido,5,l1)           ,ch(ido,l1,5)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)     ,wa4(*)
           
      integer k, i
      real cr2, ci2, cr3, ci3, cr4, ci4, cr5, ci5, tr2, ti2, tr3, ti3
      real tr4, ti4, tr5, ti5, tr11, ti11, tr12, ti12, dr2, di2
      real dr3, di3, dr4, di4, dr5, di5

      data tr11,ti11,tr12,ti12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/

      if (ido .ne. 2) go to 102
      do 101 k=1,l1
         ti5 = cc(2,2,k)-cc(2,5,k)
         ti2 = cc(2,2,k)+cc(2,5,k)
         ti4 = cc(2,3,k)-cc(2,4,k)
         ti3 = cc(2,3,k)+cc(2,4,k)
         tr5 = cc(1,2,k)-cc(1,5,k)
         tr2 = cc(1,2,k)+cc(1,5,k)
         tr4 = cc(1,3,k)-cc(1,4,k)
         tr3 = cc(1,3,k)+cc(1,4,k)
         ch(1,k,1) = cc(1,1,k)+tr2+tr3
         ch(2,k,1) = cc(2,1,k)+ti2+ti3
         cr2 = cc(1,1,k)+tr11*tr2+tr12*tr3
         ci2 = cc(2,1,k)+tr11*ti2+tr12*ti3
         cr3 = cc(1,1,k)+tr12*tr2+tr11*tr3
         ci3 = cc(2,1,k)+tr12*ti2+tr11*ti3
         cr5 = ti11*tr5+ti12*tr4
         ci5 = ti11*ti5+ti12*ti4
         cr4 = ti12*tr5-ti11*tr4
         ci4 = ti12*ti5-ti11*ti4
         ch(1,k,2) = cr2-ci5
         ch(1,k,5) = cr2+ci5
         ch(2,k,2) = ci2+cr5
         ch(2,k,3) = ci3+cr4
         ch(1,k,3) = cr3-ci4
         ch(1,k,4) = cr3+ci4
         ch(2,k,4) = ci3-cr4
         ch(2,k,5) = ci2-cr5
  101 continue
      return
  102 do 104 k=1,l1
         do 103 i=2,ido,2
            ti5 = cc(i,2,k)-cc(i,5,k)
            ti2 = cc(i,2,k)+cc(i,5,k)
            ti4 = cc(i,3,k)-cc(i,4,k)
            ti3 = cc(i,3,k)+cc(i,4,k)
            tr5 = cc(i-1,2,k)-cc(i-1,5,k)
            tr2 = cc(i-1,2,k)+cc(i-1,5,k)
            tr4 = cc(i-1,3,k)-cc(i-1,4,k)
            tr3 = cc(i-1,3,k)+cc(i-1,4,k)
            ch(i-1,k,1) = cc(i-1,1,k)+tr2+tr3
            ch(i,k,1) = cc(i,1,k)+ti2+ti3
            cr2 = cc(i-1,1,k)+tr11*tr2+tr12*tr3
            ci2 = cc(i,1,k)+tr11*ti2+tr12*ti3
            cr3 = cc(i-1,1,k)+tr12*tr2+tr11*tr3
            ci3 = cc(i,1,k)+tr12*ti2+tr11*ti3
            cr5 = ti11*tr5+ti12*tr4
            ci5 = ti11*ti5+ti12*ti4
            cr4 = ti12*tr5-ti11*tr4
            ci4 = ti12*ti5-ti11*ti4
            dr3 = cr3-ci4
            dr4 = cr3+ci4
            di3 = ci3+cr4
            di4 = ci3-cr4
            dr5 = cr2+ci5
            dr2 = cr2-ci5
            di5 = ci2-cr5
            di2 = ci2+cr5
            ch(i-1,k,2) = wa1(i-1)*dr2-wa1(i)*di2
            ch(i,k,2) = wa1(i-1)*di2+wa1(i)*dr2
            ch(i-1,k,3) = wa2(i-1)*dr3-wa2(i)*di3
            ch(i,k,3) = wa2(i-1)*di3+wa2(i)*dr3
            ch(i-1,k,4) = wa3(i-1)*dr4-wa3(i)*di4
            ch(i,k,4) = wa3(i-1)*di4+wa3(i)*dr4
            ch(i-1,k,5) = wa4(i-1)*dr5-wa4(i)*di5
            ch(i,k,5) = wa4(i-1)*di5+wa4(i)*dr5
  103    continue
  104 continue
      return
      end

      subroutine rffti (n,wsave)

      integer n
      real       wsave(*)

      if (n .eq. 1) return
      call rffti1 (n,wsave(n+1),wsave(2*n+1))
      return
      end

      subroutine rffti1 (n,wa,ifac)

      integer n, ifac(*) 
      real wa(*)

      integer nl, nf, j, ntry, nq, nr, i, ib, is, nfm1, l1, k1, ip
      integer ld, l2, ido, ipm, ii

      real ntryh(4), tpi, argh, argld, fi, arg

      data ntryh(1),ntryh(2),ntryh(3),ntryh(4)/4,2,3,5/

      nl = n
      nf = 0
      j = 0
  101 j = j+1
      if (j-4) 102,102,103
  102 ntry = ntryh(j)
      go to 104
  103 ntry = ntry+2
  104 nq = nl/ntry
      nr = nl-ntry*nq
      if (nr) 101,105,101
  105 nf = nf+1
      ifac(nf+2) = ntry
      nl = nq
      if (ntry .ne. 2) go to 107
      if (nf .eq. 1) go to 107
      do 106 i=2,nf
         ib = nf-i+2
         ifac(ib+2) = ifac(ib+1)
  106 continue
      ifac(3) = 2
  107 if (nl .ne. 1) go to 104                
      ifac(1) = n
      ifac(2) = nf
      tpi = 6.28318530717959
      argh = tpi/float(n)
      is = 0
      nfm1 = nf-1
      l1 = 1
      if (nfm1 .eq. 0) return
      do 110 k1=1,nfm1
         ip = ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         ipm = ip-1
         do 109 j=1,ipm
            ld = ld+l1
            i = is
            argld = float(ld)*argh
            fi = 0.
            do 108 ii=3,ido,2
               i = i+2
               fi = fi+1.
               arg = fi*argld
               wa(i-1) = cos(arg)
               wa(i) = sin(arg)
  108       continue
            is = is+ido
  109    continue
         l1 = l2
  110 continue
      return
      end

      subroutine rfftf (n,r,wsave)

      integer n
      real       r(*)       ,wsave(*)
      logical zerochk

      if (n .eq. 1) return
      if (zerochk(n,r)) return
      call rfftf1 (n,r,wsave,wsave(n+1),wsave(2*n+1))
      return
      end

      subroutine rfftf1 (n,c,ch,wa,ifac)

      integer n, ifac(*)
      real       ch(*)      ,c(*)       ,wa(*)

      integer nf, na, l2, iw, k1, kh, ip, l1, ido, idl1, ix2, ix3
      integer ix4, i

      nf = ifac(2)
      na = 1
      l2 = n
      iw = n
      do 111 k1=1,nf
         kh = nf-k1
         ip = ifac(kh+3)
         l1 = l2/ip
         ido = n/l2
         idl1 = ido*l1
         iw = iw-(ip-1)*ido
         na = 1-na
         if (ip .ne. 4) go to 102
         ix2 = iw+ido
         ix3 = ix2+ido
         if (na .ne. 0) go to 101
         call radf4 (ido,l1,c,ch,wa(iw),wa(ix2),wa(ix3))
         go to 110
  101    call radf4 (ido,l1,ch,c,wa(iw),wa(ix2),wa(ix3))
         go to 110
  102    if (ip .ne. 2) go to 104
         if (na .ne. 0) go to 103
         call radf2 (ido,l1,c,ch,wa(iw))
         go to 110
  103    call radf2 (ido,l1,ch,c,wa(iw))
         go to 110
  104    if (ip .ne. 3) go to 106
         ix2 = iw+ido
         if (na .ne. 0) go to 105
         call radf3 (ido,l1,c,ch,wa(iw),wa(ix2))
         go to 110
  105    call radf3 (ido,l1,ch,c,wa(iw),wa(ix2))
         go to 110
  106    if (ip .ne. 5) go to 108
         ix2 = iw+ido
         ix3 = ix2+ido
         ix4 = ix3+ido
         if (na .ne. 0) go to 107
         call radf5 (ido,l1,c,ch,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110
  107    call radf5 (ido,l1,ch,c,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110
  108    if (ido .eq. 1) na = 1-na
         if (na .ne. 0) go to 109
         call radfg (ido,ip,l1,idl1,c,c,c,ch,ch,wa(iw))
         na = 1
         go to 110
  109    call radfg (ido,ip,l1,idl1,ch,ch,ch,c,c,wa(iw))
         na = 0
  110    l2 = l1
  111 continue
      if (na .eq. 1) return
      do 112 i=1,n
         c(i) = ch(i)
  112 continue
      return
      end

      subroutine rfftb (n,r,wsave)

      integer n
      real       r(*)       ,wsave(*)
      logical zerochk

      if (n .eq. 1) return
      if (zerochk(n,r)) return
      call rfftb1 (n,r,wsave,wsave(n+1),wsave(2*n+1))
      return
      end

      subroutine rfftb1 (n,c,ch,wa,ifac)

      integer n, ifac(*)
      real       ch(*)      ,c(*)       ,wa(*)

      integer nf, na, l1, l2, i, iw, k1, ip, ido, idl1, ix2, ix3, ix4

      nf = ifac(2)
      na = 0
      l1 = 1
      iw = 1
      do 116 k1=1,nf
         ip = ifac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idl1 = ido*l1
         if (ip .ne. 4) go to 103
         ix2 = iw+ido
         ix3 = ix2+ido
         if (na .ne. 0) go to 101
         call radb4 (ido,l1,c,ch,wa(iw),wa(ix2),wa(ix3))
         go to 102
  101    call radb4 (ido,l1,ch,c,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na
         go to 115
  103    if (ip .ne. 2) go to 106
         if (na .ne. 0) go to 104
         call radb2 (ido,l1,c,ch,wa(iw))
         go to 105
  104    call radb2 (ido,l1,ch,c,wa(iw))
  105    na = 1-na
         go to 115
  106    if (ip .ne. 3) go to 109
         ix2 = iw+ido
         if (na .ne. 0) go to 107
         call radb3 (ido,l1,c,ch,wa(iw),wa(ix2))
         go to 108
  107    call radb3 (ido,l1,ch,c,wa(iw),wa(ix2))
  108    na = 1-na
         go to 115
  109    if (ip .ne. 5) go to 112
         ix2 = iw+ido
         ix3 = ix2+ido
         ix4 = ix3+ido
         if (na .ne. 0) go to 110
         call radb5 (ido,l1,c,ch,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111
  110    call radb5 (ido,l1,ch,c,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na
         go to 115
  112    if (na .ne. 0) go to 113
         call radbg (ido,ip,l1,idl1,c,c,c,ch,ch,wa(iw))
         go to 114
  113    call radbg (ido,ip,l1,idl1,ch,ch,ch,c,c,wa(iw))
  114    if (ido .eq. 1) na = 1-na
  115    l1 = l2
         iw = iw+(ip-1)*ido
  116 continue
      if (na .eq. 0) return
      do 117 i=1,n
         c(i) = ch(i)
  117 continue
      return
      end

      subroutine radf2 (ido,l1,cc,ch,wa1)

      integer ido, l1
      real 		ch(ido,2,l1)           ,cc(ido,l1,2)           ,
     1                wa1(*)

      integer k, idp2, i, ic
      real tr2, ti2

      do 101 k=1,l1
         ch(1,1,k) = cc(1,k,1)+cc(1,k,2)
         ch(ido,2,k) = cc(1,k,1)-cc(1,k,2)
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            tr2 = wa1(i-2)*cc(i-1,k,2)+wa1(i-1)*cc(i,k,2)
            ti2 = wa1(i-2)*cc(i,k,2)-wa1(i-1)*cc(i-1,k,2)
            ch(i,1,k) = cc(i,k,1)+ti2
            ch(ic,2,k) = ti2-cc(i,k,1)
            ch(i-1,1,k) = cc(i-1,k,1)+tr2
            ch(ic-1,2,k) = cc(i-1,k,1)-tr2
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 do 106 k=1,l1
         ch(1,2,k) = -cc(ido,k,2)
         ch(ido,1,k) = cc(ido,k,1)
  106 continue
  107 return
      end

      subroutine radf3 (ido,l1,cc,ch,wa1,wa2)

      integer ido, l1
      real 	      ch(ido,3,l1)           ,cc(ido,l1,3)           ,
     1                wa1(*)     ,wa2(*)

      integer k, idp2, i, ic
      real cr2, ci2, dr2, di2, dr3, di3, tr2, ti2, tr3, ti3
      real taur, taui

      data taur,taui /-.5,.866025403784439/

      do 101 k=1,l1
         cr2 = cc(1,k,2)+cc(1,k,3)
         ch(1,1,k) = cc(1,k,1)+cr2
         ch(1,3,k) = taui*(cc(1,k,3)-cc(1,k,2))
         ch(ido,2,k) = cc(1,k,1)+taur*cr2
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            dr2 = wa1(i-2)*cc(i-1,k,2)+wa1(i-1)*cc(i,k,2)
            di2 = wa1(i-2)*cc(i,k,2)-wa1(i-1)*cc(i-1,k,2)
            dr3 = wa2(i-2)*cc(i-1,k,3)+wa2(i-1)*cc(i,k,3)
            di3 = wa2(i-2)*cc(i,k,3)-wa2(i-1)*cc(i-1,k,3)
            cr2 = dr2+dr3
            ci2 = di2+di3
            ch(i-1,1,k) = cc(i-1,k,1)+cr2
            ch(i,1,k) = cc(i,k,1)+ci2
            tr2 = cc(i-1,k,1)+taur*cr2
            ti2 = cc(i,k,1)+taur*ci2
            tr3 = taui*(di2-di3)
            ti3 = taui*(dr3-dr2)
            ch(i-1,3,k) = tr2+tr3
            ch(ic-1,2,k) = tr2-tr3
            ch(i,3,k) = ti2+ti3
            ch(ic,2,k) = ti3-ti2
  102    continue
  103 continue
      return
      end

      subroutine radf4 (ido,l1,cc,ch,wa1,wa2,wa3)

      integer ido, l1
      real 	      cc(ido,l1,4)           ,ch(ido,4,l1)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)

      integer k, idp2, i, ic
      real tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4, cr2, ci2, cr3, ci3
      real cr4, ci4, hsqt2

      data hsqt2 /.7071067811865475/

      do 101 k=1,l1
         tr1 = cc(1,k,2)+cc(1,k,4)
         tr2 = cc(1,k,1)+cc(1,k,3)
         ch(1,1,k) = tr1+tr2
         ch(ido,4,k) = tr2-tr1
         ch(ido,2,k) = cc(1,k,1)-cc(1,k,3)
         ch(1,3,k) = cc(1,k,4)-cc(1,k,2)
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            cr2 = wa1(i-2)*cc(i-1,k,2)+wa1(i-1)*cc(i,k,2)
            ci2 = wa1(i-2)*cc(i,k,2)-wa1(i-1)*cc(i-1,k,2)
            cr3 = wa2(i-2)*cc(i-1,k,3)+wa2(i-1)*cc(i,k,3)
            ci3 = wa2(i-2)*cc(i,k,3)-wa2(i-1)*cc(i-1,k,3)
            cr4 = wa3(i-2)*cc(i-1,k,4)+wa3(i-1)*cc(i,k,4)
            ci4 = wa3(i-2)*cc(i,k,4)-wa3(i-1)*cc(i-1,k,4)
            tr1 = cr2+cr4
            tr4 = cr4-cr2
            ti1 = ci2+ci4
            ti4 = ci2-ci4
            ti2 = cc(i,k,1)+ci3
            ti3 = cc(i,k,1)-ci3
            tr2 = cc(i-1,k,1)+cr3
            tr3 = cc(i-1,k,1)-cr3
            ch(i-1,1,k) = tr1+tr2
            ch(ic-1,4,k) = tr2-tr1
            ch(i,1,k) = ti1+ti2
            ch(ic,4,k) = ti1-ti2
            ch(i-1,3,k) = ti4+tr3
            ch(ic-1,2,k) = tr3-ti4
            ch(i,3,k) = tr4+ti3
            ch(ic,2,k) = tr4-ti3
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 continue
      do 106 k=1,l1
         ti1 = -hsqt2*(cc(ido,k,2)+cc(ido,k,4))
         tr1 = hsqt2*(cc(ido,k,2)-cc(ido,k,4))
         ch(ido,1,k) = tr1+cc(ido,k,1)
         ch(ido,3,k) = cc(ido,k,1)-tr1
         ch(1,2,k) = ti1-cc(ido,k,3)
         ch(1,4,k) = ti1+cc(ido,k,3)
  106 continue
  107 return
      end

      subroutine radf5 (ido,l1,cc,ch,wa1,wa2,wa3,wa4)

      integer ido, l1
      real 	      cc(ido,l1,5)           ,ch(ido,5,l1)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)     ,wa4(*)

      integer k, idp2, i, ic
      real dr2, di2, dr3, di3, dr4, di4, dr5, di5, cr2, ci2, cr3, ci3
      real cr4, ci4, cr5, ci5, tr2, ti2, tr3, ti3, tr4, ti4, tr5, ti5
      real tr11, ti11, tr12, ti12

      data tr11,ti11,tr12,ti12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/

      do 101 k=1,l1
         cr2 = cc(1,k,5)+cc(1,k,2)
         ci5 = cc(1,k,5)-cc(1,k,2)
         cr3 = cc(1,k,4)+cc(1,k,3)
         ci4 = cc(1,k,4)-cc(1,k,3)
         ch(1,1,k) = cc(1,k,1)+cr2+cr3
         ch(ido,2,k) = cc(1,k,1)+tr11*cr2+tr12*cr3
         ch(1,3,k) = ti11*ci5+ti12*ci4
         ch(ido,4,k) = cc(1,k,1)+tr12*cr2+tr11*cr3
         ch(1,5,k) = ti12*ci5-ti11*ci4
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            dr2 = wa1(i-2)*cc(i-1,k,2)+wa1(i-1)*cc(i,k,2)
            di2 = wa1(i-2)*cc(i,k,2)-wa1(i-1)*cc(i-1,k,2)
            dr3 = wa2(i-2)*cc(i-1,k,3)+wa2(i-1)*cc(i,k,3)
            di3 = wa2(i-2)*cc(i,k,3)-wa2(i-1)*cc(i-1,k,3)
            dr4 = wa3(i-2)*cc(i-1,k,4)+wa3(i-1)*cc(i,k,4)
            di4 = wa3(i-2)*cc(i,k,4)-wa3(i-1)*cc(i-1,k,4)
            dr5 = wa4(i-2)*cc(i-1,k,5)+wa4(i-1)*cc(i,k,5)
            di5 = wa4(i-2)*cc(i,k,5)-wa4(i-1)*cc(i-1,k,5)
            cr2 = dr2+dr5
            ci5 = dr5-dr2
            cr5 = di2-di5
            ci2 = di2+di5
            cr3 = dr3+dr4
            ci4 = dr4-dr3
            cr4 = di3-di4
            ci3 = di3+di4
            ch(i-1,1,k) = cc(i-1,k,1)+cr2+cr3
            ch(i,1,k) = cc(i,k,1)+ci2+ci3
            tr2 = cc(i-1,k,1)+tr11*cr2+tr12*cr3
            ti2 = cc(i,k,1)+tr11*ci2+tr12*ci3
            tr3 = cc(i-1,k,1)+tr12*cr2+tr11*cr3
            ti3 = cc(i,k,1)+tr12*ci2+tr11*ci3
            tr5 = ti11*cr5+ti12*cr4
            ti5 = ti11*ci5+ti12*ci4
            tr4 = ti12*cr5-ti11*cr4
            ti4 = ti12*ci5-ti11*ci4
            ch(i-1,3,k) = tr2+tr5
            ch(ic-1,2,k) = tr2-tr5
            ch(i,3,k) = ti2+ti5
            ch(ic,2,k) = ti5-ti2
            ch(i-1,5,k) = tr3+tr4
            ch(ic-1,4,k) = tr3-tr4
            ch(i,5,k) = ti3+ti4
            ch(ic,4,k) = ti4-ti3
  102    continue
  103 continue
      return
      end

      subroutine radfg (ido,ip,l1,idl1,cc,c1,c2,ch,ch2,wa)

      integer ido, ip, l1, idl1
      real 	      ch(ido,l1,ip)          ,cc(ido,ip,l1)          ,
     1                c1(ido,l1,ip)          ,c2(idl1,ip),
     2                ch2(idl1,ip)           ,wa(*)

      integer ipph, ipp2, idp2, nbd, ik, j, k, is, idij, i, jc, l, lc
      integer j2, ic
      real arg, tpi, dcp, dsp, ar1, ai1, ar1h, ar2, ai2, ar2h, dc2, ds2

      data tpi/6.28318530717959/

      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      ipph = (ip+1)/2
      ipp2 = ip+2
      idp2 = ido+2
      nbd = (ido-1)/2
      if (ido .eq. 1) go to 119
      do 101 ik=1,idl1
         ch2(ik,1) = c2(ik,1)
  101 continue
      do 103 j=2,ip
         do 102 k=1,l1
            ch(1,k,j) = c1(1,k,j)
  102    continue
  103 continue
      if (nbd .gt. l1) go to 107
      is = -ido
      do 106 j=2,ip
         is = is+ido
         idij = is
         do 105 i=3,ido,2
            idij = idij+2
            do 104 k=1,l1
               ch(i-1,k,j) = wa(idij-1)*c1(i-1,k,j)+wa(idij)*c1(i,k,j)
               ch(i,k,j) = wa(idij-1)*c1(i,k,j)-wa(idij)*c1(i-1,k,j)
  104       continue
  105    continue
  106 continue
      go to 111
  107 is = -ido
      do 110 j=2,ip
         is = is+ido
         do 109 k=1,l1
            idij = is
            do 108 i=3,ido,2
               idij = idij+2
               ch(i-1,k,j) = wa(idij-1)*c1(i-1,k,j)+wa(idij)*c1(i,k,j)
               ch(i,k,j) = wa(idij-1)*c1(i,k,j)-wa(idij)*c1(i-1,k,j)
  108       continue
  109    continue
  110 continue
  111 if (nbd .lt. l1) go to 115
      do 114 j=2,ipph
         jc = ipp2-j
         do 113 k=1,l1
            do 112 i=3,ido,2
               c1(i-1,k,j) = ch(i-1,k,j)+ch(i-1,k,jc)
               c1(i-1,k,jc) = ch(i,k,j)-ch(i,k,jc)
               c1(i,k,j) = ch(i,k,j)+ch(i,k,jc)
               c1(i,k,jc) = ch(i-1,k,jc)-ch(i-1,k,j)
  112       continue
  113    continue
  114 continue
      go to 121
  115 do 118 j=2,ipph
         jc = ipp2-j
         do 117 i=3,ido,2
            do 116 k=1,l1
               c1(i-1,k,j) = ch(i-1,k,j)+ch(i-1,k,jc)
               c1(i-1,k,jc) = ch(i,k,j)-ch(i,k,jc)
               c1(i,k,j) = ch(i,k,j)+ch(i,k,jc)
               c1(i,k,jc) = ch(i-1,k,jc)-ch(i-1,k,j)
  116       continue
  117    continue
  118 continue
      go to 121
  119 do 120 ik=1,idl1
         c2(ik,1) = ch2(ik,1)
  120 continue
  121 do 123 j=2,ipph
         jc = ipp2-j
         do 122 k=1,l1
            c1(1,k,j) = ch(1,k,j)+ch(1,k,jc)
            c1(1,k,jc) = ch(1,k,jc)-ch(1,k,j)
  122    continue
  123 continue
c
      ar1 = 1.
      ai1 = 0.
      do 127 l=2,ipph
         lc = ipp2-l
         ar1h = dcp*ar1-dsp*ai1
         ai1 = dcp*ai1+dsp*ar1
         ar1 = ar1h
         do 124 ik=1,idl1
            ch2(ik,l) = c2(ik,1)+ar1*c2(ik,2)
            ch2(ik,lc) = ai1*c2(ik,ip)
  124    continue
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do 126 j=3,ipph
            jc = ipp2-j
            ar2h = dc2*ar2-ds2*ai2
            ai2 = dc2*ai2+ds2*ar2
            ar2 = ar2h
            do 125 ik=1,idl1
               ch2(ik,l) = ch2(ik,l)+ar2*c2(ik,j)
               ch2(ik,lc) = ch2(ik,lc)+ai2*c2(ik,jc)
  125       continue
  126    continue
  127 continue
      do 129 j=2,ipph
         do 128 ik=1,idl1
            ch2(ik,1) = ch2(ik,1)+c2(ik,j)
  128    continue
  129 continue
c
      if (ido .lt. l1) go to 132
      do 131 k=1,l1
         do 130 i=1,ido
            cc(i,1,k) = ch(i,k,1)
  130    continue
  131 continue
      go to 135
  132 do 134 i=1,ido
         do 133 k=1,l1
            cc(i,1,k) = ch(i,k,1)
  133    continue
  134 continue
  135 do 137 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 136 k=1,l1
            cc(ido,j2-2,k) = ch(1,k,j)
            cc(1,j2-1,k) = ch(1,k,jc)
  136    continue
  137 continue
      if (ido .eq. 1) return
      if (nbd .lt. l1) go to 141
      do 140 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 139 k=1,l1
            do 138 i=3,ido,2
               ic = idp2-i
               cc(i-1,j2-1,k) = ch(i-1,k,j)+ch(i-1,k,jc)
               cc(ic-1,j2-2,k) = ch(i-1,k,j)-ch(i-1,k,jc)
               cc(i,j2-1,k) = ch(i,k,j)+ch(i,k,jc)
               cc(ic,j2-2,k) = ch(i,k,jc)-ch(i,k,j)
  138       continue
  139    continue
  140 continue
      return
  141 do 144 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 143 i=3,ido,2
            ic = idp2-i
            do 142 k=1,l1
               cc(i-1,j2-1,k) = ch(i-1,k,j)+ch(i-1,k,jc)
               cc(ic-1,j2-2,k) = ch(i-1,k,j)-ch(i-1,k,jc)
               cc(i,j2-1,k) = ch(i,k,j)+ch(i,k,jc)
               cc(ic,j2-2,k) = ch(i,k,jc)-ch(i,k,j)
  142       continue
  143    continue
  144 continue
      return
      end

      subroutine radb2 (ido,l1,cc,ch,wa1)

      integer ido, l1
      real 	      cc(ido,2,l1)           ,ch(ido,l1,2)           ,
     1                wa1(*)
                      
      integer k, idp2, i, ic
      real tr2, ti2

      do 101 k=1,l1
         ch(1,k,1) = cc(1,1,k)+cc(ido,2,k)
         ch(1,k,2) = cc(1,1,k)-cc(ido,2,k)
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            ch(i-1,k,1) = cc(i-1,1,k)+cc(ic-1,2,k)
            tr2 = cc(i-1,1,k)-cc(ic-1,2,k)
            ch(i,k,1) = cc(i,1,k)-cc(ic,2,k)
            ti2 = cc(i,1,k)+cc(ic,2,k)
            ch(i-1,k,2) = wa1(i-2)*tr2-wa1(i-1)*ti2
            ch(i,k,2) = wa1(i-2)*ti2+wa1(i-1)*tr2
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 do 106 k=1,l1
         ch(ido,k,1) = cc(ido,1,k)+cc(ido,1,k)
         ch(ido,k,2) = -(cc(1,2,k)+cc(1,2,k))
  106 continue
  107 return
      end

      subroutine radb3 (ido,l1,cc,ch,wa1,wa2)

      integer ido, l1
      real 	      cc(ido,3,l1)           ,ch(ido,l1,3)           ,
     1                wa1(*)     ,wa2(*)

      integer k, idp2, i, ic
      real tr2, ti2, cr2, ci2, cr3, ci3, dr2, di2, dr3, di3, taur, taui

      data taur,taui /-.5,.866025403784439/

      do 101 k=1,l1
         tr2 = cc(ido,2,k)+cc(ido,2,k)
         cr2 = cc(1,1,k)+taur*tr2
         ch(1,k,1) = cc(1,1,k)+tr2
         ci3 = taui*(cc(1,3,k)+cc(1,3,k))
         ch(1,k,2) = cr2-ci3
         ch(1,k,3) = cr2+ci3
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            tr2 = cc(i-1,3,k)+cc(ic-1,2,k)
            cr2 = cc(i-1,1,k)+taur*tr2
            ch(i-1,k,1) = cc(i-1,1,k)+tr2
            ti2 = cc(i,3,k)-cc(ic,2,k)
            ci2 = cc(i,1,k)+taur*ti2
            ch(i,k,1) = cc(i,1,k)+ti2
            cr3 = taui*(cc(i-1,3,k)-cc(ic-1,2,k))
            ci3 = taui*(cc(i,3,k)+cc(ic,2,k))
            dr2 = cr2-ci3
            dr3 = cr2+ci3
            di2 = ci2+cr3
            di3 = ci2-cr3
            ch(i-1,k,2) = wa1(i-2)*dr2-wa1(i-1)*di2
            ch(i,k,2) = wa1(i-2)*di2+wa1(i-1)*dr2
            ch(i-1,k,3) = wa2(i-2)*dr3-wa2(i-1)*di3
            ch(i,k,3) = wa2(i-2)*di3+wa2(i-1)*dr3
  102    continue
  103 continue
      return
      end

      subroutine radb4 (ido,l1,cc,ch,wa1,wa2,wa3)

      integer ido, l1
      real 	      cc(ido,4,l1)           ,ch(ido,l1,4)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)

      integer k, idp2, i, ic
      real tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4, cr2, ci2, cr3, ci3
      real cr4, ci4, sqrt2

      data sqrt2 /1.414213562373095/

      do 101 k=1,l1
         tr1 = cc(1,1,k)-cc(ido,4,k)
         tr2 = cc(1,1,k)+cc(ido,4,k)
         tr3 = cc(ido,2,k)+cc(ido,2,k)
         tr4 = cc(1,3,k)+cc(1,3,k)
         ch(1,k,1) = tr2+tr3
         ch(1,k,2) = tr1-tr4
         ch(1,k,3) = tr2-tr3
         ch(1,k,4) = tr1+tr4
  101 continue
      if (ido-2) 107,105,102
  102 idp2 = ido+2
      do 104 k=1,l1
         do 103 i=3,ido,2
            ic = idp2-i
            ti1 = cc(i,1,k)+cc(ic,4,k)
            ti2 = cc(i,1,k)-cc(ic,4,k)
            ti3 = cc(i,3,k)-cc(ic,2,k)
            tr4 = cc(i,3,k)+cc(ic,2,k)
            tr1 = cc(i-1,1,k)-cc(ic-1,4,k)
            tr2 = cc(i-1,1,k)+cc(ic-1,4,k)
            ti4 = cc(i-1,3,k)-cc(ic-1,2,k)
            tr3 = cc(i-1,3,k)+cc(ic-1,2,k)
            ch(i-1,k,1) = tr2+tr3
            cr3 = tr2-tr3
            ch(i,k,1) = ti2+ti3
            ci3 = ti2-ti3
            cr2 = tr1-tr4
            cr4 = tr1+tr4
            ci2 = ti1+ti4
            ci4 = ti1-ti4
            ch(i-1,k,2) = wa1(i-2)*cr2-wa1(i-1)*ci2
            ch(i,k,2) = wa1(i-2)*ci2+wa1(i-1)*cr2
            ch(i-1,k,3) = wa2(i-2)*cr3-wa2(i-1)*ci3
            ch(i,k,3) = wa2(i-2)*ci3+wa2(i-1)*cr3
            ch(i-1,k,4) = wa3(i-2)*cr4-wa3(i-1)*ci4
            ch(i,k,4) = wa3(i-2)*ci4+wa3(i-1)*cr4
  103    continue
  104 continue
      if (mod(ido,2) .eq. 1) return
  105 continue
      do 106 k=1,l1
         ti1 = cc(1,2,k)+cc(1,4,k)
         ti2 = cc(1,4,k)-cc(1,2,k)
         tr1 = cc(ido,1,k)-cc(ido,3,k)
         tr2 = cc(ido,1,k)+cc(ido,3,k)
         ch(ido,k,1) = tr2+tr2
         ch(ido,k,2) = sqrt2*(tr1-ti1)
         ch(ido,k,3) = ti2+ti2
         ch(ido,k,4) = -sqrt2*(tr1+ti1)
  106 continue
  107 return
      end

      subroutine radb5 (ido,l1,cc,ch,wa1,wa2,wa3,wa4)

      integer ido, l1
      real 	      cc(ido,5,l1)           ,ch(ido,l1,5)           ,
     1                wa1(*)     ,wa2(*)     ,wa3(*)     ,wa4(*)

      integer k, idp2, i, ic
      real tr2, ti2, tr3, ti3, tr4, ti4, tr5, ti5, cr2, ci2, cr3, ci3
      real cr4, ci4, cr5, ci5, dr2, di2, dr3, di3, dr4, di4, dr5, di5
      real tr11, ti11, tr12, ti12

      data tr11,ti11,tr12,ti12 /.309016994374947,.951056516295154,
     1-.809016994374947,.587785252292473/

      do 101 k=1,l1
         ti5 = cc(1,3,k)+cc(1,3,k)
         ti4 = cc(1,5,k)+cc(1,5,k)
         tr2 = cc(ido,2,k)+cc(ido,2,k)
         tr3 = cc(ido,4,k)+cc(ido,4,k)
         ch(1,k,1) = cc(1,1,k)+tr2+tr3
         cr2 = cc(1,1,k)+tr11*tr2+tr12*tr3
         cr3 = cc(1,1,k)+tr12*tr2+tr11*tr3
         ci5 = ti11*ti5+ti12*ti4
         ci4 = ti12*ti5-ti11*ti4
         ch(1,k,2) = cr2-ci5
         ch(1,k,3) = cr3-ci4
         ch(1,k,4) = cr3+ci4
         ch(1,k,5) = cr2+ci5
  101 continue
      if (ido .eq. 1) return
      idp2 = ido+2
      do 103 k=1,l1
         do 102 i=3,ido,2
            ic = idp2-i
            ti5 = cc(i,3,k)+cc(ic,2,k)
            ti2 = cc(i,3,k)-cc(ic,2,k)
            ti4 = cc(i,5,k)+cc(ic,4,k)
            ti3 = cc(i,5,k)-cc(ic,4,k)
            tr5 = cc(i-1,3,k)-cc(ic-1,2,k)
            tr2 = cc(i-1,3,k)+cc(ic-1,2,k)
            tr4 = cc(i-1,5,k)-cc(ic-1,4,k)
            tr3 = cc(i-1,5,k)+cc(ic-1,4,k)
            ch(i-1,k,1) = cc(i-1,1,k)+tr2+tr3
            ch(i,k,1) = cc(i,1,k)+ti2+ti3
            cr2 = cc(i-1,1,k)+tr11*tr2+tr12*tr3
            ci2 = cc(i,1,k)+tr11*ti2+tr12*ti3
            cr3 = cc(i-1,1,k)+tr12*tr2+tr11*tr3
            ci3 = cc(i,1,k)+tr12*ti2+tr11*ti3
            cr5 = ti11*tr5+ti12*tr4
            ci5 = ti11*ti5+ti12*ti4
            cr4 = ti12*tr5-ti11*tr4
            ci4 = ti12*ti5-ti11*ti4
            dr3 = cr3-ci4
            dr4 = cr3+ci4
            di3 = ci3+cr4
            di4 = ci3-cr4
            dr5 = cr2+ci5
            dr2 = cr2-ci5
            di5 = ci2-cr5
            di2 = ci2+cr5
            ch(i-1,k,2) = wa1(i-2)*dr2-wa1(i-1)*di2
            ch(i,k,2) = wa1(i-2)*di2+wa1(i-1)*dr2
            ch(i-1,k,3) = wa2(i-2)*dr3-wa2(i-1)*di3
            ch(i,k,3) = wa2(i-2)*di3+wa2(i-1)*dr3
            ch(i-1,k,4) = wa3(i-2)*dr4-wa3(i-1)*di4
            ch(i,k,4) = wa3(i-2)*di4+wa3(i-1)*dr4
            ch(i-1,k,5) = wa4(i-2)*dr5-wa4(i-1)*di5
            ch(i,k,5) = wa4(i-2)*di5+wa4(i-1)*dr5
  102    continue
  103 continue
      return
      end

      subroutine radbg (ido,ip,l1,idl1,cc,c1,c2,ch,ch2,wa)

      integer ido, ip, l1, idl1
      real 	      ch(ido,l1,ip)          ,cc(ido,ip,l1)          ,
     1                c1(ido,l1,ip)          ,c2(idl1,ip),
     2                ch2(idl1,ip)           ,wa(*)

      integer idp2, nbd, ipp2, ipph, k, i, j, jc, j2, ic, l, lc, ik
      integer is, idij
      real arg, tpi, dcp, dsp, ar1, ai1, ar1h, ar2, ai2, ar2h, dc2
      real ds2

      data tpi/6.28318530717959/

      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      idp2 = ido+2
      nbd = (ido-1)/2
      ipp2 = ip+2
      ipph = (ip+1)/2
      if (ido .lt. l1) go to 103
      do 102 k=1,l1
         do 101 i=1,ido
            ch(i,k,1) = cc(i,1,k)
  101    continue
  102 continue
      go to 106
  103 do 105 i=1,ido
         do 104 k=1,l1
            ch(i,k,1) = cc(i,1,k)
  104    continue
  105 continue
  106 do 108 j=2,ipph
         jc = ipp2-j
         j2 = j+j
         do 107 k=1,l1
            ch(1,k,j) = cc(ido,j2-2,k)+cc(ido,j2-2,k)
            ch(1,k,jc) = cc(1,j2-1,k)+cc(1,j2-1,k)
  107    continue
  108 continue
      if (ido .eq. 1) go to 116
      if (nbd .lt. l1) go to 112
      do 111 j=2,ipph
         jc = ipp2-j
         do 110 k=1,l1
            do 109 i=3,ido,2
               ic = idp2-i
               ch(i-1,k,j) = cc(i-1,2*j-1,k)+cc(ic-1,2*j-2,k)
               ch(i-1,k,jc) = cc(i-1,2*j-1,k)-cc(ic-1,2*j-2,k)
               ch(i,k,j) = cc(i,2*j-1,k)-cc(ic,2*j-2,k)
               ch(i,k,jc) = cc(i,2*j-1,k)+cc(ic,2*j-2,k)
  109       continue
  110    continue
  111 continue
      go to 116
  112 do 115 j=2,ipph
         jc = ipp2-j
         do 114 i=3,ido,2
            ic = idp2-i
            do 113 k=1,l1
               ch(i-1,k,j) = cc(i-1,2*j-1,k)+cc(ic-1,2*j-2,k)
               ch(i-1,k,jc) = cc(i-1,2*j-1,k)-cc(ic-1,2*j-2,k)
               ch(i,k,j) = cc(i,2*j-1,k)-cc(ic,2*j-2,k)
               ch(i,k,jc) = cc(i,2*j-1,k)+cc(ic,2*j-2,k)
  113       continue
  114    continue
  115 continue
  116 ar1 = 1.
      ai1 = 0.
      do 120 l=2,ipph
         lc = ipp2-l
         ar1h = dcp*ar1-dsp*ai1
         ai1 = dcp*ai1+dsp*ar1
         ar1 = ar1h
         do 117 ik=1,idl1
            c2(ik,l) = ch2(ik,1)+ar1*ch2(ik,2)
            c2(ik,lc) = ai1*ch2(ik,ip)
  117    continue
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do 119 j=3,ipph
            jc = ipp2-j
            ar2h = dc2*ar2-ds2*ai2
            ai2 = dc2*ai2+ds2*ar2
            ar2 = ar2h
            do 118 ik=1,idl1
               c2(ik,l) = c2(ik,l)+ar2*ch2(ik,j)
               c2(ik,lc) = c2(ik,lc)+ai2*ch2(ik,jc)
  118       continue
  119    continue
  120 continue
      do 122 j=2,ipph
         do 121 ik=1,idl1
            ch2(ik,1) = ch2(ik,1)+ch2(ik,j)
  121    continue
  122 continue
      do 124 j=2,ipph
         jc = ipp2-j
         do 123 k=1,l1
            ch(1,k,j) = c1(1,k,j)-c1(1,k,jc)
            ch(1,k,jc) = c1(1,k,j)+c1(1,k,jc)
  123    continue
  124 continue
      if (ido .eq. 1) go to 132
      if (nbd .lt. l1) go to 128
      do 127 j=2,ipph
         jc = ipp2-j
         do 126 k=1,l1
            do 125 i=3,ido,2
               ch(i-1,k,j) = c1(i-1,k,j)-c1(i,k,jc)
               ch(i-1,k,jc) = c1(i-1,k,j)+c1(i,k,jc)
               ch(i,k,j) = c1(i,k,j)+c1(i-1,k,jc)
               ch(i,k,jc) = c1(i,k,j)-c1(i-1,k,jc)
  125       continue
  126    continue
  127 continue
      go to 132
  128 do 131 j=2,ipph
         jc = ipp2-j
         do 130 i=3,ido,2
            do 129 k=1,l1
               ch(i-1,k,j) = c1(i-1,k,j)-c1(i,k,jc)
               ch(i-1,k,jc) = c1(i-1,k,j)+c1(i,k,jc)
               ch(i,k,j) = c1(i,k,j)+c1(i-1,k,jc)
               ch(i,k,jc) = c1(i,k,j)-c1(i-1,k,jc)
  129       continue
  130    continue
  131 continue
  132 continue
      if (ido .eq. 1) return
      do 133 ik=1,idl1
         c2(ik,1) = ch2(ik,1)
  133 continue
      do 135 j=2,ip
         do 134 k=1,l1
            c1(1,k,j) = ch(1,k,j)
  134    continue
  135 continue
      if (nbd .gt. l1) go to 139
      is = -ido
      do 138 j=2,ip
         is = is+ido
         idij = is
         do 137 i=3,ido,2
            idij = idij+2
            do 136 k=1,l1
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
  136       continue
  137    continue
  138 continue
      go to 143
  139 is = -ido
      do 142 j=2,ip
         is = is+ido
         do 141 k=1,l1
            idij = is
            do 140 i=3,ido,2
               idij = idij+2
               c1(i-1,k,j) = wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
               c1(i,k,j) = wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
  140       continue
  141    continue
  142 continue
  143 return
      end

      logical function zerochk(n,r)
      integer n
      real r(*)
     
      integer i

      zerochk = .TRUE.
      do 100 i = 1,n
         if (r(i) .NE. 0.0) then
            zerochk = .FALSE.
            return
         endif
  100 continue
      return
      end


      subroutine mfft(a,b,ntot,n,nspan,isn,at,bt,ck,sk,np,nfac)
      IMPLICIT NONE
c  multivariate complex fourier transform, computed in place
c    using mixed-radix fast fourier transform algorithm.
c  by r. c. singleton, stanford research institute, sept. 1968
c  arrays a and b originally hold the real and imaginary
c    components of the data, and return the real and
c    imaginary components of the resulting fourier coefficients.
c
c  modified by Philippe Lachlan McLean, 26 March 1995. The caller must
c     now pass work arrays of a suitable size. at, bt, ck, and sk
c     must be real arrays of size n; nfac and np are integer arrays.
c     nfac must be at least 16 in size; np must be of size n.
c     If storage is exceeded, isn is set to zero.
c
c  This modification removes the previous static limit on the size
c    of n. FFTs of abitrary size can be performed; it is up to 
c    the caller to pass suitably sized work arrays. The following
c    information about multi-dimensional transforms is still 
c    relevant.
c
c  multivariate data are indexed according to the fortran
c    array element successor function, without limit
c    on the number of implied multiple subscripts.
c    the subroutine is called once for each variate.
c    the calls for a multivariate transform may be in any order.
c  ntot is the total number of complex data values.
c  n is the dimension of the current variable.
c  nspan/n is the spacing of consecutive data values
c    while indexing the current variable.
c  the sign of isn determines the sign of the complex
c    exponential, and the magnitude of isn is normally one.
c  the magnitude of isn determines the indexing increment
c    between consecutive data values while using the 
c    nspan/n spacing
c  a tri-variate transform with a(n1,n2,n3), b(n1,n2,n3)
c    is computed by
c      call fft(a,b,n1*n2*n3,n1,n1,1,at,bt,ck,sk,np,nfac)
c      call fft(a,b,n1*n2*n3,n2,n1*n2,1,at,bt,ck,sk,np,nfac)
c      call fft(a,b,n1*n2*n3,n3,n1*n2*n3,1,at,bt,ck,sk,np,nfac)
c
c  Note that the work arrays must be the size of the current 
c    dimension, except nfac, which must be of size 16.
c
c  for a single-variate transform,
c    ntot = n = nspan = (number of complex data values), e.g.
c      call fft(a,b,n,n,n,1)
c  the data can alternatively be stored in a single complex array c
c    in standard fortran fashion, i.e. alternating real and imaginary
c    parts. then with most fortran compilers, the complex array c can
c    be equivalenced to a real array a, the magnitude of isn changed
c    to two to give correct indexing increment, and a(1) and a(2) used
c    to pass the initial addresses for the sequences of real and
c    imaginary values, e.g.
c       complex c(ntot)
c       real    a(2*ntot)
c       equivalence (c(1),a(1))
c       call fft(a(1),a(2),ntot,n,nspan,2,at,bt,ck,sk,np,nfac)
c  arrays at(maxf), ck(maxf), bt(maxf), sk(maxf), and np(maxp)
c    are used for temporary storage.  if the available storage
c    is insufficient, isn is set to 0.
c    maxf must be .ge. the maximum prime factor of n.
c    maxp must be .gt. the number of prime factors of n.
c    in addition, if the square-free portion k of n has two or
c    more prime factors, then maxp must be .ge. k-1.

      real a(*), b(*)
      integer n, ntot, nspan, isn
      real at(*), bt(*), ck(*), sk(*)
      integer nfac(*), np(*)

c  array storage in nfac for a maximum of 15 prime factors of n.
c  array storage for maximum prime factor of n
c    in at, ck, bt, and sk

c     Philippe McLean, 26 March 1995. Abitrary limits removed;
c     nfac, np, at, ck, bt, and sk are now passed as parameters.

      real    aa, aj, ajm, ajp, ak, akm, akp
      real    bb, bj, bjm, bjp, bk, bkm, bkp
      real    c1, c2, c3, c72, cd
      
      integer i, ii, inc
      integer j, jc, jf, jj
      integer k, k1, k2, k3, k4, kk, ks, kspan, kspnn, kt
      integer m, maxf, maxp
      integer nn, nt

      real    rad, radf, s1, s120, s2, s3, s72, sd
      

      equivalence (i,ii)
c     the following two constants should agree with the array dimensions.
      maxf=n
      maxp=n
      if(n .lt. 2) return
      inc=isn
      c72=0.30901699437494742
      s72=0.95105651629515357
      s120=0.86602540378443865
      rad=6.2831853071796
      if(isn .ge. 0) go to 10
      s72=-s72
      s120=-s120
      rad=-rad
      inc=-inc
   10 nt=inc*ntot
      ks=inc*nspan
      kspan=ks
      nn=nt-inc
      jc=ks/n
      radf=rad*float(jc)*0.5
      i=0
      jf=0
c  determine the factors of n
      m=0
      k=n
      go to 20
   15 m=m+1
      nfac(m)=4
      k=k/16
   20 if(k-(k/16)*16 .eq. 0) go to 15
      j=3
      jj=9
      go to 30
   25 m=m+1
      nfac(m)=j
      k=k/jj
   30 if(mod(k,jj) .eq. 0) go to 25
      j=j+2
      jj=j**2
      if(jj .le. k) go to 30
      if(k .gt. 4) go to 40
      kt=m
      nfac(m+1)=k
      if(k .ne. 1) m=m+1
      go to 80
   40 if(k-(k/4)*4 .ne. 0) go to 50
      m=m+1
      nfac(m)=2
      k=k/4
   50 kt=m
      j=2
   60 if(mod(k,j) .ne. 0) go to 70
      m=m+1
      nfac(m)=j
      k=k/j
   70 j=((j+1)/2)*2+1
      if(j .le. k) go to 60
   80 if(kt .eq. 0) go to 100
      j=kt
   90 m=m+1
      nfac(m)=nfac(j)
      j=j-1
      if(j .ne. 0) go to 90
c  compute fourier transform
  100 sd=radf/float(kspan)
      cd=2.0*sin(sd)**2
      sd=sin(sd+sd)
      kk=1
      i=i+1
      if(nfac(i) .ne. 2) go to 400
c  transform for factor of 2 (including rotation factor)
      kspan=kspan/2
      k1=kspan+2
  210 k2=kk+kspan
      ak=a(k2)
      bk=b(k2)
      a(k2)=a(kk)-ak
      b(k2)=b(kk)-bk
      a(kk)=a(kk)+ak
      b(kk)=b(kk)+bk
      kk=k2+kspan
      if(kk .le. nn) go to 210
      kk=kk-nn
      if(kk .le. jc) go to 210
      if(kk .gt. kspan) go to 800
  220 c1=1.0-cd
      s1=sd
  230 k2=kk+kspan
      ak=a(kk)-a(k2)
      bk=b(kk)-b(k2)
      a(kk)=a(kk)+a(k2)
      b(kk)=b(kk)+b(k2)
      a(k2)=c1*ak-s1*bk
      b(k2)=s1*ak+c1*bk
      kk=k2+kspan
      if(kk .lt. nt) go to 230
      k2=kk-nt
      c1=-c1
      kk=k1-k2
      if(kk .gt. k2) go to 230
      ak=c1-(cd*c1+sd*s1)
      s1=(sd*c1-cd*s1)+s1
      c1=2.0-(ak**2+s1**2)
      s1=c1*s1
      c1=c1*ak
      kk=kk+jc
      if(kk .lt. k2) go to 230
      k1=k1+inc+inc
      kk=(k1-kspan)/2+jc
      if(kk .le. jc+jc) go to 220
      go to 100
c  transform for factor of 3 (optional code)
  320 k1=kk+kspan
      k2=k1+kspan
      ak=a(kk)
      bk=b(kk)
      aj=a(k1)+a(k2)
      bj=b(k1)+b(k2)
      a(kk)=ak+aj
      b(kk)=bk+bj
      ak=-0.5*aj+ak
      bk=-0.5*bj+bk
      aj=(a(k1)-a(k2))*s120
      bj=(b(k1)-b(k2))*s120
      a(k1)=ak-bj
      b(k1)=bk+aj
      a(k2)=ak+bj
      b(k2)=bk-aj
      kk=k2+kspan
      if(kk .lt. nn) go to 320
      kk=kk-nn
      if(kk .le. kspan) go to 320
      go to 700
c  transform for factor of 4
  400 if(nfac(i) .ne. 4) go to 600
      kspnn=kspan
      kspan=kspan/4
  410 c1=1.0
      s1=0
  420 k1=kk+kspan
      k2=k1+kspan
      k3=k2+kspan
      akp=a(kk)+a(k2)
      akm=a(kk)-a(k2)
      ajp=a(k1)+a(k3)
      ajm=a(k1)-a(k3)
      a(kk)=akp+ajp
      ajp=akp-ajp
      bkp=b(kk)+b(k2)
      bkm=b(kk)-b(k2)
      bjp=b(k1)+b(k3)
      bjm=b(k1)-b(k3)
      b(kk)=bkp+bjp
      bjp=bkp-bjp
      if(isn .lt. 0) go to 450
      akp=akm-bjm
      akm=akm+bjm
      bkp=bkm+ajm
      bkm=bkm-ajm
      if(s1 .eq. 0) go to 460
  430 a(k1)=akp*c1-bkp*s1
      b(k1)=akp*s1+bkp*c1
      a(k2)=ajp*c2-bjp*s2
      b(k2)=ajp*s2+bjp*c2
      a(k3)=akm*c3-bkm*s3
      b(k3)=akm*s3+bkm*c3
      kk=k3+kspan
      if(kk .le. nt) go to 420
  440 c2=c1-(cd*c1+sd*s1)
      s1=(sd*c1-cd*s1)+s1
      c1=2.0-(c2**2+s1**2)
      s1=c1*s1
      c1=c1*c2
      c2=c1**2-s1**2
      s2=2.0*c1*s1
      c3=c2*c1-s2*s1
      s3=c2*s1+s2*c1
      kk=kk-nt+jc
      if(kk .le. kspan) go to 420
      kk=kk-kspan+inc
      if(kk .le. jc) go to 410
      if(kspan .eq. jc) go to 800
      go to 100
  450 akp=akm+bjm
      akm=akm-bjm
      bkp=bkm-ajm
      bkm=bkm+ajm
      if(s1 .ne. 0) go to 430
  460 a(k1)=akp
      b(k1)=bkp
      a(k2)=ajp
      b(k2)=bjp
      a(k3)=akm
      b(k3)=bkm
      kk=k3+kspan
      if(kk .le. nt) go to 420
      go to 440
c  transform for factor of 5 (optional code)
  510 c2=c72**2-s72**2
      s2=2.0*c72*s72
  520 k1=kk+kspan
      k2=k1+kspan
      k3=k2+kspan
      k4=k3+kspan
      akp=a(k1)+a(k4)
      akm=a(k1)-a(k4)
      bkp=b(k1)+b(k4)
      bkm=b(k1)-b(k4)
      ajp=a(k2)+a(k3)
      ajm=a(k2)-a(k3)
      bjp=b(k2)+b(k3)
      bjm=b(k2)-b(k3)
      aa=a(kk)
      bb=b(kk)
      a(kk)=aa+akp+ajp
      b(kk)=bb+bkp+bjp
      ak=akp*c72+ajp*c2+aa
      bk=bkp*c72+bjp*c2+bb
      aj=akm*s72+ajm*s2
      bj=bkm*s72+bjm*s2
      a(k1)=ak-bj
      a(k4)=ak+bj
      b(k1)=bk+aj
      b(k4)=bk-aj
      ak=akp*c2+ajp*c72+aa
      bk=bkp*c2+bjp*c72+bb
      aj=akm*s2-ajm*s72
      bj=bkm*s2-bjm*s72
      a(k2)=ak-bj
      a(k3)=ak+bj
      b(k2)=bk+aj
      b(k3)=bk-aj
      kk=k4+kspan
      if(kk .lt. nn) go to 520
      kk=kk-nn
      if(kk .le. kspan) go to 520
      go to 700
c  transform for odd factors
  600 k=nfac(i)
      kspnn=kspan
      kspan=kspan/k
      if(k .eq. 3) go to 320
      if(k .eq. 5) go to 510
      if(k .eq. jf) go to 640
      jf=k
      s1=rad/float(k)
      c1=cos(s1)
      s1=sin(s1)
      if(jf .gt. maxf) go to 998
      ck(jf)=1.0
      sk(jf)=0.0
      j=1
  630 ck(j)=ck(k)*c1+sk(k)*s1
      sk(j)=ck(k)*s1-sk(k)*c1
      k=k-1
      ck(k)=ck(j)
      sk(k)=-sk(j)
      j=j+1
      if(j .lt. k) go to 630
  640 k1=kk
      k2=kk+kspnn
      aa=a(kk)
      bb=b(kk)
      ak=aa
      bk=bb
      j=1
      k1=k1+kspan
  650 k2=k2-kspan
      j=j+1
      at(j)=a(k1)+a(k2)
      ak=at(j)+ak
      bt(j)=b(k1)+b(k2)
      bk=bt(j)+bk
      j=j+1
      at(j)=a(k1)-a(k2)
      bt(j)=b(k1)-b(k2)
      k1=k1+kspan
      if(k1 .lt. k2) go to 650
      a(kk)=ak
      b(kk)=bk
      k1=kk
      k2=kk+kspnn
      j=1
  660 k1=k1+kspan
      k2=k2-kspan
      jj=j
      ak=aa
      bk=bb
      aj=0.0
      bj=0.0
      k=1
  670 k=k+1
      ak=at(k)*ck(jj)+ak
      bk=bt(k)*ck(jj)+bk
      k=k+1
      aj=at(k)*sk(jj)+aj
      bj=bt(k)*sk(jj)+bj
      jj=jj+j
      if(jj .gt. jf) jj=jj-jf
      if(k .lt. jf) go to 670
      k=jf-j
      a(k1)=ak-bj
      b(k1)=bk+aj
      a(k2)=ak+bj
      b(k2)=bk-aj
      j=j+1
      if(j .lt. k) go to 660
      kk=kk+kspnn
      if(kk .le. nn) go to 640
      kk=kk-nn
      if(kk .le. kspan) go to 640
c  multiply by rotation factor (except for factors of 2 and 4)
  700 if(i .eq. m) go to 800
      kk=jc+1
  710 c2=1.0-cd
      s1=sd
  720 c1=c2
      s2=s1
      kk=kk+kspan
  730 ak=a(kk)
      a(kk)=c2*ak-s2*b(kk)
      b(kk)=s2*ak+c2*b(kk)
      kk=kk+kspnn
      if(kk .le. nt) go to 730
      ak=s1*s2
      s2=s1*c2+c1*s2
      c2=c1*c2-ak
      kk=kk-nt+kspan
      if(kk .le. kspnn) go to 730
      c2=c1-(cd*c1+sd*s1)
      s1=s1+(sd*c1-cd*s1)
      c1=2.0-(c2**2+s1**2)
      s1=c1*s1
      c2=c1*c2
      kk=kk-kspnn+jc
      if(kk .le. kspan) go to 720
      kk=kk-kspan+jc+inc
      if(kk .le. jc+jc) go to 710
      go to 100
c  permute the results to normal order---done in two stages
c  permutation for square factors of n
  800 np(1)=ks
      if(kt .eq. 0) go to 890
      k=kt+kt+1
      if(m .lt. k) k=k-1
      j=1
      np(k+1)=jc
  810 np(j+1)=np(j)/nfac(j)
      np(k)=np(k+1)*nfac(j)
      j=j+1
      k=k-1
      if(j .lt. k) go to 810
      k3=np(k+1)
      kspan=np(2)
      kk=jc+1
      k2=kspan+1
      j=1
      if(n .ne. ntot) go to 850
c  permutation for single-variate transform (optional code)
  820 ak=a(kk)
      a(kk)=a(k2)
      a(k2)=ak
      bk=b(kk)
      b(kk)=b(k2)
      b(k2)=bk
      kk=kk+inc
      k2=kspan+k2
      if(k2 .lt. ks) go to 820
  830 k2=k2-np(j)
      j=j+1
      k2=np(j+1)+k2
      if(k2 .gt. np(j)) go to 830
      j=1
  840 if(kk .lt. k2) go to 820
      kk=kk+inc
      k2=kspan+k2
      if(k2 .lt. ks) go to 840
      if(kk .lt. ks) go to 830
      jc=k3
      go to 890
c  permutation for multivariate transform
  850 k=kk+jc
  860 ak=a(kk)
      a(kk)=a(k2)
      a(k2)=ak
      bk=b(kk)
      b(kk)=b(k2)
      b(k2)=bk
      kk=kk+inc
      k2=k2+inc
      if(kk .lt. k) go to 860
      kk=kk+ks-jc
      k2=k2+ks-jc
      if(kk .lt. nt) go to 850
      k2=k2-nt+kspan
      kk=kk-nt+jc
      if(k2 .lt. ks) go to 850
  870 k2=k2-np(j)
      j=j+1
      k2=np(j+1)+k2
      if(k2 .gt. np(j)) go to 870
      j=1
  880 if(kk .lt. k2) go to 850
      kk=kk+jc
      k2=kspan+k2
      if(k2 .lt. ks) go to 880
      if(kk .lt. ks) go to 870
      jc=k3
  890 if(2*kt+1 .ge. m) return
      kspnn=np(kt+1)
c  permutation for square-free factors of n
      j=m-kt
      nfac(j+1)=1
  900 nfac(j)=nfac(j)*nfac(j+1)
      j=j-1
      if(j .ne. kt) go to 900
      kt=kt+1
      nn=nfac(kt)-1
      if(nn .gt. maxp) go to 998
      jj=0
      j=0
      go to 906
  902 jj=jj-k2
      k2=kk
      k=k+1
      kk=nfac(k)
  904 jj=kk+jj
      if(jj .ge. k2) go to 902
      np(j)=jj
  906 k2=nfac(kt)
      k=kt+1
      kk=nfac(k)
      j=j+1
      if(j .le. nn) go to 904
c  determine the permutation cycles of length greater than 1
      j=0
      go to 914
  910 k=kk
      kk=np(k)
      np(k)=-kk
      if(kk .ne. j) go to 910
      k3=kk
  914 j=j+1
      kk=np(j)
      if(kk .lt. 0) go to 914
      if(kk .ne. j) go to 910
      np(j)=-j
      if(j .ne. nn) go to 914
      maxf=inc*maxf
c  reorder a and b, following the permutation cycles
      go to 950
  924 j=j-1
      if(np(j) .lt. 0) go to 924
      jj=jc
  926 kspan=jj
      if(jj .gt. maxf) kspan=maxf
      jj=jj-kspan
      k=np(j)
      kk=jc*k+ii+jj
      k1=kk+kspan
      k2=0
  928 k2=k2+1
      at(k2)=a(k1)
      bt(k2)=b(k1)
      k1=k1-inc
      if(k1 .ne. kk) go to 928
  932 k1=kk+kspan
      k2=k1-jc*(k+np(k))
      k=-np(k)
  936 a(k1)=a(k2)
      b(k1)=b(k2)
      k1=k1-inc
      k2=k2-inc
      if(k1 .ne. kk) go to 936
      kk=k2
      if(k .ne. j) go to 932
      k1=kk+kspan
      k2=0
  940 k2=k2+1
      a(k1)=at(k2)
      b(k1)=bt(k2)
      k1=k1-inc
      if(k1 .ne. kk) go to 940
      if(jj .ne. 0) go to 926
      if(j .ne. 1) go to 924
  950 j=k3+1
      nt=nt-kspnn
      ii=nt-inc+1
      if(nt .ge. 0) go to 924
      return
c  error finish, insufficient array storage
  998 isn=0
      return
      end

      subroutine mdfft(a,b,ntot,n,nspan,isn,at,bt,ck,sk,np,nfac)
      IMPLICIT NONE
c     double precision version of mdfft. Usage same, except
c     that all real arrays should be double precision. See 
c     comments under mdfft.

      double precision a(*), b(*)
      integer n, ntot, nspan, isn
      double precision at(*), bt(*), ck(*), sk(*)
      integer nfac(*), np(*)

      double precision    aa, aj, ajm, ajp, ak, akm, akp
      double precision    bb, bj, bjm, bjp, bk, bkm, bkp
      double precision   c1, c2, c3, c72, cd
      
      integer i, ii, inc
      integer j, jc, jf, jj
      integer k, k1, k2, k3, k4, kk, ks, kspan, kspnn, kt
      integer m, maxf, maxp
      integer nn, nt

      double precision    rad, radf, s1, s120, s2, s3, s72, sd
      

      equivalence (i,ii)
c  the following two constants should agree with the array dimensions.
      maxf=n
      maxp=n
      if(n .lt. 2) return
      inc=isn
      c72=0.30901699437494742
      s72=0.95105651629515357
      s120=0.86602540378443865
      rad=6.2831853071796
      if(isn .ge. 0) go to 10
      s72=-s72
      s120=-s120
      rad=-rad
      inc=-inc
   10 nt=inc*ntot
      ks=inc*nspan
      kspan=ks
      nn=nt-inc
      jc=ks/n
      radf=rad*dble(jc)*0.5
      i=0
      jf=0
c  determine the factors of n
      m=0
      k=n
      go to 20
   15 m=m+1
      nfac(m)=4
      k=k/16
   20 if(k-(k/16)*16 .eq. 0) go to 15
      j=3
      jj=9
      go to 30
   25 m=m+1
      nfac(m)=j
      k=k/jj
   30 if(mod(k,jj) .eq. 0) go to 25
      j=j+2
      jj=j**2
      if(jj .le. k) go to 30
      if(k .gt. 4) go to 40
      kt=m
      nfac(m+1)=k
      if(k .ne. 1) m=m+1
      go to 80
   40 if(k-(k/4)*4 .ne. 0) go to 50
      m=m+1
      nfac(m)=2
      k=k/4
   50 kt=m
      j=2
   60 if(mod(k,j) .ne. 0) go to 70
      m=m+1
      nfac(m)=j
      k=k/j
   70 j=((j+1)/2)*2+1
      if(j .le. k) go to 60
   80 if(kt .eq. 0) go to 100
      j=kt
   90 m=m+1
      nfac(m)=nfac(j)
      j=j-1
      if(j .ne. 0) go to 90
c  compute fourier transform
  100 sd=radf/dble(kspan)
      cd=2.0*sin(sd)**2
      sd=sin(sd+sd)
      kk=1
      i=i+1
      if(nfac(i) .ne. 2) go to 400
c  transform for factor of 2 (including rotation factor)
      kspan=kspan/2
      k1=kspan+2
  210 k2=kk+kspan
      ak=a(k2)
      bk=b(k2)
      a(k2)=a(kk)-ak
      b(k2)=b(kk)-bk
      a(kk)=a(kk)+ak
      b(kk)=b(kk)+bk
      kk=k2+kspan
      if(kk .le. nn) go to 210
      kk=kk-nn
      if(kk .le. jc) go to 210
      if(kk .gt. kspan) go to 800
  220 c1=1.0-cd
      s1=sd
  230 k2=kk+kspan
      ak=a(kk)-a(k2)
      bk=b(kk)-b(k2)
      a(kk)=a(kk)+a(k2)
      b(kk)=b(kk)+b(k2)
      a(k2)=c1*ak-s1*bk
      b(k2)=s1*ak+c1*bk
      kk=k2+kspan
      if(kk .lt. nt) go to 230
      k2=kk-nt
      c1=-c1
      kk=k1-k2
      if(kk .gt. k2) go to 230
      ak=c1-(cd*c1+sd*s1)
      s1=(sd*c1-cd*s1)+s1
      c1=2.0-(ak**2+s1**2)
      s1=c1*s1
      c1=c1*ak
      kk=kk+jc
      if(kk .lt. k2) go to 230
      k1=k1+inc+inc
      kk=(k1-kspan)/2+jc
      if(kk .le. jc+jc) go to 220
      go to 100
c  transform for factor of 3 (optional code)
  320 k1=kk+kspan
      k2=k1+kspan
      ak=a(kk)
      bk=b(kk)
      aj=a(k1)+a(k2)
      bj=b(k1)+b(k2)
      a(kk)=ak+aj
      b(kk)=bk+bj
      ak=-0.5*aj+ak
      bk=-0.5*bj+bk
      aj=(a(k1)-a(k2))*s120
      bj=(b(k1)-b(k2))*s120
      a(k1)=ak-bj
      b(k1)=bk+aj
      a(k2)=ak+bj
      b(k2)=bk-aj
      kk=k2+kspan
      if(kk .lt. nn) go to 320
      kk=kk-nn
      if(kk .le. kspan) go to 320
      go to 700
c  transform for factor of 4
  400 if(nfac(i) .ne. 4) go to 600
      kspnn=kspan
      kspan=kspan/4
  410 c1=1.0
      s1=0
  420 k1=kk+kspan
      k2=k1+kspan
      k3=k2+kspan
      akp=a(kk)+a(k2)
      akm=a(kk)-a(k2)
      ajp=a(k1)+a(k3)
      ajm=a(k1)-a(k3)
      a(kk)=akp+ajp
      ajp=akp-ajp
      bkp=b(kk)+b(k2)
      bkm=b(kk)-b(k2)
      bjp=b(k1)+b(k3)
      bjm=b(k1)-b(k3)
      b(kk)=bkp+bjp
      bjp=bkp-bjp
      if(isn .lt. 0) go to 450
      akp=akm-bjm
      akm=akm+bjm
      bkp=bkm+ajm
      bkm=bkm-ajm
      if(s1 .eq. 0) go to 460
  430 a(k1)=akp*c1-bkp*s1
      b(k1)=akp*s1+bkp*c1
      a(k2)=ajp*c2-bjp*s2
      b(k2)=ajp*s2+bjp*c2
      a(k3)=akm*c3-bkm*s3
      b(k3)=akm*s3+bkm*c3
      kk=k3+kspan
      if(kk .le. nt) go to 420
  440 c2=c1-(cd*c1+sd*s1)
      s1=(sd*c1-cd*s1)+s1
      c1=2.0-(c2**2+s1**2)
      s1=c1*s1
      c1=c1*c2
      c2=c1**2-s1**2
      s2=2.0*c1*s1
      c3=c2*c1-s2*s1
      s3=c2*s1+s2*c1
      kk=kk-nt+jc
      if(kk .le. kspan) go to 420
      kk=kk-kspan+inc
      if(kk .le. jc) go to 410
      if(kspan .eq. jc) go to 800
      go to 100
  450 akp=akm+bjm
      akm=akm-bjm
      bkp=bkm-ajm
      bkm=bkm+ajm
      if(s1 .ne. 0) go to 430
  460 a(k1)=akp
      b(k1)=bkp
      a(k2)=ajp
      b(k2)=bjp
      a(k3)=akm
      b(k3)=bkm
      kk=k3+kspan
      if(kk .le. nt) go to 420
      go to 440
c  transform for factor of 5 (optional code)
  510 c2=c72**2-s72**2
      s2=2.0*c72*s72
  520 k1=kk+kspan
      k2=k1+kspan
      k3=k2+kspan
      k4=k3+kspan
      akp=a(k1)+a(k4)
      akm=a(k1)-a(k4)
      bkp=b(k1)+b(k4)
      bkm=b(k1)-b(k4)
      ajp=a(k2)+a(k3)
      ajm=a(k2)-a(k3)
      bjp=b(k2)+b(k3)
      bjm=b(k2)-b(k3)
      aa=a(kk)
      bb=b(kk)
      a(kk)=aa+akp+ajp
      b(kk)=bb+bkp+bjp
      ak=akp*c72+ajp*c2+aa
      bk=bkp*c72+bjp*c2+bb
      aj=akm*s72+ajm*s2
      bj=bkm*s72+bjm*s2
      a(k1)=ak-bj
      a(k4)=ak+bj
      b(k1)=bk+aj
      b(k4)=bk-aj
      ak=akp*c2+ajp*c72+aa
      bk=bkp*c2+bjp*c72+bb
      aj=akm*s2-ajm*s72
      bj=bkm*s2-bjm*s72
      a(k2)=ak-bj
      a(k3)=ak+bj
      b(k2)=bk+aj
      b(k3)=bk-aj
      kk=k4+kspan
      if(kk .lt. nn) go to 520
      kk=kk-nn
      if(kk .le. kspan) go to 520
      go to 700
c  transform for odd factors
  600 k=nfac(i)
      kspnn=kspan
      kspan=kspan/k
      if(k .eq. 3) go to 320
      if(k .eq. 5) go to 510
      if(k .eq. jf) go to 640
      jf=k
      s1=rad/dble(k)
      c1=cos(s1)
      s1=sin(s1)
      if(jf .gt. maxf) go to 998
      ck(jf)=1.0
      sk(jf)=0.0
      j=1
  630 ck(j)=ck(k)*c1+sk(k)*s1
      sk(j)=ck(k)*s1-sk(k)*c1
      k=k-1
      ck(k)=ck(j)
      sk(k)=-sk(j)
      j=j+1
      if(j .lt. k) go to 630
  640 k1=kk
      k2=kk+kspnn
      aa=a(kk)
      bb=b(kk)
      ak=aa
      bk=bb
      j=1
      k1=k1+kspan
  650 k2=k2-kspan
      j=j+1
      at(j)=a(k1)+a(k2)
      ak=at(j)+ak
      bt(j)=b(k1)+b(k2)
      bk=bt(j)+bk
      j=j+1
      at(j)=a(k1)-a(k2)
      bt(j)=b(k1)-b(k2)
      k1=k1+kspan
      if(k1 .lt. k2) go to 650
      a(kk)=ak
      b(kk)=bk
      k1=kk
      k2=kk+kspnn
      j=1
  660 k1=k1+kspan
      k2=k2-kspan
      jj=j
      ak=aa
      bk=bb
      aj=0.0
      bj=0.0
      k=1
  670 k=k+1
      ak=at(k)*ck(jj)+ak
      bk=bt(k)*ck(jj)+bk
      k=k+1
      aj=at(k)*sk(jj)+aj
      bj=bt(k)*sk(jj)+bj
      jj=jj+j
      if(jj .gt. jf) jj=jj-jf
      if(k .lt. jf) go to 670
      k=jf-j
      a(k1)=ak-bj
      b(k1)=bk+aj
      a(k2)=ak+bj
      b(k2)=bk-aj
      j=j+1
      if(j .lt. k) go to 660
      kk=kk+kspnn
      if(kk .le. nn) go to 640
      kk=kk-nn
      if(kk .le. kspan) go to 640
c  multiply by rotation factor (except for factors of 2 and 4)
  700 if(i .eq. m) go to 800
      kk=jc+1
  710 c2=1.0-cd
      s1=sd
  720 c1=c2
      s2=s1
      kk=kk+kspan
  730 ak=a(kk)
      a(kk)=c2*ak-s2*b(kk)
      b(kk)=s2*ak+c2*b(kk)
      kk=kk+kspnn
      if(kk .le. nt) go to 730
      ak=s1*s2
      s2=s1*c2+c1*s2
      c2=c1*c2-ak
      kk=kk-nt+kspan
      if(kk .le. kspnn) go to 730
      c2=c1-(cd*c1+sd*s1)
      s1=s1+(sd*c1-cd*s1)
      c1=2.0-(c2**2+s1**2)
      s1=c1*s1
      c2=c1*c2
      kk=kk-kspnn+jc
      if(kk .le. kspan) go to 720
      kk=kk-kspan+jc+inc
      if(kk .le. jc+jc) go to 710
      go to 100
c  permute the results to normal order---done in two stages
c  permutation for square factors of n
  800 np(1)=ks
      if(kt .eq. 0) go to 890
      k=kt+kt+1
      if(m .lt. k) k=k-1
      j=1
      np(k+1)=jc
  810 np(j+1)=np(j)/nfac(j)
      np(k)=np(k+1)*nfac(j)
      j=j+1
      k=k-1
      if(j .lt. k) go to 810
      k3=np(k+1)
      kspan=np(2)
      kk=jc+1
      k2=kspan+1
      j=1
      if(n .ne. ntot) go to 850
c  permutation for single-variate transform (optional code)
  820 ak=a(kk)
      a(kk)=a(k2)
      a(k2)=ak
      bk=b(kk)
      b(kk)=b(k2)
      b(k2)=bk
      kk=kk+inc
      k2=kspan+k2
      if(k2 .lt. ks) go to 820
  830 k2=k2-np(j)
      j=j+1
      k2=np(j+1)+k2
      if(k2 .gt. np(j)) go to 830
      j=1
  840 if(kk .lt. k2) go to 820
      kk=kk+inc
      k2=kspan+k2
      if(k2 .lt. ks) go to 840
      if(kk .lt. ks) go to 830
      jc=k3
      go to 890
c  permutation for multivariate transform
  850 k=kk+jc
  860 ak=a(kk)
      a(kk)=a(k2)
      a(k2)=ak
      bk=b(kk)
      b(kk)=b(k2)
      b(k2)=bk
      kk=kk+inc
      k2=k2+inc
      if(kk .lt. k) go to 860
      kk=kk+ks-jc
      k2=k2+ks-jc
      if(kk .lt. nt) go to 850
      k2=k2-nt+kspan
      kk=kk-nt+jc
      if(k2 .lt. ks) go to 850
  870 k2=k2-np(j)
      j=j+1
      k2=np(j+1)+k2
      if(k2 .gt. np(j)) go to 870
      j=1
  880 if(kk .lt. k2) go to 850
      kk=kk+jc
      k2=kspan+k2
      if(k2 .lt. ks) go to 880
      if(kk .lt. ks) go to 870
      jc=k3
  890 if(2*kt+1 .ge. m) return
      kspnn=np(kt+1)
c  permutation for square-free factors of n
      j=m-kt
      nfac(j+1)=1
  900 nfac(j)=nfac(j)*nfac(j+1)
      j=j-1
      if(j .ne. kt) go to 900
      kt=kt+1
      nn=nfac(kt)-1
      if(nn .gt. maxp) go to 998
      jj=0
      j=0
      go to 906
  902 jj=jj-k2
      k2=kk
      k=k+1
      kk=nfac(k)
  904 jj=kk+jj
      if(jj .ge. k2) go to 902
      np(j)=jj
  906 k2=nfac(kt)
      k=kt+1
      kk=nfac(k)
      j=j+1
      if(j .le. nn) go to 904
c  determine the permutation cycles of length greater than 1
      j=0
      go to 914
  910 k=kk
      kk=np(k)
      np(k)=-kk
      if(kk .ne. j) go to 910
      k3=kk
  914 j=j+1
      kk=np(j)
      if(kk .lt. 0) go to 914
      if(kk .ne. j) go to 910
      np(j)=-j
      if(j .ne. nn) go to 914
      maxf=inc*maxf
c  reorder a and b, following the permutation cycles
      go to 950
  924 j=j-1
      if(np(j) .lt. 0) go to 924
      jj=jc
  926 kspan=jj
      if(jj .gt. maxf) kspan=maxf
      jj=jj-kspan
      k=np(j)
      kk=jc*k+ii+jj
      k1=kk+kspan
      k2=0
  928 k2=k2+1
      at(k2)=a(k1)
      bt(k2)=b(k1)
      k1=k1-inc
      if(k1 .ne. kk) go to 928
  932 k1=kk+kspan
      k2=k1-jc*(k+np(k))
      k=-np(k)
  936 a(k1)=a(k2)
      b(k1)=b(k2)
      k1=k1-inc
      k2=k2-inc
      if(k1 .ne. kk) go to 936
      kk=k2
      if(k .ne. j) go to 932
      k1=kk+kspan
      k2=0
  940 k2=k2+1
      a(k1)=at(k2)
      b(k1)=bt(k2)
      k1=k1-inc
      if(k1 .ne. kk) go to 940
      if(jj .ne. 0) go to 926
      if(j .ne. 1) go to 924
  950 j=k3+1
      nt=nt-kspnn
      ii=nt-inc+1
      if(nt .ge. 0) go to 924
      return
c  error finish, insufficient array storage
  998 isn=0
      return
      end

