//# Primes.cc:  This class provides some prime number operations using a cached table
//# Copyright (C) 1994,1995,1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes

#include <casacore/casa/BasicMath/Primes.h>
#include <casacore/casa/BasicMath/Math.h>			// For sqrt only

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//The minimum size for the cacheTable
const uInt MINSIZE = 31; 

Block<uInt>  Primes::cacheTable;
Mutex        Primes::theirMutex;


Bool Primes::isPrime(uInt number)
{
    if (number < 2) return False;
    return(smallestPrimeFactor(number)==number ? True : False);
}

uInt Primes::aLargerPrimeThan( uInt number ) 
{    
    ScopedMutexLock lock(theirMutex);
    // If number is equal to or larger than the last (and largest) element in 
    // the table of primes, this function returns zero; otherwise, this 
    // function returns the next higher prime in the table.
    
    if ( cacheTable.nelements() < MINSIZE ) initializeCache();

    if ( number >= cacheTable[cacheTable.nelements() - 1] ) return 0;

    Int index = -1;
    for ( uInt i = cacheTable.nelements(); i > 0; i-- ) {
	if ( cacheTable[(i-1)] > number ) {
	    index =(i-1);
	}
    }
    return cacheTable[ index ];
}

uInt Primes::nextLargerPrimeThan( uInt number ) 
{
    ScopedMutexLock lock(theirMutex);
    uInt i;
    // This function increments number until it is prime.  It finds the next
    // entry in the table of primes which is larger, and stores this entry's
    // index number.  The table is resized to accomodate another entry, and
    // every entry after the stored index is moved over by one.  The new prime
    // number is inserted to the spot marked by the stored index.

    if ( cacheTable.nelements() < MINSIZE ) {
	initializeCache();
    }
    while( !isPrime( ++number ) ) {}
    uInt index = cacheTable.nelements();
    for( i = cacheTable.nelements(); i > 0; i-- ) {
	if ( cacheTable[(i-1)] == number ) {
	    return number;
	}
	if ( cacheTable[(i-1)] > number ) {
	    index =(i-1);
	}
    }
    cacheTable.resize(cacheTable.nelements() + 1);
    for( i = ( cacheTable.nelements()-1 ); i > index; i-- ) {
	cacheTable[i] = cacheTable[i-1];
    }
    cacheTable[ index ] = number;
    return number;
}
 
uInt Primes::smallestPrimeFactor( uInt number ) 
{
    // This function checks for factors: if found, the first (smallest) one is
    // returned, otherwise the original value is returned.
    
    // This algorithm is not the best, but checks for divisability by 6n +/- 1

    if (number == 0) return 0;
    if ((number % 2) == 0) return 2;
    if ((number % 3) == 0) return 3;

    for (uInt i=5,k=7,sq=(uInt)(sqrt(Double(number))+1); i<sq; i=i+6,k=k+6) {
	if ((number % i) == 0) return i;
	if ((number % k) == 0) return k;
    }
    return number;
} 

Block<uInt> Primes::factor( uInt number ) 
{
    //If number is zero or one, this function returns a one-cell block
    //containing number; otherwise this fuction continues to resize the 
    //block by one and store the next smallest factor of number in the 
    //block until number equals the product of all the factors stored 
    //in the block.

    Block<uInt> multiples;

    if (number < 2) {
	multiples.resize(1);
	multiples[0] = number;
    } else {
	for (uInt index=0; number > 1; index++) {
	    multiples.resize( index+1 );
	    multiples[index] = smallestPrimeFactor(number);
	    number = number / multiples[index];
	}
    }
    return multiples;
}

void Primes::initializeCache()
{
    // This function resets the cache to a block of 30, which
    // contains the next prime greater than each power of two.

    cacheTable.resize( 30, True, False );
    cacheTable[0] = 3;
    cacheTable[1] = 5;      
    cacheTable[2] = 11;     
    cacheTable[3] = 17;     
    cacheTable[4] = 37;    
    cacheTable[5] = 67;     
    cacheTable[6] = 131;    
    cacheTable[7] = 257;    
    cacheTable[8] = 521;    
    cacheTable[9] = 1031;   
    cacheTable[10] = 2053;  
    cacheTable[11] = 4099;  
    cacheTable[12] = 8209;  
    cacheTable[13] = 16411; 
    cacheTable[14] = 32771; 
    cacheTable[15] = 65537; 
    cacheTable[16] = 131101;
    cacheTable[17] = 262147;
    cacheTable[18] = 524309;
    cacheTable[19] = 1048583;   
    cacheTable[20] = 2097169;    
    cacheTable[21] = 4194319;
    cacheTable[22] = 8388617;   
    cacheTable[23] = 16777259;
    cacheTable[24] = 33554467;
    cacheTable[25] = 67108879;  
    cacheTable[26] = 134217757;
    cacheTable[27] = 268435459; 
    cacheTable[28] = 536870923; 
    cacheTable[29] = 1073741827;
}

} //# NAMESPACE CASACORE - END

