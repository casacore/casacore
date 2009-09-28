//# tPrimes.cc: This program tests the Primes class
//# Copyright (C) 1994,1995,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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

#include <casa/BasicMath/Primes.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// Test the non-cache functions.

void primesNoncacheTests (uInt number, Bool shouldBePrime, uInt numberOfFactors)
{
    AlwaysAssertExit(Primes::isPrime(number) == shouldBePrime);

    Block<uInt> factors=Primes::factor(number);
    AlwaysAssertExit(factors.nelements() == numberOfFactors);

    AlwaysAssertExit(Primes::smallestPrimeFactor(number) == factors[0]); 
    
}

void largerPrimesTest (uInt number, uInt next, uInt closest)
{
    AlwaysAssertExit(Primes::aLargerPrimeThan(number) == next);

    AlwaysAssertExit(Primes::nextLargerPrimeThan(number) == closest);

}

void compareTables ()
{
    Primes::initializeCache();
    Block<uInt> table(30), table2 = Primes::cachedPrimes();
    table[0] = 3;
    table[1] = 5;
    table[2] = 11;
    table[3] = 17;
    table[4] = 37;
    table[5] = 67;
    table[6] = 131;
    table[7] = 257;
    table[8] = 521;
    table[9] = 1031;
    table[10] = 2053;
    table[11] = 4099;
    table[12] = 8209;
    table[13] = 16411;
    table[14] = 32771;
    table[15] = 65537;
    table[16] = 131101;
    table[17] = 262147;
    table[18] = 524309;
    table[19] = 1048583;
    table[20] = 2097169;
    table[21] = 4194319;
    table[22] = 8388617;
    table[23] = 16777259;
    table[24] = 33554467;
    table[25] = 67108879;
    table[26] = 134217757;
    table[27] = 268435459;
    table[28] = 536870923;
    table[29] = 1073741827;
  
    for ( uInt i = 0; i < Primes::nCachedPrimes(); i++ ) {
	AlwaysAssertExit( table[i] == table2[i] );
    }
}
    
int main()
{
    // First test the non-cache functions with some large numbers (for 32 bits)

    primesNoncacheTests (1610612736, False, 30);	// 3 x 2^29
    primesNoncacheTests (5*7*11*13*17*19*23*29, False, 8);
    primesNoncacheTests (46337*46337, False, 2);	// Largest prime square
    primesNoncacheTests (46307*46309, False, 2);	// Largest prime pair
    primesNoncacheTests (2147483647, True, 1);		// 2^31 - 1
    primesNoncacheTests (2147483629, True, 1);		// Next smaller prime
    primesNoncacheTests (0, False, 1);                

    largerPrimesTest (4098, 4099, 4099);                //immediately followed
                                                        //by cached prime
    largerPrimesTest (1073741828, 0, 1073741831);       //larger than largest
                                                        //cached prime
    largerPrimesTest (0, 3, 2);                  
    largerPrimesTest (0, 2, 2);                         //"2" is now in cache

    compareTables();

    cout << "OK" << endl;
    return 0;
}
