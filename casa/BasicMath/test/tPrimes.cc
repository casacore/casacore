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

#include <casacore/casa/BasicMath/Primes.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
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

    cout << "OK" << endl;
    return 0;
}
