//# Primes.h: This class provides some prime number operations using a cached table
//# Copyright (C) 1994,1995,1999,2001
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

//# $Id$

#ifndef SCIMATH_PRIMES_H
#define SCIMATH_PRIMES_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Creates a reference table of prime numbers, and some functions </summary>
//
// <reviewed reviewer="Gareth Hunt" date="94/08/19" tests="tPrimes">
//
// <prerequisite>
//   <li>Understanding Block is only peripherally important.
// </prerequisite>
//
// <etymology>
// Prime has its usual definition (a number whose only factors are
// itself and one.)  Zero and one are not considered to be prime.
// </etymology>
//
// <synopsis> 
// The primary function of the Primes class is to create and maintain a list of
// prime numbers.  This class has no constructors; all member functions are
// therefore declared static.  The class maintains an internal cache table of
// prime numbers.  There are also several member functions of more general use,
// which do not access the cached table.
// 
// The table is initialized to contain the next prime greater than each power
// of two.  The function "aLargerPrimeThan" accepts a number and returns a
// larger prime number from the table of prime numbers.  The function
// "nextLargerPrimeThan" finds the next greater integer that is a prime,
// returns it, and inserts it into the table.  There are also functions to
// initialize and examine the table.
// 
// The basic prime number operations are done in three functions: "isPrime"
// determines whether a given number is a prime; "smallestPrimeFactor" finds
// the smallest factor of a given number; and "factor" returns a Block<uInt>
// containing a number's factors.
// </synopsis> 
//
// <example>
// <srcblock>
// #include <casacore/scimath/Mathematics/Primes.h>
// #include <casacore/casa/Utilities/Assert.h>
// #include <iostream>
//
// // Refer also to tPrimes.cc
//
// int main() {
//     Block<uInt> BB, DD;
//     uInt C, i;
//     if(! Primes::isPrime(4)) {                         //if this number 
//         cout<<"Four is not a prime number"<<endl;      //is not prime
//         BB = Primes::factor(4);                        //than factor it
//
//         if( BB[0] != Primes::smallestPrimeFactor(4) ){ //check that first 
//                                                        //factor equals
//              cerr<<"something is wrong"<<endl;         //the smallest 
//         }
//         cout<<"4 factored:"<<endl;
//         for ( i = 0; i < BB.nelements(); i++ ) {
//             cout<<BB[i]<<endl;
//         }
//
//         C = Primes::aLargerPrimeThan(4);  
//         if ( (C-5) > 4 ) {                         //if difference is more 
//                                                    //than five, then
//             C = Primes::nextLargerPrimeThan(4);    //find next lprime
//         } 
//         DebugAssertExit( C == Primes::aLargerPrimeThan(4)); //now the prime resides
//     }                                              //in the cache table 
//     if( Primes::nCachedPrimes() > 50 ) {
//         Primes::initializeCache();
//     }
//     DD = Primes::cachedPrimes();
//     cout<<"The Table of Primes"<<endl;
//     for ( i = 0; i < Primes::nCachedPrimes(); i++ ) {
//         cout<<DD[i]<<endl;
//     }
//     return 0;
// }
//
// </srcblock>
// </example>
//
// <motivation>
// This class was conceived during the coding of a class that creates hash 
// tables.  The hash table class works best with a table whose size is prime.
// It uses the Primes class's function "nextLargerPrimeThan" to find a prime
// number that is close to an appropriate size.  This prime number becomes the
// size of the hash table.
// </motivation>
//
// <todo asof="$DATE:$">
//   <li> This class should not be used to generate large sets of prime
//        numbers - it is not designed for efficiency at this.
//        The algorithm checks 2, 3, and (6n +/- 1) up to the square
//        root of the candidate prime.
//   <li> The size of the prime numbers are restricted by the size of an
//        unsigned integer (2^31-1 on 32 bit machines).
// </todo>

class Primes {
public:
 
    //This function takes number and returns "True" if number is prime, "False" 
    //if it is not.
    static Bool isPrime(uInt number);

    //This function returns the closest integer larger than number from the 
    //table of primes.  If there is no entry in the table of primes which is 
    //larger than number, a zero is returned.
    static uInt aLargerPrimeThan(uInt number);

    //This function finds the next largest prime than number, returns that 
    //value and stores it in the table of primes.
    static uInt nextLargerPrimeThan(uInt number);   // adds to cache

    //This function returns the smallest factor of number.
    static uInt smallestPrimeFactor(uInt number);

    //This function returns a block, of variable length, with each factor 
    //indexed.  For example, if number equaled 4, then the return block would 
    //have a length of two, and have a two stored in each cell. One and zero
    //are special cases; this function returns a one-cell block which holds
    //one or zero, respectively.
    static Block<uInt> factor(uInt number);

    // This function returns the number of primes stored in the primes table.
  //    static uInt nCachedPrimes()
  //	{ return cacheTable.nelements(); }

    //This function returns the table of prime numbers.
    //static Block<uInt> cachedPrimes()
  //	{ return cacheTable; }
    
private:

    //This function resets the table of prime numbers to contain 31 prime 
    //numbers to avoid consuming too much memory.
    static void initializeCache();

    //This is the table which stores the prime numbers.
    static Block<uInt> cacheTable;
    static Mutex       theirMutex;
};


} //# NAMESPACE CASACORE - END

#endif
