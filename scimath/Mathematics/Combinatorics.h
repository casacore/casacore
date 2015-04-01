//# Smooth.h: smooth vectors and arrays 
//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef SCIMATH_COMBINATORICS_H
#define SCIMATH_COMBINATORICS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Combinatorics related functions.
// </summary>

// <use visibility=export>

//# <author>Dave Mehringer</author>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Vector">Vector</linkto>
//   <li> <linkto class="Array">Array</linkto>
// </prerequisite>

// <etymology>
// self-explanatory
// </etymology>

// <synopsis>
// Various factorial and combinatorical functions.
// </synopsis>

// <motivation>
// Binomial coefficients needed for Images/ImageProfileFitter
// </motivation>

class Combinatorics {
  
  public:
 
  // Get n!
  static uInt factorial(const uInt n)
  {
    //# This test is thread-safe.
    if (n >= _factorialCacheSize) fillCache(n);
    return _factorialCache[n];
  }
  
  // "n choose k" = n!/(k!(n-k)!)
  // Exception is thrown if k > n.
  static uInt choose(const uInt n, const uInt k);

  private:
  static void fillCache(const uInt n);

  static Vector<uInt> _factorialCache;
  static volatile uInt _factorialCacheSize; //# volatile for double checked lock
  static Mutex theirMutex;
};
} //# NAMESPACE CASACORE - END

#endif

