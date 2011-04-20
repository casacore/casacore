//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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
//   

#include <scimath/Mathematics/Combinatorics.h>

namespace casa { //# NAMESPACE CASA - BEGIN

    // Initialize factorial with first 2 values (0! and 1! are both 1).
    Vector<uInt> Combinatorics::_factorialCache(2,1);
    Mutex Combinatorics::theirMutex;

    void Combinatorics::fillCache(const uInt n) {
        // Make updating the cache thread-safe.
        // After acquiring a lock, test again if an update needs to be done
        // because another thread might have done it in the mean time.
        ScopedLock lock(theirMutex);
        if (n >= _factorialCache.size()) {
          uInt oldSize = _factorialCache.size();
          // Create a new cache vector.
          // Note: do not resize the existing one, because that makes
          // the first test on size non thread-safe (because resize is done
          // before the vector is filled).
          Vector<uInt> newCache(n+1);
          for (uInt i=0; i<oldSize; ++i) {
            newCache[i] = _factorialCache[i];
          }
          for (uInt i=oldSize; i<=n; ++i) {
            newCache[i] = i * newCache[i-1];
          }
          // Note that reference first copies the data pointer, thereafter the
          // size. So the test in the .h file on size() is thread-safe.
          _factorialCache.reference (newCache);
        }
    }

    uInt Combinatorics::choose(const uInt n, const uInt k) {
        if (k > n) {
            throw AipsError("k cannot be greater than n");
        }
        return factorial(n)/(factorial(k)*factorial(n-k));
    }
} //# NAMESPACE CASA - END

