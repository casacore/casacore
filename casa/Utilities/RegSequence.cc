//# RegSequence.cc: Sequence class for the Register template functions
//# Copyright (C) 1993,1994,1995
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

#include <casacore/casa/Utilities/RegSequence.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Handed out sequence starts at 1 as Register.cc uses 0 as default initializer
// and we want to simply use fetch_add(), which returns the previous value.
#if defined(USE_THREADS) && defined(AIPS_CXX11)
std::atomic<uInt> RegSequence::next(1);
#else // !USE_THREADS (empty Mutex impl) or pre-C++11
uInt RegSequence::next = 1;
Mutex RegSequence::theirMutex;
#endif

uInt RegSequence::getNext() {
  return RegSequence::SgetNext();
}

} //# NAMESPACE CASACORE - END

