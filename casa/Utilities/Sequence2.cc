//# Sequence.cc: Templated virtual base class for sequences
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Utilities/Sequence.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#if defined(USE_THREADS)
std::atomic<uInt> uIntSequence::next(1); // start at 1 to stay in sync with RegSequence, FIXME fix comment, RegSequnce no longer exists
#else
uInt uIntSequence::next = 1;
#endif

uInt uIntSequence::SgetNext()
{
#if defined(USE_THREADS)
  return next.fetch_add(1);
#else
  return next++;
#endif
}

} //# NAMESPACE CASACORE - END

