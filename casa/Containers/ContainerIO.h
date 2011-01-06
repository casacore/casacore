//# ContainerIO.h: text output IO for any container
//# Copyright (C) 2011
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

#ifndef CASA_CONTAINERIO_H
#define CASA_CONTAINERIO_H

//# Includes
#include <casa/aips.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
//    Input/output operators for Containers.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/02/21" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> STL container concept
// </prerequisite>

// <synopsis> 
// The function <src>showContainer</src> makes it possible to show
// any STL-like container (having forward iterators) on an ostream.
// This include casacore classes like Array, IPosition, and Block, but
// also STL classes like vector. The separator, prefix, and postfix
// can be defined at will (they default to , [ ]).
//
// The function <src>showDataIter</src> is similar to <src>showContainer</src>,
// but uses iterators directly.
// </synopsis>

// <example>
// <srcblock>
// IPosition shape (3,10,10,3);
// showContainer (cout, shape);
// </srcblock>
//
// <motivation>
// Effortless input/output is clearly a big win.
// </motivation>
//
// <group name="Container IO">

// Write out an ascii representation of any container using the
// given begin and end iterator.
// An arbitrary separator, prefix, and postfix can be given.
// E.g. for separator ', ' the output looks like [1, 2, 3].
template<class ITER> void showDataIter (ostream&,
                                        ITER begin, const ITER& end,
                                        const char* separator=",",
                                        const char* prefix="[",
                                        const char* postfix="]");

// Write out an ascii representation of any container having a
// forward iterator.
// Note that a multi-dimensional Array object is linearized.
// An arbitrary separator, prefix, and postfix can be given.
// E.g. for separator ', ' the output looks like [1, 2, 3].
template<class CONTAINER> void showContainer (ostream& os, const CONTAINER& c,
                                              const char* separator=",",
                                              const char* prefix="[",
                                              const char* postfix="]")
  { showDataIter (os, c.begin(), c.end(), separator, prefix, postfix); }

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Containers/ContainerIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
