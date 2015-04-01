//# LELScalar.h: Hold a scalar with a mask in LEL
//# Copyright (C) 1999
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

#ifndef LATTICES_LELSCALAR_H
#define LATTICES_LELSCALAR_H


//# Includes
#include <casacore/casa/aips.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// This LEL class holds a scalar with a mask.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This LEL class holds a scalar with a mask.
// </synopsis>

// <motivation>
// It maskes it possible to handle a scalar with its mask as a single object.
// </motivation>

// <todo asof="1998/01/20">
// </todo>
 

template <class T> class LELScalar
{
public: 
// Default constructor sets a False mask.
   LELScalar();

// Constructor takes value and optional mask.
   LELScalar (const T& value, Bool mask=True)
      : itsValue (value), itsMask(mask) {}

// Get value.
// <group>
   const T& value() const
      { return itsValue; }
   T& value()
      { return itsValue; }
// </group>

// Get mask.
   Bool mask() const
      { return itsMask; }

private:
   T    itsValue;
   Bool itsMask;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELScalar.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
