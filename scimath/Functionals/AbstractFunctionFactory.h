//# FunctionFactory.h: a class for creating Function objects from Records
//# Copyright (C) 2002
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
//#
//# $Id$

#ifndef SCIMATH_ABSTRACTFUNCTIONFACTORY_H
#define SCIMATH_ABSTRACTFUNCTIONFACTORY_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/FunctionFactoryErrors.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Record;

// <summary>
// a class for creating Function objects from Records
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> FunctionFactory
// </prerequisite>
//
// <etymology>
// This class is based on the Factory pattern, similar to the 
// ApplicationObjectFactory
// </etymology>
//
// <synopsis>
//
//
//
//
// </synopsis>
//
// <example>
//
//
//
// </example>
//
// <motivation>
//
//
//
// </motivation>
//
// <thrown>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
// </todo>

template<class T> 
class FunctionFactory
{
public:
    FunctionFactory() {}
    FunctionFactory(const FunctionFactory<T>& factory) {}
    virtual ~FunctionFactory() {}
    virtual Function<T> *create(const Record& gr) const 
	throw (FunctionFactoryError) = 0;
    FunctionFactory<T>& operator=(const FunctionFactory<T>& factory) {
	return *this;
    }
};


} //# NAMESPACE CASACORE - END

#endif


