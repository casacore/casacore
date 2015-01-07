//# SpecificFunctionFactory.h: a class for creating a Function object from Records
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

#ifndef SCIMATH_SPECIFICFUNCTIONFACTORY_H
#define SCIMATH_SPECIFICFUNCTIONFACTORY_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/AbstractFunctionFactory.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Record;

// <summary>
//
//
//
//
//
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
// <templating arg=T>
//    <li> F class must be a subclass of Function<T>
//    <li> F class must have a constructor for the form F(const Record&)
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> 
//   <li> 
//   <li> 
// </todo>
template<class T, class F> 
class SpecificFunctionFactory : public FunctionFactory<T>
{
public:
    SpecificFunctionFactory() {}
    SpecificFunctionFactory(const SpecificFunctionFactory<T,F>& factory) {}
    virtual ~SpecificFunctionFactory() {}
    virtual Function<T> *create(const Record& gr) const 
	throw (FunctionFactoryError) 
    {
	return new F(gr);
    }
    SpecificFunctionFactory<T,F>& 
	operator=(const SpecificFunctionFactory<T,F>& factory) 
    {
	return *this;
    }
};


} //# NAMESPACE CASACORE - END

#endif


