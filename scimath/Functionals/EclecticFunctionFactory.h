//# EclecticFunctionFactory.cc: a class for creating various Function objects from Records
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

#ifndef SCIMATH_ECLECTICFUNCTIONFACTORY_H
#define SCIMATH_ECLECTICFUNCTIONFACTORY_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/Containers/OrderedMap.h>
#include <casacore/casa/Containers/OrderedPair.h>
#include <casacore/casa/Exceptions/Error.h>
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
//    <li> Function must have a constructor for the form T(const Record&)
// </templating>
//
// <thrown>
//    <li> UnrecognizedFunctionError by create() if the Record field 
//         "functype" does not match a Function added via addFactory()
//    <li> InvalidSerializationError by create() if 
//         <ul>
//            <li> Record does not contain a "functype" field containing
//                 a string
//            <li> the associated specific factory throws an
//                 InvalidSerializationError
//         </ul>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> 
//   <li> 
//   <li> 
// </todo>

template<class T> 
class EclecticFunctionFactory : public FunctionFactory<T>
{
public:

    // create an empty EclecticFunctionFactory
    EclecticFunctionFactory();

    // create a shallow copy of another EclecticFunctionFactory
    EclecticFunctionFactory(const EclecticFunctionFactory& factory);

    // delete this EclecticFunctionFactory.  Those specific factories added
    // via addFactory() with <em>own=True</em> will be deleted.
    virtual ~EclecticFunctionFactory();

    // create the Function object described in the given Record.  This
    // implementation will use the value of the "functype" field to lookup
    // the specific factory to use to create the function.  That is, the 
    // the "functype" value will be matched against the type names loaded 
    // via addFactory().
    virtual Function<T> *create(const Record&) const
	throw(FunctionFactoryError);

    // add a factory for creating a specific type of function, associating
    // it with a given "functype" name.
    void addFactory(const String& type, FunctionFactory<T> *factory,
		    Bool own=True);

    // return the number of factories that have been loaded thus far.
    Int ndefined() { return lookup.ndefined(); }

    // return True if a factory with a given "functype" name has been 
    // loaded.
    Bool isDefined(const String& type) { return lookup.isDefined(type); }

    // a shallow assignment operator
    EclecticFunctionFactory& operator=(const EclecticFunctionFactory& factory);

protected:

private:
    OrderedMap<String, OrderedPair<FunctionFactory<T>*, Bool> > lookup;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/EclecticFunctionFactory.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif


