//# FunctionMarshallable.h: a class for serializing/reconstituting Function objects to/from Records
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

#ifndef SCIMATH_FUNCTIONMARSHALLABLE_H
#define SCIMATH_FUNCTIONMARSHALLABLE_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/FunctionFactoryErrors.h>
#include <casacore/scimath/Functionals/SerialHelper.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// a class for serializing/reconstituting Function objects to/from Records
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> FunctionFactory
//   <li> Function
// </prerequisite>
//
// <etymology>
// Marshalling (a.k.a. serialization) is the process of converting the 
// state of an object into a transmitable form so that an another object with 
// identical state can be created in another execution context.  This class
// defines an interface for marshalling Functions.  
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

class FunctionMarshallable {
public:

    // create a FunctionMarshallable.  <em>functype</em> is the name that 
    // store() will load into the Record's <tt>functype</tt> field.
    FunctionMarshallable(const String& functype) : ftype(functype) {}
    FunctionMarshallable(const FunctionMarshallable& other) : ftype() { 
	ftype = other.ftype;
    }
    virtual ~FunctionMarshallable() {}

    // store the state of this Function into a Record
    // <thrown>
    //  <li> InvalidSerializationError  if an error during serialization
    // </thrown>
    virtual void store(Record& gr) const = 0;

    virtual FunctionMarshallable& 
         operator=(const FunctionMarshallable& other) 
    {
	ftype = other.ftype;
	return *this;
    }

    // return the name representing the Function type that will be placed 
    // in the <tt>functype</tt> field of Record passed to store().
    const String& getFuncType() const { return ftype; }

    // load functype field into the given Record
    void loadFuncType(Record& gr) const {
	gr.define(SerialHelper::FUNCTYPE.c_str(), ftype.c_str());
    }

private:
    FunctionMarshallable() : ftype() {}

    String ftype;
};


} //# NAMESPACE CASACORE - END

#endif


