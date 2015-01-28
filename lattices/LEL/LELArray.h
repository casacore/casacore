//# LELArray.h: Hold an array with a mask in LEL
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

#ifndef LATTICES_LELARRAY_H
#define LATTICES_LELARRAY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELArrayBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// This LEL class holds an array with a mask.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This LEL class holds an array with a mask.
// The mask can be a single Bool valid for all elements of the array.
// Otherwise it is a full mask with the same shape as the array.
// </synopsis>

// <motivation>
// It makes it possible to handle an array with its mask as a single object.
// </motivation>

// <todo asof="1998/01/20">
// </todo>
 

template <class T> class LELArray : public LELArrayBase
{
public: 
// Constructor takes value.
// Its mask is set to all True.
   LELArray (const Array<T>& value)
      : itsValue (value) {}

// Constructor takes value and mask.
   LELArray (const Array<T>& value, const Array<Bool>& mask)
      : LELArrayBase (mask), itsValue (value) {}

// Constructor takes shape.
// Its mask is set to all True.
   LELArray (const IPosition& shape);

// Copy constructor (reference semantics).
   LELArray (const LELArray<T>& other);

   ~LELArray();

   // Assignment (reference semantics).
   LELArray<T>& operator= (const LELArray<T>& other);

// Get shape (of the value).
   const IPosition& shape() const
      { return itsValue.shape(); }

// Get value.
// <group>
   const Array<T>& value() const
      { return itsValue; }
   Array<T>& value()
      { return itsValue; }
// </group>

private:
   Array<T> itsValue;
};




// <summary>
// This LEL class holds a possible referenced array with a mask.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This LEL class is derived from LELArray.
// Its purpose is to provide only const access to the array value, so
// the array can be a reference to another array.
// It is meant for optimization, so references can safely be used
// when evaluating a subexpression.
// </synopsis>

// <motivation>
// It makes it possible to use the function evalRef in a safe way.
// It would be unsafe to use a LELArray object, because that
// gives non-const access to the value.
// </motivation>

// <todo asof="1998/01/20">
// </todo>
 

template <class T> class LELArrayRef : public LELArray<T>
{
public: 
// Constructor takes shape.
// Its mask is set to all True.
   LELArrayRef (const IPosition& shape)
    : LELArray<T> (shape) {}

   ~LELArrayRef()
    {}

// Get value.
   const Array<T>& value() const
      { return LELArray<T>::value(); }

private:
// Copy constructor is not needed.
   LELArrayRef (const LELArrayRef<T>& other);
// Assignment is not needed.
   LELArrayRef<T>& operator= (const LELArrayRef<T>& other);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELArray.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
