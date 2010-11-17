//# Compare.h: compare two objects of the same type
//# Copyright (C) 1994,1995,1999
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

#ifndef CASA_COMPARE_H
#define CASA_COMPARE_H

//# Includes
#include <casa/aips.h>
#include <casa/Utilities/DataType.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> signature of comparison functions </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/02/24" tests="" demos="">

// <synopsis>
// This typedef defines the signature of the comparison functions used
// in, for instance, the <linkto class="Sort">Sort</linkto> class: functions
// with two <src>const void*</src> arguments returning an
// <src>int</src> value. One such function is defined in the
// <linkto class="ObjCompare">ObjCompare</linkto> class.
// </synopsis>

// <group name=ObjCompareFunc>
typedef int ObjCompareFunc (const void*, const void*);
// </group>


// <summary> abstract base class for comparing two objects </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/02/24" tests="" demos="">
//
// <synopsis> 
// The abstract class <src>BaseCompare<T></src> is used for comparisons
// in sorting or iterating. One can derive a concrete comparison class
// from it.
// </synopsis>

class BaseCompare
{
public:
    virtual ~BaseCompare() {}

    // Compare two objects, and return
    // <ul>
    //  <li> -1  if obj1 < obj2;
    //  <li>  0  if obj1 == obj2;
    //  <li>  1  otherwise.
    // </ul>
    virtual int comp (const void* obj1, const void* obj2) const = 0;

    // Get the data type of the comparison.
    // By default it returns TpOther.
    virtual DataType dataType() const
      { return TpOther; }
};

// <summary> compare two objects </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/02/24" tests="" demos="">

// <synopsis> 
// The templated class <src>ObjCompare<T></src> really is only a place
// holder for the static function <src>compare</src> which compares two
// objects of type T.
// </synopsis>

// <templating arg=T>
// <li> operator==
// <li> operator<
// </templating>

template<class T> class ObjCompare: public BaseCompare
{
public:
    virtual ~ObjCompare();

    // Compare two objects, and return
    // <ul>
    //  <li> -1  if obj1 < obj2;
    //  <li>  0  if obj1 == obj2;
    //  <li>  1  otherwise.
    // </ul>
    // The static function is not inlined allowing one to take the address of
    // it. Furthermore, the function's signature agrees with
    // <linkto group="Compare.h#ObjCompareFunc">ObjCompareFunc</linkto> so
    // that it can be used in the <linkto class="Sort">Sort</linkto> class.
    static int compare (const void* obj1, const void* obj2);
    virtual int comp (const void* obj1, const void* obj2) const;

    // Get the data type of the comparison.
    virtual DataType dataType() const;
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Utilities/Compare.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
