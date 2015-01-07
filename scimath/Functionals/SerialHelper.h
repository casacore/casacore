//# SerialHelper: a helper class for (un)serializing a Function object
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

#ifndef SCIMATH_SERIALHELPER_H
#define SCIMATH_SERIALHELPER_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/FunctionFactoryErrors.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Array;

template <class V>
void getArrayVal(V &val, int type, const Record& gr, 
                 const String& name, uInt index=0);

template <class V>
void getArray(Array<V> &val, int type, const Record& gr, 
              const String& name);

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
// 
// 
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
//    <li> InvalidSerializationError by getFuncType() if Record 
//         does not contain a "functype" field containing a string.
//    <li> InvalidSerializationError
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> 
//   <li> 
//   <li> 
// </todo>

class SerialHelper {
public: 
    static const String FUNCTYPE;
    static const String gtype[]; 
    enum shType { shtBOOL=0, shtBYTE, shtSHORT, shtINT, shtFLOAT,
                  shtDOUBLE, shtCOMPLEX, shtDCOMPLEX, shtSTRING};

    SerialHelper(const Record& record) : gr(record) { }
    SerialHelper(const SerialHelper& other) { gr = other.gr; }
    virtual ~SerialHelper() { }

    // load the function type name as given in the record's "functype" 
    // field into the given String <em>ftype</em>.  <em>gr</em> is the 
    //  record to extract from.  False is returned if the record 
    // does not contain this field.
    // <thrown>
    //   <li> InvalidSerializationError if "functype" exists but is 
    //          empty or the incorrect type
    // </thrown>
    Bool getFuncType(String& ftype) const;

    // ensure that the Function type stored in the given record, <em>gr</em>,
    // matches <em>ftype</em>.  If it does not, an 
    // InvalidSerializationError is thrown.
    void checkFuncType(const String& ftype) const;

    // return True if a field with the given <em>name</em> exists
    Bool exists(const String &name) const { return gr.isDefined(name); }

    // Get the <em>index</em>th element of the <em>name</em> field 
    // This should be 
    // particularly useful for Array objects with only one element,
    // i.e. a <em>scalar</em>.
    // Note that unlike the native  classes, indexing is zero-relative.
    // 
    // InvalidSerializationError is thrown if:
    // <ul>
    //  <li> if the given record does not contain a field called <em>name</em>
    //  <li> if the field is not a vector of the correct type.
    //  <li> if the index is out of range.
    // </ul>
    // <group>
    void get(Bool &val, const String& name, uInt index = 0) const;
//      void get(uChar &val, const String& name, uInt index = 0) const;
    void get(Short &val, const String& name, uInt index = 0) const;
    void get(Int &val, const String& name, uInt index = 0) const;
    void get(Float &val, const String& name, uInt index = 0) const;
    void get(Double &val, const String& name, uInt index = 0) const;
    void get(Complex &val, const String& name, uInt index = 0) const;
    void get(DComplex &val, const String& name, uInt index = 0) const;
    void get(String &val, const String& name, uInt index = 0) const;
    void get(Record &val, const String& name) const;
    // </group>

    // Get the <em>index</em>th element of the <em>name</em> field 
    // This should be 
    // particularly useful for Array objects with only one element,
    // i.e. a <em>scalar</em>.
    // Note that unlike the native  classes, indexing is zero-relative.
    // 
    // InvalidSerializationError is thrown if:
    // <ul>
    //  <li> if the given record does not contain a field called <em>name</em>
    //  <li> if the field is not a vector of the correct type.
    //  <li> if the index is out of range.
    // </ul>
    // <group>
    void get(Array<Bool> &val, const String& name) const;
//      void get(Array<uChar &val, const String& name) const;
    void get(Array<Short> &val, const String& name) const;
    void get(Array<Int> &val, const String& name) const;
    void get(Array<Float> &val, const String& name) const;
    void get(Array<Double> &val, const String& name) const;
    void get(Array<Complex> &val, const String& name) const;
    void get(Array<DComplex> &val, const String& name) const;
    void get(Array<String> &val, const String& name) const;
    // </group>

    SerialHelper& operator=(const SerialHelper& other) { 
	gr = other.gr;
	return *this;
    }

protected:
    SerialHelper() { }

private:

    Record gr;
};


} //# NAMESPACE CASACORE - END

#endif
