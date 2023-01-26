//# DataType.h: data types (primarily) in the table system
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

#ifndef CASA_DATATYPE_H
#define CASA_DATATYPE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class Table;
template<class T> class Quantum;
class String;
class Record;

// <summary> Data types (primarily) in the table system </summary>
// <use visibility=export>
// <reviewed reviewer="Paul Shannon" date="1995/05/01" tests="tDataType" demos="">
// </reviewed>

// <synopsis>
// DataType enumerates possible data types. While this enum is primarily
// used in the <linkto module="Tables:description">table</linkto> system, some
// use of it is made elsewhere. Besides the enum
// itself, <src>operator<<</src> is defined for DataType; it prints a DataType
// in the form <src>DataType=bool</src>.
//
// Also, global functions are written which take a "const pointer to type" and
// return its DataType (TpOther if unknown). These functions can occasionally
// allow one to avoid a switch on type, and can be useful in constructing
// templated classes which are only valid for certain types.
//
// Global functions are also provided which allow one to convert an
// array type to the equivalent scalar type and vice versa.
//
// <note role=warning>
// New data types should be added just before TpNumberOfTypes, and after all
// the existing enumerations, to avoid changing the number of an existing type
// which would cause misinterpretation of data types stored in existing  files.
// Note also that if any new scalar and array types are added that this
// will break the exising isScalar, isArray, asScalar and asArray functions.
// </note>
//
// <note role=tip>
// Data types <src>long</src> and <src>unsigned long</src> are not
// possible.  The types <src>int32_t</src> and <src>uint32_t</src> are always
// 4 bytes, so <src>long</src> is not needed and may only cause
// confusion.
// </note>
// 
// </synopsis>

// <example>
// The simplest uses of the DataType enumeration and functions are fairly 
// obvious, for example:
// <srcblock>
//    double d;
//    DataType type = whatType(&d);
//    cout << type << endl;
//    switch(type) {
//    case TpChar:   ...
//    ...
//    case TpDouble: ...
//    }
// </srcblock>
//
// A less obvious use is for "attaching" a templated object or function to a
// non-templated object in a safe way. For example:
// <srcblock>
// class IntFloatContainer {
// public:
//     int32_t intval;
//     float floatval;
//     void *ptr(DataType type) {
//         if (type == whatType(&intval))
//             return &intval;
//         else if (type == whatType(&floatval))
//             return &floatval;
//         else
//             return 0; // Illegal type
//     }
// };
//
// template<class T> class ValueAccessor {
// public:
//     ValueAccessor(IntFloatContainer *container) : container_p(container) {
//         if (container_p->ptr(whatType(static_cast<T *>(0))) == 0)
//             throw(AipsError("Illegal type..."));
//     }
//     T &value() { return *((T*)container_p->ptr(whatType(static_cast<T *>(0)))); }
// private:
//     IntFloatContainer *container_p;
// };
// </srcblock>
//
// So, this example provides a typesafe interface to values of only a small
// number of types (and it fairly gracefully allows additional types to be
// added; in particular the accessor class needs no modification). Techniques
// such as this are appropriate for situations where one needs to deal with
// many (but finite) numbers of types. For example, with FITS.
// </example>

// <todo asof="1995/03/01">
//   <li> Clean up comment as soon as enum's are properly extracted.
// </todo>

// <linkfrom anchor=DataType modules="Tables">
//   Enumeration of the <here>data types</here> in the table system
// </linkfrom>
//
// Enumeration of the possible data types for keywords and table columns.
// <group name=DataType>
enum DataType {TpBool,    TpChar,     TpUChar,
	       TpShort,   TpUShort,   TpInt,     TpUInt,
	       TpFloat,   TpDouble,   
	       TpComplex, TpDComplex, TpString,
	       TpTable,
	       TpArrayBool,    TpArrayChar,     TpArrayUChar,
	       TpArrayShort,   TpArrayUShort,   TpArrayInt,   TpArrayUInt,
               TpArrayFloat,   TpArrayDouble,
	       TpArrayComplex, TpArrayDComplex, TpArrayString,
	       TpRecord, TpOther,
//#// TpLDouble,
//#// TpArrayLDouble,
	       TpQuantity, TpArrayQuantity,
               TpInt64, TpArrayInt64,
	       // Since we start at zero, this is the number of types in the
	       // enum.
               TpNumberOfTypes
              };


// Write a formated representation (e.g., Type=bool) of the given data type.
ostream &operator<<(ostream &os, DataType type);

// These (specialized) functions return the DataType that corresponds
// to the template type. TpOther is returned for types that are
// not specialized, as is void.
// <group>
template<typename T>
inline DataType whatType() { return TpOther; }

#define DEFINE_WHATTYPE(SPECIALIZED_TYPE, RETURN_TYPE) \
  template<> inline DataType whatType<SPECIALIZED_TYPE>() { return RETURN_TYPE; }
  
DEFINE_WHATTYPE(void, TpOther)
DEFINE_WHATTYPE(bool, TpBool)
DEFINE_WHATTYPE(char, TpChar)
DEFINE_WHATTYPE(unsigned char, TpUChar)
DEFINE_WHATTYPE(int16_t, TpShort)
DEFINE_WHATTYPE(uint16_t, TpUShort)
DEFINE_WHATTYPE(int32_t, TpInt)
DEFINE_WHATTYPE(uint32_t, TpUInt)
DEFINE_WHATTYPE(int64_t, TpInt64)
DEFINE_WHATTYPE(float, TpFloat)
DEFINE_WHATTYPE(double, TpDouble)
DEFINE_WHATTYPE(Complex, TpComplex)
DEFINE_WHATTYPE(DComplex, TpDComplex)
DEFINE_WHATTYPE(String, TpString)
DEFINE_WHATTYPE(Table, TpTable)
DEFINE_WHATTYPE(Array<bool>, TpArrayBool)
DEFINE_WHATTYPE(Array<char>, TpArrayChar)
DEFINE_WHATTYPE(Array<unsigned char>, TpArrayUChar)
DEFINE_WHATTYPE(Array<int16_t>, TpArrayShort)
DEFINE_WHATTYPE(Array<uint16_t>, TpArrayUShort)
DEFINE_WHATTYPE(Array<int32_t>, TpArrayInt)
DEFINE_WHATTYPE(Array<uint32_t>, TpArrayUInt)
DEFINE_WHATTYPE(Array<int64_t>, TpArrayInt64)
DEFINE_WHATTYPE(Array<float>, TpArrayFloat)
DEFINE_WHATTYPE(Array<double>, TpArrayDouble)
DEFINE_WHATTYPE(Array<Complex>, TpArrayComplex)
DEFINE_WHATTYPE(Array<DComplex>, TpArrayDComplex)
DEFINE_WHATTYPE(Array<String>, TpArrayString)
DEFINE_WHATTYPE(Record, TpRecord)
DEFINE_WHATTYPE(Quantum<double>, TpQuantity)
DEFINE_WHATTYPE(Array<Quantum<double>>, TpArrayQuantity)

#undef DEFINE_WHATTYPE

// </group>

// It is sometimes useful to discover what the corresponding
// scalar (or array) type is for a given array (or scalar) type.
// Calling these with TpOther, TpTable, and TpRecord results
// in an exception being thrown.
// <group>
DataType asScalar(DataType type);
DataType asArray(DataType type);
// </group>

// It is occasionally useful to discover whether or not a DataType represents
// an array or scalar value. Note that TpTable, TpRecord, and TpOther are neither
// scalar nor array types.
// <group>
bool isScalar(DataType type);
bool isArray(DataType type);
bool isScalarFun(DataType type); //{return isScalar(type);}
// </group>

// It is sometimes useful to discover if a DataType represents a real 
// numeric value (i.e., can it be cast to a double?) This returns true
// for both real scalar and array type.
bool isReal(DataType type);

// Returns true for Complex or DComplex scalar or array types
bool isComplex(DataType type);

// Returns true if the type is either Real or Complex/DComplex
bool isNumeric(DataType type);

// </group>


} //# NAMESPACE CASACORE - END

#endif
