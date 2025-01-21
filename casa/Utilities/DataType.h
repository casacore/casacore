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
//#        Internet email: casa-feedback@nrao.edu.
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
// in the form <src>DataType=Bool</src>.
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
// possible.  The types <src>Int</src> and <src>uInt</src> are always
// 4 bytes, so <src>long</src> is not needed and may only cause
// confusion.
// </note>
// 
// </synopsis>

// <example>
// The simplest uses of the DataType enumeration and functions are fairly 
// obvious, for example:
// <srcblock>
//    Double d;
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
//     Int intval;
//     Float floatval;
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


// Write a formated representation (e.g., Type=Bool) of the given data type.
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
DEFINE_WHATTYPE(Bool, TpBool)
DEFINE_WHATTYPE(Char, TpChar)
DEFINE_WHATTYPE(uChar, TpUChar)
DEFINE_WHATTYPE(Short, TpShort)
DEFINE_WHATTYPE(uShort, TpUShort)
DEFINE_WHATTYPE(Int, TpInt)
DEFINE_WHATTYPE(uInt, TpUInt)
DEFINE_WHATTYPE(Int64, TpInt64)
DEFINE_WHATTYPE(float, TpFloat)
DEFINE_WHATTYPE(double, TpDouble)
DEFINE_WHATTYPE(Complex, TpComplex)
DEFINE_WHATTYPE(DComplex, TpDComplex)
DEFINE_WHATTYPE(String, TpString)
DEFINE_WHATTYPE(Table, TpTable)
DEFINE_WHATTYPE(Array<Bool>, TpArrayBool)
DEFINE_WHATTYPE(Array<Char>, TpArrayChar)
DEFINE_WHATTYPE(Array<uChar>, TpArrayUChar)
DEFINE_WHATTYPE(Array<Short>, TpArrayShort)
DEFINE_WHATTYPE(Array<uShort>, TpArrayUShort)
DEFINE_WHATTYPE(Array<Int>, TpArrayInt)
DEFINE_WHATTYPE(Array<uInt>, TpArrayUInt)
DEFINE_WHATTYPE(Array<Int64>, TpArrayInt64)
DEFINE_WHATTYPE(Array<float>, TpArrayFloat)
DEFINE_WHATTYPE(Array<double>, TpArrayDouble)
DEFINE_WHATTYPE(Array<Complex>, TpArrayComplex)
DEFINE_WHATTYPE(Array<DComplex>, TpArrayDComplex)
DEFINE_WHATTYPE(Array<String>, TpArrayString)
DEFINE_WHATTYPE(Record, TpRecord)
DEFINE_WHATTYPE(Quantum<Double>, TpQuantity)
DEFINE_WHATTYPE(Array<Quantum<Double>>, TpArrayQuantity)

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

/**
 * Returns the number of bytes that this type takes when serialized
 * to disk. For dynamic types (arrays, records, etc.), a value
 * of 0 is returned. TpBool returns a value of 1, but be aware that
 * it may be stored with bit-packing.
 */
constexpr size_t SizeOfType(DataType dtype) {
  switch (dtype) {
    case DataType::TpBool:
    case DataType::TpChar:
    case DataType::TpUChar:
      return 1;
    case DataType::TpShort:
    case DataType::TpUShort:
      return 2;
    case DataType::TpInt:
    case DataType::TpUInt:
    case DataType::TpFloat:
      return 4;
    case DataType::TpDouble:
    case DataType::TpComplex:
    case DataType::TpInt64:
      return 8;
    case DataType::TpDComplex:
      return 16;
    case DataType::TpArrayBool:
    case DataType::TpArrayChar:
    case DataType::TpArrayUChar:
    case DataType::TpArrayShort:
    case DataType::TpArrayUShort:
    case DataType::TpArrayInt:
    case DataType::TpArrayUInt:
    case DataType::TpArrayInt64:
    case DataType::TpArrayFloat:
    case DataType::TpArrayDouble:
    case DataType::TpArrayComplex:
    case DataType::TpArrayDComplex:
    case DataType::TpArrayQuantity:
    case DataType::TpArrayString:
    case DataType::TpOther:
    case DataType::TpQuantity:
    case DataType::TpRecord:
    case DataType::TpString:
    case DataType::TpTable:
    case DataType::TpNumberOfTypes:
      return 0;
  }
  return 0;
}

// It is occasionally useful to discover whether or not a DataType represents
// an array or scalar value. Note that TpTable, TpRecord, and TpOther are neither
// scalar nor array types.
// <group>
Bool isScalar(DataType type);
Bool isArray(DataType type);
Bool isScalarFun(DataType type); //{return isScalar(type);}
// </group>

// It is sometimes useful to discover if a DataType represents a real 
// numeric value (i.e., can it be cast to a Double?) This returns True
// for both real scalar and array type.
Bool isReal(DataType type);

// Returns True for Complex or DComplex scalar or array types
Bool isComplex(DataType type);

// Returns True if the type is either Real or Complex/DComplex
Bool isNumeric(DataType type);

// </group>


} //# NAMESPACE CASACORE - END

#endif
