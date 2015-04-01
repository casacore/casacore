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
//#
//# $Id$

#ifndef CASA_DATATYPE_H
#define CASA_DATATYPE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class Table;
template<class T> class Array;
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

// These (overloaded) functions return DataType that corresponds to to the
// type that is being pointed at. A pointer is used to avoid to avoid having
// to create the object if it is of Array or Table types. At least for CFront,
// it also avoids those types from being instantiated (they are forward
// declared). The void* function matches any type (if none other will), and
// returns TpOther.
// <group>
inline DataType whatType(const void *)   { return TpOther; }
inline DataType whatType(const Bool *)   { return TpBool; }
inline DataType whatType(const Char *)   { return TpChar; }
inline DataType whatType(const uChar *)  { return TpUChar; }
inline DataType whatType(const Short*) {return TpShort ; }
inline DataType whatType(const uShort*) {return TpUShort ; }
inline DataType whatType(const Int*) {return TpInt ; }
inline DataType whatType(const uInt*) {return TpUInt ; }
inline DataType whatType(const Int64*) {return TpInt64 ; }
inline DataType whatType(const float*) {return TpFloat ; }
inline DataType whatType(const double*) {return TpDouble ; }
inline DataType whatType(const Complex*) {return TpComplex ; }
inline DataType whatType(const DComplex*) {return TpDComplex ; }
inline DataType whatType(const String*) {return TpString ; }
inline DataType whatType(const Table*) {return TpTable ; }
inline DataType whatType(const Array<Bool> *)   { return TpArrayBool; }
inline DataType whatType(const Array<Char> *)   { return TpArrayChar; }
inline DataType whatType(const Array<uChar> *)  { return TpArrayUChar; }
inline DataType whatType(const Array<Short>*) {return TpArrayShort ; }
inline DataType whatType(const Array<uShort> *) {return TpArrayUShort ; }
inline DataType whatType(const Array<Int> *) {return TpArrayInt ; }
inline DataType whatType(const Array<uInt> *) {return TpArrayUInt ; }
inline DataType whatType(const Array<Int64> *) {return TpArrayInt64 ; }
inline DataType whatType(const Array<float> *) {return TpArrayFloat ; }
inline DataType whatType(const Array<double> *) {return TpArrayDouble ; }
inline DataType whatType(const Array<Complex> *) {return TpArrayComplex ; }
inline DataType whatType(const Array<DComplex> *) {return TpArrayDComplex ; }
inline DataType whatType(const Array<String> *) {return TpArrayString ; }
inline DataType whatType(const Record *) {return TpRecord ; }
inline DataType whatType(const Quantum<Double> *) {return TpQuantity ; }
inline DataType whatType(const Array<Quantum<Double> > *)
                                                  {return TpArrayQuantity ; }
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
