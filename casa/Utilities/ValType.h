//# ValType.h: Data types and their undefined values
//# Copyright (C) 1993,1994,1995,1996,1998,2001,2002
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

#ifndef CASA_VALTYPE_H
#define CASA_VALTYPE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;


// <summary>
// Data types and their undefined values.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso olnon" date="1995/03/20" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> enum <linkto group="DataType.h#DataType">DataType</linkto>
// </prerequisite>

// <synopsis> 
// Class <src>ValType</src> describes the data types and their
// "undefined values".
//
// Supported are built-in data types, <src>Bool</src>,
// <src>String</src>, <src>Complex</src> and <src>DComplex</src>.
// As a rule, the smallest possible value of a data type is used as its
// "undefined value"; for <src>String</src> we use the null string, and
// for <src>Bool</src> the value <em>False</em>.
//
// The class does not contain data. It merely defines constants and
// has overloaded functions that return in some form the "undefined
// value", the data type, or certain other information about the data
// type.
// </synopsis>


class ValType {
public:

    // Get the "undefined value" for this data type as the function's
    // return value.
    //<group>
    static Bool      undefBool     ();
    static Char      undefChar     ();
    static uChar     undefUChar    ();
    static Short     undefShort    ();
    static uShort    undefUShort   ();
    static Int       undefInt      ();
    static uInt      undefUInt     ();
    static Int64     undefInt64    ();
    static float     undefFloat    ();
    static double    undefDouble   ();
    static Complex   undefComplex  ();
    static DComplex  undefDComplex ();
    static String    undefString   ();
    //</group>
    
    // Get the "undefined value" for this data type in the argument.
    // The <src>void*</src> function is not doing anything and is for 
    // TpOther types.
    //<group>
    static void getUndef (Bool*);
    static void getUndef (Char*);
    static void getUndef (uChar*);
    static void getUndef (Short*);
    static void getUndef (uShort*);
    static void getUndef (Int*);
    static void getUndef (uInt*);
    static void getUndef (Int64*);
    static void getUndef (float*);
    static void getUndef (double*);
    static void getUndef (Complex*);
    static void getUndef (DComplex*);
    static void getUndef (String*);
    static void getUndef (void*);
    //</group>

    // Get the data type code for this type as the function's
    // return value.
    //<group>
    static DataType getType (const Bool*);
    static DataType getType (const Char*);
    static DataType getType (const uChar*);
    static DataType getType (const Short*);
    static DataType getType (const uShort*);
    static DataType getType (const Int*);
    static DataType getType (const uInt*);
    static DataType getType (const Int64*);
    static DataType getType (const float*);
    static DataType getType (const double*);
    static DataType getType (const Complex*);
    static DataType getType (const DComplex*);
    static DataType getType (const String*);
    static DataType getType (const TableRecord*);
    static DataType getType (const void*);
    //</group>

    // Get the name of the data type. The <src>void*</src> returns
    // the string "Other   ".
    //<group>
    static const String& getTypeStr (DataType);
    static const String& getTypeStr (const Bool*);
    static const String& getTypeStr (const Char*);
    static const String& getTypeStr (const uChar*);
    static const String& getTypeStr (const Short*);
    static const String& getTypeStr (const uShort*);
    static const String& getTypeStr (const Int*);
    static const String& getTypeStr (const uInt*);
    static const String& getTypeStr (const Int64*);
    static const String& getTypeStr (const float*);
    static const String& getTypeStr (const double*);
    static const String& getTypeStr (const Complex*);
    static const String& getTypeStr (const DComplex*);
    static const String& getTypeStr (const String*);
    static const String& getTypeStr (const TableRecord*);
    static const String& getTypeStr (const void*);
    //</group>

    // Get the size of data type (in local format).
    static int getTypeSize (DataType);

    // Get the size of data type in canonical format.
    // <br>The argument <src>BECanonical</src> determines if the big-endian
    // or little-endian canonical format is used.
    static int getCanonicalSize (DataType, Bool BECanonical = True);

    // Get the functions to convert to/from canonical format.
    // These functions take the number of pixels as the length argument.
    // It returns the number of elements per value; normally this is 1,
    // but for complex values it is 2 (since they convert float/double).
    // <br>The argument <src>BECanonical</src> determines if the big-endian
    // or little-endian canonical format is used.
    static void getCanonicalFunc (DataType dt,
				  Conversion::ValueFunction*& readFunc,
				  Conversion::ValueFunction*& writeFunc,
				  uInt& nrElementsPerValue,
				  Bool BECanonical = True);

    // Test if a data type can be promoted to another.
    static Bool isPromotable (DataType from, DataType to);

    // Get the pointer to the routine which compares two values.
    static ObjCompareFunc* getCmpFunc (DataType);

    // Get the object which compares two values.
    static CountedPtr<BaseCompare> getCmpObj (DataType);

    // Put the value into <src>AipsIO</src>.
    // The <src>void*</src> function is not doing anything and is for
    // TpOther types.
    //<group>
    static void put (AipsIO&, const Bool*);
    static void put (AipsIO&, const Char*);
    static void put (AipsIO&, const uChar*);
    static void put (AipsIO&, const Short*);
    static void put (AipsIO&, const uShort*);
    static void put (AipsIO&, const Int*);
    static void put (AipsIO&, const uInt*);
    static void put (AipsIO&, const Int64*);
    static void put (AipsIO&, const float*);
    static void put (AipsIO&, const double*);
    static void put (AipsIO&, const Complex*);
    static void put (AipsIO&, const DComplex*);
    static void put (AipsIO&, const String*);
    static void put (AipsIO&, const void*);
    //</group>

    // Get the value from <src>AipsIO</src>.
    // The <src>void*</src> function is not doing anything and is for
    // TpOther types.
    //<group>
    static void get (AipsIO&, Bool*);
    static void get (AipsIO&, Char*);
    static void get (AipsIO&, uChar*);
    static void get (AipsIO&, Short*);
    static void get (AipsIO&, uShort*);
    static void get (AipsIO&, Int*);
    static void get (AipsIO&, uInt*);
    static void get (AipsIO&, Int64*);
    static void get (AipsIO&, float*);
    static void get (AipsIO&, double*);
    static void get (AipsIO&, Complex*);
    static void get (AipsIO&, DComplex*);
    static void get (AipsIO&, String*);
    static void get (AipsIO&, void*);
    //</group>

    // Put the value into the <src>ostream</src>.
    // The <src>void*</src> function is not doing anything and is for
    // TpOther types.
    //<group>
    static void put (ostream&, const Bool*);
    static void put (ostream&, const Char*);
    static void put (ostream&, const uChar*);
    static void put (ostream&, const Short*);
    static void put (ostream&, const uShort*);
    static void put (ostream&, const Int*);
    static void put (ostream&, const uInt*);
    static void put (ostream&, const Int64*);
    static void put (ostream&, const float*);
    static void put (ostream&, const double*);
    static void put (ostream&, const Complex*);
    static void put (ostream&, const DComplex*);
    static void put (ostream&, const String*);
    static void put (ostream&, const void*);
    //</group>

    // Check if a value is defined, i.e. if it mismatches the given
    // undefined value. The <src>void*</src> function (for non-standard
    // data types) always returns the value <src>1</src>, since such
    // values cannot be undefined.
    //<group>
    static int isDefined (const Bool* value,     const Bool* undef);
    static int isDefined (const Char* value,     const Char* undef);
    static int isDefined (const uChar* value,    const uChar* undef);
    static int isDefined (const Short* value,    const Short* undef);
    static int isDefined (const uShort* value,   const uShort* undef);
    static int isDefined (const Int* value,      const Int* undef);
    static int isDefined (const uInt* value,     const uInt* undef);
    static int isDefined (const Int64* value,    const Int64* undef);
    static int isDefined (const float* value,    const float* undef);
    static int isDefined (const double* value,   const double* undef);
    static int isDefined (const Complex* value,  const Complex* undef);
    static int isDefined (const DComplex* value, const DComplex* undef);
    static int isDefined (const String* value,   const String* undef);
    static int isDefined (const void* value,     const void* undef);
    //</group>

private:
    static const Bool     undefbool    ;
    static const Char     undefchar    ;
    static const uChar    undefuchar   ;
    static const Short    undefshort   ;
    static const uShort   undefushort  ;
    static const Int      undefint     ;
    static const uInt     undefuint    ;
    static const Int64    undefint64   ;
    static const float    undeffloat   ;
    static const double   undefdouble  ;
    static const Complex  undefcomplex ;
    static const DComplex undefdcomplex;
    static const String   undefstring  ;

    static const String strbool;
    static const String strchar;
    static const String struchar;
    static const String strshort;
    static const String strushort;
    static const String strint;
    static const String struint;
    static const String strint64;
    static const String strfloat;
    static const String strdouble;
    static const String strcomplex;
    static const String strdcomplex;
    static const String strstring;
    static const String strrecord;
    static const String strtable;
    static const String strother;
    static const String strunknown;
    //
    // This class is not meant to be constructed.
    //
    ValType ();
};



inline Bool ValType::undefBool ()
    {return undefbool;}
inline Char ValType::undefChar ()
    {return undefchar;}
inline uChar ValType::undefUChar ()
    {return undefuchar;}
inline Short ValType::undefShort ()
    {return undefshort;}
inline uShort ValType::undefUShort ()
    {return undefushort;}
inline Int ValType::undefInt ()
    {return undefint;}
inline uInt ValType::undefUInt ()
    {return undefuint;}
inline Int64 ValType::undefInt64 ()
    {return undefint64;}
inline float ValType::undefFloat ()
    {return undeffloat;}
inline double ValType::undefDouble ()
    {return undefdouble;}
inline Complex ValType::undefComplex ()
    {return undefcomplex;}
inline DComplex ValType::undefDComplex ()
    {return undefdcomplex;}
inline String ValType::undefString ()
    {return undefstring;}


inline void ValType::getUndef (Bool* val)
    {*val = undefbool;}
inline void ValType::getUndef (Char* val)
    {*val = undefchar;}
inline void ValType::getUndef (uChar* val)
    {*val = undefuchar;}
inline void ValType::getUndef (Short* val)
    {*val = undefshort;}
inline void ValType::getUndef (uShort* val)
    {*val = undefushort;}
inline void ValType::getUndef (Int* val)
    {*val = undefint;}
inline void ValType::getUndef (uInt* val)
    {*val = undefuint;}
inline void ValType::getUndef (Int64* val)
    {*val = undefint64;}
inline void ValType::getUndef (float* val)
    {*val = undeffloat;}
inline void ValType::getUndef (double* val)
    {*val = undefdouble;}
inline void ValType::getUndef (Complex* val)
    {*val = undefcomplex;}
inline void ValType::getUndef (DComplex* val)
    {*val = undefdcomplex;}
inline void ValType::getUndef (String* val)
    {*val = undefstring;}
inline void ValType::getUndef (void*)
    {}

inline DataType ValType::getType (const Bool*)
    {return TpBool;}
inline DataType ValType::getType (const Char*)
    {return TpChar;}
inline DataType ValType::getType (const uChar*)
    {return TpUChar;}
inline DataType ValType::getType (const Short*)
    {return TpShort;}
inline DataType ValType::getType (const uShort*)
    {return TpUShort;}
inline DataType ValType::getType (const Int*)
    {return TpInt;}
inline DataType ValType::getType (const uInt*)
    {return TpUInt;}
inline DataType ValType::getType (const Int64*)
    {return TpInt64;}
inline DataType ValType::getType (const float*)
    {return TpFloat;}
inline DataType ValType::getType (const double*)
    {return TpDouble;}
inline DataType ValType::getType (const Complex*)
    {return TpComplex;}
inline DataType ValType::getType (const DComplex*)
    {return TpDComplex;}
inline DataType ValType::getType (const String*)
    {return TpString;}
inline DataType ValType::getType (const TableRecord*)
    {return TpRecord;}
inline DataType ValType::getType (const void*)
    {return TpOther;}

inline const String& ValType::getTypeStr (const Bool*)
    {return strbool;}
inline const String& ValType::getTypeStr (const Char*)
    {return strchar;}
inline const String& ValType::getTypeStr (const uChar*)
    {return struchar;}
inline const String& ValType::getTypeStr (const Short*)
    {return strshort;}
inline const String& ValType::getTypeStr (const uShort*)
    {return strushort;}
inline const String& ValType::getTypeStr (const Int*)
    {return strint;}
inline const String& ValType::getTypeStr (const uInt*)
    {return struint;}
inline const String& ValType::getTypeStr (const Int64*)
    {return strint64;}
inline const String& ValType::getTypeStr (const float*)
    {return strfloat;}
inline const String& ValType::getTypeStr (const double*)
    {return strdouble;}
inline const String& ValType::getTypeStr (const Complex*)
    {return strcomplex;}
inline const String& ValType::getTypeStr (const DComplex*)
    {return strdcomplex;}
inline const String& ValType::getTypeStr (const String*)
    {return strstring;}
inline const String& ValType::getTypeStr (const TableRecord*)
    {return strrecord;}
inline const String& ValType::getTypeStr (const void*)
    {return strother;}

inline void ValType::put (AipsIO& ios, const Bool* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const Char* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const uChar* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const Short* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const uShort* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const Int* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const uInt* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const Int64* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const float* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const double* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const Complex* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const DComplex* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const String* value)
    {ios << *value;}
inline void ValType::put (AipsIO&, const void*)
    {}

inline void ValType::get (AipsIO& ios, Bool* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, Char* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, uChar* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, Short* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, uShort* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, Int* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, uInt* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, Int64* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, float* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, double* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, Complex* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, DComplex* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, String* value)
    {ios >> *value;}
inline void ValType::get (AipsIO&, void*)
    {}

inline void ValType::put (ostream& ios, const Bool* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const Char* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const uChar* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const Short* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const uShort* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const Int* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const uInt* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const Int64* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const float* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const double* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const Complex* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const DComplex* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const String* value)
    {ios << *value;}
inline void ValType::put (ostream&, const void*)
    {}


inline int ValType::isDefined (const Bool* value,     const Bool* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const Char* value,     const Char* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const uChar* value,    const uChar* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const Short* value,    const Short* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const uShort* value,   const uShort* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const Int* value,      const Int* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const uInt* value,     const uInt* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const Int64* value,    const Int64* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const float* value,    const float* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const double* value,   const double* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const Complex* value,  const Complex* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const DComplex* value, const DComplex* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const String* value,   const String* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const void*, const void*)
    {return 1;}



} //# NAMESPACE CASACORE - END

#endif



