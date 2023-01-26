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
// Supported are built-in data types, <src>bool</src>,
// <src>String</src>, <src>Complex</src> and <src>DComplex</src>.
// As a rule, the smallest possible value of a data type is used as its
// "undefined value"; for <src>String</src> we use the null string, and
// for <src>bool</src> the value <em>false</em>.
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
    static bool      undefBool     ();
    static char      undefChar     ();
    static unsigned char     undefUChar    ();
    static int16_t     undefShort    ();
    static uint16_t    undefUShort   ();
    static int32_t       undefInt      ();
    static uint32_t      undefUInt     ();
    static int64_t     undefInt64    ();
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
    static void getUndef (bool*);
    static void getUndef (char*);
    static void getUndef (unsigned char*);
    static void getUndef (int16_t*);
    static void getUndef (uint16_t*);
    static void getUndef (int32_t*);
    static void getUndef (uint32_t*);
    static void getUndef (int64_t*);
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
    static DataType getType (const bool*);
    static DataType getType (const char*);
    static DataType getType (const unsigned char*);
    static DataType getType (const int16_t*);
    static DataType getType (const uint16_t*);
    static DataType getType (const int32_t*);
    static DataType getType (const uint32_t*);
    static DataType getType (const int64_t*);
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
    static const String& getTypeStr (const bool*);
    static const String& getTypeStr (const char*);
    static const String& getTypeStr (const unsigned char*);
    static const String& getTypeStr (const int16_t*);
    static const String& getTypeStr (const uint16_t*);
    static const String& getTypeStr (const int32_t*);
    static const String& getTypeStr (const uint32_t*);
    static const String& getTypeStr (const int64_t*);
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
    static int getCanonicalSize (DataType, bool BECanonical = true);

    // Get the functions to convert to/from canonical format.
    // These functions take the number of pixels as the length argument.
    // It returns the number of elements per value; normally this is 1,
    // but for complex values it is 2 (since they convert float/double).
    // <br>The argument <src>BECanonical</src> determines if the big-endian
    // or little-endian canonical format is used.
    static void getCanonicalFunc (DataType dt,
				  Conversion::ValueFunction*& readFunc,
				  Conversion::ValueFunction*& writeFunc,
				  uint32_t& nrElementsPerValue,
				  bool BECanonical = true);

    // Test if a data type can be promoted to another.
    static bool isPromotable (DataType from, DataType to);

    // Get the pointer to the routine which compares two values.
    static ObjCompareFunc* getCmpFunc (DataType);

    // Get the object which compares two values.
    static CountedPtr<BaseCompare> getCmpObj (DataType);

    // Put the value into <src>AipsIO</src>.
    // The <src>void*</src> function is not doing anything and is for
    // TpOther types.
    //<group>
    static void put (AipsIO&, const bool*);
    static void put (AipsIO&, const char*);
    static void put (AipsIO&, const unsigned char*);
    static void put (AipsIO&, const int16_t*);
    static void put (AipsIO&, const uint16_t*);
    static void put (AipsIO&, const int32_t*);
    static void put (AipsIO&, const uint32_t*);
    static void put (AipsIO&, const int64_t*);
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
    static void get (AipsIO&, bool*);
    static void get (AipsIO&, char*);
    static void get (AipsIO&, unsigned char*);
    static void get (AipsIO&, int16_t*);
    static void get (AipsIO&, uint16_t*);
    static void get (AipsIO&, int32_t*);
    static void get (AipsIO&, uint32_t*);
    static void get (AipsIO&, int64_t*);
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
    static void put (ostream&, const bool*);
    static void put (ostream&, const char*);
    static void put (ostream&, const unsigned char*);
    static void put (ostream&, const int16_t*);
    static void put (ostream&, const uint16_t*);
    static void put (ostream&, const int32_t*);
    static void put (ostream&, const uint32_t*);
    static void put (ostream&, const int64_t*);
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
    static int isDefined (const bool* value,     const bool* undef);
    static int isDefined (const char* value,     const char* undef);
    static int isDefined (const unsigned char* value,    const unsigned char* undef);
    static int isDefined (const int16_t* value,    const int16_t* undef);
    static int isDefined (const uint16_t* value,   const uint16_t* undef);
    static int isDefined (const int32_t* value,      const int32_t* undef);
    static int isDefined (const uint32_t* value,     const uint32_t* undef);
    static int isDefined (const int64_t* value,    const int64_t* undef);
    static int isDefined (const float* value,    const float* undef);
    static int isDefined (const double* value,   const double* undef);
    static int isDefined (const Complex* value,  const Complex* undef);
    static int isDefined (const DComplex* value, const DComplex* undef);
    static int isDefined (const String* value,   const String* undef);
    static int isDefined (const void* value,     const void* undef);
    //</group>

private:
    static const bool     undefbool    ;
    static const char     undefchar    ;
    static const unsigned char    undefuchar   ;
    static const int16_t    undefshort   ;
    static const uint16_t   undefushort  ;
    static const int32_t      undefint     ;
    static const uint32_t     undefuint    ;
    static const int64_t    undefint64   ;
    static const float    undeffloat   ;
    static const double   undefdouble  ;
    static const Complex  undefcomplex ;
    static const DComplex undefdcomplex;
    static const String   undefstring  ;

    static const String &strbool( ) {
        static String result("Bool    ");
        return result;
    }
    static const String &strchar( ) {
        static String result("Char    ");
        return result;
    }
    static const String &struchar( ) {
        static String result("uChar   ");
        return result;
    }
    static const String &strshort( ) {
        static String result("Short   ");
        return result;
    }
    static const String &strushort( ) {
        static String result("uShort  ");
        return result;
    }
    static const String &strint( ) {
        static String result("Int     ");
        return result;
    }
    static const String &struint( ) {
        static String result("uInt    ");
        return result;
    }
    static const String &strint64( ) {
        static String result("Int64   ");
        return result;
    }
    static const String &strfloat( ) {
        static String result("float   ");
        return result;
    }
    static const String &strdouble( ) {
        static String result("double  ");
        return result;
    }
    static const String &strcomplex( ) {
        static String result("Complex ");
        return result;
    }
    static const String &strdcomplex( ) {
        static String result("DComplex");
        return result;
    }
    static const String &strstring( ) {
        static String result("String  ");
        return result;
    }
    static const String &strrecord( ) {
        static String result("Record  ");
        return result;
    }
    static const String &strtable( ) {
        static String result("Table   ");
        return result;
    }
    static const String &strother( ) {
        static String result("Other   ");
        return result;
    }
    static const String &strunknown( ) {
        static String result("unknown ");
        return result;
    }
    //
    // This class is not meant to be constructed.
    //
    ValType ();
};



inline bool ValType::undefBool ()
    {return undefbool;}
inline char ValType::undefChar ()
    {return undefchar;}
inline unsigned char ValType::undefUChar ()
    {return undefuchar;}
inline int16_t ValType::undefShort ()
    {return undefshort;}
inline uint16_t ValType::undefUShort ()
    {return undefushort;}
inline int32_t ValType::undefInt ()
    {return undefint;}
inline uint32_t ValType::undefUInt ()
    {return undefuint;}
inline int64_t ValType::undefInt64 ()
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


inline void ValType::getUndef (bool* val)
    {*val = undefbool;}
inline void ValType::getUndef (char* val)
    {*val = undefchar;}
inline void ValType::getUndef (unsigned char* val)
    {*val = undefuchar;}
inline void ValType::getUndef (int16_t* val)
    {*val = undefshort;}
inline void ValType::getUndef (uint16_t* val)
    {*val = undefushort;}
inline void ValType::getUndef (int32_t* val)
    {*val = undefint;}
inline void ValType::getUndef (uint32_t* val)
    {*val = undefuint;}
inline void ValType::getUndef (int64_t* val)
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

inline DataType ValType::getType (const bool*)
    {return TpBool;}
inline DataType ValType::getType (const char*)
    {return TpChar;}
inline DataType ValType::getType (const unsigned char*)
    {return TpUChar;}
inline DataType ValType::getType (const int16_t*)
    {return TpShort;}
inline DataType ValType::getType (const uint16_t*)
    {return TpUShort;}
inline DataType ValType::getType (const int32_t*)
    {return TpInt;}
inline DataType ValType::getType (const uint32_t*)
    {return TpUInt;}
inline DataType ValType::getType (const int64_t*)
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

inline const String& ValType::getTypeStr (const bool*)
    {return strbool();}
inline const String& ValType::getTypeStr (const char*)
    {return strchar();}
inline const String& ValType::getTypeStr (const unsigned char*)
    {return struchar();}
inline const String& ValType::getTypeStr (const int16_t*)
    {return strshort();}
inline const String& ValType::getTypeStr (const uint16_t*)
    {return strushort();}
inline const String& ValType::getTypeStr (const int32_t*)
    {return strint();}
inline const String& ValType::getTypeStr (const uint32_t*)
    {return struint();}
inline const String& ValType::getTypeStr (const int64_t*)
    {return strint64();}
inline const String& ValType::getTypeStr (const float*)
    {return strfloat();}
inline const String& ValType::getTypeStr (const double*)
    {return strdouble();}
inline const String& ValType::getTypeStr (const Complex*)
    {return strcomplex();}
inline const String& ValType::getTypeStr (const DComplex*)
    {return strdcomplex();}
inline const String& ValType::getTypeStr (const String*)
    {return strstring();}
inline const String& ValType::getTypeStr (const TableRecord*)
    {return strrecord();}
inline const String& ValType::getTypeStr (const void*)
    {return strother();}

inline void ValType::put (AipsIO& ios, const bool* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const char* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const unsigned char* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const int16_t* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const uint16_t* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const int32_t* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const uint32_t* value)
    {ios << *value;}
inline void ValType::put (AipsIO& ios, const int64_t* value)
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

inline void ValType::get (AipsIO& ios, bool* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, char* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, unsigned char* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, int16_t* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, uint16_t* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, int32_t* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, uint32_t* value)
    {ios >> *value;}
inline void ValType::get (AipsIO& ios, int64_t* value)
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

inline void ValType::put (ostream& ios, const bool* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const char* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const unsigned char* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const int16_t* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const uint16_t* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const int32_t* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const uint32_t* value)
    {ios << *value;}
inline void ValType::put (ostream& ios, const int64_t* value)
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


inline int ValType::isDefined (const bool* value,     const bool* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const char* value,     const char* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const unsigned char* value,    const unsigned char* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const int16_t* value,    const int16_t* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const uint16_t* value,   const uint16_t* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const int32_t* value,      const int32_t* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const uint32_t* value,     const uint32_t* undef)
    {return *value != *undef;}
inline int ValType::isDefined (const int64_t* value,    const int64_t* undef)
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



