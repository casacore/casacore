//# ValType.cc: Class describing the data types and their undefined values
//# Copyright (C) 1993,1994,1995,1996,1998
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

#include <aips/Utilities/ValType.h>
#include <aips/OS/CanonicalConversion.h>
#include <aips/Mathematics/Constants.h>
#include <values.h>

//# This is the implementation of the ValType class.
//# Most functions are inlined in the header file.


ValType::ValType ()
{}


const Bool           ValType::undefbool     = False;
const Char           ValType::undefchar     = (Char)-128;
const uChar          ValType::undefuchar    = 0;
const Short          ValType::undefshort    = -32768;
const uShort         ValType::undefushort   = 0;
const Int            ValType::undefint      = (Int)-2147483648;
const uInt           ValType::undefuint     = 0;
const float          ValType::undeffloat    = -C::minfloat;
const Complex        ValType::undefcomplex   (-C::minfloat,  -C::minfloat);
const double         ValType::undefdouble   = -C::mindouble;
const DComplex       ValType::undefdcomplex  (-C::mindouble, -C::mindouble);
const String         ValType::undefstring    ("");


const String ValType::strbool        = "Bool    ";
const String ValType::strchar        = "Char    ";
const String ValType::struchar       = "uChar   ";
const String ValType::strshort       = "Short   ";
const String ValType::strushort      = "uShort  ";
const String ValType::strint         = "Int     ";
const String ValType::struint        = "uInt    ";
const String ValType::strfloat       = "float   ";
const String ValType::strdouble      = "double  ";
const String ValType::strcomplex     = "Complex ";
const String ValType::strdcomplex    = "DComplex";
const String ValType::strstring      = "String  ";
const String ValType::strrecord      = "Record  ";
const String ValType::strtable       = "Table   ";
const String ValType::strother       = "Other   ";
const String ValType::strunknown     = "unknown ";


//# Get the name of the data type.
const String& ValType::getTypeStr (DataType dt)
{
    switch (dt) {
    case TpBool:
	return strbool;
    case TpChar:
	return strchar;
    case TpUChar:
	return struchar;
    case TpShort:
	return strshort;
    case TpUShort:
	return strushort;
    case TpInt:
	return strint;
    case TpUInt:
	return struint;
    case TpFloat:
	return strfloat;
    case TpDouble:
	return strdouble;
    case TpComplex:
	return strcomplex;
    case TpDComplex:
	return strdcomplex;
    case TpString:
	return strstring;
    case TpRecord:
	return strrecord;
    case TpTable:
	return strtable;
    case TpOther:
	return strother;
    default:
	break;
    }
    return strunknown;
}


//# Get the size of the data type.
int ValType::getTypeSize (DataType dt)
{
    switch (dt) {
    case TpBool:
    case TpArrayBool:
	return sizeof(Bool);
    case TpChar:
    case TpArrayChar:
	return sizeof(Char);
    case TpUChar:
    case TpArrayUChar:
	return sizeof(uChar);
    case TpShort:
    case TpArrayShort:
	return sizeof(short);
    case TpUShort:
    case TpArrayUShort:
	return sizeof(unsigned short);
    case TpInt:
    case TpArrayInt:
	return sizeof(Int);
    case TpUInt:
    case TpArrayUInt:
	return sizeof(uInt);
    case TpFloat:
    case TpArrayFloat:
	return sizeof(float);
    case TpDouble:
    case TpArrayDouble:
	return sizeof(double);
    case TpComplex:
    case TpArrayComplex:
	return sizeof(Complex);
    case TpDComplex:
    case TpArrayDComplex:
	return sizeof(DComplex);
    case TpString:
    case TpArrayString:
	return sizeof(String);
    default:
	break;
    }
    return 0;
}

//# Get the canonical size of the data type.
int ValType::getCanonicalSize (DataType dt)
{
    switch (dt) {
    case TpChar:
    case TpArrayChar:
	return CanonicalConversion::canonicalSize ((Char*)0);
    case TpUChar:
    case TpArrayUChar:
	return CanonicalConversion::canonicalSize ((uChar*)0);
    case TpShort:
    case TpArrayShort:
	return CanonicalConversion::canonicalSize ((Short*)0);
    case TpUShort:
    case TpArrayUShort:
	return CanonicalConversion::canonicalSize ((uShort*)0);
    case TpInt:
    case TpArrayInt:
	return CanonicalConversion::canonicalSize ((Int*)0);
    case TpUInt:
    case TpArrayUInt:
	return CanonicalConversion::canonicalSize ((uInt*)0);
    case TpFloat:
    case TpArrayFloat:
	return CanonicalConversion::canonicalSize ((float*)0);
    case TpDouble:
    case TpArrayDouble:
	return CanonicalConversion::canonicalSize ((double*)0);
    case TpComplex:
    case TpArrayComplex:
	return 2 * CanonicalConversion::canonicalSize ((float*)0);
    case TpDComplex:
    case TpArrayDComplex:
	return 2 * CanonicalConversion::canonicalSize ((double*)0);
    default:
	break;
    }
    return 0;
}


void ValType::getCanonicalFunc (DataType dt,
				Conversion::ValueFunction*& readFunc,
				Conversion::ValueFunction*& writeFunc,
				uInt& nrElementsPerValue)
{
    nrElementsPerValue = 1;
    switch (dt) {
    case TpBool:
    case TpArrayBool:
	readFunc  = Conversion::bitToBool;
	writeFunc = Conversion::boolToBit;
	break;
    case TpChar:
    case TpArrayChar:
	readFunc  = CanonicalConversion::getToLocal ((uChar*)0);
	writeFunc = CanonicalConversion::getFromLocal ((uChar*)0);
	break;
    case TpUChar:
    case TpArrayUChar:
	readFunc  = CanonicalConversion::getToLocal ((uChar*)0);
	writeFunc = CanonicalConversion::getFromLocal ((uChar*)0);
	break;
    case TpShort:
    case TpArrayShort:
	readFunc  = CanonicalConversion::getToLocal ((Short*)0);
	writeFunc = CanonicalConversion::getFromLocal ((Short*)0);
	break;
    case TpUShort:
    case TpArrayUShort:
	readFunc  = CanonicalConversion::getToLocal ((uShort*)0);
	writeFunc = CanonicalConversion::getFromLocal ((uShort*)0);
	break;
    case TpInt:
    case TpArrayInt:
	readFunc  = CanonicalConversion::getToLocal ((Int*)0);
	writeFunc = CanonicalConversion::getFromLocal ((Int*)0);
	break;
    case TpUInt:
    case TpArrayUInt:
	readFunc  = CanonicalConversion::getToLocal ((uInt*)0);
	writeFunc = CanonicalConversion::getFromLocal ((uInt*)0);
	break;
    case TpComplex:
    case TpArrayComplex:
	nrElementsPerValue = 2;
    case TpFloat:
    case TpArrayFloat:
	readFunc  = CanonicalConversion::getToLocal ((float*)0);
	writeFunc = CanonicalConversion::getFromLocal ((float*)0);
	break;
    case TpDComplex:
    case TpArrayDComplex:
	nrElementsPerValue = 2;
    case TpDouble:
    case TpArrayDouble:
	readFunc  = CanonicalConversion::getToLocal ((double*)0);
	writeFunc = CanonicalConversion::getFromLocal ((double*)0);
	break;
    default:
	readFunc  = 0;
	writeFunc = 0;
    }
}

//# Test if a data type can be promoted to another.
//# Note that the cases fall through.
Bool ValType::isPromotable (DataType from, DataType to)
{
    if (from == TpOther)
	return False;
    if (from == to)
	return True;
    switch (from) {
    case TpChar:
	if (to == TpShort)
	    return True;
    case TpShort:
	if (to == TpInt)
	    return True;
    case TpInt:
    case TpFloat:
    case TpDouble:
	if (to == TpFloat  ||  to == TpDouble)
	    return True;
    case TpComplex:
    case TpDComplex:
	if (to == TpComplex  ||  to == TpDComplex)
	    return True;
	return False;
    case TpUChar:
	if (to == TpUShort)
	    return True;
    case TpUShort:
	if (to == TpUInt)
	    return True;
    case TpUInt:
	if (to == TpFloat  ||  to == TpDouble)
	    return True;
	if (to == TpComplex  ||  to == TpDComplex)
	    return True;
	return False;
    default:
	break;
    }
    return False;
}


//# Get the comparison routine.
ObjCompareFunc* ValType::getCmpFunc (DataType dt)
{
    switch (dt) {
    case TpBool:
	return &ObjCompare<Bool>::compare;
    case TpChar:
	return &ObjCompare<Char>::compare;
    case TpUChar:
	return &ObjCompare<uChar>::compare;
    case TpShort:
	return &ObjCompare<Short>::compare;
    case TpUShort:
	return &ObjCompare<uShort>::compare;
    case TpInt:
	return &ObjCompare<Int>::compare;
    case TpUInt:
	return &ObjCompare<uInt>::compare;
    case TpFloat:
	return &ObjCompare<float>::compare;
    case TpDouble:
	return &ObjCompare<double>::compare;
    case TpComplex:
	return &ObjCompare<Complex>::compare;
    case TpDComplex:
	return &ObjCompare<DComplex>::compare;
    case TpString:
	return &ObjCompare<String>::compare;
    default:
	break;
    }
    return 0;
}
