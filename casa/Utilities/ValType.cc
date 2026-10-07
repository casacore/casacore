// # ValType.cc: Class describing the data types and their undefined values
// # Copyright (C) 1993,1994,1995,1996,1998,1999,2000,2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <limits.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # This is the implementation of the ValType class.
// # Most functions are inlined in the header file.

ValType::ValType() {}

const bool ValType::undefbool = false;
const char ValType::undefchar = (char)-128;
const unsigned char ValType::undefuchar = 0;
const short ValType::undefshort = -32768;
const unsigned short ValType::undefushort = 0;
const int ValType::undefint = -2 * int(32768 * 32768);
const unsigned int ValType::undefuint = 0;
const int64_t ValType::undefint64 = -2 * int64_t(32768 * 32768) * int64_t(32768 * 32768);
const float ValType::undeffloat = -FLT_MIN;
const Complex ValType::undefcomplex(-FLT_MIN, -FLT_MIN);
const double ValType::undefdouble = -DBL_MIN;
const DComplex ValType::undefdcomplex(-DBL_MIN, -DBL_MIN);
const String ValType::undefstring("");

// # Get the name of the data type.
const String& ValType::getTypeStr(DataType dt) {
  switch (dt) {
    case TpBool:
      return strbool();
    case TpChar:
      return strchar();
    case TpUChar:
      return struchar();
    case TpShort:
      return strshort();
    case TpUShort:
      return strushort();
    case TpInt:
      return strint();
    case TpUInt:
      return struint();
    case TpInt64:
      return strint64();
    case TpFloat:
      return strfloat();
    case TpDouble:
      return strdouble();
    case TpComplex:
      return strcomplex();
    case TpDComplex:
      return strdcomplex();
    case TpString:
      return strstring();
    case TpRecord:
      return strrecord();
    case TpTable:
      return strtable();
    case TpOther:
      return strother();
    default:
      break;
  }
  return strunknown();
}

// # Get the size of the data type.
int ValType::getTypeSize(DataType dt) {
  switch (dt) {
    case TpBool:
    case TpArrayBool:
      return sizeof(bool);
    case TpChar:
    case TpArrayChar:
      return sizeof(char);
    case TpUChar:
    case TpArrayUChar:
      return sizeof(unsigned char);
    case TpShort:
    case TpArrayShort:
      return sizeof(short);
    case TpUShort:
    case TpArrayUShort:
      return sizeof(unsigned short);
    case TpInt:
    case TpArrayInt:
      return sizeof(int);
    case TpUInt:
    case TpArrayUInt:
      return sizeof(unsigned int);
    case TpInt64:
    case TpArrayInt64:
      return sizeof(int64_t);
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

// # Get the canonical size of the data type.
int ValType::getCanonicalSize(DataType dt, bool BECanonical) {
  if (BECanonical) {
    switch (dt) {
      case TpChar:
      case TpArrayChar:
        return CanonicalConversion::canonicalSize(static_cast<char*>(nullptr));
      case TpUChar:
      case TpArrayUChar:
        return CanonicalConversion::canonicalSize(static_cast<unsigned char*>(nullptr));
      case TpShort:
      case TpArrayShort:
        return CanonicalConversion::canonicalSize(static_cast<short*>(nullptr));
      case TpUShort:
      case TpArrayUShort:
        return CanonicalConversion::canonicalSize(static_cast<unsigned short*>(nullptr));
      case TpInt:
      case TpArrayInt:
        return CanonicalConversion::canonicalSize(static_cast<int*>(nullptr));
      case TpUInt:
      case TpArrayUInt:
        return CanonicalConversion::canonicalSize(static_cast<unsigned int*>(nullptr));
      case TpInt64:
      case TpArrayInt64:
        return CanonicalConversion::canonicalSize(static_cast<int64_t*>(nullptr));
      case TpFloat:
      case TpArrayFloat:
        return CanonicalConversion::canonicalSize(static_cast<float*>(nullptr));
      case TpDouble:
      case TpArrayDouble:
        return CanonicalConversion::canonicalSize(static_cast<double*>(nullptr));
      case TpComplex:
      case TpArrayComplex:
        return 2 * CanonicalConversion::canonicalSize(static_cast<float*>(nullptr));
      case TpDComplex:
      case TpArrayDComplex:
        return 2 * CanonicalConversion::canonicalSize(static_cast<double*>(nullptr));
      default:
        break;
    }
  } else {
    switch (dt) {
      case TpChar:
      case TpArrayChar:
        return LECanonicalConversion::canonicalSize(static_cast<char*>(nullptr));
      case TpUChar:
      case TpArrayUChar:
        return LECanonicalConversion::canonicalSize(static_cast<unsigned char*>(nullptr));
      case TpShort:
      case TpArrayShort:
        return LECanonicalConversion::canonicalSize(static_cast<short*>(nullptr));
      case TpUShort:
      case TpArrayUShort:
        return LECanonicalConversion::canonicalSize(static_cast<unsigned short*>(nullptr));
      case TpInt:
      case TpArrayInt:
        return LECanonicalConversion::canonicalSize(static_cast<int*>(nullptr));
      case TpUInt:
      case TpArrayUInt:
        return LECanonicalConversion::canonicalSize(static_cast<unsigned int*>(nullptr));
      case TpInt64:
      case TpArrayInt64:
        return LECanonicalConversion::canonicalSize(static_cast<int64_t*>(nullptr));
      case TpFloat:
      case TpArrayFloat:
        return LECanonicalConversion::canonicalSize(static_cast<float*>(nullptr));
      case TpDouble:
      case TpArrayDouble:
        return LECanonicalConversion::canonicalSize(static_cast<double*>(nullptr));
      case TpComplex:
      case TpArrayComplex:
        return 2 * LECanonicalConversion::canonicalSize(static_cast<float*>(nullptr));
      case TpDComplex:
      case TpArrayDComplex:
        return 2 * LECanonicalConversion::canonicalSize(static_cast<double*>(nullptr));
      default:
        break;
    }
  }
  return 0;
}

void ValType::getCanonicalFunc(DataType dt, Conversion::ValueFunction*& readFunc,
                               Conversion::ValueFunction*& writeFunc,
                               unsigned int& nrElementsPerValue, bool BECanonical) {
  nrElementsPerValue = 1;
  if (BECanonical) {
    switch (dt) {
      case TpBool:
      case TpArrayBool:
        readFunc = &Conversion::bitToBool;
        writeFunc = &Conversion::boolToBit;
        break;
      case TpChar:
      case TpArrayChar:
        readFunc = CanonicalConversion::getToLocal(static_cast<unsigned char*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<unsigned char*>(nullptr));
        break;
      case TpUChar:
      case TpArrayUChar:
        readFunc = CanonicalConversion::getToLocal(static_cast<unsigned char*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<unsigned char*>(nullptr));
        break;
      case TpShort:
      case TpArrayShort:
        readFunc = CanonicalConversion::getToLocal(static_cast<short*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<short*>(nullptr));
        break;
      case TpUShort:
      case TpArrayUShort:
        readFunc = CanonicalConversion::getToLocal(static_cast<unsigned short*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<unsigned short*>(nullptr));
        break;
      case TpInt:
      case TpArrayInt:
        readFunc = CanonicalConversion::getToLocal(static_cast<int*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<int*>(nullptr));
        break;
      case TpUInt:
      case TpArrayUInt:
        readFunc = CanonicalConversion::getToLocal(static_cast<unsigned int*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<unsigned int*>(nullptr));
        break;
      case TpInt64:
      case TpArrayInt64:
        readFunc = CanonicalConversion::getToLocal(static_cast<int64_t*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<int64_t*>(nullptr));
        break;
      case TpComplex:
      case TpArrayComplex:
        nrElementsPerValue = 2;
        CASACORE_FALLTHROUGH;
      case TpFloat:
      case TpArrayFloat:
        readFunc = CanonicalConversion::getToLocal(static_cast<float*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<float*>(nullptr));
        break;
      case TpDComplex:
      case TpArrayDComplex:
        nrElementsPerValue = 2;
        CASACORE_FALLTHROUGH;
      case TpDouble:
      case TpArrayDouble:
        readFunc = CanonicalConversion::getToLocal(static_cast<double*>(nullptr));
        writeFunc = CanonicalConversion::getFromLocal(static_cast<double*>(nullptr));
        break;
      default:
        readFunc = nullptr;
        writeFunc = nullptr;
    }
  } else {
    switch (dt) {
      case TpBool:
      case TpArrayBool:
        readFunc = &Conversion::bitToBool;
        writeFunc = &Conversion::boolToBit;
        break;
      case TpChar:
      case TpArrayChar:
        readFunc = LECanonicalConversion::getToLocal(static_cast<unsigned char*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<unsigned char*>(nullptr));
        break;
      case TpUChar:
      case TpArrayUChar:
        readFunc = LECanonicalConversion::getToLocal(static_cast<unsigned char*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<unsigned char*>(nullptr));
        break;
      case TpShort:
      case TpArrayShort:
        readFunc = LECanonicalConversion::getToLocal(static_cast<short*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<short*>(nullptr));
        break;
      case TpUShort:
      case TpArrayUShort:
        readFunc = LECanonicalConversion::getToLocal(static_cast<unsigned short*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<unsigned short*>(nullptr));
        break;
      case TpInt:
      case TpArrayInt:
        readFunc = LECanonicalConversion::getToLocal(static_cast<int*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<int*>(nullptr));
        break;
      case TpUInt:
      case TpArrayUInt:
        readFunc = LECanonicalConversion::getToLocal(static_cast<unsigned int*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<unsigned int*>(nullptr));
        break;
      case TpInt64:
      case TpArrayInt64:
        readFunc = LECanonicalConversion::getToLocal(static_cast<int64_t*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<int64_t*>(nullptr));
        break;
      case TpComplex:
      case TpArrayComplex:
        nrElementsPerValue = 2;
        CASACORE_FALLTHROUGH;
      case TpFloat:
      case TpArrayFloat:
        readFunc = LECanonicalConversion::getToLocal(static_cast<float*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<float*>(nullptr));
        break;
      case TpDComplex:
      case TpArrayDComplex:
        nrElementsPerValue = 2;
        CASACORE_FALLTHROUGH;
      case TpDouble:
      case TpArrayDouble:
        readFunc = LECanonicalConversion::getToLocal(static_cast<double*>(nullptr));
        writeFunc = LECanonicalConversion::getFromLocal(static_cast<double*>(nullptr));
        break;
      default:
        readFunc = nullptr;
        writeFunc = nullptr;
    }
  }
}

// # Test if a data type can be promoted to another.
// # Note that the cases fall through.
bool ValType::isPromotable(DataType from, DataType to) {
  if (from == TpOther) return false;
  if (from == to) return true;
  switch (from) {
    case TpChar:
      if (to == TpShort) return true;
      CASACORE_FALLTHROUGH;
    case TpShort:
      if (to == TpInt) return true;
      CASACORE_FALLTHROUGH;
    case TpInt:
      if (to == TpInt64) return true;
      CASACORE_FALLTHROUGH;
    case TpInt64:
    case TpFloat:
    case TpDouble:
      if (to == TpFloat || to == TpDouble) return true;
      CASACORE_FALLTHROUGH;
    case TpComplex:
    case TpDComplex:
      if (to == TpComplex || to == TpDComplex) return true;
      return false;
    case TpUChar:
      if (to == TpUShort) return true;
      CASACORE_FALLTHROUGH;
    case TpUShort:
      if (to == TpUInt) return true;
      CASACORE_FALLTHROUGH;
    case TpUInt:
      if (to == TpInt64) return true;
      if (to == TpFloat || to == TpDouble) return true;
      if (to == TpComplex || to == TpDComplex) return true;
      return false;
    default:
      break;
  }
  return false;
}

// # Get the comparison routine.
ObjCompareFunc* ValType::getCmpFunc(DataType dt) {
  switch (dt) {
    case TpBool:
      return &ObjCompare<bool>::compare;
    case TpChar:
      return &ObjCompare<char>::compare;
    case TpUChar:
      return &ObjCompare<unsigned char>::compare;
    case TpShort:
      return &ObjCompare<short>::compare;
    case TpUShort:
      return &ObjCompare<unsigned short>::compare;
    case TpInt:
      return &ObjCompare<int>::compare;
    case TpUInt:
      return &ObjCompare<unsigned int>::compare;
    case TpInt64:
      return &ObjCompare<int64_t>::compare;
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
  return nullptr;
}

// # Get the comparison object.
std::shared_ptr<BaseCompare> ValType::getCmpObj(DataType dt) {
  switch (dt) {
    case TpBool:
      return std::make_shared<ObjCompare<bool>>();
    case TpChar:
      return std::make_shared<ObjCompare<char>>();
    case TpUChar:
      return std::make_shared<ObjCompare<unsigned char>>();
    case TpShort:
      return std::make_shared<ObjCompare<short>>();
    case TpUShort:
      return std::make_shared<ObjCompare<unsigned short>>();
    case TpInt:
      return std::make_shared<ObjCompare<int>>();
    case TpUInt:
      return std::make_shared<ObjCompare<unsigned int>>();
    case TpInt64:
      return std::make_shared<ObjCompare<int64_t>>();
    case TpFloat:
      return std::make_shared<ObjCompare<float>>();
    case TpDouble:
      return std::make_shared<ObjCompare<double>>();
    case TpComplex:
      return std::make_shared<ObjCompare<Complex>>();
    case TpDComplex:
      return std::make_shared<ObjCompare<DComplex>>();
    case TpString:
      return std::make_shared<ObjCompare<String>>();
    default:
      break;
  }
  return std::shared_ptr<BaseCompare>();
}

}  // namespace casacore
