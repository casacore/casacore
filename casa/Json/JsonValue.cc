//# JsonValue.cc: Class to hold a general Json value
//# Copyright (C) 2016
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

#include <casacore/casa/Json/JsonValue.h>
#include <casacore/casa/Json/JsonKVMap.h>
#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>

using namespace std;

namespace casacore {

  JsonValue::JsonValue()
  : itsDataType (TpNumberOfTypes),      // use a non-existing type
    itsValuePtr (0)
  {}

  JsonValue::JsonValue (Bool value)
  : itsDataType (TpBool),
    itsValuePtr (new Bool(value))
  {}

  JsonValue::JsonValue (int value)
  : itsDataType (TpInt64),
    itsValuePtr (new Int64(value))
  {}

  JsonValue::JsonValue (Int64 value)
  : itsDataType (TpInt64),
    itsValuePtr (new Int64(value))
  {}

  JsonValue::JsonValue (double value)
  : itsDataType (TpDouble),
    itsValuePtr (new double(value))
  {}

  JsonValue::JsonValue (const DComplex& value)
  : itsDataType (TpDComplex),
    itsValuePtr (new DComplex(value))
  {}

  JsonValue::JsonValue (const char* value)
  : itsDataType (TpString),
    itsValuePtr (new String(value))
  {}

  JsonValue::JsonValue (const String& value)
  : itsDataType (TpString),
    itsValuePtr (new String(value))
  {}

  JsonValue::JsonValue (const vector<JsonValue>& value)
  : itsDataType (TpOther),
    itsValuePtr (new vector<JsonValue>(value))
  {}

  JsonValue::JsonValue (const JsonKVMap& value)
  : itsDataType (TpRecord),
    itsValuePtr (new JsonKVMap(value))
  {}

  JsonValue::JsonValue (const JsonValue& that)
  : itsValuePtr (0)
  {
    copyValue (that);
  }
 
  JsonValue& JsonValue::operator= (const JsonValue& that)
  {
    if (this != &that) {
      clear();
      copyValue (that);
    }
    return *this;
  }

  JsonValue::~JsonValue()
  {
    clear();
  }

  void JsonValue::clear()
  {
    if (itsValuePtr) {
      switch (itsDataType) {
      case TpBool:
        delete (Bool*)itsValuePtr;
        break;
      case TpInt64:
        delete (Int64*)itsValuePtr;
        break;
      case TpDouble:
        delete (double*)itsValuePtr;
        break;
      case TpDComplex:
        delete (DComplex*)itsValuePtr;
        break;
      case TpString:
        delete (String*)itsValuePtr;
        break;
      case TpOther:
        delete (vector<JsonValue>*)itsValuePtr;
        break;
      case TpRecord:
        delete (JsonKVMap*)itsValuePtr;
        break;
      default:
        throw JsonError("JsonValue::clear - invalid data type");
      }
    }
    itsValuePtr = 0;
  }

  void JsonValue::copyValue (const JsonValue& that)
  {
    itsDataType = that.itsDataType;
    if (that.itsValuePtr) {
      switch (itsDataType) {
      case TpBool:
        itsValuePtr = new Bool (that.getBool());
        break;
      case TpInt64:
        itsValuePtr = new Int64 (that.getInt());
        break;
      case TpDouble:
        itsValuePtr = new double (that.getDouble());
        break;
      case TpDComplex:
        itsValuePtr = new DComplex (that.getDComplex());
        break;
      case TpString:
        itsValuePtr = new String (that.getString());
        break;
      case TpOther:
        itsValuePtr = new vector<JsonValue> (that.getVector());
        break;
      case TpRecord:
        itsValuePtr = new JsonKVMap (that.getValueMap());
        break;
      default:
        throw JsonError("JsonValue::copyValue - invalid data type");
      }
    }
  }

  size_t JsonValue::size() const
  {
    if (isNull()) {
      return 0;
    }
    switch (itsDataType) {
    case TpOther:
      return ((vector<JsonValue>*)itsValuePtr)->size();
      break;
    case TpRecord:
      return ((JsonKVMap*)itsValuePtr)->size();
      break;
    default:
      return 1;
    }
  }

  DataType JsonValue::arrayDataType() const
  {
    DataType vdt = dataType();
    if (vdt != TpOther) {
      return vdt;
    }
    return vectorDataType (getVector());
  }

  DataType JsonValue::vectorDataType (const vector<JsonValue>& vec) const
  {
    DataType vdt = TpOther;   // indicates first time
    for (vector<JsonValue>::const_iterator iter=vec.begin();
         iter!=vec.end(); ++iter) {
      DataType dt = iter->dataType();
      if (dt == TpRecord) {
        return dt;
      } else if (vdt == TpOther) {
        if (dt == TpOther) {
          dt = vectorDataType (iter->getVector());
        }
        vdt = dt;
      } else if (dt != vdt) {
        if (dt  == TpBool  ||  dt  == TpString  ||
            vdt == TpBool  ||  vdt == TpString) {
          return TpRecord;
        }
        if (dt == TpDComplex  ||  vdt == TpDComplex) {
          vdt = TpDComplex;
        } else {
          vdt = TpDouble;
        }
      }
    }
    return vdt;
  }

  IPosition JsonValue::shape() const
  {
    if (dataType() == TpRecord) {
      throw JsonError("JsonValue::shape - vector contains a ValueMap");
    }
    if (dataType() == TpOther) {
      return vectorShape (getVector());
    }
    return IPosition(1,1);
  }

  IPosition JsonValue::vectorShape (const vector<JsonValue>& vec) const
  {
    IPosition shp(1,0);
    IPosition nshp;
    Bool first  = True;
    Bool nested = False;
    for (vector<JsonValue>::const_iterator iter=vec.begin();
         iter!=vec.end(); ++iter) {
      if (iter->dataType() == TpRecord) {
        throw JsonError("JsonValue::shape - vector contains a ValueMap");
      }
      shp[0]++;
      if (first) {
        first = False;
        if (iter->isVector()) {
          nested = True;
          nshp   = iter->shape();
        }
      } else {
        if (nested != iter->isVector()) {
          throw JsonError("JsonValue::shape - vector contains scalars and vectors");
        }
        if (nested  &&  !nshp.isEqual (iter->shape())) {
          throw JsonError("JsonValue::shape - irregular nested vector sizes");
        }
      }
    }
    return nshp.concatenate (shp);
  }

  ValueHolder JsonValue::getValueHolder() const
  {
    if (isNull()) {
      return ValueHolder();
    }
    switch (itsDataType) {
    case TpBool:
      return ValueHolder(*(Bool*)itsValuePtr);
    case TpInt64:
      return ValueHolder(*(Int64*)itsValuePtr);
    case TpDouble:
      return ValueHolder(*(double*)itsValuePtr);
    case TpDComplex:
      return ValueHolder(*(DComplex*)itsValuePtr);
    case TpString:
      return ValueHolder(*(String*)itsValuePtr);
    case TpRecord:
      return ValueHolder(((JsonKVMap*)itsValuePtr)->toRecord());
    case TpOther:
      break;   // a vector which is handled below
    default:
      throw JsonError("JsonValue::getValueHolder - invalid data type");
    }
    vector<JsonValue> vec = getVector();
    switch (vectorDataType(vec)) {
    case TpBool:
      return ValueHolder(Vector<Bool>(getVecBool()));
    case TpInt64:
      return ValueHolder(Vector<Int64>(getVecInt()));
    case TpDouble:
      return ValueHolder(Vector<double>(getVecDouble()));
    case TpDComplex:
      return ValueHolder(Vector<DComplex>(getVecDComplex()));
    case TpString:
      return ValueHolder(Vector<String>(getVecString()));
    case TpOther:
      return ValueHolder(1, True);    // untyped array with 1 axis
    default:
      throw JsonError("JsonValue::getValueHolder - vector of mixed data types");
    }
  }

  Bool JsonValue::getBool() const
  {
    switch (itsDataType) {
    case TpBool:
      return *(Bool*)itsValuePtr;
    case TpInt64:
      return (*(Int64*)itsValuePtr != 0);
    default:
      throw JsonError("JsonValue::getBool - invalid data type");
    }
  }

  Int64 JsonValue::getInt() const
  {
    switch (itsDataType) {
    case TpInt64:
      return *(Int64*)itsValuePtr;
    default:
      throw JsonError("JsonValue::getInt - invalid data type");
    }
  }

  double JsonValue::getDouble() const
  {
    if (isNull()) {
      return doubleNaN();
    }
    switch (itsDataType) {
    case TpInt64:
      return *(Int64*)itsValuePtr;
    case TpDouble:
      return *(double*)itsValuePtr;
    default:
      throw JsonError("JsonValue::getDouble - invalid data type");
    }
  }

  DComplex JsonValue::getDComplex() const
  {
    if (isNull()) {
      return DComplex(doubleNaN(), doubleNaN());
    }
    switch (itsDataType) {
    case TpInt64:
      return DComplex(*(Int64*)itsValuePtr, 0.0);
    case TpDouble:
      return DComplex(*(double*)itsValuePtr, 0.0);
    case TpDComplex:
      return *(DComplex*)itsValuePtr;
    default:
      throw JsonError("JsonValue::getDComplex - invalid data type");
    }
  }

  const String& JsonValue::getString() const
  {
    switch (itsDataType) {
    case TpString:
      return *(String*)itsValuePtr;
    default:
      throw JsonError("JsonValue::getString - invalid data type");
    }
  }

  vector<Bool> JsonValue::getVecBool() const
  {
    if (itsDataType == TpOther) {
      const vector<JsonValue>& kvvec = *(const vector<JsonValue>*)itsValuePtr;
      vector<Bool> vec(kvvec.size());
      for (size_t i=0; i<vec.size(); i++) {
        vec[i] = kvvec[i].getBool();
      }
      return vec;
    }
    vector<Bool> vec(1);
    vec[0] = getBool();
    return vec;
  }

  vector<Int64> JsonValue::getVecInt() const
  {
    if (itsDataType == TpOther) {
      const vector<JsonValue>& kvvec = *(const vector<JsonValue>*)itsValuePtr;
      vector<Int64> vec(kvvec.size());
      for (size_t i=0; i<vec.size(); i++) {
        vec[i] = kvvec[i].getInt();
      }
      return vec;
    }
    vector<Int64> vec(1);
    vec[0] = getInt();
    return vec;
  }

  vector<double> JsonValue::getVecDouble() const
  {
    if (itsDataType == TpOther) {
      const vector<JsonValue>& kvvec = *(const vector<JsonValue>*)itsValuePtr;
      vector<double> vec(kvvec.size());
      for (size_t i=0; i<vec.size(); i++) {
        vec[i] = kvvec[i].getDouble();
      }
      return vec;
    }
    vector<double> vec(1);
    vec[0] = getDouble();
    return vec;
  }

  vector<DComplex> JsonValue::getVecDComplex() const
  {
    if (itsDataType == TpOther) {
      const vector<JsonValue>& kvvec = *(const vector<JsonValue>*)itsValuePtr;
      vector<DComplex> vec(kvvec.size());
      for (size_t i=0; i<vec.size(); i++) {
        vec[i] = kvvec[i].getDComplex();
      }
      return vec;
    }
    vector<DComplex> vec(1);
    vec[0] = getDComplex();
    return vec;
  }

  vector<String> JsonValue::getVecString() const
  {
    if (itsDataType == TpOther) {
      const vector<JsonValue>& kvvec = *(const vector<JsonValue>*)itsValuePtr;
      vector<String> vec(kvvec.size());
      for (size_t i=0; i<vec.size(); i++) {
        vec[i] = kvvec[i].getString();
      }
      return vec;
    }
    vector<String> vec(1);
    vec[0] = getString();
    return vec;
  }

  const vector<JsonValue>& JsonValue::getVector() const
  {
    if (itsDataType == TpOther) {
      return *(vector<JsonValue>*)itsValuePtr;
    }
    throw JsonError("JsonValue::getVector - invalid data type");
  }

  const JsonKVMap& JsonValue::getValueMap() const
  {
    switch (itsDataType) {
    case TpRecord:
      return *(JsonKVMap*)itsValuePtr;
    default:
      throw JsonError("JsonValue::getValueMap - invalid data type");
    }
  }

  void JsonValue::get (JsonKVMap& value) const
  {
    value = getValueMap();
  }

  Array<Bool> JsonValue::getArrayBool() const
  {
    Array<Bool> arr(shape());
    Bool* data = arr.data();
    fillArray (data, data+arr.size(), getVector());
    return arr;
  }

  Array<Int64> JsonValue::getArrayInt() const
  {
    Array<Int64> arr(shape());
    Int64* data = arr.data();
    fillArray (data, data+arr.size(), getVector());
    return arr;
  }

  Array<double> JsonValue::getArrayDouble() const
  {
    Array<double> arr(shape());
    double* data = arr.data();
    fillArray (data, data+arr.size(), getVector());
    return arr;
  }

  Array<DComplex> JsonValue::getArrayDComplex() const
  {
    Array<DComplex> arr(shape());
    DComplex* data = arr.data();
    fillArray (data, data+arr.size(), getVector());
    return arr;
  }

  Array<String> JsonValue::getArrayString() const
  {
    Array<String> arr(shape());
    String* data = arr.data();
    fillArray (data, data+arr.size(), getVector());
    return arr;
  }

  ostream& operator<< (ostream& os, const JsonValue& param)
  {
    JsonOut js(os);
    if (param.isNull()) {
      js.putNull();
    } else {
      switch (param.itsDataType) {
      case TpBool:
        js.put (*(Bool*)(param.itsValuePtr));
        break;
      case TpInt64:
        js.put (*(Int64*)(param.itsValuePtr));
        break;
      case TpDouble:
        js.put (*(double*)(param.itsValuePtr));
        break;
      case TpDComplex:
        js.put (*(DComplex*)(param.itsValuePtr));
        break;
      case TpString:
        js.put (*(String*)(param.itsValuePtr));
        break;
      case TpOther:
        {
          const vector<JsonValue>& vec = param.getVector();
          os << '[';
          for (size_t i=0; i<vec.size(); i++) {
            if (i > 0) {
              os << ',';
            }
            os << vec[i];
          }
          os << ']';
        }
        break;
      case TpRecord:
        {
          const JsonKVMap& blk = param.getValueMap();
          os << '[';
          if (blk.size() == 0) {
            os << '=';
          } else {
            os << blk;
          }
          os << ']';
        }
        break;
      default:
        throw JsonError("JsonValue::operator<< - invalid data type");
      }
    }
    return os;
  }
    
    
} // end namespace
