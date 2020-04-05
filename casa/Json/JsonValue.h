//# JsonValue.h: Class to hold any JSON value
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
//#
//# $Id: JsonValue.h 14057 2009-09-18 12:26:29Z diepen $

#ifndef CASA_JSONVALUE_H
#define CASA_JSONVALUE_H

//# Includes
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <vector>
#include <iosfwd>

namespace casacore {

  //# Forward Declarations
  class JsonKVMap;
  class ValueHolder;
  class IPosition;

  
  // <summary>
  // Class to hold any JSON value
  // </summary>

  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="tJsonValue">
  // </reviewed>

  //# <prerequisite>
  //# </prerequisite>

  // <synopsis>
  // Class JsonValue can hold an arbitrary JSON value which can be a scalar,
  // a JsonKVMap object, or a vector of JsonValue objects. In this way
  // JSON values can be nested in any way.
  // 
  // Internally scalar values are kept as Bool, Int64, Double, DComplex or
  // String values. The functions to obtain the value convert if possible.
  // Note that conversion from Int64 to Bool is supported.
  // The value can also be obtained as a ValueHolder object making it easier
  // to use in other Casacore code.
  // Null is also a valid JsonValue. A null value can be obtained as a
  // floating point value resulting in a NaN. It can also be obtained as a
  // null ValueHolder. Getting it for other types results in an exception.
  //
  // It is possible to obtain the value as a multi-dimensional Array object
  // if the values are regular, thus if nested vectors have the same sizes.
  // The data type of an Array is the 'highest' data type of a value in it.
  //
  // Normally a JsonValue object is created by JsonParser and is the
  // interface to obtain a value of a field in a parsed JSON file.
  // However, users can create JsonValue objects as well.
  // </synopsis>

  // <motivation>
  // JSON is a commonly used interchange format.
  // </motivation>

  //# <todo asof="1996/03/10">
  //#   <li> 
  //# </todo>
  
  class JsonValue
  {
  public:
    // The default constructor results in a null value.
    JsonValue();

    // Construct value with given type.
    // <group>
    JsonValue (Bool);
    JsonValue (int);
    JsonValue (Int64);
    JsonValue (double);
    JsonValue (const DComplex&);
    JsonValue (const char*);
    JsonValue (const String&);
    JsonValue (const std::vector<JsonValue>&);
    JsonValue (const JsonKVMap&);
    // </group>
      
    // Copy constructor (copy semantics).
    JsonValue (const JsonValue&);
      
    // Assignment (copy semantics).
    JsonValue& operator= (const JsonValue&);
      
    ~JsonValue();

    // Is the value a null value?
    Bool isNull() const
      { return itsValuePtr == 0; }

    // Is the value a vector?
    Bool isVector() const
      { return itsDataType == TpOther; }
      
    // Is the value a value map?
    Bool isValueMap() const
      { return itsDataType == TpRecord; }
      
    // Return the size of a value vector or map (1 is returned for a scalar).
    size_t size() const;
      
    // Get the data type of the value.
    // A ValueMap is returned as TpRecord, a vector as TpOther.
    DataType dataType() const
      { return itsDataType; }

    // Get the most common data type of the value inside a possibly
    // nested vector.
    // <br>- If the value is a single value, that type is returned.
    // <br>- If any vector value is a ValueMap, TpRecord is returned.
    // <br>- If any vector contains non-matching data types, TpOther is
    //       returned.
    // <br>- Otherwise the 'highest' data type is returned.
    // <group>
    DataType arrayDataType() const;
    DataType vectorDataType (const std::vector<JsonValue>& vec) const;
    // </group>

    // Get the shape of an array (possibly nested vector).
    // An exception is thrown if a vector contains a ValueMap or if
    // the array shape is irregular (nested vectors have different sizes).
    IPosition shape() const;
    IPosition vectorShape (const std::vector<JsonValue>& vec) const;

    // Get the value as a ValueHolder.
    // A null value results in a null (empty) ValueHolder.
    // An exception is thrown if the value cannot be represented as such,
    // because it is a vector of differently typed values or nested vectors.
    ValueHolder getValueHolder() const;

    // Get the value in the given data type.
    // Numeric data type promotion can be done as well as conversion of
    // integer to bool (0=False, other=True). An exception is thrown if
    // a mismatching data type is used.
    // Note that a null value can only be obtained as double (giving NaN).
    // <group>
    Bool getBool() const;
    Int64 getInt() const;
    double getDouble() const;
    DComplex getDComplex() const;
    const String& getString() const;
    // </group>

    // As above, but get the value as a vector.
    // If the value is a scalar, a vector with length 1 is returned.
    // <group>
    std::vector<Bool> getVecBool() const;
    std::vector<Int64> getVecInt() const;
    std::vector<double> getVecDouble() const;
    std::vector<DComplex> getVecDComplex() const;
    std::vector<String> getVecString() const;
    const std::vector<JsonValue>& getVector() const;
    // </group>

    // Get the value as a JsonKVMap (no conversion is possible).
    const JsonKVMap& getValueMap() const;

    // Get the value as an Array. The value must be a scalar or a
    // regularly nested vector.
    // <group>
    Array<Bool> getArrayBool() const;
    Array<Int64> getArrayInt() const;
    Array<double> getArrayDouble() const;
    Array<DComplex> getArrayDComplex() const;
    Array<String> getArrayString() const;
    // </group>

    // Get functions for templated purposes
    // <group>
    void get (Bool& value) const
      { value = getBool(); }
    void get (Int64& value) const
      { value = getInt(); }
    void get (double& value) const
      { value = getDouble(); }
    void get (DComplex& value) const
      { value = getDComplex(); }
    void get (String& value) const
      { value = getString(); }
    void get (std::vector<Bool>& value) const
      { value = getVecBool(); }
    void get (std::vector<Int64>& value) const
      { value = getVecInt(); }
    void get (std::vector<double>& value) const
      { value = getVecDouble(); }
    void get (std::vector<DComplex>& value) const
      { value = getVecDComplex(); }
    void get (std::vector<String>& value) const
      { value = getVecString(); }
    void get (std::vector<JsonValue>& value) const
      { value = getVector(); }
    void get (JsonKVMap& value) const;
    // </group>

    // Show value on given ostream.
    friend ostream& operator<< (ostream&, const JsonValue&);
      
  private:
    // Remove the value.
    void clear();
      
    // Copy the value from another one.
    void copyValue (const JsonValue& that);

    // Fill an array from nested vector in a recursive way.
    template<typename T>
    T* fillArray (T* data, const T* dataEnd,
                  const std::vector<JsonValue>& vec) const
    {
      for (std::vector<JsonValue>::const_iterator iter=vec.begin();
           iter!=vec.end(); ++iter) {
        if (iter->dataType() == TpOther) {
          data = fillArray (data, dataEnd, iter->getVector());
        } else {
          AlwaysAssert (data<dataEnd, AipsError);
          iter->get (*data);
          data++;
        }
      }
      return data;
    }

    DataType itsDataType;
    void*    itsValuePtr;
  };


} // end namespace

#endif 
