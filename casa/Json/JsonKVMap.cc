//# JsonKVMap.cc: Class to hold a collection of parameter name/value pairs.
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Json/JsonKVMap.h>
#include <casacore/casa/Json/JsonError.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <iostream>

using namespace std;

namespace casacore {

  JsonKVMap::JsonKVMap()
  {}

  JsonKVMap::JsonKVMap (const JsonKVMap& that)
  : map<String, JsonValue> (that)
  {}

  JsonKVMap::~JsonKVMap()
  {}

  JsonKVMap& JsonKVMap::operator= (const JsonKVMap& that)
  {
    if (this != &that) {
      map<String, JsonValue>::operator= (that);
    }
    return *this;
  }

  const JsonValue& JsonKVMap::get (const String& name) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      throw JsonError("JsonKVMap: unknown key " + name);
    }
    return value->second;
  }

  Bool JsonKVMap::getBool (const String& name, Bool defVal) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      return defVal;
    }
    return value->second.getBool();
  }
  Int64 JsonKVMap::getInt (const String& name, Int64 defVal) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      return defVal;
    }
    return value->second.getInt();
  }
  double JsonKVMap::getDouble (const String& name, double defVal) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      return defVal;
    }
    return value->second.getDouble();
  }
  DComplex JsonKVMap::getDComplex (const String& name,
                                   const DComplex& defVal) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      return defVal;
    }
    return value->second.getDComplex();
  }
  const String& JsonKVMap::getString (const String& name,
                                      const String& defVal) const
  {
    const_iterator value = find(name);
    if (value == end()) {
      return defVal;
    }
    return value->second.getString();
  }

  Record JsonKVMap::toRecord() const
  {
    Record rec;
    for (const_iterator iter=begin(); iter!=end(); ++iter) {
      rec.defineFromValueHolder (iter->first, iter->second.getValueHolder());
    }
    return rec;
  }

  void JsonKVMap::show (ostream& os) const
  {
    for (const_iterator iter=begin(); iter!=end(); ++iter) {
      os << iter->first << "\t= " << iter->second << endl;
    }
  }

  ostream& operator<< (ostream& os, const JsonKVMap& param)
  {
    for (JsonKVMap::const_iterator iter=param.begin();
         iter!=param.end();
         ++iter) {
      if (iter != param.begin()) {
        os << ", ";
      }
      os << '"' << iter->first << '"' << ':' << iter->second;
    }
    return os;
  }


} // end namespace
