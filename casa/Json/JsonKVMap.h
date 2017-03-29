//# JsonKVMap.h: Class to hold a collection of JSON key:value pairs
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
//# $Id: JsonKVMap.h 14057 2009-09-18 12:26:29Z diepen $

#ifndef CASA_JSONKVMAP_H
#define CASA_JSONKVMAP_H

#include <casacore/casa/Json/JsonValue.h>
#include <map>
#include <iosfwd>

namespace casacore {

  //# Forward Declarations
  class ValueHolder;

  // <summary>
  // Class to hold a collection of JSON key:value pairs.
  // </summary>
  
  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="tJsonKVMap">
  // </reviewed>

  //# <prerequisite>
  //# </prerequisite>

  // <synopsis>
  // A JsonKVMap object is the result of a JSON file parsed by JsonParser.
  // It is a map of name to a JsonValue object holding an arbitrary value
  // (including a JsonKVMap for nested structs).
  //
  // JsonKVMap has functions to test if a given field is present and to
  // get its JsonValue. It also has functions to get a scalar value
  // where a default value is used if the key is undefined.
  //
  // JsonKVMap is derived from std::map, so all its functions are available.
  // Iterators to make standard iteration possible.
  // </synopsis>

  // <motivation>
  // JSON is a commonly used interchange format.
  // </motivation>

  //# <todo asof="1996/03/10">
  //#   <li> 
  //# </todo>

  class JsonKVMap: public std::map<String, JsonValue>
  {
  public:
    typedef std::map<String,JsonValue>::const_iterator const_iterator;
    typedef std::map<String,JsonValue>::iterator iterator;

    JsonKVMap();
      
    // Copy constructor (copy semantics)
    JsonKVMap (const JsonKVMap& that);
      
    ~JsonKVMap();
      
    // Assignment (copy semantics)
    JsonKVMap& operator= (const JsonKVMap& that);
      
    // Is a key defined?
    Bool isDefined (const String& name) const
      { return find(name) != end(); }

    // Get the value of a key. An exception is thrown if undefined.
    const JsonValue& get (const String& name) const;

    // \name Get the typed value of a key
    // Use the default if not existing.
    // <group>
    Bool getBool (const String& name, Bool defVal) const;
    Int64 getInt (const String& name, Int64 defVal) const;
    double getDouble (const String& name, double defVal) const;
    DComplex getDComplex (const String& name, const DComplex& defVal) const;
    const String& getString (const String& name, const String& defVal) const;
    // </group>

    // Convert the map to a Record.
    Record toRecord() const;

    // \name Show the contents of the object
    // <group>
    void show (ostream&) const;
    friend ostream& operator<< (ostream&, const JsonKVMap&);
    // </group>
  };

} //end namespace

#endif 
