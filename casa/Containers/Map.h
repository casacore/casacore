//# Map.h: Associative array classes
//# Copyright (C) 1994,1995,1999,2000
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

#ifndef CASA_MAP_H
#define CASA_MAP_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>

//
// Work around bugs in SUN\'s stupid compiler
//
#define AIPS_STUPID_SUN 1

namespace casacore { //#Begin casa namespace

//# Forward Declarations
class AipsIO;

extern void throw_mapiter_init_error();
extern void throw_map_init_error();
extern void throw_invalid_mapiter_error();
extern void throw_map_constop_error();

template<class key, class value> class MapIterRep;
template<class key, class value> class ConstMapIter;
template<class key, class value> class Map;

// <summary>Map representation class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

template<class key, class value> class MapRep {
public:

  //
  //  This is the only MapRep constructor. It takes as a parameter the
  //  default value for the map.
  //
  MapRep(const value &dflt) : DefaultVal(dflt) { }

  //
  //  This is the mapping function which maps keys to values. If the
  //  map from the key to a value is not defined, a mapping will be
  //  defined from the key to the default value (which is set from
  //  the constructor. The "isDefined()" member function can be used
  //  to check to see if a mapping is defined before using the
  //  "operator()()".
  //
  //  <note> With a constant map in the case where the key is not
  //         defined, the mapping between key and default value is 
  //         not created, but rather an exception is thrown.
  //  </note>
  //+grp
  value &operator()(const key &ky);
  const value &operator()(const key &ky) const;
  //-grp

  //
  // Returns the default value for the Map.
  //
  //+grp
  value &defaultVal() {return DefaultVal;}
  const value &defaultVal() const {return DefaultVal;}
  //-grp

  //
  // Returns a non-zero value if a mapping is defined for
  // the key parameter.
  //
  //+grp
  virtual const value *isDefined(const key &) const = 0;
  virtual value *isDefined(const key &) = 0;
  //-grp

  //
  // Returns the number of user defined mappings
  //
  virtual uInt ndefined() const = 0;

  //
  //  These functions allow for the definition and removal of key/value
  //  relations. The "define(key &, value &)" call defines a key/value
  //  relation, and "remove(key &)" removes a relation if it has
  //  been previously defined.
  //
  //+grp
  virtual value &define(const key &, const value &) = 0;
  virtual void remove(const key &) = 0;
  //-grp

  //
  // Clear all of the mappings.
  //
  virtual void clear() = 0;

  virtual MapIterRep<key,value> *getRep(Map<key,value>*) const = 0;

  virtual MapRep<key,value> *Clone() const = 0;

  //
  // Does nothing.
  //
  virtual ~MapRep();

  enum {MapRepVersion = 1};

protected:

  //  This is the default value which is return when no match is found.
  //  This prevents this class from being a PartialMap.
  value DefaultVal;

};


//
// <category lib=aips sect="Containers">
// <summary>Abstract base class for associative arrays</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
//  This is the abstract class for all "Map" classes which implement the
//  equivalent of an associative array.
//
template<class key, class value> class Map
{
public:

  //
  //  This is the mapping function which maps keys to values. If the
  //  map from the key to a value is not defined, a mapping will be
  //  defined from the key to the default value (which is set from
  //  the constructor. The "isDefined()" member function can be used
  //  to check to see if a mapping is defined before using the
  //  "operator()()".
  //
  //  <note> With a constant map in the case where the key is not
  //         defined, the mapping between key and default value is 
  //         not created, but rather an exception is thrown.
  //  </note>
  //+grp
  value &operator()(const key &ky);
  const value &operator()(const key &ky) const;
  //-grp


  //
  // Returns the default value for the Map.
  //
  //+grp
  value &defaultVal();
  const value &defaultVal() const;
  //-grp

  //
  // Returns a non-zero value if a mapping is defined for
  // the key parameter.
  //
  //+grp
  const value *isDefined(const key &k) const;
  value *isDefined(const key &k);
  //-grp

  //
  // Returns the number of user defined mappings
  //
  uInt ndefined() const;

  //
  //  These functions allow for the definition and removal of key/value
  //  relations. The "define(key &, value &)" call defines a key/value
  //  relation, and "remove(key &)" removes a relation if it has
  //  been previously defined.
  //
  //+grp
  value &define(const key &k, const value &v);
  void remove(const key &k);
  //-grp

  //
  // Clear all of the mappings.
  //
  void clear();

  //
  // Returns the iterator rep appropriate for this particular Map
  //
  MapIterRep<key,value> *getRep() const;

  //
  // This copy constructor will, for the moment, be the only
  // way to create a map.
  //
  //+grp
  Map(const Map<key,value> &m);
  Map(const Map<key,value> *m);
  //-grp

  Map<key,value> &operator=(const Map<key,value> &);
  Map<key,value> &operator=(const Map<key,value> *);

  //*display 2
  //
  // Does nothing.
  //
  virtual ~Map();

  enum {MapVersion = 1};

#if defined(AIPS_STUPID_SUN)
  ConstMapIter<key,value> *getIter() const;
#endif

protected:

  MapRep<key,value> *Rep;

  //
  // Used by derived classes
  //
  Map(MapRep<key,value> *nRep);

  //
  // Used the set the representation.
  // Always DELETES Rep if necessary.
  //
  void SetRep(MapRep<key,value> *st) {
    if (Rep) 
      delete Rep;
    Rep = st;
  }

};

//
// <category lib=aips sect="Containers">
// <summary>Abstract base class for associative array iterators</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This is the abstract base class for all (Const)MapIter
// "letters". That is all Map specializations must provide
// a "IterRep" for the particular specialization which
// will allow the (Const)MapIter envelope to traverse the
// new type of map.
//
template<class key, class value> class MapIterRep {
public:

  //
  // Check to see if the iterator is in a valid state.
  //
  virtual Bool isValid() const = 0;

  //
  // Check to see if the iterator position is at the 
  // end or beginning of the Map.
  //
  //+grp
  virtual Bool atEnd() const = 0;
  virtual Bool atStart() const = 0;
  //-grp

  //
  // Move the iterator to the start of the Map.
  //
  virtual void toStart() = 0;

  //
  // Advance to the next element of the Map.
  //
  //+grp
  virtual void operator++() = 0;
  virtual void operator++(int) = 0;
  //-grp

  //
  // Get the key for the current position in 
  // the Map.
  //
  virtual const key &getKey() const = 0;

  //
  // Return the value at the current location of the map iterator.
  // Should throw an exception if the iterator is "past the end of 
  // the Map" or if the iterator is invalid.
  //
  //+grp
  virtual value &getVal() = 0;
  virtual const value &getVal() const = 0;
  //-grp

  //
  // This returns the default value for the map that this iterator
  // is tracking. With a non-const iterator the default value can
  // be changed.
  //
  //+grp
  const value &defaultVal() const;
  value &defaultVal();
  //-grp

  //
  //  These functions allow for the definition and removal of key/value
  //  relations. The "define(key &, value &)" function defines a key/value
  //  relation, and "remove(key &)" function removes a relation if it has
  //  been previously defined.
  //
  //+grp
  value &define(const key &ky, const value &val);
  void remove(const key &ky);
  //-grp

  //
  // Clear all of the mappings.
  //
  void clear();
    

  //
  // Allows mapping functions to be performed with the
  // map on which this iterator operates. If this iterator
  // is invalid, then an exception will be thrown. With
  // a non-const operator, the value can be changed.
  //
  //+grp
  const value &operator()(const key &ky) const;
  value &operator()(const key &ky);
  //-grp

  //
  // Allows one to check to see if a given key is defined
  // in the map which this iterator tracks. If this iterator
  // is invalid, then an exception will be thrown. With
  // a non-const iterator the returned pointer can be used
  // to change the value in the map.
  //
  //+grp
  const value *isDefined(const key &ky) const;
  value *isDefined(const key &ky);
  //-grp

  //
  // Returns the number of user defined mappings
  //
  uInt ndefined() const;

  //
  // Returns the container on which this iterator is
  // operating.
  //
  //+grp
  Map<key,value> &container();
  const Map<key,value> &container() const;
  //-grp

  //
  // Duplicate a map iterator
  //
  //+grp
  virtual MapIterRep<key,value> *Clone() = 0;
  //-grp

  //
  // This allows a MapIter to be constructed from a Map. When
  // created the new MapIter maintains a reference to the original
  // Map. If the Map to which this MapIter points is deleted, then
  // the MapIter is marked as invalid.
  //
  //+grp
  MapIterRep(Map<key,value> &st);
  MapIterRep(Map<key,value> *st);
  //-grp

  virtual ~MapIterRep();

  enum {MapIterRepVersion = 1};

protected:

  Map<key,value> *Container;

};

//
// <category lib=aips sect="Containers">
// <summary>Const associative array iterator</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This class implements the mechanism for traversing constant
// associative arrays, i.e. "Map"s. This allows one to move
// the cursor to the beginning of the map and serially traverse
// the map. The key and value elements can be extracted at
// each position in the Map. For example:
// <code>
//   template<class key,class value> void print(const Map<key,value> &xx){
//     ConstMapIter<key,value> x(xx);
//     x.toStart();
//     while (!x.atEnd()) {
//       cout << "(" << x.getKey() << "," << x.getVal() << ")" << " ";
//       x++;
//     }
//     cout << endl;
//  }
// </code>
// This example declares a templated function which accepts a const
// Map as a parameter, and iterates through the map displaying the
// key/value pairs at each positon.
//
template<class key, class value> class ConstMapIter
{
public:

  //
  // Move the iterator to the start of the Map.
  //
  virtual void toStart();

  //
  // Advance to the next element of the Map.
  //
  //+grp
  virtual void operator++();
  virtual void operator++(int);
  //-grp

  //
  // Get the key or value for the current position in 
  // the Map.
  //
  //+grp
  virtual const key &getKey() const;
  virtual const value &getVal() const;
  //-grp

  //
  // Check to see if the iterator position is at the 
  // end or beginning of the Map.
  //
  //+grp
  virtual Bool atEnd() const;
  virtual Bool atStart() const;
  //-grp

  //
  // Check to see if the iterator is in a valid state.
  //
  virtual Bool isValid() const;

  //
  // Constructs a Map iterator from a Map (with reference semantics).
  //
  //+grp
  ConstMapIter(const Map<key,value> *st);
  ConstMapIter(const Map<key,value> &st);
  //-grp

  //
  // Assign one map iterator to a map (with reference semantics).
  //
  //+grp
  virtual ConstMapIter<key,value> &operator=(const Map<key,value> &other);
  virtual ConstMapIter<key,value> &operator=(const Map<key,value> *other);
  //-grp

  //
  // Constructs a Map iterator from another iterator (with reference semantics).
  //
  //+grp
  ConstMapIter(const ConstMapIter<key,value> *st);
  ConstMapIter(const ConstMapIter<key,value> &st);

  //-grp

  //
  // Assign one map iterator to another iterator (with reference semantics).
  //
  //+grp
  virtual ConstMapIter<key,value> &operator=(const ConstMapIter<key,value> &other);
  virtual ConstMapIter<key,value> &operator=(const ConstMapIter<key,value> *other);
  //-grp

  //
  // Default constructor creates an invalid Map iterator.
  //
  ConstMapIter() : Rep(0) {}

  
  //
  // Returns the default value for the Map on which this
  // iterator is operating if it is a valid iterator, otherwise
  // it throws an exception.
  //
  const value &defaultVal() const;

  //
  // Allows mapping functions to be performed with the
  // map on which this iterator operates. If this iterator
  // is invalid, then an exception will be thrown.
  //
  const value &operator()(const key &ky) const;

  //
  // Allows one to check to see if a given key is defined
  // in the map which this iterator tracks. If this iterator
  // is invalid, then an exception will be thrown.
  //
  const value *isDefined(const key &ky) const;

  //
  // Returns the number of user defined mappings
  //
  uInt ndefined() const;

  //
  // Returns the container on which this iterator is
  // operating.
  //
  const Map<key,value> &container() const;

  virtual ~ConstMapIter();

  enum {ConstMapIterVersion = 1};

protected:
  MapIterRep<key,value> *Rep;

  //
  // Dummy used to initialization by derived classes.
  //
  ConstMapIter(MapIterRep<key,value> *st) : Rep(st) {}

  //
  // Always DELETES Rep if necessary
  //
  void SetRep(MapIterRep<key,value> *st) {
    if (Rep) 
      delete Rep;
    Rep = st;
  }

};


//#
//
// <category lib=aips sect="Containers">
// <summary>Associative array iterator</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This class implements the mechanism for traversing associative 
// arrays, i.e. "Map"s. It provides the traversal mechanisms of the
// ConstMapIter, but adds the mechansims to modify the values, and
// perform other modification functions which the Maps provide, e.g.
// define(). 
//
template<class key, class value> class MapIter : virtual public ConstMapIter<key,value> {
public:

  //
  // Return the value at the current location of the map iterator.
  // Should throw an exception if the iterator is "past the end of 
  // the Map" or if the iterator is invalid.
  //
  //+grp
  virtual value &getVal();

  virtual const value &getVal() const;
  //-grp

  //
  //  These functions allow for the definition and removal of key/value
  //  relations. The "define(key &, value &)" function defines a key/value
  //  relation, and "remove(key &)" function removes a relation if it has
  //  been previously defined.
  //
  //+grp
  value &define(const key &ky, const value &val) {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    return(this->Rep->define(ky,val));
  }
  void remove(const key &ky) {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    this->Rep->remove(ky);
  }
  //-grp

  //
  // This returns the default value for the map that this iterator
  // is tracking. With a non-const iterator the default value can
  // be changed.
  //
  //+grp
  const value &defaultVal() const {
    return ConstMapIter<key,value>::defaultVal();
  }

  value &defaultVal() {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    return this->Rep->defaultVal();
  }
  //-grp

  //
  // Clear all of the mappings.
  //
  void clear() {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    this->Rep->clear();
  }

  //
  // Allows mapping functions to be performed with the
  // map on which this iterator operates. If this iterator
  // is invalid, then an exception will be thrown. With
  // a non-const operator, the value can be changed.
  //
  //+grp
  const value &operator()(const key &ky) const {
    return ConstMapIter<key,value>::operator()(ky);
  }

  value &operator()(const key &ky) {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    return(this->Rep->operator()(ky));
  }
  //-grp

  //
  // Allows one to check to see if a given key is defined
  // in the map which this iterator tracks. If this iterator
  // is invalid, then an exception will be thrown. With
  // a non-const iterator the returned pointer can be used
  // to change the value in the map.
  //
  //+grp
  const value *isDefined(const key &ky) const {
    return ConstMapIter<key,value>::isDefined(ky);
  }

  value *isDefined(const key &ky) {
    if (!this->isValid())
      throw_invalid_mapiter_error();
    return(this->Rep->isDefined(ky));
  }
  //-grp

  //
  // This allows a MapIter to be constructed from a Map. When
  // created the new MapIter maintains a reference to the original
  // Map. If the Map to which this MapIter points is deleted, then
  // the MapIter is marked as invalid.
  //
  //+grp
  MapIter(Map<key,value> *other) : 
              ConstMapIter<key,value>(other ? other->getRep() : 0) {}
  MapIter(Map<key,value> &st) : ConstMapIter<key,value>(st.getRep()) {}
  //-grp

  //
  // This allows a MapIter to be constructed from another MapIter. 
  // When created the new MapIter maintains a reference to the Map
  // which the MapIter parameter tracked. If this Map is deleted, then
  // this MapIter is marked as invalid.
  //
  //+grp
  MapIter(const MapIter<key,value> &other) : 
              ConstMapIter<key,value>(other.isValid() ? other.Rep->Clone() : 0) {}

  MapIter(const MapIter<key,value> *other) : 
              ConstMapIter<key,value>(other && (*other).isValid() ? other->Rep->Clone() : 0) {}
  //-grp

  //
  // Default constructor creates an invalid Map iterator.
  //
  MapIter() : ConstMapIter<key,value>() {}


  //
  // This assignment operator allows the Map which this MapIter tracks
  // to be changed. After a call to this operator, the MapIter will track
  // the Map parameter.
  //
  //+grp
  virtual MapIter<key,value> &operator=(Map<key,value> &other);

  virtual MapIter<key,value> &operator=(Map<key,value> *other);
  //-grp

  //
  // This assignment operator allows the Map which this MapIter tracks
  // to be changed. After a call to this operator, this MapIter will track
  // the Map which the MapIter parameter trackes, i.e. it will contain a
  // reference to this new Map.
  //
  //+grp
  virtual MapIter<key,value> &operator=(const MapIter<key,value> &other);

  virtual MapIter<key,value> &operator=(const MapIter<key,value> *other);
  //-grp
  
  //
  // Returns the container on which this iterator is
  // operating.
  //
  //+grp
  Map<key,value> &container() { 
    return(this->Rep->container());}
  const Map<key,value> &container() const {
    return(ConstMapIter<key,value>::container());}
  //-grp

  ~MapIter() {}

  enum {MapIterVersion = 1};

protected:
  //*display 4
  //
  // These assignment operators are private and ONLY throw an 
  // exception to prevent incorrect assignments to a non-const
  // iterator.
  //
  //+grp
  ConstMapIter<key,value> &operator=(const Map<key,value> &) {
    throw_mapiter_init_error();
    return *this;}
  ConstMapIter<key,value> &operator=(const Map<key,value> *) {
    throw_mapiter_init_error();
    return *this;}
  ConstMapIter<key,value> &operator=(const ConstMapIter<key,value> &) {
    throw_mapiter_init_error();
    return *this;}
  ConstMapIter<key,value> &operator=(const ConstMapIter<key,value> *) {
    throw_mapiter_init_error();
    return *this;}
  //-grp

};

} //#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/Map.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
