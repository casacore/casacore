//# <HashMap.h>: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996,1998,1999,2001
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
#ifndef CASA_HASHMAPITER_H
#define CASA_HASHMAPITER_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/HashMap.h>

// <summary>
//     Step through a const HashMap
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
//
// <synopsis>
//     This class is an iterator, and it used to step through const
//     <linkto class=HashMap><src>HashMap</src></linkto>s. This is useful
//     when one wishes to find each of the user defined mappings in a
//     particular map.
// </synopsis>
//
// <example>
//    <srcblock>
//    #include <casacore/casa/Containers/HashMap.h>
//    #include <casacore/casa/BasicSL/String.h>
//    #include <casacore/casa/iostream.h>
//   
//    main() {
//      HashMap<String,Int> hash;
//    
//      hash.define("one",1);
//      hash.define("two",2);
//      hash.define("three",3);
//      hash.define("four",4);
//      hash.define("five",5);
//      hash.define("six",6);
//    
//      ConstHashMapIter<String,Int> iter(hash);
//      for ( iter.toStart(); ! iter.atEnd(); iter++ )
//          cout << iter.getVal() << ": " << iter.getKey() << endl;
//    }
//    </srcblock>
// </example>
//
// <motivation>
//     Sometimes one needs to step through the defined elements of an
//     associative array. The user should be told when iterator does
//     not modify the underlying data structure. The standard C++
//     <em>const</em> is not sufficient because while the internal
//     state of the iterator changes, the underlying data structure
//     is not modified. For this reason, both const and non-const
//     versions of the iterator are useful.
// </motivation>
//
namespace casacore { //#Begin casa namespace

template<class key, class val> class ConstHashMapIter {
public:

    //
    // Move the iterator to the start of the Map.
    //
    void toStart();

    //
    // Advance to the next element of the Map.
    //
    // <group>
    void operator++() { step(); }
    void operator++(int) { step(); }
    // </group>

    //
    // Get the key or value for the current position in 
    // the Map.
    //
    // <group>
    const key &getKey() const;
    const val &getVal() const;
    // </group>

    //
    // Check to see if the iterator position is at the 
    // end or beginning of the Map.
    //
    // <group>
    Bool atEnd() const { return atEnd_; }
    Bool atStart() const;
    // </group>

    //
    // Check to see if the iterator is in a valid state.
    //
    Bool isValid() const { return Container != 0 ? True : False; }

    //
    // Constructs a Map iterator from a Map (with reference semantics).
    //
    ConstHashMapIter(const HashMap<key,val> &st);

    //
    // Assign one map iterator to a map (with reference semantics).
    //
    virtual ConstHashMapIter<key,val> &operator=(const HashMap<key,val> &other);

    //
    // Constructs a Map iterator from another iterator (with reference semantics).
    //
    ConstHashMapIter(const ConstHashMapIter<key,val> &st);

    //
    // Assign one map iterator to another iterator (with reference semantics).
    //
    virtual ConstHashMapIter<key,val> &operator=(const ConstHashMapIter<key,val> &other);

    //
    // Default constructor creates an invalid Map iterator.
    //
    ConstHashMapIter() : Container(0), curBucket(0), atEnd_(False) {}

  
    //
    // Returns the default value for the Map on which this
    // iterator is operating if it is a valid iterator, otherwise
    // it throws an exception.
    //
    const val &defaultVal() const {
	if ( ! isValid() )
	    throw_invalid_hashmapiter_error();
	return Container->defaultVal();
    }

    //
    // Allows mapping functions to be performed with the
    // map on which this iterator operates. If this iterator
    // is invalid, then an exception will be thrown.
    //
    const val &operator()(const key &ky) const {
	if ( ! isValid() )
	    throw_invalid_hashmapiter_error();
	return Container->operator()(ky);
    }

    //
    // Allows one to check to see if a given key is defined
    // in the map which this iterator tracks. If this iterator
    // is invalid, then an exception will be thrown.
    //
    Bool isDefined(const key &ky) const {
	if (! isValid() )
	    throw_invalid_hashmapiter_error();
	return Container->isDefined(ky);
    }

    //
    // Returns the number of user defined mappings
    //
    uInt ndefined() const {
	if (! isValid() )
	    throw_invalid_hashmapiter_error();
	return Container->ndefined();
    }

    //
    // Returns the container on which this iterator is
    // operating.
    //
    const HashMap<key,val> &container() const {
	if ( !isValid() )
	    throw_invalid_hashmapiter_error();
	return *Container;
    }

    // dtor
    virtual ~ConstHashMapIter();

protected:

    void step();

    ListIter<OrderedPair<key,val> > iter;
    HashMap<key,val> *Container;
    uInt curBucket;
    Bool atEnd_;
};


// <summary>
//     Step through a non-const HashMap
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
//
// <synopsis>
//     This class is an iterator, and it used to step through non-const
//     <linkto class=HashMap><src>HashMap</src></linkto>s. This is useful
//     when one wishes to find each of the user defined mappings in a
//     particular map.
// </synopsis>
//
// <example>
//    <srcblock>
//    #include <aips/Containers/HashMap.h>
//    #include <casacore/casa/BasicSL/String.h>
// #include <iostream>
//   
//    main() {
//      HashMap<String,Int> hash;
//    
//      hash.define("one",1);
//      hash.define("two",2);
//      hash.define("three",3);
//      hash.define("four",4);
//      hash.define("five",5);
//      hash.define("six",6);
//    
//      HashMapIter<String,Int> iter(hash);
//      for ( iter.toStart(); ! iter.atEnd(); iter++ )
//          cout << iter.getVal() << ": " << iter.getKey() << endl;
//    }
//    </srcblock>
// </example>
//
// <motivation>
//     Same as <linkto class=ConstHashMapIter><src>ConstHashMapIter</src></linkto>,
//     but allows for modification of the underlying structure.
// </motivation>
//
template<class key, class val> class HashMapIter : public ConstHashMapIter<key,val> {
public:
    //
    // Get the key or value for the current position in 
    // the Map.
    //
    // <group>
    val &getVal();

    virtual const val &getVal() const;
    // </group>

    //
    //  These functions allow for the definition and removal of key/value
    //  relations. The "define(key &, value &)" function defines a key/value
    //  relation, and "remove(key &)" function removes a relation if it has
    //  been previously defined.
    //
    // <group>
    val &define(const key &k, const val &v) {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      return(this->Container->define(k,v));
    }
    void remove(const key &k) {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      this->Container->remove(k);
    }
    // </group>

    //
    // This returns the default value for the map that this iterator
    // is tracking. With a non-const iterator the default value can
    // be changed.
    //
    // <group>
    const val &defaultVal() const {
      return ConstHashMapIter<key,val>::defaultVal();
    }

    val &defaultVal() {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      return this->Container->defaultVal();
    }
    // </group>

    //
    // Clear all of the mappings.
    //
    void clear() {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      this->Container->clear();
    }

    //
    // Allows mapping functions to be performed with the
    // map on which this iterator operates. If this iterator
    // is invalid, then an exception will be thrown. With
    // a non-const operator, the value can be changed.
    //
    // <group>
    const val &operator()(const key &ky) const {
      return ConstHashMapIter<key,val>::operator()(ky);
    }

    val &operator()(const key &ky) {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      return(this->Container->operator()(ky));
    }
    // </group>

    //
    // This allows a MapIter to be constructed from a Map. When
    // created the new MapIter maintains a reference to the original
    // Map. If the Map to which this MapIter points is deleted, then
    // the MapIter is marked as invalid.
    //
    HashMapIter(HashMap<key,val> &st) : ConstHashMapIter<key,val>(st) {}

    //
    // This allows a MapIter to be constructed from another MapIter. 
    // When created the new MapIter maintains a reference to the Map
    // which the MapIter parameter tracked. If this Map is deleted, then
    // this MapIter is marked as invalid.
    //
    HashMapIter(const HashMapIter<key,val> &other) : ConstHashMapIter<key,val>(other) {}

    //
    // Default constructor creates an invalid Map iterator.
    //
    HashMapIter() : ConstHashMapIter<key,val>() {}


    //
    // This assignment operator allows the Map which this MapIter tracks
    // to be changed. After a call to this operator, the MapIter will track
    // the Map parameter.
    //
    virtual HashMapIter<key,val> &operator=(HashMap<key,val> &other);

    //
    // This assignment operator allows the Map which this MapIter tracks
    // to be changed. After a call to this operator, this MapIter will track
    // the Map which the MapIter parameter tracks, i.e. it will contain a
    // reference to this new Map.
    //
    virtual HashMapIter<key,val> &operator=(const HashMapIter<key,val> &other);
  
    //
    // Returns the container on which this iterator is
    // operating.
    //
    // <group>
    HashMap<key,val> &container() {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      return(*this->Container);
    }
    const HashMap<key,val> &container() const {
      if (!this->isValid())
        throw_invalid_hashmapiter_error();
      return(*this->Container);
    }
    // </group>

    // dtor
    ~HashMapIter();

protected:
    //*display 4
    //
    // These assignment operators are private and ONLY throw an 
    // exception to prevent incorrect assignments to a non-const
    // iterator.
    //
    // <group>
    ConstHashMapIter<key,val> &operator=(const HashMap<key,val> &) {
      throw_hashmapiter_init_error();
      return *this;}
    ConstHashMapIter<key,val> &operator=(const ConstHashMapIter<key,val> &) {
      throw_hashmapiter_init_error();
      return *this;}
    // </group>

};

} //#End casa namespace

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/HashMapIter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
