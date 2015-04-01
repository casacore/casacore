//# SimOrdMap.h: Simple map with ordered keys
//# Copyright (C) 1993,1994,1995,1996,1999,2000
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

#ifndef CASA_SIMORDMAP_H
#define CASA_SIMORDMAP_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/OrderedPair.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Define a macro to cast kvblk[i] to OrderedPair<K,V>*.
//# This is needed because the compiler outlines the inline functions pair.
#define KVBLKpair(INX) ((OrderedPair<K,V>*)(kvblk[INX]))

// <category lib=aips sect="Containers">
// <summary>Simple map with keys ordered</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// SimpleOrderedMap<key,value> is a template class.
// It is similar to OrderedMap<key,value>, but lacks its
// sophisticated iterator capability. Instead iteration can be
// done using the getKey and getVal function with a simple index.
// The function ndefined() gives the number of key,value pairs in the map.
// 
// It uses a Block to store an array of pointers to the keys and
// the associated values.
// The keys and values themselves are stored on the heap.
// The keys are kept in order to allow a binary search through
// the keys for rapid access.
//
// This is one (simple) implementation of an ordered map.
// It is not suitable for large arrays of keys, since the overhead
// of keeping the keys in order would get too big.
//
// Exceptions are raised when new[] is failing or when the next()
// getKey() or getValue() function is failing.
//
// The AipsIO >> and << operators are defined in <aips/SimOrdMapIO.h>.


template<class K, class V> class SimpleOrderedMap
{
public:

    // Creates a map with the specified default value, "value", and the
    // internal block size.
    SimpleOrderedMap (const V& defaultValue, uInt size);

    // Creates a map with the specified default value, "value".
    explicit SimpleOrderedMap (const V& defaultValue);

    // Creates a map from another one; use copy semantics.
    SimpleOrderedMap (const SimpleOrderedMap<K,V>&);

    // Removes a map.
    ~SimpleOrderedMap ();

    // Assigns this SimpleOrderedMap to another with copy semantics.
    SimpleOrderedMap<K,V>& operator= (const SimpleOrderedMap<K,V>&);

    // Defines a mapping (ie. create a key value mapping)
    V &define (const K&, const V&);

    // This is the mapping function which maps keys to values. If the
    // map from the key to a value is not defined, a mapping will be
    // defined from the key to the default value (which is set from
    // the constructor. The "isDefined()" member function can be used
    // to check to see if a mapping is defined before using the
    // "operator()()".
    //
    // <note> With a constant map in the case where the key is not
    //        defined, the mapping between key and default value is 
    //        not created, but rather an exception is thrown.
    // </note>
    //+grp
    V &operator()(const K &ky);
    // <thrown>
    //   <li> indexError<K>
    // </thrown>
    const V &operator()(const K &ky) const;
    //-grp

    // Returns the default value for the Map.
    //+grp
    V &defaultVal()             {return DefaultVal;}
    const V &defaultVal() const {return DefaultVal;}
    //-grp

    // These functions check to see if a mapping is defined between
    // the specified key and some value. If one is, a pointer to
    // the value is returned, otherwise 0 is returned.
    //+grp
    V *isDefined(const K&);
    const V *isDefined(const K& k) const
	{ return ((SimpleOrderedMap<K,V>*)this)->isDefined(k); }
    //-grp

    // Get the number of elements in the map.
    uInt ndefined() const { return nrused; }

    // Get the i-th key in the map.
    // It can be used to iterate through the keys as:
    // <code>
    //   for (uInt i=0; i<map.ndefined(); i++) {
    //       cout << map.getKey(i) << " " << map.getVal(i) << endl;
    //   }
    // </code>
    // Index checking is only done if Block is doing it.
    const K& getKey (uInt inx) const
	{ return KVBLKpair(inx)->x(); }

    // Get the i-th value in the map.
    // It can be used to iterate through the keys as:
    // <code>
    //   for (uInt i=0; i<map.ndefined(); i++) {
    //       cout << map.getKey(i) << " " << map.getVal(i) << endl;
    //   }
    // </code>
    // Index checking is only done if Block is doing it.
    //+grp
    const V& getVal (uInt inx) const
	{ return KVBLKpair(inx)->y(); }
    V& getVal (uInt inx)
	{ return KVBLKpair(inx)->y(); }
    //-grp


    // Rename a key.
    // If the new key already exists, the existing key will be removed.
    // <thrown>
    //   <li> indexError<K>
    // </thrown>
    void rename (const K& newkey, const K& oldkey);

    // Undefines a mapping (ie. remove a key value mapping).
    // <thrown>
    //   <li> indexError<K>
    // </thrown>
    void remove (const K&);

    // Clear the entire map (ie. remove all mappings).
    void clear ();

    // Get the total size of the block in use.
    uInt ntot() const  { return kvblk.nelements(); }

    // Get or set the Block allocation increment.
    //+grp
    uInt incr() const { return nrincr; }
    uInt incr(uInt nri) { return (nrincr = nri); }
    //-grp

    // Check the internal state.
    // <thrown>
    //   <li> AipsError
    // </thrown>
    Bool ok() const;

    // Version for major change (used by SimOrdMapIO).
    // enum did not work properly with cfront 3.0.1), so replaced
    // by a static inline function. Users won't normally use this.
    //*display 8
    static uInt Version() {return 1;}

protected:
    // The blocks to hold the keys and values
    // and the total, used and increment size of these blocks.
    Block<void*> kvblk;
    uInt         nrused;
    uInt         nrincr;
    V            DefaultVal;

    // Binary search for the key.
    uInt findKey (const K&, Bool&) const;

    // Copy from another Block of OrderedPair's.
    void copyBlock (const SimpleOrderedMap<K,V>&);
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/SimOrdMap.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
