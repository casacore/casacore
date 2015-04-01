//# HashMap.h: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996,1999,2000,2001
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

#ifndef CASA_HASHMAP_H
#define CASA_HASHMAP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/List.h>
#include <casacore/casa/Containers/OrderedPair.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class key,class val> class ConstHashMapIter;
extern void throw_invalid_hashmapiter_error();
extern void throw_hashmapiter_init_error();

// <summary>
//     Hash functions for standard types
// </summary>
//
// <synopsis>
// These are the declarations for the standard hash functions
// used by <linkto class=HashMap>HashMap</linkto>. In general, a function
// such as these is defined for each type that is to be used as
// a key in <linkto class=HashMap>HashMap</linkto>.
//
// These hash functions map the key type to any integer. This
// integer is then used by <linkto class=HashMap>HashMap</linkto> to
// select a bucket in the hash table.
// </synopsis>
//
// <group name=hashfunc>
uInt hashFunc(const String &);
uInt hashFunc(const float &);
uInt hashFunc(const double &);
uInt hashFunc(const int &);
uInt hashFunc(const unsigned &);
//</group>


// <summary>
//     Specify the default values for HashMap keys
// </summary>
//
// <synopsis>
// These are the declarations for a set of functions which provide
// the default values for types which are used as keys in
// <linkto class=HashMap>HashMap</linkto>.
// </synopsis>
//
// <group name=defaulthashvalue>
const Int &defaultHashValue(const Int *);
const uInt &defaultHashValue(const uInt *);
const Long &defaultHashValue(const Long *);
const uLong &defaultHashValue(const uLong *);
const Float &defaultHashValue(const Float *);
const Double &defaultHashValue(const Double *);
const lDouble &defaultHashValue(const lDouble *);
// </group>

// <summary>
//     Hash function with state
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
//
// <etymology>
//      This is basically a way of specifying a hash function, but
//      it is implemented as a class. Thus it is called HashClass,
//      similar to "hash function".
// </etymology>
//
// <synopsis>
//      This class is used to specify a hash function. Sometimes a hash
//      function may require state, it may be useful to create a
//      hierarchy of hash functions, or it may be useful to create a class
//      which provides for hashing as well as other functionality. This
//      class can be used as a base class for any of these purposed. This
//      class is intended for parameterization of
//      <linkto class=HashMap>HashMap</linkto>.
//
//      The hash function maps the key type to any integer. This
//      integer is then used by <linkto class=HashMap>HashMap</linkto> to
//      select a bucket in the hash table.
// </synopsis>
//
// <example>
//      If one wished to make a HashClass for integers, something like the
//      following might be done:
//      <srcblock>
//	class IntHash : public HashClass<Int> {
//	public:
//	  uInt hash(const Int &v) const { return (uInt) v; }
//	  uInt hash(const Int &v) { return (uInt) v; }
//	  HashClass<Int> *clone() const { return new IntHash; }
//	  IntHash() : HashClass<Int>() { }
//	};
//	</srcblock>
// </example>
//
// <motivation>
//	There may be occasions when it is more convenient to use a class
//	instead of a single function. This base class provides a starting
//      point plus, and <src>HashMap<k,v></src> has the necessary hooks to
//      make use of classes derived from this class.
// </motivation>
//
template<class key> class HashClass {
public:
    //
    // This function maps elements of <src>key</src> type to any integer.
    // This integer is then used by <linkto class=HashMap>HashMap</linkto> to
    // select a bucket in the hash table.
    //
    virtual uInt hash(const key &) = 0;

    //
    // This function is used to make a <em>deep copy</em>. This means that
    // the copy, which this function returns, contains all derived information.
    //
    virtual HashClass<key> *clone() const = 0;

    HashClass();
    virtual ~HashClass();
};


// <summary>
//     Associative Array with a hash table implementation
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
//
// <prerequisite>
//   <li> basic concepts behind hash tables
// </prerequisite>
//
// <etymology>
//   This is an associative array, also known as a map, and it is implemented
//   using a hash table, so it is called HashMap.
// </etymology>
//
// <synopsis>
//   This class is an implementation of an associative array. This is a common
//   data structure which associates a key of one type with a value of the same
//   or different type. Essentially it is an (unordered) array which is indexed
//   by an arbitrary type of index, e.g. strings.
//
//   This class has two template type parameters. The first is the type of the
//   key and the second is the type of the value. Thus the associative array
//   is a mapping from the domain, any valid object of the key type, to the
//   range, any valid object of the value type. This is a <em>complete</em>
//   map which means that every element in the domain maps to one and only
//   one element in the range. Those elements which have not been set by the
//   user of this class map to a default value which is set at construction
//   time.
//
//   One of the important features of this class which must be understood
//   is the load factor. This factor indicates the average number of items
//   in the buckets of the hash table which are currently in use. The goal
//   is to have the hash function greatly reduce the number of item which
//   must be searched, i.e. to have a limited number of items in each bucket.
//   The load factor determines this. Thus a load factor of 1000 or 0 is a
//   poor choice. The default load factor is 4 which should generally be
//   fine. The load factor is set with <src>setMaxLoad()</src> and retrieved
//   with <src>maxLoad()</src>.
//
//   For this class to be used,
//   three things must be defined:
//   <ul>
//      <li> a specialization of the <src>hash()</src> templated function for
//              the key type or a class derived from <src>HashClass<key></src>.
//		Either of which can be used to implement the hash function for
//		a particular type.
//      <li> an equality operator ( '==' ) for the key
//      <li> a default constructor or a specialization of
//              <src>defaultHashValue()</src> for the key type
//   </ul>
//
//   The implementation of this hash map is derived from work on a proposed
//   addition to the Standard Template Library by Javier Barreiro, Robert Fraley
//   and <a href="http://www.cs.rpi.edu/~musser/">David R. Musser</a>. The information
//   which is available includes:
//   <ul>
//        <li> <a href="ftp://ftp.cs.rpi.edu/pub/stl/hashrationale.ps.Z">
//             rationale for hash map addition to the STL </a>
//        <li> <a href="ftp://ftp.cs.rpi.edu/pub/stl/hashdoc.ps.Z">
//             hash map addition proposal</a>
//        <li> <a href="ftp://ftp.cs.rpi.edu/pub/stl/hashimp2.Z">
//             preliminary implementation</a>
//   </ul>
//   each of these sources was utilized in the development of this set of classes,
//   and in particular, the preliminary implementation was the source for the hashing
//   algorithm used in this set of classes.
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
//      hash("one") = 1;		// sets the value of key "one" to "1"
//      hash.define("two",2);		// defines a mapping from key "two" to "2"
//      hash("three") = 3;
//      hash.define("four",4);
//      hash("five") = 5;
//      hash.define("six",6);
//    
//      HashMapIter<String,Int> iter(hash);
//      for ( iter.toStart(); ! iter.atEnd(); iter++ )
//          cout << iter.getVal() << ": " << iter.getKey() << endl;
//    
//      cout << endl << "Diagnostics" << endl << 
//    		  "========================" << endl;
//      cout << "number defined:    " << hash.ndefined() << endl;
//      cout << "buckets used:      " << hash.usedBuckets() << endl;
//      cout << "buckets available: " << hash.availableBuckets() << endl;
//      cout << "buckets allocated: " << hash.allocBuckets() << endl;
//      cout << "loading:           " << hash.loading() << endl;
//      cout << "size (in bytes):   " << hash.totalSize() << endl;
//    
//    }
//    </srcblock>
// </example>
//
// <motivation>
//    There are a couple of reasons why this class was built:
//         <ul>
//             <li> use of a hash table can be more efficient
//             <li> there are a couple of Map classes currently:
//               <ul>
//                 <li> <linkto class=OrderedMap>OrderedMap</linkto>
//                 <li> <linkto class=SimpleOrderedMap>SimpleOrderedMap</linkto>
//               </ul>
//               <src>OrderedMap</src> is derived from a map base class,
//               <linkto class=Map><src>Map</src></linkto> while
//               <src>SimpleOrderedMap</src> is not. This collection of classes
//               has resulted in confusion for the users. It is hoped that this
//               class can in time replace these other "map" classes by
//               satisfying the performance demands of
//               <src>SimpleOrderedMap</src> while still meeting the constraints
//               of the other map classes.
//         </ul>
// </motivation>
//
// <templating arg=key>
//    <li> equality operator (operator==)
//    <li> function hashFunc() or HashClass derived class provided
//    <li> default constructor or defaultHashValue() specialization provided or
//         default value provided at time of construction
// </templating>
// <templating arg=val>
//    <li> copy constructor
// </templating>
//
// <thrown>
//    <li> AipsError
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>
template<class key, class val> class HashMap {
friend class ConstHashMapIter<key,val>;
private:
    enum HashMap_Constants { defaultSize_ = 131, defaultMaxLoad_ = 4 };
public:
    static float defaultMaxLoad() { return float(defaultMaxLoad_); }
    static uInt  defaultSize() { return uInt(defaultSize_); }

    // Signature of the hash functions
    typedef uInt (*Func)(const key&);
    //
    // Copy constructor with copy semantics
    //
    HashMap(const HashMap &);

    //
    // Default constructor (and variation) which allows for
    // specifying:
    //   <ul>
    //     <li> a default value, <src>dflt</src>
    //     <li> the initial number of buckets, <src>size</src>
    //     <li> the hash function, <src>newfunc</src>
    //     <li> the maximum load factor, <src>maxlf</src>
    //   </ul>
    //
    // This is a pair because the hash function can either be
    // a simple function or a class derived from
    // <linkto class=HashClass><src>HashClass</src></linkto>.
    // <group>
    HashMap(const val &dflt = defaultHashValue((const val*)(0)),
	    uInt size = uInt(defaultSize_), 
	    Func newfunc = hashFunc,
	    float maxlf = float(defaultMaxLoad_))
      : total_(size),
	used_(size),
	filled_(0),
	defs_(0),
	maxLoad_(maxlf),
	blk(size, static_cast<List<OrderedPair<key,val> >*>(0)),
	func(newfunc),
	hashClass(0),
	dfltVal(dflt)
      { }

    HashMap(const val &dflt, uInt size, const HashClass<key> &newfunc,
	    float maxlf = float(defaultMaxLoad_))
      : total_(size),
	used_(size),
	filled_(0),
	defs_(0), 
	maxLoad_(maxlf),
	blk(size, static_cast<List<OrderedPair<key,val> >*>(0)),
	func(0),
	hashClass(newfunc.clone()),
	dfltVal(dflt)
      { }
    // </group>


    //
    // Constructor which allows for specifying:
    //   <ul>
    //     <li> default value, <src>dflt</src>
    //     <li> hash function, <src>newfunc</src>
    //     <li> maximum load factor, <src>maxlf</src>
    //   </ul>
    // This is provided because often the user will not be interested
    // in specifying the initial number of buckets since the number is
    // increased as needed to maintain the max load factor.
    //
    // This is a pair because the hash function can either be
    // a simple function or a class derived from
    // <linkto class=HashClass><src>HashClass</src></linkto>.
    // <group>
    HashMap(const val &dflt, Func newfunc, float maxlf = float(defaultMaxLoad_))
      : total_(uInt(defaultSize())), used_(uInt(defaultSize())),
	filled_(0), defs_(0), maxLoad_(maxlf),
	blk(uInt(defaultSize()), static_cast<List<OrderedPair<key,val> >*>(0)),
	func(newfunc), hashClass(0), dfltVal(dflt)
    { }

    HashMap(const val &dflt, const HashClass<key> &newfunc,
	    float maxlf = float(defaultMaxLoad_)) 
      : total_(defaultSize()), used_(defaultSize()),
	filled_(0), defs_(0), maxLoad_(maxlf), 
	blk(uInt(defaultSize_), static_cast<List<OrderedPair<key,val> >*>(0)), func(0),
	hashClass(newfunc.clone()), dfltVal(dflt)
    { }
    // </group>

    //
    // This copies the right hand side of the assignment. Assignment is done
    // with <em>copy semantics</em>. This means that all the state is copied.
    //
    HashMap<key,val> &operator=(const HashMap<key,val> &);

    //
    // Retrieve values from the map, possibly for later assignment.
    // It is important to realize that for the <em>non-const</em> version
    // accessing the key causes an entry to be created in the map if it
    // didn't already exist. The "value" for this new entry is the
    // default value. "isDefined()" should be used if this behavior is
    // not desired.
    // <group>
    const val &operator() (const key &ky) const;
    val &operator() (const key &ky);
    // </group>

    //
    // Define a complete mapping.
    //
    val &define(const key &k, const val &v);
    //
    // Remove a user defined mapping from <src>k</src> to a value.
    // After this, the value which <src>k</src> maps to reverts to
    // the default value.
    //
    void remove(const key &k);
    //
    // Check to see if a user defined mapping exists for
    // <src>k</src>. This does <em>not</em> modify the map.
    //
    Bool isDefined(const key &k) const;
    //
    // Retrieve the default value.
    // <group>
    const val &defaultVal() const { return dfltVal; }
    val &defaultVal() { return dfltVal; }
    // </group>

    //
    // Remove all user defined mapping.
    //
    void clear() { freeTable(); }
    //
    //  Get or set the maximum load factor.
    //
    // <group>
    float maxLoad() const { return maxLoad_; }
    void setMaxLoad( float new_max ) { maxLoad_ = new_max; }
    // </group>

    // Total number of buckets, i.e. the number the
    // hashing mechanism thinks it has. This is the total
    // number of buckets used for calculations in the hashing
    // mechanism. This may be smaller than <src>allocBuckets()</src>
    // because more underlying storage may be allocated than the
    // hashing mechanism needs.
    uInt totalBuckets() const { return total_; }
    // Number of buckets available, i.e. those which
    // the hashing mechanism allows itself to use. This
    // may be smaller than <src>totalBuckets()</src> because
    // the hashing mechanism can restrict itself to some subset
    // of the buckets available.
    uInt availableBuckets() const { return used_; }
    // Number of buckets currently populated by a bucket list.
    uInt usedBuckets() const { return filled_; }
    // Number of buckets which have been allocated, i.e. the
    // total number which have currently been allocated. This
    // is the number of buckets created. It may be bigger than
    // <src>totalBuckets()</src> because more memory can be
    // allocated than the hashing mechanism needs.
    uInt allocBuckets() const { return blk.nelements(); }

    // Number of mappings which have been defined by the user.
    uInt ndefined() const { return defs_; }

    // Current hash table loading factor.
    float loading() const { return ndefined() ? (float) ndefined() / (float) availableBuckets() : 0.0; }
    // Have any mappings been defined by the user.
    Bool empty() const { return ndefined() == 0 ? True : False; }
    // This returns a Block which has the number of elements in each bucket
    // of the table.
    Block<uInt> distribution() const;
    // Total size of this HashMap minus dynamically allocated memory for
    // key or val.
    uInt totalSize() const;

    //
    // dtor
    //
    virtual ~HashMap();

    enum {HashMapVersion = 1};

protected:
    // Call the hash function.
    uInt hash(const key &k) const {
	uInt off = func ? (*func)(k) % totalBuckets() :
		hashClass ? hashClass->hash(k) % totalBuckets() : 0;
	return off >= availableBuckets() ? off - (totalBuckets() >> 1) : off;
    }
    //
    // delete the contents of the hash table
    //
    void freeTable();
    //
    // enlarge the hash table. Returns the bucket which is being
    // moved...
    //
    uInt enlarge();
    //
    // populate bucket "to". Returns the bucket which is being
    // moved...
    //
    uInt populate( uInt to );

private:
    // Total Slots
    uInt total_;
    // Slots Being Used
    uInt used_;
    // Slots with At Least One Value in the Bucket
    uInt filled_;
    // Number of Defined Mappings
    uInt defs_;
    // Maximum load
    float maxLoad_;
    PtrBlock<List<OrderedPair<key,val> >*> blk;
    Func func;
    HashClass<key> *hashClass;
    val dfltVal;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/HashMap.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
