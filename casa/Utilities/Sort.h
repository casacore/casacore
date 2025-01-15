//# Sort.h: Sort objects on one or more keys
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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

#ifndef CASA_SORT_H
#define CASA_SORT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Compare.h>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Define a Sort key </summary>
// <use visibility=local>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="tSort, tSort_1">
// </reviewed>

// <synopsis>
// SortKey is a helper class for the <linkto class=Sort>Sort</linkto> class.
// It holds the following information about a sort key:
// <ul>
//  <li> Address of the data array containing the sort key;
//  <li> A std::shared_ptr to a comparison object to be used
//       (of a class derived from the abstract base class BaseCompare).
//  <li> Increment for the next data point -- this lets you specify a
//       stride for keys embedded in a struct;
//  <li> Sort order -- ascending or descending;
// </ul>
// </synopsis> 

class SortKey
{
public:
    friend class Sort;

    // Define a sort key in a given data array using the indicated
    // comparison object, stride and sort order.
    SortKey (const void* data, const std::shared_ptr<BaseCompare>&,
             uInt increment, int order);

    // Copy constructor (copy semantics).
    SortKey (const SortKey&);

    ~SortKey();

    // Assignment (copy semantics).
    SortKey& operator= (const SortKey&);

    // Try if GenSort can be used for this single key.
    // If it succeeds, it returns the resulting number of elements.
    // Otherwise it returns 0.
    uInt tryGenSort (Vector<uInt>& indexVector, uInt nrrec, int opt) const;
    uInt64 tryGenSort (Vector<uInt64>& indexVector, uInt64 nrrec, int opt) const;

    // Get the sort order.
    int order() const
      { return order_p; }

protected:
    // sort order; -1 = ascending, 1 = descending
    int               order_p;
    // address of first data point
    const void*       data_p;
    // increment for next data point
    uInt              incr_p;
    // comparison object; use std::shared_ptr for memory management
    std::shared_ptr<BaseCompare> ccmpObj_p;
    // comparison object; use raw pointer for performance
    BaseCompare* cmpObj_p;
};



// <summary> Sort on one or more keys, ascending and/or descending </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="tSort, tSort_1">
// </reviewed>

// <synopsis>
// <src>Sort</src> lets you sort data on one or more keys in a mix of
// <src>Sort::ascending</src> and <src>Sort::descending</src> order.
// Duplicates can be skipped by giving the option
// <src>Sort::NoDuplicates</src>. Only in this case the number of output
// elements can be different from the number of input elements.
// <br>The <src>unique</src> function offers another way of getting
// unique values.
// <p>
// Class <src>Sort</src> does not sort the data themselves, but
// returns an index to them. This gives more flexibility and
// allows the sort to be stable; but it is slower.
// <br>Very fast sorting of the data themselves can be done with the
// functions in class <linkto class=GenSort>GenSort</linkto>.
// If sorting on a single key with a standard data type is done,
// Sort will use GenSortIndirect to speed up the sort.
// <br>
// Four sort algorithms are provided:
// <DL>
//  <DT> <src>Sort::ParSort</src>
//  <DD> The parallel merge sort is the fastest if it can use multiple threads.
//       For a single thread it has O(n*log(n)) behaviour, but is slower
//       than quicksort.
//       A drawback is that it needs an extra index array to do the merge.
//  <DT> <src>Sort::InsSort</src>
//  <DD> Insertion sort has O(n*n) behaviour, thus is very slow for large
//       arrays. However, it is the fastest method for small arrays
//       (< 50 elements) and for arrays already (almost) in the right order.
//  <DT> <src>Sort::QuickSort</src>
//  <DD> Care has been taken to solve the well-known quicksort problems
//       like "array already in order" or "many equal elements".  The
//       behaviour is O(n*log(n)) in all the cases tested, even in
//       degenerated cases where the SUN Solaris qsort algorithm is O(n*n).
//  <DT> <src>Sort::HeapSort</src>
//  <DD> Heapsort has O(n*log(n)) behaviour. Its speed is lower than
//       that of QuickSort, so QuickSort is the default algorithm.
// </DL>
// The default is to use QuickSort for small arrays or if only a single
// thread can be used. Otherwise ParSort is the default.
// 
// All sort algorithms are <em>stable</em>, which means that the original
// order is kept when keys are equal.
//
// The sort is a four step process:
// <ol>
//  <li> Construct the <src>Sort</src> object.
//  <li> Define the sort keys. The function <src>sortKey</src> must be
//       called for each sort key (the most significant one first).
//       The comparison object can be passed in directly, or a 
//       <linkto group="DataType.h#DataType">basic data type</linkto>
//       can be given. In the latter case the appropriate ObjCompare
//       comparison object will be created.
//  <li> Sort the data. The function <src>sort</src> returns an index
//       array, which is allocated when needed.
//  <li> Destruct the <src>Sort</src> object (usually done automatically)
//       and delete the index array.
// </ol>
// The data can be in a single array of structs, in separate arrays, or
// in a mix of those. Of course, all arrays must have the same length.
// The data can be passed to the <src>Sort</src> constructor and/or to the
// <src>sortKey</src> function. If passed to the <src>Sort</src> constructor,
// the offset of the data item in the data array must be given to
// <src>sortKey</src>.
// </synopsis>

// <example>
// In the first example we sort the data contained in two "parallel"
// arrays, <src>idata</src> and <src>ddata</src>, both of length
// <src>nrdata</src>.
// <srcblock>
//    Sort sort;
//    sort.sortKey (idata, TpInt);                       // define 1st sort key
//    sort.sortKey (ddata, TpDouble,0,Sort::Descending); // define 2nd sort key
//    Vector<uInt> inx;
//    sort.sort (inx, nrdata);
//    for (uInt i=0; i<nrdata; i++) {                    // show sorted data
//        cout << idata[inx[i]] << " " << ddata[inx[i]] << endl;
//    }
// </srcblock>
// Now <src>nr</src> contains the nr of records (=<src>nrdata</src>)
// and <src>inx</src> an array of (sorted) indices.
//
// In the second example we sort the data stored in an array of structs
// on the double (ascending) and the string (descending). We can pass
// the data to the <src>Sort</src> constructor, and the offsets of the
// struct elements to the <src>sortKey</src> function.
// <srcblock>
//    struct Ts {
//         String as;
//         double ad;
//    }
//    Vector<uInt> inx;
//    Sort sort (tsarr, sizeof(Ts));
//    sort.sortKey ((char*)&tsarr[0].ad - (char*)tsarr, TpDouble);
//    sort.sortKey ((char*)&tsarr[0].as - (char*)tsarr, TpString,
//                                                       Sort::Descending);
//    sort.sort (inx, nrts);
// </srcblock>
// Note that the first argument in function <src>sortKey</src> gives
// the offset of the variable in the struct.
//
// Alternatively, and probably slightly easier, we could pass the data
// to the <src>sortKey</src> function and use an increment:
// <srcblock>
//    struct Ts {
//         String as;
//         double ad;
//    }
//    Vector<uInt> inx;
//    Sort sort;
//    sort.sortKey (&tsarr[0].ad, TpDouble, sizeof(Ts));
//    sort.sortKey (&tsarr[0].as, TpString, sizeof(Ts), Sort::Descending);
//    sort.sort (inx, nrts);
// </srcblock>
//
// Finally, we could provide a comparison object for the struct.
// <srcblock>
//    struct Ts {
//         String as;
//         double ad;
//    }
//    class MyCompare: public BaseCompare {
//      virtual int comp (const void* val1, const void* val2) const
//      {
//        const Ts& t1 = *(Ts*)val1;
//        const Ts& t2 = *(Ts*)val2;
//        if (t1.ad < t2.ad) return -1;
//        if (t1.ad > t2.ad) return 1;
//        if (t1.as < t2.as) return 1;    // string must be descending
//        if (t1.as > t2.as) return -1;
//        return 0;
//      }
//    };
//    Vector<uInt> inx;
//    Sort sort;
//    sort.sortKey (tsarr, compareTs, sizeof(Ts)); 
//    sort.sort (inx, nrts);
// </srcblock>

class Sort
{
public:
    // Enumerate the sort options:
    enum Option {DefaultSort=0,     // ParSort, but QuickSort for small array
                 HeapSort=1,        // use Heapsort algorithm
                 InsSort=2,         // use insertion sort algorithm
                 QuickSort=4,       // use Quicksort algorithm
                 ParSort=8,         // use parallel merge sort algorithm
                 NoDuplicates=16};  // skip data with equal sort keys

    // Enumerate the sort order:
    enum Order {Ascending=-1,
                Descending=1};

    // The default constructor can be used when the data is only passed
    // in via function <src>sortKey</src>.
    Sort();

    // Construct a Sort object for the given data array with elements
    // of <src>elementSize</src> bytes.  This data array will be used
    // when an offset is given to the <src>sortKey</src> functions.
    // You can still pass additional data arrays to the
    // <src>sortKey</src> functions.
    Sort (const void* data, uInt elementSize);

    // Copy constructor (copy semantics).
    Sort (const Sort&);

    ~Sort();

    // Assignment (copy semantics).
    Sort& operator= (const Sort&);

    // Define a sort key (the most significant key should be defined first).
    // The key contains:
    // <ul>
    // <li> A pointer to the start of the data array. --- When structs are
    //   sorted on an element in the struct, the pointer must point to
    //   that element in the first struct.
    // <li> A pointer to the comparison object to be used. --- The
    //   comparison object can be specified in two ways:
    //   <ul>
    //   <li> by giving a
    //     <linkto group="DataType.h#DataType">basic data type</linkto>,
    //     in which case the appropriate comparison object will be
    //     created automatically, or
    //   <li> by a std::shared_ptr of a comparison object.
    //     You may want to use the templated comparison classes
    //     <linkto class=ObjCompare>ObjCompare</linkto>(),
    //     but you are free to use any other class derived from BaseCompare
    //     that implements the <src>comp</src> function.
    //   </ul>
    // <li> The increment from one data element to the next. --- When
    //   structs are sorted on an element in the struct, the increment
    //   should be the size of the struct. If the comparison object is
    //   automatically created using the data type specified, the default
    //   increment is the size of the data type.
    // <li> The sort order. --- <src>Ascending</src> (default) or 
    //   <src>Descending</src>;
    // </ul>
    //
    // When the data array has been passed to the Sort constructor,
    // the data pointer and the increment arguments can be replaced by a
    // single argument: the offset of the key in each element of the array.
    //
    // <group>
    void sortKey (const void* data, DataType, uInt increment = 0,
		  Order = Ascending);
    void sortKey (const void* data, const std::shared_ptr<BaseCompare>&,
                  uInt increment, Order = Ascending);
    void sortKey (uInt offset, DataType, Order = Ascending);
    void sortKey (uInt offset, const std::shared_ptr<BaseCompare>&,
                  Order = Ascending);
    // </group>

    // Sort the data array of <src>nrrec</src> records.
    // The result is an array of indices giving the requested order.
    // It returns the number of resulting records. The indices array
    // is resized to that number.
    // <br> By default it'll try if the faster GenSortIndirect can be used
    // if a sort on a single key is used.
    uInt sort (Vector<uInt>& indexVector, uInt nrrec,
               int options = DefaultSort, Bool tryGenSort = True) const;
    uInt64 sort (Vector<uInt64>& indexVector, uInt64 nrrec,
                 int options = DefaultSort, Bool tryGenSort = True) const;

    // Get all unique records in a sorted array. The array order is
    // given in the indexVector (as possibly returned by the sort function).
    // The default indexVector is 0..nrrec-1.
    // The index of each first unique record is returned in the uniqueVector.
    // They are indices in the supplied indexVector, so
    // <src>data[indexVector(uniqueVector(i))]</src>
    // is giving the i-th unique record.
    // Note that the records indexed by <src>indexVector(uniqueVector(i))</src>
    // till <src>indexVector(uniqueVector(i+1))</src> are all the same.
    // <br>
    // It returns the number of unique records. The unique array
    // is resized to that number.
    // The third version also gives back a vector with the keys that
    // change in each sorting group. The size of changeKey is the same as
    // uniqueVector, and for each unique sorting group indicates the index
    // of the keyword that will change at the end of the group.
    // <group>
    uInt unique (Vector<uInt>& uniqueVector, uInt nrrec) const;
    uInt unique (Vector<uInt>& uniqueVector,
                 const Vector<uInt>& indexVector) const;
    uInt unique (Vector<uInt>& uniqueVector,
                 Vector<size_t>& changeKey,
                 const Vector<uInt>& indexVector) const;
    uInt64 unique (Vector<uInt64>& uniqueVector, uInt64 nrrec) const;
    uInt64 unique (Vector<uInt64>& uniqueVector,
                   const Vector<uInt64>& indexVector) const;
    uInt64 unique (Vector<uInt64>& uniqueVector,
                   Vector<size_t>& changeKey,
                   const Vector<uInt64>& indexVector) const;
    // </group>

private:
    template<typename T>
    T doSort (Vector<T>& indexVector, T nrrec,
              int options = DefaultSort, Bool tryGenSort = True) const;

    template <typename T>
    T doUnique (Vector<T>& uniqueVector, T nrrec) const;
    template <typename T>
    T doUnique (Vector<T>& uniqueVector, const Vector<T>& indexVector) const;
    template <typename T>
    T doUnique (Vector<T>& uniqueVector, Vector<size_t>& changeKey,
                const Vector<T>& indexVector) const;

    // Copy that Sort object to this.
    void copy (const Sort& that);

    // Add a sort key giving a data type and stride or the sort key.
    // <group>
    void addKey (const void* data, DataType, uInt increment, int options);
    void addKey (SortKey*);
    // </group>

    // Do an insertion sort, optionally skipping duplicates.
    // <group>
    template<typename T>
    T insSort (T nr, T* indices) const;
    template<typename T>
    T insSortNoDup (T nr, T* indices) const;
    // </group>

    // Do a merge sort, if possible in parallel using OpenMP.
    // Note that the env.var. OMP_NUM_TRHEADS sets the maximum nr of threads
    // to use. It defaults to the number of cores.
    template<typename T>
    T parSort (int nthr, T nrrec, T* inx) const;
    template<typename T>
    void merge (T* inx, T* tmp, T size, T* index,
                T nparts) const;

    // Do a quicksort, optionally skipping duplicates
    // (qkSort is the actual quicksort function).
    // <group>
    template<typename T>
    T quickSort (T nr, T* indices) const;
    template<typename T>
    T quickSortNoDup (T nr, T* indices) const;
    template<typename T>
    void qkSort (T nr, T* indices) const;
    // </group>

    // Do a heapsort, optionally skipping duplicates.
    // <group>
    template<typename T>
    T heapSort (T nr, T* indices) const;
    template<typename T>
    T heapSortNoDup (T nr, T* indices) const;
    // </group>

    // Siftdown algorithm for heapsort.
    template<typename T>
    void siftDown (T low, T up, T* indices) const;

    // Compare 2 records based on the comparison functions
    template<typename T>
    int compare (T index1, T index2) const;

    // As compare() but it also gives back the index of the first comparison
    // function that didn't match.
    template<typename T>
    int compareChangeIdx(T i1, T i2, size_t& idxComp) const;

    // Swap 2 indices.
    template<typename T>
    inline void swap (T index1, T index2, T* indices) const
    {
      T t = indices[index1];
      indices[index1] = indices[index2];
      indices[index2] = t;
    }

    //# Data memebers
    PtrBlock<SortKey*> keys_p;                    //# keys to sort on
    size_t             nrkey_p;                   //# #sort-keys
    const void*        data_p;                    //# pointer to data records
    uInt               size_p;                    //# size of data record
    int                order_p;                   //# -1=asc 0=mixed 1=desc
};


} //# NAMESPACE CASACORE - END

#endif
