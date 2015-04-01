//# Block.h: Simple templated array classes
//# Copyright (C) 1993-1997,2000,2002,2005
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

#ifndef CASA_BLOCK_H
#define CASA_BLOCK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <cstddef>                  // for ptrdiff_t

//# For index checking
#if defined(AIPS_ARRAY_INDEX_CHECK)
#include <casacore/casa/Exceptions/Error.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>simple 1-D array</summary>
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <etymology>
//     This should be viewed as a <em>block</em> of memory without sophisticated
//     manipulation functions. Thus it is called <src>Block</src>.
// </etymology>
//
// <synopsis>
// <src>Block<T></src> is a simple templated 1-D array class. Indices are always
// 0-based. For efficiency reasons, no index checking is done unless the
// preprocessor symbol <src>AIPS_ARRAY_INDEX_CHECK</src> is defined. 
// <src>Block<T></src>'s may be assigned to and constructed from other
// <src>Block<T></src>'s.
// As no reference counting is done this can be an expensive operation, however.
//
// The net effect of this class is meant to be unsurprising to users who think
// of arrays as first class objects. The name "Block" is  intended to convey
// the concept of a solid "chunk" of things without any intervening "fancy"
// memory management, etc. This class was written to be
// used in the implementations of more functional Vector, Matrix, etc. classes,
// although it is expected <src>Block<T></src> will be useful on its own.
//
// The Block class should be efficient. You should normally use <src>Block</src>.
//
// <note role=warning> If you use the assignment operator on an element of this
// class, you may leave dangling references to pointers released from
// <src>storage()</src>.
// Resizing the array will also have this effect if the underlying storage
// is actually affected.
// </note>
//
// If index checking is turned on, an out-of-bounds index will
// generate an <src>indexError<uInt></src> exception.
// </synopsis>
//
// <example> 
// <srcblock>
// Block<Int> a(100,0);  // 100 ints initialized to 0
// Block<Int> b;         // 0-length Block
// // ...
// b = a;                // resize b and copy a into it
// for (size_t i=0; i < a.nelements(); i++) {
//     a[i] = i;    // Generate a sequence
//                  // with Vectors, could simply say "indgen(myVector);"
// }
// b.set(-1);       // All positions in b have the value -1
// b.resize(b.nelements()*2); // Make b twice as long, by default the old
//                            // elements are copied over, although this can
//                            // be defeated.
// some_c_function(b.storage());  // Use a fn that takes an
//                                // <src>Int *</src> pointer
// </srcblock> 
// </example>
//
class BlockTrace
{
public:
  // Set the trace size. The (de)allocation of Blocks with >= sz elements
  // will be traced using the MemoryTrace class.
  // A value 0 means no tracing.
  static void setTraceSize (size_t sz);
protected:
  // Write alloc and free trace messages.
  static void doTraceAlloc (const void* addr, size_t nelem,
                            DataType type, size_t sz);
  static void doTraceFree (const void* addr, size_t nelem,
                           DataType type, size_t sz);
protected:
  static size_t itsTraceSize;
};


// <summary>simple 1-D array</summary>
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <etymology>
//     This should be viewed as a <em>block</em> of memory without sophisticated
//     manipulation functions. Thus it is called <src>Block</src>.
// </etymology>
//
// <synopsis>
// <src>Block<T></src> is a simple templated 1-D array class. Indices are always
// 0-based. For efficiency reasons, no index checking is done unless the
// preprocessor symbol <src>AIPS_ARRAY_INDEX_CHECK</src> is defined. 
// <src>Block<T></src>'s may be assigned to and constructed from other
// <src>Block<T></src>'s.
// As no reference counting is done this can be an expensive operation, however.
//
// The net effect of this class is meant to be unsurprising to users who think
// of arrays as first class objects. The name "Block" is  intended to convey
// the concept of a solid "chunk" of things without any intervening "fancy"
// memory management, etc. This class was written to be
// used in the implementations of more functional Vector, Matrix, etc. classes,
// although it is expected <src>Block<T></src> will be useful on its own.
//
// The Block class should be efficient. You should normally use <src>Block</src>.
//
// <note role=warning> If you use the assignment operator on an element of this
// class, you may leave dangling references to pointers released from
// <src>storage()</src>.
// Resizing the array will also have this effect if the underlying storage
// is actually affected.
// </note>
//
// If index checking is turned on, an out-of-bounds index will
// generate an <src>indexError<uInt></src> exception.
// </synopsis>
//
// <example> 
// <srcblock>
// Block<Int> a(100,0);  // 100 ints initialized to 0
// Block<Int> b;         // 0-length Block
// // ...
// b = a;                // resize b and copy a into it
// for (size_t i=0; i < a.nelements(); i++) {
//     a[i] = i;    // Generate a sequence
//                  // with Vectors, could simply say "indgen(myVector);"
// }
// b.set(-1);       // All positions in b have the value -1
// b.resize(b.nelements()*2); // Make b twice as long, by default the old
//                            // elements are copied over, although this can
//                            // be defeated.
// some_c_function(b.storage());  // Use a fn that takes an
//                                // <src>Int *</src> pointer
// </srcblock> 
// </example>
//
template<class T> class Block: public BlockTrace
{
public:
  // Create a zero-length Block. Note that any index into this Block
  // is an error.
  Block() : npts(0), array(0), destroyPointer(True) {}
  // Create a Block with the given number of points. The values in Block
  // are uninitialized. Note that indices range between 0 and n-1.
  explicit Block(size_t n) : npts(n), array(n>0 ? new T[n] : 0), destroyPointer(True)
    { traceAlloc (array, npts); }
  // Create a Block of the given length, and initialize (via operator= for 
  // objects of type T) with the provided value.
  Block(size_t n, T val) : npts(n), array(n > 0 ? new T[n] : 0), destroyPointer(True)
    { traceAlloc (array, npts);
      objset(array, val, npts);
    }

  // Create a <src>Block</src> from a C-array (i.e. pointer). If 
  // <src>takeOverStorage</src> is <src>True</src>, The Block assumes that
  // it owns the pointer, i.e. that it is safe to <src>delet[]</src> it when
  // the Block is destructed, otherwise the actual storage is not destroyed.
  // If true, <src>storagePointer</src> is set to <src>0</src>.
  Block(size_t n, T *&storagePointer, Bool takeOverStorage = True)
    : npts(n), array(storagePointer), destroyPointer(takeOverStorage)
    { if (destroyPointer) storagePointer = 0;}

  // Copy the other block into this one. Uses copy, not reference, semantics.
  Block(const Block<T> &other)
    : npts(other.npts), array(npts > 0 ? new T[npts] : 0), destroyPointer(True)
    { traceAlloc (array, npts);
      objcopy(array, other.array, npts);
    }
  
  // Assign other to this. this resizes itself to the size of other, so after
  // the assignment, this->nelements() == other.elements() always.
  Block<T> &operator=(const Block<T> &other) {
    if (&other != this) {
      this->resize(other.npts, True, False);
      objcopy(array, other.array, npts);
    }
    return *this; }
  
  // Frees up the storage pointed contained in the Block.
  ~Block()
    { if (array && destroyPointer) {
        traceFree (array, npts);
        delete [] array; array = 0;
      }
    } 

  // Resizes the Block. If <src>n == nelements()</src> resize just returns. If
  // a larger size is requested (<src>n > nelements()</src>) the Block always
  // resizes. If the requested size is smaller (<src>n < nelements()</src>),
  // by default the Block does not resize smaller, although it can be
  // forced to with <src>forceSmaller</src>. The reasoning behind this is that
  // often the user will just want a buffer of at least a certain size,
  // and won't want to pay the cost of multiple resizings.
  // <srcblock>
  // Block<float> bf(100, 0.0);
  // bf.resize(10);        // bf.nelements() == 100
  // bf.resize(10, True)   // bf.nelements() == 10
  // bf.resize(200)        // bf.nelements() == 200
  // </srcblock>
  // Normally the old elements are copied over (although if the
  // Block is lengthened the trailing elements will have undefined
  // values), however this can be turned off by setting copyElements to
  // False.
  //
  // This is written as three functions because default parameters do
  // not always work properly with templates.
  // <group>
  void resize(size_t n, Bool forceSmaller=False, Bool copyElements=True) {
    if (!(n == npts || (n < npts && forceSmaller == False))) {
      T *tp = n > 0 ? new T[n] : 0;
      traceAlloc (tp, n);
      if (copyElements) {
	size_t nmin = npts < n ? npts : n;  // Don't copy too much!
	objcopy(tp, array, nmin);
      };
      if (array && destroyPointer) { // delete...
        traceFree (array, npts);
	delete [] array;
	array = 0;
      };
      npts = n;
      destroyPointer = True;
      array = tp;                       // ... and swap pointer
    };
  }
  // </group>

  // Remove a single element from the Block. If forceSmaller is True this
  // will resize the Block and hence involve new memory allocations. This is
  // relatively expensive so setting forceSmaller to False is preferred. When
  // forcesmaller is False the Block is not resized but the elements with an
  // index above the removed element are shuffled down by one. For backward
  // compatibility forceSmaller is True by default.
  // <group>
  void remove(size_t whichOne, Bool forceSmaller=True) {
    if (whichOne >= npts) {
#if defined(AIPS_ARRAY_INDEX_CHECK)
      throw(indexError<uInt>(whichOne, "Block::remove() - "
			     "index out of range"));
#else
      return;
#endif
    }
    if (forceSmaller == True) {
      T *tp = new T[npts - 1];
      traceAlloc (array, npts-1);
      objcopy(tp, array, whichOne);
      objcopy(tp+whichOne, array + whichOne + 1, npts - whichOne - 1);
      if (array && destroyPointer) {
        traceFree (array, npts);
	delete [] array;
	array = 0;
      };
      npts--;
      array = tp;
      destroyPointer = True;
    } else objmove(array+whichOne, array + whichOne + 1, npts - whichOne - 1);
  }
  // </group>

  // Replace the internal storage with a C-array (i.e. pointer).
  // If <src>takeOverStorage</src> is True, The Block assumes that it
  // owns the pointer, i.e. that it is safe to <src>delete[]</src> it when the 
  // <src>Block</src>is destructed, otherwise the actual storage is not destroyed.
  // If true, storagePointer is set to <src>NULL</src>.
  void replaceStorage(size_t n, T *&storagePointer, Bool takeOverStorage=True) {
    if (array && destroyPointer) {
      traceFree (array, npts);
      delete [] array;
      array = 0;
    };
    npts = n;
    array = storagePointer;
    destroyPointer = takeOverStorage;
    if (destroyPointer) storagePointer = 0;
  };

  // Index into the block (0-based). If the preprocessor symbol
  // <src>AIPS_ARRAY_INDEX_CHECK</src> is defined, index checking will be done
  // and an out-of-bounds index will cause an <src>indexError<uInt></src> to be
  // thrown. Note that valid indices range between 0 and <src>nelements()-1</src>.
  // <thrown>
  //    <li> indexError
  // </thrown>
  // <group>
  T &operator[](size_t index) {
#if defined(AIPS_ARRAY_INDEX_CHECK)
    // Write it this way to avoid casts; remember index and npts are
    // unsigned.
    if ((npts == 0) || (index > npts - 1)) {
      throw(indexError<uInt>(index, "Block::operator[] - "
			     "index out of range"));
    };
#endif
    return array[index];
  }
  const T &operator[](size_t index) const {
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if ((npts == 0) || (index > npts - 1)) {
      throw(indexError<uInt>(index, "Block::operator[] const - "
			     "index out of range"));
    };
#endif
    return array[index];
  }
  // </group>
  
  // Set all values in the block to "val".
  // <group>
  Block<T> &operator=(const T &val)
    { T tmp=val; objset(array, tmp, npts); return *this;}
  void set(const T &val) { *this = val; }
  // </group>

  // If you really, really, need a "raw" pointer to the beginning of the
  // storage area this will give it to you. This may leave dangling pointers
  // if the block is destructed or if the assignment operator or resize 
  // is used. Returns a null pointer if <src>nelements() == 0</src>.
  // It is best to only use this if you completely control the extent and
  // lifetime of the <src>Block</src>.
  // <h3> Examples of misuse </h3> <srcblock>
  // Block<Int> *bp = new Block<Int>(100);
  // Int *ip = bp->storage();
  // delete bp;      // Oops, ip is now dangling
  // Block<Int> a(100),b(100);
  // Int *ip = a.storage();
  // a = b;          // Likewise
  // </srcblock>
  // <group>
  T *storage() {return array;}
  const T *storage() const {return array;}
  // </group>

  // The number of elements contained in this <src>Block<T></src>.
  // <group>
  size_t nelements() const {return npts;}
  size_t size() const {return npts;}
  // </group>

  // Is the block empty (i.e. no elements)?
  Bool empty() const {return npts == 0;}

  // Define the STL-style iterators.
  // It makes it possible to iterate through all data elements.
  // <srcblock>
  //  Block<Int> bl(100,0);
  //  for (Block<Int>::iterator iter=bl.begin(); iter!=bl.end(); iter++) {
  //    *iter += 1;
  //  }
  // </srcblock>
  // <group name=STL-iterator>
  // STL-style typedefs.
  // <group>
  typedef T                 value_type;
  typedef T*                iterator;
  typedef const T*          const_iterator;
  typedef value_type*       pointer;
  typedef const value_type* const_pointer; 
  typedef value_type&       reference;
  typedef const value_type& const_reference;
  typedef size_t            size_type;
  typedef ptrdiff_t         difference_type;
  // </group>
  // Get the begin and end iterator object for this block.
  // <group>
  iterator begin()
    { return array; }
  const_iterator begin() const
    { return array; }
  iterator end()
    { return array + npts; }
  const_iterator end() const
    { return array + npts; }
  // </group>
  // </group>

  inline void traceAlloc (const void* addr, size_t sz) const
  {
    if (itsTraceSize>0 && sz>=itsTraceSize) {
      doTraceAlloc (addr, sz, whatType(static_cast<T*>(0)), sizeof(T));
    }
  }
  inline void traceFree (const void* addr, size_t sz) const
  {
    if (itsTraceSize>0 && sz>=itsTraceSize) {
      doTraceFree (addr, sz, whatType(static_cast<T*>(0)), sizeof(T));
    }
  }

 private:
  // The number of points in the vector
  size_t npts;
  // The actual storage
  T *array;
  // Can we delete the storage upon destruction?
  Bool destroyPointer;
};

// <summary>
// A drop-in replacement for <src>Block<T*></src>.
// </summary>
 
// <use visibility=export>
// <prerequisite>
//   <li> <linkto class=Block>Block</linkto>
// </prerequisite>
 
// <synopsis>
// <src>PtrBlock<T*></src> has exactly the same interface as <src>Block<T*></src>
// and should be used in preference to the latter. It's purpose is solely to
// reduce the number of template instantiations.
// </synopsis>
 
// <todo asof="1996/05/01">
//   <li> Partial template specialization is another implementation choice that 
//        will be possible eventually.
//   <li> It might be useful to have functions that know the templte parameter
//        is a pointer, e.g. that delete all the non-null pointers.
// </todo>
 
 template<class T> class PtrBlock {
 public:
   PtrBlock() : block_p() {}
   explicit PtrBlock(size_t n) : block_p(n) {}
   PtrBlock(size_t n, T val) : block_p(n, (void *)val) {}
   PtrBlock(size_t n, T *&storagePointer, Bool takeOverStorage = True)
     : block_p(n, (void **&)storagePointer, takeOverStorage) {}
   PtrBlock(const PtrBlock<T> &other) : block_p(other.block_p) {}
   PtrBlock<T> &operator=(const PtrBlock<T> &other)
     { block_p = other.block_p; return *this;}
   ~PtrBlock() {}
   void resize(size_t n, Bool forceSmaller, Bool copyElements)
     { block_p.resize(n,forceSmaller, copyElements); }
   void resize(size_t n) {block_p.resize(n);}
   void resize(size_t n, Bool forceSmaller) {block_p.resize(n, forceSmaller);}
   void remove(size_t whichOne, Bool forceSmaller) {
     block_p.remove(whichOne, forceSmaller);}
   void remove(size_t whichOne) {block_p.remove(whichOne);}
   void replaceStorage(size_t n, T *&storagePointer, 
		       Bool takeOverStorage=True)
     {block_p.replaceStorage(n, (void **&)storagePointer, takeOverStorage);}
   T &operator[](size_t index) {return (T &)block_p[index];}
   const T &operator[](size_t index) const {return (const T &)block_p[index];}
   void set(const T &val) {block_p.set((void *const &)val);}
   PtrBlock<T> &operator=(const T &val) {set(val); return *this;}
   T *storage()  {return (T *)block_p.storage();}
   const T *storage() const {return (const T *)block_p.storage();}
   size_t nelements() const {return block_p.nelements();}
   size_t size() const {return block_p.size();}
   Bool empty() const {return block_p.empty();}
 private:
   Block<void*> block_p;
 };


} //# NAMESPACE CASACORE - END

#endif
