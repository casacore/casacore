//# Block.h: Simple templated array classes
//# Copyright (C) 1993,1994,1995,1996,1997,2000
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

#if !defined (AIPS_BLOCK_H)
#define AIPS_BLOCK_H

#include <aips/aips.h>
#include <aips/Utilities/Copy.h>
#include <aips/Containers/BlockIO.h>


//# For index checking
#if defined(AIPS_ARRAY_INDEX_CHECK)
#include <aips/Exceptions/Error.h>
#endif

// <summary>
// Collected error messages for Block<T>::*
// </summary>

// <use visibility=local>
// <prerequisite>
//   <li> <linkto class=Block>Block</linkto>
// </prerequisite>

// <synopsis>
// This class is purely an internal class used to throw errors by methods of the
// Block class. Unless you are maintaining Block, you don't need to know about
// this class.
// </synopsis>
class ThrowBlockError
{
public:
  enum  error {alloc1, alloc2, alloc3, alloc4, pastend1, alloc5};
  static void raise(int condition, error which, uInt n);
};

// <summary>simple 1-D array</summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
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
// <src>Block<T></src>'s may be assigned to and constructed from other <src>Block<T></src>'s.
// As no reference counting is done this can be an expensive operation, however.
//
// The net effect of this class is meant to be unsurprising to users who think
// of arrays as first class objects. The name "Block" is  intended to convey
// the concept of a solid "chunk" of things without any intervening "fancy"
// memory management, etc. This class was written to be
// used in the implementations of more functional Vector, Matrix, etc. classes,
// although it is expected <src>Block<T></src> will be useful on its own.
//
//
// The Block class should be efficient. You should normally use <src>Block</src>
// in preference to C-style arrays, particularly those obtained from "new".
// <src>Block<T></src> is not derived from <src>Cleanup</src>
// so if an exception is thrown the storage won't be reclaimed: <src>Block</src>'s should
// be used to build other classes which are themselves reclaimed.
//
// <note role=warning> If you use the assignment operator on an element of this class, you
// will may leave dangling references to pointers released from <src>storage()</src>.
// Resizing the array will also have this effect if the underlying storage
// is actually affected.
//
// Currently the <src>AllocError</src> is thrown are in response to new[] failing.
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
// for (uInt i=0; i < a.nelements(); i++) {
//     a[i] = i;    // Generate a sequence
//                  // with Vectors, could simply say "indgen(myVector);"
// }
// b.set(-1);       // All positions in b have the value -1
// b.resize(b.nelements()*2); // Make b twice as long, by default the old
//                            // elements are copied over, although this can
//                            // be defeated.
// some_c_function(b.storage());  // Use a fn that takes an Int * pointer
// </srcblock> 
// </example>
//
template<class T> class Block {
public:

    // Create a zero-length Block. Note that any index into this Block
    // is an error.
    Block() : npts(0), array(0), destroyPointer(True) {}
    // Create a Block with the given number of points. The values in Block
    // are uninitialized. Note that indices range between 0 and n-1.
    Block(uInt n) : npts(n), array(n>0 ? new T[n] : 0), destroyPointer(True)
      {	ThrowBlockError::raise(n>0 && !array, ThrowBlockError::alloc1, n);  }
    // Create a Block of the given length, and initialize (via operator= for 
    // objects of type T) with the provided value.
    Block(uInt n, T val) : npts(n), array(n > 0 ? new T[n] : 0), destroyPointer(True)
      {	ThrowBlockError::raise(n>0 && !array, ThrowBlockError::alloc2, n);
        objset(array, val, n); }

    // Create a <src>Block</src> from a C-array (i.e. pointer). If 
    // <src>takeOverStorage</src> is <src>True</src>, The Block assumes that
    // it owns the pointer, i.e. that it is safe to <src>delet[]</src> it when
    // the Block is destructed, otherwise the actual storage is not destroyed.
    // If true, <src>storagePointer</src> is set to <src>NULL</src>.
    Block(uInt n, T *&storagePointer, Bool takeOverStorage = True)
      : npts(n), array(storagePointer), destroyPointer(takeOverStorage)
      { if (destroyPointer) { storagePointer = 0; } }

    // Copy the other block into this one. Uses copy, not reference, semantics.
    Block(const Block<T> &other)
      : npts(other.npts), array(npts > 0 ? new T[npts] : 0), destroyPointer(True)
      {	ThrowBlockError::raise(npts>0 && !array, ThrowBlockError::alloc3, npts);
        objcopy(array, other.array, npts); }
  
    // Assign other to this. this resizes itself to the size of other, so after
    // the assignment, this->nelements() == other.elements() always.
    Block<T> &operator=(const Block<T> &other)
      {
	if (&other != this) {
	  this->resize(other.npts, True, False);
	  objcopy(array, other.array, npts);
	}
	return *this;
      }

    // Frees up the storage pointed contained in the Block.
    ~Block() { if (array && destroyPointer) { delete [] array; array = 0;} }

    // Resizes the Block. If <src>n == nelements()</src> resize just returns. If
    // A larger size is requested (<src>n > nelements()</src>) the Block always
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
    void resize(uInt n, Bool forceSmaller, Bool copyElements)
      {
	if (!(n == npts || (n < npts && forceSmaller == False))) {
	  T *tp = n > 0 ? new T[n] : 0;
	  ThrowBlockError::raise(tp == 0 && n > 0, ThrowBlockError::alloc4, n);
	  if (copyElements) {
	    uInt nmin = npts < n ? npts : n;  // Don't copy too much!
	    objcopy(tp, array, nmin);
	  }
	  if (array && destroyPointer) { // delete...
	    delete [] array;
	    array = 0;
	  }
	  npts = n;
	  destroyPointer = True;
	  array = tp;                       // ... and swap pointer
	}
      }
    void resize(uInt n) { resize(n, False, True); }
    void resize(uInt n, Bool forceSmaller)   { resize(n, forceSmaller, True); }
    // </group>

    // Remove a single element from the Block. If forceSmaller is True this
    // will resize the Block and hence involve new memory allocations. This is
    // relatively expensive so setting forceSmaller to False is preferred. When
    // forcesmaller is False the Block is not resized but the elements with an
    // index above the removed element are shuffled down by one. For backward
    // compatibility forceSmaller is True by default.
    // <group>
    void remove(uInt whichOne, Bool forceSmaller)
      {
	ThrowBlockError::raise(whichOne >= npts, ThrowBlockError::pastend1, 999);
	if (forceSmaller == True) {
	  T *tp = new T[npts - 1];
	  ThrowBlockError::raise(tp == 0, ThrowBlockError::alloc5, npts-1);
	  objcopy(tp, array, whichOne);
	  objcopy(tp+whichOne, array + whichOne + 1, npts - whichOne - 1);
	  if (array && destroyPointer) {
	    delete [] array;
	    array = 0;
	  }
	  npts--;
	  array = tp;
	  destroyPointer = True;
	}
	else
	  objmove(array+whichOne, array + whichOne + 1, npts - whichOne - 1);
      }
    void remove(uInt whichOne) { remove(whichOne, True); }
    // </group>

    // Replace the internal storage with a C-array (i.e. pointer).
    // If <src>takeOverStorage</src> is True, The Block assumes that it
    // owns the pointer, i.e. that it is safe to <src>delete[]</src> it when the 
    // <src>Block</src>is destructed, otherwise the actual storage is not destroyed.
    // If true, storagePointer is set to <src>NULL</src>.
    void replaceStorage(uInt n, T *&storagePointer, Bool takeOverStorage=True)
      {
	if (array && destroyPointer) {
	  delete [] array;
          array = 0;
	}
	npts = n;
	array = storagePointer;
	destroyPointer = takeOverStorage;
	if (destroyPointer) {
	  storagePointer = 0;
	}
      }

    // Index into the block (0-based). If the preprocessor symbol
    // <src>AIPS_ARRAY_INDEX_CHECK</src> is defined, index checking will be done
    //and an out-of-bounds index will cause an <src>indexError<uInt></src> to be
    // thrown. Note that valid indices range between 0 and <src>nelements()-1</src>.
    // <thrown>
    //    <li> indexError
    // </thrown>
    // <group>
    T &operator[](uInt index)
    {
#if defined(AIPS_ARRAY_INDEX_CHECK)
        // Write it this way to avoid casts; remember index and npts are
        // unsigned.
        if ((npts == 0) || (index > npts - 1)) {
	    throw(indexError<uInt>(index, "Block::operator[] - "
				   "index out of range"));
	}
#endif
	return array[index];
    }
    const T &operator[](uInt index) const
    {
#if defined(AIPS_ARRAY_INDEX_CHECK)
        if ((npts == 0) || (index > npts - 1)) {
	    throw(indexError<uInt>(index, "Block::operator[] const - "
				   "index out of range"));
	}
#endif
	return array[index];
    }
    // </group>

    // Set all values in the block to "val".
    // <group>
    Block<T> &operator=(const T &val)
      {T tmp=val; objset(array, tmp, npts); return *this;}
    void set(const T &val) { *this = val; }
    // </group>

    // If you really, really, need a "raw" pointer to the beginning of the
    // storage area this will give it to you. This may leave dangling pointers
    // if the block is destructed or if the assignment operator or resize 
    // is used. Returns a null pointer if <src>nelements() == 0</src>.
    // It is best to only use this if you completely control the extent and
    // lifetime of the <src>Block</src>.
    // <h3> Examples of misuse </h3> <srcblock>
    // Block<int> *bp = new Block<int>(100);
    // int *ip = bp->storage();
    // delete bp;      // Oops, ip is now dangling
    // Block<int> a(100),b(100);
    // int *ip = a.storage();
    // a = b;          // Likewise
    // </srcblock>
    // <group>
    T *storage() {return array;}
    const T *storage() const {return array;}
    // </group>

    // The number of elements contained in this <src>Block<T></src>.
    uInt nelements() const {return npts;}
private:
    // The number of points in the vector
    uInt  npts;
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
// <src>PtrBlock<T*></src> has the same interface as <src>Block<T*></src>
// and should be used in preference to the latter. Its main purpose is
// to reduce the number of template instantiations.
// <br>However, it adds 2 functions to make it possible to delete
// the objects the pointers are pointing to.
// </synopsis>

//# <todo asof="1996/05/01">
//#   <li>
//# </todo>

template<class T> class PtrBlock {
public:
    PtrBlock() : block_p() {}
    explicit PtrBlock(uInt n) : block_p(n) {}
    PtrBlock(uInt n, T val) : block_p(n, (void *)val) {}
    PtrBlock(uInt n, T *&storagePointer, Bool takeOverStorage = True)
      : block_p(n, (void **&)storagePointer, takeOverStorage) {}
    PtrBlock(const PtrBlock<T> &other) : block_p(other.block_p) {}
    PtrBlock<T> &operator=(const PtrBlock<T> &other)
      { block_p = other.block_p; return *this;}
   ~PtrBlock() {}
    // Delete all objects the pointers are pointing to and set pinter to 0.
    // <group>
    void deleteAll() {deleteAll (nelements());}
    void deleteAll(uInt n)
      { if (n>nelements()) n=nelements();
        for (uInt i=0; i<n; i++) {
          delete (*this)[i];
	  (*this)[i] = 0;
	}
      }
    // </group>
    void resize(uInt n, Bool forceSmaller, Bool copyElements)
      { block_p.resize(n,forceSmaller, copyElements); }
    void resize(uInt n) {block_p.resize(n);}
    void resize(uInt n, Bool forceSmaller) {block_p.resize(n, forceSmaller);}
    // Remove the entry after optionally first deleting the object
    // it is pointing to.
    void removeAndDelete(uInt whichOne, Bool forceSmaller)
      { delete (*this)[whichOne];
        block_p.remove(whichOne, forceSmaller);}
    void remove(uInt whichOne, Bool forceSmaller) {block_p.remove(whichOne, forceSmaller);}
    void remove(uInt whichOne) {block_p.remove(whichOne);}
    void replaceStorage(uInt n, T *&storagePointer, 
			       Bool takeOverStorage=True)
      {block_p.replaceStorage(n, (void **&)storagePointer, takeOverStorage);}
    T &operator[](uInt index) {return (T &)block_p[index];}
    const T &operator[](uInt index) const {return (const T &)block_p[index];}
    void set(const T &val) {block_p.set((void *const &)val);}
    PtrBlock<T> &operator=(const T &val) {set(val); return *this;}
    T *storage()  {return (T *)block_p.storage();}
    const T *storage() const {return (const T *)block_p.storage();}
    uInt nelements() const {return block_p.nelements();}
private:
    Block<void*> block_p;
};

#endif
