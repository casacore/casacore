//# Vector.h: A 1-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2000,2001,2002,2003
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

#ifndef CASA_VECTOR_2_H
#define CASA_VECTOR_2_H

//# Includes
#include "Array.h"

namespace casacore { //#Begin namespace casacore

// <summary> A 1-D Specialization of the Array class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// Vector objects are one-dimensional specializations (e.g., more convenient
// and efficient indexing) of the general Array class. You might also want
// to look at the Array documentation to see inherited functionality.
//
// Generally the member functions of Array are also available in
// Vector versions
// which take an integer where the array needs an IPosition. Since the Vector
// is one-dimensional, the IPositions are overkill, although you may
// use those versions if you want to.
// <srcblock>
// Vector<int> vi(100);  // Vector 100 elements long.
// vi.resize(50);        // Now only 50 long.
// </srcblock>
//
// Slices may be taken with the Slice class. To take a slice, one "indexes" 
// with Slice(start, length, inc) where end and inc are optional.
// <srcblock>
// Vector<float> vf(100);
// //...
// vf(Slice(0,50,2)) = vf(Slice(1,50,2));  // Copy values from odd onto even
// Vector<float> firstHalf, secondHalf;
// firstHalf.reference(vf(Slice(0,50)));
// secondHalf.reference(vf(Slice(50,50)));
// // Now we have aliases for two slices into the Vector
// </srcblock>
//
// Element-by-element arithmetic and logical operations are available (in
// aips/ArrayMath.h and aips/ArrayLogical.h) as well as dot and cross
// products (in aips/MatrixMath.h).
//
// A Vector can be constructed from an STL <src>vector</src>. The reverse
// operation (<src>Array::tovector()</src>) can construct an STL
// <src>vector</src> from any Array.
// <note role=tip> To create any other STL container from an Array (or
// the reverse), always create from/to a <src>vector</src>, and use the
// range constructor to create from/to others (like set, list, deque). </note>
// 
// As with the Arrays, if the preprocessor symbol AIPS_DEBUG is
// defined at compile time invariants will be checked on entry to most
// member functions. Additionally, if AIPS_ARRAY_INDEX_CHECK is defined
// index operations will be bounds-checked. Neither of these should
// be defined for production code.

template<typename T, typename Alloc> class Vector : public Array<T, Alloc>
{
public:
    // A zero-length Vector.
    Vector();

    // A Vector with a defined length and origin of zero.
    // <group>
    explicit Vector(size_t Length);
    explicit Vector(const IPosition& Length);
    // </group>

    // A Vector with a defined length and origin of zero.
    // Fill it with the initial value.
    // <group>
    Vector(size_t Length, const T &initialValue);
    Vector(const IPosition& Length, const T &initialValue);
    // </group>

    // An uninitialized Vector with a defined length.
    // <group>
    Vector(size_t Length, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator=Alloc());
    Vector(const IPosition& Length, typename Array<T, Alloc>::uninitializedType, const Alloc& allocator=Alloc());
    // </group>
    
    // Create a Vector from the given std::vector "other." Make it length "nr"
    // and copy over that many elements.
    // This used to take a 'Block'
    Vector(const std::vector<T> &other, long long nr);
    
    // Create a Vector of length other.size() and copy over its values.
    // This used to take a 'Block'
    explicit Vector(const std::vector<T> &other, const Alloc& allocator=Alloc());

    // Create a Vector from an initializer list.
    Vector(std::initializer_list<T> list);

    // Create a reference to other.
    Vector(const Vector<T, Alloc> &other);
    
    // Move
    Vector(Vector<T, Alloc>&& source) noexcept;
    
    // Create a reference to the other array.
    // It is always possible if the array has zero or one axes.
    // If it has > 1 axes, it is only possible if the array has at most
    // one axis with length > 1. In that case the degenerated axes are removed.
    Vector(const Array<T, Alloc> &other);

    // Create an Vector of a given shape from a pointer.
    Vector(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY);
    // Create an Vector of a given shape from a pointer.
    Vector(const IPosition &shape, T *storage, StorageInitPolicy policy, Alloc& allocator);
    // Create an Vector of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Vector(const IPosition &shape, const T *storage);

    template<typename InputIterator>
    Vector(InputIterator startIter, InputIterator endIter, const Alloc& allocator = Alloc());

    // Create a Vector from an STL vector (see <src>tovector()</src> in
    // <linkto class=Array>Array</linkto>  for the reverse operation).
    // <note role=tip> Both this constructor and the tovector() are
    // defined in <src>Vector2.cc</src>. </note>
    // It does implicit promotion/demotion of the type U if different from T.
    template <typename U, typename V>
    explicit Vector(const std::vector<U, V> &other);

    // Create a Vector from a container iterator and its length.
    // <note> The length is used instead of last, because the distance
    // function needed to calculate the length can be expensive.
    // <br>A third dummy argument is unfortunately needed to avoid ambiguity
    // with another Vector constructor (taking two uInts).
    // </note>
    template<typename Iterator>
    Vector(Iterator first, size_t size, int dummy);

    // Resize this Vector to the given length.
    // The default copyValues flag is false.
    //# Note that the 3rd resize function is necessary, because that
    //# function is virtual in the base class (otherwise it would
    //# be hidden).
    // Resize without argument is equal to resize(0, false).
    // <group>
    using Array<T, Alloc>::resize;
    void resize(size_t len, bool copyValues=false)
      { if (len != this->nelements()) resize(IPosition(1,len), copyValues); }
    virtual void resize(const IPosition &len, bool copyValues=false) final override;
    // </group>
    
    // Assign to this Vector. If this Vector is zero-length, then resize
    // to be the same size as other. Otherwise this and other have to be
    // conformant (same size).
    // <br>Note that the assign function can be used to assign a
    // non-conforming vector.
    // <group>
    // TODO unlike Array, a Vector assign to an empty Vector does
    // not create a reference but does a value copy of the source.
    // This should be made consistent.
    using Array<T, Alloc>::assign_conforming;
    Vector<T, Alloc>& assign_conforming(const Vector<T, Alloc>& source)
    {
      return assign_conforming_implementation(source, std::is_copy_assignable<T>());
    }
    Vector<T, Alloc>& assign_conforming(Vector<T, Alloc>&& source);
    // source must be a 1-dimensional array.
    Vector<T, Alloc>& assign_conforming(const Array<T, Alloc>& source);
    Vector<T, Alloc>& assign_conforming(Array<T, Alloc>&& source);
    // </group>

    using Array<T, Alloc>::operator=;
    Vector<T, Alloc>& operator=(const Vector<T, Alloc>& source)
    { return assign_conforming(source); }
    Vector<T, Alloc>& operator=(Vector<T, Alloc>&& source)
    { return assign_conforming(std::move(source)); }
    Vector<T, Alloc>& operator=(const Array<T, Alloc>& source)
    { assign_conforming(source); return *this; }
    Vector<T, Alloc>& operator=(Array<T, Alloc>&& source)
    { assign_conforming(std::move(source)); return *this; }

    // Convert a Vector to a Block, resizing the block and copying values.
    // This is done this way to avoid having the simpler Block class 
    // containing dependencies on the Vector class.
    //void toBlock(Block<T> &other) const;

    // Single-pixel addressing. If AIPS_ARRAY_INDEX_CHECK is defined,
    // bounds checking is performed (not for [])..
    // <group>
    T &operator[](size_t index)
      { return (this->contiguous_p  ?  this->begin_p[index] : this->begin_p[index*this->inc_p(0)]); }
    const T &operator[](size_t index) const
      { return (this->contiguous_p  ?  this->begin_p[index] : this->begin_p[index*this->inc_p(0)]); }
    T &operator()(const IPosition &i)
      { return Array<T>::operator()(i); }
    const T &operator()(const IPosition &i) const 
      { return Array<T>::operator()(i); }
    T &operator()(size_t index)
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
	this->validateIndex(index);   //# Throws an exception on failure
#endif
        return *(this->begin_p + index*this->inc_p(0));
      }

    const T &operator()(size_t index) const
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
	this->validateIndex(index);   //# Throws an exception on failure
#endif
        return *(this->begin_p + index*this->inc_p(0));
      }
    // </group>

    // Take a slice of this vector. Slices are always indexed starting
    // at zero. This uses reference semantics, i.e. changing a value
    // in the slice changes the original.
    // <srcblock>
    // Vector<double> vd(100);
    // //...
    // vd(Slice(0,10)) = -1.0; // First 10 elements of vd set to -1
    // </srcblock>
    // <group>
    Vector<T, Alloc> operator()(const Slice &slice);
    const Vector<T, Alloc> operator()(const Slice &slice) const;
    // </group>

    // Slice using IPositions. Required to be defined, otherwise the base
    // class versions are hidden.
    // <group>
    Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc,
			const IPosition &incr)
      { return Array<T, Alloc>::operator()(blc,trc,incr); }
    const Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc,
                              const IPosition &incr) const
      { return Array<T, Alloc>::operator()(blc,trc,incr); }
    Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc)
      { return Array<T, Alloc>::operator()(blc,trc); }
    const Array<T, Alloc> operator()(const IPosition &blc, const IPosition &trc) const
      { return Array<T, Alloc>::operator()(blc,trc); }
    Array<T, Alloc> operator()(const Slicer& slicer)
      { return Array<T, Alloc>::operator()(slicer); }
    const Array<T, Alloc> operator()(const Slicer& slicer) const
      { return Array<T, Alloc>::operator()(slicer); }
    // </group>

    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask) const
      { return Array<T>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const LogicalArray &mask)
      { return Array<T>::operator() (mask); }

    // </group>


    // The array is masked by the input MaskedLogicalArray.
    // The mask is effectively the AND of the internal LogicalArray
    // and the internal mask of the MaskedLogicalArray.
    // The MaskedLogicalArray must conform to the array.
    // <group>

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask) const
      { return Array<T>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask)
      { return Array<T>::operator() (mask); }

    // </group>


    // The length of the Vector.
    const IPosition &shape() const
      { return this->length_p; }
    void shape(int &Shape) const
      { Shape = this->length_p(0); }

    // Verify that dimensionality is 1 and then call Array<T>::ok()
    virtual bool ok() const final override;

protected:
    // Remove the degenerate axes from other and store result in this vector.
    // An exception is thrown if removing degenerate axes does not result
    // in a vector.
    virtual void doNonDegenerate(const Array<T> &other,
                                 const IPosition &ignoreAxes) final override;
                                 
    virtual size_t fixedDimensionality() const final override { return 1; }

private:
    Vector<T, Alloc>& assign_conforming_implementation(const Vector<T, Alloc> &v, std::false_type isCopyable);
    Vector<T, Alloc>& assign_conforming_implementation(const Vector<T, Alloc> &v, std::true_type isCopyable);
  
    // Helper functions for constructors.
    void initVector(const std::vector<T>&, long long nr);      // copy semantics
    
    // The following two constructors are used to make distinguish between
    // Vector<literal>(literal size, literal value)
    // and
    // Vector<literal>(iterator, iterator)
    template<typename InputIterator>
    Vector(InputIterator startIter, InputIterator endIter, const Alloc& allocator, std::false_type /* T is not a literal */);
    
    template<typename InputIterator>
    Vector(InputIterator startIter, InputIterator endIter, const Alloc& allocator, std::true_type /* T is literal */);
};

} //#End namespace casacore

#include "Vector.tcc"
#include "Vector2.tcc"

#endif
