//# Cube.h: A 3-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1999
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

#if !defined(AIPS_CUBE_H)
#define AIPS_CUBE_H

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/CubeRtti.h>
// Could forward declare
#include <aips/Arrays/Slice.h>
#include <aips/Arrays/MaskedArray.h>

//# For index checking
#if defined(AIPS_ARRAY_INDEX_CHECK)
#include <aips/Arrays/IPosition.h>
#endif

//# Wouldn't be necessary if we could forward declare properly.
#include <aips/Arrays/Matrix.h>

// 
// <summary> A 3-D Specialization of the Array class </summary>
//
// Cube objects are three-dimensional specializations (e.g., more convenient
// and efficient indexing) of the general Array class. You might also want
// to look at the Array documentation to see inherited functionality. A
// tutorial on using the array classes in general is available in the
// "AIPS++ Programming Manual".
//
// Generally the member functions of Array are also available in
// Cube versions which take a pair of integers where the array 
// needs an IPosition. Since the Cube
// is three-dimensional, the IPositions are overkill, although you may
// use those versions if you want to.
// <srcblock>
// Cube<Int> ci(100,100,100);   // Shape is 100x100
// ci.resize(50,50,50);         // Shape now 50x50
// </srcblock>
//
// Slices may be taken with the Slice class. To take a slice, one "indexes" 
// with one Slice(start, length, inc) for each axis, where end and inc 
// are optional. Additionally, there is an xyPlane()
// member function which return a Matrix which corresponds to some plane:
// <srcblock>
// Cube<Float> cube(10,20,30);
// for(uInt i=0; i < 30; i++) {
//    cube.xyPlane(i) = i;   // Set every 10x20 plane to its "height"
// }
// </srcblock>
//
// Element-by-element arithmetic and logical operations are available (in
// aips/ArrayMath.h and aips/ArrayLogical.h).
//
// As with the Arrays, if the preprocessor symbol AIPS_DEBUG is
// defined at compile time invariants will be checked on entry to most
// member functions. Additionally, if AIPS_ARRAY_INDEX_CHECK is defined
// index operations will be bounds-checked. Neither of these should
// be defined for production code.

template<class T> class Cube : public Array<T>
{
public:

    // A Cube of length zero in each dimension; zero origin.
    Cube();

    // A l1xl2xl3 sized cube.
    Cube(uInt l1, uInt l2, uInt l3);

    // A l1xl2xl3 sized cube.
    // Fill it with the initial value.
    Cube(uInt l1, uInt l2, uInt l3, const T &initialValue);

    // A Cube where the shape ("len") is defined with IPositions.
    Cube(const IPosition &len);

    // A Cube where the shape ("len") is defined with IPositions.
    // Fill it with the initial value.
    Cube(const IPosition &len, const T &initialValue);

    // The copy constructor uses reference semantics.
    // <note role=warning> The copy constructor should normally be avoided. More
    //         details are available under the documentation for Array.
    //
    Cube(const Cube<T> &);

    // Construct a cube by reference from "other". "other must have
    // ndim() of 3 or less. The warning which applies to the copy constructor
    // is also valid here.
    Cube(const Array<T> &);

    // Create an Cube of a given shape from a pointer.
    Cube(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY);
    // Create an  Cube of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Cube(const IPosition &shape, const T *storage);

    // Define a destructor, otherwise the (SUN) compiler makes a static one.
    virtual ~Cube();

    // Make this cube a reference to other. Other must be of dimensionality
    // 3 or less.
    virtual void reference(Array<T> &other);

    // Resize to the given shape.
    // Resize without argument is equal to resize(0,0,0).
    // <group>
    void resize(uInt nx, uInt ny, uInt nz);
    virtual void resize();
    virtual void resize(const IPosition &);
    // </group>

    // Copy the values from other to this cube. If this cube has zero
    // elements then it will resize to be the same shape as other; otherwise
    // other must conform to this.
    // <group>
    Cube<T> &operator=(const Cube<T> &other);
    virtual Array<T> &operator=(const Array<T> &other);
    // </group>

    // Copy val into every element of this cube; i.e. behaves as if
    // val were a constant conformant cube.
    Array<T> &operator=(const T &val)
      { return Array<T>::operator=(val); }

    // Copy to this those values in marray whose corresponding elements
    // in marray's mask are True.
    Cube<T> &operator= (const MaskedArray<T> &marray)
      { Array<T> (*this) = marray; return *this; }


    // Single-pixel addressing. If AIPS_ARRAY_INDEX_CHECK is defined,
    // bounds checking is performed.
    // <group>
    T &operator()(const IPosition &i)
      { return Array<T>::operator()(i); }
    const T &operator()(const IPosition &i) const 
      { return Array<T>::operator()(i); }
    T &operator()(uInt i1, uInt i2, uInt i3)
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
	// It would be better performance wise for this to be static, but
	// CFront 3.0.1 doesn't like that.
        IPosition index(3);
        index(0) = i1; index(1) = i2; index(2) = i3;
        validateIndex(index);   // Throws an exception on failure
#endif
        return *(begin_p + i1*xinc_p + i2*yinc_p + i3*zinc_p);
      }

    const T &operator()(uInt i1, uInt i2, uInt i3) const
      {
#if defined(AIPS_ARRAY_INDEX_CHECK)
	// It would be better performance wise for this to be static, but
	// CFront 3.0.1 doesn't like that.
        IPosition index(3);
        index(0) = i1; index(1) = i2; index(2) = i3;
        validateIndex(index);   // Throws an exception on failure
#endif
        return *(begin_p + i1*xinc_p + i2*yinc_p + i3*zinc_p);
      }
    // </group>

    // Take a slice of this cube. Slices are always indexed starting
    // at zero. This uses reference semantics, i.e. changing a value
    // in the slice changes the original.
    // <srcblock>
    // Cube<Double> vd(100,100,100);
    // //...
    // vd(Slice(0,10),Slice(10,10,Slice(0,10))) = -1.0; // sub-cube set to -1.0
    // </srcblock>
    Cube<T> operator()(const Slice &sliceX, const Slice &sliceY,
		       const Slice &sliceZ);

    // Slice using IPositions. Required to be defined, otherwise the base
    // class versions are hidden.
    // <group>
    Array<T> operator()(const IPosition &blc, const IPosition &trc,
			const IPosition &incr)
      { return Array<T>::operator()(blc,trc,incr); }
    Array<T> operator()(const IPosition &blc, const IPosition &trc)
      { return Array<T>::operator()(blc,trc); }
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


    // Extract a plane as a cube. We could have xzPlane, etc also if that
    // would be of use to anyone. Of course you could also use a Matrix
    // iterator on the cube.
    // <group>
    Matrix<T> xyPlane(uInt zplane); 
    const  Matrix<T> xyPlane(uInt zplane) const; 
    // </group>

    // The length of each axis of the cube.
    // <group>
    void shape(Int &s1, Int &s2, Int &s3) const
      { s1 = length_p(0); s2=length_p(1); s3=length_p(2); }
    const IPosition &shape() const
      { return length_p; }
    // </group>

    // The position of the last element of the cube.
    // This is the same as shape(i) - 1; i.e. this is
    // a convenience funtion.
    // <group>
    void end(Int &e1, Int &e2, Int &e3) const 
      { e1=length_p(0)-1; e2=length_p(1)-1; e3=length_p(2)-1; }
    IPosition end() const;
    // </group>

    // The number of rows in the Cube, i.e. the length of the first axis.
    uInt nrow() const
      { return length_p(0); }

    // The number of columns in the Cube, i.e. the length of the 2nd axis.
    uInt ncolumn() const
      { return length_p(1); }

    // The number of planes in the Cube, i.e. the length of the 3rd axis.
    uInt nplane() const
      { return length_p(2); }

    // Replace the data values with those in the pointer <src>storage</src>.
    // The results are undefined is storage does not point at nelements() or
    // more data elements. After takeStorage() is called, <src>unique()</src>
    // is True.
    // <group>
    virtual void takeStorage(const IPosition &shape, T *storage,
		     StorageInitPolicy policy = COPY);
    // Since the pointer is const, a copy is always taken.
    virtual void takeStorage(const IPosition &shape, const T *storage);
    // </group>

    // Checks that the cube is consistent (invariants check out).
    virtual Bool ok() const;

    // Macro to define the typeinfo member functions
    rtti_dcl_mbrf_p1(Cube<T>, Array<T>);

protected:
    // Remove the degenerate axes from other and store result in this cube.
    // An exception is thrown if removing degenerate axes does not result
    // in a cube.
    virtual void doNonDegenerate(Array<T> &other, const IPosition &ignoreAxes);

private:
    // Cached constants to improve indexing.
    Int xinc_p, yinc_p, zinc_p;
    // Helper fn to calculate the indexing constants.
    void makeIndexingConstants();
};

#endif
