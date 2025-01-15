//# Cube.h: A 3-D Specialization of the Array Class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2003
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

#ifndef CASA_CUBE_2_H
#define CASA_CUBE_2_H

#include "Array.h"

namespace casacore { //#Begin casa namespace

// <summary> A 3-D Specialization of the Array class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// Cube objects are three-dimensional specializations (e.g., more convenient
// and efficient indexing) of the general Array class. You might also want
// to look at the Array documentation to see inherited functionality.
//
// Generally the member functions of Array are also available in
// Cube versions which take a pair of integers where the array 
// needs an IPosition. Since the Cube
// is three-dimensional, the IPositions are overkill, although you may
// use those versions if you want to.
// <srcblock>
// Cube<int> ci(100,100,100);   // Shape is 100x100
// ci.resize(50,50,50);         // Shape now 50x50
// </srcblock>
//
// Slices may be taken with the Slice class. To take a slice, one "indexes" 
// with one Slice(start, length, inc) for each axis, where end and inc 
// are optional. Additionally, there is an xyPlane()
// member function which return a Matrix which corresponds to some plane:
// <srcblock>
// Cube<float> cube(10,20,30);
// for(size_t i=0; i < 30; i++) {
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

template<typename T> class Cube : public Array<T>
{
public:

    // A Cube of length zero in each dimension; zero origin.
    Cube();

    // A l1xl2xl3 sized cube.
    // Fill it with the initial value.
    Cube(size_t l1, size_t l2, size_t l3, const T &initialValue=T());
    
    // An uninitialized l1xl2xl3 sized cube.
    Cube(size_t l1, size_t l2, size_t l3, typename Array<T>::uninitializedType);

    // A Cube where the shape ("len") is defined with IPositions.
    // Fill it with the initial value.
    Cube(const IPosition &length, const T &initialValue = T());

    // An uninitialized Cube where the shape ("len") is defined with IPositions.
    Cube(const IPosition& length, typename Array<T>::uninitializedType);
    
    // The copy constructor uses reference semantics.
    Cube(const Cube<T> &);
    Cube(Cube<T> &&);

    // Construct a cube by reference from "other". "other must have
    // ndim() of 3 or less. The warning which applies to the copy constructor
    // is also valid here.
    Cube(const Array<T> &);
    Cube(Array<T> &&);

    // Create an Cube of a given shape from a pointer.
    Cube(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY);
    // Create an  Cube of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Cube(const IPosition &shape, const T *storage);
    
    // Resize to the given shape.
    // Resize without argument is equal to resize(0,0,0).
    // <group>
    using Array<T>::resize;
    void resize(size_t nx, size_t ny, size_t nz, bool copyValues=false);
    // </group>

    // Copy the values from other to this cube. If this cube has zero
    // elements then it will resize to be the same shape as other; otherwise
    // other must conform to this.
    // Note that the assign function can be used to assign a
    // non-conforming cube.
    // <group>
    Cube<T> &operator=(const Cube<T>& source)
    { Array<T>::operator=(source); return *this; }
    Cube<T> &operator=(Cube<T>&& source)
    { Array<T>::operator=(std::move(source)); return *this; }
    
    Cube<T>& operator=(const Array<T>& source)
    {
      // TODO is it highly confusing that operator= is specialized for Cube, e.g.
      // this is allowed:
      //   Cube<int> cube(5,1,1);
      //   Vector<int> v(5,0);
      //   cube = v;
      // But this is not:
      //   Array arr(IPosition{5,1,1});
      //   Vector<int> v(5,0);
      //   arr = v;
      // If it should be allowed to assign from dim(5,1,1) to dim(5), this should
      // be supported already by the Array class so that the semantics are the
      // same!
      
      if (source.ndim() == 3) {
        Array<T>::operator=(source);
      } else {
        // This might work if a.ndim == 1 or 2
        (*this) = Cube<T>(source);
      }
      return *this;
    }
   
    Cube<T>& operator=(Array<T>&& source)
    {
      if (source.ndim() == 3) {
        Array<T>::operator=(std::move(source));
      } else {
        (*this) = Cube<T>(std::move(source));
      }
      return *this;
    }
   
    // </group>

    // Copy val into every element of this cube; i.e. behaves as if
    // val were a constant conformant cube.
    Array<T> &operator=(const T &val)
      { return Array<T>::operator=(val); }

    // Copy to this those values in marray whose corresponding elements
    // in marray's mask are true.
    
    // TODO
    //Cube<T> &operator= (const MaskedArray<T> &marray)
    //  { Array<T> (*this) = marray; return *this; }


    // Single-pixel addressing. If AIPS_ARRAY_INDEX_CHECK is defined,
    // bounds checking is performed.
    // <group>
    T &operator()(const IPosition &i)
      { return Array<T>::operator()(i); }
    const T &operator()(const IPosition &i) const 
      { return Array<T>::operator()(i); }

    T &operator()(size_t i1, size_t i2, size_t i3)
      {
	return this->begin_p[index(i1, i2, i3)];
      }

    const T &operator()(size_t i1, size_t i2, size_t i3) const
    {
      return this->begin_p[index(i1, i2, i3)];
    }
    // </group>

    // Take a slice of this cube. Slices are always indexed starting
    // at zero. This uses reference semantics, i.e. changing a value
    // in the slice changes the original.
    // <srcblock>
    // Cube<double> vd(100,100,100);
    // //...
    // vd(Slice(0,10),Slice(10,10,Slice(0,10))) = -1.0; // sub-cube set to -1.0
    // </srcblock>
    // <group>
    Cube<T> operator()(const Slice &sliceX, const Slice &sliceY,
		       const Slice &sliceZ);
    const Cube<T> operator()(const Slice &sliceX, const Slice &sliceY,
                             const Slice &sliceZ) const;
    // </group>

    // Slice using IPositions. Required to be defined, otherwise the base
    // class versions are hidden.
    // <group>
    Array<T> operator()(const IPosition &blc, const IPosition &trc,
			const IPosition &incr)
      { return Array<T>::operator()(blc,trc,incr); }
    const Array<T> operator()(const IPosition &blc, const IPosition &trc,
                              const IPosition &incr) const
      { return Array<T>::operator()(blc,trc,incr); }
    Array<T> operator()(const IPosition &blc, const IPosition &trc)
      { return Array<T>::operator()(blc,trc); }
    const Array<T> operator()(const IPosition &blc, const IPosition &trc) const
      { return Array<T>::operator()(blc,trc); }
    Array<T> operator()(const Slicer& slicer)
      { return Array<T>::operator()(slicer); }
    const Array<T> operator()(const Slicer& slicer) const
      { return Array<T>::operator()(slicer); }
    // </group>


    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>

    // Return a MaskedArray.
    const MaskedArray<T> operator() (const LogicalArray &mask) const
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
    const MaskedArray<T> operator() (const MaskedLogicalArray &mask) const
      { return Array<T>::operator() (mask); }

    // Return a MaskedArray.
    MaskedArray<T> operator() (const MaskedLogicalArray &mask)
      { return Array<T>::operator() (mask); }

    // </group>


    // Extract a plane as a matrix referencing the original data.
    // Of course you could also use a Matrix
    // iterator on the cube.
    // <group>
    Matrix<T> xyPlane(size_t zplane);
    const  Matrix<T> xyPlane(size_t zplane) const;
    Matrix<T> xzPlane(size_t yplane);
    const  Matrix<T> xzPlane(size_t yplane) const;
    Matrix<T> yzPlane(size_t xplane);
    const  Matrix<T> yzPlane(size_t xplane) const;
    // </group>

    // The length of each axis of the cube.
    const IPosition &shape() const
      { return this->length_p; }
    void shape(int &s1, int &s2, int &s3) const
      { s1 = this->length_p(0); s2=this->length_p(1); s3=this->length_p(2); }

    // The number of rows in the Cube, i.e. the length of the first axis.
    size_t nrow() const
      { return this->length_p(0); }

    // The number of columns in the Cube, i.e. the length of the 2nd axis.
    size_t ncolumn() const
      { return this->length_p(1); }

    // The number of planes in the Cube, i.e. the length of the 3rd axis.
    size_t nplane() const
      { return this->length_p(2); }

    // Checks that the cube is consistent (invariants check out).
    virtual bool ok() const override;

protected:
    virtual void preTakeStorage(const IPosition &shape) override;
    // Remove the degenerate axes from other and store result in this cube.
    // An exception is thrown if removing degenerate axes does not result
    // in a cube.
    virtual void doNonDegenerate(const Array<T> &other,
                                 const IPosition &ignoreAxes) override;
                                 
    size_t fixedDimensionality() const override { return 3; }

private:
    // Cached constants to improve indexing.
    //size_t xinc_p, yinc_p, zinc_p;
    // Helper fn to calculate the indexing constants.
    //void makeIndexingConstants();
    size_t xinc() const { return this->inc_p(0); }
    size_t yinc() const { return this->inc_p(1)*this->originalLength_p(0); }
    size_t zinc() const { return this->inc_p(2)*this->originalLength_p(0)*this->originalLength_p(1); }
    size_t index(size_t i1, size_t i2, size_t i3) const {
      return xinc()*i1 + 
        this->originalLength_p(0)*(this->inc_p(1)*i2 + 
          this->inc_p(2)*this->originalLength_p(1)*i3);
    }
    size_t index_continuous(size_t i1, size_t i2, size_t i3) const {
      return i1 + 
        this->originalLength_p(0)*(this->inc_p(1)*i2 + 
          this->inc_p(2)*this->originalLength_p(1)*i3);
    }

};

} //#End casa namespace

#include "Cube.tcc"

#endif
