//# VectorIter.h: Iterate a vector cursor through another array
//# Copyright (C) 1993,1994,1995,1999
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

#ifndef CASA_VECTORITER_H
#define CASA_VECTORITER_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// 
// <summary> Iterate an Vector cursor through another Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// VectorIterator steps a Vector (the "cursor") through an array for the
// given axis.
// The cursor "refers" to storage in the array, so that changing the
// values in the cursor changes values in the original array.
//
// This class is derived from ArrayIterator; basically it only adds
// the vector() member function which allows you to access the cursor
// as a Vector.
//
// <note role=tip>
// The origin of the cursor, i.e. the subarray that moves through the
// larger array, is always zero.
// </note>
//
// In this example we sum all the elements of an array; of course we already
// have the "sum" function in ArrayMath.h that we should use instead.
//
// <srcblock>
// Array<Float> af;
// // set af
// VectorIterator vi(af);
// Float sum = 0.0;
// uInt n = vi.vector().nelements();
// while (! vi.pastEnd()) {
//     for (Int i=0; i < n; i++) {   // N.B.; cursor always 0 based.
//         sum += vi.vector()(i);
//     }
//     vi.next();
// }
// </srcblock>

template<class T> class VectorIterator : public ArrayIterator<T>
{
public:
    // Iterate by vector cursors through array "a".
    // The vector cursor is taken for the given axis.
    explicit VectorIterator(Array<T> &a, uInt axis=0);

    // Return a Vector at the current position.
    Vector<T> &vector() {return *(Vector<T> *)this->ap_p;}

private:
    // Not implemented.
    VectorIterator(const VectorIterator<T> &);
    // Not implemented.
    VectorIterator<T> &operator=(const VectorIterator<T> &);
};

// 
// <summary> Iterate a Vector cursor through another Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// ReadOnlyVectorIterator behaves exactly like VectorIterator (cf.) only
// it should be used on const Arrays.
//
// <note role=tip> Note that the R/O VectorIterator is not derived from R/O 
//        ArrayIterator.
// </note>

template<class T> class ReadOnlyVectorIterator 
{
public:
    // <group>
    explicit ReadOnlyVectorIterator(const Array<T> &a, uInt axis=0)
      : vi(const_cast<Array<T>&>(a), axis) {}

    void next()   {vi.next();}
    void reset() {vi.origin();}
    void origin() {vi.origin();}
    
    const Array<T> &array() {return vi.array();}
    const Vector<T> &vector() {return vi.vector();}

    Bool atStart() const {return vi.atStart();}
    Bool pastEnd() const {return vi.pastEnd();}
    const IPosition &pos() const {return vi.pos();}
    IPosition endPos() const {return vi.endPos();}
    uInt ndim() const {return vi.ndim();}
    // </group>
private:
    // Not implemented.
    ReadOnlyVectorIterator(const ReadOnlyVectorIterator<T> &);
    // Not implemented.
    ReadOnlyVectorIterator<T> &operator=(const ReadOnlyVectorIterator<T> &);

    VectorIterator<T> vi;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/VectorIter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
