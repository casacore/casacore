//# Slice.h: Define a (start,length,increment) along an axis
//# Copyright (C) 1993,1994,1995,1997
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

#ifndef CASA_SLICE_H
#define CASA_SLICE_H

#include <casacore/casa/aips.h>
#include <unistd.h>         //# for ssize_t

#if defined(AIPS_DEBUG)
#include <casacore/casa/Utilities/Assert.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations.
class Slicer;
class IPosition;
template<class T> class Vector;

// <summary> define a (start,length,increment) along an axis </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <synopsis>
// A "slice" (aka Section) is a a regular sub-Array (and ultimately sub-Image)
// that is defined by defining a (start,length,increment) for each axis in
// the array. That is, the output array's axis is of size "length", and the
// elements are sampled by stepping along the input array in strides of 
// "increment".
// <note role=warning> 
//   The "length" is the length of the OUTPUT array, the output length
//        is NOT divided by the increment/stride.
// </note>
// If increment is not defined, then it defaults to one. 
// (Increment, if defined, must be >= 1). If length
// is not defined, then it defaults to a length of one also (i.e. just the pixel
// "start"). If start is also undefined, then all pixels along this axis are
// chosen. This class deprecates the "_" (IndexRange) class, which had a failed
// syntax and used (start,end,increment) which is generally less convenient.
// Some simple examples follow:
// <srcblock> 
// Vector<Int> vi(100);          // Vector of length 100;
// //...
//                               // Copy odd values onto even values
// vi(Slice(0,50,2)) = vi(Slice(1,50,2));
// 
// Matrix<float> mf(100,50), smallMf;
// smallMf.reference(mf(Slice(0,10,10), Slice(0,5,10)));
//                               // smallMF is now a "dezoomed" (every 10th pix)
//                               // refference to mf. Of course we could also
//                               // make it a copy by using assignment; e.g:
//
// smallMf.resize(0,0);          // Make it so it will "size to fit"
// smallMf = mf(Slice(0,10,10), Slice(0,5,10));
// </srcblock> 
// As shown above, normally Slices will normally be used as temporaries,
// but they may also be put into variables if desired (the default
// copy constructors and assignment operators suffice for this class).
//
// While it will be unusual for a user to want this, a zero-length slice
// is allowable.
//
// Another way to produce a slice from any of the Array classes is to use
// SomeArray(blc,trc,inc) where blc,trc,inc are IPositions. This is described
// in the documentation for Array<T>.
// </synopsis>

class Slice
{
public:
    // The entire range of indices on the axis is desired.
    Slice();
    // Create a Slice with a given start, length, and increment. The latter
    // two default to one if not given.
    Slice(size_t Start, size_t Length=1, size_t Inc=1);
    // Create a Slice with a given start, end or length, and increment.
    // If <src>endIsLength=False</src>, end is interpreted as length.
    Slice(size_t Start, size_t End, size_t Inc, Bool endIsLength);
    // Was the entire range of indices on this axis selected?
    Bool all() const;
    // Report the selected starting position. If all() is true,
    // start=len=inc=0 is set.
    size_t start() const;
    // Report the defined length. If all() is true, start=len=inc=0 is set.
    size_t length() const;
    // Report the defined increment. If all() is true, start=len=inc=0 is set.
    size_t inc() const;
    // Attempt to report the last element of the slice. If all() is
    // True, end() returns -1 (which is less than start(), which returns
    // zero  in that case).
    size_t end() const;

    // Check a vector of slices.
    // If a vector of an axis is empty or missing, it is replaced by a Slice
    // representing the entire axis.
    // It checks if the Slices do not exceed the array shape.
    // It returns the shape of the combined slices and fills the Slicer
    // for the first array part defined by the slices.
    static IPosition checkSlices (Vector<Vector<Slice> >& slices, Slicer& first,
                                  const IPosition& shape);

private:
    //# Inc of <0 is used as a private flag to mean that the whole axis is
    //# selected. Users are given a uInt in their interface, so they cannot
    //# set it to this. Chose Inc rather than length since it's more likely
    //# that we'd need all bits of length than of inc. The "p" in the names
    //# stands for private to avoid it colliding with the accessor names.
    //# incp < 0 is chosen as the flag since the user can set inc to be zero
    //# although that is an error that can be caught if AIPS_DEBUG is defined).
    size_t  startp;
    ssize_t incp;
    size_t  lengthp;
};

inline Slice::Slice() : startp(0), incp(-1), lengthp(0)
{
    // Nothing
}

inline
Slice::Slice(size_t Start, size_t Length, size_t Inc)
  : startp(Start), incp(Inc), lengthp(Length)
{
#if defined(AIPS_DEBUG)
    DebugAssert(incp > 0, AipsError);
#endif
}

inline
Slice::Slice(size_t Start, size_t End, size_t Inc, Bool endIsLength)
  : startp(Start), incp(Inc), lengthp(endIsLength ? End : 1+(End-Start)/Inc)
{
#if defined(AIPS_DEBUG)
    if (! endIsLength) {
        DebugAssert(End >= Start, AipsError);
    }
    DebugAssert(incp > 0, AipsError);
#endif
}

inline Bool Slice::all() const
{
    return incp<0;
}

inline size_t Slice::start() const
{
    return startp;
}

inline size_t Slice::length() const
{
    return lengthp;
}

inline size_t Slice::inc() const
{
    if (all()) {
	return 0;
    } else {
	return incp;
    }
}

inline size_t Slice::end() const
{
    // return -1 if all()
    return startp + (lengthp-1)*incp;
}


} //# NAMESPACE CASACORE - END

#endif
