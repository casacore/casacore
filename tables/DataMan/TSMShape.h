//# TSMShape.h: Expanded IPosition for shapes
//# Copyright (C) 1994,1995,1996,1999
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

#ifndef TABLES_TSMSHAPE_H
#define TABLES_TSMSHAPE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Expanded IPosition for shapes.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=IPosition>IPosition</linkto>
// </prerequisite>

// <etymology>
// TSMShape handles the shapes for the Tiled Storage Manager.
// </etymology>

// <synopsis> 
// TSMShape is an extension of class
// <linkto class=IPosition>IPosition</linkto>
// to handle shapes.
// It contains some precalculated values to speed up the calculation
// of an array offset from an array index (and vice-versa).
// </synopsis> 

// <motivation>
// The Tiled Hypercube Storage Manager is heavily using array shapes
// and determining offsets from array indices. This class makes these
// calculations more efficient.
// </motivation>

// <todo asof="$DATE:$">
//  <li> Integrate in a class like LatticeLayout.
// </todo>


class TSMShape
{
public:
    // A zero-length TSMShape.
    TSMShape();

    // Construct from a shape and precalculate some values.
    TSMShape (const IPosition& shape);

    // Copy constructor (copy semantics).
    TSMShape (const TSMShape& that);
    
    // Assignment (copy semantics).
    // "this" and "that" must either be conformant (same size)
    // or "this" must be 0-length, in which case it will
    // resize itself to be the same length as "that".
    TSMShape& operator= (const TSMShape& that);

    ~TSMShape();

    // Index into the TSMShape. Indices are zero-based. If the preprocessor
    // symbol AIPS_ARRAY_INDEX_CHECK is defined, "index" will be
    // checked to ensure it is not out of bounds. If this check fails, an
    // AipsError will be thrown.
    Int operator() (uInt index) const;

    // The number of elements in this TSMShape. Since TSMShape
    // objects use zero-based indexing, the maximum available index is
    // nelements() - 1.
    uInt nelements() const;

    // conform returns true if nelements() == other.nelements().
    Bool conform (const TSMShape& other) const;

    // Calculate the offset for a given position.
    // <group>
    size_t offset (const IPosition& position) const;
    size_t offset (const IPosition& position, const IPosition& origin) const;
    // </group>

    // Calculate the position for a given offset.
    // <group>
    IPosition position (size_t offset) const;
    IPosition position (size_t offset, const IPosition& origin) const;
    // </group>

    // Calculate the increments when stepping through an array in
    // a linear way. This can be used to update the array offset
    // without recalculating it after each step.
    // For example:
    // <srcblock>
    // template<class T>
    // Array<T> someFunc (const Array<T>& array,
    //                    const IPosition& subArrayShape,
    //                    const IPosition& subArrayStart) const
    // {
    //     TSMShape TSM (array.shape());
    //     IPosition offsetIncr = TSM.offsetIncrement (subArrayShape);
    //     Array<T> subArray(subArrayShape);
    //     Bool deleteMain;
    //     const T* mainData = array.getStorage (deleteMain);
    //     mainData += TSM.offset (subArrayStart)
    //     Bool deleteSub;
    //     T* subData = subArray.getStorage (deleteSub);
    //     for (uInt i=0; i<subArrayShape(2); i++) {
    //         for (uInt j=0; j<subArrayShape(1); j++) {
    //             for (uInt k=0; k<subArrayShape(0); k++) {
    //                 *subData++ = *mainData++;
    //             }
    //             mainData += offsetIncr(1);
    //         }
    //         mainData += offSetIncr(2);
    //     }
    // }
    // </srcblock>
    // <group>
    IPosition offsetIncrement (const IPosition& subShape) const;
    IPosition offsetIncrement (const IPosition& subShape,
			       const IPosition& stride) const;
    // </group>

private:
    IPosition data_p;
    uInt      size_p;     //# Not necessary, but done for speedup
};


inline uInt TSMShape::nelements() const
{
    return size_p;
}

inline Int TSMShape::operator()(uInt index) const
{
    return data_p(index);
}

inline Bool TSMShape::conform (const TSMShape& other) const
{
    return data_p.conform (other.data_p);
}



} //# NAMESPACE CASACORE - END

#endif
