//# TiledCollapser.h: Abstract base class to collapse chunks for LatticeApply
//# Copyright (C) 1996,1997,1998
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

#ifndef LATTICES_TILEDCOLLAPSER_H
#define LATTICES_TILEDCOLLAPSER_H
 

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Array;
class IPosition;

// <summary>
// Abstract base class to collapse chunks for LatticeApply
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeApply>LatticeApply</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// This is an abstract base class for the collapsing of chunks to
// be used in function <src>tiledApply</src>
// in class <linkto class=LatticeApply>LatticeApply</linkto>.
// It is meant for cases where an entire line or plane is not needed
// (e.g. calculation of maximum). If that is needed (e.g. to calculate moment),
// it is better to use function <src>LatticeApply::lineApply</src>
// with class <linkto class=LineCollapser>LineCollapser</linkto>.
// <p>
// The user has to derive a concrete class from this base class
// and implement the (pure) virtual functions.
// <br> The main function is <src>process</src>, which needs to do the
// calculation.
// <br> Other functions make it possible to perform an initial check.
// <p>
// The class is Doubly templated.  Ths first template type
// is for the data type you are processing.  The second type is
// for what type you want the results of the processing assigned to.
// For example, if you are computing sums of squares for statistical
// purposes, you might use higher precision (FLoat->Double) for this.
// No check is made that the template types are self-consistent.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// </motivation>

// <todo asof="1997/08/01">   
//   <li> 
// </todo>


template <class T, class U=T> class TiledCollapser
{
public:

// Destructor
    virtual ~TiledCollapser();

// The init function for a derived class.
// It can be used to check if <src>nOutPixelsPerCollapse</src>
// corresponds with the number of pixels produced per collapsed chunk.
// <br><src>processAxis</src> is the axis of the line being passed
// to the <src>process</src> function.
    virtual void init (uInt nOutPixelsPerCollapse) = 0;

// Can the process function in the derived class handle a null mask pointer?
// If not, LatticeApply ensures that it'll always pass a mask block,
// even if the lattice does not have a mask (in that case that mask block
// contains all True values).
// <br>The default implementation returns False.
// <br>The function is there to make optimization possible when no masks
// are involved. On the other side, it allows the casual user to ignore
// optimization.
    virtual Bool canHandleNullMask() const;

// Create and initialize the accumulator.
// The accumulator can be a cube with shape [n1,n2,n3],
// where <src>n2</src> is equal to <src>nOutPixelsPerCollapse</src>.
// However, one can also use several matrices as accumulator.
// <br> The data type of the accumulator can be any. E.g. when
// accumulating Float lattices, the accumulator could be of
// type Double to have enough precision.
// <br>In the <src>endAccumulator</src> function the accumulator
// data has to be copied into an Array object with the correct
// shape and data type.
    virtual void initAccumulator (uInt n1, uInt n3) = 0;

// Collapse the given input data containing (<src>nrval</src> values
// with an increment of <src>inDataIncr</src> elements).
// <src>inMask</src> is a Bool block representing a mask with the
// same nr of values and increment as the input data. If a mask
// value is False, the corresponding input value is masked off.
// <br>When function <src>canHandleNullMask</src> returned True,
// it is possible that <src>inMask</src> is a null pointer indicating
// that the input has no mask, thus all values are valid.
// <br>
// The result(s) have to be stored in the accumulator at the given indices.
// <br><src>startPos</src> gives the lattice position of the first value.
// The position of other values can be calculated from index and shape
// using function <src>toPositionInArray</src> in class
// <linkto class=IPosition>IPosition</linkto>.
    virtual void process (uInt accumIndex1, uInt accumIndex3,
			  const T* inData, const Bool* inMask,
                          uInt inDataIncr, uInt inMaskIncr,
                          uInt nrval,
			  const IPosition& startPos,
			  const IPosition& shape) = 0;

// End the accumulator. It should return the accumulator as an
// Array of datatype U (e.g. double the precision of type T) 
// with the given shape. The accumulator should thereafter be deleted when needed.
    virtual void endAccumulator (Array<U>& result, 
                                 Array<Bool>& resultMask,
				 const IPosition& shape) = 0;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/TiledCollapser.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
