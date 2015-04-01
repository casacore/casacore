//# LineCollapser.h: Abstract base class to collapse lines for LatticeApply
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

#ifndef LATTICES_LINECOLLAPSER_H
#define LATTICES_LINECOLLAPSER_H
 

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Vector;
class IPosition;

// <summary>
// Abstract base class for LatticeApply function signatures
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
// This is an abstract base class for the collapsing of lines to
// be used in function <src>lineApply</src> or <src>lineMultiApply</src>
// in class <linkto class=LatticeApply>LatticeApply</linkto>.
// It is meant for cases where the entire line is needed (e.g. moment
// calculation). If that is not needed (e.g. to calculate maximum),
// it is better to use function <src>LatticeApply::tiledApply</src>
// with class <linkto class=TiledCollapser>TiledCollapser</linkto>.
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

template <class T, class U=T> class LineCollapser
{
public:
// Destructor
    virtual ~LineCollapser();

// The init function for a derived class.
// It can be used to check if <src>nOutPixelsPerCollapse</src>
// corresponds with the number of pixels produced per collapsed line.
    virtual void init (uInt nOutPixelsPerCollapse) = 0;

// Can the process function in the derived class handle a null mask?
// If not, LatticeApply ensures that it'll always pass a filled mask vector,
// even if the lattice does not have a mask (in that case that mask
// contains all True values).
// <br>The default implementation returns False.
// <br>The function is there to make optimization possible when no masks
// are involved. On the other side, it allows the casual user to ignore
// optimization.
    virtual Bool canHandleNullMask() const;

// Collapse the given line and return one value from that operation.
// The position in the Lattice at the start of the line is input
// as well.
// <br>When function <src>canHandleNullMask</src> returned True,
// it is possible that <src>mask</src> is an empty vector indicating
// that the input has no mask, thus all values are valid.
// If not empty, the mask has the same length as the line.
    virtual void process (U& result, Bool& resultMask,
			  const Vector<T>& line,
			  const Vector<Bool>& mask,
			  const IPosition& pos) = 0;

// Collapse the given line and return a line of values from that operation.
// The position in the Lattice at the start of the line is input
// as well.
// <br>When function <src>canHandleNullMask</src> returned True,
// it is possible that <src>mask</src> is an empty vector indicating
// that the input has no mask, thus all values are valid.
// If not empty, the mask has the same length as the line.
    virtual void multiProcess (Vector<U>& result, Vector<Bool>& resultMask,
			       const Vector<T>& line,
			       const Vector<Bool>& mask,
			       const IPosition& pos) = 0;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LineCollapser.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
