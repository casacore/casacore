//# LatticeApply.h: Optimally iterate through Lattices and apply provided function
//# Copyright (C) 1996,1997
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
//
#if !defined(AIPS_LATTICEAPPLY_H)
#define AIPS_LATTICEAPPLY_H
 
#if defined(_AIX)
#pragma implementation ("LatticeApply.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Containers/Block.h>

//# Forward Declarations
template <class T> class VectorCollapser;
template <class T> class Lattice;
class IPosition;


// <summary>
// Optimally iterate through Lattices and apply provided function
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Lattice
//   <li> TiledLineStepper
// </prerequisite>

// <synopsis>
// Optimally iterate though a Lattice and apply the supplied function
// to the Lattice data.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// </motivation>

//# <todo asof="1997/08/01">   
//#   <li> 
//# </todo>

 
// Provided by the experts to push the data through the algorithm as
// efficiently as possible

template <class T> class LatticeApply
{
public:
// This function iterates through a Lattice and applies a user given
// function to profiles along the specified axis.  The result of the
// user supplied function is written into the output Lattice at the
// location of the collapsed profile.  The output lattice must be supplied
// with the correct shape; it must have the shape of the supplied region
// 
// Inputs
//  latticeIn  The input Lattice
//  collapser  The user supplies an object of a class derived from
//             the base class VectorCollapser.  The user provides the
//             implementation of the method "collapse" which takes
//             a profile and returns a value from it.
//  profileAxis     
//             The profile axis
//  blc,trc    The region of interest of the input Lattice.  Will be
//             filled in (0 & shape-1)if values not given or illegal
//  dropAxis   If False, then the output Lattice must have as many 
//             dimensions as the input Lattice, but its shape for
//             axis "index" must be unity.   E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,1,6]
//          
//             If True, then the output Lattice must have one less 
//             dimension than the input Lattice; the axis "index" is 
//             thus dropped from the output Lattice.    E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,6]
//  showProgress 
//             Show ProgressMeter
//  progressTitle
//             title for progress meter
// Output
//  latticeOut The output lattice.  It must be the correct shape to account
//             for the desired region (i.e. shape = trc - blc + 1)

    static void vectorApply (Lattice<T>& latticeOut, 
			     const Lattice<T>& latticeIn,
			     VectorCollapser<T>& operation,
			     const Int profileAxis,
			     const IPosition& blc,
			     const IPosition& trc,
////			     const IPosition& inc,
			     const Bool dropAxis,
			     const Bool showProgress,
			     const String& progressTitle);
    
// This function iterates through a Lattice and applies a user given
// function to profiles along the specified axis.  The result of the
// user supplied function is written into the output Lattices at the
// location of the collapsed profile.  The output lattices must be supplied
// with the correct shape; it must have the shape of the supplied region
//
// The collapser member function "collapse" must return a vector of numbers.
// These are assigned to the output lattices, one to one.
// 
// Inputs
//  latticeIn  The input Lattice
//  collapser  The user supplies an object of a class derived from
//             the base class VectorCollapser.  The user provides the
//             implementation of the method "multiCollapse" which takes
//             a profile and returns a vector of values from it.
//  profileAxis     
//             The profile axis
//  blc,trc    The region of interest of the input Lattice.  Will be
//             filled in (0 & shape-1)if values not given or illegal
//  dropAxis   If False, then the output Lattice must have as many 
//             dimensions as the input Lattice, but its shape for
//             axis "index" must be unity.   E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,1,6]
//          
//             If True, then the output Lattice must have one less 
//             dimension than the input Lattice; the axis "index" is 
//             thus dropped from the output Lattice.    E.g. if
//               in.shape() = [2,4,6]  and profileAxis=1,  then
//              out.shape() = [2,6]
//  showProgress 
//             Show ProgressMeter
//  progressTitle
//             title for progress meter
// Output
//  latticeOut The output lattices.  They must be the correct shape to account
//             for the desired region (i.e. shape = trc - blc + 1)

    static void vectorMultiApply (PtrBlock<Lattice<T>*>& latticeOut, 
				  const Lattice<T>& latticeIn,
				  VectorCollapser<T>& collapser,
				  const Int VectorAxis,
				  const IPosition& blc,
				  const IPosition& trc,
////				  const IPosition& inc,
				  const Bool dropAxis,
				  const Bool showProgress,
				  const String& progressTitle);
    
private:

   static void prepare (IPosition& ioMap,
			IPosition& inBlc,
			IPosition& inTrc,
			IPosition& inInc,
			const Lattice<T>& latticeIn,
			Lattice<T>& latticeOut,
			const Int VectorAxis,
			const Bool dropAxis);

};

#endif
