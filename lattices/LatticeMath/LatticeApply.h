//# LatticeApply.h: Optimally iterate through a Lattice and apply provided function object
//# Copyright (C) 1997,1998,1999
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

#ifndef LATTICES_LATTICEAPPLY_H
#define LATTICES_LATTICEAPPLY_H
 

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T, class U> class TiledCollapser;
template <class T, class U> class LineCollapser;
template <class T> class Lattice;
template <class T> class MaskedLattice;
class LatticeProgress;
class IPosition;
class LatticeRegion;


// <summary>
// Optimally iterate through a Lattice and apply provided function object
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Lattice>MaskedLattice</linkto>
//   <li> <linkto class=LineCollapser>LineCollapser</linkto>
//   <li> <linkto class=TiledCollapser>TiledCollapser</linkto>
// </prerequisite>

// <synopsis>
// This function iterates through a Lattice and applies a user given
// function object to chunks along the specified axes. Usually the
// function collapses the chunk to 1 or a few values (e.g. get min/max).
// The result of the function is written into the output Lattice(s) at the
// location of the collapsed chunk.  The output lattice(s) must be supplied
// with the correct shape. E.g. when a lattice with shape [nx,ny,nz] is
// collapsed by calculating the mean of each y-line, the output lattice
// has to have shape [nx,nz]. It is also possible to have output shape
// [nx,1,nz], [1,nx,nz], [nx,nz,1] or even e.g. [nx,1,1,1,nz].
// <p>
// By specifying a region it is possible to apply the function object
// to a subset of the lattice. Of course, the shape of the output lattice(s)
// have to match the shape of the region.
// <p>
// The iteration is done in an optimal way. To keep memory usage down,
// it caches as few tiles as possible.
// There are 2 ways to iterate.
// <ol>
// <li> For some applications an entire line is needed. An example is
// the calculation of the moment. The functions <src>lineApply</src>
// and <src>lineMultiApply</src> can be used for that purpose.
// Internally they use the
// <linkto class=TiledLineStepper>TiledLineStepper</linkto>
// navigator, so only a few tiles are kept in the cache.
// <br> One can also think of applications where an entire plane (or cube)
// is needed. This is not supported, but can be implemented when needed.
// <li> Other applications do not care how the data are traversed,
// making it possible to iterate tile by tile (which is optimal).
// An example is the calculation of the minimum, maximum, mean of
// a line, plane, etc..
// For this purpose the function <src>tiledApply</src> can be used.
// This function is faster and uses less memory than <src>lineApply</src>,
// so whenever possible this one should be used. Another advantage of
// this function is that it is possible to operate per line, plane, etc.
// or even for the entire lattice.
// </ol>
// The user has to supply a function object derived from the abstract base
// class <linkto class=LineCollapser>LineCollapser</linkto> or 
// <linkto class=TiledCollapser>TiledCollapser</linkto>, resp..
// The <src>process</src> function in these classes has to process
// the chunk of data passed in. The <src>nstepsDone</src> function
// in these classes can be used to monitor the progress.
// <p>
// The class is Doubly templated.  Ths first template type
// is for the data type you are processing.  The second type is
// for what type you want the results of the processing assigned to.
// For example, if you are computing sums of squares for statistical
// purposes, you might use higher precision (Float->Double) for this.
// No check is made that the template types are self-consistent.
// </synopsis>

// <example>
// Collapse each line in the y-direction using my collapser function object.
// <srcblock>
//    MyLineCollapser collapser;
//    PagedArray<Float> latticeIn("lattice.file");
//    IPosition shape = latticeIn.shape();
//    shape(1) = 1;
//    ArrayLattice<Double> latticeOut(shape);
//    LatticeApply<Float,Double>::lineApply (latticeOut, latticeIn, collapser, 1);
// </srcblock>
// </example>

// <motivation>
// This class makes it possible that a user can apply functions to
// a lattice in an optimal way, without having to know all the details
// of iterating through a lattice.
// </motivation>

//# <todo asof="1997/08/01">   
//#   <li> 
//# </todo>

 
template <class T, class U=T> class LatticeApply
{
public:

// This function iterates line by line through an input lattice and applies
// a user supplied function object to each line along the specified axis.
// The scalar result of the function object is written into the output
// lattice at the location of the collapsed line. The output lattice must
// be supplied with the correct shape (the shape of the supplied region).
// The default region is the entire input lattice.
// <group>
    static void lineApply (MaskedLattice<U>& latticeOut, 
			   const MaskedLattice<T>& latticeIn,
			   LineCollapser<T,U>& collapser,
			   uInt collapseAxis,
			   LatticeProgress* tellProgress = 0);
    static void lineApply (MaskedLattice<U>& latticeOut, 
			   const MaskedLattice<T>& latticeIn,
			   const LatticeRegion& region,
			   LineCollapser<T,U>& collapser,
			   uInt collapseAxis,
			   LatticeProgress* tellProgress = 0);
// </group>
    
// This function iterates line by line through an input lattice and applies
// a user supplied function object to each line along the specified axis.
// The vector result of the function object is written into the output
// lattices at the location of the collapsed line (1 value per lattice).
// The output lattices must be supplied with the correct shape (the shape
// of the supplied region).
// The default region is the entire input lattice.
// <group>
    static void lineMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut, 
				const MaskedLattice<T>& latticeIn,
				LineCollapser<T,U>& collapser,
				uInt collapseAxis,
				LatticeProgress* tellProgress = 0);
    static void lineMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut, 
				const MaskedLattice<T>& latticeIn,
				const LatticeRegion& region,
				LineCollapser<T,U>& collapser,
				uInt collapseAxis,
				LatticeProgress* tellProgress = 0);
// </group>

// This function iterates tile by tile through an input lattice and applies
// a user supplied function object to each chunk along the specified axes.
// A chunk can be a line, plane, etc. which is determined by the argument
// <src>collapseAxes</src>. E.g. IPosition(2,1,2) means planes along
// axes 1 and 2 (thus y,z planes).
// The result of the function object is written into the output
// lattice at the location of the collapsed chunk. The output lattice must
// be supplied with the correct shape (the shape of the supplied region
// plus the number of values resulting from the collapse).
// The default region is the entire input lattice.
// <group>
    static void tiledApply (MaskedLattice<U>& latticeOut,
			    const MaskedLattice<T>& latticeIn,
			    TiledCollapser<T,U>& collapser,
			    const IPosition& collapseAxes,
			    Int newOutAxis = -1,
			    LatticeProgress* tellProgress = 0);
    static void tiledApply (MaskedLattice<U>& latticeOut,
			    const MaskedLattice<T>& latticeIn,
			    const LatticeRegion& region,
			    TiledCollapser<T,U>& collapser,
			    const IPosition& collapseAxes,
			    Int newOutAxis = -1,
			    LatticeProgress* tellProgress = 0);
// </group>

// This function iterates tile by tile through an input lattice and applies
// a user supplied function object to each chunk along the specified axes.
// A chunk can be a line, plane, etc. which is determined by the argument
// <src>collapseAxes</src>. E.g. IPosition(2,1,2) means planes along
// axes 1 and 2 (thus y,z planes).
// The result of the function object is written into the output
// lattices at the location of the collapsed chunk. The output lattices must
// be supplied with the correct shape (the shape of the supplied region).
// The default region is the entire input lattice.
// <note role=warning>
// These functions are only declared, but not implemented yet.
// Thus they cannot be used yet.
// </note>
// <group>
    static void tiledMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut, 
				 const MaskedLattice<T>& latticeIn,
				 TiledCollapser<T,U>& collapser,
				 const IPosition& collapseAxes,
				 LatticeProgress* tellProgress = 0);
    static void tiledMultiApply (PtrBlock<MaskedLattice<U>*>& latticeOut, 
				 const MaskedLattice<T>& latticeIn,
				 const LatticeRegion& region,
				 TiledCollapser<T,U>& collapser,
				 const IPosition& collapseAxes,
				 LatticeProgress* tellProgress = 0);
// </group>


private:
    // Do some checks on the given arguments.
    // It returns an IPosition with the same length as shapeOut.
    // It contains a mapping of output to input axes. A value of -1
    // indicates that the axis is new (to contain the collapse result).
    // <br>Argument newOutAxis tells the output axis to store the results.
    // -1 means that the function has to find it out itself; it takes the
    // first axis with a length mismatching the corresponding input axis.
    static IPosition prepare (const IPosition& shapeIn,
			      const IPosition& shapeOut,
			      const IPosition& collapseAxes,
			      Int newOutAxis);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeApply.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
