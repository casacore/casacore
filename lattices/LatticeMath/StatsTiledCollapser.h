//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//# $Id: LatticeStatistics.h 20739 2009-09-29 01:15:15Z Malte.Marquarding $

#ifndef LATTICES_STATSTILEDCOLLAPSER_H
#define LATTICES_STATSTILEDCOLLAPSER_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore {


// <summary> Generate statistics, tile by tile, from a masked lattice </summary>

// NOTE this version was moved from LatticeStatistics (early Dec 2014 version)
// and slightly modified mostly for style issues (no significant semantic differences
// from that version). For a large number of statistics sets that need to be computed
// simultaneously, this version is more efficient than using the new stats framework,
// because creating large numbers of eg ClassicalStatistics objects is much less efficient
// than the direct manipulation of pointers to primitive types that this class does.
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=LatticeApply>LatticeApply</linkto>
//   <li> <linkto class=TiledCollapser>TiledCollapser</linkto>
// </prerequisite>
//
// <etymology>
// This class is used by <src>LatticeStatistics</src> to generate
// statistical sum from an input <src>MaskedLattice</src>.
// The input lattice is iterated through in tile-sized chunks
// and fed to an object of this class.
// </etymology>
//
// <synopsis>
// <src>StatsTiledCollapser</src> is derived from <src>TiledCollapser</src> which
// is a base class used to define methods.  Objects of this base class are
// used by <src>LatticeApply</src> functions.  In this particular case,
// we are interested in <src>LatticeApply::tiledApply</src>.  This  function iterates
// through a <src>MaskedLattice</src> and allows you to collapse one or more
// axes, computing some values from it, and placing those values into
// an output <src>MaskedLattice</src>.  It iterates through the input
// lattice in optimal tile-sized chunks.    <src>LatticeStatistics</src> 
// uses a <src>StatsTiledCollapser</src> object which it gives to 
// <src>LatticeApply::tiledApply</src> for digestion.  After it has
// done its work, <src>LatticeStatistics</src> then accesses the output
// <src>Lattice</src> that it made.
// </synopsis>
//
// <example>
// <srcblock>
//// Create collapser. Control information is passed in via the constructor
//
//   StatsTiledCollapser<T> collapser(range_p, noInclude_p, noExclude_p,   
//                                    fixedMinMax_p, blcParent_p);
// 
//// This is the first output axis getting  collapsed values. In LatticeStatistics
//// this is the last axis of the output lattice
// 
//   Int newOutAxis = outLattice.ndim()-1;
//
//// tiledApply does the work by passing the collapser data in chunks
//// and by writing the results into the output lattice 
//
//   LatticeApply<T>::tiledApply(outLattice, inLattice,
//                               collapser, collapseAxes,
//                               newOutAxis);
//
// </srcblock>
// In this example, a collapser is made and passed to LatticeApply.
// Afterwards, the output Lattice is available for use.
// The Lattices must all be the correct shapes on input to tiledApply
// </example>
//
// <motivation>
// The LatticeApply classes enable the ugly details of optimal
// Lattice iteration to be hidden from the user.
// </motivation>
//
// <todo asof="1998/05/10">   
//   <li> 
// </todo>

template <class T, class U=T>
class StatsTiledCollapser : public TiledCollapser<T, U> {
public:
	// Constructor provides pixel selection range and whether that
	// range is an inclusion or exclusion range.  If <src>fixedMinMax=True</src>
	// and an inclusion range is given, the min and max is set to
	// that inclusion range.
    StatsTiledCollapser(
    	const Vector<T>& pixelRange, Bool noInclude,
    	Bool noExclude, Bool fixedMinMax
    );

    virtual ~StatsTiledCollapser() {}

    // Initialize process, making some checks
    virtual void init (uInt nOutPixelsPerCollapse);

    // Initialiaze the accumulator
    virtual void initAccumulator (uInt n1, uInt n3);

    // Process the data in the current chunk.
    virtual void process (
    	uInt accumIndex1, uInt accumIndex3,
    	const T* inData, const Bool* inMask,
    	uInt dataIncr, uInt maskIncr,
    	uInt nrval,	const IPosition& startPos,
    	const IPosition& shape
    );

    // End the accumulation process and return the result arrays
    virtual void endAccumulator(Array<U>& result,
                                Array<Bool>& resultMask,
                                const IPosition& shape);

    // Can handle null mask
    virtual Bool canHandleNullMask() const {return True;};

    // Find the location of the minimum and maximum data values
    // in the input lattice.
 	void minMaxPos(IPosition& minPos, IPosition& maxPos);

private:
    Vector<T> _range;
    Bool _include, _exclude, _fixedMinMax, _isReal;
    IPosition _minpos, _maxpos;

    // Accumulators for sum, sum squared, number of points
    // minimum, and maximum

    CountedPtr<Block<U> > _sum, _sumSq, _npts,
    	_mean, _variance, _nvariance;
    CountedPtr<Block<T> > _min, _max;
    CountedPtr<Block<Bool> > _initMinMax;

    uInt _n1, _n3;
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/StatsTiledCollapser.tcc>
#endif
#endif
