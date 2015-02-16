//# LatticeMathUtil.cc: defines the Lattice Utilities global functions//# Copyright (C) 1995,1996,1997,1999,2000,2001,2002,2003,2004
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
//# $Id: LatticeUtilities.tcc 21531 2014-12-24 11:46:02Z gervandiepen $

#ifndef LATTICES_LATTICEMATHUTIL_TCC
#define LATTICES_LATTICEMATHUTIL_TCC

#include <casacore/lattices/LatticeMath/LatticeMathUtil.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore {  //# namespace casacore begin

template <class T> 
void minMax(T & globalMin, T & globalMax, 
	    IPosition & globalMinPos, IPosition & globalMaxPos, 
	    const Lattice<T> & lat) 
{
  //check if IPositions are conformant
  IPosition zeroPos = IPosition( lat.shape().nelements(), 0); 
  DebugAssert((zeroPos.nelements() == globalMinPos.nelements()), AipsError);
  DebugAssert((zeroPos.nelements() == globalMaxPos.nelements()), AipsError);
  
  IPosition cursorShape(lat.niceCursorShape());
  RO_LatticeIterator<T> latIter(lat, cursorShape);
  
  globalMin = lat.getAt( zeroPos );
  globalMinPos = zeroPos;
  globalMax = lat.getAt( zeroPos );
  globalMaxPos = zeroPos;
  
  for(latIter.reset(); !latIter.atEnd(); latIter++) {

    T localMin;
    IPosition localMinPos( latIter.cursor().ndim() );
    T localMax;
    IPosition localMaxPos( latIter.cursor().ndim() );

    Array<T>  arr = latIter.cursor();

    minMax(localMin, localMax, localMinPos, localMaxPos, arr);

    IPosition loc (latIter.position());
    
    if (localMin < globalMin) {
      globalMin = localMin;
      globalMinPos = loc + localMinPos;
    }
    if (localMax > globalMax) {
      globalMax = localMax;
      globalMaxPos = loc + localMaxPos;
    }
  }
}


// LatticeMathUtil

template <class T>
void LatticeMathUtil::collapse (Array<T>& out, const IPosition& axes,
                                 const MaskedLattice<T>& in,
                                 Bool dropDegenerateAxes) 
{ 
   out.resize();
   if (axes.nelements()==0) {
      out = in.get(dropDegenerateAxes);
   } else {
      LatticeStatistics<T> stats(in, False, False);
      AlwaysAssert(stats.setAxes(axes.asVector()), AipsError);
      stats.getConvertedStatistic(out, LatticeStatsBase::MEAN, dropDegenerateAxes);
   }
}

template <class T>
void LatticeMathUtil::collapse(
	Array<T>& data, Array<Bool>& mask,
    const IPosition& axes,
    const MaskedLattice<T>& in,
    Bool dropDegenerateAxes,
    Bool getPixels, Bool getMask,
    const LatticeStatsBase::StatisticsTypes stat
)
{ 
   data.resize();
   mask.resize();
   if (axes.nelements()==0) {
      if (getPixels) data = in.get(dropDegenerateAxes);
      if (getMask) mask = in.getMask(dropDegenerateAxes);
      return;
   }

// These lattice are all references so should be reasonably
// fast.  I can't do it the otherway around, i.e. drop degenerate
// axes first with an axes specifier because then the 'axes'
// argument won't match one to one with the lattice axes and
// that would be confusing.  Pity.
                      
   LatticeStatistics<T> stats(in, False, False);
   stats.setAxes(axes.asVector());
//
   if (getPixels) {
      stats.getConvertedStatistic(data, stat, dropDegenerateAxes);
   } else {
      data.resize(IPosition(0,0));
   }

// CLumsy way to get mask.  I should add it to LS

   if (getMask) {
      Array<T> n;
      stats.getConvertedStatistic(n, LatticeStatsBase::NPTS, dropDegenerateAxes);
      mask.resize(n.shape());
//
      T lim = (
    	stat == LatticeStatsBase::SIGMA
    	|| stat == LatticeStatsBase::VARIANCE
      ) ? 1.5 : 0.5;
      typename Array<T>::const_iterator itend = n.end();
      typename Array<T>::const_iterator it;
      typename Array<Bool>::iterator mIt;
      for (it=n.begin(),mIt=mask.begin(); it!=itend; ++it,++mIt) {
         *mIt = *it >= lim;
      }
   } else {
      mask.resize();
   }
}



} //# End namespace casacore

#endif
