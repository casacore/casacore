//# LatticeMathUtil.h: useful global functions for Lattices
//# Copyright (C) 1995,1996,1997,1999,2000,2001,2002,2004
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
//# $Id: LatticeUtilities.h 21538 2015-01-07 09:08:57Z gervandiepen $

#ifndef LATTICES_LATTICEMATHUTIL_H
#define LATTICES_LATTICEMATHUTIL_H

#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class Array;
template <class T> class Lattice;
template <class T> class MaskedLattice;
template <class T> class MaskedArray;
class IPosition;
class LogIO;
class Slicer;

// <summary>Static math functions for Lattices</summary>
// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeMathUtil.cc" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
// </prerequisite>
//
// <synopsis>
// Some static helper math functions for Lattices
// </synopsis>
//
// <motivation>
// Common functionality not appropriate for Lattice member functions
// </motivation>
//
// <todo asof="2001/08/27">
//   <li> nothing I know of
// </todo>
//


class LatticeMathUtil
{
   public:

// Collapse the specified axes by averaging and recover the
// pixel values.  If axes is empty, then the data just contains
// all of the lattice (i.e. no collapse), 
// but dropDegenerateAxes is stil honoured
   template <class T>
   static void collapse (Array<T>& data, const IPosition& axes,
                         const MaskedLattice<T>& in,
                         Bool dropDegenerateAxes);
//
// Collapse the specified axes by averaging and recover either/and
// the pixel values and mask. If axes is empty, then the data and mask just contains
// all of the lattice (i.e. no collapse)
// but dropDegenerateAxes is stil honoured
   template <class T>
   static void collapse (
	Array<T>& data, Array<Bool>& mask,
    const IPosition& axes,
    const MaskedLattice<T>& lat,
    Bool dropDegenerateAxes,
    Bool getPixels=True, Bool getMask=True,
    const LatticeStatsBase::StatisticsTypes stat=LatticeStatsBase::MEAN
   );

};

// <summary>Global functions on Lattices</summary>
// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeMathUtil.cc" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
// </prerequisite>
//
// <synopsis>
// Global functions using Lattices
// </synopsis>
//
// <example>
// <h4>Example 1:</h4>
// Copy the lattice-type data between two Images.// <srcblock>
//
// PagedImage<Float> myImg ("myimagefile");
// Float lmin;
// Float lmax;
// IPosition posMin = myImg.shape();
// IPosition posMax = myImg.shape();
// minMax( lmin, lmax, posMin, posMax, myImg );
//
// </srcblock>
// </example>
//
//
// <motivation>
// Algorithms like CLEAN need to know the position of the MIN and MAX
// of an image, but easy things like LEL's min and max don't tell you
// the location of the min and max.  It seems there may be other global
// functions involving lattices.
// </motivation>
//
// <todo asof="1999/10/27">
//   <li> nothing I know of
// </todo>
//
// <group name=LatticeMathUtil>

// This global function finds the max of a Lattice, and also
// the IPositions of the max.  (LEL does not get you the IPositions of the
// min and max)

   template <class T>
   void minMax(T & min, T & max, 
               IPosition & posMin, IPosition & posMax, 
               const Lattice<T>& lat);

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeMathUtil.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
