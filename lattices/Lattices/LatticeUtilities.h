//# LatticeUtilities.h: useful global functions for Lattices
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
//# $Id$

#ifndef LATTICES_LATTICEUTILITIES_H
#define LATTICES_LATTICEUTILITIES_H

#include <lattices/Lattices/LatticeStatsBase.h>

#include <casa/aips.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> class Array;
template <class T> class Lattice;
template <class T> class MaskedLattice;
template <class T> class MaskedArray;
class IPosition;
class LogIO;
class Slicer;

// <summary>Static functions for Lattices</summary>
// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeUtilities.cc" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
// </prerequisite>
//
// <synopsis>
// Some static helper functions for Lattices
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


class LatticeUtilities 
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

// Copy data and mask from input to output.  If the input has no mask,
// that means all True (good), and these values will be transferred
// to the output.   Mask transfer only  occurs if the output has
// a writeable mask.
   template <class T>
   static void copyDataAndMask (LogIO& os, MaskedLattice<T>& out,
                                const MaskedLattice<T>& in, Bool zeroMasked=False);

// Replicate array through lattice in the specified region.
// The shape of <src>pixels</src> has to fit exactly into the shape of
// the selected region for each axis of <src>pixels</src>. Otherwise
// and exception will be thrown. For example,
// if the shape of the region is [10,20], the shape of pixels could
// be [10] and it will be replicated 20 times.  Another example would
// be that the shape of pixels could be [5,10] and it would be
// replicated 4 times fitting into the Lattice
   template <class T>
   static void replicate (Lattice<T>& lat,
                          const Slicer& region,
                          const Array<T>& pixels);

// Bin up one axis of MaskedArray (uses Lattices in implementation)
   template <class T>
   static void bin (MaskedArray<T>& out, const MaskedArray<T>& in, uInt axis, uInt bin);

// Add degenerate axes to the lattice if needed (nDim is the desired number of dimensions
// for the output lattice).  If the shapes are the same, the returned
// pointer holds a SubLattice.  If a reshape was necessary, the pointer
// holds an ExtendLattice.  The pointer is the callers responsibility to delete.
   template <class T>
   static void addDegenerateAxes (Lattice<T>*& pLatOut, const Lattice<T>& latIn, uInt nDim);
};

// <summary>Global functions on Lattices</summary>
// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeUtilities.cc" demos="">
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
// <group name=LatticeUtilities>

// This global function finds the max of a Lattice, and also
// the IPositions of the max.  (LEL does not get you the IPositions of the
// min and max)

   template <class T>
   void minMax(T & min, T & max, 
               IPosition & posMin, IPosition & posMax, 
               const Lattice<T>& lat);

// </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <lattices/Lattices/LatticeUtilities.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif


