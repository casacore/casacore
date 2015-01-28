//# LatticeFractile.cc: Static functions to get median and fractiles
//# Copyright (C) 1999,2000,2001
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

#ifndef LATTICES_LATTICEFRACTILE_H
#define LATTICES_LATTICEFRACTILE_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Lattice;
template<class T> class MaskedLattice;
template<class T> class Vector;
template<class T> class Block;


// <summary>
// Static functions to get median and fractiles of a lattice
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeFractile.cc tLELMedian.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <synopsis>
// This class contains a few static functions to find 1 or 2 fractiles
// in a lattice. They are primarily used by the LEL classes, but can
// also be used standalone.
// <br>
// A fractile is the same as a percentile be it that it is given as a
// fraction instead of a percentage. A fraction of 0.5 yields the median.
// <br>
// When the lattice has a mask, only the masked-on elements are taken into
// account. If all elements are masked_off, an empty Vector is returned
// indicating that no fractiles were found.
// <p>
// The algorithm used depends on the size of the lattice.
// Smallish lattices (i.e. not exceeding the argument smallSize)
// are handled in one pass im memory.
// For bigger lattices a multi-pass algorithm is used. First the
// lattices is binned. Thereafter the algorithm continues with the
// elements of the bins containing the fractiles. This continues
// until the number of elements left is less than <src>smallSize</src>.
// Typically only 2 passes are needed for a big image.
// <br>
// The algorithm is robust and takes possible rounding errors into account.
// It also takes into account that the lattice can contain many equal values.
// </synopsis> 

// <motivation>
// Separated from file LELFunction.h to make it more commonly usable
// and to make the source files more readable.
// </motivation>

//# <todo asof="2001/02/10">
//# </todo>


template<class T> class LatticeFractile
{
public: 
  // Determine the fractile of the given lattice. It returns the value
  // of the lattice at the given fraction. A fraction of 0.5 returns
  // the median. If the lattice has an even number of elements and if
  // the lattice is small enough (< 100 elements), the median is the
  // mean of the 2 middle elements.
  // <br>If the lattice is masked, only masked-on elements are taken
  // into account.
  // <br>If the lattice is large, successive histograms are made until
  // <src>smallSize</src> elements are left. Thereafter an in-memory
  // algorithm will be used to finish.
  // The number of passes made over the data is undetermined, but
  // a typical number is 2 passes.
  // <br>Normally a vector with 1 element is returned.
  // If the lattice has no masked-on elements, an empty vector is returned.
  // <group>
  static Vector<T> unmaskedFractile (const Lattice<T>& lattice,
				     Float fraction,
				     uInt smallSize = 4096*4096);
  static Vector<T> maskedFractile (const MaskedLattice<T>& lattice,
				   Float fraction,
				   uInt smallSize = 4096*4096);
  // </group>

  // Determine the values of the 2 elements at the given fractiles.
  // Thus <src>left=0.25; right=0.75</src> gives the quartiles of the lattice.
  // <br>If the lattice is masked, onlu masked-on elements are taken
  // into account.
  // <br>If the lattice is large, successive histograms are made until
  // <src>smallSize</src> elements are left. Thereafter an in-memory
  // algorithm will be used to finish.
  // The number of passes made over the data is undetermined, but
  // a typical number is 2 passes.
  // <br>Normally a vector with 2 elements is returned.
  // If the lattice has no masked-on elements, an empty vector is returned.
  // <group>
  static Vector<T> unmaskedFractiles (const Lattice<T>& lattice,
				      Float left, Float right,
				      uInt smallSize = 4096*4096);
  static Vector<T> maskedFractiles (const MaskedLattice<T>& lattice,
				    Float left, Float right,
				    uInt smallSize = 4096*4096);
  // </group>

private:
  // Determine the fractile for a small masked lattice.
  static Vector<T> smallMaskedFractile (const MaskedLattice<T>& lattice,
					Float fraction);

  // Determine the fractiles for a small masked lattice.
  static Vector<T> smallMaskedFractiles (const MaskedLattice<T>& lattice,
					 Float left, Float right);

  // Calculate the first histogram (with 10000 bins).
  // Also calculate the minimum and maximum. It returns the number
  // of masked-on values. Masked-off values are ignored.
  // <group>
  static uInt maskedHistogram (T& stv, T& endv, T& minv, T& maxv,
			       Block<uInt>& hist,
			       Block<T>& boundaries,
			       const MaskedLattice<T>& lattice);
  static void unmaskedHistogram (T& stv, T& endv, T& minv, T& maxv,
				 Block<uInt>& hist,
				 Block<T>& boundaries,
				 const Lattice<T>& lattice);
  // </group>

  // Helper function which determines which bin in the histogram 
  // contains the passed index. 
  // On input fractileInx gives the index of the fractile in the entire
  // histogram.
  // On output stv and endv are set to the boundaries of the bin containing
  // the index and fractileInx is set to the index in that bin.
  // The nr of values in that bin is returned as the function value.
  // minv and maxv are used as the outer limits, thus the first bin extends
  // to minv and the last bin to maxv.
  // If the bins are getting too small (i.e. if stv is nearly endv), 0 is
  // returned. In that case endv contains the fractile.
  static uInt findBin (uInt& fractileInx,
		       T& stv, T& endv,
		       T minv, T maxv,
		       const Block<uInt>& hist,
		       const Block<T>& boundaries);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeFractile.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
