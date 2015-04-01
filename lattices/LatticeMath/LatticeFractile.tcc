//# LatticeFractile.cc: Static functions to get fractiles
//# Copyright (C) 1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTICEFRACTILE_TCC
#define LATTICES_LATTICEFRACTILE_TCC

#include <casacore/lattices/LatticeMath/LatticeFractile.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
uInt LatticeFractile<T>::findBin (uInt& fractileInx,
				  T& stv, T& endv,
				  T minv, T maxv,
				  const Block<uInt>& hist,
				  const Block<T>& boundaries)
{
  // Return 0 if minimum and maximum value are about equal.
  if (near (minv, maxv)) {
    endv = (minv+maxv)/2;
    return 0;
  }
  uInt foundBin = 0;
  uInt ndone = 0;
  const uInt nbins = hist.nelements()-1;
  // First determine the index of the bin containing the specified index.
  // If not found (rounding problems are possible) 0 is returned.
  while (ndone <= fractileInx) {
    if (foundBin == nbins) {
      endv = maxv;
      return 0;
    }
    ndone += hist[foundBin++];
  }
  foundBin--;
  // Now foundBin is the bin containing the requested index.
  // The nr of values in there have to be examined again.
  // Determine the offset of the fractile in the bin.
  // The start/end values are reset to the boundaries of this bin.
  uInt ntodo = hist[foundBin];
  ndone -= ntodo;
  fractileInx -= ndone;
  stv  = boundaries[foundBin];
  endv = boundaries[foundBin+1];
  if (foundBin == 0  ||  stv < minv) {
    stv = minv;
  }
  if (foundBin == nbins-1  ||  endv > maxv) {
    endv = maxv;
  }
  // Return 0 if bin gets too narrow.
  if (near (stv, endv)) {
    endv = (stv+endv)/2;
    ntodo = 0;
  }
  return ntodo;
}


template <class T>
void LatticeFractile<T>::unmaskedHistogram (T& stv, T& endv, T& minv, T& maxv,
					    Block<uInt>& hist,
					    Block<T>& boundaries,
					    const Lattice<T>& lattice)
{
  AlwaysAssert (hist.nelements() == boundaries.nelements(), AipsError);
  // Find number of bins (last one is for extraneous values).
  // Scale between -50 and +50 (which is usually okay for
  // radio-astronomical images, both for the image itself and for
  // difference of image with median or so).
  // It is only a first guess, so nothing goes wrong if grossly incorrect.
  // It may only result in one more iteration.
  const uInt nbins = hist.nelements() - 1;
  T step = 2*50./nbins;
  minv = 0;
  maxv = 0;
  for (uInt i=0; i<=nbins; ++i) {
    boundaries[i] = i*step - 50.;
  }
  stv  = boundaries[0];
  endv = boundaries[nbins];
  Bool firstTime = True;
  // Iterate through the lattice.
  RO_LatticeIterator<T> iter(lattice);
  while (! iter.atEnd()) {
    Bool delData;
    const Array<T>& array = iter.cursor();
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    if (firstTime) {
      firstTime = False;
      minv = dataPtr[0];
      maxv = dataPtr[0];
    }
    for (uInt i=0; i<n; i++) {
      if (dataPtr[i] < minv) {
	minv = dataPtr[i];
      } else if (dataPtr[i] > maxv) {
	maxv = dataPtr[i];
      }
      Int bin = Int((dataPtr[i] - stv)/step);
      if (bin < 0) {
	hist[0]++;
      } else if (bin >= Int(nbins)) {
	hist[nbins-1]++;
      } else {
	if (dataPtr[i] < boundaries[bin] &&  bin > 0) {
	  bin--;
	} else if (dataPtr[i] >= boundaries[bin+1]) {
	  bin++;
	}
	hist[bin]++;
      }
    }
    array.freeStorage (dataPtr, delData);
    iter++;
  }
}


template <class T>
uInt LatticeFractile<T>::maskedHistogram (T& stv, T& endv, T& minv, T& maxv,
					  Block<uInt>& hist,
					  Block<T>& boundaries,
					  const MaskedLattice<T>& lattice)
{
  AlwaysAssert (hist.nelements() == boundaries.nelements(), AipsError);
  uInt ntodo = 0;
  // Find number of bins (last one is for extraneous values).
  // Scale between -50 and +50 (which is usually okay for
  // radio-astronomical images, both for the image itself and for
  // difference of image with median or so).
  // It is only a first guess, so nothing goes wrong if grossly incorrect.
  // It may only result in one more iteration.
  const uInt nbins = hist.nelements() - 1;
  T step = 2*50./nbins;
  minv = 0;
  maxv = 0;
  for (uInt i=0; i<=nbins; ++i) {
    boundaries[i] = i*step - 50.;
  }
  stv  = boundaries[0];
  endv = boundaries[nbins];
  Bool firstTime = True;
  // Iterate through the lattice.
  COWPtr<Array<Bool> > mask;
  RO_MaskedLatticeIterator<T> iter(lattice);
  while (! iter.atEnd()) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    iter.getMask (mask);
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	ntodo++;
	if (firstTime) {
	  firstTime = False;
	  minv = dataPtr[i];
	  maxv = dataPtr[i];
	} else {
	  if (dataPtr[i] < minv) {
	    minv = dataPtr[i];
	  } else if (dataPtr[i] > maxv) {
	    maxv = dataPtr[i];
	  }
	}
	Int bin = Int((dataPtr[i] - stv)/step);
	if (bin < 0) {
	  hist[0]++;
	} else if (bin >= Int(nbins)) {
	  hist[nbins-1]++;
	} else {
	  if (dataPtr[i] < boundaries[bin] &&  bin > 0 ) {
	    bin--;
	  } else if (dataPtr[i] >= boundaries[bin+1] &&  bin < Int(nbins)-1) {
	    bin++;
	  }
	  hist[bin]++;
	}
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  return ntodo;
}


template <class T>
Vector<T> LatticeFractile<T>::unmaskedFractile (const Lattice<T>& lattice,
						Float fraction,
						uInt smallSize)
						{
	AlwaysAssert (fraction >= 0  &&  fraction <= 1, AipsError);
	// Determine the number of elements in the lattice.
	// If empty, return empty vector.
	// If small enough, we read them all and do it in memory.
	uInt ntodo = lattice.shape().product();
	if (ntodo == 0) {
		return Vector<T>();
	}
	Vector<T> result(1);
	if (ntodo <= smallSize) {
		if (fraction == 0.5) {
			result(0) = median (lattice.get());
		} else {
			result(0) = fractile (lattice.get(), fraction);
		}
		return result;
	}
	// Bad luck. We have to do some more work.
	// Do a first binning while determining min/max at the same time.
	// Hopefully the start and end values make some sense.
	// Make the block 1 element larger, because possible roundoff errors
	// could result in a binnr just beyond the end.
	const uInt nbins = 10000;
	Block<uInt> hist(nbins+1, 0u);
	Block<T> boundaries(nbins+1);
	T stv, endv, minv, maxv;
	unmaskedHistogram (stv, endv, minv, maxv, hist, boundaries, lattice);
	// The index of the fractile in the lattice is the middle one.
	// In case of an even nr of elements, it is the first one of the
	// two middle ones.
	uInt fractileInx = uInt(fraction * (ntodo-1));
	// Iterate until the bin containing the fractile does not
	// contain too many values anymore.
	RO_LatticeIterator<T> iter(lattice);
	while (True) {
		// Determine which bin contains the fractile and update the various values.
		// On return fractileInx,stv,endv form the basis of the new histogram.
		ntodo = findBin (fractileInx, stv, endv, minv, maxv, hist, boundaries);
		// If only a 'few' more points to do, stop making histograms.
		// Exit if nothing left to do.
		if (ntodo <= smallSize) {
			if (ntodo == 0) {
				result(0) = endv;
				return result;
			}
			break;
		}
		// Histogram the fractile bin with a much smaller bin size.
		// Determine the min and max of the remaining values.
		minv = endv;
		maxv = stv;
		hist = 0;
		T step = (endv - stv) / nbins;
		for (uInt i=0; i<=nbins; i++) {
			boundaries[i] = stv + i*step;
		}
		uInt ndone = 0;
		iter.reset();
		while (! iter.atEnd()  &&  ndone<ntodo) {
			const Array<T>& array = iter.cursor();
			Bool delData;
			const T* dataPtr = array.getStorage (delData);
			uInt n = array.nelements();
			for (uInt i=0; i<n; i++) {
				if (dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
					Int bin = Int((dataPtr[i] - stv) / step);
					// Due to rounding the bin number might get one too low or high.
					if (dataPtr[i] < boundaries[bin]) {
						bin--;
					} else if (dataPtr[i] >= boundaries[bin+1]) {
						bin++;
					}
					hist[bin]++;
					if (dataPtr[i] < minv) {
						minv = dataPtr[i];
					}
					if (dataPtr[i] > maxv) {
						maxv = dataPtr[i];
					}
					ndone++;
				}
			}
			array.freeStorage (dataPtr, delData);
			iter++;
		}
		// In principle the last bin should be empty, but roundoff errors
		// might have put a few in there. So add them to previous one.
		hist[nbins-1] += hist[nbins];
	}
	// There are only a 'few' points left.
	// So read them all in and determine the fractileInx'th-largest.
	// Again, due to rounding we might find a few elements more or less.
	// So take care that the receiving block is not exceeded and that
	// the number of elements found are used in kthLargest.
	// Note it also makes sense to stop the iteration when we found all
	// elements. It may save a few reads from the lattice.
	Block<T> tmp(ntodo);
	T* tmpPtr = tmp.storage();
	uInt ndone = 0;
	iter.reset();
	while (! iter.atEnd()  &&  ndone<ntodo) {
		const Array<T>& array = iter.cursor();
		Bool delData;
		const T* dataPtr = array.getStorage (delData);
		uInt n = array.nelements();
		for (uInt i=0; i<n; i++) {
			if (dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
				tmpPtr[ndone++] = dataPtr[i];
				if (ndone == ntodo) {
					break;
				}
			}
		}
		array.freeStorage (dataPtr, delData);
		iter++;
	}
	// By rounding it is possible that not enough elements were found.
	// In that case return the middle of the (very small) interval.
	if (fractileInx >= ndone) {
		result(0) = (stv+endv)/2;
	} else {
		result(0) = GenSort<T>::kthLargest (tmp.storage(), ndone, fractileInx);
	}
	return result;
}


template <class T>
Vector<T> LatticeFractile<T>::maskedFractile (const MaskedLattice<T>& lattice,
					      Float fraction,
					      uInt smallSize)
{
  AlwaysAssert (fraction >= 0  &&  fraction <= 1, AipsError);
  // If unmasked, a simpler way can be used.
  if (! lattice.isMasked()) {
    return unmaskedFractile (lattice, fraction, smallSize);
  }
  // Determine the number of elements in the lattice.
  // If small enough, we read them all and do it in memory.
  uInt ntodo = lattice.shape().product();
  if (ntodo <= smallSize) {
    return smallMaskedFractile (lattice, fraction);
  }
  Vector<T> result(1);
  // Bad luck. We have to do some more work.
  // Do a first binning while determining min/max at the same time.
  // Hopefully the start and end values make some sense.
  // Make the block 1 element larger, because possible roundoff errors
  // could result in a binnr just beyond the end.
  const uInt nbins = 10000;
  Block<uInt> hist(nbins+1, 0u);
  Block<T> boundaries(nbins+1);
  T stv, endv, minv, maxv;
  ntodo = maskedHistogram (stv, endv, minv, maxv, hist, boundaries, lattice);
  if (ntodo == 0) {
    return Vector<T>();
  }
  // The index of the fractile in the lattice is the middle one.
  // In case of an even nr of elements, it is the first one of the
  // two middle ones.
  uInt fractileInx = uInt(fraction * (ntodo-1));
  // Iterate until the bin containing the fractile does not
  // contain too many values anymore.
  COWPtr<Array<Bool> > mask;
  RO_MaskedLatticeIterator<T> iter(lattice);
  while (True) {
    // Determine which bin contains the fractile and update the various values.
    // On return fractileInx,stv,endv form the basis of the new histogram.
    ntodo = findBin (fractileInx, stv, endv, minv, maxv, hist, boundaries);
    // If only a 'few' more points to do, stop making histograms.
    // Exit if nothing left to do.
    if (ntodo <= smallSize) {
      if (ntodo == 0) {
	result(0) = endv;
	return result;
      }
      break;
    }
    // Histogram the fractile bin with a much smaller bin size.
    // Determine the min and max of the remaining values.
    minv = endv;
    maxv = stv;
    hist = 0;
    T step = (endv - stv) / nbins;
    for (uInt i=0; i<=nbins; i++) {
      boundaries[i] = stv + i*step;
    }
    uInt ndone = 0;
    iter.reset();
    while (! iter.atEnd()  &&  ndone<ntodo) {
      Bool delData, delMask;
      const Array<T>& array = iter.cursor();
      iter.getMask (mask);
      const Bool* maskPtr = mask->getStorage (delMask);
      const T* dataPtr = array.getStorage (delData);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	if (maskPtr[i]  &&  dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	  Int bin = Int((dataPtr[i] - stv) / step);
	  // Due to rounding the bin number might get one too low or high.
	  if (dataPtr[i] < boundaries[bin]) {
	    bin--;
	  } else if (dataPtr[i] >= boundaries[bin+1]) {
	    bin++;
	  }
	  hist[bin]++;
	  if (dataPtr[i] < minv) {
	    minv = dataPtr[i];
	  }
 	  if (dataPtr[i] > maxv) {
	    maxv = dataPtr[i];
	  }
	  ndone++;
	}
      }
      array.freeStorage (dataPtr, delData);
      mask->freeStorage (maskPtr, delMask);
      iter++;
    }
    // In principle the last bin should be empty, but roundoff errors
    // might have put a few in there. So add them to previous one.
    hist[nbins-1] += hist[nbins];
  }
  // There are only a 'few' points left.
  // So read them all in and determine the fractileInx'th-largest.
  // Again, due to rounding we might find a few elements more or less.
  // So take care that the receiving block is not exceeded and that
  // the number of elements found are used in kthLargest.
  // Note it also makes sense to stop the iteration when we found all
  // elements. It may save a few reads from the lattice.
  Block<T> tmp(ntodo);
  T* tmpPtr = tmp.storage();
  uInt ndone = 0;
  iter.reset();
  while (! iter.atEnd()  &&  ndone<ntodo) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    iter.getMask (mask);
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]  &&  dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	tmpPtr[ndone++] = dataPtr[i];
	if (ndone == ntodo) {
	  break;
	}
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  // By rounding it is possible that not enough elements were found.
  // In that case return the middle of the (very small) interval.
  if (fractileInx >= ndone) {
    result(0) = (stv+endv)/2;
  } else {
    result(0) = GenSort<T>::kthLargest (tmp.storage(), ndone, fractileInx);
  }
  return result;
}


template <class T>
Vector<T> LatticeFractile<T>::smallMaskedFractile
                                       (const MaskedLattice<T>& lattice,
					Float fraction)
{
  // Make a buffer to hold all masked-on elements.
  // The number of values is not more than the number of elements in the
  // lattice, so make the buffer that long.
  uInt size = lattice.shape().product();
  Block<T> buffer(size);
  uInt npts = 0;
  // Iterate through the lattice and assemble all masked-on elements.
  COWPtr<Array<Bool> > mask;
  RO_MaskedLatticeIterator<T> iter(lattice);
  while (! iter.atEnd()) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    iter.getMask (mask);
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	buffer[npts++] = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  if (npts == 0) {
    return Vector<T>();
  }
  // Use median of an Array instead of kthLargest directly, because
  // for a small array with an even number of elements, median takes
  // the average of the 2 middle elements.
  Vector<T> result(1);
  if (fraction == 0.5) {
    result(0) = median (Array<T> (IPosition(1,npts), buffer.storage(), SHARE));
  } else {
    uInt fractileInx = uInt (fraction * (npts-1));
    result(0) = GenSort<T>::kthLargest (buffer.storage(), npts, fractileInx);
  }
  return result;
}


template <class T>
Vector<T> LatticeFractile<T>::unmaskedFractiles (const Lattice<T>& lattice,
						 Float left, Float right,
						 uInt smallSize)
{
  AlwaysAssert (left >= 0  &&  left <= right  &&  right <= 1, AipsError);
  // Determine the number of elements in the lattice.
  // If small enough, we read them all and do it in memory.
  uInt ntodo1 = lattice.shape().product();
  if (ntodo1 == 0) {
    return Vector<T>();
  }
  // Find which elements are left and right fractile.
  uInt leftInx = uInt (left * (lattice.nelements()-1));
  uInt rightInx = uInt (right * (lattice.nelements()-1));
  Vector<T> result(2);
  if (ntodo1 <= smallSize) {
    // We can hold all data in memory.
    Bool delData;
    Array<T> array = lattice.get();
    T* dataPtr = array.getStorage (delData);
    result(0) = GenSort<T>::kthLargest (dataPtr, ntodo1, leftInx);
    result(1) = GenSort<T>::kthLargest (dataPtr, ntodo1, rightInx);
    // Storage only needs to be freed, but we need a const pointer for that.
    const T* constDataPtr = dataPtr;
    array.freeStorage (constDataPtr, delData);
    return result;
  }
  // Bad luck. We have to do some more work.
  // Do a first binning while determining min/max at the same time.
  // Hopefully the start and end values make some sense.
  // Make the block 1 element larger, because possible roundoff errors
  // could result in a binnr just beyond the end.
  const uInt nbins = 10000;
  Block<uInt> hist1(nbins+1, 0u);
  Block<T> boundaries1(nbins+1);
  T stv1, endv1, minv1, maxv1;
  unmaskedHistogram (stv1, endv1, minv1, maxv1, hist1, boundaries1, lattice);
  // Init variables for both fractiles.
  uInt ntodo2 = ntodo1;
  T stv2 = stv1;
  T endv2 = endv1;
  T minv2 = minv1;
  T maxv2 = maxv1;
  Block<uInt> hist2 (hist1);
  Block<T> boundaries2 (boundaries1);
  Bool finished1 = False;
  Bool finished2 = False;
  // Iterate until the bins containing the fractiles do not
  // contain too many values anymore.
  RO_LatticeIterator<T> iter(lattice);
  while (True) {
    // Determine which bin contains the requested values, determine
    // new boundaries, max/min and offset in bin.
    // Do that for left and right fractile.
    uInt ntodo = 0;
    if (!finished1) {
      ntodo1 = findBin (leftInx,  stv1, endv1, minv1, maxv1,
			hist1, boundaries1);
      // If only a 'few' more points to do, stop making histograms.
      // Otherwise histogram the fractile bin with a much smaller bin size.
      if (ntodo1 <= smallSize) {
	finished1 = True;
	if (ntodo1 == 0) {
	  result(0) = endv1;
	}
      } else {
	ntodo += ntodo1;
      }
    }
    if (!finished2) {
      ntodo2 = findBin (rightInx, stv2, endv2, minv2, maxv2,
			hist2, boundaries2);
      if (ntodo2 <= smallSize) {
	finished2 = True;
	if (ntodo2 == 0) {
	  result(1) = endv2;
	}
      } else {
	ntodo += ntodo2;
      }
    }
    // Stop if both fractiles have small enough bins.
    if (finished1 && finished2) {
      break;
    }
    // Build new histograms with determined subsets
    minv1 = endv1;
    minv2 = endv2;
    maxv1 = stv1;
    maxv2 = stv2;
    hist1 = 0;
    hist2 = 0;
    T step1 = (endv1 - stv1) / nbins;
    T step2 = (endv2 - stv2) / nbins;
    for (uInt i=0; i<=nbins; i++) {
      boundaries1[i] = stv1 + i*step1;
      boundaries2[i] = stv2 + i*step2;
    }
    uInt ndone = 0;
    iter.reset();
    while (! iter.atEnd()  &&  ndone<ntodo) {
      const Array<T>& array = iter.cursor();
      Bool delData;
      const T* dataPtr = array.getStorage (delData);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	if (!finished1  &&  dataPtr[i] >= stv1  &&  dataPtr[i] < endv1) {
	  Int bin = Int((dataPtr[i] - stv1) / step1);
	  // Due to rounding the bin number might get one too low or high.
	  if (dataPtr[i] < boundaries1[bin]) {
	    bin--;
	  } else if (dataPtr[i] >= boundaries1[bin+1]) {
	    bin++;
	  }
	  hist1[bin]++;
	  if (dataPtr[i] < minv1) {
	    minv1 = dataPtr[i];
	  }
 	  if (dataPtr[i] > maxv1) {
	    maxv1 = dataPtr[i];
	  }
	  ndone++;
	}
	if (!finished2  &&  dataPtr[i] >= stv2  &&  dataPtr[i] < endv2) {
	  Int bin = Int((dataPtr[i] - stv2) / step2);
	  // Due to rounding the bin number might get one too low or high.
	  if (dataPtr[i] < boundaries2[bin]) {
	    bin--;
	  } else if (dataPtr[i] >= boundaries2[bin+1]) {
	    bin++;
	  }
	  hist2[bin]++;
	  if (dataPtr[i] < minv2) {
	    minv2 = dataPtr[i];
	  }
 	  if (dataPtr[i] > maxv2) {
	    maxv2 = dataPtr[i];
	  }
	  ndone++;
	}
      }
      array.freeStorage (dataPtr, delData);
      iter++;
    }
    // In principle the last bins should be empty, but roundoff errors
    // might have put a few in there. So add them to previous one.
    hist1[nbins-1] += hist1[nbins];
    hist2[nbins-1] += hist2[nbins];
  }
  if (ntodo1 == 0  &&  ntodo2 == 0) {
    return result;
  }
  // There are only a 'few' points left in both histograms.
  // So read them all in and determine the Inx'th-largest.
  // Again, due to rounding we might find a few elements more or less.
  // So take care that the receiving block is not exceeded and that
  // the number of elements found are used in kthLargest.
  // Note it also makes sense to stop the iteration when we found all
  // elements. It may save a few reads from the lattice.
  Block<T> tmp1(ntodo1);
  Block<T> tmp2(ntodo2);
  T* tmpPtr1 = tmp1.storage();
  T* tmpPtr2 = tmp2.storage();
  uInt ndone1 = 0;
  uInt ndone2 = 0;
  iter.reset();
  while (! iter.atEnd() && (ndone1<ntodo1 || ndone2<ntodo2)) {
    const Array<T>& array = iter.cursor();
    Bool delData;
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (ndone1 < ntodo1  &&  dataPtr[i] >= stv1  &&  dataPtr[i] < endv1) {
	tmpPtr1[ndone1++] = dataPtr[i];
      }
      if (ndone2 < ntodo2  &&  dataPtr[i] >= stv2  &&  dataPtr[i] < endv2) {
	tmpPtr2[ndone2++] = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    iter++;
  }
  // By rounding it is possible that not enough elements were found.
  if (leftInx >= ndone1) {
    result(0) = endv1;
  } else {
    result(0) = GenSort<T>::kthLargest (tmp1.storage(), ndone1, leftInx);
  }
  if (rightInx >= ndone2) {
    result(1) = endv2;
  } else {
    result(1) = GenSort<T>::kthLargest (tmp2.storage(), ndone2, rightInx);
  }
  return result;
}


template <class T>
Vector<T> LatticeFractile<T>::maskedFractiles (const MaskedLattice<T>& lattice,
					       Float left, Float right,
					       uInt smallSize)
{
  AlwaysAssert (left >= 0  &&  left <= right  &&  right <= 1, AipsError);
  // If unmasked, a simpler way can be used.
  if (! lattice.isMasked()) {
    return unmaskedFractiles (lattice, left, right, smallSize);
  }
  // Determine the number of elements in the lattice.
  // If small enough, we read them all and do it in memory.
  uInt ntodo1 = lattice.shape().product();
  if (ntodo1 <= smallSize) {
    return smallMaskedFractiles (lattice, left, right);
  }
  Vector<T> result(2);
  // Bad luck. We have to do some more work.
  // Do a first binning while determining min/max at the same time.
  // Hopefully the start and end values make some sense.
  // Make the block 1 element larger, because possible roundoff errors
  // could result in a binnr just beyond the end.
  const uInt nbins = 10000;
  Block<uInt> hist1(nbins+1, 0u);
  Block<T> boundaries1(nbins+1);
  T stv1, endv1, minv1, maxv1;
  ntodo1 = maskedHistogram (stv1, endv1, minv1, maxv1, hist1, boundaries1,
			    lattice);
  if (ntodo1 == 0) {
    return Vector<T>();
  }
  // Find which elements are left and right fractile.
  uInt leftInx = uInt (left * (ntodo1-1));
  uInt rightInx = uInt (right * (ntodo1-1));
  // Init variables for both fractiles.
  uInt ntodo2 = ntodo1;
  T stv2 = stv1;
  T endv2 = endv1;
  T minv2 = minv1;
  T maxv2 = maxv1;
  Block<uInt> hist2 (hist1);
  Block<T> boundaries2 (boundaries1);
  Bool finished1 = False;
  Bool finished2 = False;
  // Iterate until the bins containing the fractiles do not
  // contain too many values anymore.
  COWPtr<Array<Bool> > mask;
  RO_MaskedLatticeIterator<T> iter(lattice);
  while (True) {
    // Determine which bin contains the requested values, determine
    // new boundaries, max/min and offset in bin.
    // Do that for left and right fractile.
    uInt ntodo = 0;
    if (!finished1) {
      ntodo1 = findBin (leftInx,  stv1, endv1, minv1, maxv1,
			hist1, boundaries1);
      // If only a 'few' more points to do, stop making histograms.
      // Otherwise histogram the fractile bin with a much smaller bin size.
      if (ntodo1 <= smallSize) {
	finished1 = True;
	if (ntodo1 == 0) {
	  result(0) = endv1;
	}
      } else {
	ntodo += ntodo1;
      }
    }
    if (!finished2) {
      ntodo2 = findBin (rightInx, stv2, endv2, minv2, maxv2,
			hist2, boundaries2);
      if (ntodo2 <= smallSize) {
	finished2 = True;
	if (ntodo2 == 0) {
	  result(1) = endv2;
	}
      } else {
	ntodo += ntodo2;
      }
    }
    // Stop if both fractiles have small enough bins.
    if (finished1 && finished2) {
      break;
    }
    // Build new histograms with determined subsets
    minv1 = endv1;
    minv2 = endv2;
    maxv1 = stv1;
    maxv2 = stv2;
    hist1 = 0;
    hist2 = 0;
    T step1 = (endv1 - stv1) / nbins;
    T step2 = (endv2 - stv2) / nbins;
    for (uInt i=0; i<=nbins; i++) {
      boundaries1[i] = stv1 + i*step1;
      boundaries2[i] = stv2 + i*step2;
    }
    uInt ndone = 0;
    iter.reset();
    while (! iter.atEnd()  &&  ndone<ntodo) {
      Bool delData, delMask;
      const Array<T>& array = iter.cursor();
      iter.getMask (mask);
      const Bool* maskPtr = mask->getStorage (delMask);
      const T* dataPtr = array.getStorage (delData);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	if (maskPtr[i]) {
	  if (!finished1  &&  dataPtr[i] >= stv1  &&  dataPtr[i] < endv1) {
	    Int bin = Int((dataPtr[i] - stv1) / step1);
	    // Due to rounding the bin number might get one too low or high.
	    if (dataPtr[i] < boundaries1[bin]) {
	      bin--;
	    } else if (dataPtr[i] >= boundaries1[bin+1]) {
	      bin++;
	    }
	    hist1[bin]++;
	    if (dataPtr[i] < minv1) {
	      minv1 = dataPtr[i];
	    }
	    if (dataPtr[i] > maxv1) {
	      maxv1 = dataPtr[i];
	    }
	    ndone++;
	  }
	  if (!finished2  &&  dataPtr[i] >= stv2  &&  dataPtr[i] < endv2) {
	    Int bin = Int((dataPtr[i] - stv2) / step2);
	    // Due to rounding the bin number might get one too low or high.
	    if (dataPtr[i] < boundaries2[bin]) {
	      bin--;
	    } else if (dataPtr[i] >= boundaries2[bin+1]) {
	      bin++;
	    }
	    hist2[bin]++;
	    if (dataPtr[i] < minv2) {
	      minv2 = dataPtr[i];
	    }
	    if (dataPtr[i] > maxv2) {
	      maxv2 = dataPtr[i];
	    }
	    ndone++;
	  }
	}
      }
      array.freeStorage (dataPtr, delData);
      mask->freeStorage (maskPtr, delMask);
      iter++;
    }
    // In principle the last bins should be empty, but roundoff errors
    // might have put a few in there. So add them to previous one.
    hist1[nbins-1] += hist1[nbins];
    hist2[nbins-1] += hist2[nbins];
  }
  if (ntodo1 == 0  &&  ntodo2 == 0) {
    return result;
  }
  // There are only a 'few' points left in both histograms.
  // So read them all in and determine the Inx'th-largest.
  // Again, due to rounding we might find a few elements more or less.
  // So take care that the receiving block is not exceeded and that
  // the number of elements found are used in kthLargest.
  // Note it also makes sense to stop the iteration when we found all
  // elements. It may save a few reads from the lattice.
  Block<T> tmp1(ntodo1);
  Block<T> tmp2(ntodo2);
  T* tmpPtr1 = tmp1.storage();
  T* tmpPtr2 = tmp2.storage();
  uInt ndone1 = 0;
  uInt ndone2 = 0;
  iter.reset();
  while (! iter.atEnd() && (ndone1<ntodo1 || ndone2<ntodo2)) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    iter.getMask (mask);
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	if (ndone1 < ntodo1  &&  dataPtr[i] >= stv1  &&  dataPtr[i] < endv1) {
	  tmpPtr1[ndone1++] = dataPtr[i];
	}
	if (ndone2 < ntodo2  &&  dataPtr[i] >= stv2  &&  dataPtr[i] < endv2) {
	  tmpPtr2[ndone2++] = dataPtr[i];
	}
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  // By rounding it is possible that not enough elements were found.
  // In that case return the middle of the (very small) interval.
  if (leftInx >= ndone1) {
    result(0) = endv1;
  } else {
    result(0) = GenSort<T>::kthLargest (tmp1.storage(), ndone1, leftInx);
  }
  if (rightInx >= ndone2) {
    result(1) = endv2;
  } else {
    result(1) = GenSort<T>::kthLargest (tmp2.storage(), ndone2, rightInx);
  }
  return result;
}


template <class T>
Vector<T> LatticeFractile<T>::smallMaskedFractiles
                                       (const MaskedLattice<T>& lattice,
					Float left, Float right)
{
  // Make a buffer to hold all masked-on elements.
  // The number of values is not more than the number of elements in the
  // lattice, so make the buffer that long.
  uInt size = lattice.shape().product();
  Block<T> buffer(size);
  uInt npts = 0;
  // Iterate through the lattice and assemble all masked-on elements.
  COWPtr<Array<Bool> > mask;
  RO_MaskedLatticeIterator<T> iter(lattice);
  while (! iter.atEnd()) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    iter.getMask (mask);
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	buffer[npts++] = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  if (npts == 0) {
    return Vector<T>();
  }
  // Use median of an Array instead of kthLargest directly, because
  // for a small array with an even number of elements, median takes
  // the average of the 2 middle elements.
  uInt leftInx = uInt (left * (npts-1));
  uInt rightInx = uInt (right * (npts-1));
  Vector<T> result(2);
  result(0) = GenSort<T>::kthLargest (buffer.storage(), npts, leftInx);
  result(1) = GenSort<T>::kthLargest (buffer.storage(), npts, rightInx);
  return result;
}

} //# NAMESPACE CASACORE - END


#endif
