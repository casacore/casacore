//# Copyright (C) 2000,2001
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
//# $Id: Array.h 21545 2015-01-22 19:36:35Z gervandiepen $

#ifndef LATTICES_LATTICESTATSDATAPROVIDER_H
#define LATTICES_LATTICESTATSDATAPROVIDER_H

#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LatticeMath/LatticeStatsDataProviderBase.h>

#include <casacore/casa/aips.h>

namespace casacore {

// Data provider which allows stats framework to iterate through an unmasked lattice.

template <class T> class LatticeStatsDataProvider
	: public  LatticeStatsDataProviderBase<T> {

public:

	// default constructor, must set lattice after construction but before
	// using the object
	LatticeStatsDataProvider();

	// <src>iteratorLimitBytes</src> is related to the size of the lattice.
	// If the lattice is greater than this size, then a lattice iterator will
	// be used to step through the lattice. If less, then all the data in the
	// values in the lattice are retrieved in a single chunk. The advantage of
	// the iterator is that less memory is used. The disadvantage is there is
	// a significant performace cost, so if the lattice is small, it is better to
	// get all its values in a single chunk and forgo the iterator. This is particularly
	// true when looping for a large number of iterations and creating a
	// LatticeStatsDataProvider each loop (in that case, you probably will want
	// to create a single object before the loop and use setLattice() to update
	// its lattice).
	LatticeStatsDataProvider(
		const Lattice<T>& lattice, uInt iteratorLimitBytes=4096*4096
	);

	~LatticeStatsDataProvider();

	void operator++();

	// estimated number of steps to iterate through the the lattice
	uInt estimatedSteps() const;

	// Are there any data sets left to provide?
	Bool atEnd() const;

	// Take any actions necessary to finalize the provider. This will be called when
	// atEnd() returns True.
	void finalize();

	// get the count of elements in the current data set. When implementing this method, be
	// certain to take stride into account; ie for a data set with nominally 100 elements that
	// is to have a stride of two, this method should return 50.
	uInt64 getCount();

	// get the current data set
	const T* getData();

	// Get the associated mask of the current dataset. Only called if hasMask() returns True;
	const Bool* getMask();

	// Does the current data set have an associated mask?
	Bool hasMask() const;

	// reset the provider to point to the first data set it manages.
	void reset();

	// set the lattice. Automatically resets the lattice iterator
	// <src>iteratorLimitBytes</src> is related to the size of the lattice.
	// If the lattice is greater than this size, then a lattice iterator will
	// be used to step through the lattice. If less, then all the data in the
	// values in the lattice are retrieved in a single chunk. The advantage of
	// the iterator is that less memory is used. The disadvantage is there is
	// a significant performace cost, so if the lattice is small, it is better to
	// get all its values in a single chunk and forgo the iterator. This is particularly
	// true when looping for a large number of iterations and creating a
	// LatticeStatsDataProvider each loop (in that case, you probably will want
	// to create a single object before the loop and use setLattice() to update
	// its lattice).
	void setLattice(
		const Lattice<T>& lattice, uInt iteratorLimitBytes=4096*4096
	);

	// <group>
	// see base class documentation.
	void updateMaxPos(const std::pair<Int64, Int64>& maxpos);

	void updateMinPos(const std::pair<Int64, Int64>& minpos);
	// </group>

private:
	CountedPtr<RO_LatticeIterator<T> > _iter;
	Array<T> _currentSlice;
	const T* _currentPtr;
	Bool _delData, _atEnd;

	void _freeStorage();

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
