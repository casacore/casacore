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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#ifndef SCIMATH_STATSALGORITHMQUANTILECOMPUTER_TCC
#define SCIMATH_STATSALGORITHMQUANTILECOMPUTER_TCC

#include <casacore/scimath/StatsFramework/StatisticsAlgorithmQuantileComputer.h>

namespace casacore {

CASA_STATD StatisticsAlgorithmQuantileComputer<CASA_STATP>
::StatisticsAlgorithmQuantileComputer(StatisticsDataset<CASA_STATP>* ds)
  : _dataset(ds) {}

CASA_STATD StatisticsAlgorithmQuantileComputer<CASA_STATP>
::~StatisticsAlgorithmQuantileComputer() {}

// explicitly set _dataset to NULL, this needs to be
// set by the caller upon a copy
CASA_STATD StatisticsAlgorithmQuantileComputer<CASA_STATP>
::StatisticsAlgorithmQuantileComputer(
    const StatisticsAlgorithmQuantileComputer& other
) : _sortedArray(other._sortedArray), _dataset(nullptr),
    _median(other._median ? new AccumType(*other._median) : nullptr),
    _medAbsDevMed(
        other._medAbsDevMed ? new AccumType(*other._medAbsDevMed) : nullptr
    ) {}

CASA_STATD StatisticsAlgorithmQuantileComputer<CASA_STATP>&
StatisticsAlgorithmQuantileComputer<CASA_STATP>::operator=(
    const StatisticsAlgorithmQuantileComputer<CASA_STATP>& other
) {
     if (this == &other) {
         return *this;
     }
     _sortedArray = other._sortedArray;
     // explicitly set to NULL, this needs to be
     // set by the caller upon assignment
     _dataset = nullptr;
     _median.reset(other._median ? new AccumType(*other._median) : nullptr);
     _medAbsDevMed.reset(
         other._medAbsDevMed ? new AccumType(*other._medAbsDevMed) : nullptr
     );
     return *this;
}

CASA_STATD
void StatisticsAlgorithmQuantileComputer<CASA_STATP>::deleteSortedArray() {
    _sortedArray.clear();
}

CASA_STATD void StatisticsAlgorithmQuantileComputer<CASA_STATP>::reset() {
    _sortedArray.clear();
    _median.reset();
    _medAbsDevMed.reset();
}

CASA_STATD void StatisticsAlgorithmQuantileComputer<CASA_STATP>::setDataset(
    StatisticsDataset<CASA_STATP>* ds
) {
    _dataset = ds;
}

}

#endif
