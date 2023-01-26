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

#ifndef SCIMATH_STATSALGORITHMQUANTILECOMPUTER_H
#define SCIMATH_STATSALGORITHMQUANTILECOMPUTER_H

#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

#include <casacore/casa/aips.h>

namespace casacore {

// This is the virtual base class from which concrete QuantileComputer classes
// are derived. The API developer should never explicitly instantiate a
// QuantileComputer class; they are used internally by other StatsFramework
// classes. See the documentation of StatisticsAlgorithm for more details.

template <
    class AccumType, class DataIterator, class MaskIterator=const bool *,
    class WeightsIterator=DataIterator
>
class StatisticsAlgorithmQuantileComputer {

public:

    StatisticsAlgorithmQuantileComputer() = delete;

    virtual ~StatisticsAlgorithmQuantileComputer();

    // clone this object by returning a pointer to a copy
    virtual StatisticsAlgorithmQuantileComputer<CASA_STATP>* clone() const = 0;

    // delete any (partially) sorted array
    void deleteSortedArray();

    // reset this object by clearing data.
    virtual void reset();

    // This must be called upon the copy or assignment of the
    // associated statistics algorithm object. Otherwise, there is generally
    // no reason to call it.
    void setDataset(StatisticsDataset<CASA_STATP>* ds);

    // FIXME make protected once refactor is complete
    std::vector<AccumType>& _getSortedArray() { return _sortedArray; }

    // FIXME make protected once refactor is complete
    void _setSortedArray(const std::vector<AccumType>& v) { _sortedArray = v; }

    void setMedian(CountedPtr<AccumType> median) { _median = median; }

protected:

    // ds should be the dataset object held in the StatisticsAlgorithm object.
    // The QuantileComputer calculator object should never hold its own version
    // of a dataset object. The algorithm object (caller of this method) is
    // always responsible for deleting the passed object, usually upon its
    // destruction.
    StatisticsAlgorithmQuantileComputer(StatisticsDataset<CASA_STATP>* ds);

    // use copy semantics. statistics algorithm object's responsibility to set
    // the _dataset object in the new QuantileComputer calculator object upon a
    // copy. The underlying _dataset object in the new stats calculator object
    // should be a reference to the new _dataset object in the copied statistics
    // algorithm object.
    StatisticsAlgorithmQuantileComputer(
        const StatisticsAlgorithmQuantileComputer& other
    );

    // use copy semantics. The _dataset object is not copied. It is the
    // associated statistics algorithm object's responsibility to set the
    // _dataset object in the new QuantileComputer calculator object upon an
    // assignment. The underlying _dataset object in the new stats calculator
    // object should be a reference to that in the newly assigned statistics
    // algorithm object.
    StatisticsAlgorithmQuantileComputer& operator=(
        const StatisticsAlgorithmQuantileComputer& other
    );

    StatisticsDataset<CASA_STATP>* _getDataset() { return _dataset; }

    CountedPtr<AccumType> _getMedian() const { return _median; }

    CountedPtr<AccumType> _getMedianAbsDevMedian() const {
        return _medAbsDevMed;
    }

    void _setMedianAbsDevMedian(CountedPtr<AccumType> medAbsDevMed) {
        _medAbsDevMed = medAbsDevMed;
    }

private:
    std::vector<AccumType> _sortedArray{};
    // This pointer references the (non-pointer) object
    // in the associated non-QuantileComputer computer object,
    // so this should not be wrapped in a smart pointer.
    StatisticsDataset<CASA_STATP>* _dataset{nullptr};
    CountedPtr<AccumType> _median{}, _medAbsDevMed{};

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatisticsAlgorithmQuantileComputer.tcc>
#endif

#endif
