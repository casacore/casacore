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

#ifndef SCIMATH_CHAUVENETCRITERIONSTATISTICS_H
#define SCIMATH_CHAUVENETCRITERIONSTATISTICS_H

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/ConstrainedRangeStatistics.h>

#include <set>
#include <utility>

namespace casacore {

// Class to calculate statistics using the so-called Chauvenet criterion. This method
// iteratively calculates statistics by discarding outliers on the basis of Chauvenet's
// criterion, until the specified maximum number of iterations is reached, or the final
// iteration results in no additional points being discarded.
// Alternatively, one can specify a z-score which indicates the number of standard deviations
// beyond which to discard points, which is held fixed while iterating.
//
// When instantiated, objects of this class use a ConstrainedRangeQuantileComputer
// object for computing quantile-like statistics. See class documentation for
// StatisticsAlgorithm for details of QuantileComputer classes.

template <class AccumType, class DataIterator, class MaskIterator=const Bool*, class WeightsIterator=DataIterator>
class ChauvenetCriterionStatistics
    : public ConstrainedRangeStatistics<CASA_STATP> {
public:

    // If <src>zscore</src> is not negative, use that value to discard outliers beyond
    // zscore standard deviations from the mean, and compute statistics based on the
    // remaining data. If <src>zscore</src> is negative, use Chauvenet's Criterion to
    // determine which outliers to discard. <src>maxIterations</src> is the maximum
    // number of iterations to use before stopping. If negative, continue iterating until the
    // set zscore or Chauvenet's criterion is met (ie that there are no remaining outliers).
    ChauvenetCriterionStatistics(Double zscore=-1, Int maxIterations=0);

    ChauvenetCriterionStatistics(const ChauvenetCriterionStatistics<CASA_STATP>& other);

    virtual ~ChauvenetCriterionStatistics();

    // copy semantics
    ChauvenetCriterionStatistics<CASA_STATP>& operator=(
        const ChauvenetCriterionStatistics<CASA_STATP>& other
    );

    // Clone this instance
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const;
 
    // get the algorithm that this object uses for computing stats
    virtual StatisticsData::ALGORITHM algorithm() const {
        return StatisticsData::CHAUVENETCRITERION;
    };

    // reset object to initial state. Clears all private fields including data,
    // accumulators, global range. It does not affect the fence factor (_f), which was
    // set at object construction.
    virtual void reset();

    // This class does not allow statistics to be calculated as datasets are added, so
    // an exception will be thrown if <src>c</src> is True.
    void setCalculateAsAdded(Bool c);

    // get the number of iterations
    uInt getNiter() const { return _niter; }

private:

    Double _zscore;
    Int _maxIterations;
    Bool _rangeIsSet;
    uInt _niter;

    void _setRange();
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/ChauvenetCriterionStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
