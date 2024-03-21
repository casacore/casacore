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

#ifndef SCIMATH_STATISTICSALGORITHM_H
#define SCIMATH_STATISTICSALGORITHM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/StatsFramework/StatsDataProvider.h>
#include <casacore/scimath/StatsFramework/StatisticsData.h>
#include <casacore/scimath/StatsFramework/StatisticsDataset.h>
#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

#include <map>
#include <set>
#include <vector>
#include <memory>

namespace casacore {

// Base class of statistics algorithm class hierarchy.

// The default implementation is such that statistics are only calculated when
// methods that actually compute statistics are called. Until then, the
// iterators which point to the beginning of data sets, masks, etc. are held in
// memory. Thus, the caller must keep all data sets available for the statistics
// object until these methods are called, and of course, if the actual data
// values are changed between adding data and calculating statistics, the
// updated values are used when calculating statistics. Derived classes may
// override this behavior.
//
// PRECISION CONSIDERATIONS
// Many statistics are computed via accumulators. This can lead to precision
// issues, especially for large datasets. For this reason, it is highly
// recommended that the data type one uses as the AccumType be of higher
// precision, if possible, than the data type pointed to by input iterator. So
// for example, if one has a data set of Float values (to which the
// InputIterator type points to), then one should use type Double for the
// AccumType. In this case, the Float data values will be converted to Doubles
// before they are accumulated.
//
// METHODS OF PROVIDING DATA
// Data may be provided in one of two mutually exclusive ways. The first way is
// simpler, and that is to use the setData()/addData() methods. Calling
// setData() will clear any previous data that was added via these methods or
// via a data provider (see below). Calling addData() after having called
// setData() will add a data set to the set of data sets on which statistics
// will be calculated. In order for this to work correctly, the iterators which
// are passed into these methods must still be valid when statistics are
// calculated (although note that some derived classes allow certain statistics
// to be updated as data sets are added via these methods. See specific classes
// for details).
//
// The second way to provide data is via an object derived from class
// StatsDataProvider, in which methods are implemented for retrieving various
// information about the data sets to be included. Such an interface is
// necessary for data structures which do not easily lend themselves to be
// provided via the setData()/addData() methods. For example, in the case of
// iterating through a Lattice, a lattice iterator will overwrite the memory
// location of the previous chunk of data with the current chunk of data.
// Therefore, if one does not wish to load data from the entire lattice into
// memory (which is why LatticeIterator was designed to have the behavior it
// does), one must use the LatticeStatsDataProvider class, which the statistics
// framework will use to iterate through the lattice, only keeping one chunk of
// the data of the lattice in memory any given moment.
//
// STORAGE OF DATA
// In order to reduce maintenance costs, the accounting details of the data sets
// are maintained in a StatisticsDataset object. This object is held in memory
// at the StatisticsAlgorithm level in the _dataset private field of this class
// when a derived class is instantiated. A StatisticsDataset object should never
// need to be explicitly instantiated by an API developer.
//
// QUANTILES
// A quantile is a value contained in a data set, such that, it has a zero-based
// index of ceil(q*n)-1 in the equivalent ordered dataset, where 0 < q < 1
// specifies the fractional location within the ordered dataset and n is the
// total number of valid elements. Note that, for a dataset with an odd number
// of elements, the median is the same as the quantile value when q = 0.5.
// However, there is no such correspondence between the median in a dataset with
// an even number of elements, since the median in that case is given by the
// mean of the elements of zero-based indices n/2-1 and n/2 in the equivalent
// ordered dataset. Thus, in the case of a dataset with an even number of
// values, the median may not even exist in the dataset, while a generic
// quantile value must exist in the dataset by definition. Note when calculating
// quantile values, a dataset that does not fall in specified dataset ranges,
// is not included via a stride specification, is masked, or has a weight of
// zero, is not considered a member of the dataset for the purposes of quantile
// calculations.
//
// CLASS ORGANIZATION
// In general, in the StatsFramework class hierarchy, classes derived from
// StatisticsAlgorithm and its descendants contain methods which calculate the
// relevant statistics which are computed via accumulation. These classes also
// contain the top level methods for computing the quantile-like statistics, for
// the convenience of the API developer. Derived classes of StatisticsAlgorithm
// normally will have a private field which is an object that contains methods
// which compute the various quantile-like statistics. These so-called
// QuantileComputer classes have been created to reduce maintainability costs;
// because putting all the code into single class files was becoming unwieldy.
// The concrete QuantileComputer classes are ultimately derived from
// StatisticsAlgorithmQuantileComputer, which is the virtual base class of this
// hierarchy. StatisticsAlgorithm objects do not contain a
// StatisticsAlgorithmQuantileComputer private field, since StatisticsAlgorithm
// is also a virtual base class and hence no actual statistics are computed
// within it. The design is such that the only classes an API developer should
// over instantiate are the derived classes of StatisticsAlgorithm; the
// QuantileComputer classes should never be explicitly instantiated in code
// which uses the StatsFramework API.

template <
    class AccumType, class DataIterator, class MaskIterator=const Bool *,
    class WeightsIterator=DataIterator
>
class StatisticsAlgorithm {

public:

    virtual ~StatisticsAlgorithm();

    // Clone this instance
    virtual StatisticsAlgorithm<CASA_STATP>* clone() const = 0;

    // <group>
    // Add a dataset to an existing set of datasets on which statistics are to
    // be calculated. nr is the number of points to be considered. If
    // <src>dataStride</src> is greater than 1, when
    // <src>nrAccountsForStride</src>=True indicates that the stride has been
    // taken into account in the value of <src>nr</src>. Otherwise, it has not
    // so that the actual number of points to include is nr/dataStride if
    // nr % dataStride == 0 or (int)(nr/dataStride) + 1 otherwise. if one calls
    // this method after a data provider has been set, an exception will be
    // thrown. In this case, one should call setData(), rather than addData(),
    // to indicate that the underlying data provider should be removed.
    // <src>dataRanges</src> provide the ranges of data to include if
    // <src>isInclude</src> is True, or ranges of data to exclude if
    // <src>isInclude</src> is False. If a datum equals the end point of a data
    // range, it is considered good (included) if <src>isInclude</src> is True,
    // and it is considered bad (excluded) if <src>isInclude</src> is False.

    void addData(
        const DataIterator& first, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, uInt nr,
        const DataRanges& dataRanges, Bool isInclude=True, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, const DataRanges& dataRanges, Bool isInclude=True,
        uInt dataStride=1, Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, const DataRanges& dataRanges, Bool isInclude=True,
        uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );
    // </group>

    // get the algorithm that this object uses for computing stats
    virtual StatisticsData::ALGORITHM algorithm() const = 0;

    virtual AccumType getMedian(
        std::shared_ptr<uInt64> knownNpts=nullptr,
        std::shared_ptr<AccumType> knownMin=nullptr,
        std::shared_ptr<AccumType> knownMax=nullptr,
        uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    ) = 0;

    // The return value is the median; the quantiles are returned in the
    // <src>quantileToValue</src> map.
    virtual AccumType getMedianAndQuantiles(
        std::map<Double, AccumType>& quantileToValue,
        const std::set<Double>& quantiles,
        std::shared_ptr<uInt64> knownNpts=nullptr,
        std::shared_ptr<AccumType> knownMin=nullptr,
        std::shared_ptr<AccumType> knownMax=nullptr,
        uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    ) = 0;

    // get the median of the absolute deviation about the median of the data.
    virtual AccumType getMedianAbsDevMed(
        std::shared_ptr<uInt64> knownNpts=nullptr,
        std::shared_ptr<AccumType> knownMin=nullptr,
        std::shared_ptr<AccumType> knownMax=nullptr,
        uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    ) = 0;

    // Purposefully not virtual. Derived classes should not implement.
    AccumType getQuantile(
        Double quantile, std::shared_ptr<uInt64> knownNpts=nullptr,
        std::shared_ptr<AccumType> knownMin=nullptr,
        std::shared_ptr<AccumType> knownMax=nullptr,
        uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    );

    // get a map of quantiles to values.
    virtual std::map<Double, AccumType> getQuantiles(
        const std::set<Double>& quantiles, std::shared_ptr<uInt64> npts=nullptr,
        std::shared_ptr<AccumType> min=nullptr, std::shared_ptr<AccumType> max=nullptr,
        uInt binningThreshholdSizeBytes=4096*4096,
        Bool persistSortedArray=False, uInt nBins=10000
    ) = 0;

    // get the value of the specified statistic. Purposefully not virtual.
    // Derived classes should not implement.
    AccumType getStatistic(StatisticsData::STATS stat);

    // certain statistics such as max and min have locations in the dataset
    // associated with them. This method gets those locations. The first value
    // in the returned pair is the zero-based dataset number that was set or
    // added. The second value is the zero-based index in that dataset. A data
    // stride of greater than one is not accounted for, so the index represents
    // the actual location in the data set, independent of the dataStride value.
    virtual LocationType getStatisticIndex(StatisticsData::STATS stat) = 0;

    // Return statistics. Purposefully not virtual. Derived classes should not
    // implement.
    StatsData<AccumType> getStatistics();

    // reset this object by clearing data.
    virtual void reset();

    // <group>
    // setdata() clears any current datasets or data provider and then adds the
    // specified data set as the first dataset in the (possibly new) set of data
    // sets for which statistics are to be calculated. See addData() for
    // parameter meanings. These methods are purposefully not virtual. Derived
    // classes should not implement.
    void setData(
        const DataIterator& first, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst, uInt nr,
        uInt dataStride=1, Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, const DataRanges& dataRanges, Bool isInclude=True,
        uInt dataStride=1, Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst, uInt nr,
        uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst, uInt nr,
        const DataRanges& dataRanges, Bool isInclude=True, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );
    // </group>

    // instead of setting and adding data "by hand", set the data provider
    // that will provide all the data sets. Calling this method will clear
    // any other data sets that have previously been set or added. Method
    // is virtual to allow derived classes to carry out any necessary
    // specialized accounting when resetting the data provider.
    virtual void setDataProvider(StatsDataProvider<CASA_STATP> *dataProvider);

    // Provide guidance to algorithms by specifying a priori which statistics
    // the caller would like calculated.
    virtual void setStatsToCalculate(std::set<StatisticsData::STATS>& stats);

protected:
    StatisticsAlgorithm();

    // use copy semantics, except for the data provider which uses reference
    // semantics
    StatisticsAlgorithm(const StatisticsAlgorithm& other);

    // use copy semantics, except for the data provider which uses reference
    // semantics
    StatisticsAlgorithm& operator=(const StatisticsAlgorithm& other);

    // Allows derived classes to do things after data is set or added.
    // Default implementation does nothing.
    virtual void _addData() {}

    // <group>
    // These methods are purposefully not virtual. Derived classes should
    // not implement.
    const StatisticsDataset<CASA_STATP>& _getDataset() const {
        return _dataset;
    }
    
    StatisticsDataset<CASA_STATP>& _getDataset() { return _dataset; }
    // </group>

    virtual AccumType _getStatistic(StatisticsData::STATS stat) = 0;

    virtual StatsData<AccumType> _getStatistics() = 0;

    const std::set<StatisticsData::STATS> _getStatsToCalculate() const {
        return _statsToCalculate;
    }

    virtual const std::set<StatisticsData::STATS>&
    _getUnsupportedStatistics() const {
        return _unsupportedStats;
    }

    // Derived classes should normally call this in their constructors, if
    // applicable.
    void _setUnsupportedStatistics(
        const std::set<StatisticsData::STATS>& stats
    ) {
        _unsupportedStats = stats;
    }

private:
    std::set<StatisticsData::STATS> _statsToCalculate{}, _unsupportedStats{};
    StatisticsDataset<CASA_STATP> _dataset{};
    Bool _resetDataset{True};

    void _resetExceptDataset();

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatisticsAlgorithm.tcc>
#endif

#endif
