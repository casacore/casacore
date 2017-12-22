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

#ifndef SCIMATH_STATISTICSDATASET_H
#define SCIMATH_STATISTICSDATASET_H

#include <casa/aips.h>

namespace casacore {

// Representation of a statistics dataset used in statistics framework calculatations.
//
// This class is used internally by StatisticsAlgorithm and its derived classes.
// There should be no need for an API developer to make direct use of this class. It
// encapsulates the data-related portions of StatisticsAlgorithm and derived classes.
// To add and set data or to set a data provider, one should call the relevant methods
// in StatisticsAlgorithm which have been left unchanged for the convenience of the
// API developer. Those methods call the analogous methods in this class (and the
// methods in StatisticsAlgorithm also do necessary bookkeeping for the
// StatisticsAlgorithm and derived objects).

template <
    class AccumType, class DataIterator, class MaskIterator=const Bool *,
    class WeightsIterator=DataIterator
>
class StatisticsDataset {

public:

    StatisticsDataset();

    StatisticsDataset(const StatisticsDataset& other);

    ~StatisticsDataset();

    // use copy semantics, except for the data provider which uses reference semantics
    StatisticsDataset<CASA_STATP>& operator=(
        const StatisticsDataset<CASA_STATP>& other
    );

    // <group>
    // Add a dataset to an existing set of datasets on which statistics are
    // to be calculated. nr is the number of points to be considered.
    // If <src>dataStride</src> is greater than 1, when <src>nrAccountsForStride</src>=True indicates
    // that the stride has been taken into account in the value of <src>nr</src>. Otherwise, it has
    // not so that the actual number of points to include is nr/dataStride if nr % dataStride == 0 or
    // (int)(nr/dataStride) + 1 otherwise.
    // If one calls this method after a data provider has been set, an exception will be
    // thrown. In this case, one should call setData(), rather than addData(), to indicate
    // that the underlying data provider should be removed.
    // <src>dataRanges</src> provide the ranges of data to include if <src>isInclude</src> is True,
    // or ranges of data to exclude if <src>isInclude</src> is False. If a datum equals the end point
    // of a data range, it is considered good (included) if <src>isInclude</src> is True, and it is
    // considered bad (excluded) if <src>isInclude</src> is False.

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
        uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False, uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );
    // </group>

    // returns ! dataProvider && _data.empty()
    Bool empty() const;

    const std::vector<Int64>& getCounts() const { return _counts; }

    const std::vector<DataIterator>& getData() const { return _data; }

    StatsDataProvider<CASA_STATP>* getDataProvider() {
        return _dataProvider;
    }

    const StatsDataProvider<CASA_STATP>* getDataProvider() const {
        return _dataProvider;
    }

    const std::vector<uInt>& getDataStrides() const { return _dataStrides; }

    const std::map<uInt, Bool>& getIsIncludeRanges() const { return _isIncludeRanges; }

    const std::map<uInt, MaskIterator> getMasks() const { return _masks; }

    const std::map<uInt, uInt>& getMaskStrides() const { return _maskStrides; }

    const std::map<uInt, DataRanges>& getRanges() const { return _dataRanges; }

    const std::map<uInt, WeightsIterator>& getWeights() const {
        return _weights;
    }

    void reset();

    // <group>
    // setdata() clears any current datasets or data provider and then adds the specified data set as
    // the first dataset in the (possibly new) set of data sets for which statistics are
    // to be calculated. See addData() for parameter meanings.
    void setData(const DataIterator& first, uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False);

    void setData(
        const DataIterator& first, uInt nr,
        const DataRanges& dataRanges, Bool isInclude=True, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1,
        Bool nrAccountsForStride=False
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, uInt dataStride=1,
        Bool nrAccountsForStride=False,
        uInt maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uInt nr, const DataRanges& dataRanges,
        Bool isInclude=True, uInt dataStride=1, Bool nrAccountsForStride=False,
        uInt maskStride=1
    );
    // </group>

    // instead of settng and adding data "by hand", set the data provider that will provide
    // all the data sets. Calling this method will clear any other data sets that have
    // previously been set or added.
    void setDataProvider(StatsDataProvider<CASA_STATP> *dataProvider);

private:
    std::vector<DataIterator> _data;
    // maps data to weights
    std::map<uInt, WeightsIterator> _weights;
    // maps data to masks
    std::map<uInt, MaskIterator> _masks;
    std::vector<Int64> _counts;
    std::vector<uInt> _dataStrides;
    std::map<uInt, uInt> _maskStrides;
    std::map<uInt, Bool> _isIncludeRanges;
    std::map<uInt, DataRanges> _dataRanges;
    StatsDataProvider<CASA_STATP> *_dataProvider;

    void _throwIfDataProviderDefined() const;
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatisticsDataset.tcc>
#endif

#endif
