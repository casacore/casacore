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

#include <casacore/casa/aips.h>

#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

namespace casacore {

template <class T> class PtrHolder;

// Representation of a statistics dataset used in statistics framework
// calculatations.
//
// This class is used internally by StatisticsAlgorithm and its derived classes.
// There should be no need for an API developer to make direct use of this
// class. It encapsulates the data-related portions of StatisticsAlgorithm and
// derived classes. To add and set data or to set a data provider, one should
// call the relevant methods in StatisticsAlgorithm which have been left
// unchanged for the convenience of the API developer. Those methods call the
// analogous methods in this class (and the methods in StatisticsAlgorithm also
// do necessary bookkeeping for the StatisticsAlgorithm and derived objects).

template <
    class AccumType, class DataIterator, class MaskIterator=const bool *,
    class WeightsIterator=DataIterator
>
class StatisticsDataset {

public:

    // holds information about a data chunk. A data chunk is either an
    // individual underlying dataset (if no data provider), or a chunk of data
    // served by the data provider if it exists.
    struct ChunkData {
        // start of data
        DataIterator data;
        // total number of points
        uint64_t count;
        // data stride
        uint32_t dataStride;
        // associated ranges. If nullptr, then there are none. If not, the
        // second member of the pair indicates if they are include ranges.
        PtrHolder<std::pair<DataRanges, bool>> ranges;
        // associated mask. If nullptr, then there is no mask.
        // If there is a mask, the second member is the mask stride.
        PtrHolder<std::pair<MaskIterator, uint32_t>> mask;
        // associated weights. If nullptr, then there are no weights.
        PtrHolder<WeightsIterator> weights;
    };

    StatisticsDataset();

    StatisticsDataset(const StatisticsDataset& other);

    ~StatisticsDataset();

    // use copy semantics, except for the data provider which uses reference
    // semantics
    StatisticsDataset<CASA_STATP>& operator=(
        const StatisticsDataset<CASA_STATP>& other
    );

    // <group>
    // Add a dataset to an existing set of datasets on which statistics are to
    // be calculated. nr is the number of points to be considered. If
    // <src>dataStride</src> is greater than 1, when
    // <src>nrAccountsForStride</src>=true indicates that the stride has been
    // taken into account in the value of <src>nr</src>. Otherwise, it has not
    // so that the actual number of points to include is nr/dataStride if
    // nr % dataStride == 0 or (int)(nr/dataStride) + 1 otherwise. If one calls
    // this method after a data provider has been set, an exception will be
    // thrown. In this case, one should call setData(), rather than addData(),
    // to indicate that the underlying data provider should be removed.
    // <src>dataRanges</src> provide the ranges of data to include if
    // <src>isInclude</src> is true, or ranges of data to exclude if
    // <src>isInclude</src> is false. If a datum equals the end point of a data
    // range, it is considered good (included) if <src>isInclude</src> is true,
    // and it is considered bad (excluded) if <src>isInclude</src> is false.

    void addData(
        const DataIterator& first, uint32_t nr, uint32_t dataStride=1,
        bool nrAccountsForStride=false
    );

    void addData(
        const DataIterator& first, uint32_t nr, const DataRanges& dataRanges,
        bool isInclude=true, uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void addData(
        const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
        uint32_t dataStride=1, bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void addData(
        const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
        const DataRanges& dataRanges, bool isInclude=true, uint32_t dataStride=1,
        bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uint32_t nr, uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uint32_t nr, const DataRanges& dataRanges, bool isInclude=true,
        uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uint32_t nr, uint32_t dataStride=1,
        bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void addData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uint32_t nr, const DataRanges& dataRanges,
        bool isInclude=true, uint32_t dataStride=1, bool nrAccountsForStride=false,
        uint32_t maskStride=1
    );
    // </group>

    // returns ! dataProvider && _data.empty()
    bool empty() const;

    // get data counts associated with the underlying data sets
    const std::vector<int64_t>& getCounts() const { return _counts; }

    StatsDataProvider<CASA_STATP>* getDataProvider() {
        return _dataProvider;
    }

    const StatsDataProvider<CASA_STATP>* getDataProvider() const {
        return _dataProvider;
    }

    int64_t iDataset() const { return _idataset; }

    bool increment(bool includeIDataset);

    void incrementThreadIters(
        DataIterator& dataIter, MaskIterator& maskIter,
        WeightsIterator& weightsIter, uint64_t& offset, uint32_t nthreads
    ) const;

    void initIterators();

    // used for threaded methods
    void initLoopVars(
        uint64_t& chunkCount, uint32_t& chunkStride, bool& chunkHasRanges,
        DataRanges& chunkRanges, bool& chunkIsIncludeRanges,
        bool& chunkHasMask, uint32_t& chunkMaskStride, bool& chunkHasWeights
    );

    // used for unthreaded methods
    void initLoopVars(
        DataIterator& chunkData, uint64_t& chunkCount, uint32_t& chunkStride,
        bool& chunkHasRanges, DataRanges& chunkRanges,
        bool& chunkIsIncludeRanges, bool& chunkHasMask, MaskIterator& chunkMask,
        uint32_t& chunkMaskStride, bool& chunkHasWeights,
        WeightsIterator& chunkWeights
    );

    const ChunkData& initLoopVars();

    void initThreadVars(
        uint32_t& nBlocks, uint64_t& extra, uint32_t& nthreads,
        std::unique_ptr<DataIterator[]>& dataIter, std::unique_ptr<MaskIterator[]>& maskIter,
        std::unique_ptr<WeightsIterator[]>& weightsIter, std::unique_ptr<uint64_t[]>& offset,
        uint32_t nThreadsMax
    ) const;

    void reset();

    void resetIDataset() { _idataset = 0; }

    // <group>
    // setdata() clears any current datasets or data provider and then adds the
    // specified data set as the first dataset in the (possibly new) set of data
    // sets for which statistics are to be calculated. See addData() for
    // parameter meanings.
    void setData(
        const DataIterator& first, uint32_t nr, uint32_t dataStride=1,
        bool nrAccountsForStride=false
    );

    void setData(
        const DataIterator& first, uint32_t nr, const DataRanges& dataRanges,
        bool isInclude=true, uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst, uint32_t nr,
        uint32_t dataStride=1, bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void setData(
        const DataIterator& first, const MaskIterator& maskFirst,
        uint32_t nr, const DataRanges& dataRanges, bool isInclude=true,
        uint32_t dataStride=1, bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uint32_t nr, uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        uint32_t nr, const DataRanges& dataRanges, bool isInclude=true,
        uint32_t dataStride=1, bool nrAccountsForStride=false
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uint32_t nr, uint32_t dataStride=1,
        bool nrAccountsForStride=false, uint32_t maskStride=1
    );

    void setData(
        const DataIterator& first, const WeightsIterator& weightFirst,
        const MaskIterator& maskFirst, uint32_t nr, const DataRanges& dataRanges,
        bool isInclude=true, uint32_t dataStride=1, bool nrAccountsForStride=false,
        uint32_t maskStride=1
    );
    // </group>

    // instead of setting and adding data "by hand", set the data provider that
    // will provide all the data sets. Calling this method will clear any other
    // data sets that have previously been set or added.
    void setDataProvider(StatsDataProvider<CASA_STATP> *dataProvider);

private:
    std::vector<DataIterator> _data{};
    // maps data to weights. maps are used rather than vectors because only some
    // (or none) of the data sets in the _data vector may have associated
    // weights, masks, and/or ranges.
    std::map<uint32_t, WeightsIterator> _weights{};
    // maps data to masks
    std::map<uint32_t, MaskIterator> _masks{};
    std::vector<int64_t> _counts{};
    std::vector<uint32_t> _dataStrides{};
    std::map<uint32_t, uint32_t> _maskStrides{};
    std::map<uint32_t, bool> _isIncludeRanges{};
    std::map<uint32_t, DataRanges> _dataRanges{};
    StatsDataProvider<CASA_STATP>* _dataProvider{nullptr};

    int64_t _idataset{0};
    typename std::vector<DataIterator>::const_iterator _dend{}, _diter{};
    std::vector<int64_t>::const_iterator _citer{};
    std::vector<uint32_t>::const_iterator _dsiter{};
    uint32_t _dataCount{0};
    ChunkData _chunk;

    void _throwIfDataProviderDefined() const;
};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/StatsFramework/StatisticsDataset.tcc>
#endif

#endif
