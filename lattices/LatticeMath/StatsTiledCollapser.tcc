//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004
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

#include <casacore/lattices/LatticeMath/StatsTiledCollapser.h>

#include <casacore/scimath/StatsFramework/StatisticsUtilities.h>
#include <casacore/casa/BasicMath/ConvertScalar.h>

namespace casacore {

template <class T, class U>
StatsTiledCollapser<T,U>::StatsTiledCollapser(
    const Vector<T>& pixelRange,
    bool noInclude, bool noExclude,
    bool fixedMinMax
) : _range(pixelRange), _include(! noInclude),
    _exclude(! noExclude), _fixedMinMax(fixedMinMax),
    _isReal(isReal(whatType<T>())),
    _minpos(0), _maxpos(0) {}

template <class T, class U>
void StatsTiledCollapser<T,U>::init (uint32_t nOutPixelsPerCollapse) {
    AlwaysAssert (nOutPixelsPerCollapse == LatticeStatsBase::NACCUM, AipsError);
}

template <class T, class U>
void StatsTiledCollapser<T,U>::initAccumulator (uint64_t n1, uint64_t n3) {
   _sum = new Block<U>(n1*n3);
   _sumSq = new Block<U>(n1*n3);
   _npts = new Block<double>(n1*n3);
   _mean = new Block<U>(n1*n3);
   _variance = new Block<U>(n1*n3);
   _sigma = new Block<U>(n1*n3);
   _nvariance = new Block<U>(n1*n3);

   _min = new Block<T>(n1*n3);
   _max = new Block<T>(n1*n3);
   _initMinMax = new Block<bool>(n1*n3);
   _sum->set(0);
   _sumSq->set(0);
   _npts->set(0);
   _mean->set(0);
   _variance->set(0);
   _sigma->set(0);
   _nvariance->set(0);

   _min->set(0);
   _max->set(0);
   _initMinMax->set(true);
   _n1 = n1;
   _n3 = n3;
}

template <class T, class U>
void StatsTiledCollapser<T,U>::process (
    uint32_t index1, uint32_t index3,
    const T* pInData, const bool* pInMask,
    uint32_t dataIncr, uint32_t maskIncr,
    uint32_t nrval, const IPosition& startPos,
    const IPosition& shape
) {
    // Process the data in the current chunk.   Everything in this
    // chunk belongs in one output location in the storage
    // lattices
    uint64_t index = index1 + index3*_n1;
    U& sum = (*_sum)[index];
    U& sumSq = (*_sumSq)[index];
    double& nPts = (*_npts)[index];
    T& dataMin = (*_min)[index];
    T& dataMax = (*_max)[index];
    U& mean = (*_mean)[index];
    U& variance = (*_variance)[index];
    U& sigma = (*_sigma)[index];
    U& nvariance = (*_nvariance)[index];

    // If these are != -1 after the accumulating, then
    // the min and max were updated
    int64_t minLoc = -1;
    int64_t maxLoc = -1;

    std::vector<std::pair<U, U> > ranges;
    bool isInclude = false;
    bool hasRange = _include || _exclude;
    if (hasRange) {
        ranges.resize(1);
        ranges[0] = std::make_pair(_range[0], _range[1]);
        isInclude = _include;
    }
    typename vector<std::pair<U, U> >::const_iterator beginRange = ranges.begin();
    typename vector<std::pair<U, U> >::const_iterator endRange = ranges.end();
    int64_t i = 0;
    if (pInMask == 0) {
        // All pixels are unmasked
        if (hasRange) {
            for (i=0; i<(int64_t)nrval; ++i) {
                if (
                    StatisticsUtilities<U>::includeDatum(
                        *pInData, beginRange, endRange, isInclude
                    )
                ) {
                    StatisticsUtilities<U>::accumulate(
                        nPts, sum, mean, nvariance,
                        sumSq, dataMin, dataMax, minLoc,
                        maxLoc, *pInData, i
                    );
                }
                pInData += dataIncr;
            }
            if (_include && _fixedMinMax) {
                dataMin = _range(0);
                dataMax = _range(1);
            }
        }
        else {
            // no range
            for (int64_t i=0; i<(int64_t)nrval; ++i) {
                StatisticsUtilities<U>::accumulate(
                    nPts, sum, mean, nvariance,
                    sumSq, dataMin, dataMax, minLoc,
                    maxLoc, *pInData, i
                );
                pInData += dataIncr;
            }
        }
    }
    else {
        // Some pixels are masked
        if (hasRange) {
            for (i=0; i<(int64_t)nrval; ++i) {
                if (
                    *pInMask && StatisticsUtilities<U>::includeDatum(
                        *pInData, beginRange, endRange, isInclude
                    )
                ) {
                    StatisticsUtilities<U>::accumulate(
                        nPts, sum, mean, nvariance,
                        sumSq, dataMin, dataMax, minLoc,
                        maxLoc, *pInData, i
                    );
                }
                pInData += dataIncr;
                pInMask += maskIncr;
            }
            if (_include && _fixedMinMax) {
                dataMin = _range(0);
                dataMax = _range(1);
            }
        }
        else {
            // no ranges
            for (i=0; i<(int64_t)nrval; ++i) {
                if (*pInMask) {
                    StatisticsUtilities<U>::accumulate(
                        nPts, sum, mean, nvariance,
                        sumSq, dataMin, dataMax, minLoc,
                        maxLoc, *pInData, i
                    );
                }
                pInData += dataIncr;
                pInMask += maskIncr;
            }
        }
    }
    variance = nPts > 1 ? nvariance/(nPts - 1) : 0;
    sigma = sqrt(variance);

    // Update overall min and max location.  These are never updated
    // if fixedMinMax is true.  These values are only meaningful for
    // float images.  For Complex they are useless currently.

    if (_isReal) {
        if (minLoc != -1) {
            _minpos = startPos + toIPositionInArray(minLoc, shape);
        }
        if (maxLoc != -1) {
            _maxpos = startPos + toIPositionInArray(maxLoc, shape);
        }
    }
}

template <class T, class U>
void StatsTiledCollapser<T,U>::endAccumulator(
    Array<U>& result, Array<bool>& resultMask,
    const IPosition& shape
) {
    // Reshape arrays.  The mask is always true.  Any locations
    // in the storage lattice for which there were no valid points
    // will have the NPTS field set to zero.  That is what
    // we use to effectively mask it.
    result.resize(shape);
    result.set(U(0));
    resultMask.resize(shape);
    resultMask.set(true);

    bool deleteRes;
    U* res = result.getStorage (deleteRes);
    U* resptr = res;
    U* sumPtr = _sum->storage();
    U* sumSqPtr = _sumSq->storage();
    CountedPtr<Block<DComplex> > nptsComplex;
    if (! isReal(whatType<U>())) {
        nptsComplex = new Block<DComplex>(_n1*_n3);
    }
    U* nPtsPtr;
    _convertNPts(nPtsPtr, _npts, nptsComplex);
    U* meanPtr = _mean->storage();
    U* variancePtr = _variance->storage();
    U* sigmaPtr = _sigma->storage();
    const T* minPtr = _min->storage();
    const T* maxPtr = _max->storage();
    uint64_t i, j;
    U* resptr_root = resptr;
    for (i=0; i<_n3; ++i) {
       resptr = resptr_root + (int32_t(LatticeStatsBase::NPTS) * _n1);
       objcopy (resptr, nPtsPtr, _n1);
       nPtsPtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::SUM) * _n1);
       objcopy (resptr, sumPtr, _n1);
       sumPtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::SUMSQ) * _n1);
       objcopy (resptr, sumSqPtr, _n1);
       sumSqPtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::MEAN) * _n1);
       objcopy (resptr, meanPtr, _n1);
       meanPtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::VARIANCE) * _n1);
       objcopy (resptr, variancePtr, _n1);
       variancePtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::SIGMA) * _n1);
       objcopy (resptr, sigmaPtr, _n1);
       sigmaPtr += _n1;

       resptr = resptr_root + (int32_t(LatticeStatsBase::MIN) * _n1);
       for (j=0; j<_n1; ++j) {
          convertScalar (*resptr++, *minPtr++);
       }

       resptr = resptr_root + (int32_t(LatticeStatsBase::MAX) * _n1);
       for (j=0; j<_n1; ++j) {
          convertScalar (*resptr++, *maxPtr++);
       }

       resptr_root += _n1 * int32_t(LatticeStatsBase::NACCUM);
    }
    result.putStorage (res, deleteRes);
}

template <class T, class U>
void StatsTiledCollapser<T,U>::_convertNPts(
    double*& nptsPtr, CountedPtr<Block<double> > npts,
    CountedPtr<Block<DComplex> >
) const {
    nptsPtr = npts->storage();
}

template <class T, class U>
void StatsTiledCollapser<T,U>::_convertNPts(
    DComplex*& nptsPtr, CountedPtr<Block<double> > npts,
    CountedPtr<Block<DComplex> > nptsComplex
) const {
    DComplex* storage = nptsComplex->storage();
    double* realStorage = npts->storage();
    for (uint64_t i=0; i<_n1*_n3; ++i) {
         ///C++11 storage[i].real(realStorage[i]);
         ///C++11 storage[i].imag(0);
        storage[i] = DComplex(realStorage[i], 0);
    }
    nptsPtr = storage;
}

template <class T, class U>
void StatsTiledCollapser<T,U>::minMaxPos(IPosition& minPos, IPosition& maxPos)
{
   minPos.resize(_minpos.nelements());
   minPos = _minpos;
   maxPos.resize(_maxpos.nelements());
   maxPos = _maxpos;
}

}

