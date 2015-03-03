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
//#
//# $Id: LatticeStatistics.tcc 20652 2009-07-06 05:04:32Z Malte.Marquarding $

#include <casacore/lattices/LatticeMath/StatsTiledCollapser.h>

namespace casacore {

template <class T, class U>
StatsTiledCollapser<T,U>::StatsTiledCollapser(
	const Vector<T>& pixelRange,
    Bool noInclude, Bool noExclude,
    Bool fixedMinMax
) : _range(pixelRange), _include(! noInclude),
	_exclude(! noExclude), _fixedMinMax(fixedMinMax),
	_isReal(isReal(whatType(&*(CountedPtr<T>(new T(0)))))),
	_minpos(0), _maxpos(0) {}

template <class T, class U>
void StatsTiledCollapser<T,U>::init (uInt nOutPixelsPerCollapse) {
    AlwaysAssert (nOutPixelsPerCollapse == LatticeStatsBase::NACCUM, AipsError);
}

template <class T, class U>
void StatsTiledCollapser<T,U>::initAccumulator (uInt n1, uInt n3) {
   _sum = new Block<U>(n1*n3);
   _sumSq = new Block<U>(n1*n3);
   _npts = new Block<U>(n1*n3);
   _mean = new Block<U>(n1*n3);
   _variance = new Block<U>(n1*n3);
   _nvariance = new Block<U>(n1*n3);

   _min = new Block<T>(n1*n3);
   _max = new Block<T>(n1*n3);
   _initMinMax = new Block<Bool>(n1*n3);
   _sum->set(0);
   _sumSq->set(0);
   _npts->set(0);
   _mean->set(0);
   _variance->set(0);
   _nvariance->set(0);

   _min->set(0);
   _max->set(0);
   _initMinMax->set(True);
   _n1 = n1;
   _n3 = n3;
}

template <class T, class U>
void StatsTiledCollapser<T,U>::process (
	uInt index1, uInt index3,
	const T* pInData, const Bool* pInMask,
	uInt dataIncr, uInt maskIncr,
	uInt nrval, const IPosition& startPos,
	const IPosition& shape
) {
	// Process the data in the current chunk.   Everything in this
	// chunk belongs in one output location in the storage
	// lattices
	uInt index = index1 + index3*_n1;
	U& sum = (*_sum)[index];
	U& sumSq = (*_sumSq)[index];
	U& nPts = (*_npts)[index];
	T& dataMin = (*_min)[index];
	T& dataMax = (*_max)[index];
	U& mean = (*_mean)[index];
	U& variance = (*_variance)[index];
	U& nvariance = (*_nvariance)[index];
	Bool& minMaxInit = (*_initMinMax)[index];

	// If these are != -1 after the accumulating, then
	// the min and max were updated

	Int minLoc = -1;
	Int maxLoc = -1;
	//
	T useIt;
	if (pInMask == 0) {
		// All pixels are good
		if (_include) {
			T datum;
			for (uInt i=0; i<nrval; i++) {
				datum = *pInData;
				useIt = LattStatsSpecialize::usePixelInc (_range(0), _range(1), datum);
				LattStatsSpecialize::accumulate(
					nPts, sum, mean, nvariance, variance, sumSq,
					dataMin, dataMax, minLoc, maxLoc, minMaxInit,
					False, *pInData, i, useIt
				);
				pInData += dataIncr;
			}
			if (_fixedMinMax) {
				dataMin = _range(0);
				dataMax = _range(1);
			}
		} else if (_exclude) {
			T datum;
			for (uInt i=0; i<nrval; i++) {
				datum = *pInData;
				useIt = LattStatsSpecialize::usePixelExc (
					_range(0), _range(1), datum
				);
				LattStatsSpecialize::accumulate(
					nPts, sum, mean, nvariance, variance, sumSq,
					dataMin, dataMax, minLoc, maxLoc, minMaxInit,
					False, *pInData, i, useIt
				);
				pInData += dataIncr;
			}
		}
		else {
			// All data accepted
			LattStatsSpecialize::setUseItTrue(useIt);
			for (uInt i=0; i<nrval; i++) {
				LattStatsSpecialize::accumulate(
					nPts, sum, mean, nvariance, variance, sumSq,
					dataMin, dataMax, minLoc, maxLoc, minMaxInit,
					False, *pInData, i, useIt
				);
				pInData += dataIncr;
			}
		}
	}
	else {
		// Some pixels are masked
		if (_include) {
			T datum;
			Bool mask;
			for (uInt i=0; i<nrval; i++) {
				datum = *pInData;
				mask = *pInMask;
				if (mask) {
					useIt = LattStatsSpecialize::usePixelInc (
						_range(0), _range(1), datum
					);
					LattStatsSpecialize::accumulate(
						nPts, sum, mean, nvariance, variance, sumSq,
						dataMin, dataMax, minLoc, maxLoc, minMaxInit,
						False, *pInData, i, useIt
					);
				}
				pInData += dataIncr;
				pInMask += maskIncr;
			}
			if (_fixedMinMax) {
				dataMin = _range(0);
				dataMax = _range(1);
			}
		}
		else if (_exclude) {
			T datum;
			Bool mask;
			for (uInt i=0; i<nrval; i++) {
				datum = *pInData;
				mask = *pInMask;
				if (mask) {
					useIt = LattStatsSpecialize::usePixelExc (_range(0), _range(1), datum);
					LattStatsSpecialize::accumulate(
						nPts, sum, mean, nvariance, variance, sumSq,
						dataMin, dataMax, minLoc, maxLoc, minMaxInit,
						False, *pInData, i, useIt
					);
				}
				pInData += dataIncr;
				pInMask += maskIncr;
			}
		}
		else {
			// All data accepted
			LattStatsSpecialize::setUseItTrue(useIt);
			for (uInt i=0; i<nrval; i++) {
				if (*pInMask) {
					LattStatsSpecialize::accumulate(
						nPts, sum, mean, nvariance, variance, sumSq,
						dataMin, dataMax, minLoc, maxLoc, minMaxInit,
						False, *pInData, i, useIt
					);
				}
				pInData += dataIncr;
				pInMask += maskIncr;
			}
		}
	}

	// Update overall min and max location.  These are never updated
	// if fixedMinMax is true.  These values are only meaningful for
	// Float images.  For Complex they are useless currently.

	//DataType type = whatType(*T(0));
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
	Array<U>& result, Array<Bool>& resultMask,
	const IPosition& shape
) {

	// Reshape arrays.  The mask is always true.  Any locations
	// in the storage lattice for which there were no valid points
	// will have the NPTS field set to zero.  That is what
	// we use to effectively mask it.

    result.resize(shape);
    resultMask.resize(shape);
    resultMask.set(True);

    Bool deleteRes;
    U* res = result.getStorage (deleteRes);
    U* resptr = res;

    U* sumPtr = _sum->storage();
    U* sumSqPtr = _sumSq->storage();
    U* nPtsPtr = _npts->storage();
    U* meanPtr = _mean->storage();
    U* variancePtr = _variance->storage();
    const T* minPtr = _min->storage();
    const T* maxPtr = _max->storage();

    uInt i,j;
    U* resptr_root = resptr;
    for (i=0; i<_n3; i++) {
       resptr = resptr_root + (Int(LatticeStatsBase::NPTS) * _n1);
       objcopy (resptr, nPtsPtr, _n1);
       nPtsPtr += _n1;

       resptr = resptr_root + (Int(LatticeStatsBase::SUM) * _n1);
       objcopy (resptr, sumPtr, _n1);
       sumPtr += _n1;

       resptr = resptr_root + (Int(LatticeStatsBase::SUMSQ) * _n1);
       objcopy (resptr, sumSqPtr, _n1);
       sumSqPtr += _n1;

       resptr = resptr_root + (Int(LatticeStatsBase::MEAN) * _n1);
       objcopy (resptr, meanPtr, _n1);
       meanPtr += _n1;

       resptr = resptr_root + (Int(LatticeStatsBase::VARIANCE) * _n1);
       objcopy (resptr, variancePtr, _n1);
       variancePtr += _n1;

       resptr = resptr_root + (Int(LatticeStatsBase::MIN) * _n1);
       for (j=0; j<_n1; j++) {
          convertScalar (*resptr++, *minPtr++);
       }

       resptr = resptr_root + (Int(LatticeStatsBase::MAX) * _n1);
       for (j=0; j<_n1; j++) {
          convertScalar (*resptr++, *maxPtr++);
       }

       resptr_root += _n1 * Int(LatticeStatsBase::NACCUM);
    }

    _sum = NULL;
    _sumSq = NULL;
    _npts = NULL;
    _min = NULL;
    _max = NULL;
    _initMinMax = NULL;
    _mean = NULL;
    _variance = NULL;
    _nvariance = NULL;

    result.putStorage (res, deleteRes);
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

