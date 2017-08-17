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

#ifndef SCIMATH_STATSALGORITHMFACTORY_H
#define SCIMATH_STATSALGORITHMFACTORY_H

#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/scimath/Mathematics/FitToHalfStatisticsData.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/scimath/Mathematics/StatisticsAlgorithm.h>
#include <casacore/scimath/Mathematics/StatisticsData.h>

namespace casacore {

// Allows a common way for configuring and creating stats algorithm objects

template <class AccumType, class DataIterator, class MaskIterator=const Bool *, class WeightsIterator=DataIterator>
class StatisticsAlgorithmFactory {

public:

    struct FitToHalfData {
        FitToHalfStatisticsData::CENTER center;
        // fit to half data portion to use
        FitToHalfStatisticsData::USE_DATA side;
        // fit to half center value (only relevent if center=CVALUE)
        AccumType centerValue;
    };

    struct ChauvenetData {
        Double zScore;
        Int maxIter;
    };

    // upon construction, the object is configured to use the classical stats algorithm
    StatisticsAlgorithmFactory();

    ~StatisticsAlgorithmFactory();

    void configureClassical();

    // configure to use fit to half algorithm.
    void configureFitToHalf(
        FitToHalfStatisticsData::CENTER centerType=FitToHalfStatisticsData::CMEAN,
        FitToHalfStatisticsData::USE_DATA useData=FitToHalfStatisticsData::LE_CENTER,
        AccumType centerValue=0
    );

    // configure to use hinges-fences algorithm
    void configureHingesFences(Double f);

    // configure to use Chauvenet's criterion
    void configureChauvenet(Double zscore=-1, Int maxIterations=-1);

    // Create a pointer to an object of a class derived from StatisticsAlgorithm
    // that reflects the current configuration
    CountedPtr<StatisticsAlgorithm<CASA_STATP> > createStatsAlgorithm() const;

    StatisticsData::ALGORITHM algorithm() const { return _algorithm; }

    // Throws an exception if the current configuration is not relevant
    // to the Chauvenet/zscore algorithm
    ChauvenetData chauvenetData() const;

    // Throws an exception if the current configuration is not relevant
    // to the hinges-fences algorithm
    Double hingesFencesFactor() const;

    // Throws an exception if the current configuration is not relevant
    // to the fit-to-half algorithm
    FitToHalfData fitToHalfData() const;

    // create a record from the current configuration that can be used
    // to create another object using the fromRecord() method.
    Record toRecord() const;

    // create an object from a record
    static StatisticsAlgorithmFactory<CASA_STATP> fromRecord(const Record& r);

private:

    StatisticsData::ALGORITHM _algorithm;
    // hinges-fences f factor
    Double _hf;
    FitToHalfData _fitToHalfData;
    ChauvenetData _chauvData;

};

}

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatisticsAlgorithmFactory.tcc>
#endif

#endif
