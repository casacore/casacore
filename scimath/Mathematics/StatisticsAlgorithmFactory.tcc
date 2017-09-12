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

#ifndef SCIMATH_STATISTICSALGORITHMFACTORY_TCC
#define SCIMATH_STATISTICSALGORITHMFACTORY_TCC

#include <casacore/scimath/Mathematics/StatisticsAlgorithmFactory.h>

#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/scimath/Mathematics/ChauvenetCriterionStatistics.h>
#include <casacore/scimath/Mathematics/ClassicalStatistics.h>
#include <casacore/scimath/Mathematics/FitToHalfStatistics.h>
#include <casacore/scimath/Mathematics/HingesFencesStatistics.h>

namespace casacore {

CASA_STATD StatisticsAlgorithmFactory<CASA_STATP>::StatisticsAlgorithmFactory() {
    configureClassical();
}

CASA_STATD StatisticsAlgorithmFactory<CASA_STATP>::~StatisticsAlgorithmFactory() {}

CASA_STATD
void StatisticsAlgorithmFactory<CASA_STATP>::configureClassical() {
    _algorithm = StatisticsData::CLASSICAL;
}

CASA_STATD
void StatisticsAlgorithmFactory<CASA_STATP>::configureFitToHalf(
    FitToHalfStatisticsData::CENTER centerType,
    FitToHalfStatisticsData::USE_DATA useData,
    AccumType centerValue
) {
    _algorithm = StatisticsData::FITTOHALF;
    _fitToHalfData.center = centerType;
    _fitToHalfData.side = useData;
    _fitToHalfData.centerValue = centerValue;
}

CASA_STATD void StatisticsAlgorithmFactory<CASA_STATP>::configureHingesFences(Double f) {
    _algorithm = StatisticsData::HINGESFENCES;
    _hf = f;
}

CASA_STATD void StatisticsAlgorithmFactory<CASA_STATP>::configureChauvenet(
    Double zscore, Int maxIterations
) {
    _algorithm = StatisticsData::CHAUVENETCRITERION;
    _chauvData.zScore = zscore;
    _chauvData.maxIter= maxIterations;
}

CASA_STATD CountedPtr<StatisticsAlgorithm<CASA_STATP> >
StatisticsAlgorithmFactory<CASA_STATP>::createStatsAlgorithm() const {
    casacore::CountedPtr<StatisticsAlgorithm<CASA_STATP> > sa;
    switch (_algorithm) {
    case StatisticsData::CLASSICAL:
        sa = new ClassicalStatistics<CASA_STATP>();
        return sa;
    case StatisticsData::HINGESFENCES: {
        sa = new HingesFencesStatistics<CASA_STATP>(_hf);
        return sa;
    }
    case StatisticsData::FITTOHALF: {
        sa = new FitToHalfStatistics<CASA_STATP>(
            _fitToHalfData.center, _fitToHalfData.side,
            _fitToHalfData.centerValue
        );
        return sa;
    }
    case StatisticsData::CHAUVENETCRITERION: {
        sa = new ChauvenetCriterionStatistics<CASA_STATP>(
            _chauvData.zScore, _chauvData.maxIter
        );
        return sa;
    }
    default:
        ThrowCc(
            "Logic Error: Unhandled algorithm "
                + String::toString(_algorithm)
        );
    }
}

CASA_STATD Double StatisticsAlgorithmFactory<CASA_STATP>::hingesFencesFactor() const {
    ThrowIf(
        _algorithm != StatisticsData::HINGESFENCES,
        "Object is currently not configured to use the hinges-fences algorithm"
    );
    return _hf;
}

CASA_STATD typename StatisticsAlgorithmFactory<CASA_STATP>::FitToHalfData
StatisticsAlgorithmFactory<CASA_STATP>::fitToHalfData() const {
    ThrowIf(
        _algorithm != StatisticsData::FITTOHALF,
        "Object is currently not configured to use the fit to half algorithm"
    );
    return _fitToHalfData;
}

CASA_STATD typename StatisticsAlgorithmFactory<CASA_STATP>::ChauvenetData
StatisticsAlgorithmFactory<CASA_STATP>::chauvenetData() const {
    ThrowIf(
        _algorithm != StatisticsData::CHAUVENETCRITERION,
        "Object is currently not configured to use the chauvenet/zscore algorithm"
    );
    return _chauvData;
}

CASA_STATD Record StatisticsAlgorithmFactory<CASA_STATP>::toRecord() const {
    Record r;
    r.define("algorithm", _algorithm);
    switch (_algorithm) {
    case StatisticsData::CLASSICAL:
        // nothing else to add
        return r;
    case StatisticsData::HINGESFENCES: {
        r.define("hf", _hf);
        return r;
    }
    case StatisticsData::FITTOHALF: {
        r.define("center", _fitToHalfData.center);
        r.define("side", _fitToHalfData.side);
        if (_fitToHalfData.center == FitToHalfStatisticsData::CVALUE) {
            r.define("center_value", _fitToHalfData.centerValue);
        }
        return r;
    }
    case StatisticsData::CHAUVENETCRITERION: {
        r.define("zscore", _chauvData.zScore);
        r.define("max_iter", _chauvData.maxIter);
        return r;
    }
    default:
        ThrowCc(
            "Logic Error: Unhandled algorithm "
                + String::toString(_algorithm)
        );
    }
}

CASA_STATD StatisticsAlgorithmFactory<CASA_STATP>
StatisticsAlgorithmFactory<CASA_STATP>::fromRecord(const Record& r) {
    Int fieldNum = r.fieldNumber("algorithm");
    ThrowIf(fieldNum < 0, "field 'algorithm' not defined");
    // algorithm can be a string or int
    DataType dt = r.type(fieldNum);
    StatisticsData::ALGORITHM algorithm;
    if (dt == TpString) {
        String rAlg = r.asString(fieldNum);
        rAlg.downcase();
        if (rAlg.startsWith("cl")) {
            algorithm = StatisticsData::CLASSICAL;
        }
        else if (rAlg.startsWith("ch")) {
            algorithm = StatisticsData::CHAUVENETCRITERION;
        }
        else if (rAlg.startsWith("f")) {
            algorithm = StatisticsData::FITTOHALF;
        }
        else if (rAlg.startsWith("h")) {
            algorithm = StatisticsData::HINGESFENCES;
        }
        else {
            ThrowCc("Unrecognized algorithm " + r.asString(fieldNum));
        }
    }
    else if (dt == TpInt) {
        algorithm = (StatisticsData::ALGORITHM)r.asInt(fieldNum);
    }
    else {
        ThrowCc("Unsupported type for field 'algorithm'");
    }
    StatisticsAlgorithmFactory<CASA_STATP> saf;
    switch (algorithm) {
    case StatisticsData::CLASSICAL:
        return saf;
    case StatisticsData::HINGESFENCES: {
        ThrowIf(! r.isDefined("hf"), "field 'hf' is not defined");
        saf.configureHingesFences(r.asDouble("hf"));
        return saf;
    }
    case StatisticsData::FITTOHALF: {
        ThrowIf(! r.isDefined("center"), "field 'center' is not defined");
        FitToHalfStatisticsData::CENTER center = (FitToHalfStatisticsData::CENTER)r.asInt("center");
        AccumType centerValue = 0;
        if (center == FitToHalfStatisticsData::CVALUE) {
            ThrowIf (
                ! r.isDefined("center_value"),
                "field 'center_value' is not defined"
            );
            r.get("center_value", centerValue);
        }
        ThrowIf(! r.isDefined("side"), "field 'side' is not defined");
        FitToHalfStatisticsData::USE_DATA side = (FitToHalfStatisticsData::USE_DATA)r.asInt("side");
        saf.configureFitToHalf(center, side, centerValue);
        return saf;
    }
    case StatisticsData::CHAUVENETCRITERION: {
        ThrowIf(! r.isDefined("zscore"), "field 'zscore' is not defined");
        ThrowIf(! r.isDefined("max_iter"), "field 'max_iter' is not defined");
        Double zscore = r.asDouble("zscore");
        Int maxIter = r.asInt("max_iter");
        saf.configureChauvenet(zscore, maxIter);
        return saf;
    }
    default:
        ThrowCc(
            "Logic Error: Unhandled algorithm "
            + String::toString(algorithm)
        );
    }
}

}

#endif
