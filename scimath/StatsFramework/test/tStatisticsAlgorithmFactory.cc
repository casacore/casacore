//# Copyright (C) 1999,2000,2001
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

#include <casacore/scimath/StatsFramework/StatisticsAlgorithmFactory.h>

#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

#if __cplusplus < 201103L
#  define nullptr NULL
#endif

int main() {
    try {
        StatisticsAlgorithmFactory<Double, Float*> saf;
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::CLASSICAL,
            AipsError
        );
        saf.configureChauvenet();
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::CHAUVENETCRITERION,
            AipsError
        );
        saf.configureFitToHalf();
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::FITTOHALF,
            AipsError
        );
        saf.configureHingesFences(0.6);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::HINGESFENCES,
            AipsError
        );
        StatisticsAlgorithmFactory<Double, Float*> saf2;
        Record r = saf2.toRecord();
        saf = StatisticsAlgorithmFactory<Double, Float*>::fromRecord(r);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::CLASSICAL,
            AipsError
        );

        Double zscore = 4.5;
        Int maxIter = 20;
        saf2.configureChauvenet(zscore, maxIter);
        r = saf2.toRecord();
        saf = StatisticsAlgorithmFactory<Double, Float*>::fromRecord(r);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::CHAUVENETCRITERION,
            AipsError
        );
        StatisticsAlgorithmFactoryData::ChauvenetData cd = saf.chauvenetData();
        AlwaysAssert(
            cd.zScore == zscore, AipsError
        );
        AlwaysAssert(
            cd.maxIter == maxIter, AipsError
        );

        FitToHalfStatisticsData::CENTER center = FitToHalfStatisticsData::CVALUE;
        FitToHalfStatisticsData::USE_DATA side = FitToHalfStatisticsData::GE_CENTER;
        Double centerValue = 5.5;
        saf2.configureFitToHalf(center, side, centerValue);
        r = saf2.toRecord();
        saf = StatisticsAlgorithmFactory<Double, Float*>::fromRecord(r);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::FITTOHALF,
            AipsError
        );
        StatisticsAlgorithmFactoryData::FitToHalfData<Double> fd = saf.fitToHalfData();
        AlwaysAssert(
            fd.center == center, AipsError
        );
        AlwaysAssert(
            fd.centerValue == centerValue, AipsError
        );
        AlwaysAssert(
            fd.side == side, AipsError
        );

        Double hf = 45.2;
        saf2.configureHingesFences(hf);
        r = saf2.toRecord();
        saf = StatisticsAlgorithmFactory<Double, Float*>::fromRecord(r);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::HINGESFENCES,
            AipsError
        );
        AlwaysAssert(saf.hingesFencesFactor() == hf, AipsError);

        maxIter = 22;
        Double c = 15.2;
        saf2.configureBiweight(maxIter, c);
        r = saf2.toRecord();
        saf = StatisticsAlgorithmFactory<Double, Float*>::fromRecord(r);
        AlwaysAssert(
            saf.createStatsAlgorithm()->algorithm() == StatisticsData::BIWEIGHT,
            AipsError
        );
        StatisticsAlgorithmFactoryData::BiweightData bd = saf.biweightData();
        AlwaysAssert(bd.maxIter == maxIter, AipsError);
        AlwaysAssert(bd.c == c, AipsError);
	}
	catch (const AipsError& x) {
		cout << x.getMesg() << endl;
		return 1;
	}
	return 0;
}
