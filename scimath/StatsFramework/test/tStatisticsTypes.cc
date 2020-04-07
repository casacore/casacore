//# tStatisticsTypes.cc: Test program for class StatisticsTypes
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

#include <casacore/scimath/StatsFramework/StatisticsTypes.h>

#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>


int main() {
	// Unit tests for toRecord(const StatsData<AccumType>& stats) from
	// StatisticsTypes.
	try {
		struct StatsData<Double> stats;
		stats.masked = True;
		stats.max = new Double(27.3);
		stats.maxpos = std::make_pair(2, 55);
		stats.mean = 22.1;
		stats.median = new Double(22.8);
		stats.medAbsDevMed = new Double(1.3);
		stats.min = new Double(18.4);
		stats.minpos = std::make_pair(1, 2);
		stats.npts = 111.0;
		stats.nvariance = 249.75;
		stats.rms = 22.15;
		stats.stddev = 1.5;
		stats.sum = 2453.1;
		stats.sumsq = 54463.26;
		stats.sumweights = 105.8;
		stats.variance = 2.25;
		stats.weighted = True;

		// The following four tests should be done in the given order, as the
		// sequence of tests incrementally removes some values from the "stats"
		// structure to test the conversion of optional fields (or lack
		// thereof).
		{
			// Test conversion of fully defined structure. (Note that some
			// fields in StatsData are never converted by toRecord, and so are
			// not in this test.)
			Record rec = toRecord(stats);
			AlwaysAssert(
				rec.asBool("isMasked") == stats.masked,
				AipsError);
			AlwaysAssert(
				rec.asBool("isWeighted") == stats.weighted,
				AipsError);
			AlwaysAssert(
				rec.asInt64("maxDatasetIndex") == stats.maxpos.first,
				AipsError);
			AlwaysAssert(
				rec.asInt64("maxIndex") == stats.maxpos.second,
				AipsError);
			AlwaysAssert(
				rec.asInt64("minDatasetIndex") == stats.minpos.first,
				AipsError);
			AlwaysAssert(
				rec.asInt64("minIndex") == stats.minpos.second,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::SUMWEIGHTS))
				== stats.sumweights,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::MEAN))
				== stats.mean,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::NPTS))
				== stats.npts,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::RMS))
				== stats.rms,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::STDDEV))
				== stats.stddev,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::SUM))
				== stats.sum,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::SUMSQ))
				== stats.sumsq,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::VARIANCE))
				== stats.variance,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::MAX))
				== *stats.max,
				AipsError);
			AlwaysAssert(
				rec.asDouble(StatisticsData::toString(StatisticsData::MIN))
				== *stats.min,
				AipsError);
		}
		{
			// "sumweights" should be absent from output record when "weighted"
			// flag is False.
			stats.weighted = False;
			Record rec = toRecord(stats);
			AlwaysAssert(
				rec.isDefined(StatisticsData::toString(StatisticsData::SUMWEIGHTS)),
				AipsError);
		}
		{
			// Index of maximum value should be absent from output record when
			// "max" value is missing.
			stats.max = nullptr;
			Record rec = toRecord(stats);
			AlwaysAssert(!rec.isDefined("maxDatasetIndex"), AipsError);
			AlwaysAssert(!rec.isDefined("maxIndex"), AipsError);
		}
		{
			// Index of minimum value should be absent from output record when
			// "min" value is missing.
			stats.min = nullptr;
			Record rec = toRecord(stats);
			AlwaysAssert(!rec.isDefined("minDatasetIndex"), AipsError);
			AlwaysAssert(!rec.isDefined("minIndex"), AipsError);
		}
	}
	catch (const AipsError& x) {
		cout << x.what() << endl;
		return 1;
	}
	return 0;
}
