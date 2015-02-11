//# tStatAcc.cc: Test program for class StatAcc
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
//# $Id: tStatAcc.cc 20329 2008-06-06 07:59:22Z gervandiepen $

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/scimath/Mathematics/ChauvenetCriterionStatistics.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

int main() {
    try {
    	// just check for compilation success, the real tests are in
    	// tImageStatistics2
    	Double data[] = {
    		-2.61279178e+00,  -2.59342551e+00,  -2.16943479e+00,
    		-2.13970494e+00,  -1.91509378e+00,  -1.91133809e+00,
    		-1.84780550e+00,  -1.67959487e+00,  -1.55754685e+00,
    		-1.49124575e+00,  -1.47779667e+00,  -1.38040781e+00,
    		-1.37083769e+00,  -1.34913635e+00,  -1.29416192e+00,
    		-1.10022914e+00,  -1.07126451e+00,  -1.05194223e+00,
    		-1.03733921e+00,  -1.02524054e+00,  -9.84085381e-01,
    		-9.46198046e-01,  -9.23078358e-01,  -9.21401978e-01,
    		-8.76483500e-01,  -8.60657215e-01,  -8.26754928e-01,
    		-7.59524405e-01,  -7.36167967e-01,  -6.76235080e-01,
    		-6.72010839e-01,  -6.33015037e-01,  -5.91541886e-01,
    		-5.87743282e-01,  -5.28600693e-01,  -5.03111005e-01,
    		-4.84272331e-01,  -3.87220532e-01,  -3.62094551e-01,
    		-3.12986404e-01,  -3.01742464e-01,  -2.86407530e-01,
    		-2.77583510e-01,  -2.37437248e-01,  -2.37364024e-01,
    		-2.35247806e-01,  -2.11185545e-01,  -1.92734912e-01,
    		-1.87121660e-01,  -1.77792773e-01,  -1.69995695e-01,
    		-1.45033970e-01,  -1.16942599e-01,  -6.27262741e-02,
    		-3.45510058e-02,  -3.06752156e-02,  -1.79617219e-02,
    		-1.14524942e-02,  -3.16955987e-03,   7.29589257e-04,
    		1.24999344e-01,   2.12515876e-01,   2.50957519e-01,
    		2.79240131e-01,   2.81288683e-01,   3.05763662e-01,
    		3.11809599e-01,   3.40768367e-01,   3.51874888e-01,
    		3.91162097e-01,   4.58450705e-01,   4.82642174e-01,
    		4.96854514e-01,   7.20111370e-01,   7.22756803e-01,
    		7.25001752e-01,   8.35289240e-01,   8.46509099e-01,
    		8.93022776e-01,   9.00427580e-01,   9.17734325e-01,
    		9.18030262e-01,   1.04210591e+00,   1.05506992e+00,
    		1.09472048e+00,   1.15250385e+00,   1.16275501e+00,
    		1.21244884e+00,   1.22725236e+00,   1.31463480e+00,
    		1.33273876e+00,   1.57637489e+00,   1.58221984e+00,
    		1.65665936e+00,   1.80032420e+00,   1.91410339e+00,
    		2.02669597e+00,   2.08605909e+00,   2.09777880e+00,
    		2.21240473e+00,
    		3.5, 4, 5, 6, 7, 8, 1000000
    	};
    	{
    		// zscore=3.5, no iterations
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(3.5, 0);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 106, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    	}
    	{
    		// zscore=3.5, one iteration
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(3.5, 1);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 104, AipsError);
    		AlwaysAssert(*sd.max == 6, AipsError);
    	}
    	{
    		// zscore=3.5, iterate until converged
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(3.5, -1);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 102, AipsError);
    		AlwaysAssert(*sd.max == 4, AipsError);
    	}
    	{
    		// use Chauvenet criterion, no iterations
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(-1, 0);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 106, AipsError);
    		AlwaysAssert(*sd.max == 8, AipsError);
    	}
    	{
    		// use Chauvenet criterion, one iteration
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(-1, 1);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 103, AipsError);
    		AlwaysAssert(*sd.max == 5, AipsError);
    	}
    	{
    		// use Chauvenet criterion, iterate until converged
    		ChauvenetCriterionStatistics<Double, Double*, Bool*> cs(-1, -1);
    		cs.setData(data, 107);
    		StatsData<Double> sd = cs.getStatistics();
    		AlwaysAssert(sd.npts == 100, AipsError);
    		AlwaysAssert(*sd.max == data[99], AipsError);
    	}

    }

    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






