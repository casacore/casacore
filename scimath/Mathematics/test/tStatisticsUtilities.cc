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

#include <casacore/scimath/Mathematics/StatisticsUtilities.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/casa/Exceptions/Error.h>

#include <vector>

#include <casacore/casa/namespace.h>

int main() {
    try {
    	vector<Double> v(5);
    	v[0] = 1.5;
    	v[1] = 1;
    	v[2] = 2;
    	v[3] = 3;
    	v[4] = 2.5;

    	Double npts = 0;
    	Double sum = 0;
    	Double mean = 0;

    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::accumulate (
    			npts, sum, mean, v[i]
    		);
    	}
    	AlwaysAssert(npts == 5, AipsError);
    	AlwaysAssert(sum == 10, AipsError);
    	AlwaysAssert(mean == 2, AipsError);

    	npts = 0;
    	sum = 0;
    	mean = 0;
    	Double nvariance = 0;
    	Double sumsq = 0;
    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::accumulate (
    			npts, sum, mean, nvariance,
    			sumsq, v[i]
    		);
    	}
    	AlwaysAssert(npts == 5, AipsError);
    	AlwaysAssert(sum == 10, AipsError);
    	AlwaysAssert(mean == 2, AipsError);
    	AlwaysAssert(nvariance == 2.5, AipsError);
    	AlwaysAssert(sumsq == 22.5, AipsError);

    	npts = 0;
    	sum = 0;
    	mean = 0;
    	nvariance = 0;
    	sumsq = 0;
    	Double datamin = 0;
    	Double datamax = 0;
    	uInt minpos = 0;
    	uInt maxpos = 0;
    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::accumulate (
    			npts, sum, mean, nvariance, sumsq,
    			datamin, datamax, minpos, maxpos,
    			v[i], i
    		);
    	}
    	AlwaysAssert(npts == 5, AipsError);
    	AlwaysAssert(sum == 10, AipsError);
    	AlwaysAssert(mean == 2, AipsError);
    	AlwaysAssert(nvariance == 2.5, AipsError);
    	AlwaysAssert(sumsq == 22.5, AipsError);
    	AlwaysAssert(datamin == 1, AipsError);
    	AlwaysAssert(datamax == 3, AipsError);
    	AlwaysAssert(minpos == 1, AipsError);
    	AlwaysAssert(maxpos == 3, AipsError);

    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::doMax(
    			datamax, maxpos, i==0, v[i], i
    		);
    		StatisticsUtilities<Double>::doMin(
    			datamin, minpos, i==0, v[i], i
    		);
    	}
    	AlwaysAssert(datamin == 1, AipsError);
    	AlwaysAssert(datamax == 3, AipsError);
    	AlwaysAssert(minpos == 1, AipsError);
    	AlwaysAssert(maxpos == 3, AipsError);

    	vector<Double> w(5);
    	w[0] = 3;
    	w[1] = 2;
    	w[2] = 1;
    	w[3] = 2;
    	w[4] = 1;
    	npts = 0;
    	Double sumweights = 0;
    	Double wsum = 0;
    	Double wmean = 0;
    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::waccumulate (
    			npts, sumweights, wsum, wmean, v[i], w[i]
    		);
    	}
    	AlwaysAssert(npts == 5, AipsError);
    	AlwaysAssert(sumweights == 9, AipsError);
    	AlwaysAssert(wsum == 17, AipsError);
    	AlwaysAssert(near(wmean, 17.0/9.0), AipsError);

    	npts = 0;
    	sumweights = 0;
    	wsum = 0;
    	wmean = 0;
    	Double wsumsq = 0;
    	Double wnvariance = 0;
    	for (uInt i=0; i<5; i++) {
    		StatisticsUtilities<Double>::waccumulate (
    			npts, sumweights, wsum, wmean,
    			wnvariance, wsumsq, v[i], w[i]
    		);
    	}
    	AlwaysAssert(npts == 5, AipsError);
    	AlwaysAssert(sumweights == 9, AipsError);
    	AlwaysAssert(wsum == 17, AipsError);
    	AlwaysAssert(near(wmean, 17.0/9.0), AipsError);
    	AlwaysAssert(wsumsq == 37, AipsError);
    	AlwaysAssert(near(wnvariance, wsumsq - sumweights*wmean*wmean) , AipsError);
    	vector<Double>::const_iterator vbegin = v.begin();
    	vector<Double>::const_iterator viter = vbegin;
    	vector<Double>::const_iterator vend = v.end();
    	npts = 0;
    	nvariance = 0;
    	sumsq = 0;
    	Double center = 3;
    	while (viter != vend) {
    		StatisticsUtilities<Double>::accumulateSym(
    			npts, nvariance, sumsq, *viter, center
    		);
    		++viter;
    	}
    	AlwaysAssert(npts == 10, AipsError);
    	AlwaysAssert(sumsq == 105, AipsError);
    	AlwaysAssert(nvariance == 15, AipsError);

    	npts = 0;
    	nvariance = 0;
    	sumsq = 0;
    	sumweights = 0;
    	center = 3;
    	vector<Double>::const_iterator wbegin = w.begin();
    	vector<Double>::const_iterator witer = wbegin;
    	viter = vbegin;
    	while (viter != vend) {
    		StatisticsUtilities<Double>::waccumulateSym(
    			npts, sumweights, nvariance, sumsq, *viter, *witer, center
    		);
    		++viter;
    		++witer;
    	}
    	AlwaysAssert(npts == 10, AipsError);
    	AlwaysAssert(sumweights == 18, AipsError);
    	AlwaysAssert(nvariance == 32, AipsError);
    	AlwaysAssert(sumsq == 194, AipsError);

    	npts = 0;
    	nvariance = 0;
    	sumsq = 0;
    	center = 3;
    	uInt count = 0;
    	viter = vbegin;
    	while (viter != vend) {
    		StatisticsUtilities<Double>::accumulateSym(
    			npts, nvariance, sumsq, datamin, datamax,
    			minpos, maxpos, *viter, count, center
    		);
    		++viter;
    		++count;
    	}
    	AlwaysAssert(npts == 10, AipsError);
    	AlwaysAssert(sumsq == 105, AipsError);
    	AlwaysAssert(nvariance == 15, AipsError);
    	AlwaysAssert(datamin == 1, AipsError);
    	AlwaysAssert(datamax == 3, AipsError);
    	AlwaysAssert(minpos == 1, AipsError);
    	AlwaysAssert(maxpos == 3, AipsError);

    	npts = 0;
    	nvariance = 0;
    	sumsq = 0;
    	sumweights = 0;
    	center = 3;
    	viter = vbegin;
    	witer = wbegin;
    	count = 0;
    	while (viter != vend) {
    		StatisticsUtilities<Double>::waccumulateSym(
    			npts, sumweights, nvariance, sumsq, datamin,
    			datamax, minpos, maxpos, *viter, *witer, count, center
    		);
    		++viter;
    		++witer;
    		++count;
    	}
    	AlwaysAssert(npts == 10, AipsError);
    	AlwaysAssert(sumweights == 18, AipsError);
    	AlwaysAssert(nvariance == 32, AipsError);
    	AlwaysAssert(sumsq == 194, AipsError);
    	AlwaysAssert(datamin == 1, AipsError);
    	AlwaysAssert(datamax == 3, AipsError);
    	AlwaysAssert(minpos == 1, AipsError);
    	AlwaysAssert(maxpos == 3, AipsError);
    }
    catch (const AipsError& x) {
        cout << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}






