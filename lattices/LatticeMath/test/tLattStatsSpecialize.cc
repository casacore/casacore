//# tLatticeStatistics.cc: test LatticeStatistics class
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//# $Id$
// 
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/namespace.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>

int main() {
	try {
		{
			Vector<Float> mySamples(10);
			Double nPts = 0;
			Double sum = 0;
			Double mean = 0;
			Double nvariance = 0;
			Double variance = 0;
			Double sumSq = 0;
			Float dataMin, dataMax;
			Int minPos, maxPos;
			Bool minMaxInit = True;
			Bool fixedMinMax = False;
			uInt pos = 0;
			Bool useIt = True;
			for (
					Vector<Float>::iterator iter=mySamples.begin();
					iter != mySamples.end(); iter++
			) {
				*iter = 2*nPts;
				Float datum = *iter;
				LattStatsSpecialize::accumulate (
						nPts, sum, mean, nvariance, variance,
						sumSq, dataMin, dataMax, minPos,
						maxPos, minMaxInit, fixedMinMax, datum,
						pos, useIt
				);
				pos++;
			}
			AlwaysAssert(nPts == 10, AipsError);
			AlwaysAssert(sum == 90, AipsError);
			AlwaysAssert(mean == 9, AipsError);
			AlwaysAssert(near(variance, 36.6666666666, 1e-11), AipsError);
			AlwaysAssert(sumSq == 1140, AipsError);
			AlwaysAssert(dataMin == 0, AipsError);
			AlwaysAssert(dataMax == 18, AipsError);
			AlwaysAssert(minPos == 0, AipsError);
			AlwaysAssert(maxPos == 9, AipsError);
		}
		{
			Vector<Complex> mySamples(10);
			DComplex nPts = 0;
			DComplex sum = 0;
			DComplex mean = 0;
			DComplex nvariance = 0;
			DComplex variance = 0;
			DComplex sumSq = 0;
			Complex dataMin, dataMax;
			Int minPos, maxPos;
			Bool minMaxInit;
			Bool fixedMinMax = False;
			uInt pos = 0;
			Complex useIt(1, 1);
			Double count = 0;
			for (
					Vector<Complex>::iterator iter=mySamples.begin();
					iter != mySamples.end(); iter++
			) {
				*iter = Complex(2*count, count);
				Complex datum = *iter;
				LattStatsSpecialize::accumulate (
						nPts, sum, mean, nvariance, variance,
						sumSq, dataMin, dataMax, minPos,
						maxPos, minMaxInit, fixedMinMax, datum,
						pos, useIt
				);
				pos++;
				count++;
			}
			AlwaysAssert(nPts == DComplex(10, 10), AipsError);
			AlwaysAssert(sum == DComplex(90, 45), AipsError);
			AlwaysAssert(mean == DComplex(9, 4.5), AipsError);
			AlwaysAssert(near(variance.real(), 36.6666666666, 1e-11), AipsError);
			AlwaysAssert(near(variance.imag(), 9.166666666, 1e-10), AipsError);
			AlwaysAssert(sumSq == DComplex(1140, 285), AipsError);
			AlwaysAssert(dataMin == Complex(0, 0), AipsError);
			AlwaysAssert(dataMax == Complex(18, 9), AipsError);


			/*

			AlwaysAssert(sumSq == 1140, AipsError);
			AlwaysAssert(dataMin == 0, AipsError);
			AlwaysAssert(dataMax == 18, AipsError);
			AlwaysAssert(minPos == 0, AipsError);
			AlwaysAssert(maxPos == 9, AipsError);
			*/
		}
	}
	catch (AipsError x) {
		cerr << "aipserror: error " << x.getMesg() << endl;
		return 1;
	}
	return 0;
}
   

