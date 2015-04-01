//# tImageFitter.cc:  test the PagedImage class
//# Copyright (C) 1994,1995,1998,1999,2000,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


#include <casacore/images/Images/ImageStatistics.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/casa/namespace.h>

void writeTestString(const String& test) {
    cout << "\n" << "*** " << test << " ***" << endl;
}

int main() {
	String test;
    FITSImage image("imageStats.fits");
    try {
    	{
            writeTestString(
                "test normal and copy constructors"
            );
            ImageStatistics<Float> stats(image);
            ImageStatistics<Float> stats2(stats);
            AlwaysAssert(stats.getBlc() == stats2.getBlc(), AipsError);

            stats.setBlc(IPosition(image.coordinates().nPixelAxes(),1));
            ImageStatistics<Float> stats3(stats);
            AlwaysAssert(stats.getBlc() == stats3.getBlc(), AipsError);

    	}

    	{
			writeTestString(
				"test multi-beam images"
			);
			CoordinateSystem csys = CoordinateUtil::defaultCoords4D();
			IPosition shape(4, 10, 15, 4, 20);
			TempImage<Float> tim(TiledShape(shape), csys);
			Array<Float> arr(IPosition(4, shape[0], shape[1], 1, 1));
			indgen(arr);
			for (uInt i=0; i<shape[2]; i++) {
				for (uInt j=0; j<shape[3]; j++) {
					tim.putSlice(arr, IPosition(4, 0, 0, i, j));
				}
			}
			tim.setUnits("Jy/pixel");
			ImageStatistics<Float> stats(tim);
			Vector<Int> axes(2, 0);
			axes[1] = 1;
			stats.setAxes(axes);
			Vector<LatticeStatistics<Float>::AccumType> myStats;
			Vector<LatticeStatistics<Float>::AccumType> exp;
			for (uInt i=0; i<shape[2]; i++) {
				for (uInt j=0; j<shape[3]; j++) {
					stats.getStats(myStats, IPosition(2, i, j), False);
					if (i == 0 && j == 0) {
						exp = myStats;
					}
					AlwaysAssert(allTrue(myStats == exp), AipsError);
				}
			}
			tim.setUnits("Jy/beam");
			GaussianBeam beam (
				Quantity(3, "arcmin"),
				Quantity(2.5, "arcmin"),
				Quantity(30, "deg")
			);
			ImageInfo info = tim.imageInfo();
			info.setRestoringBeam(beam);
			tim.setImageInfo(info);
			stats = ImageStatistics<Float>(tim);
			stats.setAxes(axes);
			for (uInt i=0; i<shape[2]; i++) {
				for (uInt j=0; j<shape[3]; j++) {
					stats.getStats(myStats, IPosition(2, i, j), False);
					if (i == 0 && j == 0) {
						exp = myStats;
					}
					AlwaysAssert(allTrue(myStats == exp), AipsError);
				}
			}
			info.removeRestoringBeam();
			info.setAllBeams(shape[3], shape[2], beam);
			tim.setImageInfo(info);
			stats = ImageStatistics<Float>(tim);
			stats.setAxes(axes);
			for (uInt i=0; i<shape[2]; i++) {
				for (uInt j=0; j<shape[3]; j++) {
					stats.getStats(myStats, IPosition(2, i, j), False);
					if (i == 0 && j == 0) {
						exp = myStats;
					}
					AlwaysAssert(allTrue(myStats == exp), AipsError);
				}
			}
			info.setBeam(
				1, 1, beam.getMajor() + Quantity(0.2, "arcmin"),
				beam.getMinor(), beam.getPA()
			);

			tim.setImageInfo(info);
			stats = ImageStatistics<Float>(tim);
			stats.setAxes(axes);
			for (uInt i=0; i<shape[2]; i++) {
				for (uInt j=0; j<shape[3]; j++) {
					stats.getStats(myStats, IPosition(2, i, j), False);
					if (i == 0 && j == 0) {
						exp = myStats;
					}
					if (i == 1 && j == 1) {
						Slice s(0, myStats.size() - 2);
						AlwaysAssert(allTrue(myStats(s) == exp(s)), AipsError);
						Quantity ratio = beam.getMajor()/info.restoringBeam(1, 1).getMajor();
						Double gotFlux = myStats[myStats.size() - 1];
						Double expFlux = ratio.getValue() * exp[exp.size() - 1];
						AlwaysAssert(fabs(gotFlux - expFlux)/expFlux < 1e-7, AipsError)
					}
					else {
						AlwaysAssert(allTrue(myStats == exp), AipsError);
					}
				}
			}
    	}
    	{
    		writeTestString(
    			"test getConvertedStatistic(FLUX) and getStats() for FLUX"
    		);
    		CoordinateSystem csys = CoordinateUtil::defaultCoords3D();
    		IPosition shape(3, 10, 15, 20);
    		TempImage<Float> tim(TiledShape(shape), csys);
    		Array<Float> arr(shape);
    		indgen(arr);
    		tim.put(arr);
    		tim.setUnits("Jy/beam");
    		GaussianBeam beam (
    			Quantity(3, "arcmin"),
    			Quantity(2.5, "arcmin"),
    			Quantity(30, "deg")
    		);
    		ImageInfo info = tim.imageInfo();
    		info.setRestoringBeam(beam);
    		tim.setImageInfo(info);
			ImageStatistics<Float> stats(tim);
			Array<Float> flux;
			AlwaysAssert(
				stats.getConvertedStatistic (flux, LatticeStatsBase::FLUX), AipsError
			);
			AlwaysAssert(near(*flux.begin(), 111724.9893), AipsError);
			Vector<Int> axes(2, 0);
			axes[1] = 1;
			stats.setAxes(axes);
			AlwaysAssert(
				stats.getConvertedStatistic (flux, LatticeStatsBase::FLUX), AipsError
			);
			AlwaysAssert(flux.shape() == IPosition(1, 20), AipsError);
			Vector<Double> statVals;
			Double area = beam.getArea("arcmin2");
			for (uInt i=0; i<20; ++i) {
				Float expFlux = sum(arr(IPosition(3,0,0,i), IPosition(3, 9, 14, i)))/area;
				AlwaysAssert(near(flux(IPosition(1,i)), expFlux), AipsError);
				AlwaysAssert(
					stats.getStats(
						statVals, IPosition(1, i), False
					), AipsError
				);
				AlwaysAssert(near(statVals[LatticeStatsBase::FLUX], expFlux), AipsError);
			}
			tim.setUnits("K");
			stats = ImageStatistics<Float>(tim);
			AlwaysAssert(
				stats.getConvertedStatistic (flux, LatticeStatsBase::FLUX), AipsError
			);
			AlwaysAssert(near(*flux.begin(), 3.4180507464507e9), AipsError);
			axes = Vector<Int>(2, 0);
			axes[1] = 1;
			stats.setAxes(axes);
			AlwaysAssert(
				stats.getConvertedStatistic (flux, LatticeStatsBase::FLUX), AipsError
			);
			AlwaysAssert(flux.shape() == IPosition(1, 20), AipsError);
			for (uInt i=0; i<20; ++i) {
				Float expFlux = sum(arr(IPosition(3,0,0,i), IPosition(3, 9, 14, i)))*3600;
				AlwaysAssert(near(flux(IPosition(1,i)), expFlux), AipsError);
				AlwaysAssert(
					stats.getStats(
						statVals, IPosition(1, i), False
					), AipsError
				);
				AlwaysAssert(near(statVals[LatticeStatsBase::FLUX], expFlux), AipsError);
			}
			Vector<GaussianBeam> beams(20);
			Vector<GaussianBeam>::iterator bIter = beams.begin();
			Vector<GaussianBeam>::iterator bEnd = beams.end();
			uInt count = 0;
			while (bIter != bEnd) {
				*bIter = GaussianBeam(
					Quantity(3 + count, "arcmin"),
					Quantity(2.5, "arcmin"),
					Quantity(30, "deg")
		    	);
				++count;
				++bIter;
			}
			ImageBeamSet beamSet(beams);
			ImageInfo ii = tim.imageInfo();
			ii.setBeams(beamSet);
    		tim.setUnits("Jy/beam");
			tim.setImageInfo(ii);
			stats = ImageStatistics<Float> (tim);
			stats.setAxes(indgen(2, 0, 1));
			Array<Float> fluxDensities;
			AlwaysAssert(
				stats.getConvertedStatistic (
					fluxDensities, LatticeStatsBase::FLUX
				), AipsError
			);
			cout << "flux densities " << fluxDensities << endl;
			// 0.2110611 is channel width in km/s
			Double expected = sum(fluxDensities) * 0.2110611;
			stats.setAxes(Vector<Int>());
			AlwaysAssert(
				stats.getConvertedStatistic(flux, LatticeStatsBase::FLUX),
				AipsError
			);
			AlwaysAssert(near(*flux.begin(), expected), AipsError);
    	}
        cout << "ok" << endl;
    }
    catch (const AipsError& x) {
        cerr << "Exception caught: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}

