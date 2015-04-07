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
//# $Id: $

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/EnvVar.h>

#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/SubImage.h>

#include <casacore/scimath/Mathematics/ClassicalStatistics.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>

#include <iomanip>

using namespace casacore;

int main() {
	try {
		String *parts = new String[2];
		split(EnvironmentVariable::get("CASAPATH"), parts, 2, String(" "));
		String datadir = parts[0] + "/data/";
                delete [] parts;
		String imageName = datadir + "regression/unittest/stats/stats200M.im";
		if (! File(imageName).exists()) {
			cout << "Cannot find image so tests cannot be run" << endl;
			return 0;
		}
 		casacore::PagedImage<Float> im(imageName);
		RO_LatticeIterator<Float> imIter(im);
		/*
		{
			CountedPtr<StatsDataProvider<Double, const Float*, const Bool* > > dataProvider
				= new LatticeStatsDataProvider<Double, Float>(im);
			ClassicalStatistics<Double, const Float*> cs;
			cs.setDataProvider(dataProvider);
			std::set<Double> quartiles;
			quartiles.insert(0.25);
			quartiles.insert(0.75);
			std::map<Double, Double> quantileToValue;
			Double median = cs.getMedianAndQuantiles(
				quantileToValue, quartiles
			);
		}
		*/
		/*
		{
			cout << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(True);
			Bool deleteIt = False;
			while (! imIter.atEnd()) {
				const Float* begin = imIter.cursor().getStorage(deleteIt);
				cs.addData(begin, imIter.cursor().size());
				++imIter;
			}
			Record stats = cs.getStatistics();
			cout << stats << endl;
		}
		*/
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(True);
			Bool deleteIt = False;
			while (! imIter.atEnd()) {
				const Float* begin = imIter.cursor().getStorage(deleteIt);
				cs.addData(begin, imIter.cursor().size());
				++imIter;
			}
			Double mymin, mymax;
			cs.getMinMax(mymin, mymax);
			cout << "min " << mymin << " max " << mymax << endl;
		}
		{
			// This code will produce bogus results because the iterators are
			// no longer valid when getMinMax is called
			cout << endl << "This should not produce the desired results." << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			while (! imIter.atEnd()) {
				const Float* begin = imIter.cursor().copy().getStorage(deleteIt);
				cs.addData(begin, imIter.cursor().size());
				++imIter;
			}

			Double mymin, mymax;
			cs.getMinMax(mymin, mymax);
			cout << "min " << mymin << " max " << mymax << endl;
			//Record stats = cs.getStatistics();
			//cout << stats << endl;
		}
		{
			// This will work
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			while (! imIter.atEnd()) {
				Array<Float> chunk = imIter.cursor().copy();
				const Float* begin = chunk.getStorage(deleteIt);
				cs.addData(begin, chunk.size());
				chunks.push_back(chunk);
				++imIter;
			}
			Double mymin, mymax;
			cs.getMinMax(mymin, mymax);
			cout << "min " << mymin << " max " << mymax << endl;
		}
		{
			// this will work as expected, because all arrays are held
			// in memory before getStatistics() is called. Note that copies
			// of the arrays have to be made because imIter.cursor() will overwrite
			// the same location in memory on subsequent calls. If the arrays are not
			// copied, getStatistics will iterate over N copies of the same array.
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			uInt count = 0;
			while (! imIter.atEnd()) {
				chunks.push_back(imIter.cursor().copy());
				const Float* begin = chunks.back().getStorage(deleteIt);
				cs.addData(begin, chunks.back().size());
				++imIter;
				++count;
			}
			Double mymin, mymax;
			cs.getMinMax(mymin, mymax);
			cout << "min " << mymin << " max " << mymax << endl;
		}
		*/
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			uInt count = 0;
			while (! imIter.atEnd()) {
				chunks.push_back(imIter.cursor().copy());
				const Float* begin = chunks.back().getStorage(deleteIt);
				cs.addData(begin, chunks.back().size());
				++imIter;
				++count;
			}
			cout << "begin quantile computation" << endl;
			cout << std::setprecision(15) << "0.5 quantile value " << cs.getQuantile(0.5) << endl;
			vector<Double> qs;
			qs.push_back(0.9);
			qs.push_back(0.1);
			qs.push_back(0.5);
			qs.push_back(0.50000001);
			cout << std::setprecision(15) << "quantile values " << cs.getQuantiles(qs) << endl;
		}
		{
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			uInt count = 0;
			while (! imIter.atEnd()) {
				chunks.push_back(imIter.cursor().copy());
				const Float* begin = chunks.back().getStorage(deleteIt);
				cs.addData(begin, chunks.back().size());
				++imIter;
				++count;
			}
			cout << "begin quantile computation" << endl;
			cout << std::setprecision(15) << "0.5 quantile value " << cs.getQuantile(0.5, 10000) << endl;
			vector<Double> qs;
			qs.push_back(0.9);
			qs.push_back(0.1);
			qs.push_back(0.5);
			qs.push_back(0.50000001);
			cout << std::setprecision(15) << "quantile values " << cs.getQuantiles(qs, 10000) << endl;
		}
		*/
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
			imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			uInt count = 0;
			while (! imIter.atEnd()) {
				chunks.push_back(imIter.cursor().copy());
				const Float* begin = chunks.back().getStorage(deleteIt);
				cs.addData(begin, chunks.back().size());
				++imIter;
				++count;
			}
			cout << "begin median computation" << endl;
			cout << std::setprecision(15) << "median " << cs.getMedian() << endl;
		}
		*/
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
			LatticeStatsDataProvider<Float, Double> dataProvider(imIter);
			ClassicalStatistics<Double, const Float*, const Bool*> cs;
			StatsDataProvider<Double, const Float*, const Bool*>* dp =
					dynamic_cast<StatsDataProvider<Double, const Float*, const Bool*>* >(
						&dataProvider
					);
				ThrowIf(! dp, "unable to dynamic cast");
				cs.setDataProvider(dp);
				cout << "begin stats computation" << endl;
				cout << std::setprecision(15) << "stats " << cs.getStatistics() << endl;
			}
	*/
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
			LatticeStatsDataProvider<Float, Double> dataProvider(imIter);
			ClassicalStatistics<Double, const Float*, const Bool*> cs;
			StatsDataProvider<Double, const Float*, const Bool*>* dp =
				dynamic_cast<StatsDataProvider<Double, const Float*, const Bool*>* >(
					&dataProvider
				);
			ThrowIf(! dp, "unable to dynamic cast");
			cs.setDataProvider(dp);
			cout << "begin median computation" << endl;
			cout << std::setprecision(15) << "median " << cs.getMedian() << endl;
		}
	*/
		//im = PagedImage<Float>("stats200M.im");
		//imIter = RO_LatticeIterator<Float> (im);
		/*
		{
			cout << endl << "This should produce the desired results" << endl;
		    imIter.reset();
			ClassicalStatistics<Double, const Float*, Bool*> cs;
			cs.setCalculateAsAdded(False);
			Bool deleteIt = False;
			vector<Array<Float> > chunks;
			uInt count = 0;
			while (! imIter.atEnd()) {
				chunks.push_back(imIter.cursor().copy());
				const Float* begin = chunks.back().getStorage(deleteIt);
				cs.addData(begin, chunks.back().size());
				++imIter;
				++count;
			}
			cout << "begin statistics computation" << endl;
            Record stats = cs.getStatistics();
			cout << std::setprecision(15)  << stats << endl;
            AlwaysAssert(stats.asInt64("npts") == im.size(), AipsError);
        }
        */
		/*
        {
			cout << endl << "This should produce the desired results" << endl;
			LatticeStatsDataProvider<Double, Float> *dataProvider
				= new LatticeStatsDataProvider<Double, Float>(im);
			ClassicalStatistics<Double, const Float*> cs;
			StatsDataProvider<Double, const Float*, const Bool*> *dp =
				dynamic_cast<StatsDataProvider<Double, const Float*, const Bool*>* >(
					dataProvider
				);


			//  there are problems with slightly non-reproducable binning for quantiles because of
			//  finite machine precision when the AccumType is Float
			//LatticeStatsDataProvider<Double, Float> *dataProvider
			//				= new LatticeStatsDataProvider<Double, Float>(im);
			//			ClassicalStatistics<Double, const Float*> cs;
			//			StatsDataProvider<Double, const Float*, const Bool*> *dp =
			//				dynamic_cast<StatsDataProvider<Double, const Float*, const Bool*>* >(
			//					dataProvider
			//				);
			ThrowIf(! dp, "unable to dynamic cast");
			cs.setDataProvider(dp);
			cout << "begin statistics computation" << endl;
            Record stats = cs.getStatistics();
			cout << std::setprecision(15)  << stats << endl;
            AlwaysAssert(stats.asInt64("npts") == im.size(), AipsError);
            cout << "begin median computation" << endl;
            Double median = cs.getMedian();
			cout << std::setprecision(15)  << median << endl;
        }
        */
        {
            LatticeStatistics<Float> lattStats(im);
            Array<Double> d;
            lattStats.getStatistic(d, LatticeStatsBase::SUM);
            cout << d << endl;
            /*
            Array<Double> res;
            lattStats.getStatistic(res, LatticeStatsBase::MEDIAN);
            AlwaysAssert(near(*res.begin(), -0.00010517791088204831), AipsError);
            */
        }
		/*
        {
			cout << endl << "This should produce the desired results" << endl;
		    CountedPtr<StatsDataProvider<Double, const Float*, const Bool* > > dataProvider
                = new LatticeStatsDataProvider<Double, Float>(im);
			ClassicalStatistics<Double, const Float*> cs;
            cout << im.name() << endl; 
			// StatsDataProvider<Double, const Float*, const Bool*> *dp =
			//	dynamic_cast<StatsDataProvider<Double, const Float*, const Bool*>* >(
			//		dataProvider
			//	);
			cs.setDataProvider(dataProvider);
			cout << "begin median computation" << endl;
            Double median = cs.getMedian();
			cout << "median " << std::setprecision(15)  << median << endl;
            std::set<Double> quantiles;
            quantiles.insert(0.25);
            quantiles.insert(0.75);
            cout << "begin quartile computation" << endl;
            std::map<Double, Double> vals = cs.getQuantiles(quantiles);
            cout << "first and third quartiles " << vals << endl;


        }
        */
		/*

		{
			cout << endl << "This should produce the desired results" << endl;
			CountedPtr<StatsDataProvider<Double, const Float*, const Bool* > > dataProvider
				= new LatticeStatsDataProvider<Double, Float>(im);
			ClassicalStatistics<Double, const Float*> cs;
			cs.setDataProvider(dataProvider);
			cout << "begin medabsdevmed computation" << endl;
			Double medabsdevmed = cs.getMedianAbsDevMed();
			cout << "medabsdevmed " << std::setprecision(15)  << medabsdevmed << endl;
		}
		*/
        /*
        {
        	String imageName2 = datadir + "regression/unittest/stats/ngc4826.tutorial.16apr98.src.clean.model";
        	if (! File(imageName2).exists()) {
        		cout << "Cannot find image " << imageName2 << " so some tests cannot be run" << endl;
        		return 0;
        	}
        	casacore::PagedImage<Float> im2(imageName2);
        	LatticeStatistics<Float> lattStats(im2);
        	Array<Double> res;
        	lattStats.getStatistic(res, LatticeStatsBase::MEDIAN);
        	AlwaysAssert(*res.begin() == 0, AipsError);
        }
		*/
        /*
		{
			String imageName2 = datadir + "regression/unittest/stats/stats2G.im";
			if (! File(imageName2).exists()) {
				cout << "Cannot find image " << imageName2 << " so some tests cannot be run" << endl;
				return 0;
			}
			casacore::PagedImage<Float> im2(imageName2);
			Slicer slice(IPosition(im2.ndim(), 0), IPosition(im2.ndim(), 800));
			SubImage<Float> x(im2, slice);
			LatticeStatistics<Float> lattStats(x);
			Array<Double> res;
			lattStats.getStatistic(res, LatticeStatsBase::MEAN);
		}
        */

    }
    catch (const AipsError& x) {

        cerr << "Exception caught: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}

