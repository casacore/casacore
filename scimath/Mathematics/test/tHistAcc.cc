//# tHistAcc.cc: Test program for class HistAcc
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
//# $Id$

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/scimath/Mathematics/StatAcc.h>
#include <casacore/scimath/Mathematics/HistAcc.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>

int main()
{
    try {

      uInt i;                           // Index
      uInt nv = 1000;                   // Nr of input values

      String captioni(" tHistAcc test for <Int>");
      String captionf(" tHistAcc test for <Float>");

      StatAcc<Int> si;               // Statistics accumulator
      StatAcc<Float> sf;               // Statistics accumulator

      HistAcc<Int> hmani(-2,10,1);  // Manually defined bins
      HistAcc<Float> hmanf(-2,10,1);  // Manually defined bins

      HistAcc<Float> hautof(25);         // Fully automatic
      HistAcc<Int> hautoi(25);           // Fully automatic
      HistAcc<Float> hsemif(25,2.0);     // Semi automatic (width given)

      Vector<Int> vvi(nv,0);         // values of required type
      Vector<Float> vvf(nv,0.0);         // values of required type
      Block<Float> bvf(nv);            // values of required type

      ACG gen(10,20);                  // random number generator
      Normal rnd(& gen, -5.0, 10.0);     // Normal distr (mean, variance)
      cout << " Nr of input values= " << nv << endl;
      
      for (i=0; i<nv; i++) {
	  vvi(i) = i;                     // temporary
	  vvf(i) = rnd();                 // Array values
	  vvi(i) = Int(vvf(i));                // round to Int
	  vvf(i) = Float(vvi(i));                // whole numbers Float
	  bvf[i] = vvi(i);                 // Block too
      }

      hmanf.put(vvf);           
      hsemif.put(vvf);           
      hautof.put(vvf);           
      hmani.put(vvi);           
      hmani.printHistogram(cout,"vvi hmani(-2,10,1)");
      hmanf.printHistogram(cout,"vvf hmanf(-2,10,1)");
      hsemif.printHistogram(cout,"vvf hsemif(25,2)");
      hautof.printHistogram(cout,"vvf hautof(25)");

      cout << " " << endl;
      sf.printSummaryLineHeader(cout,"remove low-contents bins");
      hmani.emptyBinsWithLessThan(2);
      si = hmani.getStatistics();          // get statistics accumulator
      si.printSummaryLine(cout,"hmani ");
      hmanf.emptyBinsWithLessThan(2);
      sf = hmanf.getStatistics();          // get statistics accumulator
      sf.printSummaryLine(cout,"hmanf ");
      hautof.emptyBinsWithLessThan(2);
      sf = hautof.getStatistics();          // get statistics accumulator
      sf.printSummaryLine(cout,"hautof ");
      hautof.printHistogram(cout,"hautof");

      cout << " " << endl;
      cout << "  hmanf.getStatistics().getMean():    ";
      cout << hmanf.getStatistics().getMean() << endl;

      Block<uInt> binsi;
      Block<Float> valsf;
      hmanf.getHistogram(binsi,valsf);
      cout << "  length of binsi=" << binsi.nelements() << endl; 
      cout << "  length of valsf=" << valsf.nelements() << endl; 

      hmanf.reset();
      hmanf.printHistogram(cout,"hmanf reset");
      hmanf.put(bvf);           
      hmanf.printHistogram(cout,"hmanf put bvf");

      cout << " *** end of tHistAcc *** " << endl;  

    } catch (AipsError x) {
        cout << x.getMesg() << endl;
        return 1;                       // unexpected error
    } 
    return 0;                           // exit with success status
}






