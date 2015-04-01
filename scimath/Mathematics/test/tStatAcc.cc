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
//# $Id$

#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays.h>
#include <casacore/scimath/Mathematics/StatAcc.h>
// #include <casacore/casa/Utilities/Assert.h>
// #include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


#include <casacore/casa/namespace.h>

int main()
{
    try {

      Int v1 = -10;
      Int v2 = 9;
      Int v3 = 1;
      uInt nv = 0;                     // nr ov test-values
      Int v;
      for (v=v1; v<=v2; v+=v3) {nv++;}  
      cout << "value-vector vv:  nv=" << nv;
      cout << "  v1=" << v1;
      cout << "  v2=" << v2;
      cout << "  v3=" << v3;
      cout << endl;
      cout << "weight-vector ww: 0,nv,1" << endl;

// For testing other types, change them here:

      String caption(" tStatAcc test for <Int>");
      StatAcc<Int> s;               // accumulator
      StatAcc<Int> s1;                   
      Vector<Int> vv(nv,0);         // values of required type
      Block<Int> bv(nv);            // values of required type

      Vector<Float> ww(nv,0.0);          // weights are always Float
      Block<Float> bw(nv);             // weights are always Float
      
      Int i=0;                         // index
      for (v=v1; v<=v2; v+=v3) {
	  vv(i) = v;                   // Array values
	  bv[i] = v;                   // Block values
	  ww(i) = i;                   // Array weights 
	  bw[i] = i;                   // Block weights 
	  i++;
      }

      s.printSummaryLineHeader(cout,caption);
      s.printSummaryLine(cout,"After initialisation");

      s.reset();
      s.printSummaryLine(cout,"After reset");
      for (i=0; i<20; i++) {
	  s.put(vv(i));
      }    
      s.printSummaryLine(cout,"vv(i)");

      s.reset();
      for (i=0; i<20; i++) {
	  s.put(vv(i),ww(i));
      }
      s.printSummaryLine(cout,"vv(i), weight=ww(i)");

      s.reset();
      s.put(vv);           
      s.printSummaryLine(cout,"vv");

      s.reset();
      s.put(bv);           
      s.printSummaryLine(cout,"bv");

      s.reset();
      s.put(vv,ww);           
      s.printSummaryLine(cout,"vv, weight=ww");
      s.put(vv,ww);           
      s.printSummaryLine(cout,"again");

      s.reset();
      s.put(bv,bw);           
      s.printSummaryLine(cout,"bv, weight=bw");

      s.reset();
      s.put(vv);
      s.put(-50);
      s.printSummaryLine(cout,"s=vv, plus extra -50");
      s1 = s;
      s1.put(100);
      s1.printSummaryLine(cout,"s1=s, plus extra 100");
      s1 += s;           
      s1.printSummaryLine(cout,"s1 += s");
      s1 = s1 + s;           
      s1.printSummaryLine(cout,"s1 = s1 + s");
      s1 = s + s1;           
      s1.printSummaryLine(cout,"s1 = s + s1");

      s.printSummaryLine(cout,"s");

      cout << "Test of s.get-functions: " << endl;
      cout << "  s.getWtot:    " << s.getWtot() << endl;
      cout << "  s.getCount:   " << s.getCount() << endl;
      cout << "  s.getMean:    " << s.getMean() << endl;
      cout << "  s.getRms:     " << s.getRms() << endl;
      cout << "  s.getVariance:" << s.getVariance() << endl;
      cout << "  s.getRmsAbs:  " << s.getRmsAbs() << endl;
      cout << "  s.getMin:     " << s.getMin() << endl;
      cout << "  s.getMax:     " << s.getMax() << endl;

      s.printSummaryList(cout,"Test of s.printSummaryList");


    } catch (AipsError x) {
        cout << x.getMesg() << endl;
        return 1;                       // unexpected error
    } 
    return 0;                           // exit with success status
}






