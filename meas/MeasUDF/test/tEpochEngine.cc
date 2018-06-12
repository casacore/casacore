//# tEpochEngine.cc: Test program for EpochEngine
//# Copyright (C) 2016
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
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

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <iostream>

using namespace casacore;
using namespace std;

void testScalar()
{
  cout << "test scalars ..." << endl;
  // Convert an epoch from UTC to LAST.
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(pos);
  {
    Double value = MEpoch::Convert
      (epo, MEpoch::Ref(MEpoch::LAST,frame))()
        .getValue().get();
    TableExprNode node(tableCommand
                       ("calc meas.epoch('f-last',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')d").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Double sca1 = node.getDouble(0);
    AlwaysAssertExit (node.unit().getName() == "d");
    AlwaysAssertExit (near(value, sca1, 1e-8));
  }
}

void testArray()
{
  cout << "test arrays ..." << endl;
  // Convert a few epochs from UTC.
  TableExprNode node1(tableCommand
                      ("calc meas.epoch('f-last',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[6.60417deg, 52.8deg, -60.60417deg, -32.8deg],"
                       "[10m,1000m], 'WGS84')d").node());
  // Do it as multi-dim TaQL arrays.
  TableExprNode node2(tableCommand
                      ("using style python calc meas.epoch('f-last',"
                       "mjdtodate([[50217.625d],[50417.625d],[50617.625]]),"
                       "'UTC',"
                       "[[6.60417deg, 52.8deg], [-60.60417deg, -32.8deg]],"
                       "[[10m,1000m]], 'WGS84')d").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  ///cout << "taql=" << node2.getArrayDouble(0) << endl;
  Array<Double> arr1 = node1.getArrayDouble(0);
  Array<Double> arr2 = node2.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(2,3,2));
  cout<<arr2.shape()<<endl;
  AlwaysAssertExit (arr2.shape() == IPosition(4,1,3,2,1));
  AlwaysAssertExit (node1.unit().getName() == "d");
  AlwaysAssertExit (node2.unit().getName() == "d");
  // Check with Measures.
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(2);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  epo[1] = MEpoch(Quantity(50417.625,"d"));     // 30-nov-1996/15:00
  epo[2] = MEpoch(Quantity(50617.625,"d"));     // 18-jun-1997/15:00
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  int inx=0;
  for (uInt ip=0; ip<pos.size(); ++ip) {
    for (uInt ie=0; ie<epo.size(); ++ie) {
      double epn = MEpoch::Convert
        (epo[ie],
         MEpoch::Ref(MEpoch::LAST, MeasFrame(pos[ip])))()
        .getValue().get();
      AlwaysAssertExit (allNear(epn, arr1.data()[inx], 1e-8));
      AlwaysAssertExit (allNear(epn, arr2.data()[inx], 1e-8));
      inx++;
    }
  }
}

void testColumn()
{
  cout << "test columns ..." << endl;
  // Check with Measures.
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(3);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  epo[1] = MEpoch(Quantity(50417.625,"d"));     // 30-nov-1996/15:00
  epo[2] = MEpoch(Quantity(50617.625,"d"));     // 18-jun-1997/15:00
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  pos[2] = MPosition(Quantity(2000,"m"),
                     Quantity(-6.60417,"deg"), Quantity(32.8,"deg"),
                     MPosition::WGS84);
  // Convert a few epochs from UTC.
  // Note that the columns have the same values as the arrays above.
  // They are filled in tEpochEngine.run.
  TableExprNode node1(tableCommand
                      ("using style python calc meas.epoch('F_LAST',"
                       "TIME,POS1)d "
                       "from tEpochEngine_tmp.tab").node());
  TableExprNode node2(tableCommand
                      ("using style python calc meas.epoch('F_LAST',"
                       "TIME,POS2)d "
                       "from tEpochEngine_tmp.tab").node());
  AlwaysAssertExit (node1.nrow() == 3  &&  node2.nrow() == 3);
  AlwaysAssertExit (node1.isScalar());
  AlwaysAssertExit (node2.isScalar());
  for (uInt i=0; i<3; ++i) {
    ///cout << "taql=" << node1.getArrayDouble(i) << endl;
    ///cout << "taql=" << node2.getArrayDouble(i) << endl;
    Double sca1 = node1.getDouble(i);
    Double sca2 = node2.getDouble(i);
    double epn = MEpoch::Convert
      (epo[i],
       MEpoch::Ref(MEpoch::LAST, MeasFrame(pos[i])))()
      .getValue().get();
    AlwaysAssertExit (near(epn, sca1, 1e-8));
    AlwaysAssertExit (near(epn, sca2, 1e-8));
  }
}

int checkErr (const String& command)
{
  Bool fail = False;
  try {
    TableExprNode node(tableCommand(command).node());
    if (node.isScalar()) {
      node.getDouble(0);
    } else {
      node.getArrayDouble(0);
    }
  } catch (const std::exception& x) {
    cout << "Expected exception: " << x.what() << endl;
    fail = True;
  }
  if (!fail) {
    cout << "Command '" + command + "' should have failed" << endl;
    return 1;
  }
  return 0;
}

void testErr()
{
  cout << "test erroneous function calls ..." << endl;
  int nsucc = 0;
  nsucc += checkErr("using style python calc meas.last(23m)");
  nsucc += checkErr("using style python calc meas.last(date())");
  nsucc += checkErr("using style python calc meas.last('23-may-2005')");
  nsucc += checkErr("using style python calc meas.epoch('XYZ')");
  nsucc += checkErr("using style python calc meas.epoch(POS1) "
                    "from tEpochEngine_tmp.tab");
  AlwaysAssertExit(nsucc == 0);
}

int main()
{
  try {
    // Register the MEAS functions in TaQL.
    register_meas();
    // Execute some tests.
    testErr();
    testScalar();
    testArray();
    testColumn();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
