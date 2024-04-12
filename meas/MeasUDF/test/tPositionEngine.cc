//# tPositionEngine.cc: Test program for PositionEngine
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <iostream>

using namespace casacore;
using namespace std;

void testScalar()
{
  cout << "test scalars ..." << endl;
  // Convert a position from WGS84 to ITRF.
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MVPosition npos = MPosition::Convert
    (pos, MPosition::Ref(MPosition::ITRF))().getValue();
    ///cout << "meas=" << npos << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.pos ('ITRF',"
                         "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    TableExprNode node2(tableCommand
                        ("calc meas.itrfxyz ("
                         "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node2.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    Array<Double> arr2 = node2.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (arr2.shape() == IPosition(1,3));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter2(arr2);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "m");
    AlwaysAssertExit (node2.unit().getName() == "m");
    AlwaysAssertExit (allNear(npos.getValue(), veciter1.vector(), 1e-8));
    AlwaysAssertExit (allNear(npos.getValue(), veciter2.vector(), 1e-8));
  }
  {
    MPosition hpos1(MVPosition(Quantity(10,"m")),
                    MPosition::WGS84);         // near WSRT
    MVPosition nhpos1 = MPosition::Convert
      (hpos1, MPosition::Ref(MPosition::ITRF))().getValue();
    MPosition hpos2(MVPosition(Quantity(1000,"m")),
                    MPosition::WGS84);         // near WSRT
    MVPosition nhpos2 = MPosition::Convert
      (hpos2, MPosition::Ref(MPosition::ITRF))().getValue();
    TableExprNode node1(tableCommand
                        ("calc meas.pos ('ITRF',"
                         "10m)").node());
    TableExprNode node2(tableCommand
                        ("calc meas.itrfxyz ("
                         "[10m, 1000m], 'WGS84H')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node2.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    Array<Double> arr2 = node2.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (arr2.shape() == IPosition(2,3,2));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter2(arr2);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "m");
    AlwaysAssertExit (node2.unit().getName() == "m");
    AlwaysAssertExit (allNear(nhpos1.getValue(), veciter1.vector(), 1e-8));
    AlwaysAssertExit (allNear(nhpos1.getValue(), veciter2.vector(), 1e-8));
    veciter2.next();
    AlwaysAssertExit (allNear(nhpos2.getValue(), veciter2.vector(), 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.itrfll ("
                         "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    TableExprNode node2(tableCommand
                        ("calc meas.itrfh ("
                         "6.60417*pi()/180., 52.8*pi()/180, 10,"
                         "'WGS84LL')").node());
    TableExprNode node3(tableCommand
                        ("calc meas.itrfllh ("
                         "[6.60417*pi()/180., 52.8*pi()/180, 10],"
                         "'WGS84LLH')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node2.getNodeRep()->isConstant());
    AlwaysAssertExit (node3.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    Double arr2 = node2.getDouble(0);
    Array<Double> arr3 = node3.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    AlwaysAssertExit (arr3.shape() == IPosition(1,3));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter3(arr3);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "rad");
    AlwaysAssertExit (node2.unit().getName() == "m");
    AlwaysAssertExit (node3.unit().getName().empty());
    AlwaysAssertExit (allNear(npos.getAngle().getValue(),
                              veciter1.vector(), 1e-8));
    AlwaysAssertExit (near(npos.getLength().getValue(),
                           arr2, 1e-8));
    AlwaysAssertExit (near(npos.getAngle().getValue()[0],
                           arr3.data()[0], 1e-8));
    AlwaysAssertExit (near(npos.getAngle().getValue()[1],
                           arr3.data()[1], 1e-8));
    AlwaysAssertExit (near(npos.getLength().getValue(),
                           arr3.data()[2], 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.itrfll ("
                         "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    TableExprNode node2(tableCommand
                        ("calc meas.itrfh ("
                         "6.60417*pi()/180., 52.8*pi()/180, 10,"
                         "'WGS84LL')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node2.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    Double arr2 = node2.getDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    VectorIterator<Double> veciter1(arr1);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "rad");
    AlwaysAssertExit (node2.unit().getName() == "m");
    AlwaysAssertExit (allNear(npos.getAngle().getValue(),
                              veciter1.vector(), 1e-8));
    AlwaysAssertExit (near(npos.getLength().getValue(),
                           arr2, 1e-8));
  }
  {
    // Test a nested meas.position function.
    TableExprNode node1(tableCommand
                        ("calc meas.itrfllh (meas.itrfxyz ("
                         "6.60417deg, 52.8deg, 10m, 'WGS84'))").node());
    TableExprNode node2(tableCommand
                        ("calc meas.itrfxyz (meas.itrfllh ("
                         "6.60417deg, 52.8deg, 10m, 'WGS84'))").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node2.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    Array<Double> arr2 = node2.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (arr2.shape() == IPosition(1,3));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter2(arr2);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName().empty());
    AlwaysAssertExit (node2.unit().getName() == "m");
    AlwaysAssertExit (near(npos.getAngle().getValue()[0],
                           arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(npos.getAngle().getValue()[1],
                           arr1.data()[1], 1e-8));
    AlwaysAssertExit (near(npos.getLength().getValue(),
                           arr1.data()[2], 1e-8));
    AlwaysAssertExit (allNear(npos.getValue(),
                              veciter2.vector(), 1e-8));
  }
}

void testArray()
{
  cout << "test arrays ..." << endl;
  // Convert a few positions from ITRF.
  TableExprNode node1(tableCommand
                      ("calc meas.wgs ("
                       "[3.82849e+06, 443253, 5.06498e+06,"
                       " 3.83924e+06, 430428, 5.05801e+06m,"
                       " 3.83849e+06, 443453, 5.06598e+06,"
                       " 3.84924e+06, 430628, 5.05901e+06,"
                       " 3.81849e+06, 443153, 5.06398e+06,"
                       " 3.82924e+06, 430328, 5.05701e+06])").node());
  // Do it as multi-dim TaQL arrays.
  TableExprNode node2(tableCommand
                      ("using style python calc meas.wgsll ("
                       "[[[3.82849e+06, 443253, 5.06498e+06,"
                       "   3.83924e+06, 430428, 5.05801e+06]],"
                       " [[3.83849e+06, 443453, 5.06598e+06,"
                       "   3.84924e+06, 430628, 5.05901e+06]],"
                       " [[3.81849e+06, 443153, 5.06398e+06,"
                       "   3.82924e+06, 430328, 5.05701e+06]]],"
                       "'itrfxyz')").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  ///cout << "taql=" << node2.getArrayDouble(0) << endl;
  Array<Double> arr1 = node1.getArrayDouble(0);
  Array<Double> arr2 = node2.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(2,3,6));
  AlwaysAssertExit (arr2.shape() == IPosition(4,2,2,1,3));
  VectorIterator<Double> arr1iter(arr1);
  VectorIterator<Double> arr2iter(arr2);
  // Check with Measures.
  Vector<MPosition> pos(6);
  pos[0] = MPosition(MVPosition(3.82849e+06, 443253, 5.06498e+06),
                     MPosition::ITRF);
  pos[1] = MPosition(MVPosition(3.83924e+06, 430428, 5.05801e+06),
                     MPosition::ITRF);
  pos[2] = MPosition(MVPosition(3.83849e+06, 443453, 5.06598e+06),
                     MPosition::ITRF);
  pos[3] = MPosition(MVPosition(3.84924e+06, 430628, 5.05901e+06),
                     MPosition::ITRF);
  pos[4] = MPosition(MVPosition(3.81849e+06, 443153, 5.06398e+06),
                     MPosition::ITRF);
  pos[5] = MPosition(MVPosition(3.82924e+06, 430328, 5.05701e+06),
                     MPosition::ITRF);
  for (uInt ip=0; ip<pos.size(); ++ip) {
    MVPosition npos = MPosition::Convert
      (pos[ip], MPosition::Ref(MPosition::WGS84))().getValue();
    AlwaysAssertExit (allNear(npos.getValue(),
                              arr1iter.vector(), 1e-8));
    AlwaysAssertExit (allNear(npos.getAngle().getValue(),
                              arr2iter.vector(), 1e-8));
    arr1iter.next();
    arr2iter.next();
  }
}

void testColumn()
{
  cout << "test columns ..." << endl;
  // Check with Measures.
  Vector<MPosition> pos(3);
  // Use same values as in tPositionEngine.run.
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  pos[2] = MPosition(Quantity(2000,"m"),
                     Quantity(-6.60417,"deg"), Quantity(32.8,"deg"),
                     MPosition::WGS84);
  // Convert a few positions.
  TableExprNode node1(tableCommand
                      ("calc meas.itrfxyz (POS1) "
                      "from tPositionEngine_tmp.tab").node());
  TableExprNode node2(tableCommand      // glish style starts at 1 !!!
                      ("calc meas.itrfxyz ([POS2[1], POS2[2], POS2[3]],"
                       "'WGS84') from tPositionEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (! node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.nrow() == 3  &&  node2.nrow() == 3);
  for (uInt i=0; i<3; ++i) {
    ///cout << "taql=" << node1.getArrayDouble(i) << endl;
    ///cout << "taql=" << node2.getArrayDouble(i) << endl;
    Array<Double> arr1 = node1.getArrayDouble(i);
    Array<Double> arr2 = node2.getArrayDouble(i);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (arr2.shape() == IPosition(1,3));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter2(arr2);
    MVPosition npos = MPosition::Convert
      (pos[i], MPosition::Ref(MPosition::ITRF))().getValue();
    AlwaysAssertExit (allNear(npos.getValue(), veciter1.vector(), 1e-8));
    AlwaysAssertExit (allNear(npos.getValue(), veciter2.vector(), 1e-6));
  }
}

void testName()
{
  cout << "test names ... " << endl;
  // Check with Measures.
  Vector<MPosition> pos(2);
  AlwaysAssertExit (MeasTable::Observatory(pos[0], "WSRT"));
  AlwaysAssertExit (MeasTable::Observatory(pos[1], "VLA"));
  TableExprNode node1(tableCommand
                      ("using style python calc meas.itrfxyz ("
                       "['WSRT','VLA'])").node());
  TableExprNode node2(tableCommand
                      ("using style python calc meas.wgsh ("
                       "['WSRT','VLA'])").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  Array<Double> arr1 = node1.getArrayDouble(0);
  Array<Double> arr2 = node2.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(2,3,2));
  AlwaysAssertExit (arr2.shape() == IPosition(1,2));
  VectorIterator<Double> arr1iter(arr1);
  // Check with Measures.
  for (uInt ip=0; ip<pos.size(); ++ip) {
    MVPosition npos1 = MPosition::Convert
      (pos[ip], MPosition::Ref(MPosition::ITRF))().getValue();
    MVPosition npos2 = MPosition::Convert
      (pos[ip], MPosition::Ref(MPosition::WGS84))().getValue();
    AlwaysAssertExit (allNear(npos1.getValue(),
                              arr1iter.vector(), 1e-8));
    AlwaysAssertExit (near(npos2.getLength().getValue(),
                              arr2.data()[ip], 1e-8));
    arr1iter.next();
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
  nsucc += checkErr("calc meas.itrfxyz ([POS1[1], POS1[2]], [POS1[3]],"
                    "'WGS84') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrf ([POS1[1], POS1[2]], [POS1[3]],"
                    "'WGS84') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrfxyz (1,2,"
                    "'WGS84XYZ') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrfxyz ([1,2],"
                    "'WGS84XYZ') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrfxyz ([POS1[1], POS1[2]],"
                    "'WGS84XYZ') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrfxyz (POS1[1], POS1[2], POS1[3],"
                    "'WGS84') from tPositionEngine_tmp.tab");
  nsucc += checkErr("calc meas.itrfxyz ('abc')");
  nsucc += checkErr("calc meas.pos ('abc')");
  nsucc += checkErr("calc meas.pos ('itrbxyz')");
  nsucc += checkErr("calc meas.pos ('itrfxyzl')");   // valid type!!!
  nsucc += checkErr("calc meas.pos ('itrf')");
  nsucc += checkErr("calc meas.pos ('itrf', [3kg, 3])");
  nsucc += checkErr("calc meas.pos ('itrf', [3, 3deg], 'itrfxyz')");
  nsucc += checkErr("calc meas.itrfll ('wxrt')");
  nsucc += checkErr("calc meas.itrfll ([1deg,2],[3m,4])");
  nsucc += checkErr("calc meas.itrfll ([[1deg],[2deg]],[3m])");
  nsucc += checkErr("calc meas.itrfll (1m,2m,3m,4m)");
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
    testName();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
