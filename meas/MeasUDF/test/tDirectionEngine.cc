//# tDirectionEngine.cc: Test program for DirectionEngine
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

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
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

void testScalar (Bool asDirCos)
{
  cout << "test scalars as "
       << (asDirCos ? "dircos" : "angles")
       << " ..." << endl;
  // Convert a direction from B1950.
  MDirection coord(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                   MDirection::B1950);
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(coord,epo,pos);
  {
    Vector<Double> dir;
    if (asDirCos) {
      dir = MDirection::Convert
        (coord, MDirection::Ref(MDirection::APP,frame))()
        .getValue().getValue();
    } else {
      dir = MDirection::Convert
        (coord, MDirection::Ref(MDirection::APP,frame))()
        .getValue().getAngle("rad").getValue();
    }
    ///cout << "meas=" << dir << endl;
    String funcStr = (asDirCos ? "dircos('APP'," : "app(");
    TableExprNode node(tableCommand
                       ("calc meas." + funcStr +
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<Double> arr1 = node.getArrayDouble(0);
    VectorIterator<Double> veciter(arr1);
    ///cout << "taql=" << arr1 << endl;
    if (asDirCos) {
      AlwaysAssertExit (node.unit().getName().empty());
    } else {
      AlwaysAssertExit (node.unit().getName() == "rad");
    }
    AlwaysAssertExit (allNear(dir, veciter.vector(), 1e-8));
  }
  {
    Vector<Double> dir;
    if (asDirCos) {
      dir = MDirection::Convert
        (coord, MDirection::Ref(MDirection::J2000))()
        .getValue().getValue();
    } else {
      dir = MDirection::Convert
        (coord, MDirection::Ref(MDirection::J2000))()
        .getValue().getAngle("deg").getValue();
    }
    ///cout << "meas=" << dir << endl;
    String funcStr = (asDirCos ? "dircos('j2000'," : "j2000(");
    TableExprNode node(tableCommand
                       ("calc meas." + funcStr +
                        "[185.425833deg, 31.799167deg],"
                        "'B1950')deg").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<Double> arr1 = node.getArrayDouble(0);
    VectorIterator<Double> veciter(arr1);
    ///cout << "diff=" << dir-veciter.vector() << endl;
    AlwaysAssertExit (allNear(dir, veciter.vector(), 1e-8));
  }
  {
    // Test a nested meas. function.
    Vector<Double> dir;
    if (asDirCos) {
      dir = coord.getValue().getValue();
    } else {
      dir = coord.getValue().getAngle("deg").getValue();
    }
    ///cout << "meas=" << dir << endl;
    String funcStr  = (asDirCos ? "dircos('b1950'," : "b1950(");
    String funcStr2 = (!asDirCos ? "dircos('galactic'," : "galactic(");
    TableExprNode node(tableCommand
                       ("calc meas." + funcStr +
                        "meas." + funcStr2 + 
                        "[185.425833deg, 31.799167deg],"
                        "'B1950'))deg").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<Double> arr1 = node.getArrayDouble(0);
    VectorIterator<Double> veciter(arr1);
    ///cout << "diff=" << dir-veciter.vector() << endl;
    AlwaysAssertExit (allNear(dir, veciter.vector(), 1e-8));
  }
  {
    // Test ZENITH.
    TableExprNode node1(tableCommand
                        ("calc meas.azel([0,0,1], 'AZEL')deg").node());
    TableExprNode node2(tableCommand
                        ("calc meas.azel('ZENITH')deg").node());
    Array<Double> arr1 = node1.getArrayDouble(0);
    AlwaysAssertExit (nearAbs(arr1.data()[0], 0.));
    AlwaysAssertExit (nearAbs(arr1.data()[1], 90.));
    Array<Double> arr2 = node2.getArrayDouble(0);
    AlwaysAssertExit (nearAbs(arr2.data()[0], 0.));
    AlwaysAssertExit (nearAbs(arr2.data()[1], 90.));
  }
}

void testArray (Bool asDirCos)
{
  cout << "test arrays as "
       << (asDirCos ? "dircos" : "angles")
       << " ..." << endl;
  // Convert a few directions from J2000.
  String funcStr = (asDirCos ? "dircos('APP'," : "app(");
  TableExprNode node1(tableCommand
                      ("calc meas." + funcStr +
                       "[185.425833deg, 31.799167deg,"
                       "175.425833deg, 41.799167deg,"
                       "165.425833deg, 51.799167deg,"
                       "155.425833deg, 61.799167deg],'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[6.60417deg, 52.8deg, -60.60417deg, -32.8deg],"
                       "[10m,1000m], 'WGS84')deg").node());
  // Do it as multi-dim TaQL arrays.
  TableExprNode node2(tableCommand
                      ("using style python calc meas." + funcStr +
                       "[[[185.425833deg, 31.799167deg],"
                       "[175.425833deg, 41.799167deg]],"
                       "[[165.425833deg, 51.799167deg],"
                       "[155.425833deg, 61.799167deg]]],'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[[6.60417deg, 52.8deg], [-60.60417deg, -32.8deg]],"
                       "[10m,1000m], 'WGS84')deg").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  ///cout << "taql=" << node2.getArrayDouble(0) << endl;
  Array<Double> arr1 = node1.getArrayDouble(0);
  Array<Double> arr2 = node2.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(4,asDirCos?3:2,4,3,2));
  AlwaysAssertExit (arr2.shape() == IPosition(5,asDirCos?3:2,2,2,3,2));
  VectorIterator<Double> arr1iter(arr1);
  VectorIterator<Double> arr2iter(arr2);
  // Check with Measures.
  Vector<MDirection> coord(4);
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(2);
  coord[0] = MDirection(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                        MDirection::J2000);
  coord[1] = MDirection(Quantity(175.425833,"deg"), Quantity(41.799167,"deg"),
                        MDirection::J2000);
  coord[2] = MDirection(Quantity(165.425833,"deg"), Quantity(51.799167,"deg"),
                        MDirection::J2000);
  coord[3] = MDirection(Quantity(155.425833,"deg"), Quantity(61.799167,"deg"),
                        MDirection::J2000);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  epo[1] = MEpoch(Quantity(50417.625,"d"));     // 30-nov-1996/15:00
  epo[2] = MEpoch(Quantity(50617.625,"d"));     // 18-jun-1997/15:00
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  for (uInt ip=0; ip<pos.size(); ++ip) {
    for (uInt ie=0; ie<epo.size(); ++ie) {
      for (uInt ic=0; ic<coord.size(); ++ic) {
        Vector<Double> dir;
        if (asDirCos) {
          dir = MDirection::Convert
            (coord[ic],
             MDirection::Ref(MDirection::APP,
                             MeasFrame(coord[ic],epo[ie],pos[ip])))()
            .getValue().getValue();
        } else {
          dir = MDirection::Convert
            (coord[ic],
             MDirection::Ref(MDirection::APP,
                             MeasFrame(coord[ic],epo[ie],pos[ip])))()
            .getValue().getAngle("deg").getValue();
        }
        ///cout << "meas=" << dir << endl;
        AlwaysAssertExit (allNear(dir, arr1iter.vector(), 1e-8));
        AlwaysAssertExit (allNear(dir, arr2iter.vector(), 1e-8));
        arr1iter.next();
        arr2iter.next();
      }
    }
  }
}

void testColumn (Bool asDirCos)
{
  cout << "test columns as "
       << (asDirCos ? "dircos" : "angles")
       << " ..." << endl;
  // Check with Measures.
  Vector<MDirection> coord(3);
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(3);
  coord[0] = MDirection(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                        MDirection::J2000);
  coord[1] = MDirection(Quantity(175.425833,"deg"), Quantity(41.799167,"deg"),
                        MDirection::J2000);
  coord[2] = MDirection(Quantity(165.425833,"deg"), Quantity(51.799167,"deg"),
                        MDirection::J2000);
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
  ///for (int i=0; i<3; ++i) {
  ///cout << MPosition::Convert(pos[i], MPosition::Ref(MPosition::ITRF))().getValue().getValue() << endl;
  ///}
  // Convert a few directions from J2000.
  String funcStr = (asDirCos ? "dircos('APP'," : "app(");
  TableExprNode node1(tableCommand
                      ("using style python calc meas." + funcStr +
                       "DIR[0,],TIME,POS1)deg "
                       "from tDirectionEngine_tmp.tab").node());
  TableExprNode node2(tableCommand
                      ("using style python calc meas." + funcStr +
                       "[DIR[0,0], DIR[0,1]]deg, 'J2000', TIME,POS2)deg "
                       "from tDirectionEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (! node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.nrow() == 3  &&  node2.nrow() == 3);
  for (uInt i=0; i<3; ++i) {
    ///cout << "taql=" << node1.getArrayDouble(i) << endl;
    ///cout << "taql=" << node2.getArrayDouble(i) << endl;
    Array<Double> arr1 = node1.getArrayDouble(i);
    Array<Double> arr2 = node2.getArrayDouble(i);
    AlwaysAssertExit (arr1.shape() == IPosition(1,asDirCos?3:2));
    AlwaysAssertExit (arr2.shape() == IPosition(1,asDirCos?3:2));
    VectorIterator<Double> veciter1(arr1);
    VectorIterator<Double> veciter2(arr2);
    Vector<Double> dir;
    if (asDirCos) {
      dir = MDirection::Convert
        (coord[i],
         MDirection::Ref(MDirection::APP,
                         MeasFrame(coord[i],epo[i],pos[i])))()
        .getValue().getValue();
    } else {
      dir = MDirection::Convert
        (coord[i],
         MDirection::Ref(MDirection::APP,
                         MeasFrame(coord[i],epo[i],pos[i])))()
        .getValue().getAngle("deg").getValue();
    }
    ///cout << "meas=" << dir << endl;
    AlwaysAssertExit (allNear(dir, veciter1.vector(), 1e-8));
    AlwaysAssertExit (allNear(dir, veciter2.vector(), 1e-8));
  }
}

void testName()
{
  cout << "test names as angles ... " << endl;
  // Check with Measures.
  Vector<MDirection> coord(3);
  Vector<MEpoch> epo(1);
  Vector<MPosition> pos(2);
  coord[0] = MDirection(MDirection::SUN);
  coord[1] = MDirection(MDirection::JUPITER);
  coord[2] = MDirection(MDirection::MOON);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  AlwaysAssertExit (MeasTable::Observatory(pos[0], "WSRT"));
  AlwaysAssertExit (MeasTable::Observatory(pos[1], "VLA"));
  TableExprNode node1(tableCommand
                      ("using style python calc meas.app ("
                       "['SUN','JUPITER','MOON'], 50217.625d,"
                       "['WSRT','VLA'])deg").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  Array<Double> arr1 = node1.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(4,2,3,1,2));
  VectorIterator<Double> arr1iter(arr1);
  // Check with Measures.
  for (uInt ip=0; ip<pos.size(); ++ip) {
    for (uInt ie=0; ie<epo.size(); ++ie) {
      for (uInt ic=0; ic<coord.size(); ++ic) {
        Vector<Double> dir = MDirection::Convert
            (coord[ic],
             MDirection::Ref(MDirection::APP,
                             MeasFrame(coord[ic],epo[ie],pos[ip])))()
            .getValue().getAngle("deg").getValue();
        ///cout << "meas=" << dir << endl;
        AlwaysAssertExit (allNear(dir, arr1iter.vector(), 1e-8));
        arr1iter.next();
      }
    }
  }
}

void testRiset()
{
  // Below is more or less the same test as in tmeas.run.
  // It is repeated here for easier debugging in case of problems.
  cout << "test rise/set ..." << endl;
  TableExprNode node(tableCommand(
                                  "calc str(meas.riseset(['SUN','MOON'], "
                                  "10Aug2010/13:12:11, 'WSRT'))").node());
  AlwaysAssertExit (node.getNodeRep()->isConstant());
  Array<String> arr = node.getArrayString(0);
  AlwaysAssertExit (arr.shape() == IPosition(4,2,2,1,1));
  AlwaysAssertExit (arr.data()[0] == "2010/08/10/04:06:04");
  AlwaysAssertExit (arr.data()[1] == "2010/08/10/19:10:42");
  AlwaysAssertExit (arr.data()[2] == "2010/08/10/04:29:35");
  AlwaysAssertExit (arr.data()[3] == "2010/08/10/18:59:10");
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
  nsucc += checkErr("using style python calc meas.j2000("
                    "[DIR[0,0], DIR[0,1]]deg, TIME,POS2)deg "
                    "from tDirectionEngine_tmp.tab");
  nsucc += checkErr("using style python calc meas.app(TIME) "
                    "from tDirectionEngine_tmp.tab");
  nsucc += checkErr("using style python calc meas.app(POS1) "
                    "from tDirectionEngine_tmp.tab");
  nsucc += checkErr("using style python calc meas.app(DIR) "
                    "from tDirectionEngine_tmp.tab");
  AlwaysAssertExit(nsucc == 0);
}

int main()
{
  try {
    // Register the MEAS functions in TaQL.
    register_meas();
    // Execute some tests.
    testErr();
    testScalar(False);
    testScalar(True);
    testArray(False);
    testArray(True);
    testColumn(False);
    testColumn(True);
    testName();
    testRiset();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
