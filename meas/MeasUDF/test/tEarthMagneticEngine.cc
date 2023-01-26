//# tEarthMagneticEngine.cc: Test program for EarthMagneticEngine
//# Copyright (C) 2018
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
#include <casacore/measures/Measures/MEarthMagnetic.h>
#include <casacore/measures/Measures/MCEarthMagnetic.h>
#include <casacore/measures/Measures/EarthMagneticMachine.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <iostream>

using namespace casacore;
using namespace std;

void testScalar()
{
  cout << "test scalars ..." << endl;
  // Convert a earthmagnetic from B1950.
  MEarthMagnetic coord(MVEarthMagnetic(Quantity(10, "nT"),
                                       Quantity(185.425833,"deg"),
                                       Quantity(31.799167,"deg")),
                       MEarthMagnetic::B1950);
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(epo,pos);
  {
    Vector<double> em = MEarthMagnetic::Convert
      (coord, MEarthMagnetic::Ref(MEarthMagnetic::APP,frame))()
      .getValue().getValue();
    cout << "meas=" << em << endl;
    TableExprNode node(tableCommand
                       ("calc meas.em('app',"
                        "185.425833deg, 31.799167deg,10nT,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    AlwaysAssertExit (node.unit().getName() == "nT");
    Array<double> arr1 = node.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    VectorIterator<double> veciter(arr1);
    cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node.unit().getName() == "nT");
    AlwaysAssertExit (allNear(em, veciter.vector(), 1e-8));
  }
  {
    Vector<double> em = MEarthMagnetic::Convert
        (coord, MEarthMagnetic::Ref(MEarthMagnetic::J2000))()
        .getValue().getValue();
    cout << "meas=" << em << endl;
    TableExprNode node(tableCommand
                       ("calc meas.em('j2000',"
                        "185.425833deg, 31.799167deg,10nT,'B1950')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    AlwaysAssertExit (node.unit().getName() == "nT");
    Array<double> arr1 = node.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    VectorIterator<double> veciter(arr1);
    cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (allNear(em, veciter.vector(), 1e-8));
  }
  {
    // Convert from nT values instead of deg,deg,nT.
    // The result is the same.
    Vector<double> em = MEarthMagnetic::Convert
        (coord, MEarthMagnetic::Ref(MEarthMagnetic::B1950))()
      .getValue().getAngle().getValue();
    cout << "meas=" << em << endl;
    TableExprNode node(tableCommand
                       ("calc meas.emang('b1950',"
                        "-8.46092318369e-9nT, -8.03641753778e-10,5.26943439197e-9,'B1950')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    AlwaysAssertExit (node.unit().getName() == "rad");
    Array<double> arr1 = node.getArrayDouble(0);
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    VectorIterator<double> veciter(arr1);
    cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (allNear(em, veciter.vector(), 1e-8));
  }
  {
    double em = MEarthMagnetic::Convert
        (coord, MEarthMagnetic::Ref(MEarthMagnetic::B1950))()
      .getValue().getLength().getValue();
    cout << "meas=" << em << endl;
    TableExprNode node(tableCommand
                       ("calc meas.emlen('b1950',"
                        "-8.46092318369e-9nT, -8.03641753778e-10,5.26943439197e-9,'B1950')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    AlwaysAssertExit (node.isScalar());
    AlwaysAssertExit (node.unit().getName() == "nT");
    double arr1 = node.getDouble(0);
    cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (near(em, arr1, 1e-8));
  }
}

void testArray()
{
  cout << "test arrays ..." << endl;
  // Convert a few earthMagnetics from J2000.
  TableExprNode node1(tableCommand
                      ("calc meas.em('APP',"
                       "[185.425833/180*pi(), 31.799167/180*pi(), 5,"
                       "175.425833/180*pi(), 41.799167/180*pi(), 10,"
                       "165.425833/180*pi(), 51.799167/180*pi(), 20,"
                       "155.425833/180*pi(), 61.799167/180*pi(), 30],"
                       "'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[6.60417deg, 52.8deg, -60.60417deg, -32.8deg],"
                       "[10m,1000m], 'WGS84')").node());
  // Do it as multi-dim TaQL arrays.
  TableExprNode node2(tableCommand
                      ("using style python calc meas.em('APP',"
                       "[[[185.425833/180*pi(), 31.799167/180*pi(), 5],"
                       "[175.425833/180*pi(), 41.799167/180*pi(), 10]],"
                       "[[165.425833/180*pi(), 51.799167/180*pi(), 20],"
                       "[155.425833/180*pi(), 61.799167/180*pi(), 30]]],"
                       "'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[[6.60417deg, 52.8deg], [-60.60417deg, -32.8deg]],"
                       "[10m,1000m], 'WGS84')").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  ///cout << "taql=" << node1.getArrayDouble(0) << endl;
  ///cout << "taql=" << node2.getArrayDouble(0) << endl;
  Array<double> arr1 = node1.getArrayDouble(0);
  Array<double> arr2 = node2.getArrayDouble(0);
  AlwaysAssertExit (arr1.shape() == IPosition(4,3,4,3,2));
  AlwaysAssertExit (arr2.shape() == IPosition(5,3,2,2,3,2));
  VectorIterator<double> arr1iter(arr1);
  VectorIterator<double> arr2iter(arr2);
  // Check with Measures.
  Vector<MEarthMagnetic> coord(4);
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(2);
  coord[0] = MEarthMagnetic(MVEarthMagnetic(Quantity(5,"nT"),
                                            Quantity(185.425833,"deg"),
                                            Quantity(31.799167,"deg")),
                            MEarthMagnetic::J2000);
  coord[1] = MEarthMagnetic(MVEarthMagnetic(Quantity(10,"nT"),
                                            Quantity(175.425833,"deg"),
                                            Quantity(41.799167,"deg")),
                            MEarthMagnetic::J2000);
  coord[2] = MEarthMagnetic(MVEarthMagnetic(Quantity(20,"nT"),
                                            Quantity(165.425833,"deg"),
                                            Quantity(51.799167,"deg")),
                            MEarthMagnetic::J2000);
  coord[3] = MEarthMagnetic(MVEarthMagnetic(Quantity(30,"nT"),
                                            Quantity(155.425833,"deg"),
                                            Quantity(61.799167,"deg")),
                            MEarthMagnetic::J2000);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  epo[1] = MEpoch(Quantity(50417.625,"d"));     // 30-nov-1996/15:00
  epo[2] = MEpoch(Quantity(50617.625,"d"));     // 18-jun-1997/15:00
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  for (uint32_t ip=0; ip<pos.size(); ++ip) {
    for (uint32_t ie=0; ie<epo.size(); ++ie) {
      for (uint32_t ic=0; ic<coord.size(); ++ic) {
        Vector<double> dir = MEarthMagnetic::Convert
          (coord[ic],
           MEarthMagnetic::Ref(MEarthMagnetic::APP,
                               MeasFrame(epo[ie],pos[ip])))()
          .getValue().getValue();
        ///cout << "meas=" << dir << endl;
        AlwaysAssertExit (allNear(dir, arr1iter.vector(), 1e-8));
        AlwaysAssertExit (allNear(dir, arr2iter.vector(), 1e-8));
        arr1iter.next();
        arr2iter.next();
      }
    }
  }
}

void testColumn()
{
  cout << "test columns ..." << endl;
  // Check with Measures.
  Vector<MEarthMagnetic> coord(3);
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(3);
  coord[0] = MEarthMagnetic(MVEarthMagnetic(-4.22979e-09,
                                            -3.98632e-10,
                                            2.63628e-09),
                            MEarthMagnetic::ITRF);
  coord[1] = MEarthMagnetic(MVEarthMagnetic(-7.42905e-09,
                                            6.00219e-10,
                                            6.66701e-09),
                            MEarthMagnetic::ITRF);
  coord[2] = MEarthMagnetic(MVEarthMagnetic(-1.28774e-08,
                                            5.90596e-09,
                                            2.64441e-08),
                            MEarthMagnetic::ITRF);
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
  // Convert a few earthMagnetics from J2000.
  TableExprNode node1(tableCommand
                      ("using style python calc meas.em('app',"
                       "EM,TIME,POS1) "
                       "from tEarthMagneticEngine_tmp.tab").node());
  TableExprNode node2(tableCommand
                      ("using style python calc meas.em('app',"
                       "[EM[0], EM[1], EM[2]], 'ITRF', TIME,POS2) "
                       "from tEarthMagneticEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (! node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.nrow() == 3  &&  node2.nrow() == 3);
  for (uint32_t i=0; i<3; ++i) {
    cout << "taql=" << node1.getArrayDouble(i) << ' ' << node1.unit().getName() << endl;
    cout << "taql=" << node2.getArrayDouble(i) << ' ' << node1.unit().getName() << endl;
    Array<double> arr1 = node1.getArrayDouble(i);
    Array<double> arr2 = node2.getArrayDouble(i);
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (arr2.shape() == IPosition(1,3));
    VectorIterator<double> veciter1(arr1);
    VectorIterator<double> veciter2(arr2);
    Vector<double> dir;
    dir = MEarthMagnetic::Convert
      (coord[i],
       MEarthMagnetic::Ref(MEarthMagnetic::APP,
                           MeasFrame(epo[i],pos[i])))()
      .getValue().getValue();
    cout << "meas=" << dir << endl;
    AlwaysAssertExit (allNear(dir, veciter1.vector(), 1e-8));
    AlwaysAssertExit (allNear(dir, veciter2.vector(), 1e-8));
  }
}

void testModel()
{
  cout << "Test IGRF model ..." << endl;
  MVTime dat(1998,5,18);
  MVPosition mvobs(Quantity(3828488.86, "m").getBaseValue(),
                   Quantity(443253.42, "m").getBaseValue(),
                   Quantity(5064977.78, "m").getBaseValue());
  MPosition obs(mvobs);
  MeasFrame frame((MEpoch(MVEpoch(dat.day()))), obs);
  MDirection::Ref mvref(MDirection::ITRF, frame);
  MVDirection mvd(obs.getValue());
  EarthMagneticMachine fm(mvref, Quantum<double>(0, "km"),
                          frame);
  fm.calculate(mvd);
  cout << "LOS:           " << fm.getLOSField() << endl;
  cout << "Long:          " << fm.getLong() << endl;
  cout << "Field:         " << fm.getField() << endl;
  {
    TableExprNode node(tableCommand
                       ("calc meas.igrf(0,[3828488.86,443253.42,5064977.78],'ITRF',18may1998,[3828488.86m,443253.42m,5064977.78m])").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<double> arr1 = node.getArrayDouble(0);
    cout<<arr1<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (node.unit().getName() == "nT");
    AlwaysAssertExit (allNear(fm.getField().getValue(), arr1, 1e-8));
  }
  {
    TableExprNode node(tableCommand
                       ("calc meas.igrflos(0,[0,90deg],'AZEL',18may1998,[3828488.86m,443253.42m,5064977.78m])").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<double> arr1 = node.getArrayDouble(0);
    cout<<arr1<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,1));
    AlwaysAssertExit (node.unit().getName() == "nT");
    AlwaysAssertExit (near(fm.getLOSField(), arr1.data()[0], 1e-8));
  }
  {
    TableExprNode node(tableCommand
                       ("calc meas.igrflong(0,[0,90deg],'AZEL',18may1998,[3828488.86m,443253.42m,5064977.78m])").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<double> arr1 = node.getArrayDouble(0);
    cout<<arr1<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,1));
    AlwaysAssertExit (node.unit().getName() == "rad");
    AlwaysAssertExit (near(fm.getLong(), arr1.data()[0], 1e-8));
  }
}

void testModelConv()
{
  cout << "Test IGRF model with conversion ..." << endl;
  MVTime dat(1998,5,18);
  MVPosition mvobs(Quantity(3828488.86, "m").getBaseValue(),
                   Quantity(443253.42, "m").getBaseValue(),
                   Quantity(5064977.78, "m").getBaseValue());
  MPosition obs(mvobs);
  MeasFrame frame((MEpoch(MVEpoch(dat.day()))), obs);
  MDirection::Ref mvref(MDirection::ITRF, frame);
  MVDirection mvd(obs.getValue());
  EarthMagneticMachine fm(mvref, Quantum<double>(0, "km"),
                          frame);
  fm.calculate(mvd);
  MEarthMagnetic coord(fm.getField(),
                       MEarthMagnetic::ITRF);
  {
    Vector<double> em = MEarthMagnetic::Convert
      (coord, MEarthMagnetic::Ref(MEarthMagnetic::APP,frame))()
      .getValue().getValue();
    cout<<"meas="<<em<<endl;
    TableExprNode node(tableCommand
                       ("calc meas.igrf('APP',0,[3828488.86,443253.42,5064977.78],'ITRF',18may1998,[3828488.86m,443253.42m,5064977.78m])").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    Array<double> arr1 = node.getArrayDouble(0);
    cout<<arr1<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,3));
    AlwaysAssertExit (node.unit().getName() == "nT");
    AlwaysAssertExit (allNear(em, arr1, 1e-8));
  }
}

int checkErr (const String& command)
{
  bool fail = false;
  try {
    TableExprNode node(tableCommand(command).node());
    if (node.isScalar()) {
      node.getDouble(0);
    } else {
      node.getArrayDouble(0);
    }
  } catch (const std::exception& x) {
    cout << "Expected exception: " << x.what() << endl;
    fail = true;
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
  // Unknown function.
  nsucc += checkErr("using style python calc meas.emxy()");
  // No arguments.
  nsucc += checkErr("using style python calc meas.emxyz()");
  // Non-real em value.
  nsucc += checkErr("using style python calc meas.emxyz(1+3i)");
  // No em values.
  nsucc += checkErr("using style python calc meas.emxyz('app')");
  // Invalid reftype.
  nsucc += checkErr("using style python calc meas.emxyz('fg')");
  // IGRF is invalid as from reftype.
  nsucc += checkErr("using style python calc meas.emxyz('app',1,2,3,'igrf')");
  // Invalid from reftype.
  nsucc += checkErr("using style python calc meas.emxyz('app',1,2,3,'xyz')");
  // No frame information (no epoch,pos).
  nsucc += checkErr("using style python calc meas.emxyz('app',1,2,3)");
  // Invalid unit m.
  nsucc += checkErr("using style python calc meas.emxyz('app',1m,2,3)");
  // No multiple of 3 values.
  nsucc += checkErr("using style python calc meas.emxyz('app',[1,2,3,4])");
  // Non-constant scalars.
  nsucc += checkErr("using style python calc meas.emxyz('app',POS1[0]'',"
                    "POS1[1]'',POS1[2]'') from tEarthMagneticEngine_tmp.tab");
  // No direction.
  nsucc += checkErr("using style python calc meas.emxyz('igrf', 1)");
  // Invalid height unit.
  nsucc += checkErr("using style python calc meas.emxyz('igrf', 1deg,2,3)");
  // Single direction scalar.
  nsucc += checkErr("using style python calc meas.igrflos('APP',1,2)");
  // Single direction scalar.
  nsucc += checkErr("using style python calc meas.igrflos(1,2)");
  // Invalid height type.
  nsucc += checkErr("using style python calc meas.igrflos(1+2i,[2,3])");
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
    testModel();
    testModelConv();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
