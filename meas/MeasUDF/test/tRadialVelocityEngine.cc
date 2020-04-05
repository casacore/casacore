//# tRadialVelocityEngine.cc: Test program for RadialVelocityEngine
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
//#
//# $Id$

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MEpoch.h>
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
  cout << "test radialvelocity scalars ..." << endl;
  // Convert a radialvelocity for a given frame.
  MDirection coord(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                   MDirection::B1950);
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(coord,epo,pos);
  MRadialVelocity rv(Quantity(100,"km/s"),
                     MRadialVelocity::Ref(MRadialVelocity::BARY, frame));
  MVRadialVelocity nrv = MRadialVelocity::Convert
    (rv, MRadialVelocity::Ref(MRadialVelocity::LSRK))().getValue();
  //cout << "meas=" << nrv << " m/s" << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.rv ('LSRK',"
                         "0100'km/s', 'BARY',"
                         "185.425833deg, 31.799167deg, 'B1950',"
                         "50217.625d,"
                         "6.60417deg, 52.8deg, 'WGS84')'m/s'").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    Double val1 = node1.getDouble(0);
    //cout << "taql=" << val1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "m/s");
    AlwaysAssertExit (near(nrv.getValue(), val1, 1e-8));
  }
}

void testDopplerScalar()
{
  cout << "test doppler scalars ..." << endl;
  // Make a radvel from a Z doppler.
  MDoppler dop(Quantity(2.5), MDoppler::Z);
  MRadialVelocity mvel(MRadialVelocity::fromDoppler(dop));
  Double vel = mvel.getValue().getValue() / 1000.;    // km/s
  //cout << "meas=" << vel << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.radvel('LSRK',"
                         "2.5, 'Z')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    Double val1 = node1.getDouble(0);
    //cout << "taql=" << val1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "km/s");
    AlwaysAssertExit (near(vel, val1, 1e-8));
  }
}

void testArray()
{
  cout << "test radialvelocity arrays ..." << endl;
  // Convert a radialvelocity for a given frame.
  MDirection coord(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                   MDirection::B1950);
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(coord,epo,pos);
  MRadialVelocity rv1(Quantity(100,"km/s"),
                      MRadialVelocity::Ref(MRadialVelocity::LSRK, frame));
  MRadialVelocity rv2(Quantity(200,"km/s"),
                      MRadialVelocity::Ref(MRadialVelocity::LSRK, frame));
  MVRadialVelocity nrv1 = MRadialVelocity::Convert
    (rv1, MRadialVelocity::Ref(MRadialVelocity::TOPO))().getValue();
  MVRadialVelocity nrv2 = MRadialVelocity::Convert
    (rv2, MRadialVelocity::Ref(MRadialVelocity::TOPO))().getValue();
  //cout << "meas=" << nrv1 <<' '<<nrv2<< << " m/s" << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.rv ('TOPO',"
                         "[100,200]'km/s', 'LSRK',"
                         "185.425833deg, 31.799167deg, 'B1950',"
                         "50217.625d,"
                         "6.60417deg, 52.8deg, 'WGS84')'m/s'").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    //cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    AlwaysAssertExit (node1.unit().getName() == "m/s");
    AlwaysAssertExit (near(nrv1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(nrv2.getValue(), arr1.data()[1], 1e-8));
  }
}

void testDopplerArray()
{
  cout << "test doppler arrays ..." << endl;
  // Make a radvel from a Z doppler.
  MDoppler dop1(Quantity(2.5), MDoppler::RADIO);
  MDoppler dop2(Quantity(1.5), MDoppler::RADIO);
  MRadialVelocity mvel1(MRadialVelocity::fromDoppler(dop1));
  MRadialVelocity mvel2(MRadialVelocity::fromDoppler(dop2));
  Double vel1 = mvel1.getValue().getValue() / 1000.;    // km/s
  Double vel2 = mvel2.getValue().getValue() / 1000.;    // km/s
  //cout << "meas=" << vel1 << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.radvel('LSRK',"
                         "[2.5,1.5], 'RADIO')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    Array<Double> arr1 = node1.getArrayDouble(0);
    //cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "km/s");
    AlwaysAssertExit (near(vel1, arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(vel2, arr1.data()[1], 1e-8));
  }
}

void testColumn()
{
  cout << "test radvel columns ..." << endl;
  // Check with Measures.
  MDirection coord1(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                    MDirection::J2000);
  MEpoch epo1(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos1(Quantity(10,"m"),
                 Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                 MPosition::WGS84);         // near WSRT
  MeasFrame frame1(coord1,epo1,pos1);
  MRadialVelocity rv1a(Quantity(200,"km/s"),
                       MRadialVelocity::Ref(MRadialVelocity::BARY, frame1));
  MRadialVelocity rv2a(Quantity(300000,"m/s"),
                       MRadialVelocity::Ref(MRadialVelocity::BARY, frame1));
  MRadialVelocity nrv1a = MRadialVelocity::Convert
    (rv1a, MRadialVelocity::Ref(MRadialVelocity::GEO))();
  MRadialVelocity nrv2a = MRadialVelocity::Convert
    (rv2a, MRadialVelocity::Ref(MRadialVelocity::GEO))();
  Double vel1a = nrv1a.getValue().getValue() / 1000.;    // km/s
  Double vel2a = nrv2a.getValue().getValue() / 1000.;    // km/s

  MDirection coord2(Quantity(175.425833,"deg"), Quantity(41.799167,"deg"),
                    MDirection::J2000);
  MEpoch epo2(Quantity(50417.625,"d"));     // 14-may-1996/15:00
  MPosition pos2(Quantity(1000,"m"),
                 Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                 MPosition::WGS84);         // near WSRT
  MeasFrame frame2(coord2,epo2,pos2);
  MRadialVelocity rv1b(Quantity(300,"km/s"),
                       MRadialVelocity::Ref(MRadialVelocity::BARY, frame2));
  MRadialVelocity rv2b(Quantity(200000,"m/s"),
                       MRadialVelocity::Ref(MRadialVelocity::BARY, frame2));
  MRadialVelocity nrv1b = MRadialVelocity::Convert
    (rv1b, MRadialVelocity::Ref(MRadialVelocity::GEO))().getValue();
  MRadialVelocity nrv2b = MRadialVelocity::Convert
    (rv2b, MRadialVelocity::Ref(MRadialVelocity::GEO))().getValue();
  Double vel1b = nrv1b.getValue().getValue() / 1000.;    // km/s
  Double vel2b = nrv2b.getValue().getValue() / 1000.;    // km/s
  //cout << "meas=" <<  vel1a<<' '<<vel2a<<' '<<vel1b<<' '<<vel2b<< endl;

  TableExprNode node1(tableCommand
                        ("using style python calc meas.rv ('GEO',"
                         "RVCOL, DIR[0,], TIME, POS1)"
                         " from tRadialVelocityEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
  AlwaysAssertExit (node1.unit().getName() == "km/s");
  Array<Double> arr1 = node1.getArrayDouble(0);
  //cout << "taql=" << arr1 << endl;
  AlwaysAssertExit (arr1.shape() == IPosition(1,2));
  AlwaysAssertExit (near(vel1a, arr1.data()[0], 1e-8));
  AlwaysAssertExit (near(vel2a, arr1.data()[1], 1e-8));
  Array<Double> arr2 = node1.getArrayDouble(1);
  AlwaysAssertExit (arr2.shape() == IPosition(1,2));
  AlwaysAssertExit (near(vel1b, arr2.data()[0], 1e-8));
  AlwaysAssertExit (near(vel2b, arr2.data()[1], 1e-8));
}

void testDopplerColumn()
{
  cout << "test doppler columns ..." << endl;
  // Check with Measures.
  MDoppler dop1(Quantity(0.2,""), MDoppler::RADIO);
  MDoppler dop2(Quantity(0.3,""), MDoppler::RADIO);
  MRadialVelocity mvel1(MRadialVelocity::fromDoppler(dop1));
  MRadialVelocity mvel2(MRadialVelocity::fromDoppler(dop2));
  Double vel1 = mvel1.getValue().getValue() / 1000.;    // km/s
  Double vel2 = mvel2.getValue().getValue() / 1000.;    // km/s
  ///cout << "meas=" << ndop << endl;
  TableExprNode node1(tableCommand
                      ("calc meas.radvel('LSRK',DOPPCOL)"
                       " from tRadialVelocityEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTScalar);
  AlwaysAssertExit (node1.unit().getName() == "km/s");
  Double val1 = node1.getDouble(0);
  ///cout << "taql=" << val1 << endl;
  AlwaysAssertExit (near(vel1, val1, 1e-8));
  Double val2 = node1.getDouble(1);
  ///cout << "taql=" << val2 << endl;
  AlwaysAssertExit (near(vel2, val2, 1e-8));
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
  nsucc += checkErr("calc meas.radialvelocity()");
  nsucc += checkErr("calc meas.radvel('BETA')");
  nsucc += checkErr("calc meas.radvel('LSRK')");
  nsucc += checkErr("calc meas.rv('BX',20)");
  nsucc += checkErr("calc meas.rv('LSRK',10,20)");
  nsucc += checkErr("calc meas.rv('BARY',10,'XZ')");
  nsucc += checkErr("calc meas.rv('BARY',10,'Z',20)");
  nsucc += checkErr("calc meas.rv('BARY',10 'm/s','Z')");
  nsucc += checkErr("calc meas.rv('BARY',DOPPCOL)");
  nsucc += checkErr("calc meas.rv(10)");
  nsucc += checkErr("calc meas.rv('BARY',20+3i)");
  nsucc += checkErr("calc meas.rv('BARY',200MHz)");
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
    testDopplerScalar();
    testArray();
    testDopplerArray();
    testColumn();
    testDopplerColumn();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
