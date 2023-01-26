//# tDopplerEngine.cc: Test program for DopplerEngine
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
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
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

void testDopplerScalar()
{
  cout << "test doppler scalars ..." << endl;
  // Convert a doppler from BETA to Z.
  MDoppler dop(Quantity(0.5,""), MDoppler::BETA);
  MVDoppler ndop = MDoppler::Convert
    (dop, MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "0.5, 'BETA')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    double val1 = node1.getDouble(0);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop.getValue(), val1, 1e-8));
  }
}

void testRadVelScalar()
{
  cout << "test radvel scalars ..." << endl;
  // Make a (BETA) doppler from the radvel and convert to Z.
  MRadialVelocity vel(Quantity(200,"km/s"), MRadialVelocity::BARY);
  MVDoppler ndop = MDoppler::Convert
    (vel.toDoppler(), MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "200 'km/s', 'BARY')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    double val1 = node1.getDouble(0);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop.getValue(), val1, 1e-8));
  }
}

void testFreqScalar()
{
  cout << "test freq scalars ..." << endl;
  // Make a (BETA) doppler from the freq/restfreq and convert to Z.
  // Use the rest freq of the CII166A line (from the Measures Lines table).
  MFrequency freq(Quantity(200,"MHz"), MFrequency::LSRK);
  MVFrequency rest(Quantity(1.425445,"GHz"));
  MVDoppler ndop = MDoppler::Convert
    (freq.toDoppler(rest), MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "200 MHz, 'LSRK', 1.425445GHz)").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    double val1 = node1.getDouble(0);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop.getValue(), val1, 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "200 MHz, 'LSRK', 'CII166A')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    double val1 = node1.getDouble(0);
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop.getValue(), val1, 1e-8));
  }
}

void testDopplerArray()
{
  cout << "test doppler arrays ..." << endl;
  // Convert a doppler from BETA to OPTICAL.
  MDoppler dop1(Quantity(0.5,""), MDoppler::RADIO);
  MVDoppler ndop1 = MDoppler::Convert
    (dop1, MDoppler::Ref(MDoppler::OPTICAL))().getValue();
  MDoppler dop2(Quantity(0.6,""), MDoppler::RADIO);
  MVDoppler ndop2 = MDoppler::Convert
    (dop2, MDoppler::Ref(MDoppler::OPTICAL))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('OPTICAL',"
                         "[0.5, 0.6], 'RADIO')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndop2.getValue(), arr1.data()[1], 1e-8));
  }
}

void testRadVelArray()
{
  cout << "test radvel arrays ..." << endl;
  // Make a (BETA) doppler from the radvel and convert to RADIO.
  MRadialVelocity vel1(Quantity(200,"km/s"), MRadialVelocity::BARY);
  MVDoppler ndop1 = MDoppler::Convert
    (vel1.toDoppler(), MDoppler::Ref(MDoppler::RADIO))().getValue();
  MRadialVelocity vel2(Quantity(300,"km/s"), MRadialVelocity::BARY);
  MVDoppler ndop2 = MDoppler::Convert
    (vel2.toDoppler(), MDoppler::Ref(MDoppler::RADIO))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('RADIO',"
                         "[200,300] 'km/s', 'BARY')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndop1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndop2.getValue(), arr1.data()[1], 1e-8));
  }
}

void testFreqArray()
{
  cout << "test freq arrays ..." << endl;
  // Make a (BETA) doppler from the freq/restfreq and convert to Z.
  // Use the rest freq of the CII166A and H110A lines.
  MFrequency freq1(Quantity(200,"MHz"), MFrequency::LSRK);
  MFrequency freq2(Quantity(300,"MHz"), MFrequency::LSRK);
  MVFrequency rest1(Quantity(1.425445,"GHz"));
  MVFrequency rest2(Quantity(4.874157,"GHz"));
  MVDoppler ndopa1 = MDoppler::Convert
    (freq1.toDoppler(rest1), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndopa2 = MDoppler::Convert
    (freq2.toDoppler(rest2), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndopb1 = MDoppler::Convert
    (freq1.toDoppler(rest1), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndopb2 = MDoppler::Convert
    (freq1.toDoppler(rest2), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndopc1 = MDoppler::Convert
    (freq1.toDoppler(rest1), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndopc2 = MDoppler::Convert
    (freq2.toDoppler(rest1), MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "[[200,300]] MHz, 'LSRK', [[1.425445GHz,4.874157GHz]])").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(2,2,1));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndopa1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndopa2.getValue(), arr1.data()[1], 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "[200,300] MHz, 'LSRK', ['CII166A','H110A'])").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndopa1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndopa2.getValue(), arr1.data()[1], 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "200 MHz, 'LSRK', ['CII166A','H110A'])").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndopb1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndopb2.getValue(), arr1.data()[1], 1e-8));
  }
  {
    TableExprNode node1(tableCommand
                        ("calc meas.doppler ('Z',"
                         "[200,300] MHz, 'LSRK', 'CII166A')").node());
    AlwaysAssertExit (node1.getNodeRep()->isConstant());
    AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
    Array<double> arr1 = node1.getArrayDouble(0);
    ///cout<<arr1.shape()<<endl;
    AlwaysAssertExit (arr1.shape() == IPosition(1,2));
    ///cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node1.unit().getName() == "");
    AlwaysAssertExit (near(ndopc1.getValue(), arr1.data()[0], 1e-8));
    AlwaysAssertExit (near(ndopc2.getValue(), arr1.data()[1], 1e-8));
  }
}

void testDopplerColumn()
{
  cout << "test doppler columns ..." << endl;
  // Check with Measures.
  MDoppler dop1(Quantity(0.5,""), MDoppler::RADIO);
  MVDoppler ndop1 = MDoppler::Convert
    (dop1, MDoppler::Ref(MDoppler::OPTICAL))().getValue();
  MDoppler dop2(Quantity(0.6,""), MDoppler::RADIO);
  MVDoppler ndop2 = MDoppler::Convert
    (dop2, MDoppler::Ref(MDoppler::OPTICAL))().getValue();
  ///cout << "meas=" << ndop << endl;
  TableExprNode node1(tableCommand
                      ("calc meas.doppler ('OPTICAL',"
                       "DOPPCOL) from tDopplerEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTScalar);
  AlwaysAssertExit (node1.unit().getName() == "");
  double val1 = node1.getDouble(0);
  ///cout << "taql=" << arr1 << endl;
  AlwaysAssertExit (near(ndop1.getValue(), val1, 1e-8));
  double val2 = node1.getDouble(1);
  AlwaysAssertExit (near(ndop2.getValue(), val2, 1e-8));
}

void testRadVelColumn()
{
  cout << "test radvel columns ..." << endl;
  // Check with Measures.
  // Make a (BETA) doppler from the radvel and convert to Z.
  MRadialVelocity vel1(Quantity(200,"km/s"), MRadialVelocity::BARY);
  MRadialVelocity vel2(Quantity(300,"km/s"), MRadialVelocity::BARY);
  MVDoppler ndop1 = MDoppler::Convert
    (vel1.toDoppler(), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndop2 = MDoppler::Convert
    (vel2.toDoppler(), MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  TableExprNode node1(tableCommand
                      ("calc meas.doppler ('Z',"
                       "RVCOL) from tDopplerEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTArray);
  AlwaysAssertExit (node1.unit().getName() == "");
  Array<double> arr1 = node1.getArrayDouble(0);
  ///cout<<arr1.shape()<<endl;
  AlwaysAssertExit (arr1.shape() == IPosition(1,2));
  ///cout << "taql=" << arr1 << endl;
  AlwaysAssertExit (near(ndop1.getValue(), arr1.data()[0], 1e-8));
  AlwaysAssertExit (near(ndop2.getValue(), arr1.data()[1], 1e-8));
  Array<double> arr2 = node1.getArrayDouble(1);
  AlwaysAssertExit (arr2.shape() == IPosition(1,2));
  AlwaysAssertExit (near(ndop2.getValue(), arr2.data()[0], 1e-8));
  AlwaysAssertExit (near(ndop1.getValue(), arr2.data()[1], 1e-8));
}

void testFreqColumn()
{
  cout << "test frequency columns ..." << endl;
  // Check with Measures.
  MFrequency freq1(Quantity(200,"MHz"), MFrequency::LSRK);
  MFrequency freq2(Quantity(300,"MHz"), MFrequency::LSRK);
  MVFrequency rest1(Quantity(1.425445,"GHz"));
  MVFrequency rest2(Quantity(4.874157,"GHz"));
  MVDoppler ndop1 = MDoppler::Convert
    (freq1.toDoppler(rest1), MDoppler::Ref(MDoppler::Z))().getValue();
  MVDoppler ndop2 = MDoppler::Convert
    (freq2.toDoppler(rest2), MDoppler::Ref(MDoppler::Z))().getValue();
  ///cout << "meas=" << ndop << endl;
  TableExprNode node1(tableCommand
                      ("calc meas.doppler ('Z',"
                       "FREQ,RESTFREQ) from tDopplerEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.getNodeRep()->valueType() == TableExprNodeRep::VTScalar);
  AlwaysAssertExit (node1.unit().getName() == "");
  double val1 = node1.getDouble(0);
  ///cout << "taql=" << arr1 << endl;
  AlwaysAssertExit (near(ndop1.getValue(), val1, 1e-8));
  double val2 = node1.getDouble(1);
  AlwaysAssertExit (near(ndop2.getValue(), val2, 1e-8));
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
  nsucc += checkErr("calc meas.redshift()");
  nsucc += checkErr("calc meas.doppler('BETA')");
  nsucc += checkErr("calc meas.doppler('BETX',20)");
  nsucc += checkErr("calc meas.doppler('BETA',10,20)");
  nsucc += checkErr("calc meas.doppler('BETA',10,'XZ')");
  nsucc += checkErr("calc meas.doppler('BETA',10,'Z',20)");
  nsucc += checkErr("calc meas.doppler('BETA','Z',20)");
  nsucc += checkErr("calc meas.doppler('BETA',20+3i,'Z')");
  nsucc += checkErr("calc meas.doppler('BETA',200MHz)");
  nsucc += checkErr("calc meas.doppler('BETA',200MHz,'LSRK')");
  nsucc += checkErr("calc meas.doppler('BETA',200MHz,'LSRK',1j)");
  nsucc += checkErr("calc meas.doppler('BETA',200MHz,'LSRK','justaline')");
  nsucc += checkErr("calc meas.doppler('BETA',[200,300]MHz,'LSRK',[100,110,120])");
  nsucc += checkErr("calc meas.doppler('BETA',200 's/km')");
  AlwaysAssertExit(nsucc == 0);
}


int main()
{
  try {
    // Register the MEAS functions in TaQL.
    register_meas();
    // Execute some tests.
    testErr();
    testDopplerScalar();
    testRadVelScalar();
    testFreqScalar();
    testDopplerArray();
    testRadVelArray();
    testFreqArray();
    testDopplerColumn();
    testRadVelColumn();
    testFreqColumn();
  } catch (const std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
