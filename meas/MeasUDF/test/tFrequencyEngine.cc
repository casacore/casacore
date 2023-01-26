//# tFrequencyEngine.cc: Test program for FrequencyEngine
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
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MDirection.h>
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

void testScalar()
{
  cout << "test scalars ..." << endl;
  // Convert a frequency for a given frame.
  MDirection coord(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                   MDirection::B1950);
  MEpoch epo(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  MPosition pos(Quantity(10,"m"),
                Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                MPosition::WGS84);         // near WSRT
  MeasFrame frame(coord,epo,pos);
  MeasFrame rvframe(coord,epo,pos);

  MFrequency freq(Quantity(1e9, "Hz"), MFrequency::LSRK);
  double res = MFrequency::Convert
    (freq, MFrequency::Ref(MFrequency::BARY,frame))().getValue().getValue();
  //cout << "meas=" << res << endl;

  MRadialVelocity radvel(Quantity(1000, "km/s"),
                         MRadialVelocity::Ref(MRadialVelocity::BARY, rvframe));
  frame.set (radvel);
  double res1 = MFrequency::Convert
    (freq, MFrequency::Ref(MFrequency::REST, frame))().getValue().getValue();
  //cout << "meas=" << res1 << endl;
  {
    TableExprNode node(tableCommand
                       ("calc meas.freq('BARY',1GHz, 'LSRK',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 << endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(res, val1, 1e-8));
  }
  {
    // Specify frequency as a period.
    TableExprNode node(tableCommand
                       ("calc meas.freq('BARY',1e-9 s, 'LSRK',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 << endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(res, val1, 1e-8));
  }
  {
    // Specify frequency as a wavelength.
    TableExprNode node(tableCommand
                       ("calc meas.freq('BARY',c()/1GHz, 'LSRK',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 << endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(res, val1, 1e-8));
  }
  {
    // Convert to rest which requires radvel.
    TableExprNode node(tableCommand
                       ("calc meas.rest(1GHz, 'LSRK',"
                        "1000 'km/s', 'BARY',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 <<endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(res1, val1, 1e-8));
  }
  {
    // Convert to rest which requires radvel.
    TableExprNode node(tableCommand
                       ("calc meas.freq('rest',1GHz, 'LSRK',"
                        "1000 'km/s', 'BARY',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 <<endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(res1, val1, 1e-8));
  }
  {
    // Convert from rest which requires radvel.
    TableExprNode node(tableCommand
                       ("calc meas.freq('LSRK',"
                        "1.0033631110721GHz, 'REST',"
                        "1000 'km/s', 'BARY',"
                        "185.425833deg, 31.799167deg,'B1950',"
                        "mjdtodate(50217.625d),'UTC',"
                        "6.60417deg, 52.8deg, 10m, 'WGS84')").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << val1 << ' '<<val1-1e9<<endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(1e9, val1, 1e-8));
  }
}

void testDopplerScalar()
{
  cout << "test doppler scalars ..." << endl;
  MDoppler dop(MVDoppler(3.5), MDoppler::RADIO);
  MFrequency freq(Quantity(1e9, "Hz"), MFrequency::LSRK);
  // Get the rest frequency.
  MFrequency rfreq = freq.toRest (dop);
  //cout << "meas=" << rfreq.getValue().getValue()<<endl;
  // And back.
  MFrequency bfreq = MFrequency::fromDoppler (dop, rfreq.getValue());
  //cout << "meas=" << bfreq.getValue().getValue()<<endl;
  {
    TableExprNode node(tableCommand
                       ("calc meas.rest(1GHz, 'LSRK', 3.5)").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(rfreq.getValue().getValue(), val1, 1e-8));
  }
  {
    TableExprNode node(tableCommand
                       ("calc meas.shift(400MHz, 'LSRK', 3.5)").node());
    AlwaysAssertExit (node.getNodeRep()->isConstant());
    double val1 = node.getDouble(0);
    //cout << "taql=" << arr1 << endl;
    AlwaysAssertExit (node.unit().getName() == "Hz");
    AlwaysAssertExit (near(bfreq.getValue().getValue(), val1, 1e-8));
  }
}

void testArray()
{
  cout << "test arrays ..." << endl;
  // Convert a few frequencies from J2000.
  TableExprNode node1(tableCommand
                      ("calc meas.freq ('LSRK', [[200],[220]]MHz, 'BARY',"
                       "[185.425833deg, 31.799167deg,"
                       "175.425833deg, 41.799167deg,"
                       "165.425833deg, 51.799167deg,"
                       "155.425833deg, 61.799167deg],'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[6.60417deg, 52.8deg, -60.60417deg, -32.8deg],"
                       "[10m,1000m], 'WGS84')").node());
  TableExprNode node2(tableCommand
                      ("calc meas.rest ([[200],[220]]MHz, 'BARY',"
                       "[500,600,700]'km/s','LSRK',"
                       "[185.425833deg, 31.799167deg,"
                       "175.425833deg, 41.799167deg,"
                       "165.425833deg, 51.799167deg,"
                       "155.425833deg, 61.799167deg],'J2000',"
                       "mjdtodate([50217.625d,50417.625d,50617.625]),'UTC',"
                       "[6.60417deg, 52.8deg, -60.60417deg, -32.8deg],"
                       "[10m,1000m], 'WGS84')").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  Array<double> arr1 = node1.getArrayDouble(0);
  Array<double> arr2 = node2.getArrayDouble(0);
  //cout << "taql=" << arr1 << endl;
  //cout << "taql=" << arr2 << endl;
  AlwaysAssertExit (arr1.shape() == IPosition(5,1,2,4,3,2));
  AlwaysAssertExit (arr2.shape() == IPosition(6,1,2,3,4,3,2));
  VectorIterator<double> arr1iter(arr1);
  VectorIterator<double> arr2iter(arr2);
  // Check with Measures.
  Vector<MFrequency> freq(2);
  Vector<MDirection> coord(4);
  Vector<MEpoch> epo(3);
  Vector<MPosition> pos(2);
  Vector<double> vel(3);
  freq[0] = MFrequency(Quantity(200, "MHz"), MFrequency::BARY);
  freq[1] = MFrequency(Quantity(220, "MHz"), MFrequency::BARY);
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
  vel[0] = 500;
  vel[1] = 600;
  vel[2] = 700;
  for (uint32_t ip=0; ip<pos.size(); ++ip) {
    for (uint32_t ie=0; ie<epo.size(); ++ie) {
      for (uint32_t ic=0; ic<coord.size(); ++ic) {
        for (uint32_t ir=0; ir<freq.size(); ++ir) {
          double fr = MFrequency::Convert
            (freq[ir],
             MFrequency::Ref(MFrequency::LSRK,
                             MeasFrame(coord[ic],epo[ie],pos[ip])))()
            .getValue().getValue();
          //cout << "meas=" << fr << ' '<< arr1iter.vector() << endl;
          AlwaysAssertExit (allNear(fr, arr1iter.vector(), 1e-8));
          arr1iter.next();
        }
      }
    }
  }
  for (uint32_t ip=0; ip<pos.size(); ++ip) {
    for (uint32_t ie=0; ie<epo.size(); ++ie) {
      for (uint32_t ic=0; ic<coord.size(); ++ic) {
        MeasFrame frame(coord[ic],epo[ie],pos[ip]);
        MeasFrame rvframe(coord[ic],epo[ie],pos[ip]);
        for (uint32_t iv=0; iv<vel.size(); ++iv) {
          MRadialVelocity vl(Quantity(vel[iv],"km/s"),
                             MRadialVelocity::Ref(MRadialVelocity::LSRK, rvframe));
          frame.set (vl);
          for (uint32_t ir=0; ir<freq.size(); ++ir) {
            double fr = MFrequency::Convert
              (freq[ir], MFrequency::Ref(MFrequency::REST, frame))()
              .getValue().getValue();
            //cout << "meas=" << fr << ' '<< arr1iter.vector() << endl;
            AlwaysAssertExit (allNear(fr, arr2iter.vector(), 1e-8));
            arr2iter.next();
          }
        }
      }
    }
  }
}

void testDopplerArray()
{
  cout << "test doppler arrays ..." << endl;
  Vector<MDoppler> dop(2);
  dop[0] = MDoppler(MVDoppler(3.5), MDoppler::RADIO);
  dop[1] = MDoppler(MVDoppler(2.5), MDoppler::RADIO);
  Vector<MFrequency> freq(3);
  freq[0] = MFrequency(Quantity(1.0e9, "Hz"), MFrequency::LSRK);
  freq[1] = MFrequency(Quantity(1.1e9, "Hz"), MFrequency::LSRK);
  freq[2] = MFrequency(Quantity(1.2e9, "Hz"), MFrequency::LSRK);
  TableExprNode node1(tableCommand
                      ("calc meas.rest([1,1.1,1.2]GHz, 'LSRK', [3.5,2.5])").node());
  TableExprNode node2(tableCommand
                      ("calc meas.shift([400,440,480]MHz, 'LSRK', [3.5])").node());
  TableExprNode node3(tableCommand
                      ("calc meas.shift([400,440,480]MHz/0.6, 'LSRK', 2.5)").node());
  AlwaysAssertExit (node1.getNodeRep()->isConstant());
  AlwaysAssertExit (node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node3.getNodeRep()->isConstant());
  Array<double> arr1 = node1.getArrayDouble(0);
  Array<double> arr2 = node2.getArrayDouble(0);
  Array<double> arr3 = node3.getArrayDouble(0);
  //cout << "taql=" << arr1 << endl;
  //cout << "taql=" << arr2 << endl;
  //cout << "taql=" << arr3 << endl;
  AlwaysAssertExit (node1.unit().getName() == "Hz");
  AlwaysAssertExit (node2.unit().getName() == "Hz");
  AlwaysAssertExit (node3.unit().getName() == "Hz");
  AlwaysAssertExit (arr1.shape() == IPosition(2,3,2));
  AlwaysAssertExit (arr2.shape() == IPosition(2,3,1));
  AlwaysAssertExit (arr3.shape() == IPosition(2,3,1));
  for (uint32_t id=0; id<dop.size(); ++id) {
    for (uint32_t ir=0; ir<freq.size(); ++ir) {
      // Get the rest frequency.
      MFrequency rfreq = freq[ir].toRest (dop[id]);
      //cout << "meas=" << rfreq.getValue().getValue()<<endl;
      AlwaysAssertExit (near(rfreq.getValue().getValue(), arr1.data()[3*id+ir], 1e-8));
      // And back.
      MFrequency bfreq = MFrequency::fromDoppler (dop[id], rfreq.getValue());
      //cout << "meas=" << bfreq.getValue().getValue()<<endl;
      if (id == 0) {
        AlwaysAssertExit (allNear(bfreq.getValue().getValue(), arr2.data()[ir], 1e-8));
      } else {
        AlwaysAssertExit (allNear(bfreq.getValue().getValue(), arr3.data()[ir], 1e-8));
      }
    }
  }
}

void testColumn()
{
  cout << "test columns ..." << endl;
  // Check with Measures.
  Vector<MFrequency> freq(2);
  Vector<MDoppler> dop(2);
  Vector<double> vel(4);
  Vector<MDirection> coord(2);
  Vector<MEpoch> epo(2);
  Vector<MPosition> pos(2);
  freq[0] = MFrequency(Quantity(0.2, "GHz"), MFrequency::LSRK);
  freq[1] = MFrequency(Quantity(0.3, "GHz"), MFrequency::LSRK);
  dop[0] = MDoppler(MVDoppler(0.5), MDoppler::RADIO);
  dop[1] = MDoppler(MVDoppler(0.6), MDoppler::RADIO);
  vel[0] = 2e5;
  vel[1] = 3e5;
  vel[2] = 3e5;
  vel[3] = 2e5;
  coord[0] = MDirection(Quantity(185.425833,"deg"), Quantity(31.799167,"deg"),
                        MDirection::J2000);
  coord[1] = MDirection(Quantity(175.425833,"deg"), Quantity(41.799167,"deg"),
                        MDirection::J2000);
  epo[0] = MEpoch(Quantity(50217.625,"d"));     // 14-may-1996/15:00
  epo[1] = MEpoch(Quantity(50417.625,"d"));     // 30-nov-1996/15:00
  pos[0] = MPosition(Quantity(10,"m"),
                     Quantity(6.60417,"deg"), Quantity(52.8,"deg"),
                     MPosition::WGS84);         // near WSRT
  pos[1] = MPosition(Quantity(1000,"m"),
                     Quantity(-60.60417,"deg"), Quantity(-32.8,"deg"),
                     MPosition::WGS84);
  // Convert a few frequencies.
  TableExprNode node1(tableCommand
                      ("using style python calc meas.freq("
                       "'BARY',FREQ,"
                       "DIR[0,],TIME,POS1) "
                       "from tFrequencyEngine_tmp.tab").node());
  TableExprNode node2(tableCommand
                      ("using style python calc meas.rest("
                       "FREQ,RVCOL,"
                       "DIR[0,],TIME,POS1) "
                       "from tFrequencyEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (! node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.nrow() == 2  &&  node2.nrow() == 2);
  for (uint32_t i=0; i<2; ++i) {
    double val1 = node1.getDouble(i);
    //cout << "taql=" << val1 << endl;
    Array<double> arr2 = node2.getArrayDouble(i);
    //cout << "taql=" << arr2 << endl;
    AlwaysAssertExit (arr2.shape() == IPosition(5,1,2,1,1,1));
    MeasFrame frame(coord[i],epo[i],pos[i]);
    MeasFrame rvframe(coord[i],epo[i],pos[i]);
    double fr = MFrequency::Convert
      (freq[i], MFrequency::Ref(MFrequency::BARY, frame))()
      .getValue().getValue();
    //cout << "meas=" << fr << ' '<< val1 << endl;
    AlwaysAssertExit (near(fr, val1, 1e-8));
    for (int j=0; j<2; ++j) {
      MRadialVelocity rv(Quantity(vel[2*i+j], "m/s"),
                         MRadialVelocity::Ref(MRadialVelocity::BARY, rvframe));
      frame.set (rv);
      double fr = MFrequency::Convert
        (freq[i], MFrequency::Ref(MFrequency::REST, frame))()
        .getValue().getValue();
      AlwaysAssertExit (near(fr, arr2.data()[j], 1e-8));
    }
  }
}

void testDopplerColumn()
{
  cout << "test doppler columns ..." << endl;
  // Check with Measures.
  Vector<MFrequency> freq(2);
  Vector<MDoppler> dop(2);
  freq[0] = MFrequency(Quantity(0.2, "GHz"), MFrequency::LSRK);
  freq[1] = MFrequency(Quantity(0.3, "GHz"), MFrequency::LSRK);
  dop[0] = MDoppler(MVDoppler(0.5), MDoppler::RADIO);
  dop[1] = MDoppler(MVDoppler(0.6), MDoppler::RADIO);
  // Convert a few frequencies.
  TableExprNode node1(tableCommand
                      ("calc meas.rest(FREQ, DOPPCOL) "
                       "from tFrequencyEngine_tmp.tab").node());
  TableExprNode node2(tableCommand
                      ("calc meas.shift(FREQ, DOPPCOL) "
                       "from tFrequencyEngine_tmp.tab").node());
  AlwaysAssertExit (! node1.getNodeRep()->isConstant());
  AlwaysAssertExit (! node2.getNodeRep()->isConstant());
  AlwaysAssertExit (node1.nrow() == 2  &&  node2.nrow() == 2);
  for (uint32_t i=0; i<2; ++i) {
    double val1 = node1.getDouble(i);
    double val2 = node2.getDouble(i);
    //cout << "taql=" << val1 << endl;
    MFrequency rfreq = freq[i].toRest (dop[i]);
    //cout << "meas=" << fr << ' '<< val1 << endl;
    AlwaysAssertExit (near(rfreq.getValue().getValue(), val1, 1e-8));
    MFrequency bfreq = MFrequency::fromDoppler (dop[i], freq[i].getValue());
    AlwaysAssertExit (near(bfreq.getValue().getValue(), val2, 1e-8));
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
  nsucc += checkErr("using style python calc meas.freqx()");
  nsucc += checkErr("using style python calc meas.freq()");
  nsucc += checkErr("using style python calc meas.frequency()");
  nsucc += checkErr("using style python calc meas.rest()");
  nsucc += checkErr("using style python calc meas.restfreq()");
  nsucc += checkErr("using style python calc meas.restfrequency()");
  nsucc += checkErr("using style python calc meas.shift()");
  nsucc += checkErr("using style python calc meas.shiftfreq()");
  nsucc += checkErr("using style python calc meas.shiftfrequency()");
  nsucc += checkErr("using style python calc meas.freq(10)");
  nsucc += checkErr("using style python calc meas.freq('BARX',10)");
  nsucc += checkErr("using style python calc meas.freq('BARY',10,'LSRX')");
  nsucc += checkErr("using style python calc meas.freq('BARY',10,'REST')");
  nsucc += checkErr("using style python calc meas.freq('BARY')");
  nsucc += checkErr("using style python calc meas.freq('BARY','LSRK')");
  nsucc += checkErr("using style python calc meas.shift('BARY',10,'LSRK')");
  nsucc += checkErr("using style python calc meas.shift(10,'LSRK')");
  nsucc += checkErr("using style python calc meas.shift(10,'LSRX')");
  nsucc += checkErr("using style python calc meas.shift('BARY',10)");
  nsucc += checkErr("using style python calc meas.shift(10,'LSRK',2,3)");
  nsucc += checkErr("using style python calc meas.shift(10,'LSRK',2MHz)");
  nsucc += checkErr("using style python calc meas.rest('LSRK',2MHz)");
  nsucc += checkErr("using style python calc meas.rest(10,'LSRK',2MHz)");
  nsucc += checkErr("using style python calc meas.freq('rest',10,'LSRK',2MHz)");
  nsucc += checkErr("using style python calc meas.freq('lsrk',10,'rest',2MHz)");
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
