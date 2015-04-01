//# tParAngleMachine.cc: This program tests the ParAngleMachine class
//# Copyright (C) 2002
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

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/ParAngleMachine.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

#include <casacore/casa/namespace.h>
int main() {

  try {
    cout << "Test Parallactic Angle  machine" << endl;
    cout << "---------------------------------------------" << endl;
    Timer tim;
    MPosition obs;
    MeasTable::Observatory(obs, "atca");
    Double dat(52332.2+1./24./15./10.);
    Quantity qdat(dat, "d");
    MVEpoch mvdat(dat);
    MEpoch  medat(mvdat, MEpoch::UTC);
    Vector<Double> vddat(5);
    Vector<MVEpoch> vmvdat(5);
    Vector<MEpoch> vmedat(5);
    for (uInt i=0; i<5; ++i) {
      vddat[i] = dat + i/12./15./10.;
      vmvdat[i] = MVEpoch(vddat[i]);
    };
    for (uInt i=0; i<5; ++i) {
      vmedat[i] = MEpoch(vmvdat[i], MEpoch::UTC);
    };
    Quantum<Vector<Double> > vqdat(vddat, "d"); 
    MeasFrame frame(medat, obs);
    MDirection::Ref refj2(MDirection::Ref(MDirection::J2000, frame));
    MDirection::Ref refaz(MDirection::Ref(MDirection::AZEL, frame));
    MDirection::Convert j2az(refj2, refaz);
    MDirection dir(Quantity(20, "deg"),
		   Quantity(-30, "deg"),
		   refj2);
    MDirection pol(Quantity(0, "deg"), Quantity(90, "deg"),
		   refj2);
    cout << "Position:  " << obs.getValue().get() << endl;
    cout << "           " << obs.getAngle("deg") << endl;
    cout << "           " << obs.getValue().getLength("km") << endl;
    cout << "Direction: " << dir.getValue().get() << endl;
    cout << "           " << dir.getAngle("deg") << endl;
    cout << "Time:      " << MVEpoch(dat) << endl;

    cout << "--------------- Full conversion to AZEL -----" << endl;

    for (uInt i=0; i<10; i+=2) {
      frame.set(MEpoch(Quantity(dat + i/24./15./10., "d")));
      MVDirection mvd(j2az(dir).getValue());
      if (i>0) cout << ", ";
      cout << setprecision(4) << 
	Quantity(mvd.positionAngle(j2az(pol).getValue()), "rad").get("deg");
    };
    cout << endl;

    cout << "--------------- Full machine through HADEC --" << endl;

    ParAngleMachine pam(dir);
    pam.set(frame);
    pam.setInterval(0.0);
    for (uInt i=0; i<10; i+=2) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << pam(MVEpoch(dat+i/24./15./10.)).get("deg");
    };
    cout << endl;

    cout << "--------------- Fast machine through HADEC --" << endl;

    pam.setInterval(0.04);
    for (uInt i=0; i<10; i+=2) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << pam(MVEpoch(dat+i/24./15./10.)).get("deg");
    };
    cout << endl;

    cout << "--------------- Quantity --------------------" << endl;
    for (uInt i=0; i<5; ++i) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << pam(Quantity(vddat[i], "d")).get("deg");
    };
    cout << endl;
    cout << "--------------- MVEpoch ---------------------" << endl;
    for (uInt i=0; i<5; ++i) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << pam(vmvdat[i]).get("deg");
    };
    cout << endl;
    cout << "--------------- MEpoch ----------------------" << endl; 
    for (uInt i=0; i<5; ++i) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << pam(vmedat[i]).get("deg"); 
    };   
    cout << endl; 
    cout << "--------------- Double ----------------------" << endl;
    for (uInt i=0; i<5; ++i) {
      if (i>0) cout << ", ";
      cout << setprecision(4) << Quantity(pam(vddat[i]), "rad").get("deg");
    };
    cout << endl;

    cout << "--------------- Vector versions -------------" << endl;
    cout << pam(vqdat).get("deg") << endl;
    cout << pam(vmvdat).get("deg") << endl;
    cout << pam(vmedat).get("deg") << endl;
    cout << Quantum<Vector<Double> >(pam(vddat), "rad").get("deg") << endl;

    cout << "--------------- Timing ----------------------" << endl;
    cout << ">>>" << endl;
    const uInt N=1000;
    tim.mark();
    for (uInt i=0; i<N; ++i) {
      frame.set(MEpoch(Quantity(dat + i/24./15./10., "d")));
      MVDirection mvd(j2az(dir).getValue());
      Quantity x(Quantity(mvd.positionAngle(j2az(pol).getValue()), "rad").
		 get("deg"));
    };
    cout << "Full AZEL  for N=" << N << ": " << tim.real() << endl;

    pam.setInterval(0.0);
    tim.mark();
    for (uInt i=0; i<N; ++i) {
      Quantity x(pam(MVEpoch(dat+i/24./15./10.)).get("deg"));
    };
    cout << "Full HADEC for N=" << N << ": " << tim.real() << endl; 

    pam.setInterval(0.04);
    tim.mark();
    for (uInt i=0; i<N; ++i) {
      Quantity x(pam(MVEpoch(dat+i/24./15./10.)).get("deg"));
    };
    cout << "Fast HADEC for N=" << N << ": " << tim.real() << endl; 

    cout << "<<<" << endl;

    cout << "---------------------------------------------" << endl;


  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 

  try {
    MDirection::Ref refj2(MDirection::J2000);
    MDirection dir(Quantity(20, "deg"),
                   Quantity(-30, "deg"),
                   refj2);
    ParAngleMachine pam(dir);
    cout << "------------- Expected exception ------------" << endl;
    Double result = pam(52230.0);
    cout << result << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  }

  cout << "---------------------------------------------" << endl;
  
  return 0;
}
