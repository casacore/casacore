//# tVelocityMachine.cc: This program tests the VelocityMachine class
//# Copyright (C) 1998,1999,2000,2002
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
#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <measures/Measures.h>
#include <measures/Measures/VelocityMachine.h>
#include <casa/Quanta/MVTime.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MDoppler.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {

  try {
    cout << "Test Velocity<->Frequency machine" << endl;
    cout << "--------------------------------------" << endl;
    MVTime dat(1998,5,10);
    MVPosition mvobs(Quantity(3828488.86, "m").getBaseValue(),
		     Quantity(443253.42, "m").getBaseValue(),
		     Quantity(5064977.78, "m").getBaseValue());
    MPosition obs(mvobs);
    MDirection dir((MVDirection(Quantity(0, "deg"),
			       Quantity(80, "deg"))));
    MeasFrame frame((MEpoch(MVEpoch(dat.day()))), obs, dir);
    
    cout << "Date:      " << dat.string(MVTime::YMD +
					MVTime::NO_TIME, 6) <<
      endl;
    cout << "Position:  " << obs.getValue().get() << endl;
    cout << "           " << obs.getAngle("deg") << endl;
    cout << "           " << obs.getValue().getLength("km") << endl;
    cout << "Direction: " << dir.getValue().get() << endl;
    cout << "           " << dir.getAngle("deg") << endl;
    
    
    MFrequency::Ref frqref(MFrequency::LSRK);
    MDoppler::Ref velref(MDoppler::RADIO);
    MVFrequency restfrq(QC::HI);
    cout << "Rest freq: "  << restfrq.get("GHz") << endl;

    VelocityMachine vm(frqref, Unit("GHz"), restfrq, velref, Unit("km/s"),
		       frame);
    cout << "------------------- Conversions ------" << endl;
    cout << "1410 MHz to RADIO: " << vm.makeVelocity(1.41) << endl;
    cout << "1410 MHz to RADIO: " << vm(MVFrequency(Quantity(1.41, "GHz"))) <<
      endl;
    Double bck(vm.makeVelocity(1.41).getValue());
    cout << "Back:              " << vm.makeFrequency(bck) << endl;
    cout << "Back:              " << vm(MVDoppler(Quantity(bck, "km/s"))) <<
      endl;
    vm.set(MFrequency::TOPO);
    cout << "1410 MHz to TOPO:  " << vm.makeVelocity(1.41) << endl;
    vm.set(MFrequency::LSRK);
    frqref.set(MFrequency(Quantity(1.405, "GHz"))); 
    cout << "Frequency offset:  " << *(frqref.offset()) << endl;
    vm.set(frqref);
    Vector<Double> fx(3);
    fx(0) = 0;
    fx(1) = 0.005;
    fx(2) = 0.010;
    cout << "Frequency list:    " << fx << endl;
    cout << "List to RADIO:     " << vm.makeVelocity(fx) << endl;
    Vector<Double> vbck(vm.makeVelocity(fx).getValue());
    cout << "Back:              " << vm.makeFrequency(vbck) << endl;
    velref.set(MDoppler(Quantity(1000, "km/s"), MDoppler::RADIO));
    cout << "Velocity offset:   " << *(velref.offset()) << endl;
    vm.set(velref);
    cout << "List to RADIO:     " << vm.makeVelocity(fx) << endl;
    cout << "--------Test copy-----------" << endl;
    VelocityMachine vmc(vm);
    cout << "List to RADIO:     " << vmc.makeVelocity(fx) << endl;
    cout << "--------Test constructors-----" << endl;
    VelocityMachine vma(frqref, Unit("GHz"), restfrq, velref, Unit("km/s"));
    cout << "List to RADIO: " << vma.makeVelocity(fx) << endl;
    VelocityMachine vmb(frqref, Unit("GHz"), restfrq,
			MFrequency::TOPO, velref, Unit("km/s"), frame);
    cout << "List to TOPO:  " << vmb.makeVelocity(fx) << endl;
    cout << "--------Test assignment-----" << endl;
    vma = vm;
    cout << "List to RADIO:     " << vma.makeVelocity(fx) << endl;
    cout << "--------Test reCalculate-----" << endl;
    vm.reCalculate();
    cout << "List to RADIO:     " << vm.makeVelocity(fx) << endl;
    cout << "--------Test (Quantum)-------" << endl;
    cout << "List(0) to RADIO: " << vm(Quantity(fx(0), "GHz")) << endl;
    cout << "--------Test set-------------" << endl;
    vm.set(Unit("GHz"));
    cout << "List to RADIO: " << vm.makeVelocity(fx) << endl;
    vm.set(restfrq);
    cout << "List to RADIO: " << vm.makeVelocity(fx) << endl;
    vm.set(frame);
    cout << "List to RADIO: " << vm.makeVelocity(fx) << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  return 0;
}
