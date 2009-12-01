//# tEarthMagneticMachine.cc: This program tests the EarthMagneticMachine class
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
#include <measures/Measures/EarthField.h>
#include <measures/Measures/EarthMagneticMachine.h>
#include <casa/Quanta/MVTime.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MEpoch.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {

  try {
    cout << "Test Earth Magnetic field machine" << endl;
    cout << "--------------------------------------" << endl;
    MVTime dat(1998,5,18);
    MVPosition mvobs(Quantity(3828488.86, "m").getBaseValue(),
		     Quantity(443253.42, "m").getBaseValue(),
		     Quantity(5064977.78, "m").getBaseValue());
    MPosition obs(mvobs);
    MeasFrame frame((MEpoch(MVEpoch(dat.day()))), obs);
    
    cout << "Date:      " << dat.string(MVTime::YMD +
					MVTime::NO_TIME, 6) <<
      endl;
    cout << "Position:  " << obs.getValue().get() << endl;
    cout << "           " << obs.getAngle("deg") << endl;
    cout << "           " << obs.getValue().getLength("km") << endl;
    
    EarthField ef(EarthField::STANDARD, dat.day());
    cout << "Result:    " << ef(obs.getValue()) << endl;
    
    cout << "----------- H=0 -- El=90 ------" << endl;
    {
      MDirection::Ref mvref(MDirection::ITRF, frame);
      MVDirection mvd(obs.getValue());
      EarthMagneticMachine fm(mvref, Quantum<Double>(0, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=0 -- El=90 ------" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(0, "deg"), Quantity(90, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(0, "km"),
			      obs, MEpoch(MVEpoch(dat.day())));
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
     cout << "----------- Along field ------" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      Vector<Double> xvd(3);
      xvd(0) = 18312;
      xvd(1) =  -381;
      xvd(2) = 45184;
      MVDirection mvd(xvd);
      EarthMagneticMachine fm(mvref, Quantum<Double>(0, "km"),
			      obs, MEpoch(MVEpoch(dat.day())));
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
   cout << "----------- Ha=0 -- Dec= 52.7316 ------" << endl;
    {
      MDirection::Ref mvref(MDirection::HADEC, frame);
      MVDirection mvd(Quantity(0, "deg"), Quantity( 52.7316, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(0, "km"),
			      obs, MEpoch(MVEpoch(dat.day())));
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=0 -- El=45 ------" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(0, "deg"), Quantity(45, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(0, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=200 -- El=45 ------" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(0, "deg"), Quantity(45, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(200, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=200 -- El=0 -- N ----" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(0, "deg"), Quantity(0, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(200, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=200 -- El=5 -- E ----" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(90, "deg"), Quantity(5, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(200, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
    }
    cout << "----------- H=200 -- El=5 -- W ----" << endl;
    {
      MDirection::Ref mvref(MDirection::AZEL, frame);
      MVDirection mvd(Quantity(-90, "deg"), Quantity(5, "deg"));
      EarthMagneticMachine fm(mvref, Quantum<Double>(200, "km"),
			      frame);
      fm.calculate(mvd);
      cout << "LOS:           " << fm.getLOSField() << endl;
      cout << "LOS:           " << fm.getLOSField("G") << endl;
      cout << "Long:          " << fm.getLong() << endl;
      cout << "Long:          " << fm.getLong("deg") << endl;
      cout << "LOS:           " << fm() << endl;
      cout << "LOS:           " << fm("G") << endl;
      cout << "------------- recalculate ----------" << endl;
      fm.reCalculate();
      cout << "LOS:           " << fm(mvd, "G") << endl;
      cout << "------------- use set() ------------" << endl;
      EarthMagneticMachine fm0;
      fm0.set(mvref);
      fm0.set(Quantity(200, "km"));
      fm0.set(frame);
      fm0.calculate(mvd);
      cout << "LOS:           " << fm0("G") << endl;
      EarthMagneticMachine fm1;
      fm1 = fm0;
      fm1.calculate(mvd);
      cout << "LOS:           " << fm1("G") << endl;
      cout << "LOS:           " << fm1(mvd) << endl;
      cout << "LOS:           " << fm1(mvd, "G") << endl;
      cout << "Long:          " << fm1.getLong(mvd) << endl;
      cout << "Long:          " << fm1.getLong(mvd, "deg") << endl;
      cout << "Field:         " << fm1.getField() << endl;
      cout << "Field:         " << fm1.getField(mvd) << endl;
      cout << "Pos:           " << fm1.getPosition() << endl;
      cout << "Pos:           " << fm1.getPosition(mvd) << endl;
      EarthMagneticMachine fm2(fm1);
      fm2.calculate(mvd);
      cout << "LOS:           " << fm2("G") << endl;
      cout << "------------- iterate height ------------" << endl;
      Quantum<Double> qhgt(200, "km"); 
      cout << "LOS:           " << fm1("G") << endl;
      cout << "LOS:           " << fm1(qhgt) << endl;
      cout << "LOS:           " << fm1(qhgt, "G") << endl;
      cout << "Long:          " << fm1.getLong() << endl;
      cout << "Long:          " << fm1.getLong("deg") << endl;
      cout << "Field:         " << fm1.getField() << endl;
      cout << "Pos:           " << fm1.getPosition() << endl;
      qhgt = Quantum<Double>(190, "km");
      cout << "LOS 190:       " << fm1(qhgt, "G") << endl;
      qhgt = Quantum<Double>(200, "km");
      cout << "LOS 200:       " << fm1(qhgt, "G") << endl;
      qhgt = Quantum<Double>(210, "km");
      cout << "LOS 210:       " << fm1(qhgt, "G") << endl;
    }
    
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  return 0;
}
