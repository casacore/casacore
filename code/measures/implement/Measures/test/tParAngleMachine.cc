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
#include <aips/aips.h>
#include <trial/Measures/ParAngleMachine.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Arrays/Vector.h>
#include <aips/iostream.h>

int main() {

  try {
    cout << "Test Parallactic Angle  machine" << endl;
    cout << "--------------------------------------" << endl;
    MPosition obs;
    MeasTable::Observatory(obs, "atca");
    Double dat(52332.2+1./24./15./10.);
    MeasFrame frame((MEpoch(Quantity(dat, "d"))), obs);
    MDirection dir(Quantity(20, "deg"),
		   Quantity(-30, "deg"),
		   MDirection::Ref(MDirection::J2000, frame));
    cout << "Position:  " << obs.getValue().get() << endl;
    cout << "           " << obs.getAngle("deg") << endl;
    cout << "           " << obs.getValue().getLength("km") << endl;
    cout << "Direction: " << dir.getValue().get() << endl;
    cout << "           " << dir.getAngle("deg") << endl;

    ParAngleMachine pam(dir);
    pam.set(frame);
    cout << "Time:      " << MVEpoch(dat) << endl;;;
    cout << "Par angle: " <<
      pam(MVEpoch(dat)) <<
      endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  exit(0);
}
