//# dPointCompRep.cc:
//# Copyright (C) 1997,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casa/aips.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/ComponentShape.h>
#include <casa/Exceptions/Error.h>
#include <measures/Measures/MDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
void printShape(const ComponentShape& theShape) {
  cout << "This is a " << ComponentType::name(theShape.type())
       << " shape " << endl 
       << "with a reference direction of "
       << theShape.refDirection() << endl;
}

int main() {
  try {
    MDirection J1934_dir;
    { // get the right direction into J1934_dir
      Quantity J1934_ra; MVAngle::read(J1934_ra, "19:39:");
      Quantity J1934_dec; MVAngle::read(J1934_dec, "-63.43.");
      J1934_dir = MDirection(J1934_ra, J1934_dec, MDirection::J2000);
    }
    { // One way to construct the SkyComponent
      SkyComponent J1934(ComponentType::POINT, ComponentType::CONSTANT_SPECTRUM);
      J1934.shape().setRefDirection(J1934_dir);
      J1934.flux() = Flux<Double>(6.28, 0.1, 0.15, 0.01);
      printShape(J1934.shape());
    }
    { // An alternative way to construct the SkyComponent
      const Flux<Double> flux(6.28, 0.1, 0.15, 0.01);
      const PointShape shape(J1934_dir);
      const ConstantSpectrum spectrum;
      SkyComponent component(flux, shape, spectrum);
      printShape(component.shape());
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 dPointShape"
// End: 
