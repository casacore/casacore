//# ClassFileName.cc:  this defines ClassName, which ...
//# Copyright (C) 1998,1999,2000,2001
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
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Exceptions/Error.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <casa/Quanta/Quantum.h>
#include <casa/BasicSL/String.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/SpectralIndex.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
void printComponent(const SkyCompBase & comp);

int main() {
  try {
    const PointShape p((MDirection(Quantity(211, "deg"), 
 				  Quantity(-34, "deg"), MDirection::J2000)));
    const SpectralIndex si((MFrequency(Quantity(1.415, "GHz"))), -0.2);
    Flux<Double> f(12.5);
    f.setUnit("mJy");
    SkyComponent comp(f, p, si);
    printComponent(comp);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
void printComponent(const SkyCompBase & comp) {
  cout << "This component has a flux of " 
       << comp.flux().value() 
       << " " << comp.flux().unit().getName() << endl;
  cout << "and a " << ComponentType::name(comp.flux().pol()) 
       << " polarisation" << endl;
  cout << "This component has a " 
       << ComponentType::name(comp.shape().type()) << " shape" << endl;
  cout << "with a reference direction of " 
       << comp.shape().refDirection().getAngle("deg") << endl;
  cout << "This component has a " 
       << ComponentType::name(comp.spectrum().type()) << " spectrum" << endl;
  cout << "with a reference frequency of " 
       << comp.spectrum().refFrequency().get("GHz") << endl;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 dSkyCompBase"
// End: 

