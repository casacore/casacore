//# ClassFileName.cc:  this defines ClassName, which ...
//# Copyright (C) 1998
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


#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/Quantum.h>
#include <aips/Utilities/String.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <iostream.h>

void printComponent(const SkyCompBase & comp);

int main() {
  try {
    const PointShape p(MDirection(Quantity(211, "deg"), 
 				  Quantity(-34, "deg"), MDirection::J2000));
    const SpectralIndex si(MFrequency(Quantity(1.415, "GHz")), -0.2);
    Flux<Double> f(12.5);
    f.setUnit("mJy");
    SkyComponent comp(f, p, si);
    printComponent(comp);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
void printComponent(const SkyCompBase & comp) {
  cout << "This component has a flux of " 
       << comp.flux().value().ac() 
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

