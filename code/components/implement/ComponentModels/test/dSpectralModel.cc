//# dSpectralModel.cc:
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
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <iostream.h>

void plotSpectrum(const Flux<Double> & refFlux,
                  const SpectralModel & modelSpectrum) {
  cout << "This is a " 
       << ComponentType::name(modelSpectrum.spectralShape())
       << " spectrum with a reference frequency of "
       << modelSpectrum.refFrequency().get("GHz") << endl
       << modelSpectrum.refFrequency().getRef() 
       << endl;
  cout << "Frequency\t Flux\n";
  const Quantum<Double> step(100.0, "MHz");
  Quantum<Double> sampleFreq = modelSpectrum.refFrequency().get("GHz");
  Flux<Double> modelFlux;
  for (uInt i = 0; i < 11; i++) {
    modelFlux = refFlux.copy();
    modelSpectrum.sample(modelFlux, MFrequency(sampleFreq));
    cout << sampleFreq.get("GHz")
	 << "\t\t " << modelFlux.value(0).re << " " 
	 << modelFlux.unit().getName() << endl;
    sampleFreq += step;
  }
}

int main() {
  try {
    SpectralIndex SImodel(MFrequency(Quantity(1.0, "GHz")), 0.5);
    plotSpectrum(Flux<Double>(1.0), SImodel);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 dSpectralModel"
// End: 
