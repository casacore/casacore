//# tSpectralModel.cc:  tests the SpectralModel class
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
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Glish/GlishRecord.h>
#include <iomanip.h>
#include <iostream.h>

void plotSpectrum(const Flux<Double> & refFlux,
		  const SpectralModel & modelSpectrum);
int main() {
  try {
    Vector<Double> indices(4);
    indices(0) = 1.0; indices(1) = 0.5; indices(2) = 0.5; indices(3) = -2.0; 
    const SpectralIndex siModel(MFrequency(Quantity(1, "GHz")), indices);
    const Flux<Double> flux(1.0, 0.1, 0.0, 0.01);
    plotSpectrum(flux, siModel);
    {
      Record rec;
      String errorMessage;
      AlwaysAssert(siModel.toRecord(errorMessage, rec), AipsError);
      AlwaysAssert(SpectralModel::getType(errorMessage, rec) ==
		   ComponentType::SPECTRAL_INDEX, AipsError);
      const String freqString("frequency");
      AlwaysAssert(rec.isDefined(freqString), AipsError);
      RecordFieldId frequency(freqString);
      AlwaysAssert(rec.dataType(frequency) == TpRecord, AipsError);
      Record fRec = rec.asRecord(frequency);
      {
	MeasureHolder mh;
	AlwaysAssert(mh.fromRecord(errorMessage, fRec), AipsError);
	AlwaysAssert(mh.isMeasure(), AipsError);
	AlwaysAssert(mh.isMFrequency(), AipsError);
      }
      SpectralIndex otherSiModel;
      AlwaysAssert(otherSiModel.fromRecord(errorMessage, rec), AipsError);
      AlwaysAssert(otherSiModel.refFrequency().getValue() ==
		   siModel.refFrequency().getValue(), AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}

void plotSpectrum(const Flux<Double> & refFlux,
                  const SpectralModel & modelSpectrum) {
  cout << "This is a "
       << ComponentType::name(modelSpectrum.type())
       << " spectrum with a reference frequency of: "
       << modelSpectrum.refFrequency().get("GHz") << endl
       << modelSpectrum.refFrequency().getRef()
       << endl;
  Vector<Double> parms(modelSpectrum.nParameters());
  modelSpectrum.parameters(parms);
  cout << "The parameters are: " << parms.ac() << endl;
  const MVFrequency step(Quantity(100.0, "MHz"));
  MVFrequency sampleFreq = modelSpectrum.refFrequency().getValue();
  Flux<Double> modelFlux;
  cout << "Frequency\t I-Flux\t Q-Flux\t U-Flux\t V-Flux\n";
  for (uInt i = 0; i < 11; i++) {
    modelFlux = refFlux.copy();
    modelSpectrum.sample(modelFlux,
			 MFrequency(sampleFreq,
				    modelSpectrum.refFrequency().getRef()));
    modelFlux.convertPol(ComponentType::STOKES);
    cout << setprecision(3) << sampleFreq.get("GHz")
 	 << "\t\t " << modelFlux.value(0u).re
 	 << "\t " << modelFlux.value(1u).re
 	 << "\t " << modelFlux.value(2u).re
 	 << "\t " << modelFlux.value(3u).re
 	 << " " << modelFlux.unit().getName() << endl;
    sampleFreq += step;
  }
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSpectralModel"
// End: 

