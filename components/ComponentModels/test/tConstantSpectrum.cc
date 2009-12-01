//# tSpectralModel.cc:  tests the SpectralModel class
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
#include <components/ComponentModels/ConstantSpectrum.h>
// #include <components/ComponentModels/Flux.h>
// #include <components/ComponentModels/SpectralModel.h>
// #include <measures/Measures/MeasureHolder.h>
// #include <casa/Arrays/Vector.h>
// #include <casa/Containers/Record.h>
// #include <casa/Containers/RecordFieldId.h>
#include <casa/Exceptions/Error.h>
// #include <measures/Measures/MFrequency.h>
// #include <casa/Quanta/MVFrequency.h>
// #include <casa/Quanta/Quantum.h>
// #include <casa/Utilities/Assert.h>
// #include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>
// #include <aips/Glish/GlishRecord.h>
// #include <casa/iomanip.h>
#include <casa/iostream.h>


#include <casa/namespace.h>
// void plotSpectrum(const Flux<Double> & refFlux,
// 		  const SpectralModel & modelSpectrum);
int main() {
  try {
    const ConstantSpectrum spModel;
    /*
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
    */
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}

// void plotSpectrum(const Flux<Double> & refFlux,
//                   const SpectralModel & modelSpectrum) {
//   cout << "This is a "
//        << ComponentType::name(modelSpectrum.type())
//        << " spectrum with a reference frequency of: "
//        << modelSpectrum.refFrequency().get("GHz") << endl
//        << modelSpectrum.refFrequency().getRef()
//        << endl;
//   Vector<Double> parms(modelSpectrum.nParameters());
//   modelSpectrum.parameters(parms);
//   cout << "The parameters are: " << parms << endl;
//   const MVFrequency step(Quantity(100.0, "MHz"));
//   MVFrequency sampleFreq = modelSpectrum.refFrequency().getValue();
//   Flux<Double> modelFlux;
//   cout << "Frequency\t I-Flux\t Q-Flux\t U-Flux\t V-Flux\n";
//   for (uInt i = 0; i < 11; i++) {
//     modelFlux = refFlux.copy();
//     modelSpectrum.sample(modelFlux,
// 			 MFrequency(sampleFreq,
// 				    modelSpectrum.refFrequency().getRef()));
//     modelFlux.convertPol(ComponentType::STOKES);
//     cout << setprecision(3) << sampleFreq.get("GHz")
//  	 << "\t\t " << modelFlux.value(0u).re
//  	 << "\t " << modelFlux.value(1u).re
//  	 << "\t " << modelFlux.value(2u).re
//  	 << "\t " << modelFlux.value(3u).re
//  	 << " " << modelFlux.unit().getName() << endl;
//     sampleFreq += step;
//   }
// }

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tConstantSpectrum"
// End: 

