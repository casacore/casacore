//# tSpectralIndex.cc: tests the SpectralIndex class
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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>
#include <iomanip.h>

int main() {
  try {
    SpectralIndex siModel;
    const MFrequency refFreq(Quantity(1.0, "GHz"));
    siModel.setRefFrequency(refFreq);
    siModel.setIndex(1.0, Stokes::I);  
    siModel.setIndex(0.5, Stokes::Q);  
    siModel.setIndex(0.5, Stokes::U);  
    siModel.setIndex(-1.0, Stokes::V);
    const Flux<Double> LBandFlux(1.0, 1.0, 1.0, 1.0);
    const MVFrequency step(Quantity(100.0, "MHz"));
    MVFrequency sampleFreq = siModel.refFrequency().getValue();
    Flux<Double> sampleFlux;
    cout << "Frequency\t I-Flux\t Q-Flux\t U-Flux\t V-Flux\n";
    for (uInt i = 0; i < 11; i++) {
      sampleFlux = LBandFlux.copy();
      sampleFlux.convertPol(ComponentType::LINEAR);
      sampleFlux.convertUnit(Unit("WU"));
      siModel.sample(sampleFlux,
		     MFrequency(sampleFreq, siModel.refFrequency().getRef()));
      cout << setprecision(3) << sampleFreq.get("GHz")
	   << "\t\t " << sampleFlux.value(0u).re
	   << "\t " << sampleFlux.value(1u).re
	   << "\t " << sampleFlux.value(2u).re
	   << "\t " << sampleFlux.value(3u).re
	   << " " << sampleFlux.unit().getName() << endl;
      sampleFreq += step;
    }
    AlwaysAssert(sampleFlux.pol() == ComponentType::STOKES, AipsError);
    Double req = 2 * 200;
    AlwaysAssert(near(sampleFlux.value(0), req, req*C::dbl_epsilon),
		 AipsError);
    req = C::sqrt2 * 200;
    AlwaysAssert(near(sampleFlux.value(1), req, req*C::dbl_epsilon),
		 AipsError);
    AlwaysAssert(near(sampleFlux.value(2), req, req*C::dbl_epsilon),
		 AipsError);
    req = 0.5 * 200;
    AlwaysAssert(near(sampleFlux.value(3), req, req*C::dbl_epsilon),
		 AipsError);
    AlwaysAssert(siModel.isIonly() == False, AipsError);
    Vector<Double> newIndices = siModel.indices();
    AlwaysAssert(newIndices.nelements() == 4, AipsError);
    req = 1.0;
    AlwaysAssert(near(newIndices(0), req, req*C::dbl_epsilon), AipsError);
    req = 0.5;
    AlwaysAssert(near(newIndices(1), req, req*C::dbl_epsilon), AipsError);
    req = 0.5;
    AlwaysAssert(near(newIndices(2), req, req*C::dbl_epsilon), AipsError);
    req = -1.0;
    AlwaysAssert(near(newIndices(3), req, abs(req)*C::dbl_epsilon), AipsError);

    AlwaysAssert(siModel.nParameters() == 4, AipsError);
    Vector<Double> parms(4);
    siModel.parameters(parms);
    AlwaysAssert(allNear(parms.ac(), newIndices.ac(), C::dbl_epsilon),
		 AipsError);

    MFrequency newFreq = siModel.refFrequency();
    AlwaysAssert(newFreq.getValue() == refFreq.getValue(), AipsError);
    AlwaysAssert(newFreq.getRef() == refFreq.getRef(), AipsError);
    newFreq.set(MVFrequency(Quantity(5, "GHz")));
    newFreq.set(MFrequency::Ref(MFrequency::TOPO));
    SpectralIndex zeroModel(siModel);
    zeroModel.setRefFrequency(newFreq);
    newIndices = 0.0;
    zeroModel.setIndices(newIndices);
    newIndices = 2.0;

    AlwaysAssert(zeroModel.isIonly() == True, AipsError);
    AlwaysAssert(nearAbs(zeroModel.index(Stokes::I), 0.0, C::dbl_min),
 		 AipsError);
    AlwaysAssert(nearAbs(zeroModel.index(Stokes::Q), 0.0, C::dbl_min),
 		 AipsError);
    AlwaysAssert(nearAbs(zeroModel.index(Stokes::U), 0.0, C::dbl_min),
 		 AipsError);
    AlwaysAssert(nearAbs(zeroModel.index(Stokes::V), 0.0, C::dbl_min),
 		 AipsError);
    AlwaysAssert(allNearAbs(zeroModel.indices(), 0.0, C::dbl_min), AipsError);
    AlwaysAssert(zeroModel.refFrequency().getValue() ==
		 MVFrequency(Quantity(5, "GHz")), AipsError);
    AlwaysAssert(zeroModel.refFrequency().getRef().getType() ==
		 MFrequency::Ref(MFrequency::TOPO).getType(), AipsError);
    
    SpectralIndex otherModel; otherModel = siModel;
    otherModel.setRefFrequency(newFreq);
    parms.ac() += 1.0;
    otherModel.setParameters(parms);
    AlwaysAssert(otherModel.isIonly() == False, AipsError);
    AlwaysAssert(near(otherModel.index(Stokes::I), 2.0, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(otherModel.index(Stokes::Q), 1.5, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(otherModel.index(Stokes::U), 1.5, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(otherModel.index(Stokes::V), 0.0, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(otherModel.refFrequency().getValue() ==
		 MVFrequency(Quantity(5, "GHz")), AipsError);
    AlwaysAssert(otherModel.refFrequency().getRef().getType() ==
		 MFrequency::Ref(MFrequency::TOPO).getType(), AipsError);

    AlwaysAssert(siModel.isIonly() == False, AipsError);
    AlwaysAssert(near(siModel.index(Stokes::I), 1.0, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(siModel.index(Stokes::Q), 0.5, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(siModel.index(Stokes::U), 0.5, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(near(siModel.index(Stokes::V), -1.0, C::dbl_epsilon),
 		 AipsError);
    AlwaysAssert(siModel.refFrequency().getValue() == refFreq.getValue(),
		 AipsError);
    AlwaysAssert(siModel.refFrequency().getRef().getType() == 
		 refFreq.getRef().getType(), AipsError);
    {
      SpectralIndex siModel1(refFreq, -0.1);
      AlwaysAssert(siModel1.isIonly() == True, AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::I), -0.1, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::Q), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::U), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::V), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(siModel1.type() == ComponentType::SPECTRAL_INDEX,
		   AipsError);
      SpectralIndex * siPtr = (SpectralIndex *) siModel1.clone();
      Record rec;
      String error;
      AlwaysAssert(siModel1.toRecord(error, rec), AipsError);
      rec.define(RecordFieldId("index"), parms);
      AlwaysAssert(siModel1.fromRecord(error, rec), AipsError);
      AlwaysAssert(siModel1.isIonly() == False, AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::I), 2.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::Q), 1.5, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::U), 1.5, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel1.index(Stokes::V), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(siPtr->isIonly() == True, AipsError);
      AlwaysAssert(near(siPtr->index(Stokes::I), -0.1, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siPtr->index(Stokes::Q), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siPtr->index(Stokes::U), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siPtr->index(Stokes::V), 0.0, C::dbl_epsilon),
		   AipsError);
      SpectralIndex siModel2(refFreq, parms);
      AlwaysAssert(siModel2.ok(), AipsError);
      AlwaysAssert(near(siModel2.index(Stokes::I), 2.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel2.index(Stokes::Q), 1.5, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel2.index(Stokes::U), 1.5, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(siModel2.index(Stokes::V), 0.0, C::dbl_epsilon),
		   AipsError);
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
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSpectralIndex"
// End: 
