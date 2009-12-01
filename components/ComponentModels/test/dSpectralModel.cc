//# dSpectralModel.cc:
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
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/SpectralIndex.h>
#include <components/ComponentModels/SpectralModel.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MeasFrame.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>
#include <casa/iomanip.h>

#include <casa/namespace.h>
void plotSpectrum(const SpectralModel& modelSpectrum) {
  cout << "This is a "
       << ComponentType::name(modelSpectrum.type())
       << " spectrum with a reference frequency of: "
       << setprecision(4) << modelSpectrum.refFrequency().get("GHz") << " ("
       << modelSpectrum.refFrequency().getRefString() << ")"
       << endl;
  const MVFrequency step(Quantity(100.0, "MHz"));
  MVFrequency sampleFreq(Quantity(1, "GHz"));
  MeasFrame obsFrame;
  {
    Quantity obsRa; MVAngle::read(obsRa, "19:39:");
    Quantity obsDec; MVAngle::read(obsDec, "-63.43.");
    Quantity obsDay; MVTime::read(obsDay, "1996/11/20/5:20");
    obsFrame.set(MEpoch(obsDay, MEpoch::UTC),
		 MDirection(obsRa, obsDec, MDirection::J2000));
  }
  MFrequency::Ref obsRef(MFrequency::GEO, obsFrame);
  cout << "Frequency\t scale\n";
  for (uInt i = 0; i < 11; i++) {
    cout << setprecision(7) << sampleFreq.get("GHz")
	 << "\t\t " << modelSpectrum.sample(MFrequency(sampleFreq, obsRef))
	 << endl;
    sampleFreq += step;
  }
}

int main() {
  try {
    SpectralIndex SImodel((MFrequency(Quantity(1.0, "GHz"))), 0.5);
    plotSpectrum(SImodel);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 dSpectralModel"
// End: 
