//# tFluxStandard.cc: Test programs for the FluxStandard class
//# Copyright (C) 2010
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

#include <casa/aips.h>
//#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/FluxStandard.h>
//#include <components/ComponentModels/FluxCalcQS.h>
//#include <components/ComponentModels/TwoSidedShape.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Constants.h>
//#include <casa/BasicMath/Math.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MEpoch.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
  try {
    String fluxScaleName;
    Bool matchedScale=False;

    Vector<String> qsScNames(5);          Vector<FluxStandard::FluxScale> qsScEnums(5);
    qsScNames[0] = "Perley-Butler 2010";  qsScEnums[0] = FluxStandard::PERLEY_BUTLER_2010;
    qsScNames[1] = "Baars";               qsScEnums[1] = FluxStandard::BAARS;
    qsScNames[2] = "Perley-Taylor 99";    qsScEnums[2] = FluxStandard::PERLEY_TAYLOR_99;
    qsScNames[3] = "Perley-Taylor 95";    qsScEnums[3] = FluxStandard::PERLEY_TAYLOR_95;
    qsScNames[4] = "Perley 90";           qsScEnums[4] = FluxStandard::PERLEY_90;

    Vector<String> srcNames(2);
    srcNames[0] = "3C147";
    //srcNames[1] = "1934-638";    
    srcNames[1] = "PKS J1939-6342";    

    Vector<MFrequency> freqs(2);
    freqs[0] = MFrequency(Quantity(2.0, "GHz"));
    freqs[1] = MFrequency(Quantity(20.0, "GHz"));
    
    MEpoch mtime(Quantity(56293.0,"d"));

    // Expected flux densities for qsScNames with srcNames at freqs.
    Vector<Vector<Vector<Float> > > expfds(5);
    for(Int scNum = qsScNames.nelements(); scNum--;){
      expfds[scNum].resize(2);
      for(Int srcInd = srcNames.nelements(); srcInd--;)
        expfds[scNum][srcInd].resize(2);
    }
    expfds[0][0][0] = 16.8847173;       // Perley-Butler 2010, 3C147, 2.0 GHz
    expfds[0][0][1] =  2.0824812;       // Perley-Butler 2010, 3C147, 20.0 GHz
    expfds[0][1][0] = 12.9502663;       // Perley-Butler 2010, 1934-638, 2.0 GHz
    expfds[0][1][1] =  0.9316584;       // Perley-Butler 2010, 1934-638, 20.0 GHz
    expfds[1][0][0] = 17.2404813;       // Baars, 3C147, 2.0 GHz
    expfds[1][0][1] =  1.9265059;       // Baars, 3C147, 20.0 GHz
    expfds[1][1][0] = 14.5077576;       // Baars, 1934-638, 2.0 GHz
    expfds[1][1][1] =  0.5434502;       // Baars, 1934-638, 20.0 GHz
    expfds[2][0][0] = 16.9075665;       // Perley-Taylor 99, 3C147, 2.0 GHz
    expfds[2][0][1] =  2.0221555;       // Perley-Taylor 99, 3C147, 20.0 GHz
    expfds[2][1][0] = 12.9502663;       // Perley-Taylor 99, 1934-638, 2.0 GHz
    expfds[2][1][1] =  0.9316584;       // Perley-Taylor 99, 1934-638, 20.0 GHz
    expfds[3][0][0] = 16.8629779;       // Perley-Taylor 95, 3C147, 2.0 GHz
    expfds[3][0][1] =  2.1317286;       // Perley-Taylor 95, 3C147, 20.0 GHz
    expfds[3][1][0] = 12.9502663;       // Perley-Taylor 95, 1934-638, 2.0 GHz
    expfds[3][1][1] =  1.0828228;       // Perley-Taylor 95, 1934-638, 20.0 GHz
    expfds[4][0][0] = 16.6844985;       // Perley 90, 3C147, 2.0 GHz
    expfds[4][0][1] =  1.8243617;       // Perley 90, 3C147, 20.0 GHz
    expfds[4][1][0] = 12.9502663;       // Perley 90, 1934-638, 2.0 GHz
    expfds[4][1][1] =  1.0828228;       // Perley 90, 1934-638, 20.0 GHz

    // dummy direction  
    MDirection srcDir(MVDirection(Quantity(0.0,"rad"),Quantity(0.0,"rad")), MDirection::J2000);
    Vector<Double> fluxUsed(4);

    for(Int scNum = qsScNames.nelements(); scNum--;){
      FluxStandard::FluxScale fluxScaleEnum;
      matchedScale = FluxStandard::matchStandard(qsScNames[scNum], fluxScaleEnum,
                                                 fluxScaleName);
      // cout << "matchStandard(" << qsScNames[scNum] << ") = " << fluxScaleEnum << endl;
      // cout << "qsScEnums[scNum] = " << qsScEnums[scNum] << endl;
      AlwaysAssert(fluxScaleEnum == qsScEnums[scNum], AipsError);
      cout << "Passed the matchStandard("
           << qsScNames[scNum] << ") test" << endl;

      FluxStandard fluxStd(fluxScaleEnum);
      Flux<Double> returnFlux, returnFluxErr;
      
      for(Int srcInd = srcNames.nelements(); srcInd--;){
        for(Int freqInd = freqs.nelements(); freqInd--;){
          Bool foundStd = fluxStd.compute(srcNames[srcInd], srcDir, freqs[freqInd],
                                          mtime,
                                          returnFlux, returnFluxErr);
          AlwaysAssert(foundStd, AipsError);
          cout << "Passed foundStd for " << qsScNames[scNum]
               << ", " << srcNames[srcInd]
               << ", " << (freqInd ? 20.0 : 2.0) << " GHz." << endl;

          returnFlux.value(fluxUsed); // Read this as fluxUsed = returnFlux.value();
          cout.precision(10);
          cout<< " fluxUsed[0] ="<< fluxUsed[0]<< endl;
          cout<< " expected ="<< expfds[scNum][srcInd][freqInd]<< endl;
          AlwaysAssert(fabs(fluxUsed[0] - expfds[scNum][srcInd][freqInd]) < 0.001,
                       AipsError);          
          cout << "Passed flux density test for " << qsScNames[scNum]
               << ", " << srcNames[srcInd]
               << ", " << (freqInd ? 20.0 : 2.0) << " GHz." << endl;
        }
      }
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
}
