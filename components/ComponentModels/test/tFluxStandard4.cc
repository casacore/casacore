//# tFluxStandard4.cc: Test programs for the FluxStandard class 
//#
//# Essentiallly same as other tFluxStandard*.cc but tests on Scaife-Heald 2012
//# for low frequency calibrators
//#
//# Copyright (C) 2013
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
#include <casa/System/Aipsrc.h>
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
int main(int argc, char* argv[])
{
  // Parse argument(s), if any.
  Bool require_stdtable = true;
  if(argc > 1){
    const String arg(argv[1]);

    if(arg == "y"){
      require_stdtable = true;
    }
    else if(arg == "n"){
      require_stdtable = false;
    }
    else{
      Int retval = 1;    // Exit with fail val unless user _asked_ for help.

      cout << argv[0] << ": Test program for Scaife-Heald 2012 FluxStandard.\n"
           << "\nUse:\n\t" << argv[0] << " [[-h|--help]|y|n]\n"
           << "\n\t-h|--help: Print this help message and exit with 0.\n"
           << "\n\ty: (Default) Make not finding the FluxStandard table an error."
           << "\n\tn: Allow the data/nrao/VLA/standards directory to be absent.\n"
           << "\n\t   The directory must be found to run the complete set of tests."
           << endl;

      if(arg == "-h" || arg == "--help"){
        retval = 0;
      }
      return retval;
    }
  }
/***
int main() {
***/
  try {
    String fluxScaleName;
    Bool matchedScale=False;

    //leave possibility to expand
    Vector<String> qsScNames(1);          Vector<FluxStandard::FluxScale> qsScEnums(1);
    qsScNames[0] = "Scaife-Heald 2012";  qsScEnums[0] = FluxStandard::SCAIFE_HEALD_2012;

    //Vector<String> srcNames(2);
    //Vector<String> srcNames(3);
    Vector<String> srcNames(6);
    srcNames[0] = "3C48";  //non-variable for S-H 2012? 
    srcNames[1] = "3C147"; 
    srcNames[2] = "3C196";
    srcNames[3] = "3C286";
    srcNames[4] = "3C295";
    srcNames[5] = "3C380";

    Vector<MFrequency> freqs(2);
    freqs[0] = MFrequency(Quantity(30.0, "MHz"));
    freqs[1] = MFrequency(Quantity(300.0, "MHz"));


    //MEpoch mtime(Quantity(56293.0,"d"));
    // 2009.01.01
    MEpoch mtime(Quantity(54832.0,"d"));
    
    // Expected flux densities for qsScNames with srcNames at freqs.
    Vector<Vector<Vector<Float> > > expfds(1);
    for(Int scNum = qsScNames.nelements(); scNum--;){
      // no. sources
      expfds[scNum].resize(6);
      for(Int srcInd = srcNames.nelements(); srcInd--;)
        // no. of test freqs
        expfds[scNum][srcInd].resize(2);
    }
    // these are the values calculated from python based on the
    // coefficients with SH's polynomial function
    expfds[0][0][0] = 65.2916611318;       // SH 2012, 3C48, 30MHz
    expfds[0][0][1] = 45.8922533637;       // SH 2012, 3C48, 300MHz
    expfds[0][1][0] = 14.3830757208;       // SH 2012, 3C147, 30MHz 
    expfds[0][1][1] = 55.0833369249;       // SH 2012, 3C147, 300MHz
    expfds[0][2][0] = 226.12882846;       // SH 2012, 3C196, 30MHz
    expfds[0][2][1] = 50.0183468338;       // SH 2012, 3C196, 300MHz 
    expfds[0][3][0] = 42.3163624714;       // SH 2012, 3C286, 30MHz
    expfds[0][3][1] = 24.5129824585;       // SH 2012, 3C286, 300MHz
    expfds[0][4][0] = 92.3958410002;       // SH 2012, 3C295, 30MHz
    expfds[0][4][1] = 63.2256617737;       // SH 2012, 3C295, 300MHz
    expfds[0][5][0] = 265.816258351;       // SH 2012, 3C380, 30MHz
    expfds[0][5][1] = 45.454987278;       // SH 2012, 3C380, 300MHz
    Vector<Double> fluxUsed(4);

    //dummy source direction
    MDirection srcDir(MVDirection(Quantity(0.0, "rad"), Quantity(0.0, "rad")), MDirection::J2000);

// test if the standard table exist
/***
    cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
    String horpath;
    Bool foundStd = Aipsrc::findDir(horpath, "data/nrao/VLA/standards/ScaifeHeald2012Coeffs");
    if(foundStd){
      cout << "Aipsrc found a flux standard directory: " << horpath << endl;
    }
    else{
      cout << "Aipsrc did not find the VLA standard directory.  horpath = "
           << horpath << endl;
      if(require_ephems){
        cout << "data/nrao/VLA/standards is required (see --help)."
             << endl;
        AlwaysAssert(foundStd, AipsError);
      }
      else{
        cout << "Exiting without raising an error (see --help)." << endl;
        return 0;
      }
    }
***/

    for(Int scNum = qsScNames.nelements(); scNum--;){
      FluxStandard::FluxScale fluxScaleEnum;
      matchedScale = FluxStandard::matchStandard(qsScNames[scNum], fluxScaleEnum,
                                                 fluxScaleName);
      //cout << "matchStandard(" << qsScNames[scNum] << ") = " << fluxScaleEnum << endl;
      //cout << "qsScEnums[scNum] = " << qsScEnums[scNum] << endl;
      AlwaysAssert(fluxScaleEnum == qsScEnums[scNum], AipsError);
      cout << "Passed the matchStandard("
           << qsScNames[scNum] << ") test" << endl;

      FluxStandard fluxStd(fluxScaleEnum);
      Flux<Double> returnFlux, returnFluxErr;
     
      for(Int srcInd = srcNames.nelements(); srcInd--;){
        for(Int freqInd = freqs.nelements(); freqInd--;){
          Bool foundStd = fluxStd.compute(srcNames[srcInd], srcDir, freqs[freqInd],
                                          mtime, returnFlux, returnFluxErr);
          AlwaysAssert(foundStd, AipsError);
          cout << "Passed foundStd for " << qsScNames[scNum]
               << ", " << srcNames[srcInd]
               << ", " << (freqInd ? 300.0 : 30.0) << " MHz." << endl;

          returnFlux.value(fluxUsed); // Read this as fluxUsed = returnFlux.value();
          cerr.precision(10);
          //cerr<<"fluxUsed[0]="<<fluxUsed[0]<<endl;
          //cerr<<"expected="<<expfds[scNum][srcInd][freqInd]<<endl;
          AlwaysAssert(fabs(fluxUsed[0] - expfds[scNum][srcInd][freqInd]) < 0.001,
                       AipsError);          
          cout << "Passed flux density test for " << qsScNames[scNum]
               << ", " << srcNames[srcInd]
               << ", " << (freqInd ? 300.0 : 30.0) << " MHz." << endl;
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
