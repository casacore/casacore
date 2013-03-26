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
#include <casa/System/Aipsrc.h>
//#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/FluxStandard.h>
//#include <components/ComponentModels/TwoSidedShape.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Constants.h>
//#include <casa/BasicMath/Math.h>
#include <components/ComponentModels/ComponentList.h>
#include <components/ComponentModels/TwoSidedShape.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MFrequency.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/OS/Path.h>		// Needed by ComponentList
#include <casa/iostream.h>
#include <tables/Tables/Table.h>

#include <casa/namespace.h>
int main(int argc, char* argv[])
{
  // Parse argument(s), if any.
  Bool require_ephems = true;
  if(argc > 1){
    const String arg(argv[1]);

    if(arg == "y"){
      require_ephems = true;
    }
    else if(arg == "n"){
      require_ephems = false;
    }
    else{
      Int retval = 1;    // Exit with fail val unless user _asked_ for help.

      cout << argv[0] << ": Test program for Solar System FluxStandards.\n"
	   << "\nUse:\n\t" << argv[0] << " [[-h|--help]|y|n]\n"
	   << "\n\t-h|--help: Print this help message and exit with 0.\n"
	   << "\n\ty: (Default) Make not finding the ephemerides an error."
	   << "\n\tn: Allow the data/ephemerides/JPL-Horizons directory to be absent.\n"
	   << "\n\t   The directory must be found to run the complete set of tests."
	   << endl;

      if(arg == "-h" || arg == "--help"){
	retval = 0;
      }
      return retval;
    }
  }
  Double reltol = 1.0e-5;
  
  try {
    String fluxScaleName;
    Bool matchedScale=False;
    Vector<Double> fluxUsed(4);

    // Test the (sub)mm FD calibration models.
    FluxStandard::FluxScale fluxScaleEnum;
    matchedScale = FluxStandard::matchStandard("Butler-JPL-Horizons 2010",
					       fluxScaleEnum, fluxScaleName);
    AlwaysAssert(matchedScale && fluxScaleEnum == FluxStandard::SS_JPL_BUTLER, AipsError);
    cout << "Found the Butler-JPL-Horizons 2010 flux density scale." << endl;
    FluxStandard fs(fluxScaleEnum);

    const uInt nspws = 2;
    Vector<Vector<MFrequency> > spws(nspws);
    spws[0].resize(1);
    spws[1].resize(1);
    spws[0][0] = MFrequency(Quantity(115.0, "GHz"));
    spws[1][0] = MFrequency(Quantity(345.0, "GHz"));
    Vector<Vector<Flux<Double> > > returnFluxes(nspws), returnFluxErrs(nspws);
    returnFluxes[0].resize(1);
    returnFluxes[1].resize(1);
    returnFluxErrs[0].resize(1);
    returnFluxErrs[1].resize(1);
    Vector<String> tempCLs(nspws);
    
    const uInt nSSobjs = 11;
    Vector<String> objnames(nSSobjs);

    // Expected flux densities (Jy)  They will be used in a relative
    // comparison, so they cannot be 0.0.
    Vector<Vector<Float> > expfds(nSSobjs);

    // Expected angular diameters (radians).  They will be used in a relative
    // comparison, so they cannot be 0.0.
    Vector<Float> expads(nSSobjs);

    // The expected quantities must be for this time.  The .1 will force
    // interpolation in nearly all cases.
    const Double mjd = 56000.1;
    const MEpoch mtime(MVEpoch(Quantity(mjd, "d")), MEpoch::Ref(MEpoch::UTC));

    for(uInt i = 0; i < nSSobjs; ++i)
      expfds[i].resize(2);

    // These expected values will of course need updating when the models are
    // updated.
    objnames[0] = "Venus";
    expfds[0][0] = 1056.06;	// 115 GHz
    expfds[0][1] = 7525.55;	// 345 GHz
    expads[0] = 9.99015e-05;
    objnames[1] = "Mars";
    expfds[1][0] = 292.084;	// 115 GHz
    expfds[1][1] = 2559.98;	// 345 GHz
    expads[1] = 6.64545e-05;
    objnames[2] = "Ceres";
    // changed due to updated ephemeris table for Ceres now
    // has triaxial radii (2013-02-21)
    //expfds[2][0] = 0.151999;	// 115 GHz
    expfds[2][0] = 0.152118;	// 115 GHz
    //expfds[2][1] = 1.32304;	// 345 GHz
    expfds[2][1] = 1.32408;	// 345 GHz
    //expads[2] = 1.70287e-06;
    expads[2] = 1.70353e-06;
    objnames[3] = "Jupiter";
    expfds[3][0] = 1438.55;	// 115 GHz
    expfds[3][1] = 12529.0;	// 345 GHz, outside model limit (lambda >= 1mm)
    expads[3] = 0.000164169;
    objnames[4] = "Ganymede";
    expfds[4][0] = 1.33757;	// 115 GHz
    expfds[4][1] = 11.4395;	// 345 GHz
    expads[4] = 6.25104e-06;
    objnames[5] = "Titan";
    expfds[5][0] = 0.35435;	// 115 GHz
    expfds[5][1] = 2.96232;	// 345 GHz
    expads[5] = 3.87696e-06;
    // radii updated in the new table (2013-02-21)
    objnames[6] = "Uranus";
    //expfds[6][0] = 9.74014;	// 115 GHz
    expfds[6][0] = 9.74463;	// 115 GHz
    //expfds[6][1] = 57.3448;	// 345 GHz
    expfds[6][1] = 57.3713;	// 345 GHz
    //expads[6] = 1.60581e-05;
    expads[6] = 1.60618e-05;
    objnames[7] = "Neptune"; //radii changed in the updated table
    //expfds[7][0] = 4.32942;	// 115 GHz
    expfds[7][0] = 4.32918;	// 115 GHz
    //expfds[7][1] = 24.7807;	// 345 GHz
    expfds[7][1] = 24.7793;	// 345 GHz
    //expads[7] = 1.06364e-05;
    expads[7] = 1.06361e-05;
    objnames[8] = "Triton";
    expfds[8][0] = 0.00385483;	// 115 GHz
    expfds[8][1] = 0.0297936;	// 345 GHz
    expads[8] = 5.84913e-07;
    objnames[9] = "Pluto";
    expfds[9][0] = 0.00250031;	// 115 GHz
    expfds[9][1] = 0.0190617;	// 345 GHz
    expads[9] = 4.92412e-07;
    objnames[10] = "Vesta"; //radii changed in the updated table
    //expfds[10][0] = 0.0522284;// 115 GHz
    expfds[10][0] = 0.052266;	// 115 GHz
    //expfds[10][1] = 0.453937;	// 345 GHz
    expfds[10][1] = 0.454264;	// 345 GHz
    //expads[10] = 1.02016e-06;
    expads[10] = 1.02053e-06;

    // Not tested; essentially a dummy.
    const MDirection fieldDir;

    const Unit hertz("Hz");

    cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
    String horpath;
    Bool foundStd = Aipsrc::findDir(horpath, "data/ephemerides/JPL-Horizons");
    if(foundStd){
      cout << "Aipsrc found an ephemeris directory: " << horpath << endl;
    }
    else{
      cout << "Aipsrc did not find an ephemeris directory.  horpath = "
	   << horpath << endl;
      cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
      cout << "AIPSARCH: " << Aipsrc::aipsArch() << endl;
      cout << "AIPSSITE: " << Aipsrc::aipsSite() << endl;
      cout << "AIPSHOST: " << Aipsrc::aipsHost() << endl;
      cout << "AIPSHOME: " << Aipsrc::aipsHome() << endl;
      if(require_ephems){
	cout << "data/ephemerides/JPL-Horizons is required (see --help)."
	     << endl;
	AlwaysAssert(foundStd, AipsError);
      }
      else{
	cout << "Exiting without raising an error (see --help)." << endl;
	return 0;
      }      
    }

    // Test 1 frequency per spw.
    for(uInt i = 0; i < nSSobjs; ++i){
      if(fs.computeCL(objnames[i], spws, mtime, fieldDir,
		      returnFluxes, returnFluxErrs, tempCLs, 
		      "setjy_")){  // Unfortunately ".setjy" -> "setjy".
	cout << objnames[i] << ':' << endl;
	AlwaysAssert(Table::isReadable(tempCLs[0]), AipsError);
	cout << "\tPassed component list existence test." << endl;

        for(uInt spwInd = 0; spwInd < spws.nelements(); ++spwInd){
	  if(spwInd == 0){
	    // cl should be temporary so tempCLs[0] can be deleted below
	    ComponentList cl(tempCLs[0]);
	    //const TwoSidedShape *csptr(cl.getShape(0));
	    cout << "\tcalculated major axis: "
		 << ((TwoSidedShape*)(cl.getShape(0)))->majorAxisInRad()
		 << " radians" << endl;
	    AlwaysAssert(fabs(((TwoSidedShape*)(cl.getShape(0)))->majorAxisInRad() / expads[i] - 1.0) < reltol,
			 AipsError);
	    cout << "\tPassed angular diameter test." << endl;
	  }

	  returnFluxes[spwInd][0].value(fluxUsed); // Read this as fluxUsed = returnFlux.value();
	  cout << "\tcalculated flux density at "
	       << 1.0e-9 * spws[spwInd][0].get(hertz).getValue() << " GHz: "
	       << fluxUsed[0] << " Jy" << endl;
	  AlwaysAssert(fabs(fluxUsed[0] / expfds[i][spwInd] - 1.0) < reltol,
	  	       AipsError);
	  cout << "\tPassed flux density test." << endl;
	  if(tempCLs[spwInd] != ""){
	    if(Table::canDeleteTable(tempCLs[spwInd]))
	      Table::deleteTable(tempCLs[spwInd]);
	    else
	      cout << "Table::canDeleteTable(" << tempCLs[spwInd]
		   << ") returned False" << endl;
	  }	    
	}
      }
      else{
	cout << "No ephemeris found for " << objnames[i] << " at MJD " << mjd << endl;
	cout << "   ...not further tested." << endl;
      }
    }

    // Test 1 spw with 2 frequencies.
    Vector<Vector<MFrequency> > onespw(1);
    onespw[0].resize(2);
    onespw[0][0] = spws[0][0];
    onespw[0][1] = spws[1][0];
    returnFluxes.resize(1);
    returnFluxes[0].resize(2);
    returnFluxErrs[0].resize(2);
    tempCLs.resize(1);
    if(fs.computeCL(objnames[0], onespw, mtime, fieldDir,
                    returnFluxes, returnFluxErrs, tempCLs, 
                    "setjy_")){  // Unfortunately ".setjy" -> "setjy".
      cout << objnames[0] << ':' << endl;
      AlwaysAssert(Table::isReadable(tempCLs[0]), AipsError);
      cout << "\tPassed TabularSpectrum component list existence test." << endl;

      {
        ComponentList cl(tempCLs[0]);
        cout << "\tcalculated major axis: "
             << ((TwoSidedShape*)(cl.getShape(0)))->majorAxisInRad()
             << " radians" << endl;
        AlwaysAssert(fabs(((TwoSidedShape*)(cl.getShape(0)))->majorAxisInRad() /
                          expads[0] - 1.0) < reltol,
                     AipsError);
        cout << "\tPassed TabularSpectrum angular diameter test." << endl;

        for(uInt freqInd = 0; freqInd < onespw[0].nelements(); ++freqInd){
          // Read this as fluxUsed = returnFlux.value();
          returnFluxes[0][freqInd].value(fluxUsed);

          cout << "\tcalculated flux density at "
               << 1.0e-9 * onespw[0][freqInd].get(hertz).getValue() << " GHz: "
               << fluxUsed[0] << " Jy" << endl;
          AlwaysAssert(fabs(fluxUsed[0] / expfds[0][freqInd] - 1.0) < reltol,
                       AipsError);
          cout << "\tPassed TabularSpectrum flux density test." << endl;
        }
      }
      if(tempCLs[0] != ""){
        if(Table::canDeleteTable(tempCLs[0]))
          Table::deleteTable(tempCLs[0]);
        else
          cout << "Table::canDeleteTable(" << tempCLs[0]
               << ") returned False" << endl;
      }
    }
    else{
      cout << "No ephemeris found for " << objnames[0] << " at MJD " << mjd << endl;
      cout << "   ...not further tested." << endl;
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
