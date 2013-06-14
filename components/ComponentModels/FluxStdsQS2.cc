//# FluxStdsQS2.cc: Definition of the flux standards some of which may varies in time
//# depend on time.
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

#include <components/ComponentModels/FluxStdsQS2.h>
#include <components/ComponentModels/FluxStdSrcs.h>
#include <casa/OS/Path.h>
#include <casa/System/Aipsrc.h>

//#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN
namespace NSTDS { //# NAMESPACE NSTDS - BEGIN
// Each of these c'tors defines the polynomial coefficients for the
// log10(fluxDensity) = polynomial(log10(frequency)) calculations
// and optionally additional coefficients for estimating the flux density
// uncertainties. 
//
// fill_coeffs() with two RigidVectors uses the first one for the flux density
// coefficients and second one for the uncertainty coefficients, starting at
// order 0.  If the second RigidVector is omitted no uncertainty will be
// estimated.


Bool FluxStdBaars::setSourceCoeffs()
{
  Bool found = true;
  
  setFreqUnit("MHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();
  
  if(srcEnum == FSS::THREEC286)
    fill_coeffs(RVF3(1.480, 0.292, -0.124), RVF3(0.018, 0.006, 0.001));
  else if(srcEnum == FSS::THREEC48)
    fill_coeffs(RVF3(2.345, 0.071, -0.138), RVF3(0.03, 0.001, 0.001));
  else if(srcEnum == FSS::THREEC147)
    fill_coeffs(RVF3(1.766, 0.447, -0.184), RVF3(0.017, 0.006, 0.001));
  else if(srcEnum == FSS::THREEC138)
    fill_coeffs(RVF3(2.009, -0.07176, -0.0862)); // No error specified
  else if(srcEnum == FSS::NINETEEN34M638)
    fill_coeffs(RVF4(-23.839, 19.569, -4.8168, 0.35836)); // No error specified
  else if(srcEnum == FSS::THREEC295)
    fill_coeffs(RVF3(1.485, 0.759, -0.255), RVF3(0.013, 0.009, 0.001));
  else
    found = false;
  return found;
}

Bool FluxStdPerley90::setSourceCoeffs()
{
  Bool found = true;
  
  setFreqUnit("MHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();

  if(srcEnum == FSS::THREEC286)
    fill_coeffs(RVF3(1.35899, 0.3599, -0.13338));
  else if(srcEnum == FSS::THREEC48)
    fill_coeffs(RVF3(2.0868, 0.20889, -0.15498));
  else if(srcEnum == FSS::THREEC147)
    fill_coeffs(RVF3(1.92641, 0.36072, -0.17389));
  else if(srcEnum == FSS::THREEC138)
    fill_coeffs(RVF3(2.009, -0.07176, -0.0862));
  else if(srcEnum == FSS::NINETEEN34M638)
    fill_coeffs(RVF4(-30.7667, 26.4908, -7.0977, 0.605334));
  else if(srcEnum == FSS::THREEC295)
    fill_coeffs(RVF3(1.485, 0.759, -0.255));
  else
    found = false;
  return found;
}

Bool FluxStdPerleyTaylor95::setSourceCoeffs()
{
  Bool found = true;
  
  setFreqUnit("MHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();

  if(srcEnum == FSS::THREEC286)
    fill_coeffs(RVF4(0.50344, 1.05026, -0.31666, 0.01602));
  else if(srcEnum == FSS::THREEC48)
    fill_coeffs(RVF4(1.16801, 1.07526, -0.42254, 0.02699));
  else if(srcEnum == FSS::THREEC147)
    fill_coeffs(RVF4(0.05702, 2.09340, -0.7076, 0.05477));
  else if(srcEnum == FSS::THREEC138)
    fill_coeffs(RVF4(1.97498, -0.23918, 0.01333, -0.01389));
  else if(srcEnum == FSS::NINETEEN34M638)
    fill_coeffs(RVF4(-30.7667, 26.4908, -7.0977, 0.605334));
  else if(srcEnum == FSS::THREEC295)
    fill_coeffs(RVF4(1.28872, 0.94172, -0.31113, 0.00569));
  else
    found = false;
  return found;
}

Bool FluxStdPerleyTaylor99::setSourceCoeffs()
{
  Bool found = true;
  
  setFreqUnit("GHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();

  //LogIO os(LogOrigin("FluxStdPerleyTaylor99", "setSourceCoeffs", WHERE));
  // os << LogIO::NORMAL
  //    << "srcEnum before fill_coeffs() = " << srcEnum
  //    << LogIO::POST;

  if(srcEnum == FSS::THREEC286)
    fill_coeffs(RVF4(1.23734, -0.43276, -0.14223, 0.00345));
  else if(srcEnum == FSS::THREEC48)
    fill_coeffs(RVF4(1.31752, -0.74090, -0.16708, 0.01525));
  else if(srcEnum == FSS::THREEC147)
    fill_coeffs(RVF4(1.44856, -0.67252, -0.21124, 0.04077));
  else if(srcEnum == FSS::THREEC138)
    fill_coeffs(RVF4(1.00761, -0.55629, -0.11134, -0.0146));
  else if(srcEnum == FSS::NINETEEN34M638){
    // The broken polynomial is smooth enough to be 1st-order differentiable.
    //
    // The coefficients have been shifted to use GHz instead of MHz.
    fill_lohi_coeffs(RVF4(1.170418, 0.248618, -1.649694, 0.605334),  // Low
                     MFrequency(Quantity(10.0, "GHz")),              // break
                     RVF4(-2.5739, 10.0707, -10.0595, 2.9372));      // High
  }
  else if(srcEnum == FSS::THREEC295)
    fill_coeffs(RVF4(1.46744, -0.7735, -0.25912, 0.00752));
  else
    found = false;
  return found;
}

Bool FluxStdPerleyButler2010::setSourceCoeffs()
{
  Bool found = true;
  
  setFreqUnit("GHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();

  if(srcEnum == FSS::THREEC286)
    fill_coeffs(RVF4(1.2361, -0.4127, -0.1864, 0.0294));
  else if(srcEnum == FSS::THREEC48)
    fill_coeffs(RVF4(1.3197, -0.7253, -0.2023, 0.0540));
  else if(srcEnum == FSS::THREEC147)
    fill_coeffs(RVF4(1.4428, -0.6300, -0.3142, 0.1032));
  else if(srcEnum == FSS::THREEC138)
    fill_coeffs(RVF4(1.0053, -0.4384, -0.1855, 0.0511));
  else if(srcEnum == FSS::NINETEEN34M638){
    // The broken polynomial is smooth enough to be 1st-order differentiable.
    //
    // The coefficients have been shifted to use GHz instead of MHz.
    fill_lohi_coeffs(RVF4(1.170418, 0.248618, -1.649694, 0.605334),  // Low
                     MFrequency(Quantity(10.0, "GHz")),              // break
                     RVF4(-2.5739, 10.0707, -10.0595, 2.9372));      // High
  }
  else if(srcEnum == FSS::THREEC295)
    fill_coeffs(RVF4(1.4605, -0.7043, -0.3951, 0.0815));
  else if(srcEnum == FSS::THREEC196)
    fill_coeffs(RVF4(1.2753, -0.7971, -0.2255, 0.0380));
  else
    found = false;
  return found;
}

Bool FluxStdPerleyButler2013::setSourceCoeffs()
{
  Bool found = true;
  Bool timevar = true;
  setFreqUnit("GHz");
  FSS::Source srcEnum = FCVQS::getSrcEnum();
  String stddatapath;
  String stdTabName("PerleyButler2013Coeffs");
  
  if(!Aipsrc::findDir(stddatapath,"./"+stdTabName)) {
    if(!Aipsrc::findDir(stddatapath, Aipsrc::aipsRoot()+"/data/nrao/VLA/standards/"+stdTabName)) {
      ostringstream oss;
      oss << "The coefficient data for Perley-Butler 2013, " <<  stdTabName
          << " is not found in ./ or in ~/data/nrao/VLA/standards/";
      throw(AipsError(String(oss)));
    }
  }
  //cerr<<"use stddatapath="<<stddatapath<<endl;
  LogIO os(LogOrigin("FluxStdPerleyButler2013", "setSourceCoeffs", WHERE));
   os << LogIO::NORMAL2
      << "Use the coefficent data table: " << stddatapath
      << LogIO::POST;

 
  Path stdTablePath(stddatapath);
  //cerr<<"stddatapath="<<stddatapath<<endl;
  FCVQS::readQSCoeffsTable(stdTablePath);
  if (srcEnum != FSS::THREEC286 && 
      srcEnum != FSS::THREEC123 &&
      srcEnum != FSS::THREEC295 &&
      srcEnum != FSS::THREEC196 &&
      srcEnum != FSS::THREEC48 &&
      srcEnum != FSS::THREEC138 &&
      srcEnum != FSS::THREEC147) 
    found=false;

  // check if considered as non-variable
  if (srcEnum == FSS::THREEC286 ||
      srcEnum == FSS::THREEC123 ||
      srcEnum == FSS::THREEC295 ||
      srcEnum == FSS::THREEC196) {
    // no need to do this now, will be filled with nepoch=1 in FluxCalcVQS::()
    //uInt row=0;
    //FluxCalcVQS::setSourceCoeffsfromVec(row);     
    //RigidVector<Vector<Float>, 2> coeffs=FluxCalcVQS::getCurrentCoeffs(); 
    //fill_coeffs(RVF4(coeffs(0)), RVF4(coeffs(1)) );
    timevar=false;
  }
  FCVQS::isTimeVar(timevar);

  return found;
}

} //# NAMESPACE NSTD - END 
} //# NAMESPACE CASA - END
