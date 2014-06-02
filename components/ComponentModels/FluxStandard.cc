//# FluxStandard.cc: Implementation of FluxStandard.h
//# Copyright (C) 1996,1997,1999,2001,2002
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
//----------------------------------------------------------------------------

#include <components/ComponentModels/FluxStandard.h>
//#include <components/ComponentModels/FluxStdsQS.h>
#include <components/ComponentModels/FluxStdsQS2.h>
#include <components/ComponentModels/FluxCalc_SS_JPL_Butler.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/ComponentList.h>
#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/TabularSpectrum.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/DiskShape.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/String.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/OS/Path.h>
#include <casa/Utilities/CountedPtr.h>
#include <casa/sstream.h>
#include <casa/iomanip.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasTable.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//----------------------------------------------------------------------------

FluxStandard::FluxStandard(const FluxStandard::FluxScale scale) : 
  itsFluxScale(scale),
  has_direction_p(False),
  interpmethod_p("")
{
// Default constructor
// Output to private data:
//    itsFluxScale      FluxStandard::FluxScale     Flux scale (eg. BAARS)
//
}

//----------------------------------------------------------------------------

FluxStandard::~FluxStandard()
{
// Default destructor
//
}

//----------------------------------------------------------------------------

Bool FluxStandard::compute (const String& sourceName, 
                            const MDirection& sourceDir,
                            const MFrequency& mfreq,
                            const MEpoch& mtime,
			    Flux <Double>& value, Flux <Double>& error)
{
  // I refuse to duplicate the monstrosity below to skip a short for loop.
  Vector<Flux<Double> > fluxes(1);
  Vector<Flux<Double> > errors(1);
  Vector<MFrequency> mfreqs(1);
  
  mfreqs[0] = mfreq;
  Bool success = compute(sourceName, sourceDir, mfreqs, mtime, fluxes, errors);
  
  value = fluxes[0];
  error = errors[0];
  return success;
}

Bool FluxStandard::compute (const String& sourceName, const MFrequency& mfreq,
                            Flux<Double>& value, Flux<Double>& error)
{
  Vector<Flux<Double> > fluxes(1);
  Vector<Flux<Double> > errors(1);
  Vector<MFrequency> mfreqs(1);
  mfreqs[0] = mfreq;
  Bool success = compute(sourceName, mfreqs, fluxes, errors);
  value = fluxes[0];
  error = errors[0];
  return success;
}
Bool FluxStandard::compute(const String& sourceName, 
                           const Vector<Vector<MFrequency> >& mfreqs,
                           Vector<Vector<Flux<Double> > >& values,
                           Vector<Vector<Flux<Double> > >& errors)
{
  Bool success = True;
  for(uInt spw = 0; spw < mfreqs.size(); ++spw) {
    success &= compute(sourceName, mfreqs[spw], values[spw], errors[spw]);
  }
  return success;
}
Bool FluxStandard::compute(const String& sourceName, 
                           const Vector<MFrequency>& mfreqs,
                           Vector<Flux<Double> >& values,
                           Vector<Flux<Double> >& errors)
{
  MDirection dir;
  if (sourceName == "3C286"  ||
      sourceName == "1328+307"  ||
      sourceName == "1331+305"  ||
      sourceName == "J1331+3030") {
    dir = MDirection(MVDirection(3.539257626070549, 0.5324850225220917),
                     MDirection::J2000);
  } else if (sourceName == "3C48"  ||
             sourceName == "0134+329"  ||
             sourceName == "0137+331"  ||
             sourceName == "J0137+3309") {
    dir = MDirection(MVDirection(0.4262457643630985, 0.5787463318245085),
                     MDirection::J2000);
  } else if (sourceName == "3C147"  ||
             sourceName == "0538+498"  ||
             sourceName == "0542+498"  ||
             sourceName == "J0542+4951") {
    dir = MDirection(MVDirection(1.4948817765383597, 0.8700805690768509),
                     MDirection::J2000);
  } else if (sourceName == "3C138"  ||
             sourceName == "0518+165"  ||
             sourceName == "0521+166"  ||
             sourceName == "J0521+1638") {
    dir = MDirection(MVDirection(1.401346673041897, 0.2904130912582342),
                     MDirection::J2000);
  } else if (sourceName == "1934-638") {
    dir = MDirection(MVDirection(5.146176021557448, -1.1119977478136984),
                     MDirection::J2000);
  } else if (sourceName == "3C295"  ||
             sourceName == "1409+524"  ||
             sourceName == "1411+522"  ||
             sourceName == "J1411+5212") {
    dir = MDirection(MVDirection(3.7146787856873478, 0.9111103509091509),
                     MDirection::J2000);
  } else if (sourceName == "3C196"  ||
             sourceName == "0809+483"  ||
             sourceName == "0813+482"  ||
             sourceName == "J0813+4813") {
    dir = MDirection(MVDirection(2.1537362969610023, 0.8415541320803659),
                     MDirection::J2000);
  }
  return compute (sourceName, dir, mfreqs, MEpoch(), values, errors);
}

Bool FluxStandard::compute(const String& sourceName, 
                           const MDirection& sourceDir,
                           const Vector<Vector<MFrequency> >& mfreqs,
                           const MEpoch& mtime,
                           Vector<Vector<Flux<Double> > >& values,
                           Vector<Vector<Flux<Double> > >& errors)
{
  Bool success = True;
  uInt nspws = mfreqs.nelements();

  for(uInt spw = 0; spw < nspws; ++spw)
    success &= compute(sourceName, sourceDir, mfreqs[spw], mtime, values[spw], errors[spw],
                       spw == 0);

  return success;
}

Bool FluxStandard::compute(const String& sourceName, 
                           const MDirection& sourceDir,
                           const Vector<MFrequency>& mfreqs,
                           const MEpoch& mtime,
                           Vector<Flux<Double> >& values,
                           Vector<Flux<Double> >& errors,
                           const Bool verbose)
{
// Compute the flux density for a specified source at a specified set of
// frequencies.
// Inputs:
//    sourceName  Source name
//    mfreqs      Desired frequencies
//    mtime       Desired time 
// Output:
//    values      Computed total flux densities
//    errors      Flux density uncertainties; 0 => not known.
//    compute     False if sourceName is not recognized
//                as a standard reference.
//
  LogIO os(LogOrigin("FluxStandard", "compute"));

  // There used to be a big
  // if(string == 'dfa" || string == "bla" || ...)
  // ...else if...else... 
  // chain here, with a similar
  // switch(standard) statement inside each if clause.  Finally, inside each
  // switch case came the actual calculation, usually a 2nd or 3rd order
  // polynomial in log(freq).
  //
  // This meant that the chain of string comparisons and case selections,
  // typically more expensive than the actual calculation, was repeated for
  // each frequency.  It would have been better to do the source and standard
  // determination at the start, and then do the calculation like
  // ans = coeffs[std][src][0] + lf * (coeffs[std][src][1] + lf * ...),
  // but:
  //   * std and src would naturally be enums, and thus not quite 
  //     natural indices for an array.
  //   * The standards do not necessarily use the same set, or number,
  //     of sources.
  // Both of those could be gotten around by using a std::map, but then
  // accessing the coeffs entails a function call.  Also, it ties the
  // functional form of the standards to a low-order polynomial in
  // log(freq).
  //
  // If a function call will be used anyway, why not use a functor to cache the
  // (std, src) state?  It is more convenient than adding a loop over
  // log10(frequency) inside each switch case.

  //CountedPtr<FluxCalcQS> fluxStdPtr;
  CountedPtr<FluxCalcVQS> fluxStdPtr;
  Bool timeVariable(false);
  if(itsFluxScale == BAARS)
    fluxStdPtr = new NSTDS::FluxStdBaars;
  else if(itsFluxScale == PERLEY_90)
    fluxStdPtr = new NSTDS::FluxStdPerley90;
  else if(itsFluxScale == PERLEY_TAYLOR_95)
    fluxStdPtr = new NSTDS::FluxStdPerleyTaylor95;
  else if(itsFluxScale == PERLEY_TAYLOR_99)
    fluxStdPtr = new NSTDS::FluxStdPerleyTaylor99;
  else if(itsFluxScale == PERLEY_BUTLER_2010)
    fluxStdPtr = new NSTDS::FluxStdPerleyButler2010;
  else if(itsFluxScale == PERLEY_BUTLER_2013) {
    fluxStdPtr = new NSTDS::FluxStdPerleyButler2013;
    timeVariable=true; // to read from the table 
    if (interpmethod_p=="") {
      ostringstream oss;
      oss << "Unset interpmethod. Please set the method first";
      throw(AipsError(String(oss)));
    }
  }
  else if(itsFluxScale == SCAIFE_HEALD_2012)
    fluxStdPtr = new NSTDS::FluxStdScaifeHeald2012;
  else{
    if(verbose)
      os << LogIO::SEVERE
         << "Flux standard " << standardName(itsFluxScale)
         << " cannot be used this way.  (Does it require a time?)"
         << LogIO::POST;
    return false;
  }
  os << LogIO::DEBUG1
     << "Using flux standard: " << standardName(itsFluxScale)
     << LogIO::POST;

  // Set the source or fail.
  // setSource needs J2000 coordinates for source matching by position
  MDirection sourceDirJ2000;
  if (sourceDir.getRefString()!="J2000") {
    MeasFrame frame(mtime);
    MDirection::Ref outref(MDirection::J2000, frame);
    sourceDirJ2000 = MDirection::Convert(sourceDir, outref)(); 
  }
  else {
    sourceDirJ2000 = sourceDir;
  }   
  if(!fluxStdPtr->setSource(sourceName,sourceDirJ2000)){
    if(verbose)
      os << LogIO::SEVERE
         << sourceName << " is not recognized by " << standardName(itsFluxScale)
         << LogIO::POST;
    // TODO?: Look for another standard that does recognize sourceName?
    return false;
  }
  else{
    direction_p = fluxStdPtr->getDirection();
    has_direction_p = True;
  }
  // Compute the flux density values and their uncertainties, returning whether
  // or not it worked.
  if (timeVariable) return (*fluxStdPtr)(values,errors,mfreqs,mtime,interpmethod_p);
  return (*fluxStdPtr)(values, errors, mfreqs);
}

// Like compute, but it also saves a ComponentList for the source to disk
// and returns the name (sourceName_mfreqGHzmtime.cl), making it suitable for
// resolved sources.
// mtime is ignored for nonvariable objects.
// Solar System objects are typically resolved and variable!
//
// Currently each component "list" only has 1 or 0 components.
//
// Inputs:
//    sourceName  const String&              Source name
//    mfreqs      const Vector<MFrequency>&  Desired frequencies
//    mtime       const MEpoch&              Desired time
// Output:
//    values      Vector<Flux>&              Computed total flux densities
//    errors      Vector<Flux>&              Flux density errors;
//                                           (0 => unknown).
//    clnames     Vector<String>&            Pathnames of the
//                                           ComponentLists.  "" if no
//                                           components were made.
//
Bool FluxStandard::computeCL(const String& sourceName,
                             const Vector<Vector<MFrequency> >& mfreqs,
                             const MEpoch& mtime, const MDirection& position,
                             Vector<Vector<Flux<Double> > >& values,
                             Vector<Vector<Flux<Double> > >& errors,
                             Vector<String>& clpaths,
			     const String& prefix)
{
  LogIO os(LogOrigin("FluxStandard", "computeCL"));
  uInt nspws = mfreqs.nelements();
  Bool success = False;

  if(itsFluxScale < FluxStandard::HAS_RESOLUTION_INFO){
    if(this->compute(sourceName, position, mfreqs, mtime, values, errors)){
      // Create a point component with the specified flux density.
      MDirection dummy;
      PointShape point(position.getValue().separation(dummy.getValue()) < 1e-7 &&
		       position.getRef() == dummy.getRef() ? direction_p : position);

      for(uInt spw = 0; spw < nspws; ++spw){
        clpaths[spw] = makeComponentList(sourceName, mfreqs[spw], mtime,
                                         values[spw], point,
					 prefix + "spw" + String::toString(spw) + "_");
      }
      success = True;
    }
  }
  else if(itsFluxScale == FluxStandard::SS_JPL_BUTLER){
    FluxCalc_SS_JPL_Butler ssobj(sourceName, mtime);
    Double angdiam;

    for(uInt spw = 0; spw < nspws; ++spw){
      ComponentType::Shape cmpshape = ssobj.compute(values[spw], errors[spw], angdiam,
                                                    mfreqs[spw], spw == 0);
    
      switch(cmpshape){
      case ComponentType::DISK:
        {
          // Create a uniform disk component with the specified flux density.
	  MDirection dummy;
          DiskShape disk;

          // Should we worry about tracking position?
          disk.setRefDirection(position.getValue().separation(dummy.getValue()) < 1e-7 &&
			       position.getRef() == dummy.getRef() ? ssobj.getDirection() :
			       position);

          disk.setWidthInRad(angdiam, angdiam, 0.0);

          clpaths[spw] = makeComponentList(sourceName, mfreqs[spw], mtime,
                                           values[spw], disk,
					   prefix + "spw" + String::toString(spw) + "_");
          success = True;
          break;
        }
      default: {
        ostringstream oss;

        oss << ComponentType::name(cmpshape) << " is not a supported component type.";
        throw(AipsError(String(oss)));
      }
      }
    }
  }
  return success;
}

void FluxStandard::setInterpMethod(const String& interpmethod)
{
  LogIO os(LogOrigin("FluxStandard", "setInterpMode"));
  if(interpmethod.contains("nearest")) {
    interpmethod_p = "nearestNeighbour";
  }
  else if(interpmethod.contains("linear")) {
    interpmethod_p = "linear";
  }
  else if(interpmethod.contains("cubic")) {
    interpmethod_p = "cubic"; 
  }
  else if(interpmethod.contains("spline")) {
    interpmethod_p = "spline";
  }
  else {
    ostringstream oss;
    oss << interpmethod << " is not a supported interpolation method";
    throw(AipsError(String(oss)));
  }  
}



String FluxStandard::makeComponentList(const String& sourceName,
                                       const Vector<MFrequency>& mfreqs,
                                       const MEpoch& mtime,
                                       const Vector<Flux<Double> >& values,
                                       const ComponentShape& cmp,
				       const String& prefix)
{
  LogIO os(LogOrigin("FluxStandard", "makeComponentList"));
  uInt nchans = mfreqs.nelements();

  if(nchans > 1){
    Vector<MVFrequency> freqvals(nchans);

    for(uInt c = 0; c < nchans; ++c)
      freqvals[c] = mfreqs[c].getValue();

    TabularSpectrum ts(mfreqs[0], freqvals, values, mfreqs[0].getRef());
          
    return makeComponentList(sourceName, mfreqs[0], mtime,
                             values[0], cmp, ts, prefix);
  }
  else{
    ConstantSpectrum cspectrum;

    return makeComponentList(sourceName, mfreqs[0], mtime,
                             values[0], cmp, cspectrum, prefix);
  }
}

String FluxStandard::makeComponentList(const String& sourceName,
                                       const MFrequency& mfreq,
                                       const MEpoch& mtime,
                                       const Flux<Double>& fluxval,
                                       const ComponentShape& cmp,
                                       const SpectralModel& spectrum,
				       const String& prefix)
{
  LogIO os(LogOrigin("FluxStandard", "makeComponentList"));

  // Make up the ComponentList's pathname.
  ostringstream oss;
  oss << prefix << sourceName << "_" //<< setprecision(1)
      << mfreq.get("GHz").getValue() << "GHz";
  //  String datetime;  // to nearest minute.
  oss << mtime.get("d").getValue() << "d.cl";
  String clpath(oss);
  // allow space as a part of the path
  //uInt nspaces = clpath.gsub(" ", "_");

  os << LogIO::DEBUG1
     << "sourceName: " << sourceName
     << "\nmfreq: " << mfreq.get("GHz").getValue() << "GHz"
     << "\nmtime: " << mtime.get("d").getValue() << "d"
   //  << "\nclpath: " << clpath << " (replaced " << nspaces
    // << " spaces)"
     << LogIO::POST;

  // If clpath already exists on disk, assume our work here is done, and don't
  // try to redo it.  It's not just laziness - it avoids collisions.
  // This happens when a continuum spw has the same center freq as a spectral
  // spw that is not being scaled by channel.
  File testExistence(clpath);
  if(!testExistence.isDirectory()){
    // Create a component list containing cmp, and force a call to its d'tor
    // using scoping rules.
    ComponentList cl;
    SkyComponent skycomp(fluxval, cmp, spectrum);
	
    cl.add(skycomp);
    cl.rename(clpath, Table::New);
  }
  return clpath;
}

//----------------------------------------------------------------------------

Bool FluxStandard::matchStandard (const String& name, 
				  FluxStandard::FluxScale& stdEnum,
				  String& stdName)
{
// Match an input string to a standard/catalog enum and descriptor
// Inputs:
//    name             const String&             Input string
// Output:
//    stdEnum          FluxStandard::FluxScale   Matching enum
//    stdName          String                    Standard descriptor for 
//                                               the matching enum.
//    matchStandard    Bool                      True if matched; False
//                                               if default returned.
//
  // Set default enum
  stdEnum = FluxStandard::PERLEY_TAYLOR_99;
  //  stdEnum = FluxStandard::PERLEY_BUTLER_2010;   // Not yet!

  // Local lowercase copy of input string
  String lname = name;
  lname.downcase();
  Bool matched = True;

  // Case input string match of:
  //
  // Perley (1990)
  if (lname.contains("perley") && 
      (lname.contains("90") || lname.contains("1990"))) {
    stdEnum = FluxStandard::PERLEY_90;
  }
  // Perley-Taylor (1995)
  else if (lname.contains("perley") && lname.contains("taylor") &&
      (lname.contains("95") || lname.contains("1995"))) {
    stdEnum = FluxStandard::PERLEY_TAYLOR_95;
  }
  // Perley-Taylor (1999)
  else if (lname.contains("perley") && lname.contains("taylor") &&
      (lname.contains("99") || lname.contains("1999"))) {
    stdEnum = FluxStandard::PERLEY_TAYLOR_99;
  }
  // Perley-Butler (2010)
  else if (lname.contains("perley") && lname.contains("butler") &&
      (lname.contains("10") || lname.contains("2010"))) {
    stdEnum = FluxStandard::PERLEY_BUTLER_2010;
  }
  // Perley-Butler (2013)
  else if (lname.contains("perley") && lname.contains("butler") &&
      (lname.contains("13") || lname.contains("2013"))) {
    stdEnum = FluxStandard::PERLEY_BUTLER_2013;
  }
  // Scaife & Heald (2012)
  else if (lname.contains("scaife") && lname.contains("heald") &&
      (lname.contains("12") || lname.contains("2012"))) {
    stdEnum = FluxStandard::SCAIFE_HEALD_2012;
  }
  // Baars
  else if (lname.contains("baars")) {
    stdEnum = FluxStandard::BAARS;
  }
  else if(lname.contains("jpl") || lname.contains("horizons")){
    stdEnum = FluxStandard::SS_JPL_BUTLER;
  }
  else
    matched = False;

  // Retrieve standard descriptor
  stdName = standardName (stdEnum);

  return matched;
}

//----------------------------------------------------------------------------

String FluxStandard::standardName (const FluxStandard::FluxScale& stdEnum)
{
// Return the standard descriptor for a specified standard/catalog enum
// Inputs:
//    stdEnum          FluxStandard::FluxScale   Standard/catalog enum
// Output:
//    standardName     String                    Standard descriptor
//
  // Case scale enum of:
  //
  String stdName;
  switch (stdEnum) {
  case BAARS: {
    stdName = "Baars";
    break;
  }
  case PERLEY_90: {
    stdName = "Perley 90";
    break;
  }
  case PERLEY_TAYLOR_95: {
    stdName = "Perley-Taylor 95";
    break;
  }
  case PERLEY_TAYLOR_99: {
    stdName = "Perley-Taylor 99";
    break;
  }
  case PERLEY_BUTLER_2010: {
    stdName = "Perley-Butler 2010";
    break;
  }
  case PERLEY_BUTLER_2013: {
    stdName = "Perley-Butler 2013";
    break;
  }
    case SCAIFE_HEALD_2012: {
    stdName = "Scaife-Heald 2012";
    break;
  }
  case SS_JPL_BUTLER: 
    {
      stdName = "JPL-Butler Solar System Object";
      break;
    }
  default: 
    {
      stdName = "unrecognized standard";
    }
  }
  return stdName;
}

//----------------------------------------------------------------------------
// End of FluxStandard definition.
//----------------------------------------------------------------------------

} //# NAMESPACE CASA - END

