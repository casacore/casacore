//# FluxCalc_SS_JPL_Butler.cc: Implementation of FluxCalc_SS_JPL_Butler.h
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
//----------------------------------------------------------------------------

//#include <components/ComponentModels/FluxStandard.h>
#include <components/ComponentModels/FluxCalc_SS_JPL_Butler.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Containers/Record.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/String.h>
#include <casa/Quanta.h>
#include <casa/Logging/LogIO.h>
#include <casa/OS/Directory.h>
#include <casa/OS/DirectoryIterator.h>
#include <casa/System/Aipsrc.h>
#include <casa/sstream.h>
#include <casa/iomanip.h>
#include <measures/Measures.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MCEpoch.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasComet.h>
#include <scimath/Mathematics/InterpolateArray1D.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ScalarColumn.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// Recommended constructor.
FluxCalc_SS_JPL_Butler::FluxCalc_SS_JPL_Butler(const String& objname,
                                               const MEpoch& time) :
  name_p(objname),
  hasName_p(true),
  time_p(time),
  hasTime_p(true),
  hasEphemInfo_p(false),
  hertz_p("Hz"),
  has_ra_p(false),
  has_dec_p(false),
  has_illu_p(false)
{
  hasObjNum_p = setObjNum();
}

FluxCalc_SS_JPL_Butler::FluxCalc_SS_JPL_Butler() :
  name_p(""),
  hasName_p(false),
  time_p(MEpoch()),
  hasTime_p(false),
  hasEphemInfo_p(false),
  hertz_p("Hz")
{
// Default constructor for making arrays, etc.
  hasObjNum_p = false;
  objnum_p    = FluxCalc_SS_JPL_Butler::N_KNOWN;
}

FluxCalc_SS_JPL_Butler::~FluxCalc_SS_JPL_Butler()
{
// Default destructor
}

Bool FluxCalc_SS_JPL_Butler::setObjNum()
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "setObjNum"));

  if(!hasName_p){
    os << LogIO::SEVERE
       << "Please provide the source name."
       << LogIO::POST;
    return false;
  }
  
  String lname = name_p;
  lname.downcase();
  Bool matched = true;
  
  if(lname == "mercury")
    objnum_p = FluxCalc_SS_JPL_Butler::Mercury;
  else if(lname == "venus")
    objnum_p = FluxCalc_SS_JPL_Butler::Venus;
  else if(lname == "mars")
    objnum_p = FluxCalc_SS_JPL_Butler::Mars;
  else if(lname == "jupiter")
    objnum_p = FluxCalc_SS_JPL_Butler::Jupiter;
  else if(lname == "io")
    objnum_p = FluxCalc_SS_JPL_Butler::Io;
  else if(lname == "ganymede")
    objnum_p = FluxCalc_SS_JPL_Butler::Ganymede;
  else if(lname == "europa")
    objnum_p = FluxCalc_SS_JPL_Butler::Europa;
  else if(lname == "callisto")
    objnum_p = FluxCalc_SS_JPL_Butler::Callisto;
  else if(lname == "uranus")
    objnum_p = FluxCalc_SS_JPL_Butler::Uranus;
  else if(lname == "neptune")
    objnum_p = FluxCalc_SS_JPL_Butler::Neptune;
  else if(lname == "triton")
    objnum_p = FluxCalc_SS_JPL_Butler::Triton;
  else if(lname == "pluto")
    objnum_p = FluxCalc_SS_JPL_Butler::Pluto;
  else if(lname == "titan")
    objnum_p = FluxCalc_SS_JPL_Butler::Titan;
  else if(lname == "ceres")
    objnum_p = FluxCalc_SS_JPL_Butler::Ceres;
  else if(lname == "pallas")
    objnum_p = FluxCalc_SS_JPL_Butler::Pallas;
  else if(lname == "vesta")
    objnum_p = FluxCalc_SS_JPL_Butler::Vesta;
  else if(lname == "juno")
    objnum_p = FluxCalc_SS_JPL_Butler::Juno;
  else if(lname == "victoria")
    objnum_p = FluxCalc_SS_JPL_Butler::Victoria;
  else if(lname == "davida")
    objnum_p = FluxCalc_SS_JPL_Butler::Davida;
  else{
    os << LogIO::SEVERE
       << "Sorry, no flux density model for " << name_p
       << "\n (not even a rudimentary one)."
       << LogIO::POST;
    matched = false;
  }
  
  // Changing the object invalidates the cached ephemeris info (if any).
  hasEphemInfo_p = false;
  
  return matched;
}

Bool FluxCalc_SS_JPL_Butler::getName(String& output) const
{
  if(!hasName_p)
    return false;
  output = name_p;
  return true;
}

Bool FluxCalc_SS_JPL_Butler::getTime(MEpoch& output) const
{
  if(!hasTime_p)
    return false;
  output = time_p;
  return true;
}

void FluxCalc_SS_JPL_Butler::setTime(const MEpoch& time)
{
  time_p = time;

  // Changing the time *possibly* invalidates the cached ephemeris info (if
  // any).  Leave it up to readEphem() to decide whether or not the ephemeris
  // info is valid (i.e. the new time is close enough to the old time).
  hasEphemInfo_p = false;  
}

ComponentType::Shape FluxCalc_SS_JPL_Butler::getShape(Double& angdiam)
{
  if(!hasEphemInfo_p && !readEphem())
    return ComponentType::UNKNOWN_SHAPE;
  
  angdiam = 2.0 * mean_rad_p / delta_p;
  return ComponentType::DISK;
}

MDirection FluxCalc_SS_JPL_Butler::getDirection()
{
  if((!hasEphemInfo_p && !readEphem()) || !(has_ra_p && has_dec_p))
    return MDirection();
  
  return MDirection(MVDirection(Quantity(ra_p, "deg"),
				Quantity(dec_p, "deg")), MDirection::J2000);
}

Double FluxCalc_SS_JPL_Butler::getHeliocentricDist()
{
  Double dist;

  if(!hasEphemInfo_p && !readEphem())
    dist = -1.0;
  if(!has_r_p)
    dist = -1.0;
  else
    dist = r_p;
  return dist;
}

uInt FluxCalc_SS_JPL_Butler::n_known() const
{
  return N_KNOWN;
}

Bool FluxCalc_SS_JPL_Butler::readEphem()
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "readEphem"));

  if(!hasName_p || !hasTime_p){
    os << LogIO::SEVERE
       << "The source and time have not been set."
       << LogIO::POST;
    return false;
  }
  
  // Try to find a matching JPL-Horizons ephemeris table.
  // Note: these are not the same as the DE200 and DE405 JPL tables used
  // by measures to get the direction to a planet.
  // There may be more than one because of overlapping date ranges.
  const String tabpat(Regex::makeCaseInsensitive(name_p +
                                                 "_[-0-9.]+-[-0-9.]+[ydhms].+\\.tab"));

  // Look for tabpat, in order, in '.', user.ephemerides.directory, and
  // Aipsrc::findDir(horpath, "data/ephemerides/JPL-Horizons").
  uInt nephdirs = 1;
  String userpath;
  Bool foundUserDir = Aipsrc::find(userpath, "user.ephemerides.directory");
  if(foundUserDir)
    ++nephdirs;
  String horpath;
  Bool foundStd = Aipsrc::findDir(horpath, "data/ephemerides/JPL-Horizons");
  if(foundStd)
    ++nephdirs;
  
  Vector<String> ephdirs(nephdirs);
  ephdirs[0] = ".";
  if(foundUserDir)
    ephdirs[1] = userpath;
  if(foundStd)
    ephdirs[nephdirs - 1] = horpath;  // nephdirs = 2 + foundUserDir

  Bool foundObj = false;
  Bool found = false;        // = foundObj && right time. 	 
  Path path; 	 
  String edir("."); 	 
  for(uInt pathnum = 0; pathnum < nephdirs && !found; ++pathnum){ 	 
    edir = ephdirs[pathnum]; 	 
	  	 
    os << LogIO::NORMAL2
       << "Looking for an ephemeris table matching " << tabpat
       << "\n\tin " << edir
       << LogIO::POST;
  
    Directory hordir(edir);
    DirectoryIterator dirIter(hordir, tabpat);
    uInt firstTimeStart = name_p.length() + 1;  // The + 1 is for the _.
    Regex timeUnitPat("[ydhms]");

    while(!dirIter.pastEnd()){
      path = dirIter.name();
      foundObj = true;
      String basename(path.baseName());

      // Look for, respectively, the positions of '--', 'd', and '.' in
      // '-12345--67890dUTC.tab'.  Note that, just to be tricky, the times in
      // this example are negative. 
      uInt firstTimeLen = basename.find('-', firstTimeStart + 1) - firstTimeStart;
      uInt lastTimeLen = basename.find(timeUnitPat,
				       firstTimeStart + firstTimeLen + 1)
	- firstTimeStart - firstTimeLen - 1;
      uInt unitPos  = firstTimeStart + firstTimeLen + 1 + lastTimeLen;
    
      Double firstTime = String::toDouble(basename.substr(firstTimeStart, firstTimeLen));
      Double lastTime = String::toDouble(basename.substr(firstTimeStart + firstTimeLen + 1,
							 lastTimeLen));
      Unit unit(basename[unitPos]);
      String ref(basename.substr(unitPos + 1,
				 basename.find('.', unitPos + 1) - unitPos - 1));
    
      os << LogIO::DEBUG1
	 << basename << ": (first, last)time = ("
	 << firstTime << ", " << lastTime << ")" << unit.getName()
	 << " " << ref
	 << LogIO::POST;
    
      MEpoch::Types refEnum;
      Bool refIsValid = MEpoch::getType(refEnum, ref);
    
      if(refIsValid){
	MEpoch::Convert mtimeToDirFrame(time_p, MEpoch::Ref(refEnum));
	MEpoch mtimeInDirFrame(mtimeToDirFrame());
	Double dtime = mtimeInDirFrame.get(unit).getValue();

	if(dtime <= lastTime && dtime >= firstTime){
	  found = true;
	  break;
	}
      }
      // else maybe tabpat isn't specific enough.  Don't panic yet.

      ++dirIter;
    }
  }

  if(!found){
    os << LogIO::SEVERE;
    if(foundObj)
      os << "Found an ephemeris for " << name_p << ", but not";
    else
      os << "Could not find an ephemeris table for " << name_p;

    // MEpoch cannot directly << to a LogIO.
    os << " at ";
    os.output() << MEpoch::Convert(time_p, MEpoch::Ref(MEpoch::UTC))();
    os << LogIO::POST;
    return false;
  }
  else{
    os << LogIO::NORMAL
       << "Using ephemeris table " << path.baseName()
       << LogIO::POST;
  }

  // path.absoluteName() is liable to give something like cwd +
  // path.baseName(), because path was never given horpath.
  const String abspath(edir + "/" + path.baseName());

  if(!Table::isReadable(abspath)){
    os << LogIO::SEVERE
       << abspath << " is not a readable table."
       << LogIO::POST;
    return false;
  }

  const Table tab(abspath);
  const TableRecord ks(tab.keywordSet());

  Bool got_q = true;
  temperature_p = MeasComet::get_Quantity_keyword(ks, "T_mean", "K", got_q);
  if(!got_q)
    temperature_p = -1;  // Hopefully a model for the obj will supply a
			 // temperature later.
  mean_rad_p = MeasComet::get_Quantity_keyword(ks, "meanrad", "AU", got_q);
  if(!got_q){
    mean_rad_p = -1.0;
    os << LogIO::SEVERE		// Remove/modify this when it starts supporting triaxiality.
       << "The table is missing the meanrad keyword, needed to calculate the apparent diameter."
       << LogIO::POST;
    return false;
  }

  // Find the row numbers with the right MJDs.
  ROScalarColumn<Double> mjd(tab, "MJD");
  uInt rowbef;
  uInt rowclosest;
  uInt rowaft;
  if(!get_row_numbers(rowbef, rowclosest, rowaft, mjd)){
    os << LogIO::SEVERE
       << "The table does not appear to cover the right time."
       << LogIO::POST;
    return false;
  }

  Double tm1 = mjd(rowbef);
  Double t0  = mjd(rowclosest);
  Double tp1 = mjd(rowaft);
  Double f = time_p.get("d").getValue() - t0;
  Double dt = tp1 - tm1;
  Double tp1mt0 = tp1 - t0;
  Double t0mtm1 = t0 - tm1;

  // The distance from Earth to the object, in AU, is mandatory.
  // JPL calls it delta, and MeasComet calls it Rho.
  hasEphemInfo_p = found && get_interpolated_value(delta_p, "Rho",
                                                   tab, rowbef, rowclosest,
                                                   rowaft, f, dt, tp1mt0,
                                                   t0mtm1, True);

  // Heliocentric distance, in AU.
  has_r_p = get_interpolated_value(r_p, "r", tab, rowbef, rowclosest, rowaft,
                                   f, dt, tp1mt0, t0mtm1, False);

  // Illumination, in %.
  has_illu_p = get_interpolated_value(illu_p, "illu", tab, rowbef, rowclosest,
                                      rowaft, f, dt, tp1mt0, t0mtm1, False);
  if(has_illu_p)
    has_illu_p *= 0.01;  // Convert it to a fraction.

  // RA, in deg.
  has_ra_p = get_interpolated_value(ra_p, "RA", tab, rowbef, rowclosest,
                                    rowaft, f, dt, tp1mt0, t0mtm1, False);

  // Declination, in deg.
  has_dec_p = get_interpolated_value(dec_p, "DEC", tab, rowbef, rowclosest,
                                     rowaft, f, dt, tp1mt0, t0mtm1, False);

  return found;
}

Bool FluxCalc_SS_JPL_Butler::get_interpolated_value(Double& val,
                                                    const String& colname,
                                                    const Table& tab,
                                                    const uInt rowbef,
                                                    const uInt rowclosest,
                                                    const uInt rowaft,
                                                    const Double f,
                                                    const Double dt,
                                                    const Double tp1mt0,
                                                    const Double t0mtm1,
                                                    const Bool verbose)
{
  Bool foundIt = False;
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "get_interpolated_value"));

  if(tab.actualTableDesc().isColumn(colname)){
    Double myf = f;
    Double d2y = 0.0;

    ROScalarColumn<Double> col(tab, colname);
    Double col_m1 = col(rowbef);
    Double col_0  = col(rowclosest);
    Double col_p1 = col(rowaft);
    
    if(dt > 0){
      myf /= dt;
      if(t0mtm1 > 0.0 && tp1mt0 > 0.0){
        d2y = (col_p1 - col_0) / tp1mt0;
        d2y -= (col_0 - col_m1) / t0mtm1;
        d2y *= dt;
      }
    }
    else{
      if(verbose){
        os << LogIO::NORMAL
           << "The table is not long enough for quadratic interpolation.\n"
           << "Nearest neighbor will be used."
           << LogIO::POST;
      }
      myf = 0.0;
    }
    val = col_0 + myf * (col_p1 - col_m1 + myf * d2y);
    foundIt = True;
  }
  else
    os << LogIO::NORMAL
       << "The table does not have a " << colname << " column."
       << LogIO::POST;
  return foundIt;
}

Bool FluxCalc_SS_JPL_Butler::get_row_numbers(uInt& rowbef, uInt& rowclosest,
					     uInt& rowaft,
					     const ROScalarColumn<Double>& mjd)
{
  // MeasComet requires a constant time increment, but since
  // FluxCalc_SS_JPL_Butler is expected to only need to use the time once, it's
  // not too expensive to allow tables with varying time increments.  As long
  // as mjd is monotonically increasing, the search is at worst O(log(n)).

  Double mjd0 = mjd(0);
  Double dmjd = mjd0;
  Int ndates = mjd.nrow();      // Don't bother trying uInts in this 
  Int step = 1;                 // function - it just leads to several
  Long rn = 0;                  // compiler warnings.

  Int ub = ndates - 1;
  Double the_time = time_p.get("d").getValue();

  if(mjd(ub) < the_time){
    return false;
  }
  else if(mjd(ub) == the_time){
    rn = ub;
    step = 0;	// Prevents going through the while loop below.
  }
  Int lb = 0;
  if(mjd(0) > the_time){
    return false;
  }
  else if(mjd(0) == the_time){
    rn = 0;
    step = 0;	// Prevents going through the while loop below.
  }    

  Int i;
  for(i = 1; dmjd == mjd0 && i < ndates; ++i)
    dmjd = mjd(i);
  if(i > 1)
    --i;
  dmjd = (dmjd - mjd0) / i;

  if(dmjd > 0.0 && step){
    rn = lrint((the_time - mjd0) / dmjd);
    if(rn < 0)
      rn = 0;
    else if(rn > ndates)
      rn = ndates - 1;
  }

  Double mjdrn = mjd(rn);
  Bool increasing = mjdrn < the_time;
  Int paranoia = 0;

  while(step && paranoia < ndates){
    if(mjdrn < the_time){
      if(rn > lb)
	lb = rn;
      if(increasing){
	step *= 2;
      }
      else{
	step /= 2;
	increasing = true;
      }
    }
    else{
      if(rn < ub)
	ub = rn;
      if(increasing){
	step /= 2;
	increasing = false;
      }
      else{
	step *= 2;
      }
    }
    if(increasing){
      if(rn + step > ub)
	step = ub - rn - 1;
      rn += step;
    }
    else{
      if(rn - step < lb)
	step = rn - lb - 1;
      rn -= step;
    }
    mjdrn = mjd(rn);
    ++paranoia;
  }
  if(paranoia == ndates)
    return false;

  rowclosest = rn;
  rowaft = (rn < ndates - 1) ? rn + 1 : rn;
  rowbef = (rn > 0) ? rn - 1 : rn;
  return true;
}

ComponentType::Shape FluxCalc_SS_JPL_Butler::compute(Vector<Flux<Double> >& values,
                                                     Vector<Flux<Double> >& errors,
                                                     Double& angdiam,
                                                     const Vector<MFrequency>& mfreqs,
						     const Bool report)
{
  // LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute"));

  // Calls readEphem() if necessary.
  ComponentType::Shape rettype(getShape(angdiam));
  if(rettype == ComponentType::UNKNOWN_SHAPE)
    return rettype;
  
  if(!hasObjNum_p){
    hasObjNum_p = setObjNum();  // Also has its own errmsgs.
    if(!hasObjNum_p)
      return ComponentType::UNKNOWN_SHAPE;
  }

  switch(objnum_p){
  case FluxCalc_SS_JPL_Butler::Venus:
    compute_venus(values, errors, angdiam, mfreqs);
    break;
  case FluxCalc_SS_JPL_Butler::Jupiter:
    compute_jupiter(values, errors, angdiam, mfreqs);
    break;
  case FluxCalc_SS_JPL_Butler::Uranus:
    compute_uranus(values, errors, angdiam, mfreqs);
    break;
  case FluxCalc_SS_JPL_Butler::Neptune:
    compute_neptune(values, errors, angdiam, mfreqs);
    break;
  default:
    Bool success = compute_constant_temperature(values, errors, angdiam, mfreqs,
						report);
    if(!success)
      return ComponentType::UNKNOWN_SHAPE;
  };

  return rettype;
}

void FluxCalc_SS_JPL_Butler::compute_BB(Vector<Flux<Double> >& values,
                                        Vector<Flux<Double> >& errors,
                                        const Double angdiam,
                                        const Vector<MFrequency>& mfreqs)
{
  const uInt nfreqs = mfreqs.nelements();
  Quantum<Double> temperature(temperature_p, "K");

  // The real peak frequency is about 2.82 x this.
  Quantum<Double> freq_peak(QC::k * temperature / QC::h);

  Quantum<Double> rocd2(0.5 * angdiam);	// Dimensionless for now.

  rocd2 /= QC::c;	// Don't put this in the c'tor, it'll give the wrong answer.
  rocd2 *= rocd2;

  // Frequency independent factor.
  Quantum<Double> freq_ind_fac(2.0e26 * QC::h * C::pi * rocd2);

  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_BB"));
  os << LogIO::DEBUG1
     << "angdiam = " << angdiam << " rad"
     << "\nrocd2 = " << rocd2.getValue() << rocd2.getUnit()
     << "\nfreq_ind_fac = " << freq_ind_fac.getValue() << freq_ind_fac.getUnit()
     << "\npeak freq = " << 2.82e-12 * freq_peak.get(hertz_p).getValue() << " THz"
     << "\ntemperature_p = " << temperature_p << " K"
     << "\nvalues[0].unit() = " << values[0].unit().getName()
     << "\nhertz_p = " << hertz_p.getName()
     << LogIO::POST;

  const Unit jy("Jy");

  for(uInt f = 0; f < nfreqs; ++f){
    Quantum<Double> freq(mfreqs[f].get(hertz_p));
    
    values[f].setUnit(jy);
    Double fd = (freq_ind_fac * freq * freq * freq).getValue() /
                (exp((freq / freq_peak).getValue()) - 1.0);
    os << LogIO::DEBUG1
       << "f.d.(" << freq.getValue() << " Hz" << ") = " << fd
       << LogIO::POST;
    values[f].setValue(fd);
    errors[f].setValue(0.0);
  }
}

void FluxCalc_SS_JPL_Butler::compute_GB(Vector<Flux<Double> >& values,
                                        Vector<Flux<Double> >& errors,
                                        const Double angdiam,
                                        const Vector<MFrequency>& mfreqs,
                                        const Vector<Double>& temps)
{
  const uInt nfreqs = mfreqs.nelements();
  Quantum<Double> rocd2(0.5 * angdiam);	// Dimensionless for now.

  rocd2 /= QC::c;	// Don't put this in the c'tor, it'll give the wrong answer.
  rocd2 *= rocd2;

  // Frequency independent factor.
  Quantum<Double> freq_ind_fac(2.0e26 * QC::h * C::pi * rocd2);

  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_GB"));
  os << LogIO::DEBUG1
     << "angdiam = " << angdiam << " rad"
     << "\nrocd2 = " << rocd2.getValue() << rocd2.getUnit()
     << "\nfreq_ind_fac = " << freq_ind_fac.getValue() << freq_ind_fac.getUnit()
     << LogIO::POST;

  const Unit jy("Jy");

  for(uInt f = 0; f < nfreqs; ++f){
    Quantum<Double> freq(mfreqs[f].get(hertz_p));

    // Guard against wayward extrapolations (possible with compute_venus()),
    // but do not emit a warning here; there may be many frequencies with bad
    // temperatures, and the warning should come from the calling function
    // anyway since it knows the limits of the model.
    //
    // I am not really claiming that the CMB temperature is the most reasonable
    // minimum temperature, but it is *a* reasonable mininum temperature, and I
    // want to avoid division by zero.
    //
    Quantum<Double> temperature(max(temps[f], 2.7), "K");

    // The real peak frequency is about 2.82 x this.
    Quantum<Double> freq_peak(QC::k * temperature / QC::h);
    
    values[f].setUnit(jy);
    Double fd = (freq_ind_fac * freq * freq * freq).getValue() /
                (exp((freq / freq_peak).getValue()) - 1.0);
    values[f].setValue(fd);
    errors[f].setValue(0.0);

    // Take this out when it's served its purpose, since it's in a possibly
    // long loop.
    // os << LogIO::DEBUG2   
    //    << "f = 0 (" << 1e-12 * freq.get(hertz_p).getValue() << " THz):\n"
    //    << "temperature = " << temps[f] << " K\n"
    //    << "freq_peak = " << 1e-12 * freq_peak.get(hertz_p).getValue() << " THz\n"
    //    << "f.d. = " << fd
    //    << LogIO::POST;
  }
  os << LogIO::DEBUG1
     << "hertz_p = " << hertz_p.getName()
     << "\nvalues[0].unit() = " << values[0].unit().getName()
     << LogIO::POST;
}

void FluxCalc_SS_JPL_Butler::compute_venus(Vector<Flux<Double> >& values,
					   Vector<Flux<Double> >& errors,
					   const Double angdiam,
					   const Vector<MFrequency>& mfreqs)
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_venus"));
  const uInt nfreqs = mfreqs.nelements();
  Vector<Double> temps(nfreqs);
  Vector<Float> ghzfreqs(nfreqs);

  Float minfreq = 0.303;
  Float maxfreq = 350.0;
  for(uInt f = 0; f < nfreqs; ++f){
    Float ghzfreq = 1.0e-9 * mfreqs[f].get(hertz_p).getValue();

    if(ghzfreq < minfreq)
      minfreq = ghzfreq;
    else if(ghzfreq > maxfreq)
      maxfreq = ghzfreq;
    ghzfreqs[f] = ghzfreq;
  }
  
  const uInt nmeas = 75;

  // GHz.  Nominally const, but there is no constructor for making a Block from
  // a const C array.
  Float measfreqarr[nmeas] = {350.000, 318.182, 289.256, 262.960, 239.055,
			      217.322, 197.566, 179.605, 163.278, 148.434,
			      134.940, 122.673, 111.521, 101.383,  92.166,
			       83.787,  76.170,  69.246,  62.951,  57.228,
			       52.025,  47.296,  42.996,  39.087,  35.534,
			       32.304,  29.367,  26.697,  24.270,  22.064,
			       20.058,  18.235,  16.577,  15.070,  13.700,
			       12.454,  11.322,  10.293,   9.357,   8.507,
			        7.733,   7.030,   6.391,   5.810,   5.282,
			        4.802,   4.365,   3.968,   3.608,   3.280,
			        2.981,   2.710,   2.464,   2.240,   2.036,
			        1.851,   1.683,   1.530,   1.391,   1.264,
			        1.149,   1.045,   0.950,   0.864,   0.785,
			        0.714,   0.649,   0.590,   0.536,   0.487,
			        0.443,   0.403,   0.366,   0.333,   0.303};
  Float *measfreqptr = measfreqarr;		// Need a referenceable pointer.
  const Block<Float> measfreqblk(nmeas, measfreqptr, false);

  // Double to match the type of temps.  Nominally const, but there is no
  // constructor for making a Block from a const C array.
  Double meastbarr[nmeas] = {270.2, 273.8, 277.7, 282.0, 286.8,
			     292.1, 297.6, 303.5, 309.7, 316.1,
			     322.8, 329.6, 336.6, 343.7, 351.1,
			     358.7, 366.7, 374.9, 383.6, 392.7,
			     402.3, 412.3, 422.8, 433.8, 445.3,
			     457.3, 469.8, 482.8, 496.1, 509.9,
			     524.1, 538.8, 553.7, 568.7, 583.8,
			     598.7, 613.0, 626.5, 638.9, 648.0,
			     657.0, 665.0, 671.0, 676.0, 679.0,
			     680.0, 680.0, 679.0, 676.0, 671.0,
			     664.0, 655.0, 646.0, 638.0, 631.0,
			     624.0, 617.0, 610.0, 604.0, 598.0,
			     592.0, 586.0, 580.0, 575.0, 570.0,
			     565.0, 560.0, 555.0, 550.0, 545.0,
			     540.0, 535.0, 530.0, 525.0, 520.0};
  Double *meastbptr = meastbarr;
  const Block<Double> meastbblk(nmeas, meastbptr, false);

  // Just let it extrapolate if necessary; the temperature_p given in the
  // ephemeris (735K) is so high that I think it's for the surface.
  InterpolateArray1D<Float, Double>::interpolate(temps, ghzfreqs,
						 Vector<Float>(measfreqblk),
						 Vector<Double>(meastbblk),
						 InterpolateArray1D<Float, Double>::cubic);

  if(minfreq < 0.303)
    os << LogIO::WARN
       << "At least one of the frequencies, " << minfreq
       << "GHz, is below the lower limit for the model (0.303GHz).\n"
       << LogIO::POST;
  if(maxfreq > 350.0)
    os << LogIO::WARN
       << "At least one of the frequencies, " << maxfreq
       << "GHz, is above the upper limit for the model (350.0GHz).\n"
       << LogIO::POST;
  if(minfreq < 0.303 || maxfreq > 350.0)
    os << LogIO::WARN
       << "The extrapolation may be very bad."
       << LogIO::POST;

  compute_GB(values, errors, angdiam, mfreqs, temps);
}

void FluxCalc_SS_JPL_Butler::compute_jupiter(Vector<Flux<Double> >& values,
                                             Vector<Flux<Double> >& errors,
                                             const Double angdiam,
                                             const Vector<MFrequency>& mfreqs)
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_jupiter"));
  Bool outOfFreqRange = false;
  const uInt nfreqs = mfreqs.nelements();
  Vector<Double> temps(nfreqs);

  for(uInt f = 0; f < nfreqs; ++f){
    Double freq = mfreqs[f].get(hertz_p).getValue();
    Double lambdacm = 100.0 * C::c / freq;      // Wavelength in cm.

    if(lambdacm < 0.1){
      outOfFreqRange = true;
      lambdacm = 0.1;
    }
    else if(lambdacm > 6.2){
      outOfFreqRange = true;
      lambdacm = 6.2;
    }

    if(lambdacm < 0.44){
      temps[f] = 170.0;
    }
    else if(lambdacm < 0.7){
      // 21.537539 = 10.0 / ln(0.7 / 0.44)
      temps[f] = 160.0 + 21.537539 * log(0.7 / lambdacm);
    }
    else if(lambdacm < 1.3){
      // 48.462196889 = 30.0 / ln(1.3 / 0.7)
      temps[f] = 130.0 + 48.462196889 * log(1.3 / lambdacm);
    }
    else
      // 65.38532335444 = 100.0 / ln(6.0 / 1.3)
      temps[f] = 130.0 + 65.38532335444 * log10(lambdacm / 1.3);
  }

  if(outOfFreqRange)
    os << LogIO::WARN
       << "At least one of the wavelengths went outside the nominal range\n"
       << "of 1mm to 6.2cm, so the wavelength was clamped to 1mm or 6.2cm for\n"
       << "calculating the effective temperature of Jupiter."
       << LogIO::POST;

  compute_GB(values, errors, angdiam, mfreqs, temps);
}

void FluxCalc_SS_JPL_Butler::compute_uranus(Vector<Flux<Double> >& values,
                                            Vector<Flux<Double> >& errors,
                                            const Double angdiam,
                                            const Vector<MFrequency>& mfreqs)
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_uranus"));
  Bool outOfFreqRange = false;
  const uInt nfreqs = mfreqs.nelements();
  Vector<Double> temps(nfreqs);

  for(uInt f = 0; f < nfreqs; ++f){
    Double freq = mfreqs[f].get(hertz_p).getValue();
    Double lambdacm = 100.0 * C::c / freq;      // Wavelength in cm.

    if(lambdacm < 0.07){
      outOfFreqRange = true;
      lambdacm = 0.07;
    }
    else if(lambdacm > 6.2){
      outOfFreqRange = true;
      lambdacm = 6.2;
    }

    if(lambdacm < 0.4){
      // 32.46063842 = 40.0 / ln(4.0)
      temps[f] = 90.0 + 32.46063842 * log(10.0 * lambdacm);
    }
    else if(lambdacm < 1.0){
      temps[f] = 135.0;
    }
    else
      temps[f] = 135.0 + 105.0 * log10(lambdacm);
  }

  if(outOfFreqRange)
    os << LogIO::WARN
       << "At least one of the wavelengths went outside the nominal range\n"
       << "of 0.7mm to 6.2cm, so the wavelength was clamped at either 0.7mm or 6.2cm\n"
       << "for calculating the effective temperature of Uranus."
       << LogIO::POST;

  compute_GB(values, errors, angdiam, mfreqs, temps);
}

void FluxCalc_SS_JPL_Butler::compute_neptune(Vector<Flux<Double> >& values,
                                             Vector<Flux<Double> >& errors,
                                             const Double angdiam,
                                             const Vector<MFrequency>& mfreqs)
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_neptune"));
  Bool outOfFreqRange = false;
  const uInt nfreqs = mfreqs.nelements();
  Vector<Double> temps(nfreqs);

  for(uInt f = 0; f < nfreqs; ++f){
    Double freq = 1.0e-9 * mfreqs[f].get(hertz_p).getValue(); // GHz

    // These temperatures agree well with the ones collected in 
    // http://adsabs.harvard.edu/abs/1995EM&P...67...89S (Spilker)
    // (0.1-20cm = 1.5-300GHz) and 
    // http://www.ericweisstein.com/research/papers/applopt/node10.html
    // (200-300GHz) except around 70 GHz.  It's not so much a knee
    // at 70 GHz as a flat interval around 70 GHz.  Unfortunately I don't yet
    // have any data right there, but Planck should publish better models for
    // the planets.
    //
    // (Spilker attributed the flat interval or dip in Jupiter, Saturn, and
    //  Neptune to NH3.  Uranus has barely any, at least while we're looking at
    //  its pole.)
    //
    if(freq < 4.0){
      outOfFreqRange = true;
      freq = 4.0;
    }
    else if(freq > 1000.0){
      outOfFreqRange = true;
      freq = 1000.0;
    }
    
    if(freq < 70.0){
      // 30.083556662 = 80.0 / ln(1000.0 / 70.0)
      temps[f] = 140.0 - 30.083556662 * log(freq / 70.0);
    }
    else
      // 34.93815 = 100.0 / ln(70.0 / 4.0)
      temps[f] = 240.0 - 34.93815 * log(freq / 4.0);
  }

  if(outOfFreqRange)
    os << LogIO::WARN
       << "At least one of the frequencies went outside the nominal range\n"
       << "of 4 to 1000 GHz for Neptune, so it was clamped to 4 or 1000 GHz\n"
       << "for calculating the effective temperature."
       << LogIO::POST;

  compute_GB(values, errors, angdiam, mfreqs, temps);
}

Bool FluxCalc_SS_JPL_Butler::compute_constant_temperature(Vector<Flux<Double> >& values,
							  Vector<Flux<Double> >& errors,
							  const Double angdiam,
							  const Vector<MFrequency>& mfreqs,
							  const Bool report)
{
  LogIO os(LogOrigin("FluxCalc_SS_JPL_Butler", "compute_constant_temperature"));

  Bool success = true;
  Double ephem_temp = temperature_p;    // Store it.

  switch(objnum_p){
  case FluxCalc_SS_JPL_Butler::Pluto:
    if(report)
      os << LogIO::NORMAL1
	 << "Using the value from:"
	 << "  Altenhoff, W. J., R. Chini, H. Hein, E. Kreysa, P. G. Mezger, "
	 << "     C. Salter, and J. B. Schraml, First radio astronomical estimate "
	 << "     of the temperature of Pluto, A&ALett, 190, L15-L17, 1988"
	 << "which is: Tb = 35 K at 1.27 mm.  this is a correction from the "
	 << "value of 39 K in the paper, due to the incorrect geometric mean size "
	 << "used for Pluto and Charon (1244 km vs the correct 1320 km).  this is"
	 << "similar to the value found in:"
	 << "  Stern, S. A., D. A. Weintraub, and M. C. Festou, Evidence for a Low "
	 << "     Surface Temperature on Pluto from Millimeter-Wave Thermal"
	 << "     Emission Measurements, Science, 261, 1713-1716, 1993"
	 << "and is a good match to the physical temperature reported in:"
	 << "  Tryka, K. A., R. H. Brown, D. P. Cruikshank, T. C. Owen, T. R."
	 << "     Geballe, and C. DeBergh, Temperature of Nitrogen Ice on Pluto and"
	 << "     Its Implications for Flux Measurements, Icarus, 112, 513-527, "
	 << "     1994"
	 << "where they give a surface temperature of 40+-2 K.  this would imply"
	 << "an emissivity of 0.875, which is certainly reasonable."
	 << LogIO::POST;
    temperature_p = 35.0;
    break;
  case FluxCalc_SS_JPL_Butler::Io:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Io's temperature (110K):\n"
	 << "Rathbun, J.A., Spencer, J.R. Tamppari, L.K., Martin, T.Z., Barnard, L.,\n"
	 << "Travis, L.D. (2004). \"Mapping of Io's thermal radiation by the Galileo\n"
	 << "photopolarimeter-radiometer (PPR) instrument\". Icarus 169:127-139.\n"
	 << "doi:10.1016/j.icarus.2003.12.021\n"
	 << LogIO::POST;
    temperature_p = 110.0;
    break;
  case FluxCalc_SS_JPL_Butler::Ganymede:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Ganymede's temperature (110K):\n"
	 << "Delitsky, Mona L., Lane, Arthur L. (1998). \"Ice chemistry of Galilean satellites\"\n"
	 << "J. of Geophys. Res. 103 (E13): 31,391-31,403. doi:10.1029/1998/JE900020\n"
	 << "http://trs-new.jpl.nasa.gov/dspace/bitstream/2014/20675/1/98-1725.pdf"
	 << LogIO::POST;
    temperature_p = 110.0;
    break;
  case FluxCalc_SS_JPL_Butler::Callisto:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Callisto's temperature (134 +- 11 K):\n"
	 << "Moore, Jeffrey M., Chapman, Clark R., Bierhaus, Edward B. et al\n"
	 << "(2004). \"Callisto\" In Bagenal, F., Dowling, T.E., McKinnon, W.B.,\n"
	 << "\"Jupiter: The Planet, Satellites, and Magnetosphere\". Cambridge Univ. Press"
	 << LogIO::POST;
    temperature_p = 134.0;   	// +- 11.
    break;
  case FluxCalc_SS_JPL_Butler::Europa:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Europa's temperature (109 K):\n"
	 << "http://science.nasa.gov/science-news/science-at-nasa/1998/ast03dec98_1/"
	 << LogIO::POST;
    temperature_p = 109.0;
    break;
  case FluxCalc_SS_JPL_Butler::Titan:
    temperature_p = 76.6;
    break;
  case FluxCalc_SS_JPL_Butler::Triton:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Triton's temperature (38 K):\n"
	 << "http://solarsystem.nasa.gov/planets/profile.cfm?Object=Triton"
	 << LogIO::POST;
    temperature_p = 38.0;
    break;
  case FluxCalc_SS_JPL_Butler::Ceres:
    if(report)
      os << LogIO::NORMAL1
	 << "Reference for Ceres' mean temperature (167K):\n"
	 << "Saint-Pe, O., Combes, N., Rigaut, F. (1993). \"Ceres surface properties\n"
	 << "by high-resolution imaging from Earth\" Icarus 105:271-281.\n"
	 << "doi:10.1006/icar.1993.1125"
	 << LogIO::POST;
    temperature_p = 167.0;
    break;
  case FluxCalc_SS_JPL_Butler::Pallas:
    if(report)
      os << LogIO::WARN
	 << "The orbit of Pallas has an eccentricity of 0.231.  This is not yet accounted\n"
	 << "for when calculating its temperature (taken to be 164K)."
	 << LogIO::POST;
    temperature_p = 164.0;
    break;
  case FluxCalc_SS_JPL_Butler::Juno:
    if(report){
      os << LogIO::WARN
	 << "Juno has a large crater and temperature changes that CASA does not fully account for."
	 << LogIO::POST;
      os << LogIO::NORMAL1
	 << "Reference for Juno's mean temperature (163K):\n"
	 << "Lim, Lucy F., McConnochie, Timothy H., Bell, James F., Hayward, Thomas L. (2005).\n"
	 << "\"Thermal infrared (8-13 um) spectra of 29 asteroids: the Cornell Mid-Infrared\n"
	 << "Asteroid Spectroscopy (MIDAS) Survey\" Icarus 173:385-408.\n"
	 << "doi:10.1016/j.icarus.2004.08.005."
	 << LogIO::POST;
    }
    temperature_p = 163.0;
    break;
  case FluxCalc_SS_JPL_Butler::Vesta:
    if(report){
      os << LogIO::WARN
	 << "Vesta has a large crater, and its mean emissivity varies from\n"
         << "0.6 in the submm to 0.7 in the mm.  Its mean submm brightness temperature varies\n"
         << "from ~116 to 173K and its rotation period is 5.342h.\n "
         << "CASA does not yet account for these variations."
	 << LogIO::POST;
      os << LogIO::NORMAL1
	 << "Reference for Vesta's mean brightness temperature (160K):\n"
	 << "Chamberlain et al., (2009).\n"
	 << "\"Submillimeter photometry and lightcurves of Ceres and other large asteroids\"\n"
         << "Icarus 202:487-501.\n"
	 << LogIO::POST;
    }
    temperature_p = 160.0;
    break;
  default:
    break;
  };
    
  if(temperature_p > 0.0){
    if(report)
      os << LogIO::NORMAL << "Using blackbody model." << LogIO::POST;
    compute_BB(values, errors, angdiam, mfreqs);
  }
  else{
    os << LogIO::SEVERE
       << "An ephemeris was found, but not a temperature."
       << LogIO::POST;
    success = false;
  }

  if(ephem_temp > 0.0)
    temperature_p = ephem_temp;         // Restore it.

  return success;
}

Bool FluxCalc_SS_JPL_Butler::setObj(const String& objname)
{
  name_p = objname;
  return setObjNum();
}

} //# NAMESPACE CASA - END

