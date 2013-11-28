//# FluxCalcVQS.h: Base class for flux standard calculations taking into account for 
//# time variability 
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
#ifndef COMPONENTS_FLUXCALCVQS_H
#define COMPONENTS_FLUXCALCVQS_H

#include <components/ComponentModels/FluxStandard.h>
#include <components/ComponentModels/FluxStdSrcs.h>
#include <casa/BasicSL/String.h>
#include <casa/OS/Path.h>
#include <measures/Measures/MDirection.h>
#include <tables/Tables/Table.h>

//# Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>

#include <scimath/Functionals/Interpolate1D.h>
#include <map>


namespace casa { //# NAMESPACE CASA - BEGIN

//class Flux;
class MFrequency;
//class Vector;

// <summary> 
// FluxCalcVQS: Base class for flux standard calculations taking account for  
// time variability of the sources. 
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density", "calculator", "variable", and "quasistatic".
// </etymology>
//
// <synopsis>
// The FluxCalcVQS class provides an interface and a small amount of machinery
// for computing total flux densities of calibrators which may be variable in time.
// For the source with significant enough time variability, inteploation in time
// with choice of nearest neighbour, linear, cubic, or cubic spline, is performed. 
// See FluxStdsQS for actual definitions of the standards. The polynomial coefficents
// for the standard include time-variable sources are assumed to be stored in an external
// table.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Provide a base interface for calculating standard flux
// densities, and include any common functions.
// </motivation>

class FluxCalcVQS: public FluxStdSrcs
{
public:

  typedef FluxCalcVQS FCVQS;
  typedef RigidVector<String, 4> RVS4;
  typedef RigidVector<String, 5> RVS5;

  // Source identifiers.
  /****
  enum Source {
    THREEC286 = 0,      // 3C286
    THREEC48,
    THREEC147,
    THREEC138,
    NINETEEN34M638,   // 1934-638
    THREEC295,
    THREEC196,
    THREEC123,
    // The number of standards in this enumerator.
    NUMBER_SOURCES,
    UNKNOWN_SOURCE = NUMBER_SOURCES
  };
  ***/
  virtual ~FluxCalcVQS();

  virtual Bool operator()(Flux<Double>& value, Flux<Double>& error,
                          const MFrequency& mfreq, const Bool updatecoeffs) = 0;
  Bool operator()(Vector<Flux<Double> >& values,
                  Vector<Flux<Double> >& errors,
                  const Vector<MFrequency>& mfreqs);
  
  //for time variable case with interpolation method 
  Bool operator()(Vector<Flux<Double> >& values,
                  Vector<Flux<Double> >& errors,
                  const Vector<MFrequency>& mfreqs, 
                  const MEpoch& mtime,
                  const String& interpmethod);

  // If a FS::Source enum matches srcName, returns the enum.
  // Otherwise, FCQS::UNKNOWN_SOURCE.
  //FCQS::Source srcNameToEnum(const String& srcName) const;

  // Sets srcEnum_p = srcNameToEnum(sourceName), and returns
  // srcEnum_p != FCQS::UNKNOWN_SOURCE
  virtual Bool setSource(const String& sourceName, const MDirection& sourceDir);

  FCVQS::Source getSrcEnum();

  //MDirection getDirection() {return directions_p[srcEnum_p];}
  MDirection getDirection() {return FluxStdSrcs::getDirection(srcEnum_p);}

  // Read the coefficient data table
  void readQSCoeffsTable(const Path& fileName);
  // Interpolate for time variable source
  void interpolate(const String& interpmethod);
  // Set the coefficients from one epoch where i is row number in the original data table  
  void setSourceCoeffsfromVec(uInt& i);
  // Get currently set coefficients
  RigidVector<Vector<Float>,2 >  getCurrentCoeffs() {return tvcoeffs_p;}

  //keep track if it is non-time var source for Perley-Butler2013
  void isTimeVar(Bool istimevar); 

protected:
  FluxCalcVQS();   // Initializes names_p.

private:
  FCVQS::Source srcEnum_p;       // The source identifier.

  // A map from an FS::Source enum to a list of recognized names for it.
  //std::map<FCQS::Source, Vector<String> > names_p;

  // A map from an FS::Source enum to its J2000 direction.
  //std::map<FCQS::Source, MDirection> directions_p;

  // get interpolate method enum
  Interpolate1D<Double,Float>::Method getInterpMethod_p(const String& interpmethod);

  //convert epochs in year.frac to mjds
  void convertYearFracToMjd(const Vector<Double>& yearfrac, Vector<Double>& mjds);

  Vector<Double> epochvec_p;
  Matrix<Float> coeffsmat_p;
  Matrix<Float> coefferrsmat_p;
  Vector<Float> fluxes_p;
  //Vector<Float> tvcoeffs_p;
  RigidVector<Vector<Float>,2> tvcoeffs_p;
  Table Table_p;
  Bool istimevar_p;
  //virtual Bool setCoeffs() = 0;
  //
};

} //# NAMESPACE CASA - END

#endif /* COMPONENTS_FLUXCALCVQS_H */
