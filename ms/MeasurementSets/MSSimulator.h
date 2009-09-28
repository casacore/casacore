//# MSSimulator.h: this defines the MeasurementSet Simulator
//# Copyright (C) 1995,1996,1998,1999,2000,2002
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

#ifndef MS_MSSIMULATOR_H
#define MS_MSSIMULATOR_H


//# Includes
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/BasicSL/Complex.h>
#include <casa/Quanta/Quantum.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MEpoch.h>
namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class MeasurementSet;
class MDirection;

// <category lib=aips module="ModuleName">
// <summary> Create an empty MeasurementSet from observation and telescope descriptions. </summary>
// <reviewed reviewer="" date="" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> MeasurementSet
// </prerequisite>
//
// <etymology>
// MS is from MeasurementSet, Simulator refers to the generation of
// 'fake' data from a set of parameters for instrument and sources.
// </etymology>
//
// <synopsis> 
// This class creates a MeasurementSet from a set of parameters for instrument
// and sources. It does not simulate the data, only the coordinates of a 
// measurement. The application "simulator" uses this class to create a true
// simulated MS with perfect or corrupted data.
// </synopsis> 
//
// <motivation>
// To test calibration and imaging programs it is necessary to have flawless
// data and data with errors that are known exactly. This class generates
// empty MeasurementSets (only coordinates filled in) that can be filled 
// with predicted data.
// </motivation>
//
// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// <li>     The amount of information to be specified by the user
//          could be much larger. For the moment it has been restricted to
//          what is needed for testing the synthesis imaging code. Already
//          it is possible to create MeasurementSets that cannot be processed
//          yet.
// </todo>

class MSSimulator
{
public: 
  
  // Default constructor - use this, then invoke initAnt, initFeed, initSpWindow,
  // initConfig.
  MSSimulator();
  
  // Copy constructor - for completeness only
  MSSimulator(const MSSimulator & mss);
  
  //# Destructor
  ~MSSimulator();
  
//# Operators
  // Assignment
  MSSimulator & operator=(const MSSimulator &);
  
  // set the antenna and array data.
  // This is held in MSSimulator vectors, and is then written into
  // the appropriate MS Tables when writeMS() and its helper function
  // extendMS() are called.   The same model is used for the Fields,
  // SpWindows, Feeds, and Times information below.
  void initAnt(const String& telname,
	       const Vector<Double>& x, 
	       const Vector<Double>& y, 
	       const Vector<Double>& z,
	       const Vector<Float>& dishDiameter,
	       const Vector<String>& mount,
	       const Vector<String>& name,
	       const String& coordsystem,
	       const MPosition& mRefLocation);

  // set the observed fields
  void initFields(const uInt nSources,
		  const Vector<String>& sourceName, 
		  const Vector<MDirection>& sourceDirection,
		  const Vector<Int>& intsPerPointing,
		  const Vector<Int>& mosPointingsX,
		  const Vector<Int>& mosPointingsY,
		  const Vector<Float>& mosSpacing);


  // set the Feeds;  brain dead version
  void initFeeds(const String& mode);

  // set the spectral windows information
  void initSpWindows(const uInt nSpWindows,
		     const Vector<String>& spWindowName,
		     const Vector<Int>& nChan,
		     const Vector<Quantity>& startFreq,
		     const Vector<Quantity>& freqInc,
		     const Vector<Quantity>& freqRes,
		     const Vector<String>& stokesString);

  void setFractionBlockageLimit(const Double fraclimit) 
    { fractionBlockageLimit_p = fraclimit; }

  void setElevationLimit(const Quantity& ellimit) 
    { elevationLimit_p = ellimit; }

  void setAutoCorrelationWt(const Float autocorrwt) 
    { autoCorrelationWt_p = autocorrwt; }

  void setTimes(const Quantity& qIntegrationTime, 
		const Quantity& qGapTime, 
		const Bool      useHourAngles,
		const Quantity& qStartTime, 
		const Quantity& qStopTime, 
		const MEpoch&   qRefTime );

  // Write out a simulated MeasurementSet with the given name
  void writeMS(const String& msname); 
  void extendMS(MeasurementSet & ms);

  
private:

//# Data Members
  Int nSources_p, nSpWindows_p, nAnt_p, nFeed_p, nTimes_p;
  Double Tstart_p, Tend_p, Tint_p, Tgap_p;
  Quantity  qIntegrationTime_p, qGapTime_p, qStartTime_p, qStopTime_p;
  MEpoch mRefTime_p;
  Bool useHourAngles_p;

  Vector<Double> mosSpacing_p, startFreq_p, freqInc_p, freqRes_p,
      arrayXYZ_p, antDiam_p;
  Vector<Int> nChan_p, nIntFld_p, nIntSpW_p, nCorr_p, antId_p, feedAntId_p,
      feedId_p, feedSpWId_p, feedBeamId_p, feedNumRec_p;
  Matrix<Int> nMos_p,stokesTypes_p;
  Cube<Double> beamOffset_p;
  Cube<Complex> polResp_p;
  Vector<String> srcName_p, antName_p, mountType_p, spWindowName_p;
  Matrix<String> feedPol_p;
  Matrix<Double> radec_p, antXYZ_p, feedXYZ_p, feedAngle_p;
  MPosition refPosition_p;
  String  radecRefFrame_p, telescope_p;
  Double  fractionBlockageLimit_p;
  Quantity elevationLimit_p;

  Float autoCorrelationWt_p;

//# Constructors

//# Secure Member Functions
  
  void local2global(Vector<Double>& xReturned,
		    Vector<Double>& yReturned,
		    Vector<Double>& zReturned,
		    const MPosition& mRefLocation,
		    const Vector<Double>& xIn,
		    const Vector<Double>& yIn,
		    const Vector<Double>& zIn);

  void longlat2global(Vector<Double>& xReturned,
		      Vector<Double>& yReturned,
		      Vector<Double>& zReturned,
		      const MPosition& mRefLocation,
		      const Vector<Double>& xIn,
		      const Vector<Double>& yIn,
		      const Vector<Double>& zIn);

  // Returns the fractional blockage of one antenna by another
  // We will want to put this somewhere else eventually, but I don't yet know where!
  // Till then.
  // fraction1: fraction of antenna 1 that is blocked by 2
  // fraction2: fraction of antenna 2 that is blocked by 1
  // hint: at least one of the two will be 0.0
  void  blockage(Double &fraction1, Double &fraction2,
		 const Vector<Double>& uvw,             // uvw in same units as diam!
		 const Double diam1, const Double diam2);

};


} //# NAMESPACE CASA - END

#endif




