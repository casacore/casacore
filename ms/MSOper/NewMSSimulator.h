//# NewMSSimulator.h: this defines the MeasurementSet Simulator
//# Copyright (C) 1995-2009
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

#ifndef MS_NEWMSSIMULATOR_H
#define MS_NEWMSSIMULATOR_H


//# Includes
#include <memory>
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MDirection.h>

#include <casacore/tables/DataMan/TiledDataStManAccessor.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;

// <category lib=aips module="ModuleName">
// <summary> Create an empty MeasurementSet from observation and telescope descriptions. </summary>
// <reviewed reviewer="" date="" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
//  <li> MeasurementSet
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

class NewMSSimulator
{
public: 
  
  // Constructor from name only
  NewMSSimulator(const String&);

  // Constructor from existing MS
  NewMSSimulator(MeasurementSet&);

  // Copy constructor - for completeness only
  NewMSSimulator(const NewMSSimulator & mss);
  
  //# Destructor
  ~NewMSSimulator();
  
//# Operators
  // Assignment
  NewMSSimulator & operator=(const NewMSSimulator &);
  
  // Set maximum amount of data (bytes) to be written into any one
  // scratch column hypercube
  void setMaxData(const double maxData=2e9) {maxData_p=maxData;}
  
  // set the antenna and array data. These are written immediately to the
  // existing MS. The same model is used for the other init infor.
  void initAnt(const String& telname,
	       const Vector<double>& x, 
	       const Vector<double>& y, 
	       const Vector<double>& z,
	       const Vector<double>& dishDiameter,
	       const Vector<double>& offset,
	       const Vector<String>& mount,
	       const Vector<String>& name,
	       const Vector<String>& padname,
	       const String& coordsystem,
	       const MPosition& mRefLocation);
  // get the info back 
  bool getAnt(String& telescope, int32_t& nAnt, Matrix<double>* antXYZ, 
	      Vector<double>& antDiam, Vector<double>& offset,
	      Vector<String>& mount, Vector<String>& name, Vector<String>& padname,
	      String& coordsystem, MPosition& mRefLocation );

  // set the observed fields
  void initFields(const String& sourceName, 
		  const MDirection& sourceDirection,
		  const String& calCode);

  bool getFields(int32_t& nField,
		 Vector<String>& sourceName, 
		 Vector<MDirection>& sourceDirection,
		 Vector<String>& calCode);

  // set the Feeds;  brain dead version
  void initFeeds(const String& mode);

  bool getFeedMode(String& mode);

  // set the Feeds;  Smart version
  void initFeeds(const String& mode,
		 const Vector<double>& x,
		 const Vector<double>& y,
		 const Vector<String>& pol);

  // set the spectral windows information
  void initSpWindows(const String& spWindowName,
		     const int32_t& nChan,
		     const Quantity& startFreq,
		     const Quantity& freqInc,
		     const Quantity& freqRes,
		     const MFrequency::Types& freqType,
		     const String& stokesString);

  bool getSpWindows(int32_t& nSpw,
		    Vector<String>& spWindowName,
		    Vector<int32_t>& nChan,
		    Vector<Quantity>& startFreq,
		    Vector<Quantity>& freqInc,
		    Vector<String>& stokesString);

  void setFractionBlockageLimit(const double fraclimit) 
    { fractionBlockageLimit_p = fraclimit; }

  void setElevationLimit(const Quantity& ellimit) 
    { elevationLimit_p = ellimit; }

  void setAutoCorrelationWt(const float autocorrwt) 
    { autoCorrelationWt_p = autocorrwt; }

  void settimes(const Quantity& qIntegrationTime, 
		const bool      useHourAngles,
		const MEpoch&   mRefTime);

  void observe(const String& sourceName,
	       const String& spWindowName,
	       const Quantity& qStartTime, 
	       const Quantity& qStopTime,
               const bool add_observation=true,
//# from int ASDM2MSFiller::addUniqueState(
//# defaults for ALMA as known on 20100831
               const bool state_sig=true,
               const bool state_ref=true,
               const double& state_cal=0.,
               const double& state_load=0.,
               const unsigned int state_sub_scan=1,
               const String& state_obs_mode="OBSERVE_TARGET.ON_SOURCE",
               const String& observername="CASA simulator",
               const String& projectname="CASA simulation");


  void observe(const Vector<String>& sourceNames,
	       const String& spWindowName,
	       const Vector<Quantity>& qStartTimes, 
	       const Vector<Quantity>& qStopTimes,
	       const Vector<MDirection>& directions,
               const bool add_observation=true,
//# from int ASDM2MSFiller::addUniqueState(
//# defaults for ALMA as known on 20100831
               const bool state_sig=true,
               const bool state_ref=true,
               const double& state_cal=0.,
               const double& state_load=0.,
               const unsigned int state_sub_scan=1,
               const String& state_obs_mode="OBSERVE_TARGET.ON_SOURCE",
               const String& observername="CASA simulator",
               const String& projectname="CASA simulation");

  casacore::CountedPtr<MeasurementSet> getMs () const;


private:

  // Prevent use of default constructor
  NewMSSimulator() {}

//# Data Members
  double fractionBlockageLimit_p;
  Quantity elevationLimit_p;
  float autoCorrelationWt_p;
  String telescope_p;
  Quantity qIntegrationTime_p;
  bool useHourAngle_p;
  bool hourAngleDefined_p;
  MEpoch mRefTime_p;
  double t_offset_p;
  double dataWritten_p;
  int32_t hyperCubeID_p;
  bool hasHyperCubes_p;
  int32_t lastSpWID_p;
  int32_t lastNchan_p;

  casacore::CountedPtr<MeasurementSet> ms_p;

  TiledDataStManAccessor dataAcc_p, scratchDataAcc_p, sigmaAcc_p, flagAcc_p;

  double maxData_p;

  void local2global(Vector<double>& xReturned,
		    Vector<double>& yReturned,
		    Vector<double>& zReturned,
		    const MPosition& mRefLocation,
		    const Vector<double>& xIn,
		    const Vector<double>& yIn,
		    const Vector<double>& zIn);

  void longlat2global(Vector<double>& xReturned,
		      Vector<double>& yReturned,
		      Vector<double>& zReturned,
		      const MPosition& mRefLocation,
		      const Vector<double>& xIn,
		      const Vector<double>& yIn,
		      const Vector<double>& zIn);

  // Returns the fractional blockage of one antenna by another
  // We will want to put this somewhere else eventually, but I don't yet know where!
  // Till then.
  // fraction1: fraction of antenna 1 that is blocked by 2
  // fraction2: fraction of antenna 2 that is blocked by 1
  // hint: at least one of the two will be 0.0
  void  blockage(double &fraction1, double &fraction2,
		 const Vector<double>& uvw,             // uvw in same units as diam!
		 const double diam1, const double diam2);

  String formatDirection(const MDirection&);
  String formatTime(const double);

  void addHyperCubes(const int32_t id, const int32_t nBase, const int32_t nChan, const int32_t nCorr);

  void defaults();

  bool calcAntUVW(MEpoch& epoch, MDirection& refdir, 
			Matrix<double>& uvwAnt);


};


} //# NAMESPACE CASACORE - END

#endif




