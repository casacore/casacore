//# MCFrame.h: Measure frame calculations proxy
//# Copyright (C) 1996-2003,2007
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

#ifndef MEASURES_MCFRAME_H
#define MEASURES_MCFRAME_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/measures/Measures/MeasFrame.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MVDirection;
class MVPosition;

// <summary>
// Measure frame calculations proxy
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class=Measure>Measure</linkto> class
// <li> <linkto class=MeasFrame>MeasFrame</linkto> class
// </prerequisite>
//
// <etymology>
// From Measure and Frame
// </etymology>
//
// <synopsis>
// The <linkto class=MeasFrame>MeasFrame</linkto> class contains the 'when
// and where' of an observed Measure. Calculations to get the appropiate
// value (e.g. the Earth's longitude) from this frame for conversions are
// done in this class, together with all the caching of (intermediate) results
// that can speed-up calculations.<br>
// The MCFrame class is used by the individual measure conversion classes
// (see <linkto class=MCBase>MCBase</linkto> class).<br>
// </synopsis>
//
// <example>
// <srcblock>
//	MEpoch my_epoch(Quantity(MeasData::MJDB1950,"d")); // an epoch
//	MCFrame frame(my_epoch);	// used in a frame
//	frame.set(obser);		// add observatory (an MPosition)
//	MEpoch::Convert conv(my_epoch, MEPoch::Ref(MEpoch::LAST, frame));
// </srcblock>
// The <em>conv</em> conversion engine will (transpararently) use the MCFrame
// class in calls from MCEpoch (the time conversions), which will be called
// by the MEpoch::Convert () operator.
// </example>
//
// <motivation>
// To separate the frame calculations from the Measure containers, to enable
// e.g. Tables to have Measures.
// </motivation>
//
// <todo asof="1997/04/17">
// </todo>

class MCFrame {

public:
  
  //# Friends
  
  //# Constructors
  // Construct using the MeasFrame parent
  MCFrame(MeasFrame &inf);

  // Destructor
  ~MCFrame();
  
  //# Operators
  
  //# General member functions
  // Reset Epoch value
  void resetEpoch();
  // Reset Position value
  void resetPosition();
  // Reset Direction value
  void resetDirection();
  // Reset RadialVelocity value
  void resetRadialVelocity();
  // Reset Comet
  void resetComet();
  // Make full Epoch
  void makeEpoch();
  // Make full Position
  void makePosition();
  // Make full Direction
  void makeDirection();
  // Make full RadialVelocity
  void makeRadialVelocity();
  // Make full Comet
  void makeComet();

  // Get TDB in days
  Bool getTDB(Double &tdb);
  // Get UT1 in days
  Bool getUT1(Double &tdb);
  // Get TT in days
  Bool getTT(Double &tdb);
  // Get the longitude (in rad)
  Bool getLong(Double &tdb);
  // Get the latitude (ITRF) (in rad)
  Bool getLat(Double &tdb);
  // Get the position
  Bool getITRF(MVPosition &tdb);
  // Get the geocentric position (in m)
  Bool getRadius(Double &tdb);
  // Get the geodetic latitude
  Bool getLatGeo(Double &tdb);
  // Get the LAST (in days)
  Bool getLAST(Double &tdb);
  // Get the LAST (in rad)
  Bool getLASTr(Double &tdb);
  // Get J2000 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getJ2000(MVDirection &tdb);
  Bool getJ2000Long(Double &tdb);
  Bool getJ2000Lat(Double &tdb);
  // </group>
  // Get B1950 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getB1950(MVDirection &tdb);
  Bool getB1950Long(Double &tdb);
  Bool getB1950Lat(Double &tdb);
  // </group>
  // Get apparent coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getApp(MVDirection &tdb);
  Bool getAppLong(Double &tdb);
  Bool getAppLat(Double &tdb);
  // </group>
  // Get LSR radial velocity (m/s)
  Bool getLSR(Double &tdb);
  // Get Comet type
  Bool getCometType(uInt &tdb);
  // Get Comet position
  Bool getComet(MVPosition &tdb);
  
private:
  //# Data
  // The belonging frame pointer
  MeasFrame myf;
  // The actual measure conversion values
  // <group>
  // Conversion to TDB time (due to some (for me) unsolvable dependency
  // errors)
  // not the proper MeasConvert* here)
  void *epConvTDB;
  // TDB time
  Double *epTDBp;
  // Conversion to UT1 time
  void *epConvUT1;
  // UT1 time
  Double *epUT1p;
  // Conversion to TT time
  void *epConvTT;
  // TT time
  Double *epTTp;
  // Conversion to LAST time
  void *epConvLAST;
  // LAST time
  Double *epLASTp;
  // Conversion to ITRF longitude/latitude
  void *posConvLong;
  // Longitude
  Vector<Double> *posLongp;
  // Position
  MVPosition *posITRFp;
  // Conversion to geodetic longitude/latitude
  void *posConvLongGeo;
  // Latitude
  Vector<Double> *posLongGeop;
  // Position
  MVPosition *posGeop;
  // Conversion to J2000
  void *dirConvJ2000;
  // Longitude
  Vector<Double> *j2000Longp;
  // J2000 coordinates
  MVDirection *dirJ2000p;
  // Conversion to B1950
  void *dirConvB1950;
  // Longitude
  Vector<Double> *b1950Longp;
  // B1950 coordinates
  MVDirection *dirB1950p;
  // Conversion to apparent coordinates
  void *dirConvApp;
  // Longitude
  Vector<Double> *appLongp;
  // Apparent coordinates
  MVDirection *dirAppp;
  // Conversion to LSR radial velocity
  void *radConvLSR;
  // Radial velocity
  Double *radLSRp;
  // </group>
  
  //# Member functions
  // Default constructor (not implemented)
  MCFrame();
  // Copy constructor (not implemented)
  MCFrame(const MCFrame &other);
  // Copy assignment (not implemented)
  MCFrame &operator=(const MCFrame &other);
};


} //# NAMESPACE CASACORE - END

#endif
