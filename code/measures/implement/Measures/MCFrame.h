//# MCFrame.h: Measure frame calculations proxy
//# Copyright (C) 1996,1997,1998
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

#if !defined(AIPS_MCFRAME_H)
#define AIPS_MCFRAME_H

#if defined(_AIX)
#pragma implementation ("MCFrame.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasFrame.h>

//# Forward Declarations
class MVDirection;
class MVPosition;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
class ostream;
#endif

// <summary>
// Measure frame calculations proxy
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasure" demos="">
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
  // Delete a conversion frame data
  friend void MCFrameDelete(void *dmf);
  
  //# Constructors
  
  //# Operators
  
  //# General member functions
  // Attach an instant of MCFrame to belonging MeasFrame
  static void make(MeasFrame &in);
  // Get TDB in days
  Bool getTDB(Double &tdb);
  // Get the longitude (in rad)
  Bool getLong(Double &tdb);
  // Get the latitude (in rad)
  Bool getLat(Double &tdb);
  // Get the position
  Bool getITRF(MVPosition &tdb);
  // Get the gecentric position (in m)
  Bool getRadius(Double &tdb);
  // Get the LAST (in days)
  Bool getLAST(Double &tdb);
  // Get the LAST (in rad)
  Bool getLASTr(Double &tdb);
  // Get J2000 coordinates (direction cosines)
  Bool getJ2000(MVDirection &tdb);
  // Get B1950 coordinates (direction cosines)
  Bool getB1950(MVDirection &tdb);
  // Get apparent coordinates (direction cosines)
  Bool getApp(MVDirection &tdb);
  // Get LSR radial velocity (m/s)
  Bool getLSR(Double &tdb);
  
private:
  //# Data
  // The belonging frame pointer
  MeasFrame myf;
  // The actual measure conversion values
  // <group>
  // Conversion to TDB time (due to some (for me) unsolvable dependency errors
  // not the proper MeasConvert* here)
  void *epConvTDB;
  // TDB time
  Double *epTDBp;
  // Conversion to LAST time
  void *epConvLAST;
  // LAST time
  Double *epLASTp;
  // Conversion to astronomical longitude/latitude
  void *posConvLong;
  // Longitude
  Vector<Double> *posLongp;
  // Position
  MVPosition *posITRFp;
  // Conversion to J2000
  void *dirConvJ2000;
  // J2000 coordinates
  MVDirection *dirJ2000p;
  // Conversion to B1950
  void *dirConvB1950;
  // B1950 coordinates
  MVDirection *dirB1950p;
  // Conversion to apparent coordinates
  void *dirConvApp;
  // Apparent coordinates
  MVDirection *dirAppp;
  // Conversion to LSR radial velocity
  void *radConvLSR;
  // Radial velocity
  Double *radLSRp;
  // </group>
  
  //# Member functions
  // Constructor
  MCFrame(MeasFrame &inf);
  // Default constructor (not implemented)
  MCFrame();
  // Copy constructor (not implemented)
  MCFrame(const MCFrame &other);
  // Copy assignment (not implemented)
  MCFrame &operator=(const MCFrame &other);
  // Destructor
  ~MCFrame();
  // Create an instance of the MCFrame class
  void create();
  // Reset Epoch value
  void resetEpoch();
  // Reset Position value
  void resetPosition();
  // Reset Direction value
  void resetDirection();
  // Reset RadialVelocity value
  void resetRadialVelocity();
  // Make full Epoch
  void makeEpoch();
  // Make full Position
  void makePosition();
  // Make full Direction
  void makeDirection();
  // Make full RadialVelocity
  void makeRadialVelocity();
};

//# Global functions
// <summary> Global functions </summary>
// <group name=MeasFrameInterface>
// Delete a frame
void MCFrameDelete(void *dmf);
// Get double value for MeasFrame
Bool MCFrameGetdbl(void *dmf, uInt tp, Double &result);
// Get MVDirection value for MeasFrame
Bool MCFrameGetmvdir(void *dmf, uInt tp, MVDirection &result);
// Get MVPosition value for MeasFrame
Bool MCFrameGetmvpos(void *dmf, uInt tp, MVPosition &result);
// </group>

#endif
