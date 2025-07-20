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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MEASURES_MCFRAME_H
#define MEASURES_MCFRAME_H

//# Includes
#include <optional>

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/Measure.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasFrame;
struct MCFrameImplementation;

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
  MCFrame();
  MCFrame(const MCFrame &other);
  MCFrame(MCFrame &&other);

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
  void makeEpoch(MeasFrame& frame);
  // Make full Position
  void makePosition(const MeasFrame& frame);
  // Make full Direction
  void makeDirection(MeasFrame& frame);
  // Make full RadialVelocity
  void makeRadialVelocity(const MeasFrame& frame);
  // Make full Comet
  void makeComet();

  // Get time as Temps Dynamique Barycentrique (TDB, or Barycentric Dynamical Time) in days
  Bool getTDB(Double &tdb, const MeasFrame& frame);
  // Get UT1 in days
  Bool getUT1(Double &tdb, const MeasFrame& frame);
  // Get TT in days
  Bool getTT(Double &tdb, const MeasFrame& frame);
  // Get the longitude (in rad)
  Bool getLong(Double &tdb, const MeasFrame& frame);
  // Get the latitude (ITRF) (in rad)
  Bool getLat(Double &tdb, const MeasFrame& frame);
  // Get the position
  Bool getITRF(MVPosition &tdb, const MeasFrame& frame);
  // Get the geocentric position (in m)
  Bool getRadius(Double &tdb, const MeasFrame& frame);
  // Get the geodetic latitude
  Bool getLatGeo(Double &tdb, const MeasFrame& frame);
  // Get the LAST (in days)
  Bool getLAST(Double &tdb, const MeasFrame& frame);
  // Get the LAST (in rad)
  Bool getLASTr(Double &tdb, const MeasFrame& frame);
  // Get J2000 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getJ2000(MVDirection &tdb, const MeasFrame& frame);
  Bool getJ2000Long(Double &tdb, const MeasFrame& frame);
  Bool getJ2000Lat(Double &tdb, const MeasFrame& frame);
  // </group>
  // Get B1950 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getB1950(MVDirection &tdb, const MeasFrame& frame);
  Bool getB1950Long(Double &tdb, const MeasFrame& frame);
  Bool getB1950Lat(Double &tdb, const MeasFrame& frame);
  // </group>
  // Get apparent coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getApp(MVDirection &tdb, const MeasFrame& frame);
  Bool getAppLong(Double &tdb, const MeasFrame& frame);
  Bool getAppLat(Double &tdb, const MeasFrame& frame);
  // </group>
  // Get LSR radial velocity (m/s)
  Bool getLSR(Double &tdb, const MeasFrame& frame);
  // Get Comet type
  Bool getCometType(uInt &tdb, const MeasFrame& frame);
  // Get Comet position
  Bool getComet(MVPosition &tdb, const MeasFrame& frame);
  
private:
  MCFrame &operator=(const MCFrame &other) = delete;
  
  // pointer-to-implementation (pimpl) pattern is used to avoid a large number of
  // dependencies in the header file.
  std::unique_ptr<MCFrameImplementation> impl_;
};


} //# NAMESPACE CASACORE - END

#endif
