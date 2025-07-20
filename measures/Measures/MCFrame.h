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
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/measures/Measures/MeasFrame.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MDirection;
template<class M> class MeasConvert;
class MEpoch;
class MVDirection;
class MVPosition;
class MPosition;
class MRadialVelocity;

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

template <typename T>
class cloned_ptr {
 public:
  cloned_ptr() noexcept {}
  cloned_ptr(std::nullptr_t) noexcept {}
  cloned_ptr(T* object) noexcept : _ptr(object) {}
  cloned_ptr(const cloned_ptr<T>& other)
      : _ptr(other._ptr == nullptr ? nullptr : new T(*other._ptr)) {}
  cloned_ptr(cloned_ptr<T>&& other) noexcept : _ptr(std::move(other._ptr)) {}

  cloned_ptr<T>& operator=(std::nullptr_t) noexcept { _ptr.reset(); return *this; }
  cloned_ptr<T>& operator=(const cloned_ptr<T>& other) {
    _ptr.reset(other._ptr == nullptr ? nullptr : new T(*other._ptr));
    return *this;
  }
  cloned_ptr<T>& operator=(cloned_ptr<T>&& other) noexcept {
    _ptr = std::move(other._ptr);
    return *this;
  }
  void reset() noexcept { _ptr.reset(); }
  void reset(T* object) noexcept { _ptr.reset(object); }

  operator bool() const { return _ptr != nullptr; }
  T& operator*() const noexcept { return *_ptr; }
  T* operator->() const noexcept { return _ptr.get(); }
  T* get() const { return _ptr.get(); }

  bool operator==(std::nullptr_t) const noexcept { return _ptr == nullptr; }
  bool operator==(const std::unique_ptr<T>& rhs) const noexcept {
    return _ptr == rhs;
  }
  bool operator==(const cloned_ptr<T>& rhs) const noexcept {
    return _ptr == rhs._ptr;
  }
  void swap(cloned_ptr<T>& other) noexcept { std::swap(_ptr, other._ptr); }

 private:
  std::unique_ptr<T> _ptr;
};

class MCFrame {

public:
  
  //# Friends
  
  //# Constructors
  // Construct using the MeasFrame parent
  MCFrame() = default;
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
  void makeEpoch(const MeasFrame& myf);
  // Make full Position
  void makePosition(const MeasFrame& myf);
  // Make full Direction
  void makeDirection(const MeasFrame& myf);
  // Make full RadialVelocity
  void makeRadialVelocity(const MeasFrame& myf);
  // Make full Comet
  void makeComet();

  // Get TDB in days
  Bool getTDB(Double &tdb, const MeasFrame& myf);
  // Get UT1 in days
  Bool getUT1(Double &tdb, const MeasFrame& myf);
  // Get TT in days
  Bool getTT(Double &tdb, const MeasFrame& myf);
  // Get the longitude (in rad)
  Bool getLong(Double &tdb, const MeasFrame& myf);
  // Get the latitude (ITRF) (in rad)
  Bool getLat(Double &tdb, const MeasFrame& myf);
  // Get the position
  Bool getITRF(MVPosition &tdb, const MeasFrame& myf);
  // Get the geocentric position (in m)
  Bool getRadius(Double &tdb, const MeasFrame& myf);
  // Get the geodetic latitude
  Bool getLatGeo(Double &tdb, const MeasFrame& myf);
  // Get the LAST (in days)
  Bool getLAST(Double &tdb, const MeasFrame& myf);
  // Get the LAST (in rad)
  Bool getLASTr(Double &tdb, const MeasFrame& myf);
  // Get J2000 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getJ2000(MVDirection &tdb, const MeasFrame& myf);
  Bool getJ2000Long(Double &tdb, const MeasFrame& myf);
  Bool getJ2000Lat(Double &tdb, const MeasFrame& myf);
  // </group>
  // Get B1950 coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getB1950(MVDirection &tdb, const MeasFrame& myf);
  Bool getB1950Long(Double &tdb, const MeasFrame& myf);
  Bool getB1950Lat(Double &tdb, const MeasFrame& myf);
  // </group>
  // Get apparent coordinates (direction cosines) and long/lat (rad)
  // <group>
  Bool getApp(MVDirection &tdb, const MeasFrame& myf);
  Bool getAppLong(Double &tdb, const MeasFrame& myf);
  Bool getAppLat(Double &tdb, const MeasFrame& myf);
  // </group>
  // Get LSR radial velocity (m/s)
  Bool getLSR(Double &tdb, const MeasFrame& myf);
  // Get Comet type
  Bool getCometType(uInt &tdb, const MeasFrame& myf);
  // Get Comet position
  Bool getComet(MVPosition &tdb, const MeasFrame& myf);
  
private:
  //# Data
  // The belonging frame pointer
  // MeasFrame myf;
  // The actual measure conversion values
  // <group>
  // Conversion to TDB time (due to some (for me) unsolvable dependency
  // errors)
  // not the proper MeasConvert* here)
  cloned_ptr<MeasConvert<MEpoch>> epConvTDB;
  // TDB time
  std::optional<Double> epTDBp;
  // Conversion to UT1 time
  cloned_ptr<MeasConvert<MEpoch>> epConvUT1;
  // UT1 time
  std::optional<Double> epUT1p;
  // Conversion to TT time
  cloned_ptr<MeasConvert<MEpoch>> epConvTT;
  // TT time
  std::optional<Double> epTTp;
  // Conversion to LAST time
  cloned_ptr<MeasConvert<MEpoch>> epConvLAST;
  // LAST time
  std::optional<Double> epLASTp;
  // Conversion to ITRF longitude/latitude
  cloned_ptr<MeasConvert<MPosition>> posConvLong;
  // Longitude
  Vector<Double> posLongp;
  // Position
  cloned_ptr<MVPosition> posITRFp;
  // Conversion to geodetic longitude/latitude
  cloned_ptr<MeasConvert<MPosition>> posConvLongGeo;
  // Latitude
  Vector<Double> posLongGeop;
  // Position
  cloned_ptr<MVPosition> posGeop;
  // Conversion to J2000
  cloned_ptr<MeasConvert<MDirection>> dirConvJ2000;
  // Longitude
  Vector<Double> j2000Longp;
  // J2000 coordinates
  cloned_ptr<MVDirection> dirJ2000p;
  // Conversion to B1950
  cloned_ptr<MeasConvert<MDirection>> dirConvB1950;
  // Longitude
  Vector<Double> b1950Longp;
  // B1950 coordinates
  cloned_ptr<MVDirection> dirB1950p;
  // Conversion to apparent coordinates
  cloned_ptr<MeasConvert<MDirection>> dirConvApp;
  // Longitude
  Vector<Double> appLongp;
  // Apparent coordinates
  cloned_ptr<MVDirection> dirAppp;
  // Conversion to LSR radial velocity
  cloned_ptr<MeasConvert<MRadialVelocity>> radConvLSR;
  // Radial velocity
  std::optional<Double> radLSRp;
  // </group>
  
  MCFrame &operator=(const MCFrame &other) = delete;
};


} //# NAMESPACE CASACORE - END

#endif
