//# NewMSDerivedValues.h: a server for values derived from a MS (e.g. P.A.)
//# Copyright (C) 1997,1999,2000
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
//#
//# $Id$

#if !defined(TRIAL_NEWMSDERIVEDVALUES_H)
#define TRIAL_NEWMSDERIVEDVALUES_H

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MCEpoch.h>
#include <aips/Measures/MCRadialVelocity.h>
#include <aips/Measures/MPosition.h>

//# Forward Declarations
class RONewMSAntennaColumns;
class String;

// <summary>
// NewMSDerivedValues calculates values derived from a MS
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// NewMSDerivedValues calculates values derived from those in a MS
// </etymology>
//
// <synopsis>
// NewMSDerivedValues is a class that computes values derived from those present
// in a MeasurementSet. E.g., calculate feed position angles on the sky from
// time, antenna positions and feed characteristics.
// </synopsis>
//
// <example>
// <srcblock>
// // calculate the parallactic angle and the observatory velocity for the
// // first time and first source in the MS.
// // set up 
// NewMSDerivedValues msd;
// MS myMS("myMS");
// RONewMSColumns msc(myMS);
// msd.setAntennas(msc.antenna());
// MEpoch ep=NewMS::epochMeasure(msc.time());
// ep.set(MVEpoch(Quantity(msc.time()(0),"s")));
// msd.setEpoch(ep);
// MDirection dir=NewMS::directionMeasure(msc.field().phaseDir());
// dir.set(MVDirection(Vector<Double>(msc.field().phaseDir()(0))));
// msd.setFieldCenter(dir);
// msd.setVelocityFrame(MRadialVelocity::LSR);
// // now we are ready for the calculations:
// Double parAngle = msd.parangle();
// MRadialVelocity observatoryVel = msd.obsVel();
// </srcblock>
// </example>
//
// <motivation>
// Values derived from those in a MS are needed in various places, e.g., for
// plotting purposes. This class combines the commonly needed calculations 
// in one place.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="1997/05/30">
//   <li> the interface should be less cumbersome
//   <li> probably needs speeding up
// </todo>

class NewMSDerivedValues
{
public:
  NewMSDerivedValues();
  ~NewMSDerivedValues();

  // Copy constructor, this will initialize with other's MS
  NewMSDerivedValues(const NewMSDerivedValues& other);
  
  // Assignment, this will initialize with other's MS
  NewMSDerivedValues& operator=(const NewMSDerivedValues& other);
  
  // Set antenna position from an antenna table
  // Returns the number of antennas. Also
  // sets the observatory position to the average of the antenna positions.
  Int setAntennas(const RONewMSAntennaColumns& ac);

  // Set antenna positions, index in vector is antenna number
  // for calls below.
  NewMSDerivedValues& setAntennaPositions(const Vector<MPosition>& antPosition);
  
  // Set the observatory position. Note that setAntennas will reset this.
  NewMSDerivedValues& setObservatoryPosition(const MPosition& obsPosition);

  // Set antenna mounts, should have same number of entries as
  // antPosition in setAntennaPosition
  NewMSDerivedValues& setAntennaMount(const Vector<String>& mount);
  
  // Set epoch
  NewMSDerivedValues& setEpoch(const MEpoch& time);

  // Set field center
  NewMSDerivedValues& setFieldCenter(const MDirection& fieldCenter);

  // Set antenna index, sets the position reference for the conversions. 
  // Use -1 to set the reference frame to the observatory position.
  NewMSDerivedValues& setAntenna(Int antenna);

  // Set the velocity frame type (e.g., MRadialVelocity::LSR) 
  NewMSDerivedValues& setVelocityFrame(MRadialVelocity::Types vType);

  // get hour angle
  Double hourAngle();

  // get parallactic angle
  Double parAngle();

  // get azimuth & elevation
  const MDirection& azel();
  
  // get LAST for given time, antenna
  const MEpoch& last();

  // get observatory radial velocity for given epoch, position and direction
  const MRadialVelocity& obsVel();

protected:

private:

  // initialize data
  void init();

  Int antenna_p;
  MEpoch::Convert cUTCToLAST_p;
  Vector<MPosition> mAntPos_p;
  MDirection::Convert cRADecToAzEl_p;
  MDirection::Convert cHADecToAzEl_p;
  MDirection::Convert cRADecToHADec_p;
  MeasFrame fAntFrame_p;
  MDirection mRADecInAzEl_p;
  MDirection mHADecPoleInAzEl_p;
  MDirection mFieldCenter_p;
  MPosition mObsPos_p;
  MRadialVelocity::Convert cTOPOToLSR_p;

  Vector<Int> mount_p;
  //  Vector<Double> receptorAngle_p;
 

};

#endif
