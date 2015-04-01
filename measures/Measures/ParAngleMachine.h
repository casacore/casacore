//# ParAngleMachine.h: Converts a direction into parallactic angle
//# Copyright (C) 2001,2002
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
#ifndef MEASURES_PARANGLEMACHINE_H
#define MEASURES_PARANGLEMACHINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasFrame;

// <summary> Converts a direction into parallactic angle
// </summary>

// <use visibility=export>

// <reviewed reviewer="Mark Wieringa" date="2001/10/06" tests="tParAngleMachine.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MDirection>MDirection</linkto> class
// </prerequisite>
//
// <etymology>
// From Parallactic Angle and machinery
// </etymology>
//
// <synopsis>
// The construction of a ParAngleMachine class object creates a machine that
// can create parallactic angles from a series of time epochs given.
//
// The machinery needs an input 
// <linkto class=MDirection>MDirection</linkto>  to specify the input 
// coordinates reference direction and coordinate system.
// The parallactic (<em>vertical</em>)
// angle will be calculated as the angle between the vertical in the
// local coordinate system (<em>Az, El</em>) through the given direction and
// the pole of the <em>J2000</em> coordinate system.
// <note role=tip> To calculate the parallactic angle for another 
// coordinate system pole, add the <src>positionAngle</src> between the
// <em>J2000</em> system and the pole in the other coordinate system. </note>
//
// The machinery also needs a <linkto class=MeasFrame>MeasFrame</linkto>,
// with a position on Earth and
// a reference epoch. The reference time is necessary to have an epoch type.
//
// The actual calculation of the parallactic angles is done by the
// <src>operator()</src> accepting a time or a list of times in various
// formats.
//
// The machine calculates the paralaactic angle for the first time given to
// the machine. For subsequent times that are within a check interval,
// the angle is calculated assuming that only the hour angle changes within
// that interval. For moving objects the test interval is always forced
// to zero. Tests show that the machine with a zero interval is about
// 8 times faster than using brute force. Having an interval of an
// hour improves that by another factor of 4.
// <note role=tip> If the parallactic angles for a series of directions have
// to be calculated, it is best to have separate machines for each such
// <em>field</em>. </note>
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// To speed up parallactic angle calculations
// </motivation>
//
// <todo asof="2001/09/15">
//   <li>
// </todo>

class ParAngleMachine {
 public:
  //# Constructors
  // Create an empty machine. It can only be used after appropriate 'set'
  // methods
  ParAngleMachine();
  // Construct for the specified direction
  ParAngleMachine(const MDirection &in);
  // Copy constructor (deep copy)
  ParAngleMachine(const ParAngleMachine &other);
  // Copy assignments (deep copy)
  ParAngleMachine &operator=(const ParAngleMachine &other);

  //# Destructor
  ~ParAngleMachine();

  //# Operators
  // Return parallactic angles (epoch in days if given as Double)
  // <thrown>
  // <li> AipsError if no frame or a frame without an Epoch (for type) or       
  //    Position.
  // </thrown>
  // <group>
  Quantum<Vector<Double> >
    operator()(const Quantum<Vector<Double> > &ep) const;
  Quantum<Vector<Double> > operator()(const Vector<MVEpoch> &ep) const;
  Quantum<Vector<Double> > operator()(const Vector<MEpoch> &ep) const;
  Quantum<Double> operator()(const Quantum<Double> &ep) const;
  Quantum<Double> operator()(const MVEpoch &ep) const;
  Quantum<Double> operator()(const MEpoch &ep) const;
  Double operator()(const Double &ep) const;
  Vector<Double> operator()(const Vector<Double> &ep) const;
  // </group>

  //# Member functions
  // Will have a group of set methods (in direction; reference time; a frame;
  // a reference time valid period
  // <group>
  void set(const MDirection &in);
  void set(const MeasFrame &frame);
  // </group>
  // Set the test interval (in days) over which to use simple formula
  void setInterval(const Double ttime);

private:

  //# Data
  // Input direction
  MDirection *indir_p;
  // Conversion engine
  mutable MDirection::Convert *convdir_p;
  // Measure frame
  MeasFrame *frame_p;
  // Converted zenith
  mutable MVDirection zenith_p;
  // Intermediate conversion result
  mutable MVDirection mvdir_p;
  // Time of last full solution (in days)
  mutable Double lastep_p;
  // Default time interval over which to do simple solution (days)
  mutable Double defintvl_p;
  // Time interval over which to do simple solution (days)
  mutable Double intvl_p;
  // Calculation cache
  // <group>
  mutable Double UTfactor_p;
  mutable Double longoff_p;
  mutable Double longdiff_p;
  mutable Double slat1_p;
  mutable Double clat1_p;
  mutable Double slat2_p;
  mutable Double clat2_p;
  // </group>

  //# Constructors

  //# Private Member Functions
  // Get position angle (Epoch is supposed to be in days if Double)
  // <thrown>
  // <li> AipsError if no frame or a frame without an Epoch (for type) or
  // 	Position.
  // </thrown>
  // <group>
  Double posAngle(const Quantum<Double> &ep) const;
  Vector<Double> posAngle(const Quantum<Vector<Double> > &ep) const;
  Double posAngle(const Double &ep) const;
  Vector<Double> posAngle(const Vector<Double> &ep) const;
  // </group>
  // Initialise machinery
  void init();
  // Initialise conversion
  void initConv() const;
  // Calculate position angle
  Double calcAngle(const Double ep) const;
};


} //# NAMESPACE CASACORE - END

#endif
