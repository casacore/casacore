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
#if !defined(AIPS_PARANGLEMACHINE_H)
#define AIPS_PARANGLEMACHINE_H

//# Includes
#include <aips/aips.h>
#include <aips/Measures.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MCEpoch.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Arrays/Vector.h>

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
// This input direction determines the coordinate system (moving directions
// will be assumed to be J2000) for which the parallactic (<em>vertical</em>)
// angle will be calculated. I.e. the angle between the vertical in the
// local coordinate system (<em>Az, El</em>) through the given direction and
// the pole of the input coordinate system.
//
// The machinery also needs a <linkto class=MeasFrame>MeasFrame</linkto>,
// with a position on Earth (unless input system is <em>HA, Dec</em>), and,
// in general, a reference time. If no reference time is given, the first
// conversion time will be used.
//
// The actual calculation of the parallactic angles is done by the
// <src>operator()</src> accepting a time or a list of times.
//
// To make the process as fast as possible, the machinery will calculate the
// direction in the coordinate system for the centre of the Earth. It will
// be assumed that that direction is constant (except for planets) for
// a period of about an hour (the actual period can be chosen). In most cases,
// unless you have a grazing passing by the Sun, this will have ample
// precision, and means that the minimum amount of direction conversion has
// to be done.
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
  // Return parallactic angles
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

private:

  //# Data
  // Input direction
  MDirection *indir_p;
  // Conversion engine
  MDirection::Convert *convdir_p;
  // Measure frame
  MeasFrame *frame_p;
  // Convert pole
  MDirection::Convert *convpole_p;
  // Intermediate conversion result
  mutable MVDirection mvdir_p;

  //# Constructors

  //# Private Member Functions
  // Get position angle
  // <group>
  Double posAngle(const Quantum<Double> &ep) const;
  Vector<Double> posAngle(const Quantum<Vector<Double> > &ep) const;
  Double posAngle(const Double &ep) const;
  Vector<Double> posAngle(const Vector<Double> &ep) const;
  // </group>
  // Initialise machinery
  void init();
  // Planet handling
  void planetinit();
  // Copy data members
  void copy(const ParAngleMachine &other);
};

#endif
