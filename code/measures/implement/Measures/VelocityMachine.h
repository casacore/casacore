//# VelocityMachine.h: Calculates between velocities and frequencies
//# Copyright (C) 1998
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

#if !defined(AIPS_VELOCITYMACHINE_H)
#define AIPS_VELOCITYMACHINE_H

//# Includes
#include <aips/aips.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MCFrequency.h>
#include <aips/Measures/MCDoppler.h>
#include <aips/Measures/MeasConvert.h>
#ifdef __GNUG__
typedef MeasConvert<MFrequency,MVFrequency,MCFrequency> gpp_velmachine_bug1;
typedef MeasConvert<MDoppler,MVDoppler,MCDoppler> gpp_velmachine_bug2;
#endif

//# Forward Declarations
class MeasFrame;
template <class T> class Vector;

// <summary> Calculates between velocities and frequencies </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tVelocityMachine.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MFrequency>MFrequency</linkto> class
//   <li> <linkto class=MDoppler>MDoppler</linkto> class
// </prerequisite>
//
// <etymology>
// From Velocity and machinery
// </etymology>
//
// <synopsis>
// The construction of a VelocityMachine class object creates a machine that can
// calculate the velocity from a frequency, or the other way around.
//
// A machine is constructed using the following information:
// <ul>
//   <li> Reference for frequencies. It should contain at least the reference
//	code (e.g. MFrequency::LSR), and can optionally contain a MeasFrame,
//	which is used to convert between reference frames (e.g. LSR
//	to/from TOPO), but wich can be given separately in the machine
//	constructor. The reference could contain an offset. In that case all
//	input frequencies are considered to be relative to this offset; all
//	output frequencies will have this offset removed.
//   <li> Preferred 'frequency' units (e.g. GHz, or cm). These units are used
//	to output a frequency, or if an input frequency is e.g. given as a
//	simple double.
//   <li> Reference for velocity (e.g. MDoppler::OPTICAL, note
// 	that MDoppler::BETA is the 'true' velocity).
//	The reference could contain an offset. In that case all
//	input velocities are considered to be relative to this offset; all
//	output velocities will have this offset removed.
//   <li> Preferred velocity units (e.g. AU/a). These units are used
//	to output a velocity, or if an input velocity is e.g. given as a
//	simple double.
//   <li> The rest frequency to be used for converting between frequency and
//	velocity. It is given as an MVFrequency.
// </ul>
// The machine can be given additional information:
// <ul>
//   <li> A reference code for the velocity (given as a frequency reference
// 	code (e.g. MFrequency::TOPO)). If given, the velocities will be
//	assumed to be in this frame on input, or will be converted to it on
//	output. If not given the reference frame of the velocity is assumed
//	to be the same as that for the frequency.
//   <li> A frame (MeasFrame). This frame will be used in any conversion
//	between reference frames. If not given explicitly here, it will be
//	tacitly assumed that if a frame is necessary, it has been specified
//	in the frequency reference.
// </ul>
// Once the machine has been set up, operator() can be used to convert
// between velocities and frequencies if the input argument (e.g. an
// MVFrequency) can be deduced. In other cases makeFrequency() or
// makeVelocity() should be used (e.g. if the argument is a Double).
// </synopsis>
//
// <example>
// <srcblock>
//	// Define a time/position frame
//	MEpoch epo(MVEpoch(MVTime(98,5,16,0.5).day()));
//	MPosition pos;
//	MeasTable::Observatory(pos, "ATCA");
//	MeasFrame frame(epo, pos);
//	// Note that e.g. the time in the frame can be changed later
//	// Specify the frequency reference
//	MFrequency::Ref fr(MFrequency::LSR);
//	// Specify the velocity reference
//	MDoppler::Ref vr(MDoppler::OPT);
//	// Specify the default units
//	Unit fu("eV");
//	Unit vu("AU/a");
//	// Get the rest frequency
//	MVFrequency rfrq(QC::HI);
//	// Set up a machine (no conversion of reference frame)
//	VelocityMachine exec(fr, fu, rfrq, vr, vu, frame);
//	// or as (with conversion of reference frame it could have been)
//	// VelocityMachine exec(fr, fu, rfrq, vr, vu, MFrequency::TOPO, frame);
//	// Given a current observational frequency of 5.87432837e-06 eV
//	// its velocity will be (in AU/yr)
//	cout << "Velocity: " << exec.makeVelocity(5.87432837e-06) << endl;
//	// Introducing an offset
//	MFrequency foff(MVFrequency(Quantity(5.87432837e-06, "eV")),
//		        MFrequency::LSR);
//	// and setting it in the reference, and regenerating machine:
//	fr.set(foff);
//	exec.set(fr);
//	// the following will give the same result:
//	cout << "Velocity: " << exec.makeVelocity(0.0) << endl;
//	
// </srcblock>
// See the test program for more examples
// </example>
//
// <motivation>
// To aid in converting series of frequencies and velocities
// </motivation>
//
// <todo asof="1998/06/02">
//   <li> Nothing I know of
// </todo>

class VelocityMachine {
public:
  //# Constructors
  // Construct a machine from the input values (no frame conversion, implicit
  // frame if necessary)
  VelocityMachine(const MFrequency::Ref &frqref, const Unit &frqun,
		  const MVFrequency &rest,
		  const MDoppler::Ref &velref, const Unit &velun);
  // Construct a machine from the input values (no frame conversion, explicit
  // frame)
  VelocityMachine(const MFrequency::Ref &frqref, const Unit &frqun,
		  const MVFrequency &rest,
		  const MDoppler::Ref &velref, const Unit &velun,
		  const MeasFrame &frame);
  // Construct a machine from the input values (frame conversion, implicit
  // frame if any)
  VelocityMachine(const MFrequency::Ref &frqref, const Unit &frqun,
		  const MVFrequency &rest,
		  const MFrequency::Types &velframe,
		  const MDoppler::Ref &velref, const Unit &velun);
  // Construct a machine from the input values (frame conversion, explicit
  // frame)
  VelocityMachine(const MFrequency::Ref &frqref, const Unit &frqun,
		  const MVFrequency &rest,
		  const MFrequency::Types &velframe,
		  const MDoppler::Ref &velref, const Unit &velun,
		  const MeasFrame &frame);
  // Copy constructor (copy semantics)
  VelocityMachine(const VelocityMachine &other);
  // Copy assignments (copy semantics)
  VelocityMachine &operator=(const VelocityMachine &other);

  //# Destructor
  ~VelocityMachine();

  //# Operators
  // Return velocity if frequency given, and vv
  // <group>
  const Quantum<Double> &operator()(const MVFrequency &in);
  const Quantum<Double> &operator()(const MVDoppler &in);
  const Quantum<Double> &operator()(const Quantum<Double> &in);
  const Quantum<Double> &makeVelocity(Double in);
  const Quantum<Double> &makeFrequency(Double in);
  const Quantum<Vector<Double> > &makeVelocity(const Vector<Double> &in);
  const Quantum<Vector<Double> > &makeFrequency(const Vector<Double> &in);
  // </group>

  //# Member functions
  // Set or reset part of the machine
  // <group>
  void set(const MFrequency::Ref &in);
  void set(const Unit &in);
  void set(const MVFrequency &in);
  void set(const MFrequency::Types &in);
  void set(const MDoppler::Ref &in);
  void set(const MeasFrame &in);
  // </group>
  // Recalculate the machinery
  void reCalculate();

private:

  //# Constructors
  // Construct an empty machine (not implemented)
  VelocityMachine();

  //# Data
  // Frequency reference
  MFrequency::Ref fref_p;
  // Frequency units
  // <group>
  Unit fun_p;
  // </group>
  // Rest frequency
  MVFrequency rest_p;
  // Velocity frame
  MFrequency::Types vfm_p;
  // Velocity reference
  MDoppler::Ref vref_p;
  // Velocity units
  // <group>
  Unit vun_p;
  Double vfac_p;
  // </group>
  // Frequency conversion forward
  MFrequency::Convert cvfv_p;
  // Frequency conversion backward
  MFrequency::Convert cvvf_p;
  // Velocity conversion forward
  MDoppler::Convert cvvo_p;
  // Velocity conversion backward
  MDoppler::Convert cvov_p;
  // Result
  // <group>
  Quantum<Double> resv_p;
  Quantum<Double> resf_p;
  Quantum<Vector<Double> > vresv_p;
  Quantum<Vector<Double> > vresf_p;
  // </group>

  //# Private Member Functions
  // Initialise machinery
  void init();
  // Copy data members
  void copy(const VelocityMachine &other);
};

#endif
