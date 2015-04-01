//# UVWMachine.cc: Converts UVW coordinates between coordinate systems
//# Copyright (C) 1998,1999,2001
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

//# Includes
#include <casacore/measures/Measures/UVWMachine.h>
#include <casacore/casa/Quanta/Euler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
UVWMachine::UVWMachine(const MDirection::Ref &out, const MDirection &in,
		       Bool EW, Bool project) 
  : ew_p(EW), proj_p(project), zp_p(True), nop_p(False), in_p(in) {
    outref_p = out;
    out_p = MDirection(outref_p);
    planetinit();
    conv_p = MDirection::Convert(in_p, outref_p);
    outin_p = conv_p();
    out_p = outin_p;
    init();
}

UVWMachine::UVWMachine(const MDirection &out, const MDirection &in,
		       Bool EW, Bool project)
  : ew_p(EW), proj_p(project), zp_p(False), nop_p(False),
    in_p(in), out_p(out) {
    outref_p = out.getRef();
    planetinit();
    conv_p = MDirection::Convert(in_p, outref_p);
    outin_p = conv_p();
    init();
  }

UVWMachine::UVWMachine(const MDirection::Ref &out, const MDirection &in,
		       const MeasFrame &frame, Bool EW, Bool project)
  : ew_p(EW), proj_p(project), zp_p(True), nop_p(False), in_p(in) {
    outref_p = out;
    out_p = MDirection(outref_p);
    outref_p.set(frame);
    planetinit();
    conv_p = MDirection::Convert(in_p, outref_p);
    outin_p = conv_p();
    out_p = outin_p;
    init();
  }
  
UVWMachine::UVWMachine(const MDirection &out, const MDirection &in, 
		       const MeasFrame &frame, Bool EW, Bool project)
  : ew_p(EW), proj_p(project), zp_p(False), nop_p(False),
    in_p(in), out_p(out) {
    outref_p = out.getRef();
    outref_p.set(frame);
    planetinit();
    conv_p = MDirection::Convert(in_p, outref_p);
    outin_p = conv_p();
    init();
  }

UVWMachine::UVWMachine(const UVWMachine &other) {
  copy(other);
  init();
}

UVWMachine &UVWMachine::operator=(const UVWMachine &other) {
  if (this != &other) {
    copy(other);
    init();
  }
  return *this;
}

//# Destructor
UVWMachine::~UVWMachine() {}

//# Operators
MVPosition UVWMachine::operator()(const MVPosition &uv) const {
  if (nop_p) return uv;
  return uv * uvproj_p;
}

Vector<MVPosition>
UVWMachine::operator()(const Vector<MVPosition> &uv) const {
  if (nop_p) return uv;
  Vector<MVPosition> tmp(uv.nelements());
  for (uInt i=0; i<uv.nelements(); i++) {
    tmp(i) = uv(i) * uvproj_p;
  }
  return tmp;
}

Vector<Double> UVWMachine::operator()(const Vector<Double> &uv) const {
  if (nop_p) return uv;
  return (MVPosition(uv) * uvproj_p).getValue();
}

Vector<Vector<Double> >
UVWMachine::operator()(const Vector<Vector<Double> > &uv) const {
  if (nop_p) return uv;
  Vector<Vector<Double> > tmp(uv.nelements());
  for (uInt i=0; i<uv.nelements(); i++) {
    tmp(i) = (MVPosition(uv(i)) * uvproj_p).getValue();
  }
  return tmp;
}

//# Member functions
void UVWMachine::reCalculate() {
  init();
}

const MDirection &UVWMachine::phaseCenter() const {
  return out_p;
}

const RotMatrix &UVWMachine::rotationUVW() const {
  return uvproj_p;
}

const MVPosition &UVWMachine::rotationPhase() const {
  return phrot_p;
}

void UVWMachine::convertUVW(Vector<Double> &uv) const {
  if (!nop_p) uv = (MVPosition(uv) * uvproj_p).getValue();
}

void UVWMachine::convertUVW(Vector<Vector<Double> > &uv) const {
  if (!nop_p) {
    for (uInt i=0; i<uv.nelements(); i++) {
      uv(i) = (MVPosition(uv(i)) * uvproj_p).getValue();
    }
  }
}

void UVWMachine::convertUVW(MVPosition &uv) const {
  if (!nop_p) uv *= uvproj_p;
}

void UVWMachine::convertUVW(Vector<MVPosition > &uv) const {
  if (!nop_p) {
    for (uInt i=0; i<uv.nelements(); i++) {
      uv(i) *= uvproj_p;
    }
  }
}

void UVWMachine::convertUVW(Double &phase, Vector<Double> &uv) const {
  phase = 0;
  if (!nop_p) {
    MVPosition tmp(uv);
    tmp *= uvrot_p;
    phase = phrot_p * tmp;
    if (proj_p) tmp *= rot4_p;
    uv = tmp.getValue();
  }
}

void UVWMachine::convertUVW(Vector<Double> &phase,
			    Vector<Vector<Double> > &uv) const {
  phase = 0;
  if (!nop_p) {
    MVPosition tmp;
    phase.resize(uv.nelements());
    for (uInt i=0; i<uv.nelements(); i++) {
      tmp = MVPosition(uv(i));
      tmp *= uvrot_p;
      phase(i) = phrot_p * tmp;
      if (proj_p) tmp *= rot4_p;
      uv(i) = tmp.getValue();
    }
  }
}

void UVWMachine::convertUVW(Double &phase, MVPosition &uv) const {
  phase = 0;
  if (!nop_p) {
    uv *= uvrot_p;
    phase = phrot_p * uv;
    if (proj_p) uv *= rot4_p;
  }
}

void UVWMachine::convertUVW(Vector<Double> &phase,
			    Vector<MVPosition> &uv) const {
  phase.resize(uv.nelements());
  phase = 0;
  if (!nop_p) {
    for (uInt i=0; i<uv.nelements(); i++) {
      uv(i) *= uvrot_p;
      phase(i) = phrot_p * uv(i);
      if (proj_p) uv(i) *= rot4_p;
    }
  }
}

Double UVWMachine::getPhase(Vector<Double> &uv) const {
  Double phase;
  convertUVW(phase, uv);
  return phase;
}

Vector<Double> UVWMachine::getPhase(Vector<Vector<Double> > &uv) const {
  Vector<Double> phase(uv.nelements());
  convertUVW(phase, uv);
  return phase;
}

Double UVWMachine::getPhase(MVPosition &uv) const {
  Double phase;
  convertUVW(phase, uv);
  return phase;
}

Vector<Double> UVWMachine::getPhase(Vector<MVPosition > &uv) const {
  Vector<Double> phase(uv.nelements());
  convertUVW(phase, uv);
  return phase;
}

//# Private member functions
void UVWMachine::init() {
  // Initialise the rotation matrices for uvw and phase conversion
  // Define axes
  static const MVDirection mVz(0.,0.,1.);
  static const MVDirection mVy(0.,1.,0.);
  static const MVDirection mVx(1.,0.,0.);
  if (!nop_p) {
    // Define rotation to a coordinate system with pole towards in-direction
    // and X-axis W; by rotating around z-axis over -(90-long); and around
    // x-axis (lat-90).
    rot1_p = RotMatrix(Euler(-(C::pi_2 - in_p.getValue().get()(0)), 3,
			     in_p.getValue().get()(1) - C::pi_2, 1));
    // Convert the input axes directions to the output reference frame, and
    // deduce a rotation matrix from these
    rot2_p.set(conv_p(mVx).getValue().getValue(),
	       conv_p(mVy).getValue().getValue(),
	       conv_p(mVz).getValue().getValue());
    rot2_p.transpose();
    // The rotation matrix from a system that has a pole towards output
    // direction, into the standard system.
    rot3_p = RotMatrix(Euler(C::pi_2 - out_p.getValue().get()(1), 1,
			     -(out_p.getValue().get()(0) - C::pi_2), 3));
    // Get the rotation matrix which re-projects an uv-plane onto another
    // reference direction:
    // <ul>
    //   <li> around x-axis (out-lat - 90)
    //   <li> around z-axis (out-long - in-long)
    //   <li> around x-axis (90 - in-lat)
    // </ul>
    // and normalise
    rot4_p = RotMatrix();
    if (proj_p) {
      RotMatrix x(Euler(-(C::pi_2 - out_p.getValue().get()(1)), 1,
			out_p.getValue().get()(0) -
			in_p.getValue().get()(0), 3,
			(C::pi_2 - in_p.getValue().get()(1)), 1));
      rot4_p(0,0) = x(1,1)/x(2,2);
      rot4_p(1,1) = x(0,0)/x(2,2);
      rot4_p(0,1) = x(1,0)/x(2,2);
      rot4_p(1,0) = x(0,1)/x(2,2);
    }
    // Complete rotation matrix for uvw change
    uvrot_p = rot3_p * rot2_p * rot1_p;
    uvrot_p.transpose();
    // Complete rotation matrix for uvw change including a re-projection
    uvproj_p = uvrot_p * rot4_p;
    // Phase change vector from input to output coordinates. 
    phrot_p = rot3_p * (MVPosition(out_p.getValue())
			- MVPosition(outin_p.getValue()));
    nop_p = conv_p.isNOP() && !proj_p && zp_p;
  }
}
  
void UVWMachine::planetinit() {
  if ((outref_p.getType() & MDirection::EXTRA)) {  // out planet
    out_p.set(outref_p);		// make sure frame set
    MDirection::Ref ref(MDirection::J2000, in_p.getRef().getFrame());
    out_p = MDirection::Convert(out_p, ref)();
  }
  if ((in_p.getRef().getType() & MDirection::EXTRA)) {  // in planet
    MDirection::Ref ref(MDirection::J2000, outref_p.getFrame());
    in_p = MDirection::Convert(in_p, ref)();
  }
}

void UVWMachine::copy(const UVWMachine &other) {
  ew_p = other.ew_p;
  proj_p = other.proj_p;
  zp_p = other.zp_p;
  nop_p = other.nop_p;
  in_p = other.in_p;
  outref_p = other.outref_p;
  conv_p = other.conv_p;
  out_p = other.out_p;
  outin_p = other.outin_p;
  rot1_p = other.rot1_p;
  rot2_p = other.rot2_p;
  rot3_p = other.rot3_p;
  rot4_p = other.rot4_p;
}

} //# NAMESPACE CASACORE - END

