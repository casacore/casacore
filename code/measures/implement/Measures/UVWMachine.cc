//# UVWMachine.cc: Converts UVW coordinates between coordinate systems
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

//# Includes
#include <trial/Measures/UVWMachine.h>
#include <aips/Measures/Euler.h>

//# Constructors
UVWMachine::UVWMachine(const MDirection::Ref &out, const MDirection &in,
		       Bool EW=False) 
  : ew_p(EW), zp_p(True), in_p(in) {
    outref_p = out;
    conv_p = MDirection::Convert(in, outref_p);
    outin_p = conv_p();
    out_p = outin_p;
    init();
}

UVWMachine::UVWMachine(const MDirection &out, const MDirection &in,
	     Bool EW=False)
  : ew_p(EW), zp_p(False), in_p(in), out_p(out) {
    outref_p = out.getRef();
    conv_p = MDirection::Convert(in, outref_p);
    outin_p = conv_p();
    init();
  }

UVWMachine::UVWMachine(const MDirection::Ref &out, const MDirection &in,
		       const MeasFrame &frame, Bool EW=False)
  : ew_p(EW), zp_p(True), in_p(in) {
    outref_p = out;
    outref_p.set(frame);
    conv_p = MDirection::Convert(in, outref_p);
    outin_p = conv_p();
    out_p = outin_p;
    init();
  }
  
UVWMachine::UVWMachine(const MDirection &out, const MDirection &in, 
		       const MeasFrame &frame, Bool EW=False)
  : ew_p(EW), zp_p(False), in_p(in), out_p(out) {
    outref_p = out.getRef();
    outref_p.set(frame);
    conv_p = MDirection::Convert(in, outref_p);
    outin_p = conv_p();
    init();
  }

UVWMachine::  UVWMachine(const UVWMachine &other) {
  copy(other);
  init();
}

UVWMachine &UVWMachine::operator=(const UVWMachine &other) {
  if (this != &other) {
    copy(other);
    init();
  };
  return *this;
}

//# Destructor
UVWMachine::~UVWMachine() {}

//# Operators
MVPosition UVWMachine::operator()(const MVPosition &uv) const {
  return uv * uvrot_p;
}

Vector<MVPosition>
UVWMachine::operator()(const Vector<MVPosition> &uv) const {
  Vector<MVPosition> tmp(uv.nelements());
  for (Int i=0; i<uv.nelements(); i++) {
    tmp(i) = uv(i) * uvrot_p;
  };
  return tmp;
}

Vector<Double> UVWMachine::operator()(const Vector<Double> &uv) const {
  return (MVPosition(uv) * uvrot_p).getValue();
}

Vector<Vector<Double> >
UVWMachine::operator()(const Vector<Vector<Double> > &uv) const {
  Vector<Vector<Double> > tmp(uv.nelements());
  for (Int i=0; i<uv.nelements(); i++) {
    tmp(i) = (MVPosition(uv(i)) * uvrot_p).getValue();
  };
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
  return uvrot_p;
}

const MVPosition &UVWMachine::rotationPhase() const {
  return phrot_p;
}

void UVWMachine::convertUVW(Double &phase, MVPosition &uv) const {
  uv *= uvrot_p;
  phase = phrot_p * uv;
}

//# Private member functions
void UVWMachine::init() {
  const MVDirection mVz(0.,0.,1.);
  const MVDirection mVy(0.,1.,0.);
  const MVDirection mVx(1.,0.,0.);
  rot1_p = RotMatrix(Euler(C::pi_2 - in_p.getValue().get()(0), 3,
			   in_p.getValue().get()(1) - C::pi_2, 1));
  rot2_p.set(conv_p(mVx).getValue().getValue(),
	     conv_p(mVy).getValue().getValue(),
	     conv_p(mVz).getValue().getValue());
  rot2_p.transpose();
  rot3_p = RotMatrix(Euler(C::pi_2 - out_p.getValue().get()(1), 1,
			   out_p.getValue().get()(0) - C::pi_2, 3));
  uvrot_p = rot3_p * rot2_p * rot1_p;
  uvrot_p.transpose();
  phrot_p = rot3_p * (MVPosition(out_p.getValue())
  		      - MVPosition(outin_p.getValue()));
}

void UVWMachine::copy(const UVWMachine &other) {
  ew_p = other.ew_p;
  zp_p = other.zp_p;
  in_p = other.in_p;
  outref_p = other.outref_p;
  conv_p = other.conv_p;
  out_p = other.out_p;
  outin_p = other.outin_p;
  rot1_p = other.rot1_p;
  rot2_p = other.rot2_p;
  rot3_p = other.rot3_p;
}
