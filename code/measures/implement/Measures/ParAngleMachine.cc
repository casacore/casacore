//# ParAngleMachine.cc: Converts a direction into parallactic angle
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

//# Includes
#include <trial/Measures/ParAngleMachine.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/Unit.h>

//# Constructors
ParAngleMachine::ParAngleMachine() :
  indir_p(0), convdir_p(0), frame_p(0),
  convpole_p(0), mvdir_p() {}

ParAngleMachine::ParAngleMachine(const MDirection &in) :
  indir_p(new MDirection(in)), convdir_p(0), frame_p(0),
  convpole_p(0), mvdir_p() {}

ParAngleMachine::ParAngleMachine(const ParAngleMachine &other) :
  indir_p(0), convdir_p(0), frame_p(0),
  convpole_p(0), mvdir_p() {
  if (other.indir_p) indir_p = new MDirection(*other.indir_p);
  if (other.frame_p) frame_p = new MeasFrame(*other.frame_p);
}

ParAngleMachine &ParAngleMachine::operator=(const ParAngleMachine &other) {
  if (this != &other) {
    delete indir_p; indir_p = 0;
    delete convdir_p; convdir_p = 0;
    delete frame_p; frame_p = 0;
    delete convpole_p; convpole_p = 0;
    if (other.indir_p) indir_p = new MDirection(*other.indir_p);
    if (other.frame_p) frame_p = new MeasFrame(*other.frame_p);
  };
  return *this;
}

ParAngleMachine::~ParAngleMachine() {
  delete indir_p; indir_p = 0;
  delete convdir_p; convdir_p = 0;
  delete frame_p; frame_p = 0;
  delete convpole_p; convpole_p = 0;
}

//# Operators
Double ParAngleMachine::posAngle(const Quantum<Double> &ep) const {
  frame_p->resetEpoch(ep);
  if (indir_p->isModel()) mvdir_p = (*convdir_p)().getValue();
  return mvdir_p.positionAngle((*convpole_p)().getValue()); 
}

Vector<Double>
ParAngleMachine::posAngle(const Quantum<Vector<Double> > &ep) const {
  uInt nel(ep.getValue().nelements());
  Vector<Double> res(nel);
  for (uInt i=0; i<nel; ++i) res[i] = posAngle(ep.getValue()[i]);
  return res;
}

Double ParAngleMachine::posAngle(const Double &ep) const {
  frame_p->resetEpoch(ep);
  if (indir_p->isModel()) mvdir_p = (*convdir_p)().getValue();
  return mvdir_p.positionAngle((*convpole_p)().getValue()); 
}

Vector<Double>
ParAngleMachine::posAngle(const Vector<Double> &ep) const {
  uInt nel(ep.nelements());
  Vector<Double> res(nel);
  for (uInt i=0; i<nel; ++i) res[i] = posAngle(ep[i]);
  return res;
}

Quantum<Double> ParAngleMachine::operator()(const Quantum<Double> &ep) const {
  static const Unit un("deg");
  return Quantity(posAngle(ep), un);
}

Quantum<Double> ParAngleMachine::operator()(const MVEpoch &ep) const {
  return operator()(ep.getTime());
}

Quantum<Double> ParAngleMachine::operator()(const MEpoch &ep) const {
  static const Unit un("s");
  return operator()(ep.get(un));
}

Quantum<Vector<Double> >
ParAngleMachine::operator()(const Quantum<Vector<Double> > &ep) const {
  static const Unit un("deg");
  return Quantum<Vector<Double> >(posAngle(ep), un);
}

Quantum<Vector<Double> >
ParAngleMachine::operator()(const Vector<MVEpoch> &ep) const {
  static const Unit un("deg");
  uInt nel(ep.nelements());
  Vector<Double> res(nel);
  for (uInt i=0; i<nel; ++i) res[i] = posAngle(ep[i].getTime());
  return Quantum<Vector<Double> >(res, un);
}

Double
ParAngleMachine::operator()(const Double &ep) const {
  return posAngle(ep);
}

Vector<Double>
ParAngleMachine::operator()(const Vector<Double> &ep) const {
  uInt nel(ep.nelements());
  Vector<Double> res(nel);
  for (uInt i=0; i<nel; ++i) res[i] = posAngle(ep[i]);
  return res;
}

Quantum<Vector<Double> >
ParAngleMachine::operator()(const Vector<MEpoch> &ep) const {
  static const Unit un("deg");
  static const Unit uns("s");
  uInt nel(ep.nelements());
  Vector<Double> res(nel);
  for (uInt i=0; i<nel; ++i) res[i] = posAngle(ep[i].get(uns));
  return Quantum<Vector<Double> >(res, un);
}

//# Member functions
void ParAngleMachine::set(const MDirection &in) {
  delete indir_p; indir_p = 0;
  delete convdir_p; convdir_p = 0;
  delete convpole_p; convpole_p = 0;
  indir_p = new MDirection(in);
}

void ParAngleMachine::set(const MeasFrame &frame) {
  delete convdir_p; convdir_p = 0;
  delete frame_p; frame_p = 0;
  delete convpole_p; convpole_p = 0;
  frame_p = new MeasFrame(frame);
}
