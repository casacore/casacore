//# MeasureHolder.cc: A holder for Measures to enable record conversions
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
#include <trial/Measures/MeasureHolder.h>
#include <aips/Exceptions.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/String.h>
#include <aips/Glish.h>

//# Constructors
MeasureHolder::MeasureHolder() : hold(0) {};

MeasureHolder::MeasureHolder(const Measure &in) : hold(0) {
  hold = in.clone();
}

MeasureHolder::MeasureHolder(const MeasureHolder &other) : hold(0) {
  if (other.hold) hold = other.hold->clone();
}

//# Destructor
MeasureHolder::~MeasureHolder() {
  delete hold;
  hold = 0;
}

//# Operators
MeasureHolder &MeasureHolder::operator=(const MeasureHolder &other) {
  if (this != &other) {
    delete hold;
    hold = 0;
    if (other.hold) hold = other.hold->clone();
  };
  return *this;
}

const Measure &MeasureHolder::operator()() const {
  return *hold;
}

//# Member Functions
Bool MeasureHolder::isMeasure() const {
  return ToBool(hold);
}

Bool MeasureHolder::isMDirection() const {
  return ToBool(hold && hold->type() == Register((MDirection *)0));
}

Bool MeasureHolder::isMDoppler() const {
  return ToBool(hold && hold->type() == Register((MDoppler *)0));
}

Bool MeasureHolder::isMEpoch() const {
  return ToBool(hold && hold->type() == Register((MEpoch *)0));
}

Bool MeasureHolder::isMFrequency() const {
  return ToBool(hold && hold->type() == Register((MFrequency *)0));
}

Bool MeasureHolder::isMPosition() const {
  return ToBool(hold && hold->type() == Register((MPosition *)0));
}

Bool MeasureHolder::isMRadialVelocity() const {
  return ToBool(hold && hold->type() == Register((MRadialVelocity *)0));
}

const Measure &MeasureHolder::asMeasure() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMDirection"));
  };
  return *hold;
}

const MDirection &MeasureHolder::asMDirection() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMDirection"));
  };
  MDirection::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MDirection *y = (MDirection *) (void *) hold;
  return *y;
}

const MDoppler &MeasureHolder::asMDoppler() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMDoppler"));
  };
  MDoppler::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MDoppler *y = (MDoppler *) (void *) hold;
  return *y;
}

const MEpoch &MeasureHolder::asMEpoch() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMEpoch"));
  };
  MEpoch::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MEpoch *y = (MEpoch *) (void *) hold;
  return *y;
}

const MFrequency &MeasureHolder::asMFrequency() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMFrequency"));
  };
  MFrequency::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MFrequency *y = (MFrequency *) (void *) hold;
  return *y;
}

const MPosition &MeasureHolder::asMPosition() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMPosition"));
  };
  MPosition::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MPosition *y = (MPosition *) (void *) hold;
  return *y;
}

const MRadialVelocity &MeasureHolder::asMRadialVelocity() const {
  if (!hold) {
    throw(AipsError("Empty MeasureHolder argument for asMRadialVelocity"));
  };
  MRadialVelocity::assert(*hold);
  /// Next lines to get rid of errors and warnings (awaiting dynamic_cast)
  MRadialVelocity *y = (MRadialVelocity *) (void *) hold;
  return *y;
}

