//# EarthMagneticMachine.cc: Calculates magnetic field in a direction
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
#include <trial/Measures/EarthMagneticMachine.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MCPosition.h>
#include <aips/Measures/MCEpoch.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/MeasConvert.h>

//# Constructors
EarthMagneticMachine::EarthMagneticMachine() :
  fex_p(False), pex_p(False), fil_p(0), inx_p(False), clx_p(False) {
    init();
}

EarthMagneticMachine::EarthMagneticMachine(const MDirection::Ref &in,
					   const Quantum<Double> &hgt,
					   MeasFrame &frame) :
  fex_p(False), pex_p(False), fil_p(0), inx_p(False), clx_p(False) {
    inref_p = in;
    inref_p.set(frame);
    hgt_p = hgt.getValue("m");
    MCFrame::make(inref_p.getFrame());
    if (!frame.getITRF(pos_p)) {
      throw(AipsError("No position in frame for EarthMagneticMachine"));
    };
    if (!frame.getTDB(epo_p)) {
      throw(AipsError("No epoch in frame for EarthMagneticMachine"));
    };
    fil_p = 15;
    init();
  }

EarthMagneticMachine::EarthMagneticMachine(const MDirection::Ref &in,
					   const Quantum<Double> &hgt,
					   const MPosition &pos,
					   const MEpoch &tm) :
  fex_p(False), pex_p(False), fil_p(0), inx_p(False), clx_p(False) {
    inref_p = in;
    hgt_p = hgt.getValue("m");
    pos_p = MPosition::Convert(pos, MPosition::ITRF)().getValue();
    epo_p = MEpoch::Convert(tm, MEpoch::TDB)().getValue().get();
    fil_p = 15;
    init();
  }

EarthMagneticMachine::EarthMagneticMachine(const EarthMagneticMachine &other) {
  copy(other);
  init();
}

EarthMagneticMachine 
&EarthMagneticMachine::operator=(const EarthMagneticMachine &other) {
  if (this != &other) {
    copy(other);
    init();
  };
  return *this;
}

//# Destructor
EarthMagneticMachine::~EarthMagneticMachine() {}

//# Operators
Double EarthMagneticMachine::operator()() {
  return getLOSField();
}

Quantum<Double> EarthMagneticMachine::operator()(const Unit &un) {
  return getLOSField(un);
}

Double EarthMagneticMachine::operator()(const MVDirection &in) {
  return getLOSField(in);
}

Quantum<Double> EarthMagneticMachine::operator()(const MVDirection &in,
						 const Unit &un) {
  return getLOSField(in, un);
}

//# Member functions

void EarthMagneticMachine::reCalculate() {
  init();
}

void EarthMagneticMachine::set(const MDirection::Ref &in) {
  inref_p = in;
  fil_p |= 1;
  init();
}

void EarthMagneticMachine::set(const Quantum<Double> &hgt) {
  hgt_p = hgt.getValue("m");
  fil_p |= 2;
  init();
}

void EarthMagneticMachine::set(MeasFrame &frame) {
  if (fil_p & 1) inref_p.set(frame);
  if (frame.getITRF(pos_p)) fil_p |= 4;
  if (frame.getTDB(epo_p))  fil_p |= 8;
  init();
}

void EarthMagneticMachine::set(const MPosition &pos) {
  pos_p = MPosition::Convert(pos, MPosition::ITRF)().getValue();
  fil_p |= 4;
  init();
}

void EarthMagneticMachine::set(const MEpoch &tm) {
  epo_p = MEpoch::Convert(tm, MEpoch::TDB)().getValue().get();
  fil_p |= 8;
  init();
}

Double EarthMagneticMachine::getLOSField() {
  if (!clx_p) {
    throw(AipsError("No value calculated for EarthMagneticMachine"));
  };
  if (!fex_p) {
    fex_p = True;
    los_p = fld_p * in_p;
  };
  return los_p;
}

Double EarthMagneticMachine::getLOSField(const MVDirection &in) {
  calculate(in);
  return getLOSField();;
}

Quantum<Double> EarthMagneticMachine::getLOSField(const Unit &un) {
  return Quantum<Double>(getLOSField(), "nT").get(un);
}

Quantum<Double> EarthMagneticMachine::getLOSField(const MVDirection &in,
						  const Unit &un) {
  calculate(in);
  return getLOSField(un);
}

const MVEarthMagnetic &EarthMagneticMachine::getField() {
  if (!clx_p) {
    throw(AipsError("No value calculated for EarthMagneticMachine"));
  };
  return fld_p;
}

const MVEarthMagnetic &EarthMagneticMachine::getField(const MVDirection &in) {
  calculate(in);
  return getField();
}

Double EarthMagneticMachine::getLong() {
  if (!clx_p) {
    throw(AipsError("No value calculated for EarthMagneticMachine"));
  };
  if (!pex_p) {
    pex_p = True;
    pl_p = sub_p.get();
  };
  return pl_p(1);
}

Double EarthMagneticMachine::getLong(const MVDirection &in) {
  calculate(in);
  return getLong();
}

Quantum<Double> EarthMagneticMachine::getLong(const Unit &un) {
  return Quantum<Double>(getLong(), "rad").get(un);
}

Quantum<Double> EarthMagneticMachine::getLong(const MVDirection &in,
					      const Unit &un) {
  calculate(in);
  return getLong(un);
}

const MVPosition &EarthMagneticMachine::getPosition() {
  if (!clx_p) {
    throw(AipsError("No value calculated for EarthMagneticMachine"));
  };
  return sub_p;
}

const MVPosition &EarthMagneticMachine::getPosition(const MVDirection &in) {
  calculate(in);
  return getPosition();
}

Bool EarthMagneticMachine::calculate(const MVDirection &in) {
  if (!inx_p) return False;
  in_p = in;
  in_p.adjust();
  in_p = conv_p(in_p).getValue();
  // Angle between direction and Earth radius
  Double an = pos_p * in_p;
  Double x = sqrt(abs(an*an + subl_p));
  x = min(abs(-an + x), abs(-an - x));
  sub_p = pos_p + (x*in_p);
  fld_p = fldc_p(sub_p);
  pex_p = False;
  fex_p = False;
  clx_p = True;
  return clx_p;
}

//# Private member functions
void EarthMagneticMachine::init() {
  if (fil_p == 15) {
    // Initialise the direction conversion engine
    conv_p = MDirection::Convert(inref_p, MDirection::ITRF);
    // Distance of observer to Earth centre
    posl_p = pos_p.radius();
    // Squared difference between posl_p and distance to sub-point
    subl_p = hgt_p*(hgt_p + 2*posl_p);
    // Field calculator
    fldc_p = EarthField(EarthField::STANDARD, epo_p);
    inx_p = True;
  };
  pex_p = False;
  fex_p = False;
  clx_p = False;
}

void EarthMagneticMachine::copy(const EarthMagneticMachine &other) {
  inref_p = other.inref_p;
  hgt_p = other.hgt_p;
  pos_p = other.pos_p;
  epo_p = other.epo_p;
  conv_p = other.conv_p;
  fil_p = other.fil_p;
  inx_p = False;
}
