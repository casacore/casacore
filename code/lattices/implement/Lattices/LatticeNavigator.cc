//# LatticeNavigator.cc: an abstract base class to steer lattice iterators
//# Copyright (C) 1994,1995,1996,1997
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

#include <trial/Lattices/LatticeNavigator.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Exceptions/Error.h>

LatticeNavigator::~LatticeNavigator() {
    // Nothing
};

Bool LatticeNavigator::operator++() {
  return operator++(0);
};

Bool LatticeNavigator::operator--() {
  return operator--(0);
};

IPosition LatticeNavigator::relativePosition() const {
  return position();
};

IPosition LatticeNavigator::relativeEndPosition() const {
  return endPosition();
};

IPosition LatticeNavigator::subLatticeShape() const {
  return latticeShape();
};

void LatticeNavigator::subSection(const IPosition & blc,
				  const IPosition & trc) {
  subSection(blc, trc, IPosition(latticeShape().nelements(),1));
};

void LatticeNavigator::subSection(const IPosition & blc,
				  const IPosition & trc, 
				  const IPosition & inc){
  throw(AipsError("LatticeNavigator::subSection(blc, trc, inc)"
	" - sub-Lattice's are not supported"));
};

IPosition LatticeNavigator::blc() const {
  return IPosition(latticeShape().nelements(),0);
};

IPosition LatticeNavigator::trc() const {
  return latticeShape() - 1;
};

IPosition LatticeNavigator::increment() const {
  return IPosition(latticeShape().nelements(),1);
};

Bool LatticeNavigator::ok() const {
  return True;
};

LatticeStepper * LatticeNavigator::castToStepper() {
    return 0;
};

const LatticeStepper * LatticeNavigator::castToConstStepper() const {
    return 0;
};

TiledStepper * LatticeNavigator::castToTiler() {
    return 0;
};

const TiledStepper * LatticeNavigator::castToConstTiler() const {
    return 0;
};
