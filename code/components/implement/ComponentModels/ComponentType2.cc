//# ComponentType2.cc:  this defines ComponentType2.cc
//# Copyright (C) 1997,1998
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

#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/DiskShape.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/SpectralModel.h>

// The functions declared below are contained in a seperate .cc file to prevent
// all the derived classes from unnecessarily being linked in when they are not
// needed.

ComponentShape * ComponentType::
construct(ComponentType::Shape shapeEnum) {
  switch (shapeEnum) {
  case ComponentType::POINT: 
    return new PointShape;
  case ComponentType::GAUSSIAN:
    return new GaussianShape;
  case ComponentType::DISK:
    return new DiskShape;
  default:
    return (ComponentShape *) 0;
  };
}

SpectralModel * ComponentType::
construct(ComponentType::SpectralShape spectralEnum) {
  switch (spectralEnum) {
  case ComponentType::CONSTANT_SPECTRUM: 
    return new ConstantSpectrum;
  case ComponentType::SPECTRAL_INDEX:
    return new SpectralIndex;
  default:
    return (SpectralModel *) 0;
  };
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentType2"
// End: 
