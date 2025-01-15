//# BaseEngine.cc: Base class for the TaQL UDF conversion engines
//# Copyright (C) 2018
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/meas/MeasUDF/BaseEngine.h>


namespace casacore {

  BaseEngine::~BaseEngine()
  {}
  
  void BaseEngine::adaptForConstant (const IPosition& shapeConstants, uInt nvalues)
  {
    uInt size = shapeConstants.product();
    // Set to shape if not empty.
    if (size > 0) {
      itsIsConst = True;
      itsShape.resize (0);
      itsShape = shapeConstants;
      // Prepend with extra axis if needed.
      if (nvalues > 0) {
        itsShape.prepend (IPosition(1, nvalues));
      }
    }
    // If returning single values, one dimension less is used.
    // Note the shape is already set (to the column's shape) if a column is used.
    if (itsShape.size() > 0) {
      if (nvalues == 1) {
        IPosition outShape = itsShape.getLast (itsShape.size() - 1);
        itsShape.resize (outShape.size());
        itsShape = outShape;
      } else if (nvalues > 0) {
        // Set correct nr of output values per element.
        itsShape[0] = nvalues;
      }
    }
    if (itsNDim < 0  &&  itsShape.size() > 0) {
      itsNDim = itsShape.size();
    }
  }
  
  void BaseEngine::extendBase (const BaseEngine& engine, Bool skipFirstAxis)
  {
    // An empty shape means it is a scalar or it is unknown.
    // ndim<0 means the dimensionality and shape are unknown.
    // ndim=0 means a scalar.
    // ndim>0 is a known dimensionality (but shape might be unknown).
    IPosition shp = engine.shape();
    Int ndim = engine.ndim();
    IPosition shape;
    if (skipFirstAxis) {
      // Remove first axis (for e.g. position and direction).
      if (shp.size() > 0) {
        shape = shp.getLast (shp.size() - 1);
      } else {
        shape = shp;
      }
      // Treat it as a scalar if only 1 value.
      if (shape.product() == 1) {
        shape.resize (0);
        ndim = 0;
      } else if (ndim > 0) {
        // Remove first axis.
        ndim--;
      }
    }
    if (ndim > 0) {
      if (itsNDim >= 0) {
        itsNDim += ndim;
      }
      itsShape.append (shape);
    }
    if (! engine.isConstant()) {
      itsIsConst = False;
    }
  }

  void BaseEngine::deriveAttr (const Unit&, Int)
  {}

  void BaseEngine::setValueType (Int)
  {}

  String BaseEngine::stripMeasType (const String& type)
  {
    return type;
  }

}
