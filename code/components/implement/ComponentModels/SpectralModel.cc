//# SpectralModel.cc:
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

#include <trial/ComponentModels/SpectralModel.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

SpectralModel::~SpectralModel() {
}

ComponentType::SpectralShape SpectralModel::
getType(String & errorMessage, const RecordInterface & record) {
  const String typeString("type");
  if (!record.isDefined(typeString)) {
    errorMessage += 
      String("\nThe record does not have a 'type' field.");
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }
  const RecordFieldId type(typeString);
  if (record.shape(type) != IPosition(1,1)) {
    errorMessage += String("\nThe 'type' field must have only 1 element");
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }      
  String typeVal;
  try {
    typeVal = record.asString(type);
  }
  catch (AipsError x) {
    errorMessage += String("\nThe 'type' field must be a String");
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  } end_try;
  return ComponentType::spectralShape(typeVal);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 SpectralModel"
// End: 
