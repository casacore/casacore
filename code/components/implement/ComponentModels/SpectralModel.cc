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
#include <trial/Measures/MeasureHolder.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/DataType.h>

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
  if (record.dataType(type) != TpString) {
    errorMessage += String("\nThe 'type' field in the spectrum record,")
      + String("must be a String");
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }      
  if (record.shape(type) != IPosition(1,1)) {
    errorMessage += String("The 'type' field, in the spectrum record,") + 
      String(" must have only 1 element\n");
    return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
  }      
  const String & typeVal = record.asString(type);
  return ComponentType::spectralShape(typeVal);
}

Bool SpectralModel::readFreq(String & errorMessage,
			     const RecordInterface & record) {
  const String freqString("frequency");
  if (!record.isDefined(freqString)) {
    errorMessage += "The 'frequency' field does not exist\n";
    return False;
  }
  const RecordFieldId frequency(freqString);
  if (record.dataType(frequency) != TpRecord) {
    errorMessage += "The 'frequency' field must be a record\n";
    return False;
  }
  const Record & freqRecord = record.asRecord(frequency);
  MeasureHolder mh;
  if (!mh.fromRecord(errorMessage, freqRecord)) {
    errorMessage += "Could not parse the reference frequency\n";
    return False;
  }
  if (!mh.isMFrequency()) {
    errorMessage += "The reference frequency is not a frequency measure\n";
    return False;
  }
  setRefFrequency(mh.asMFrequency());
  return True;
}

Bool SpectralModel::addFreq(String & errorMessage, 
			    RecordInterface & record) const {
  Record freqRecord;
  const MeasureHolder mh(refFrequency());
  if (!mh.toRecord(errorMessage, freqRecord)) {
    errorMessage += "Could not convert the reference frequency to a record\n";
    return False;
  }
  record.defineRecord(RecordFieldId("frequency"), freqRecord);
  return True;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 SpectralModel"
// End: 
