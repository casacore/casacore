//# MSConcat.cc: A class for concatenating MeasurementSets.
//# Copyright (C) 2000
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

#include <trial/MeasurementSets/MSConcat.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>

MSConcat::MSConcat(NewMeasurementSet& ms):
  NewMSColumns(ms),
  itsFixedShape(isFixedShape(ms.tableDesc()))
{
}

Bool MSConcat::isFixedShape(const TableDesc& td) {
  Bool isFixed = False;
  const Vector<String> hypercolumnNames=td.hypercolumnNames();
  const uInt nHyperCols = hypercolumnNames.nelements();
  Vector<String> dataColNames,coordColNames,idColNames;
  uInt hc = 0;
  while (isFixed == False && hc < nHyperCols) {
    td.hypercolumnDesc(hypercolumnNames(hc), dataColNames, coordColNames,
		       idColNames);
    const uInt nDataCol = dataColNames.nelements();
    uInt dc = 0;
    while (isFixed == False && dc < nDataCol) {
      const String& dataColName = dataColNames(dc);
      if (dataColName == NewMS::columnName(NewMS::DATA) ||
	  dataColName == NewMS::columnName(NewMS::SIGMA) || 
	  dataColName == NewMS::columnName(NewMS::WEIGHT) || 
	  dataColName == NewMS::columnName(NewMS::FLAG) || 
	  dataColName == NewMS::columnName(NewMS::FLAG_CATEGORY) || 
	  dataColName == NewMS::columnName(NewMS::CORRECTED_DATA) || 
	  dataColName == NewMS::columnName(NewMS::MODEL_DATA) || 
	  dataColName == NewMS::columnName(NewMS::IMAGING_WEIGHT) || 
	  dataColName == NewMS::columnName(NewMS::WEIGHT_SPECTRUM) ||
	  dataColName == NewMS::columnName(NewMS::SIGMA_SPECTRUM) ||
	  dataColName == NewMS::columnName(NewMS::FLOAT_DATA) ||
	  dataColName == NewMS::columnName(NewMS::VIDEO_POINT) ||
	  dataColName == NewMS::columnName(NewMS::LAG_DATA)) {
	isFixed = td.columnDesc(dataColNames(dc)).isFixedShape();
      }
      dc++;
    }
    hc++;
    dataColNames.resize(0);
    coordColNames.resize(0);
    idColNames.resize(0);
  }
  return isFixed;
}


void MSConcat::concatenate(const NewMeasurementSet& ms)
{
  if (itsFixedShape && isFixedShape(ms.tableDesc())) {
    checkShapes(ms);
  }
//   checkCategories();
//   const Block<uInt> newAntIndices = copyAntenna(ms.antenna());
}

void MSConcat::checkShapes(const NewMeasurementSet& ms) const 
{
  if (nrow() == 0) return;
  //  const IPosition curShape = flag().shape(0);
}

// Local Variables: 
// compile-command: "gmake MSConcat"
// End: 
