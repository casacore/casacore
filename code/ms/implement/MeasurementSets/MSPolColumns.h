//# NewMSPolarizationColumns.h: provides easy access to NewMSPolarization columns
//# Copyright (C) 1999,2000
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

#if !defined(AIPS_NewMSPOLARIZATIONCOLUMNS_H)
#define AIPS_NewMSPOLARIZATIONCOLUMNS_H

#include <aips/MeasurementSets/NewMSPolarization.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>

// <summary>
// A convenience class to provide easy access to NewMSPolarization columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSPolarization
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSPolarizationColumns stands for NewMeasurementSet Polarization Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSPolarization Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class NewMSPolarizationColumns
{
public:

NewMSPolarizationColumns(NewMSPolarization& msPolarization);

~NewMSPolarizationColumns();

  // Access to columns
  ArrayColumn<Int>& corrProduct() {return corrProduct_p;}
  ArrayColumn<Int>& corrType() {return corrType_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& numCorr() {return numCorr_p;}

private:

  ArrayColumn<Int> corrProduct_p;
  ArrayColumn<Int> corrType_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> numCorr_p;

};

// <summary>
// A convenience class to provide easy access to NewMSPolarization columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSPolarization
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSPolarizationColumns stands for Read-Only NewMeasurementSet Polarization Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSPolarization Table.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=RONewMSColumns> RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSPolarizationColumns
{
public:

RONewMSPolarizationColumns(const NewMSPolarization& msPolarization);

~RONewMSPolarizationColumns();

// Access to columns
const ROArrayColumn<Int>& corrProduct() const {return corrProduct_p;}
const ROArrayColumn<Int>& corrType() const {return corrType_p;}
const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
const ROScalarColumn<Int>& numCorr() const {return numCorr_p;}

private:

ROArrayColumn<Int> corrProduct_p;
ROArrayColumn<Int> corrType_p;
ROScalarColumn<Bool> flagRow_p;
ROScalarColumn<Int> numCorr_p;

};

#endif
