//# NewMSDataDescColumns.h: provides easy access to NewMSDataDescription columns
//# Copyright (C) 1996,1999,2000
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

#if !defined(AIPS_NewMSDATADESCCOLUMNS_H)
#define AIPS_NewMSDATADESCCOLUMNS_H

#include <aips/MeasurementSets/NewMSDataDescription.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>

// <summary>
// A convenience class to provide easy access to NewMSDataDescription columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDataDesc
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSDataDescColumns stands for NewMeasurementSet DataDescription Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSDataDesc Table,
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

class NewMSDataDescColumns
{
public:

NewMSDataDescColumns(NewMSDataDescription& msDataDesc);

~NewMSDataDescColumns();

// Access to columns
ScalarColumn<Bool>& flagRow() {return flagRow_p;}
ScalarColumn<Int>& polarizationId() {return polarizationId_p;}
ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
ScalarColumn<Int>& lagId() {return lagId_p;}

private:

ScalarColumn<Bool> flagRow_p;
ScalarColumn<Int> lagId_p;
ScalarColumn<Int> polarizationId_p;
ScalarColumn<Int> spectralWindowId_p;

};

// <summary>
// A convenience class to provide easy access to NewMSDataDesc columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDataDesc
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSDataDescColumns stands for Read-Only NewMeasurementSet DataDesc Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSDataDesc Table.
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

class RONewMSDataDescColumns
{
public:

RONewMSDataDescColumns(const NewMSDataDescription& msDataDesc);

~RONewMSDataDescColumns();

// Access to columns
const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
const ROScalarColumn<Int>& polarizationId() const {return polarizationId_p;}
const ROScalarColumn<Int>& spectralWindowId() const {return spectralWindowId_p;}
const ROScalarColumn<Int>& lagId() const {return lagId_p;}

private:

ROScalarColumn<Bool> flagRow_p;
ROScalarColumn<Int> polarizationId_p;
ROScalarColumn<Int> spectralWindowId_p;
ROScalarColumn<Int> lagId_p;

};

#endif
