//# NewMSProcessorColumns.h: provides easy access to NewMSProcessor columns
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

#if !defined(AIPS_NewMSPROCESSORCOLUMNS_H)
#define AIPS_NewMSPROCESSORCOLUMNS_H

#include <aips/MeasurementSets/NewMSProcessor.h>
#include <aips/Tables/ScalarColumn.h>

// <summary>
// A convenience class to provide easy access to NewMSProcessor columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSProcessor
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSProcessorColumns stands for NewMeasurementSet Processor Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSProcessor Table,
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

class NewMSProcessorColumns
{
public:

NewMSProcessorColumns(NewMSProcessor& msProcessor);

~NewMSProcessorColumns();

  // Access to columns
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& modeId() {return modeId_p;}
  ScalarColumn<String>& type() {return type_p;}
  ScalarColumn<Int>& typeId() {return typeId_p;}
  ScalarColumn<String>& subType() {return subType_p;}
  ScalarColumn<Int>& passId() {return passId_p;}

private:

  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> modeId_p;
  ScalarColumn<String> type_p;
  ScalarColumn<Int> typeId_p;
  ScalarColumn<String> subType_p;
  ScalarColumn<Int> passId_p;

};

// <summary>
// A convenience class to provide easy access to NewMSProcessor columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSProcessor
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSProcessorColumns stands for Read-Only NewMeasurementSet Processor Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSProcessor Table.
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

class RONewMSProcessorColumns
{
public:

RONewMSProcessorColumns(const NewMSProcessor& msProcessor);

~RONewMSProcessorColumns();

// Access to columns
const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
const ROScalarColumn<Int>& modeId() const {return modeId_p;}
const ROScalarColumn<String>& type() const {return type_p;}
const ROScalarColumn<Int>& typeId() const {return typeId_p;}
const ROScalarColumn<String>& subType() const {return subType_p;}
const ROScalarColumn<Int>& passId() const {return passId_p;}

private:

ROScalarColumn<Bool> flagRow_p;
ROScalarColumn<Int> modeId_p;
ROScalarColumn<String> type_p;
ROScalarColumn<Int> typeId_p;
ROScalarColumn<String> subType_p;
ROScalarColumn<Int> passId_p;

};

#endif
