//# NewMSDopplerColumns.h: provides easy access to NewMSDoppler columns
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

#if !defined(AIPS_NewMSDOPPLERCOLUMNS_H)
#define AIPS_NewMSDOPPLERCOLUMNS_H

#include <aips/MeasurementSets/NewMSDoppler.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>

// <summary>
// A convenience class to provide easy access to NewMSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDoppler
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSDopplerColumns stands for NewMeasurementSet Doppler Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSDoppler Table,
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

class NewMSDopplerColumns
{
public:

NewMSDopplerColumns(NewMSDoppler& msDoppler);

~NewMSDopplerColumns();

  // Is this object defined? (NewMSDoppler table is optional)
  Bool isNull() {return isNull_p;}

  // Access to columns
  ScalarColumn<Int>& dopplerId() {return dopplerId_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Int>& transitionId() {return transitionId_p;}

private:

Bool isNull_p;
ScalarColumn<Int> dopplerId_p;
ScalarColumn<Int> sourceId_p;
ScalarColumn<Int> transitionId_p;

};

// <summary>
// A convenience class to provide easy access to NewMSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDoppler
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSDopplerColumns stands for Read-Only NewMeasurementSet Doppler Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSDoppler Table.
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

class RONewMSDopplerColumns
{
public:

  RONewMSDopplerColumns(const NewMSDoppler& msDoppler);
  
  ~RONewMSDopplerColumns();
  
  // Is this object defined? (NewMSDoppler table is optional)
  Bool isNull() {return isNull_p;}
  
  // Access to columns
  const ROScalarColumn<Int>& dopplerId() const {return dopplerId_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Int>& transitionId() const {return transitionId_p;}
  
private:
  
  Bool isNull_p;
  ROScalarColumn<Int> dopplerId_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Int> transitionId_p;

};

#endif
