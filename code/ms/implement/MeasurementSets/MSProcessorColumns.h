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

#if !defined(AIPS_NEWMSPROCESSORCOLUMNS_H)
#define AIPS_NEWMSPROCESSORCOLUMNS_H

#include <aips/aips.h>
#include <aips/MeasurementSets/NewMSProcessor.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSProcessor;

// <summary>
// A class to provide easy read-only access to NewMSProcessor columns
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
// This class provides read-only access to the columns in the NewMSProcessor
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=RONewMSColumns>
// RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSProcessorColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSProcessorColumns(const NewMSProcessor& msProcessor);

  // The destructor does nothing special
  ~RONewMSProcessorColumns();

  // Access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<Int>& modeId() const {return modeId_p;}
  const ROScalarColumn<String>& type() const {return type_p;}
  const ROScalarColumn<Int>& typeId() const {return typeId_p;}
  const ROScalarColumn<String>& subType() const {return subType_p;}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& passId() const {return passId_p;}
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSProcessorColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSProcessor& msProcessor);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSProcessorColumns(const RONewMSProcessorColumns&);
  RONewMSProcessorColumns& operator=(const RONewMSProcessorColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSProcessor& msField);
  
  //# required columns
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<Int> modeId_p;
  ROScalarColumn<String> type_p;
  ROScalarColumn<Int> typeId_p;
  ROScalarColumn<String> subType_p;
  //# optional columns
  ROScalarColumn<Int> passId_p;
};

// <summary>
// A class to provide easy read-write access to NewMSProcessor columns
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

class NewMSProcessorColumns: public RONewMSProcessorColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSProcessorColumns(NewMSProcessor& msProcessor);

  // The destructor does nothing special
  ~NewMSProcessorColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& modeId() {return modeId_p;}
  ScalarColumn<String>& type() {return type_p;}
  ScalarColumn<Int>& typeId() {return typeId_p;}
  ScalarColumn<String>& subType() {return subType_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ScalarColumn<Int>& passId() {return passId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSProcessorColumns::flagRow();}
  const ROScalarColumn<Int>& modeId() const {
    return RONewMSProcessorColumns::modeId();}
  const ROScalarColumn<String>& type() const {
    return RONewMSProcessorColumns::type();}
  const ROScalarColumn<Int>& typeId() const {
    return RONewMSProcessorColumns::typeId();}
  const ROScalarColumn<String>& subType() const {
    return RONewMSProcessorColumns::subType();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& passId() const {
    return RONewMSProcessorColumns::passId();}
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSProcessorColumns();

  //# attach this object to the supplied table.
  void attach(NewMSProcessor& msProcessor);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSProcessorColumns(const NewMSProcessorColumns&);
  NewMSProcessorColumns& operator=(const NewMSProcessorColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSProcessor& msProcessor);
  
  //# required columns
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> modeId_p;
  ScalarColumn<String> type_p;
  ScalarColumn<Int> typeId_p;
  ScalarColumn<String> subType_p;
  //# optional columns
  ScalarColumn<Int> passId_p;
};
#endif
