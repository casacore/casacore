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

#if !defined(AIPS_NEWMSDATADESCCOLUMNS_H)
#define AIPS_NEWMSDATADESCCOLUMNS_H

#include <aips/aips.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSDataDescription;
// <summary>
// A class to provide easy read-only access to NewMSDataDesc columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDataDesc
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSDataDescColumns stands for Read-Only NewMeasurementSet DataDesc Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSDataDesc
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=RONewMSColumns>
// RONewMSColumns</linkto> for an example.  <note role=warning> The Table that
// is used to construct this class must not be destroyed (or go out of scope)
// before this class does. Otherwise the scalar and array columns use by this
// class will be left dangling.</note>

// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSDataDescColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSDataDescColumns(const NewMSDataDescription& msDataDesc);
  
  // The destructor does nothing special
  ~RONewMSDataDescColumns();
  
  // Access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<Int>& polarizationId() const {return polarizationId_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROScalarColumn<Int>& lagId() const {return lagId_p;}
  // </group>
  
  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return flagRow_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSDataDescColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSDataDescription& msDataDesc);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSDataDescColumns(const RONewMSDataDescColumns&);
  RONewMSDataDescColumns& operator=(const RONewMSDataDescColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSDataDescription& msDataDesc);
  
  //# required columns
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<Int> polarizationId_p;
  ROScalarColumn<Int> spectralWindowId_p;
  //# optional columns
  ROScalarColumn<Int> lagId_p;
};

// <summary>
// A class to provide easy read-write access to NewMSDataDescription columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDataDesc
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSDataDescColumns stands for NewMeasurementSet DataDescription Table
// columns.
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
// <note role=warning> The Table that is used to construct this class must not
// be destroyed (or go out of scope) before this class does. Otherwise the
// scalar and array columns use by this class will be left dangling.</note>
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class NewMSDataDescColumns: public RONewMSDataDescColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSDataDescColumns(NewMSDataDescription& msDataDesc);

  // The destructor does nothing special
  ~NewMSDataDescColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& polarizationId() {return polarizationId_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  // </group>

  // read-write access to optional columns
  // <group>
  ScalarColumn<Int>& lagId() {return lagId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSDataDescColumns::flagRow();}
  const ROScalarColumn<Int>& polarizationId() const {
    return RONewMSDataDescColumns::polarizationId();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return RONewMSDataDescColumns::spectralWindowId();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& lagId() const {
    return RONewMSDataDescColumns::lagId();}
  // </group>
  
protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSDataDescColumns();

  //# attach all the columns in the supplied table to this object
  void attach(NewMSDataDescription& msDataDesc);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSDataDescColumns(const NewMSDataDescColumns&);
  NewMSDataDescColumns& operator=(const NewMSDataDescColumns&);

  //# attach optional columns in the supplied Table (if they exist)
  void attachOptionalCols(NewMSDataDescription& msDataDesc);

  //# required columns
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> polarizationId_p;
  ScalarColumn<Int> spectralWindowId_p;
  //# optional columns
  ScalarColumn<Int> lagId_p;
};
#endif
