//# MSDataDescColumns.h: provides easy access to MSDataDescription columns
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

#ifndef MS_MSDATADESCCOLUMNS_H
#define MS_MSDATADESCCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSDataDescription;
// <summary>
// A class to provide easy read-only access to MSDataDesc columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSDataDesc
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSDataDescColumns stands for Read-Only MeasurementSet DataDesc Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSDataDesc
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=ROMSColumns>
// ROMSColumns</linkto> for an example.  <note role=warning> The Table that
// is used to construct this class must not be destroyed (or go out of scope)
// before this class does. Otherwise the scalar and array columns use by this
// class will be left dangling.</note>

// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSDataDescColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSDataDescColumns(const MSDataDescription& msDataDesc);
  
  // The destructor does nothing special
  ~ROMSDataDescColumns();
  
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

  // returns the last row that contains the specified entries in the
  // SPECTRAL_WINDOW_ID & POLARIZATION_ID columns. Returns -1 if no match could
  // be found. Flagged rows can never match. If tryRow is non-negative, then
  // that row is tested to see if it matches before any others are
  // tested. Setting tryRow to a positive value greater than the table length
  // will throw an exception (AipsError).
  Int match(uInt spwId, uInt polId, Int tryRow=-1);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSDataDescColumns();

  //# attach this object to the supplied table.
  void attach(const MSDataDescription& msDataDesc);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSDataDescColumns(const ROMSDataDescColumns&);
  ROMSDataDescColumns& operator=(const ROMSDataDescColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSDataDescription& msDataDesc);
  
  //# required columns
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<Int> polarizationId_p;
  ROScalarColumn<Int> spectralWindowId_p;
  //# optional columns
  ROScalarColumn<Int> lagId_p;
};

// <summary>
// A class to provide easy read-write access to MSDataDescription columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSDataDesc
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSDataDescColumns stands for MeasurementSet DataDescription Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSDataDesc Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=MSColumns> MSColumns</linkto> for an example.
// <note role=warning> The Table that is used to construct this class must not
// be destroyed (or go out of scope) before this class does. Otherwise the
// scalar and array columns use by this class will be left dangling.</note>
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class MSDataDescColumns: public ROMSDataDescColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSDataDescColumns(MSDataDescription& msDataDesc);

  // The destructor does nothing special
  ~MSDataDescColumns();

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
    return ROMSDataDescColumns::flagRow();}
  const ROScalarColumn<Int>& polarizationId() const {
    return ROMSDataDescColumns::polarizationId();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return ROMSDataDescColumns::spectralWindowId();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& lagId() const {
    return ROMSDataDescColumns::lagId();}
  // </group>
  
protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSDataDescColumns();

  //# attach all the columns in the supplied table to this object
  void attach(MSDataDescription& msDataDesc);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSDataDescColumns(const MSDataDescColumns&);
  MSDataDescColumns& operator=(const MSDataDescColumns&);

  //# attach optional columns in the supplied Table (if they exist)
  void attachOptionalCols(MSDataDescription& msDataDesc);

  //# required columns
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> polarizationId_p;
  ScalarColumn<Int> spectralWindowId_p;
  //# optional columns
  ScalarColumn<Int> lagId_p;
};

} //# NAMESPACE CASACORE - END

#endif
