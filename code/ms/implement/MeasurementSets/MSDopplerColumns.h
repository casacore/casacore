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

#if !defined(AIPS_NEWMSDOPPLERCOLUMNS_H)
#define AIPS_NEWMSDOPPLERCOLUMNS_H

#include <aips/aips.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSDoppler;

// <summary>
// A class to provide easy read-only access to NewMSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDoppler
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSDopplerColumns stands for Read-Only NewMeasurementSet Doppler Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSDoppler
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

class RONewMSDopplerColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSDopplerColumns(const NewMSDoppler& msDoppler);
  
  // The destructor does nothing special
  ~RONewMSDopplerColumns();
  
  // Is this object defined? (NewMSDoppler table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to columns
  // <group>
  const ROScalarColumn<Int>& dopplerId() const {return dopplerId_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Int>& transitionId() const {return transitionId_p;}
  // </group>
  
  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : dopplerId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSDopplerColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSDoppler& msDoppler);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSDopplerColumns(const RONewMSDopplerColumns&);
  RONewMSDopplerColumns& operator=(const RONewMSDopplerColumns&);

  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> dopplerId_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Int> transitionId_p;
};

// <summary>
// A class to provide easy read-write access to NewMSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSDoppler
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

class NewMSDopplerColumns: public RONewMSDopplerColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSDopplerColumns(NewMSDoppler& msDoppler);

  // The destructor does nothing special
  ~NewMSDopplerColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& dopplerId() {return dopplerId_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Int>& transitionId() {return transitionId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& dopplerId() const {
    return RONewMSDopplerColumns::dopplerId();}
  const ROScalarColumn<Int>& sourceId() const {
    return RONewMSDopplerColumns::sourceId();}
  const ROScalarColumn<Int>& transitionId() const {
    return RONewMSDopplerColumns::transitionId();}
  // </group>
  
protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSDopplerColumns();

  //# attach this object to the supplied table.
  void attach(NewMSDoppler& msDoppler);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSDopplerColumns(const NewMSDopplerColumns&);
  NewMSDopplerColumns& operator=(const NewMSDopplerColumns&);

  //# required columns
  ScalarColumn<Int> dopplerId_p;
  ScalarColumn<Int> sourceId_p;
  ScalarColumn<Int> transitionId_p;
};

#endif
