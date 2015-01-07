//# DerivedMSCal.h: Virtual column engine to return derived MS values
//# Copyright (C) 2010
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

#ifndef DERIVEDMSCAL_DERIVEDMSCAL_H
#define DERIVEDMSCAL_DERIVEDMSCAL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/derivedmscal/DerivedMC/MSCalEngine.h>
#include <casacore/tables/DataMan/VirtColEng.h>

namespace casacore {

// <summary>
// Virtual column engine to return derived MS values
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tDerivedMSCal.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> The Table Data Managers concept as described in module file
//        <linkto module="Tables:Data Managers">Tables.h</linkto>
//   <li> MeasurementSet
// </prerequisite>

// <synopsis>
// DerivedMSCal makes it possible to have virtual columns for the derived
// MeasurementSet values hourangle, parallactic angle, azimuth/elevation,
// and local sidereal time. In this way such derived values appear to be
// ordinary columns with the exception that no values can be put into them.
//
// The following columns can be defined:
// <ul>
//  <li> HA is the hourangle of the array center (observatory position).
//  <li> HA1 is the hourangle of ANTENNA1.
//  <li> HA2 is the hourangle of ANTENNA2.
//  <li> HADEC is the hourangle/DEC of the array center (observatory position).
//  <li> HADEC1 is the hourangle/DEC of ANTENNA1.
//  <li> HADEC2 is the hourangle/DEC of ANTENNA2.
//  <li> LAST is the local sidereal time of the array center.
//  <li> LAST1 is the local sidereal time of ANTENNA1.
//  <li> LAST2 is the local sidereal time of ANTENNA2.
//  <li> PA1 is the parallactic angle of ANTENNA1.
//  <li> PA2 is the parallactic angle of ANTENNA2.
//  <li> AZEL1 is the azimuth/elevation of ANTENNA1.
//  <li> AZEL2 is the azimuth/elevation of ANTENNA2.
//  <li> UVW_J2000 is the UVW coordinates in J2000 (in meters)
// </ul>
// All columns have data type double and unit radian (except UVW). The HADEC,
// AZEL, and UVW columns are array columnns while the others are scalar columns.
//
// This engine is meant for a MeasurementSet, but can be used for any table
// containing an ANTENNA and FIELD subtable and the relevant columns in the
// main table (ANTENNA1 and/or ANTENNA2, FIELD_ID, and TIME).
// <br>In principle the array center is the Observatory position, which is
// taken from the Measures Observatory table using the telescope name found
// in the OBSERVATION subtable. However, if the subtable is not defined or
// empty or if the telescope name is unknown, the position of the first antenna
// is used as the array position.
//
// The engine can also be used for a CASA Calibration Table. It understands
// how it references the MeasurementSets. Because calibration tables contain
// no ANTENNA2 columns, columns XX2 are the same as XX1.
// </synopsis>

// <motivation>
// It makes it possible to use generic table software (like querying,
// plotting, tablebrowser) on these values.
// </motivation>

// <example>
// The following example shows how to add such columns to an MS and use
// them thereafter.
// <srcblock>
//  // Open the table for update (to be able to add the columns).
//  Table tab ("tDerivedMSCal_tmp.tab", Table::Update);
//  // Define the columns and add them using DerivedMSCal.
//  TableDesc td;
//  td.addColumn (ScalarColumnDesc<double>("HA1"));
//  td.addColumn (ScalarColumnDesc<double>("HA2"));
//  td.addColumn (ScalarColumnDesc<double>("PA1"));
//  td.addColumn (ScalarColumnDesc<double>("PA2"));
//  DerivedMSCal dataMan;
//  tab.addColumn (td, dataMan);
//  // Print values of all rows.
//  ScalarColumn<double> ha1(tab, "HA1");
//  ScalarColumn<double> ha2(tab, "HA2");
//  ScalarColumn<double> pa1(tab, "PA1");
//  ScalarColumn<double> pa2(tab, "PA2");
//  for (uInt row=0; row<tab.nrow(); ++row) {
//    cout << ha1(row)<<' '<<ha2(row)<<' '<<pa1(row)<<' '<<pa2(row)<<endl;
//  }
// </srcblock>
// </example>

// <todo asof="$DATE:$">
//  <li> Take care of the feeds and their offsets.
//  <li> Have a conversion engine per field/antenna/feed?
// </todo>


class DerivedMSCal : public VirtualColumnEngine
{
public:
    // Create the data manager.
  DerivedMSCal();

  // Create a Lofar storage manager with the given name.
  // The specifications are part of the record (as created by dataManagerSpec).
  explicit DerivedMSCal (const Record& spec);
  
  ~DerivedMSCal();

  // Clone this object.
  virtual DataManager* clone() const;

  // Prepare the object. It sets the Table object in the engine.
  virtual void prepare();

  // Get the type name of the data manager (i.e. DerivedMSCal).
  virtual String dataManagerType() const;
  
  // Record a record containing data manager specifications.
  virtual Record dataManagerSpec() const;

  // Columns can be added.
  virtual Bool canAddColumn() const;
  
  // Columns can be removed.
  virtual Bool canRemoveColumn() const;
  
  // Make the object from the type name string.
  // This function gets registered in the DataManager "constructor" map.
  // The caller has to delete the object.
  // The dataManName is not used.
  static DataManager* makeObject (const String& dataManName,
                                  const Record& spec);

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();

private:
  // Copy constructor cannot be used.
  DerivedMSCal (const DerivedMSCal& that);

  // Assignment cannot be used.
  DerivedMSCal& operator= (const DerivedMSCal& that);
  
  // Do the final addition of a column.
  // It won't do anything.
  virtual void addColumn (DataManagerColumn*);
  
  // Remove a column from the data file.
  // It won't do anything.
  virtual void removeColumn (DataManagerColumn*);
  
  // Create a column in the storage manager on behalf of a table column.
  // The caller has to delete the newly created object.
  // <group>
  // Create a scalar column.
  virtual DataManagerColumn* makeScalarColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // Create an indirect array column.
  virtual DataManagerColumn* makeIndArrColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // </group>

  //# Declare member variables.
  MSCalEngine                 itsEngine;
  vector<DataManagerColumn*>  itsColumns;
};


} //# end namespace

#endif
