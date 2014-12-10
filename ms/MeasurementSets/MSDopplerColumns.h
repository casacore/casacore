//# MSDopplerColumns.h: provides easy access to MSDoppler columns
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

#ifndef MS_MSDOPPLERCOLUMNS_H
#define MS_MSDOPPLERCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSDoppler;

// <summary>
// A class to provide easy read-only access to MSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSDoppler
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSDopplerColumns stands for Read-Only MeasurementSet Doppler Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSDoppler
// Table.  It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to worry about
// getting those right. There is an access function for every predefined
// column. Access to non-predefined columns will still have to be done with
// explicit declarations.  See <linkto class=ROMSColumns>
// ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSDopplerColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSDopplerColumns(const MSDoppler& msDoppler);
  
  // The destructor does nothing special
  ~ROMSDopplerColumns();
  
  // Is this object defined? (MSDoppler table is optional)
  Bool isNull() const {return isNull_p;}
  
  // Access to columns
  // <group>
  const ROScalarColumn<Int>& dopplerId() const {return dopplerId_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Int>& transitionId() const {return transitionId_p;}
  const ROScalarColumn<Double>& velDef() const {return velDef_p;}
  const ROScalarQuantColumn<Double>& velDefQuant() const {return velDefQuant_p;}
  const ROScalarMeasColumn<MDoppler>& velDefMeas() const {return velDefMeas_p;}
  // </group>
  
  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : dopplerId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSDopplerColumns();

  //# attach this object to the supplied table.
  void attach(const MSDoppler& msDoppler);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSDopplerColumns(const ROMSDopplerColumns&);
  ROMSDopplerColumns& operator=(const ROMSDopplerColumns&);

  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> dopplerId_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Int> transitionId_p;
  ROScalarColumn<Double> velDef_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MDoppler> velDefMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> velDefQuant_p;

};

// <summary>
// A class to provide easy read-write access to MSDoppler columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSDoppler
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSDopplerColumns stands for MeasurementSet Doppler Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSDoppler Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=MSColumns> MSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class MSDopplerColumns: public ROMSDopplerColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSDopplerColumns(MSDoppler& msDoppler);

  // The destructor does nothing special
  ~MSDopplerColumns();

  // Read-write access to required columns
  // <group>
  ScalarColumn<Int>& dopplerId() {return dopplerId_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Int>& transitionId() {return transitionId_p;}
  ScalarColumn<Double>& velDef() {return velDef_p;}
  ScalarQuantColumn<Double>& velDefQuant(){return velDefQuant_p;}
  ScalarMeasColumn<MDoppler>& velDefMeas() {return velDefMeas_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& dopplerId() const {
    return ROMSDopplerColumns::dopplerId();}
  const ROScalarColumn<Int>& sourceId() const {
    return ROMSDopplerColumns::sourceId();}
  const ROScalarColumn<Int>& transitionId() const {
    return ROMSDopplerColumns::transitionId();}
  const ROScalarColumn<Double>& velDef() const {
    return ROMSDopplerColumns::velDef();}
  const ROScalarQuantColumn<Double>& velDefQuant() const {
    return ROMSDopplerColumns::velDefQuant();}
  const ROScalarMeasColumn<MDoppler>& velDefMeas() const {
    return ROMSDopplerColumns::velDefMeas();}
  // </group>

  // set the DOPPLER type for the VELDEF column.
  void setVelDefRef(MDoppler::Types ref);  

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSDopplerColumns();

  //# attach this object to the supplied table.
  void attach(MSDoppler& msDoppler);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSDopplerColumns(const MSDopplerColumns&);
  MSDopplerColumns& operator=(const MSDopplerColumns&);

  //# required columns
  ScalarColumn<Int> dopplerId_p;
  ScalarColumn<Int> sourceId_p;
  ScalarColumn<Int> transitionId_p;
  ScalarColumn<Double> velDef_p;

  //# Access to Measure columns
  ScalarMeasColumn<MDoppler> velDefMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> velDefQuant_p;
};


} //# NAMESPACE CASACORE - END

#endif
