//# MSProcessorColumns.h: provides easy access to MSProcessor columns
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

#ifndef MS_MSPROCESSORCOLUMNS_H
#define MS_MSPROCESSORCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSProcessor.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSProcessor;

// <summary>
// A class to provide easy access to MSProcessor columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSProcessor
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSProcessorColumns stands for MeasurementSet Processor Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSProcessor Table,
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

class MSProcessorColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSProcessorColumns(const MSProcessor& msProcessor);

  // The destructor does nothing special
  ~MSProcessorColumns();

  // Access to required columns
  // <group>
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& modeId() {return modeId_p;}
  ScalarColumn<String>& type() {return type_p;}
  ScalarColumn<Int>& typeId() {return typeId_p;}
  ScalarColumn<String>& subType() {return subType_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<Int>& passId() {return passId_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<Int>& modeId() const {return modeId_p;}
  const ScalarColumn<String>& type() const {return type_p;}
  const ScalarColumn<Int>& typeId() const {return typeId_p;}
  const ScalarColumn<String>& subType() const {return subType_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ScalarColumn<Int>& passId() const {return passId_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return flagRow_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSProcessorColumns();

  //# attach this object to the supplied table.
  void attach(const MSProcessor& msProcessor);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSProcessorColumns(const MSProcessorColumns&);
  MSProcessorColumns& operator=(const MSProcessorColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSProcessor& msProcessor);
  
  //# required columns
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> modeId_p;
  ScalarColumn<String> type_p;
  ScalarColumn<Int> typeId_p;
  ScalarColumn<String> subType_p;
  //# optional columns
  ScalarColumn<Int> passId_p;
};

//# Define the RO version for backward compatibility.
typedef MSProcessorColumns ROMSProcessorColumns;

} //# NAMESPACE CASACORE - END

#endif
