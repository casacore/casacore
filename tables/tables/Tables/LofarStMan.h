//# LofarStMan.h: The Lofar Storage Manager
//# Copyright (C) 2009
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

#ifndef TABLES_LOFARSTMAN_H
#define TABLES_LOFARSTMAN_H

//# Includes
#include <casa/aips.h>
#include <tables/Tables/DataManager.h>
#include <casa/Containers/Block.h>
#include <casa/vector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations.
class LofarColumn;
class MMapIO;

// <summary>
// The Storage Manager for the main table of a raw LOFAR MS
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLofarStMan.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> The Table Data Managers concept as described in module file
//        <linkto module="Tables:Data Managers">Tables.h</linkto>
// </prerequisite>

// <etymology>
// LofarStMan is the data manager which stores the data for a LOFAR MS.
// </etymology>

// <synopsis>
// LofarStMan is a specific storage manager for the main table of a LOFAR MS.
// For performance purposes the raw data from the correlator is directly
// written to a disk file. However, to be able to use the data directly as a
// MeasurementSet, this specific storage manager is created offering access to
// all mandatory columns in the main table of the MS.
//
// Similar to other storage managers, the LofarStMan files need to be part of
// the table directory. There are two files:
// <ul>
//  <li> The meta file contains the meta data describing baselines, start time,
//       integration time, etc. It needs to be written as an AipsIO file.
//       The meta info should also tell the endianness of the data file.
//  <li> The data file consists of NSEQ data blocks each containing:
//   <ul>
//    <li> 4-byte sequence number defining the time stamp.
//    <li> Complex data with shape [npol,nchan,nbasel].
//    <li> Unsigned short nr of samples used in each data point. It has shape
//         [nchan,nbasel]. It defines WEIGHT and FLAG.
//    <li> Filler bytes to align the blocks as given in the meta info.
//   </ul>
//   The sequence numbers are ascending, but there can be holes due to
//   missing time stamps.
// </ul>
// The first version of the data file can only handle regularly shaped data
// with equal integration times. A future version might be able to deal with
// varying integration times (depending on baseline length).
//
// Most of the MS columns (like DATA_DESC_ID) are not stored in the data file;
// usually they map to the value 0. This is also true for the UVW column, so
// the UVW coordinates need to be added to the table in a separate step because
// the online system does not have the resources to do it.
//
// All columns are readonly with the exception of DATA.
// </synopsis>

// <motivation>
// The common Table storage managers are too slow for the possibly high
// output rate of the LOFAR correlator.
// </motivation>

// <example>
// The following example shows how to create a table and how to attach
// the storage manager to some columns.
// <srcblock>
//   SetupNewTable newtab("name.data", tableDesc, Table::New);
//   LofarStMan stman;                     // define storage manager
//   newtab.bindColumn ("DATA", stman);    // bind column to st.man.
//   newtab.bindColumn ("FLAG", stman);    // bind column to st.man.
//   Table tab(newtab);                    // actually create table
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class LofarStMan : public DataManager
{
public:
    // Create a Lofar storage manager with the given name.
    // If no name is used, it is set to "LofarStMan"
  explicit LofarStMan (const String& dataManagerName = "LofarStMan");

  // Create a Lofar storage manager with the given name.
  // The specifications are part of the record (as created by dataManagerSpec).
  LofarStMan (const String& dataManagerName, const Record& spec);
  
  ~LofarStMan();

  // Clone this object.
  virtual DataManager* clone() const;
  
  // Get the type name of the data manager (i.e. LofarStMan).
  virtual String dataManagerType() const;
  
  // Get the name given to the storage manager (in the constructor).
  virtual String dataManagerName() const;
  
  // Record a record containing data manager specifications.
  virtual Record dataManagerSpec() const;

  // Get the number of rows in this storage manager.
  uInt getNRow() const
    { return itsNrRows; }
  
  // The storage manager cannot add rows.
  virtual Bool canAddRow() const;
  
  // The storage manager cannot delete rows.
  virtual Bool canRemoveRow() const;
  
  // The storage manager cannot add columns.
  virtual Bool canAddColumn() const;
  
  // Columns can be removed, but it does not do anything at all.
  virtual Bool canRemoveColumn() const;
  
  // Make the object from the type name string.
  // This function gets registered in the DataManager "constructor" map.
  // The caller has to delete the object.
  static DataManager* makeObject (const String& aDataManType,
				  const Record& spec);

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();


  // Get data.
  // <group>
  const Block<Int>& ant1() const
    { return itsAnt1; }
  const Block<Int>& ant2() const
    { return itsAnt2; }
  Double time (uInt blocknr);
  Double interval() const
    { return itsTimeIntv; }
  uInt nchan() const
    { return itsNChan; }
  uInt npol() const
    { return itsNPol; }
  Double maxnSample() const
    { return itsMaxNrSample; }
  void getData (uInt rownr, Complex* buf);
  void putData (uInt rownr, const Complex* buf);
  const uShort* getNSample (uInt rownr, Bool swapIfNeeded);
  // </group>

private:
  // Copy constructor cannot be used.
  LofarStMan (const LofarStMan& that);

  // Assignment cannot be used.
  LofarStMan& operator= (const LofarStMan& that);
  
  // Flush and optionally fsync the data.
  // It does nothing, and returns False.
  virtual Bool flush (AipsIO&, Bool doFsync);
  
  // Let the storage manager create files as needed for a new table.
  // This allows a column with an indirect array to create its file.
  virtual void create (uInt nrrow);
  
  // Open the storage manager file for an existing table.
  // Return the number of rows in the data file.
  // <group>
  virtual void open (uInt nrrow, AipsIO&);   //# should never be called
  virtual uInt open1 (uInt nrrow, AipsIO&);
  // </group>
  
  // Resync the storage manager with the new file contents.
  // It does nothing.
  virtual void resync (uInt nrrow);
  
  // Reopen the storage manager files for read/write.
  // It does nothing.
  virtual void reopenRW();
  
  // The data manager will be deleted (because all its columns are
  // requested to be deleted).
  // So clean up the things needed (e.g. delete files).
  virtual void deleteManager();

  // Add rows to the storage manager.
  // It cannot do it, so throws an exception.
  virtual void addRow (uInt nrrow);
  
  // Delete a row from all columns.
  // It cannot do it, so throws an exception.
  virtual void removeRow (uInt rowNr);
  
  // Do the final addition of a column.
  // It cannot do it, so throws an exception.
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
  // Create a direct array column.
  virtual DataManagerColumn* makeDirArrColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // Create an indirect array column.
  virtual DataManagerColumn* makeIndArrColumn (const String& aName,
					       int aDataType,
					       const String& aDataTypeID);
  // </group>

  // Initialize by reading the header info.
  void init();


  //# Declare member variables.
  // Name of data manager.
  String     itsDataManName;
  // The number of rows in the columns.
  uInt       itsNrRows;
  // The baseline antennas.
  Block<Int> itsAnt1;
  Block<Int> itsAnt2;
  // The start time and interval.
  Double itsStartTime;
  Double itsTimeIntv;
  uInt   itsNChan;
  uInt   itsNPol;
  // The column objects.
  vector<LofarColumn*> itsColumns;
  // The data file.
  MMapIO* itsFile;
  Bool    itsDoSwap;       //# True = byte-swapping is needed
  Int64   itsBlockSize;    //# size of a block containing a seqnr
  Int64   itsBLDataSize;   //# data size of a single baseline
  //# Buffer to hold swapped nsample values.
  Block<uShort> itsNSampleBuf;
  Double  itsMaxNrSample;  //# weight = nsample / itsMaxNrSample;
};


} //# NAMESPACE CASA - END

#endif
