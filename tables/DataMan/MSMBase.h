//# MSMBase.h: Base class for storage manager for tables using memory
//# Copyright (C) 2003
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

#ifndef TABLES_MSMBASE_H
#define TABLES_MSMBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MSMColumn;


// <summary>
// Base class for memory-based table storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=DataManager>DataManager</linkto>
// </prerequisite>

// <etymology>
// MSMBase is the base class for MemoryStMan.
// </etymology>

// <synopsis> 
// See class <linkto class=MemoryStMan>MemoryStMan</linkto> for
// the description.
// </synopsis> 


class MSMBase : public DataManager
{
public:

  // Create a memory storage manager.
  // Its name will be blank.
  MSMBase();

  // Create a memory storage manager with the given name.
  // Its name can be used later in e.g. Table::addColumn to
  // add a column to this storage manager.
  // <br> Note that the 2nd constructor is needed for table creation
  // from a record specification.
  // <group>
  MSMBase (const String& storageManagerName);
  MSMBase (const String& storageManagerName, const Record&);
  // </group>

  ~MSMBase();

  // Clone this object.
  // It does not clone MSMColumn objects possibly used.
  DataManager* clone() const;

  // Get the type name of the data manager (i.e. MemoryStMan).
  String dataManagerType() const;

  // Get the name given to this storage manager.
  String dataManagerName() const;

  // Get the nr of rows in this storage manager.
  uInt nrow() const
    { return nrrow_p; }

  // Does the storage manager allow to add rows? (yes)
  Bool canAddRow() const;

  // Does the storage manager allow to delete rows? (yes)
  Bool canRemoveRow() const;

  // Does the storage manager allow to add columns? (yes)
  Bool canAddColumn() const;

  // Does the storage manager allow to delete columns? (yes)
  Bool canRemoveColumn() const;

  // Make the object from the string.
  // This function gets registered in the DataManager "constructor" map.
  static DataManager* makeObject (const String& dataManagerType,
				  const Record& spec);


private:
  // Flush and optionally fsync the data.
  // It does not done anything and always returns a False status.
  virtual Bool flush (AipsIO&, Bool fsync);

  // Let the storage manager create the nr of rows needed.
  virtual void create (uInt nrrow);

  // Open the storage manager file for an existing table.
  // It fills the rows with 0 values.
  virtual void open (uInt nrrow, AipsIO&);

  // Let the data manager initialize itself further.
  // It creates nr of rows (given to create) if needed.
  // Note this is done after reallocateColumn.
  virtual void prepare();

  // Resync the storage manager with the new file contents.
  // It adds or removes rows as needed.
  // It cannot know which rows are deleted, so it always deletes
  // the last rows.
  virtual void resync (uInt nrrow);

  // The data manager will be deleted (because all its columns are
  // requested to be deleted).
  // It does not have to do anything.
  virtual void deleteManager();

  // Add rows to all columns.
  void addRow (uInt nrrow);

  // Delete a row from all columns.
  void removeRow (uInt rownr);

  // Create a column in the storage manager on behalf of a table column.
  // <group>
  // Create a scalar column.
  DataManagerColumn* makeScalarColumn (const String& name, int dataType,
				       const String& dataTypeID);
  // Create a direct array column.
  DataManagerColumn* makeDirArrColumn (const String& name, int dataType,
				       const String& dataTypeID);
  // Create an indirect array column.
  DataManagerColumn* makeIndArrColumn (const String& name, int dataType,
				       const String& dataTypeID);
  // </group>

  // The MemoryStMan wants to do reallocateColumn.
  Bool canReallocateColumns() const;

  // Reallocate the column object if it is part of this data manager.
  // It returns a pointer to the new column object.
  // It is used to replace an MSMIndColumn object for indirect array with
  // a fixed shape by an MSMDirColumn object.
  DataManagerColumn* reallocateColumn (DataManagerColumn* column);

  // Add a column.
  void addColumn (DataManagerColumn*);

  // Delete a column.
  void removeColumn (DataManagerColumn*);


  // Name given by user to this storage manager.
  String stmanName_p;
  // The number of rows in the columns.
  uInt   nrrow_p;
  // The number of rows in create().
  uInt   nrrowCreate_p;
  // The assembly of all columns.
  PtrBlock<MSMColumn*> colSet_p;
};




} //# NAMESPACE CASACORE - END

#endif
