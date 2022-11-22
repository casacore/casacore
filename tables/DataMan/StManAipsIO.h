//# StManAipsIO.h: Storage manager for tables using AipsIO
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2001
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

#ifndef TABLES_STMANAIPSIO_H
#define TABLES_STMANAIPSIO_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward clarations
class AipsIO;
class StManAipsIO;
class StManArrayFile;


// <summary>
// AipsIO table column storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManagerColumn
// </prerequisite>

// <etymology>
// StManColumnAipsIO handles a column for an AipsIO storage manager.
// </etymology>

// <synopsis> 
// StManColumnAipsIO is used by StManAipsIO to handle the access to
// the data in a table column.
// It is an storage manager based on AipsIO. The entire column is
// kept in memory and only written when the storage manager closes.
// When the storage manager gets opened, the entire column gets
// read back.
// It fully supports addition and removal of rows.
//
// StManColumnAipsIO serves 2 purposes:
// <ol>
// <li> It handles a column containing scalar values.
// <li> It serves as a base class for StManArrayColumnAipsIO and
//        StManIndArrayColumnAipsIO. These classes handle arrays and
//        use StManColumnAipsIO to hold a pointer to the array in each row.
// </ol>
//
// StManColumnAipsIO does not hold a column as a consecutive array,
// because extending the column (i.e. adding rows) proofed be too
// expensive due to the repeated copying involved when creating a table
// (this method was used by the old table system).
// Instead it has a number of data blocks (extensions) indexed to by a
// super block. Accessing a row means finding the appropriate extension
// via a binary search. Because there is only 1 extension when a table is
// read back, the overhead in finding a row is small.
// </synopsis> 

// <motivation>
// StManColumnAipsIO handles the standard data types. The class
// is not templated, but a switch statement is used instead.
// Templates would cause too many instantiations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManColumnAipsIO : public MSMColumn
{
public:

    // Create a column of the given type.
    // It will maintain a pointer to its parent storage manager.
    StManColumnAipsIO (StManAipsIO* stMan, int dataType, Bool byPtr);

    // Frees up the storage.
    virtual ~StManColumnAipsIO();

    // Write the column data into AipsIO.
    // It will successively write all extensions using putData.
    virtual void putFile (rownr_t nrval, AipsIO&);

    // Read the column data from AipsIO.
    // One extension gets allocated to hold all rows in the column.
    virtual void getFile (rownr_t nrval, AipsIO&);

protected:
    // initData does not do anything (only used in MSMColumn).
    virtual void initData (void* datap, rownr_t nrval);

    // Put the data (nrval elements) in an extension (starting at datap)
    // into AipsIO.
    virtual void putData (void* datap, uInt nrval, AipsIO&);

    // Get data (nrval elements) into an extension (starting at datap
    // plus the given index).
    virtual void getData (void* datap, uInt index, uInt nrval, AipsIO&,
			  uInt version);

private:
    // Forbid copy constructor.
    StManColumnAipsIO (const StManColumnAipsIO&);

    // Forbid assignment.
    StManColumnAipsIO& operator= (const StManColumnAipsIO&);
};




// <summary>
// AipsIO table storage manager class
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManager
//   <li> StManColumnAipsIO
// </prerequisite>

// <etymology>
// StManAipsIO is the storage manager using AipsIO.
// </etymology>

// <synopsis> 
// StManAipsIO is a table storage manager based on AipsIO.
// It holds the data in the columns in memory and writes them to
// a file when the table gets closed. Only the data of indirect arrays
// are directly read/written from/to a file.
// It contains pointers to the underlying StManColumnAipsIO objects,
// which do the actual data handling.
//
// The AipsIO storage manager does fully support addition and removal
// of rows and columns.
//
// All data, except indirect columns, for this storage manager are kept
// in one file. The file name is the table name appended with
// .N_AipsIO, where N is the (unique) storage manager sequence number.
// Each column containing indirect arrays is stored in a separate file
// using class StManIndArrayColumnAipsIO. The name of such a file is
// the storage manager file name appended with _cM, where M is a unique
// column sequence number acquired using function uniqueNr().
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManAipsIO : public MSMBase
{
public:

    // Create an AipsIO storage manager.
    // Its name will be blank.
    StManAipsIO();

    // Create an AipsIO storage manager with the given name.
    // Its name can be used later in e.g. Table::addColumn to
    // add a column to this storage manager.
    // <br> Note that the 2nd constructor is needed for table creation
    // from a record specification.
    // <group>
    StManAipsIO (const String& storageManagerName);
    StManAipsIO (const String& storageManagerName, const Record&);
    // </group>

    virtual ~StManAipsIO();

    // Clone this object.
    // It does not clone StManAipsIOColumn objects possibly used.
    virtual DataManager* clone() const;

    // Get the type name of the data manager (i.e. StManAipsIO).
    virtual String dataManagerType() const;

    // Get a unique column number for the column
    // (it is only unique for this storage manager).
    // This is used by StManIndArrayColumnAipsIO to create a unique file name.
    uInt uniqueNr()
	{ return uniqnr_p++; }

    // Make the object from the string.
    // This function gets registered in the DataManager "constructor" map.
    static DataManager* makeObject (const String& dataManagerType,
				    const Record& spec);

    // Open (if needed) the file for indirect arrays with the given mode.
    // Return a pointer to the object.
    StManArrayFile* openArrayFile (ByteIO::OpenOption opt);


private:
    // Forbid copy constructor.
    StManAipsIO (const StManAipsIO&);

    // Forbid assignment.
    StManAipsIO& operator= (const StManAipsIO&);

    // Flush and optionally fsync the data.
    // It returns a True status if it had to flush (i.e. if data have changed).
    virtual Bool flush (AipsIO&, Bool fsync);

    // Let the storage manager create files as needed for a new table.
    // This allows a column with an indirect array to create its file.
    virtual void create64 (rownr_t nrrow);

    // Open the storage manager file for an existing table and read in
    // the data and let the StManColumnAipsIO objects read their data.
    virtual rownr_t open64 (rownr_t nrrow, AipsIO&);

    // Resync the storage manager with the new file contents.
    // This is done by clearing the cache.
    virtual rownr_t resync64 (rownr_t nrrow);

    // Reopen the storage manager files for read/write.
    virtual void reopenRW();

    // The data manager will be deleted (because all its columns are
    // requested to be deleted).
    // So clean up the things needed (e.g. delete files).
    virtual void deleteManager();

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


    // Unique nr for column in this storage manager.
    uInt uniqnr_p;
    // The file containing the indirect arrays.
    StManArrayFile* iosfile_p;
};




} //# NAMESPACE CASACORE - END

#endif
