//# VirtColEng.h: Abstract base class for virtual column handling
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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

#ifndef TABLES_VIRTCOLENG_H
#define TABLES_VIRTCOLENG_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Abstract base class for virtual column handling
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManager
//   <li> Table
// </prerequisite>

// <etymology>
// VirtualColumnEngine is the abstract data manager class for specialized
// classes (engines) handling a group of virtual columns.
// </etymology>

// <synopsis> 
// VirtualColumnEngine is the data manager for classes handling
// a group of virtual columns in tables. It is an abstract base class
// for the specialized virtual column engines.
// Each virtual column as such is represented by a class which has
// to be derived from the abstract base classes VirtualScalarColumn
// or VirtualArrayColumn. The engine has to create the various
// column objects via the functions makeXXXColumn.
//
// Initialization of the virtual column engine is done by the
// functions create (for new tables), open (for existing tables) and prepare.
// The engine can be flushed by the function flush, which allows to
// write some data. The function open can read these data back.
// VirtualColumnEngine is closely related with the table system.
//
// A number of (pure) virtual functions have been defined. The pure
// virtual functions must be implemented in the derived class.
// The non-pure virtual functions have a default implementation throwing
// a "not possible" exception. They need to be implemented if they
// are used for this engine (e.g. makeIndArrColumn does not need to
// be implemented if the engine does not handle arrays).
// Furthermore the pure virtual function dataManagerType (defined in
// DataManager.h) has to be implemented. This should return the name
// of the data manager, which is usually its class name. This name
// has to be unique; so if the engine is templated, the template
// parameter has to be part of the data manager name.
//
// The engine has to be registered before it can be used by the table system.
// This means that a special makeObject function has to be made
// known to the table system, which allows the table system to
// reconstruct the engine using its name.
//
// An example of a virtual column engine can be found in dVirtColEng.{h,cc}
// in the test directory of the Tables module.
// Another exanple is class ScaledArray.
// </synopsis> 

// <motivation>
// It is nice if a table column can be expressed as a function
// of other columns (maybe even in other tables). A virtual column
// provides this functionality in a very flexible way.
// A specialized class can calculate the data of a virtual column,
// but a common base class is required to interface it to the
// table system.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class VirtualColumnEngine : public DataManager
{
public:

    // Create the object.
    VirtualColumnEngine()
        {};

    virtual ~VirtualColumnEngine();

private:
    // The copy constructor cannot be used for this base class.
    // The clone function should be used instead.
    // The private declaration of this constructor makes it unusable.
    VirtualColumnEngine (const VirtualColumnEngine& that);

    // Assignment cannot be used for this base class.
    // The private declaration of this operator makes it unusable.
    VirtualColumnEngine& operator= (const VirtualColumnEngine&);

    // The data manager is not a storage manager?
    virtual Bool isStorageManager() const;

    // Does the data manager allow to add rows? (default no)
    virtual Bool canAddRow() const;

    // Does the data manager allow to delete rows? (default no)
    virtual Bool canRemoveRow() const;

    // Add rows to all columns.
    // The default implementation does nothing.
    virtual void addRow (uInt nrrow);

    // Delete a row from all columns.
    // The default implementation does nothing.
    virtual void removeRow (uInt rownr);

    // Flush the data in the engine object.
    // If the object contains persistent data, this is the place to write them.
    // This can be done in two ways:
    // <ul>
    // <li>
    // They can be written in the main table file (using the AipsIO argument).
    // This should preferably be used if the object contains only little data.
    // <li>
    // They can be written in a file of its own. A unique filename
    // can be acquired using DataManager::fileName().
    // This way is preferred when the object contains a lot of data.
    // Possibly this file could already be created in function create
    // and only be flushed and closed in this function. This allows
    // getting and putting of data as needed.
    // </ul>
    // Another way of storing information is by storing it as a keyword
    // in the table. In this case it is important to know that close
    // is called AFTER the keywords are written. Thus, in this way the
    // information has to be stored and read back in create, open and/or
    // prepare.
    // It returns a True status if it had to flush (i.e. if data have changed).
    // <br>The default implementation does nothing and returns False.
    virtual Bool flush (AipsIO&, Bool fsync);

    // Resync the storage manager with the new file contents.
    // This is done by clearing the cache.
    // The default implementation does nothing.
    virtual void resync (uInt nrrow);

    // Initialize the object for a new table containing initially nrrow rows.
    // It can be used to initialize variables (possibly using data
    // from other columns in the table).
    // The default implementation does nothing.
    virtual void create (uInt initialNrrow);

    // Initialize the object for an existing table containing nrrow rows.
    // It can be used to read values back (written by close) and/or
    // to initialize variables (possibly using data from other columns
    // in the table).
    // The default implementation does nothing.
    virtual void open (uInt nrrow, AipsIO& mainTableFile);

    // Let the data manager initialize itself further.
    // Prepare is called after create/open has been called for all
    // columns. In this way one can be sure that referenced columns
    // are read back and partly initialized.
    // The default implementation does nothing.
    virtual void prepare();

    // The data manager will be deleted (because all its columns are
    // requested to be deleted).
    // So clean up the things needed (e.g. delete files).
    // By default it assumes that nothing has to be done.
    virtual void deleteManager();

    // Make a column object in the engine on behalf of a table column.
    // This column object class is derived from VirtualScalarColumn
    // or VirtualArrayColumn. It handles the gets and puts of data.
    // <group>
    // Create a scalar column.
    // The default implementation throws an exception that it cannot
    // do it for this column.
    virtual DataManagerColumn* makeScalarColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId);
    // Create a direct array column.
    // The default implementation calls makeIndArrColumn
    // (when reading the user sees no difference between direct and indirect).
    virtual DataManagerColumn* makeDirArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId);
    // Create an indirect array column.
    // The default implementation throws an exception that it cannot
    // do it for this column.
    virtual DataManagerColumn* makeIndArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId);
    // </group>
};



} //# NAMESPACE CASACORE - END

#endif



