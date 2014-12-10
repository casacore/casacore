//# TableAttr.h: Some attributes of a table
//# Copyright (C) 2001
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

#ifndef TABLES_TABLEATTR_H
#define TABLES_TABLEATTR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/TableLock.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Table;


// <summary>
// Some attributes of a table.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tTableRecord">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableRecord>TableRecord</linkto>
//   <li> <linkto class=Table>Table</linkto>
// </prerequisite>

// <synopsis>
// This class holds some attributes of a table.
// These attributes are name, readable/writable, and lock options.
// <br>
// The primary use of the class is to be able to pass the various
// attributes of the parent table to its subtables (e.g. in classes
// like <linkto class=TableRecord>TableRecord</linkto> and
// <linkto class=TableKeyword>TableKeyword</linkto>).
// </synopsis>

// <motivation>
// This class makes it possible to have more attributes without
// having to alter the classes using subtables.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableAttr
{
public:
    // Default constructor results in empty name.
    TableAttr();

    // Construct the attributes from the table.
    explicit TableAttr (const Table& table);

    // Construct with given values.
    // <group>
    explicit TableAttr (const String& name, Bool openWritable = False);
    TableAttr (const String& name, Bool openWritable, const TableLock&);
    // </group>

    // Copy constructor (copy semantics).
    TableAttr (const TableAttr& that);

    // Assignment (copy semantics).
    // <group>
    TableAttr& operator= (const TableAttr& that);
    TableAttr& operator= (const Table& table);
    // </group>

    ~TableAttr();

    // Set the object to for another table.
    void set (const Table& table);

    // Set the keyword to read/write access.
    void setRW()
      { openWritable_p = True; }

    void setName (const String& name)
      { name_p = name; }

    // Get info.
    // <group>
    const String& name() const
      { return name_p; }
    Bool openWritable() const
      { return openWritable_p; }
    const TableLock& lockOptions() const
      { return lockOptions_p; }
    // </group>

private:
    String    name_p;
    Bool      openWritable_p;
    TableLock lockOptions_p;
};



} //# NAMESPACE CASACORE - END

#endif
