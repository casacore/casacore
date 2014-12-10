//# ColDescSet.h: This class defines a set of column descriptions
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#ifndef TABLES_COLDESCSET_H
#define TABLES_COLDESCSET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Set of table column descriptions
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableDesc
//   <li> BaseColumnDesc
//   <li> Keyword module
// </prerequisite>

// <etymology>
// ColumnDescSet is the set of column descriptions in a table description.
// </etymology>

// <synopsis> 
// ColumnDescSet is used by
// <linkto class="TableDesc:description">TableDesc</linkto>
// to store all column descriptions.
//
// In principle this class is only used internally by the table system.
// However, there is a function in TableDesc which gives const access
// to this class. This can be used by the user to call functions
// like isDisjoint.
// </synopsis> 

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class ColumnDescSet
{
friend class TableDesc;

public:
    // Construct an empty column set.
    ColumnDescSet();

    // Copy constructor (copy semantics).
    ColumnDescSet (const ColumnDescSet&);

    ~ColumnDescSet();

    // Assignment (copy semantics).
    ColumnDescSet& operator= (const ColumnDescSet&);

    // Get a column by its name.
    // <group>
    ColumnDesc& operator[] (const String& name);
    const ColumnDesc& operator[] (const String& name) const
	{ return (*(ColumnDescSet*)this)[name]; }
    // </group>

    // Get a column by its index.
    // <group>
    ColumnDesc& operator[] (uInt index)
	{ return *(ColumnDesc*)(colSeq_p[index]); }
    const ColumnDesc& operator[] (uInt index) const
	{ return *(ColumnDesc*)(colSeq_p[index]); }
    // </group>

    // Get nr of columns in this set.
    uInt ncolumn() const
	{ return cols_p.ndefined(); }

    // Test if a column is defined in this set.
    Bool isDefined (const String& name) const
	{ return  (cols_p.isDefined (name)); }

    // Test if this set equals another one.
    // It is equal if the number of columns is equal and all field names in
    // this set occur in the other too. The order of the columns
    // is not important.
    // <br>The flag equalDataTypes is set to True if the data types
    // of all columns match.
    Bool isEqual (const ColumnDescSet& other, Bool& equalDataTypes) const;

    // Test if this set is a subset of another one.
    // It is similar to isEqual above.
    Bool isSubset (const ColumnDescSet& other, Bool& equalDataTypes) const;

    // Test if this set is a strict subset of another one, thus
    // if it is a subset and not equal.
    Bool isStrictSubset (const ColumnDescSet& other,
			 Bool& equalDataTypes) const;

    // Test if this set is a superset of another one.
    Bool isSuperset (const ColumnDescSet& other, Bool& equalDataTypes) const
	{ return other.isSubset (*this, equalDataTypes); }

    // Test if this set is a strict superset of another one, thus
    // if it is a superset and not equal.
    Bool isStrictSuperset (const ColumnDescSet& other,
			   Bool& equalDataTypes) const
	{ return other.isStrictSubset (*this, equalDataTypes); }

    // Test if this and the other column set are disjoint.
    Bool isDisjoint (const ColumnDescSet& other) const;

    // Get const access to the column descriptions.
//#//    const TypedKeywords<ColumnDesc>& columns() const
//#//	{ return cols_p; }

    // Show the columns in the set.
    void show (ostream& os) const;

    // Check recursevily if the descriptions of all subtables are known.
    void checkSubTableDesc() const;

private:
    // Add a column.
    // An exception is thrown if a column with this name already exists.
    ColumnDesc& addColumn (const ColumnDesc&);

    // Add a column with another name.
    // An exception is thrown if a column with this name already exists.
    ColumnDesc& addColumn (const ColumnDesc&, const String& newname);

    // Remove a column.
    // An exception is thrown if the column with this name does not exist.
    void remove (const String& name);

    // Rename a column in the set.
    // An exception is thrown if the new name already exists or if
    // the old name does not exist.
    void rename (const String& newname, const String& oldname);

    // Test if all columns are part of the other set.
    // The flag equalDataTypes is set to True if the data types of the
    // columns in both sets are the same.
    Bool allExist (const ColumnDescSet&, Bool& equalDataTypes) const;

    // Add another (disjoint) column set.
    // If the sets are not disjoint (i.e. the other set contains a column
    // with an already existing name, an exception is thrown and nothing
    // of the other set is added.
    void add (const ColumnDescSet& set);

    // Put the object.
    void putFile (AipsIO& ios, const TableAttr&) const;

    // Get the object
    void getFile (AipsIO&, const TableAttr&);


    // The set of all columns.
    SimpleOrderedMap<String,ColumnDesc> cols_p;
    // The order of addition of column descriptions.
    //# This is in fact a Block<ColumnDesc*>, but a void* is used
    //# to reduce the number of template instantiations.
    Block<void*> colSeq_p;
};




} //# NAMESPACE CASACORE - END

#endif
