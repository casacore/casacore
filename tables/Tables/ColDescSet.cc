//# ColDescSet.cc: This class defines a set of column descriptions
//# Copyright (C) 1994,1995,1996,1997,2000,2001
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

#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


ColumnDescSet::ColumnDescSet()
: cols_p   (ColumnDesc()),
  colSeq_p (0)
{
    //# Register the main DataManagers if not done yet.
    DataManager::registerMainCtor();
}

ColumnDescSet::ColumnDescSet (const ColumnDescSet& that)
: cols_p   (ColumnDesc()),
  colSeq_p (0)
    { operator= (that); }


ColumnDescSet::~ColumnDescSet()
{}


ColumnDescSet& ColumnDescSet::operator= (const ColumnDescSet& that)
{
    if (this != &that) {
	cols_p = that.cols_p;
	uInt nrcol = ncolumn();
	colSeq_p.resize (nrcol);
	//# Now we have to fill in the column order, which is the
	//# same as the order in the source.
	//# We have to point to our own ColumnDesc objects.
	for (uInt i=0; i<nrcol; i++) {
	    const String& colName = that[i].name();
	    colSeq_p[i] = &(cols_p(colName));
	}
    }
    return *this;
}


ColumnDesc& ColumnDescSet::operator[] (const String& name)
{
    // Throw an exception if the column is undefined.
    ColumnDesc* col = cols_p.isDefined (name);
    if (col == 0) {
	throw (TableError ("Table column " + name + " is unknown"));
    }
    return *col;
}


//# Add a column to the set with another name.
ColumnDesc& ColumnDescSet::addColumn (const ColumnDesc& cd,
				      const String& newname)
{
    //# First make a copy to be able to change the name.
    ColumnDesc coldes (cd);
    coldes.setName (newname);
    return addColumn (coldes);
}

//# Add a column to the set.
ColumnDesc& ColumnDescSet::addColumn (const ColumnDesc& cd)
{
    //# First check if the column name already exists.
    if (isDefined (cd.name())) {
	throw (TableInvColumnDesc (cd.name(), "column already exists"));
    }
    cd.checkAdd (*this);
    cols_p.define (cd.name(), cd);
    //# Get actual column description object.
    ColumnDesc& coldes = cols_p(cd.name());
    //# Add the new column to the sequence block.
    uInt nrcol = ncolumn();
    if (nrcol > colSeq_p.nelements()) {
	colSeq_p.resize (nrcol + 63);
    }
    colSeq_p[nrcol-1] = &coldes;
    coldes.handleAdd (*this);
    return coldes;
}


//# Remove a column.
//# Let the column first act upon its removal.
void ColumnDescSet::remove (const String& name)
{
    ColumnDesc& cd = (*this)[name];
    cd.handleRemove (*this);
    //# Remove it first from the sequence block.
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	if (colSeq_p[i] == &cd) {
	    for (; i<nrcol-1; i++) {
		colSeq_p[i] = colSeq_p[i+1];
	    }
	break;
	}
    }
    //# Now really remove the column.
    cols_p.remove (name);
	    
}

//# Rename a column in the set.
//# Let all columns act upon the rename (in case they refer to the old name).
void ColumnDescSet::rename (const String& newname, const String& oldname)
{
    if (! isDefined(oldname)) {
        throw (AipsError ("TableDesc::renameColumn - old name " + oldname +
			  " does not exist"));
    }
    if (isDefined(newname)) {
        throw (AipsError ("TableDesc::renameColumn - new name " + newname +
			  " already exists"));
    }
    cols_p(oldname).checkRename (*this, newname);
    cols_p.rename (newname, oldname);
    ColumnDesc& cd = cols_p(newname);
    //# Actually rename in BaseColDesc object.
    cd.setName (newname);
    //# Handle rename for other things.
    cd.handleRename (*this, oldname);
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	cols_p.getVal(i).renameAction (newname, oldname);
    }
}


//# Check recursevily if the descriptions of all subtables are known.
void ColumnDescSet::checkSubTableDesc() const
{
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	const ColumnDesc& cd = (*this)[i];
	if (cd.dataType() == TpTable) {
	    const TableDesc* tdp = cd.tableDesc();  // throws if unknown desc.
	    tdp->checkSubTableDesc();               // check recursively
	}
    }
}


Bool ColumnDescSet::isEqual (const ColumnDescSet& other,
			     Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (ncolumn() != other.ncolumn()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool ColumnDescSet::isSubset (const ColumnDescSet& other,
			      Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (ncolumn() > other.ncolumn()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool ColumnDescSet::isStrictSubset (const ColumnDescSet& other,
				    Bool& equalDataTypes) const
{
    equalDataTypes = False;
    if (ncolumn() >= other.ncolumn()) {
	return False;
    }
    return allExist (other, equalDataTypes);
}

Bool ColumnDescSet::allExist (const ColumnDescSet& other,
			      Bool& equalDataTypes) const
{
    equalDataTypes = True;
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	const ColumnDesc& thisCol = (*this)[i];
	if (! other.isDefined (thisCol.name())) {
	    return False;                     // name does not exist in other
	}
	if (thisCol.dataType() != other[thisCol.name()].dataType()) {
	    equalDataTypes = False;           // unequal data type
	}
    }
    return True;                              // names are equal
}

Bool ColumnDescSet::isDisjoint (const ColumnDescSet& other) const
{
    uInt nrcol = other.ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	if (isDefined (other[i].name())) {
	    return False;                     //# name exists in other
	}
    }
    return True;
}


//# Add another column set.
//# Duplicates are not allowed, because renaming or skipping them
//# may disturb the virtual columns referencing others.
//# Add each column in its turn.
void ColumnDescSet::add (const ColumnDescSet& set)
{
    //# First check if duplicates exist, otherwise we may end
    //# up with adding only part of the other set.
    if (! isDisjoint (set)) {
	throw (TableError ("ColumnDescSet::add; column sets not disjoint"));
    }
    uInt nrcol = set.ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	addColumn (set[i]);
    }
}


//# Put the object.
void ColumnDescSet::putFile (AipsIO& ios, const TableAttr& parentAttr) const
{
    uInt nrcol = ncolumn();
    ios << nrcol;
    for (uInt i=0; i<nrcol; i++) {
	(*this)[i].putFile (ios, parentAttr);
    }
}

//# Get the object.
void ColumnDescSet::getFile (AipsIO& ios, const TableAttr& parentAttr)
{
    //# Clear the entire set.
    *this = ColumnDescSet();
    uInt nrcol;
    ios >> nrcol;
    for (uInt i=0; i<nrcol; i++) {
	ColumnDesc coldes;
	coldes.getFile (ios, parentAttr);
	addColumn (coldes);
    }
}


void ColumnDescSet::show (ostream& os) const
{
    uInt nrcol = ncolumn();
    for (uInt i=0; i<nrcol; i++) {
	os << (*this)[i];
	os << endl;
    }
}

} //# NAMESPACE CASACORE - END

