//# SetupNewTab.cc: Class to construct a new or scratch table
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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

#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnSet.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/PlainColumn.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/File.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SetupNewTable::SetupNewTable (const String& tableName,
			      const String& tableDescName,
			      Table::TableOption opt,
                              const StorageOption& storageOpt)
{
    newTable_p = new SetupNewTableRep (tableName, tableDescName,
                                       opt, storageOpt);
}

SetupNewTable::SetupNewTable (const String& tableName,
			      const TableDesc& tableDesc,
			      Table::TableOption opt,
                              const StorageOption& storageOpt)
{
    newTable_p = new SetupNewTableRep (tableName, tableDesc,
                                       opt, storageOpt);
}

SetupNewTable::SetupNewTable (const SetupNewTable& that)
: newTable_p(0)
{
    operator= (that);
}

SetupNewTable::~SetupNewTable()
{
    if (newTable_p != 0) {
	if (--(newTable_p->count()) == 0) {
	    delete newTable_p;
	}
    }
}

SetupNewTable& SetupNewTable::operator= (const SetupNewTable& that)
{
    if (newTable_p != 0) {
	if (--(newTable_p->count()) == 0) {
	    delete newTable_p;
	}
    }
    newTable_p = that.newTable_p;
    if (newTable_p != 0) {
	++(newTable_p->count());
    }
    return *this;
}

SetupNewTableRep::SetupNewTableRep (const String& tableName,
				    const String& tableDescName,
				    Table::TableOption opt,
                                    const StorageOption& storageOpt)
: count_p     (1),
  tabName_p   (tableName),
  option_p    (opt),
  storageOpt_p(storageOpt),
  delete_p    (False),
  tdescPtr_p  (0),
  colSetPtr_p (0),
  dataManMap_p(static_cast<void*>(0))
{
    //# Copy the table description.
    tdescPtr_p = new TableDesc(tableDescName);
    //# Setup the new table.
    setup();
}

SetupNewTableRep::SetupNewTableRep (const String& tableName,
				    const TableDesc& tableDesc,
				    Table::TableOption opt,
                                    const StorageOption& storageOpt)
: count_p     (1),
  tabName_p   (tableName),
  option_p    (opt),
  storageOpt_p(storageOpt),
  delete_p    (False),
  tdescPtr_p  (0),
  colSetPtr_p (0),
  dataManMap_p(static_cast<void*>(0))
{
    //# Read the table description.
    tdescPtr_p = new TableDesc(tableDesc, "", "", TableDesc::Scratch);
    //# Setup the new table.
    setup();
}

SetupNewTableRep::~SetupNewTableRep()
{
    //# When the object is in use, the ColumnSet and table
    //# description pointers are taken over by the PlainTable object.
    //# So only delete them if not in use.
    if (! isUsed()) {
	delete tdescPtr_p;
	delete colSetPtr_p;
    }
}


void SetupNewTableRep::setup()
{
    //# If no name is given, create a unique name.
    if (tabName_p.empty()) {
	tabName_p = File::newUniqueName ("", "tab").originalName();
    }
    //# A scratch table is new, but marked for delete.
    if (option_p == Table::Scratch) {
	option_p = Table::New;
	delete_p = True;
    }
    //# Check the table option.
    //# Check if the table exists and can be overwritten if new.
    if (option_p == Table::NewNoReplace) {
	File file(tabName_p);
	if (file.exists()) {
	    throw (TableDuplFile(tabName_p));     // table file already exists
	}
    }else{
	if (option_p != Table::New) {
	    throw (TableInvOpt ("SetupNewTable",
			    "must be Table::New, NewNoReplace or Scratch"));
	}
    }
    // Complete the storage option.
    storageOpt_p.fillOption();
    //# Check if all subtable descriptions exist.
    tdescPtr_p->checkSubTableDesc();
    //# Create a column set.
    colSetPtr_p = new ColumnSet(tdescPtr_p, storageOpt_p);
}


DataManager* SetupNewTableRep::getDataManager (const DataManager& dataMan)
{
    //# Clone if this DataManager has not been cloned yet.
    //# The map maintains a mapping of an original DataManager object
    //# and its clone.
    //# However, it is possible that the original was a temporary and
    //# that another original is allocated at the same address.
    //# So also test if the original is indeed cloned.
    DataManager* dmp = (DataManager*) (dataManMap_p((void*)&dataMan));
    if (dmp == 0  ||  dataMan.getClone() == 0) {
	//# Not cloned yet, so clone it.
	//# Add it to the map in the ColumnSet object.
	//# Tell the original object that it has been cloned.
	dmp = dataMan.clone();
	colSetPtr_p->addDataManager (dmp);
	dataManMap_p((void*)&dataMan) = dmp;
	dataMan.setClone (dmp);
    }
    return dmp;
}

void SetupNewTableRep::bindCreate (const Record& spec)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	       ("SetupNewTable::bindCreate, object already used by Table"));
    }
    for (uInt i=0; i<spec.nfields(); i++) {
        const Record& rec = spec.subRecord(i);
	if (rec.isDefined("TYPE")  &&  rec.isDefined("NAME")
	&&  rec.isDefined("SPEC")  &&  rec.isDefined("COLUMNS")) {
	    String dmType = rec.asString ("TYPE");
	    String dmGroup = rec.asString ("NAME");
	    const Record& sp = rec.subRecord ("SPEC");;
	    Vector<String> cols (rec.asArrayString ("COLUMNS"));
	    DataManager* dataMan = DataManager::getCtor(dmType) (dmGroup, sp);
	    // Bind the columns to this data manager.
	    for (uInt j=0; j<cols.nelements(); j++) {
	        bindColumn (cols(j), *dataMan);
	    }
	    delete dataMan;
	}
    }
}


void SetupNewTableRep::bindAll (const DataManager& dataMan, Bool rebind)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	       ("SetupNewTable::bindAll, object already used by Table"));
    }
    //# Add DataManager object if not used yet.
    DataManager* dataManPtr = getDataManager (dataMan);
    //# Loop through all columns and bind them.
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	PlainColumn* col = colSetPtr_p->getColumn(i);
	if (rebind  ||  !col->isBound()) {
	    //# Great, bind the data manager to the column.
	    col->bind (dataManPtr);
	}
    }
}

void SetupNewTableRep::bindGroup (const String& groupName,
				  const DataManager& dataMan, Bool rebind)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	       ("SetupNewTable::bindGroup, object already used by Table"));
    }
    //#  Add DataManager object if not used yet.
    DataManager* dataManPtr = getDataManager (dataMan);
    //# Loop through all columns and bind those matching the group name.
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	PlainColumn* col = colSetPtr_p->getColumn(i);
	const ColumnDesc& cd = col->columnDesc();
	if (cd.dataManagerGroup() == groupName) {
	    if (rebind  ||  !col->isBound()) {
		//# Great, bind the column to the data manager.
		col->bind (dataManPtr);
	    }
	}
    }
}

void SetupNewTableRep::bindColumn (const String& columnName,
				   const DataManager& dataMan)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	       ("SetupNewTable::bindColumn, object already used by Table"));
    }
    //# Add DataManager object if not used yet.
    DataManager* dataManPtr = getDataManager (dataMan);
    //# Bind data manager to the given column.
    //# Rebind if already bound.
    PlainColumn* col = colSetPtr_p->getColumn(columnName);
    col->bind (dataManPtr);
}

void SetupNewTableRep::bindColumn (const String& columnName,
				   const String& otherColumn)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	       ("SetupNewTable::bindColumn, object already used by Table"));
    }
    //# Bind if other column has a data manager.
    //# Rebind if already bound.
    PlainColumn* col = colSetPtr_p->getColumn(columnName);
    PlainColumn* ocol = colSetPtr_p->getColumn(otherColumn);
    if (ocol->isBound()) {
	col->bind (ocol->dataManager());
    }
}


void SetupNewTableRep::handleUnbound()
{
    //# Loop through all columns and find unbound columns.
    for (uInt i=0; i<tdescPtr_p->ncolumn(); i++) {
	PlainColumn* col = colSetPtr_p->getColumn(i);
	const ColumnDesc& coldes = col->columnDesc();
	if (!col->isBound()) {
	    //# Create a data manager object for this column.
	    //# Do this by executing the appropriate "constructor"
	    //# in the static DataManager map.
	    //# Clone and add DataManager object.
	    String dmType = coldes.dataManagerType();
	    String dmGroup = coldes.dataManagerGroup();
	    DataManager* dataMan = DataManager::getCtor(dmType) (dmGroup,
								 Record());
	    DataManager* dataManPtr = getDataManager (*dataMan);
	    delete dataMan;
	    //# Bind the column.
	    col->bind (dataManPtr);
	    //# Bind this data manager to all other unbound columns with
	    //# the same group and default data manager type name.
		for (uInt j=i+1; j<tdescPtr_p->ncolumn(); j++) {
		    PlainColumn* cp = colSetPtr_p->getColumn(j);
		    const ColumnDesc& cd = cp->columnDesc();
		    if (!cp->isBound()
		    &&  cd.dataManagerGroup() == coldes.dataManagerGroup()
		    &&  cd.dataManagerType()  == coldes.dataManagerType()) {
			//# Great, bind the column to the data manager.
			cp->bind (dataManPtr);
		}
	    }
	}
    }
}


void SetupNewTableRep::setShapeColumn (const String& columnName,
				       const IPosition& shape)
{
    //# Test if object is already in use for a table.
    if (isUsed()) {
	throw (TableInvOper
	      ("SetupNewTable::setShapeColumn, object already used by Table"));
    }
    PlainColumn* col = colSetPtr_p->getColumn(columnName);
    if (! (col->columnDesc().isFixedShape())) {
	throw (TableInvOper ("SetupNewTable::setShapeColumn, column " +
			     columnName + " is not fixed shape"));
    }
    col->setShapeColumn (shape);
}

} //# NAMESPACE CASACORE - END

