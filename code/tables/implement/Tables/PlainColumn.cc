//# PlainColumn.cc: Base class for a column in a plain table
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/Tables/PlainColumn.h>
#include <aips/Tables/ColumnSet.h>
#include <aips/Tables/BaseColDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/DataManager.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/IO/AipsIO.h>
#include <aips/Tables/TableError.h>


PlainColumn::PlainColumn (const BaseColumnDesc* cdp, ColumnSet* csp)
: BaseColumn    (cdp),
  dataManPtr_p  (0),
  dataColPtr_p  (0),
  colSetPtr_p   (csp),
  originalName_p(cdp->name())
{}

PlainColumn::~PlainColumn()
{}


uInt PlainColumn:: nrow() const
    { return colSetPtr_p->nrow(); }


TableRecord& PlainColumn::rwKeywordSet()
{
    colSetPtr_p->checkLock (FileLocker::Write, True);
    colSetPtr_p->setTableChanged();
    return colDesc_p.rwKeywordSet(); 
}
TableRecord& PlainColumn::keywordSet()
    { return colDesc_p.rwKeywordSet(); }


//# By default defining the array shape is invalid.
void PlainColumn::setShapeColumn (const IPosition&)
    { throw (TableInvOper ("setShapeColumn not allowed for column " +
			   columnDesc().name())); }


Bool PlainColumn::isBound() const
    { return (dataManPtr_p == 0  ?  False : True); }
void PlainColumn::bind (DataManager* dataManPtr)
    { dataManPtr_p = dataManPtr; }

Bool PlainColumn::isWritable() const
    { return dataColPtr_p->isWritable(); }

Bool PlainColumn::isStored() const
    { return dataManPtr_p->isStorageManager(); }

ColumnCache& PlainColumn::columnCache()
    { return dataColPtr_p->columnCache(); }

void PlainColumn::setMaximumCacheSize (uInt nbytes)
    { dataManPtr_p->setMaximumCacheSize (nbytes); }


//# Read/write the column.
//# Its data will be read/written by the appropriate storage manager.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void PlainColumn::putFile (AipsIO& ios, const String&)
{
    ios << (uInt)2;                  // class version 2
    ios << originalName_p;
    putFileDerived (ios);
}
void PlainColumn::getFile (AipsIO& ios, const ColumnSet& colset,
			   Bool tableIsWritable, const String& tableName)
{
    uInt version;
    ios >> version;
    // In the older Table files the keyword set was written separately
    // and was not part of the TableDesc.
    // So read it for those and merge it into the TableDesc keywords.
    if (version == 1) {
	TableRecord tmp;
	tmp.getRecord (ios, tableIsWritable, tableName);
	keywordSet().merge (tmp, RecordInterface::OverwriteDuplicates);
    }
    ios >> originalName_p;
    getFileDerived (ios, colset);
}


void PlainColumn::checkValueLength (const String* value) const
{
    uInt maxlen = columnDesc().maxLength();
    if (maxlen > 0  &&  value->length() > maxlen) {
	throw (TableError ("ScalarColumn::put: string value '" +
			   *value + "' exceeds maximum length"));
    }
}
void PlainColumn::checkValueLength (const Array<String>* value) const
{
    uInt maxlen = columnDesc().maxLength();
    if (maxlen == 0) {
	return;
    }
    ReadOnlyArrayIterator<String> iter (*value, 1);
    while (! iter.pastEnd()) {
	Vector<String> vec (iter.array());
	for (uInt i=0; i<vec.nelements(); i++) {
	    if (vec(i).length() > maxlen) {
		throw (TableError ("Scalar/ArrayColumn::put: string value '" +
				   vec(i) + "' exceeds maximum length"));
	    }
	}
	iter.next();
    }
}
