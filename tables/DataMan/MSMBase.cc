//# MSMBase.cc: Base class for storage manager for tables using memory
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

#include <casacore/tables/DataMan/MSMBase.h>
#include <casacore/tables/DataMan/MSMColumn.h>
#include <casacore/tables/DataMan/MSMDirColumn.h>
#include <casacore/tables/DataMan/MSMIndColumn.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSMBase::MSMBase()
: DataManager   (),
  nrrow_p       (0),
  nrrowCreate_p (0),
  colSet_p      (0)
{}

MSMBase::MSMBase (const String& storageManagerName)
: DataManager   (),
  stmanName_p   (storageManagerName),
  nrrow_p       (0),
  nrrowCreate_p (0),
  colSet_p      (0)
{}

MSMBase::MSMBase (const String& storageManagerName, const Record&)
: DataManager   (),
  stmanName_p   (storageManagerName),
  nrrow_p       (0),
  nrrowCreate_p (0),
  colSet_p      (0)
{}

MSMBase::~MSMBase()
{
  for (uInt i=0; i<ncolumn(); i++) {
    delete colSet_p[i];
  }
}

DataManager* MSMBase::clone() const
{
  MSMBase* smp = new MSMBase (stmanName_p);
  return smp;
}

DataManager* MSMBase::makeObject (const String& storageManagerName,
				  const Record& spec)
{
  MSMBase* smp = new MSMBase (storageManagerName, spec);
  return smp;
}

String MSMBase::dataManagerType() const
{
  return "MemoryStMan";
}

String MSMBase::dataManagerName() const
{
  return stmanName_p;
}

//# Does the storage manager allow to add rows? (yes)
Bool MSMBase::canAddRow() const
{
  return True;
}

//# Does the storage manager allow to delete rows? (yes)
Bool MSMBase::canRemoveRow() const
{
  return True;
}

//# Does the storage manager allow to add columns? (yes)
Bool MSMBase::canAddColumn() const
{
  return True;
}

//# Does the storage manager allow to delete columns? (yes)
Bool MSMBase::canRemoveColumn() const
{
  return True;
}


DataManagerColumn* MSMBase::makeScalarColumn (const String& columnName,
					      int dataType, const String&)
{
  //# Check if data type is not TpOther.
  throwDataTypeOther (columnName, dataType);
  //# Extend colSet_p block if needed.
  if (ncolumn() >= colSet_p.nelements()) {
    colSet_p.resize (colSet_p.nelements() + 32);
  }
  MSMColumn* colp = new MSMColumn (this, dataType, False);
  colSet_p[ncolumn()] = colp;
  return colp;
}
DataManagerColumn* MSMBase::makeDirArrColumn (const String& columnName,
					      int dataType, const String&)
{
  //# Check if data type is not TpOther.
  throwDataTypeOther (columnName, dataType);
  //# Extend colSet_p block if needed.
  if (ncolumn() >= colSet_p.nelements()) {
    colSet_p.resize (colSet_p.nelements() + 32);
  }
  MSMColumn* colp = new MSMDirColumn (this, dataType);
  colSet_p[ncolumn()] = colp;
  return colp;
}
DataManagerColumn* MSMBase::makeIndArrColumn (const String& columnName,
					      int dataType, const String&)
{
  //# Check if data type is not TpOther.
  throwDataTypeOther (columnName, dataType);
  //# Extend colSet_p block if needed.
  if (ncolumn() >= colSet_p.nelements()) {
    colSet_p.resize (colSet_p.nelements() + 32);
  }
  MSMColumn* colp = new MSMIndColumn (this, dataType);
  colSet_p[ncolumn()] = colp;
  return colp;
}

Bool MSMBase::canReallocateColumns() const
{
  return True;
}

DataManagerColumn* MSMBase::reallocateColumn (DataManagerColumn* column)
{
  // Replace an indirect column by a direct one if its shape is fixed.
  for (uInt i=0; i<ncolumn(); i++) {
    if (column == colSet_p[i]) {
      MSMColumn* ptr = colSet_p[i];
      if (ptr->isFixedShape()) {
	MSMIndColumn* col = dynamic_cast<MSMIndColumn*>(ptr);
	if (col != 0) {
	  // Turn a fixed shaped indirect array into a direct array.
	  MSMDirColumn* newcol = new MSMDirColumn (this, col->dataType());
	  newcol->setShapeColumn (col->columnShape());
	  delete col;
	  colSet_p[i] = newcol;
	  column = newcol;
	}
      }
    }
  }
  return column;
}

void MSMBase::prepare()
{
  // Create the rows if needed.
  if (nrrowCreate_p > 0) {
    AlwaysAssert (nrrow_p == 0, AipsError);
    addRow (nrrowCreate_p);
    nrrowCreate_p = 0;
  }
}


// Note that the column has already been added by makeXXColumn.
// This function is merely for initializing the added column.
void MSMBase::addColumn (DataManagerColumn* colp)
{
  for (uInt i=0; i<ncolumn(); i++) {
    if (colp == colSet_p[i]) {
      colSet_p[i]->doCreate (nrrow_p);
      return;
    }
  }
  throw (DataManInternalError ("MSMBase::addColumn"));
}

void MSMBase::removeColumn (DataManagerColumn* colp)
{
  for (uInt i=0; i<ncolumn(); i++) {
    if (colSet_p[i] == colp) {
      delete colSet_p[i];
      decrementNcolumn();
      for (uInt j=i; j<ncolumn(); j++) {
	colSet_p[j] = colSet_p[j+1];
      }
      return;
    }
  }
  throw (DataManInternalError ("MSMBase::removeColumn: no such column"));
}

void MSMBase::addRow (uInt nr)
{
  //# Add the number of rows to each column.
  for (uInt i=0; i<ncolumn(); i++) {
    colSet_p[i]->addRow (nrrow_p+nr, nrrow_p);
  }
  nrrow_p += nr;
}


void MSMBase::removeRow (uInt rownr)
{
  for (uInt i=0; i<ncolumn(); i++) {
    colSet_p[i]->remove (rownr);
  }
  nrrow_p--;
}


Bool MSMBase::flush (AipsIO&, Bool)
{
  return False;
}

void MSMBase::create (uInt nrrow)
{
  //# Do not add the required nr of rows yet.
  // It is done later in reallocateColumn to avoid that all row data
  // have to be deleted and allocated again if a IndArrColumn is turned
  // into a DirArrColumn.
  nrrowCreate_p = nrrow;
}

void MSMBase::open (uInt tabNrrow, AipsIO&)
{
  nrrow_p = tabNrrow;
  //# Create the required nr of rows and initialize them.
  for (uInt i=0; i<ncolumn(); i++) {
    colSet_p[i]->doCreate (tabNrrow);
  }
}

void MSMBase::resync (uInt nrrow)
{
  // Add or remove rows if it has changed.
  // Note that removing decreases the row number, so the same row number
  // is always used.
  if (nrrow > nrrow_p) {
    addRow (nrrow-nrrow_p);
  } else {
    uInt nr=nrrow_p-nrrow;
    for (uInt i=0; i<nr; i++) {
      removeRow (nrrow);
    }
  }
}

void MSMBase::deleteManager()
{}

} //# NAMESPACE CASACORE - END

