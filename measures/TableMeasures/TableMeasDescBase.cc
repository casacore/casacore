//# TableMeasDefBase.cc: Definition of a Measure in a Table.
//# Copyright (C) 1997,1998,1999,2000,2001
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

//# Includes
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>
#include <casacore/measures/TableMeasures/TableQuantumDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableMeasDescBase::TableMeasDescBase()
{}

TableMeasDescBase::TableMeasDescBase (const TableMeasValueDesc& value,
				      const TableMeasRefDesc& ref)
: itsValue (value),
  itsRef   (ref)
{}

TableMeasDescBase::TableMeasDescBase (const TableMeasDescBase& that)
: itsValue    (that.itsValue),
  itsRef      (that.itsRef),
  itsMeasType (that.itsMeasType),
  itsUnits    (that.itsUnits)
{}

TableMeasDescBase::~TableMeasDescBase()
{}

TableMeasDescBase* TableMeasDescBase::clone() const
{
  return new TableMeasDescBase(*this);
}

TableMeasDescBase* TableMeasDescBase::reconstruct (const Table& tab, 
						   const String& columnName)
{
  Int fnr;
  TableRecord mtype;
  TableRecord measInfo;
  const TableRecord& columnKeyset = tab.tableDesc()[columnName].keywordSet();
  
  // get the Measure type
  fnr = columnKeyset.fieldNumber("MEASINFO");
  if (fnr >= 0) {
    measInfo = columnKeyset.asRecord(fnr);
    // Older TableMeasures has a separate Type record for itsMeasType.
    // Newer ones simply have the type field in the MEASINFO.
    if (measInfo.isDefined("Type")) {
      mtype = measInfo.asRecord("Type");    	
    } else {
      mtype = measInfo;
    }
  } else {
    throw(AipsError("TableMeasDescBase::reconstruct; MEASINFO record not "
		    "found for column " + columnName));
  }
  
  // get the units
  TableQuantumDesc* tqdesc = TableQuantumDesc::reconstruct (tab.tableDesc(),
							    columnName);
  Vector<String> names(tqdesc->getUnits());
  Vector<Unit> units(names.nelements());
  for (uInt i=0; i<names.nelements(); i++) {
    units(i) = names(i);
  }
  delete tqdesc;
  
  String error;
  MeasureHolder measHolder;
  measHolder.fromType (error, mtype);
  
  TableMeasDescBase* p = new TableMeasDescBase();
  p->itsValue = TableMeasValueDesc (tab.tableDesc(), columnName);
  p->itsMeasType = TableMeasType(measHolder.asMeasure());
  p->itsUnits = units;
  p->itsRef = TableMeasRefDesc (measInfo, tab, measHolder, *p);
  return p;
}

TableMeasDescBase& TableMeasDescBase::operator= (const TableMeasDescBase& that)
{
  if (this != &that) {
    itsValue    = that.itsValue;
    itsRef      = that.itsRef;
    itsMeasType = that.itsMeasType;
    itsUnits    = that.itsUnits;
  }
  return *this;
}
    
void TableMeasDescBase::write (TableDesc& td)
{
  TableRecord measInfo;
  // Create a record from the MeasType and add it to measInfo
  itsMeasType.toRecord (measInfo);
  // Put the units.
  // Use TableQuantumDesc, so the column can be used that way too.
  TableQuantumDesc tqdesc(td, itsValue.columnName(), itsUnits);
  tqdesc.write (td);
		    
  // Write the reference.
  itsRef.write (td, measInfo, *this);
  // Write the MEASINFO record into the keywords of the value column.
  itsValue.write (td, measInfo);
}

void TableMeasDescBase::write (Table& tab)
{
  TableRecord measInfo;
  // Create a record from the MeasType and add it to measInfo
  itsMeasType.toRecord (measInfo);
  // Put the units.
  // Use TableQuantumDesc, so the column can be used that way too.
  TableQuantumDesc tqdesc(tab.tableDesc(), itsValue.columnName(), itsUnits);
  tqdesc.write (tab);
		    
  // Write the reference.
  itsRef.write (tab, measInfo, *this);
  // Write the MEASINFO record into the keywords of the value column.
  itsValue.write (tab, measInfo);
}

void TableMeasDescBase::writeIfOld (const Table& tab)
{
  if (! itsRef.hasRefTab()) {
    write (const_cast<Table&>(tab));
  }
}

void TableMeasDescBase::initTabRef (const MeasureHolder& measHolder)
{
  itsRef.initTabRef (measHolder);
}

void TableMeasDescBase::setMeasUnits (const Measure& meas,
				      const Vector<Quantum<Double> >& val,
				      const Vector<Unit>& units) 
{ 
  itsMeasType = TableMeasType(meas);
  // The input unit vector cannot be longer.
  if (units.nelements() > val.nelements()) {
    throw (AipsError ("TableMeasDescBase::setMeasUnits; Unit vector"
		      " for column " + columnName() + " is too long"));
  }
  // An empty or non-given unit gets the default Quantum one.
  itsUnits.resize (val.nelements());
  for (uInt i=0; i<val.nelements(); i++) {
    if (i >= units.nelements()  ||  units(i).empty()) {
      itsUnits(i) = val(i).getUnit();
    } else {
      if (! (units(i) == val(i).getUnit())) {
	throw (AipsError ("TableMeasDescBase::setMeasUnits; invalid unit "
			  + units(i).getName() + " for column "
			  + columnName()));
      }
      itsUnits(i) = units(i);
    }
  }
}

void TableMeasDescBase::resetUnits (const Vector<Unit>& units)
{
  if (units.nelements() > itsUnits.nelements()) {
    throw (AipsError ("TableMeasDescBase::resetUnits: Unit vector"
		      " for column " + columnName() + " is too long"));
  }
  // An empty or non-given unit does not change.
  for (uInt i=0; i<units.nelements(); i++) {
    if (! units(i).empty()) {
      if (! (units(i) == itsUnits(i))) {
	throw (AipsError ("TableMeasDescBase::resetUnits; invalid unit "
			  + units(i).getName() + " for column "
			  + columnName()));
      }
      itsUnits(i) = units(i);
    }
  }
}

Bool TableMeasDescBase::hasMeasures (const TableColumn& column)
{
  return column.keywordSet().isDefined ("MEASINFO");
}

} //# NAMESPACE CASACORE - END

