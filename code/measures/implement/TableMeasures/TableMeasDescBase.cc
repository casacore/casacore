//# TableMeasDefBase.cc: Definition of a Measure in a Table.
//# Copyright (C) 1997,1998,1999
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
#include <trial/TableMeasures/TableMeasDescBase.h>
#include <trial/TableMeasures/TableMeasDesc.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Measures/Measure.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


TableMeasDescBase::TableMeasDescBase()
{}

TableMeasDescBase::TableMeasDescBase (const TableMeasValueDesc& value,
				      const TableMeasRefDesc& ref)
: itsValue(value),
  itsRef(ref)
{}

TableMeasDescBase::TableMeasDescBase (const TableMeasDescBase& that)
: itsValue(that.itsValue),
  itsRef(that.itsRef),
  itsMeasType(that.itsMeasType),
  itsUnits(that.itsUnits)
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
    mtype = measInfo.asRecord("Type");    	
  } else {
    throw(AipsError("TableMeasDescBase::reconstruct; MEASINFO record not "
		    "found for column " + columnName));
  }
  
  // get the units
  fnr = measInfo.fieldNumber("NumUnits");
  uInt numUnits;
  if (fnr >= 0) {
    numUnits = measInfo.asuInt(fnr);
  } else {
    throw(AipsError("TableMeasDescBase::reconstruct; No Units found"
		    " for column: " + columnName));
  }
  
  Vector<Unit> units(numUnits);
  for (uInt i=0; i<numUnits; i++) {
    String uname = "unit" + String::toString(i);
    fnr = measInfo.fieldNumber(uname);
    if (fnr >= 0) {
      units(i) = measInfo.asString(fnr);
    } else {
      throw(AipsError("TableMeasDescBase::reconstruct; Unit not found"
		      " for column: " + columnName));
    }
  }
  
  String error;
  MeasureHolder measHolder;
  measHolder.fromRecord (error, mtype);
  
  TableMeasDescBase* p = new TableMeasDescBase();
  p->itsValue = TableMeasValueDesc (tab.tableDesc(), columnName);
  p->itsMeasType = TableMeasType(measHolder.asMeasure());
  p->itsUnits = units;
  p->itsRef = TableMeasRefDesc (measInfo, tab, *p);

  return p;
}

TableMeasDescBase& TableMeasDescBase::operator= (const TableMeasDescBase& that)
{
  if (this != &that) {
    itsValue = that.itsValue;
    itsRef = that.itsRef;
    itsMeasType = that.itsMeasType;
    itsUnits = that.itsUnits;
  }
  return *this;
}
    
void TableMeasDescBase::write (TableDesc& td)
{
  TableRecord measInfo;
  TableRecord measType;

  // Create a record from the MeasType and add it to measInfo
  itsMeasType.toRecord (measType);
  measInfo.defineRecord ("Type", measType);
  // Add the units
  measInfo.define ("NumUnits", itsUnits.nelements());
  for (uInt i=0; i<itsUnits.nelements(); i++) {
    String uname = "unit" + String::toString(i);
    measInfo.define (uname, itsUnits(i).getName());
  }

  // Write the reference.
  itsRef.write (td, measInfo, *this);
  // Write the MEASINFO record into the keywords of the value column.
  itsValue.write (td, measInfo);
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
