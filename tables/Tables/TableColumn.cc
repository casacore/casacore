//# TableColumn.cc: Const access to a table column
//# Copyright (C) 1994,1995,1996,1997,1999,2001,2002
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

#include <tables/Tables/TableColumn.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableError.h>
#include <casa/Arrays/Array.h>


namespace casa { //# NAMESPACE CASA - BEGIN

ROTableColumn::ROTableColumn ()
: baseTabPtr_p(0),
  baseColPtr_p(0)
{}

ROTableColumn::ROTableColumn (const Table& tab, const String& columnName)
: baseColPtr_p(0)
{
    //# Get base table and base column.
    baseTabPtr_p = tab.baseTablePtr();
    if (baseTabPtr_p == 0) {
	throw (TableInvOper ("TableColumn: no table in Table object"));
    }
    baseColPtr_p  = baseTabPtr_p->getColumn (columnName);
    colCachePtr_p = &(baseColPtr_p->columnCache());
    canChangeShape_p = baseColPtr_p->canChangeShape();
}

ROTableColumn::ROTableColumn (const Table& tab, uInt columnIndex)
: baseColPtr_p(0)
{
    //# Get base table and base column.
    baseTabPtr_p = tab.baseTablePtr();
    if (baseTabPtr_p == 0) {
	throw (TableInvOper ("TableColumn: no table in Table object"));
    }
    baseColPtr_p  = baseTabPtr_p->getColumn (columnIndex);
    colCachePtr_p = &(baseColPtr_p->columnCache());
    canChangeShape_p = baseColPtr_p->canChangeShape();
}

ROTableColumn::ROTableColumn (const ROTableColumn& that)
: baseTabPtr_p     (that.baseTabPtr_p),
  baseColPtr_p     (that.baseColPtr_p),
  colCachePtr_p    (that.colCachePtr_p),
  canChangeShape_p (that.canChangeShape_p)
{}

ROTableColumn* ROTableColumn::clone() const
{
    return new ROTableColumn (*this);
}

void ROTableColumn::reference (const ROTableColumn& that)
{
    baseTabPtr_p     = that.baseTabPtr_p;
    baseColPtr_p     = that.baseColPtr_p;
    colCachePtr_p    = that.colCachePtr_p;
    canChangeShape_p = that.canChangeShape_p;
}

ROTableColumn::~ROTableColumn()
{}


void ROTableColumn::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("TableColumn is null"));
    }
}


TableRecord& ROTableColumn::rwKeywordSet()
{
    if (! baseTabPtr_p->isWritable()) {
	throw (TableError ("ROTableColumn::rwKeywordSet cannot be used: table "
			   + baseTabPtr_p->tableName() + " is not writable"));
    }
    return baseColPtr_p->rwKeywordSet();
}


const ColumnDesc& ROTableColumn::columnDesc() const
    { return baseColPtr_p->columnDesc(); }

Table ROTableColumn::table() const
    { return Table (baseTabPtr_p, False); }


Bool ROTableColumn::asBool (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    Bool value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
uChar ROTableColumn::asuChar (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    uChar value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
Short ROTableColumn::asShort (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    Short value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
uShort ROTableColumn::asuShort (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    uShort value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
Int ROTableColumn::asInt (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    Int value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
uInt ROTableColumn::asuInt (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    uInt value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
float ROTableColumn::asfloat (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    float value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
double ROTableColumn::asdouble (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    double value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
Complex ROTableColumn::asComplex (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    Complex value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
DComplex ROTableColumn::asDComplex (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    DComplex value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}
String ROTableColumn::asString (uInt rownr) const
{
    TABLECOLUMNCHECKROW(rownr); 
    String value;
    baseColPtr_p->getScalar (rownr, value);
    return value;
}


TableColumn::TableColumn()
: ROTableColumn()
{}

TableColumn::TableColumn (const Table& tab, const String& columnName)
: ROTableColumn(tab, columnName)
{
    if (! tab.isColumnWritable (columnName)) {
	throw (TableInvOper
	       (columnName + " is readonly (use the ROxxxColumn class)"));
    }
}

TableColumn::TableColumn (const Table& tab, uInt columnIndex)
: ROTableColumn(tab, columnIndex)
{
    if (! tab.isColumnWritable (columnIndex)) {
	throw (TableInvOper
	       ("column is readonly (use the ROxxxColumn class)"));
    }
}

TableColumn::TableColumn (const TableColumn& that)
: ROTableColumn (that)
{}

ROTableColumn* TableColumn::clone() const
{
    return new TableColumn (*this);
}

void TableColumn::reference (const TableColumn& that)
    { ROTableColumn::reference (that); }

TableColumn::~TableColumn()
{}


void TableColumn::put (uInt thisRownr, const ROTableColumn& that,
		       uInt thatRownr)
{
    TABLECOLUMNCHECKROW(thisRownr); 
    if (columnDesc().isScalar()) {
	switch (columnDesc().dataType()) {
	case TpBool:
	    putScalar (thisRownr, that.asBool (thatRownr));
	    return;
	case TpUChar:
	    putScalar (thisRownr, that.asuChar (thatRownr));
	    return;
	case TpShort:
	    putScalar (thisRownr, that.asShort (thatRownr));
	    return;
	case TpUShort:
	    putScalar (thisRownr, that.asuShort (thatRownr));
	    return;
	case TpInt:
	    putScalar (thisRownr, that.asInt (thatRownr));
	    return;
	case TpUInt:
	    putScalar (thisRownr, that.asuInt (thatRownr));
	    return;
	case TpFloat:
	    putScalar (thisRownr, that.asfloat (thatRownr));
	    return;
	case TpDouble:
	    putScalar (thisRownr, that.asdouble (thatRownr));
	    return;
	case TpComplex:
	    putScalar (thisRownr, that.asComplex (thatRownr));
	    return;
	case TpDComplex:
	    putScalar (thisRownr, that.asDComplex (thatRownr));
	    return;
	case TpString:
	    putScalar (thisRownr, that.asString (thatRownr));
	    return;
	default:
	    throw (TableInvDT ("TableColumn::put; invalid type promotion"));
	}
    }else{
	if (columnDesc().isArray()) {
	    if (columnDesc().dataType() != that.columnDesc().dataType()) {
		throw (TableInvDT ("TableColumn::put; array types mismatch"));
	    }
	    if (that.isDefined (thatRownr)) {
//#// If not defined, the this-value should be unset (if there is one).
//#// However, this requires an undefine function, which is not there yet.
		//# Get the shape and define it for non-FixedShape arrays.
		//# Then get the data and put it depending on the type.
		IPosition shape = that.shape (thatRownr);
		if ((columnDesc().options() & ColumnDesc::FixedShape)
		                                  != ColumnDesc::FixedShape) {
		    baseColPtr_p->setShape (thisRownr, shape);
		}
		switch (columnDesc().dataType()) {
		case TpBool:
		    { Array<Bool> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpUChar:
		    { Array<uChar> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpShort:
		    { Array<Short> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpUShort:
		    { Array<uShort> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpInt:
		    { Array<Int> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpUInt:
		    { Array<uInt> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpFloat:
		    { Array<float> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpDouble:
		    { Array<double> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpComplex:
		    { Array<Complex> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpDComplex:
		    { Array<DComplex> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
		      baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		case TpString:
		    { Array<String> array(shape);
		      baseColPtr(that)->get (thatRownr, &array);
			  baseColPtr_p->put (thisRownr, &array);
		    }
		    return;
		default:
		    break;
		}
	    }
	}
    }
    throw (TableInvDT ("TableColumn::put"));
}


//#// Currently this is a very dumb implementation.
//# It should check if types are equal and take advantage of that.
void TableColumn::putColumn (const ROTableColumn& that)
{
    uInt nrrow = nrow();
    if (nrrow != that.nrow()) {
	throw (TableConformanceError ("TableColumn::putColumn"));
    }
    for (uInt i=0; i<nrrow; i++) {
	put (i, that, i);
    }
}

} //# NAMESPACE CASA - END

