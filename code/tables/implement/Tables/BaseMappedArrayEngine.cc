//# BaseMappedArrayEngine.cc: Abstract virtual column engine for source->target mapping
//# Copyright (C) 1995,1996,2001,2002
//# Associated Universitie Inc. Washington DC, USA.
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
#include <aips/Tables/BaseMappedArrayEngine.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/DataManError.h>
#include <aips/Tables/TableRecord.h>
#include <aips/BasicSL/String.h>


template<class SourceType, class TargetType>
BaseMappedArrayEngine<SourceType, TargetType>::BaseMappedArrayEngine ()
: sourceName_p   (""),
  targetName_p   (""),
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  roColumn_p     (0),
  column_p       (0)
{}

template<class SourceType, class TargetType>
BaseMappedArrayEngine<SourceType, TargetType>::BaseMappedArrayEngine
                                        (const String& sourceColumnName,
					 const String& targetColumnName)
: sourceName_p   (sourceColumnName),
  targetName_p   (targetColumnName), 
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  roColumn_p     (0),
  column_p       (0)
{}

template<class SourceType, class TargetType>
BaseMappedArrayEngine<SourceType, TargetType>::BaseMappedArrayEngine
                   (const BaseMappedArrayEngine<SourceType, TargetType>& that)
: sourceName_p   (that.sourceName_p),
  targetName_p   (that.targetName_p),
  tempWritable_p (False),
  initialNrrow_p (0),
  arrayIsFixed_p (False),
  roColumn_p     (0),
  column_p       (0)
{}

template<class SourceType, class TargetType>
BaseMappedArrayEngine<SourceType, TargetType>::~BaseMappedArrayEngine()
{
    delete roColumn_p;
    delete column_p;
}


// The function prepare is called upon initialization of the virtual column.
// The initialization order of the columns is undetermined, which means
// that this function isWritable can be called before the column has been
// initialized.
// For example, suppose column A uses column B and A gets initialized
// before B. Then A will call B's isWritable(), while B has not been
// initialized yet.
// This all means that isWritable must take care of the case
// where the writable_p flag is not set yet.
template<class SourceType, class TargetType>
Bool BaseMappedArrayEngine<SourceType, TargetType>::isWritable() const
{
    if (tempWritable_p) {
	return True;
    }
    return table().isColumnWritable (targetName_p);
}


// Create the column object for the array column in this engine.
// This merely checks if the source column name matches.
template<class SourceType, class TargetType>
DataManagerColumn*
          BaseMappedArrayEngine<SourceType, TargetType>::makeIndArrColumn
                            (const String& columnName, int, const String&)
{
    //# Check if the column name matches the source column name.
    //# The source name is only filled in case of creating a new table.
    //# In case the table is read back, makeIndArrColumn is called
    //# before prepare, thus before the source name can be read back.
    if (sourceName_p.empty()) {
	sourceName_p = columnName;
    } else if (columnName != sourceName_p) {
	throw (DataManInvOper
	       ("BaseMappedArrayEngine with source column " + sourceName_p +
		" bound to column " + columnName + "; should be the same"));
    }
    return this;
}
 
template<class SourceType, class TargetType>
TableColumn BaseMappedArrayEngine<SourceType, TargetType>::makeTableColumn
                                                (const String& columnName)
{
    tempWritable_p = True;
    TableColumn thisCol (table(), columnName);
    tempWritable_p = False;
    return thisCol;
}


template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::create (uInt initialNrrow)
{
    //# Define the target name as a column keyword in the source.
    makeTableColumn (sourceName_p).rwKeywordSet().define
	                       ("_BaseMappedArrayEngine_Name", targetName_p);
    initialNrrow_p = initialNrrow;
}

template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::prepare()
{
    prepare1();
    prepare2();
}


template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::prepare1()
{
    //# Get the name of the target column from the keywords in the
    //# source column.
    ROTableColumn thisCol (table(), sourceName_p);
    targetName_p = thisCol.keywordSet().asString
	                                    ("_BaseMappedArrayEngine_Name");
    //# Determine if the target column is writable.
    //# Allocate an object to get from the target column.
    //# Allocate one to put if the column is writable.
    roColumn_p = new ROArrayColumn<TargetType> (table(), targetName_p);
    //# It is not permitted to have a FixedShape target and non-FixedShape
    //# source column.
    if ((! arrayIsFixed_p)  &&
              ((roColumn_p->columnDesc().options() & ColumnDesc::FixedShape)
	                                          == ColumnDesc::FixedShape)) {
	throw (DataManInvOper ("BaseMappedArrayEngine: source column " +
			       sourceName_p + " is FixedShape, but target " +
			       targetName_p + " is not"));
    }
    if (isWritable()) {
	column_p = new ArrayColumn<TargetType> (table(), targetName_p);
    }
}

template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::prepare2()
{
    //# Add the initial number of rows (thus only done after create).
    //# This will set the shape of the target arrays when needed.
    if (initialNrrow_p > 0) {
	addRowInit (0, initialNrrow_p);
    }
}

template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::reopenRW()
{
    // Create the writable ArrayColumn object if it does not exist
    // yet and if the column is writable now.
    if (column_p == 0) {
	if (isWritable()) {
	    column_p = new ArrayColumn<TargetType> (table(), targetName_p);
	}
    }
}


//# By default addition and removal of rows is allowed.
//# Deletion is a no-op.
template<class SourceType, class TargetType>
Bool BaseMappedArrayEngine<SourceType, TargetType>::canAddRow() const
    { return True; }
template<class SourceType, class TargetType>
Bool BaseMappedArrayEngine<SourceType, TargetType>::canRemoveRow() const
    { return True; }

template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::removeRow (uInt)
{}

//# Add nrrow rows to the end of the table.
//# Set the shape if source is FixedShape and target is non-FixedShape.
template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::addRow (uInt nrrow)
{
  addRowInit (table().nrow(), nrrow);
}
template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::addRowInit (uInt startRow,
								uInt nrrow)
{
    if (arrayIsFixed_p  &&
              ((column_p->columnDesc().options() & ColumnDesc::FixedShape)
	                                        != ColumnDesc::FixedShape)) {
	for (uInt i=0; i<nrrow; i++) {
	    column_p->setShape (startRow++, shapeFixed_p);
	}
    }
}

//# This function is called in case the source column has FixedShape arrays.
//# If the target has non-FixedShape arrays this shape will be set for the
//# array in each row of the target (by function addRow).
template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::setShapeColumn
                                                   (const IPosition& shape)
{
    shapeFixed_p   = shape;
    arrayIsFixed_p = True;
}

template<class SourceType, class TargetType>
void BaseMappedArrayEngine<SourceType, TargetType>::setShape
                                       (uInt rownr, const IPosition& shape)
{
    column_p->setShape (rownr, shape);
}

template<class SourceType, class TargetType>
Bool BaseMappedArrayEngine<SourceType, TargetType>::isShapeDefined (uInt rownr)
{
    return roColumn_p->isDefined (rownr);
}

template<class SourceType, class TargetType>
uInt BaseMappedArrayEngine<SourceType, TargetType>::ndim (uInt rownr)
{
    return roColumn_p->ndim (rownr);
}

template<class SourceType, class TargetType>
IPosition BaseMappedArrayEngine<SourceType, TargetType>::shape (uInt rownr)
{
    return roColumn_p->shape (rownr);
}

template<class SourceType, class TargetType>
Bool BaseMappedArrayEngine<SourceType, TargetType>::canChangeShape() const
{
    return (roColumn_p == 0  ?  False : roColumn_p->canChangeShape());
}
