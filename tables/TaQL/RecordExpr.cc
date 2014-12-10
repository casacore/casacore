//# RecordExpr.cc: Global functions to make a expression node for a record field
//# Copyright (C) 2000
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
//#
//# $Id$


#include <casacore/tables/TaQL/RecordExpr.h>
#include <casacore/tables/TaQL/ExprNodeRecord.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNode makeRecordExpr (const RecordDesc& desc,
			      Int fieldNumber)
{
  if (fieldNumber < 0  ||  fieldNumber >= Int(desc.nfields())) {
    throw (AipsError ("makeRecordExpr: invalid field number given"));
  }
  Block<Int> fieldNrs (1, fieldNumber);
  if (desc.isArray (fieldNumber)) {
    return new TableExprNodeRecordFieldArray (desc.type(fieldNumber),
					      fieldNrs);
  }
  return new TableExprNodeRecordField (desc.type(fieldNumber),
				       fieldNrs);
}


TableExprNode makeRecordExpr (const RecordDesc& desc,
			      const String& fieldName)
{
  Int fld = desc.fieldNumber (fieldName);
  if (fld < 0) {
    throw (AipsError ("makeRecordExpr: field name " + fieldName +
		      " is unknown"));
  }
  return makeRecordExpr (desc, fld);
}


TableExprNode makeRecordExpr (const RecordInterface& record,
			      const String& fieldName)
{
  Vector<String> names (stringToVector (fieldName, '.'));
  if (names.nelements() == 0) {
    throw (AipsError ("makeRecordExpr: empty field name given"));
  }
  Block<Int> fieldNrs (names.nelements());
  String name;
  Int fld=0;
  const RecordInterface* recPtr = &record;
  RecordDesc desc(record.description());
  for (uInt i=0; i<names.nelements(); i++) {
    if (i != 0) {
      name += '.';
    }
    name += names(i);
    fld = desc.fieldNumber (names(i));
    if (fld < 0) {
      throw (AipsError ("makeRecordExpr: field name " + name + " is unknown"));
    }
    
    if (i < names.nelements()-1) {
      if (! desc.isSubRecord(fld)) {
	throw (AipsError ("makeRecordExpr: field name " + name +
			  " is not a subrecord"));
      } else {
	recPtr = &(recPtr->asRecord(fld));
	desc = recPtr->description();
      }
    }
    fieldNrs[i] = fld;
  }
  if (desc.isArray (fld)) {
    return new TableExprNodeRecordFieldArray (desc.type(fld), fieldNrs);
  }
  return new TableExprNodeRecordField (desc.type(fld), fieldNrs);
}

} //# NAMESPACE CASACORE - END

