//# RecordExpr.h: Global functions to make a expression node for a record field
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
//# $Id$

#ifndef TABLES_RECORDEXPR_H
#define TABLES_RECORDEXPR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordDesc.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Global functions to make a expression node for a record field.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This file contains a few global functions to construct an expression
// node for a field in a record.
// </synopsis> 


// <group name=RecordExpr>

// Make a record expression node for the given field in the record description.
// <group>
TableExprNode makeRecordExpr (const RecordDesc& desc,
			      Int fieldNumber);
TableExprNode makeRecordExpr (const RecordDesc& desc,
			      const String& fieldName);
// </group>

// Make a record expression node for the given field in the record.
inline TableExprNode makeRecordExpr (const RecordInterface& record,
				     Int fieldNumber)
    { return makeRecordExpr (record.description(), fieldNumber); }


// Make a record expression node for the given field in the record.
// The field can be a field in the record itself, but it can also be
// a field in a subrecord (or subsubrecord, etc.). If it is not a field
// in the record itself, the name must define the 'path' to the field
// in the subrecord by preceeding the field name with the name(s) of the
// subrecord(s) separated by dots. E.g. <src>sub1.sub2.fld</src>
TableExprNode makeRecordExpr (const RecordInterface& record,
			      const String& fieldName);
// </group>




} //# NAMESPACE CASACORE - END

#endif
