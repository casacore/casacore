//# TableError.cc: Error classes for the table descriptor classes
//# Copyright (C) 1994,1995,1996,1997,2000
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

#include <aips/Tables/TableError.h>

TableError::TableError ()
: AipsError("Table error")
{}
TableError::TableError (const String& str)
: AipsError(str)
{}
TableError::~TableError ()
{}


TableInternalError::TableInternalError (const String& str)
: TableError("Internal Table error: " + str)
{}
TableInternalError::~TableInternalError ()
{}


TableDuplFile::TableDuplFile (const String& name)
: TableError("Table " + name + " already exists")
{}
TableDuplFile::TableDuplFile (const String& name, const String& msg)
: TableError("Table " + name + " already exists" + msg)
{}
TableDuplFile::~TableDuplFile ()
{}


TableNoFile::TableNoFile (const String& name)
: TableError(name.empty()  ?  String("No table name given at open") :
	                      "Table " + name + " does not exist")
{}
TableNoFile::~TableNoFile ()
{}


TableDescNoName::TableDescNoName ()
: TableError ("No name for table description")
{}
TableDescNoName::~TableDescNoName ()
{}


TableInvOpt::TableInvOpt (const String& cl, const String& str)
: TableError ("Invalid " + cl + " option: " + str)
{}
TableInvOpt::~TableInvOpt ()
{}


TableInvType::TableInvType (const String& tpin, const String& tpfil)
: TableError ("Expected type " + tpin + ", found " + tpfil)
{}
TableInvType::~TableInvType ()
{}


TableInvColumnDesc::TableInvColumnDesc (const String& name, const String& msg)
: TableError("Invalid description of column " + name + ": " + msg)
{}
TableInvColumnDesc::~TableInvColumnDesc ()
{}


TableInvHyperDesc::TableInvHyperDesc (const String& name, const String& msg)
: TableError("Invalid description of hypercolumn " + name + ": " + msg)
{}
TableInvHyperDesc::~TableInvHyperDesc ()
{}


TableUnknownDesc::TableUnknownDesc (const String& name)
: TableError("ColumnDesc class " + name + " unknown to ColumnDesc::register")
{}
TableUnknownDesc::~TableUnknownDesc ()
{}


TableInvDT::TableInvDT ()
: TableError ("Invalid Table data type")
{}
TableInvDT::TableInvDT (const String& name)
: TableError ("Invalid Table data type when accessing column" + name)
{}
TableInvDT::~TableInvDT ()
{}


TableInvOper::TableInvOper ()
: TableError ("Invalid Table operation")
{}
TableInvOper::TableInvOper (const String& s)
: TableError ("Invalid Table operation: " + s)
{}
TableInvOper::~TableInvOper ()
{}


TableArrayConformanceError::TableArrayConformanceError (const String& s)
: TableError (s + ": Table array conformance error")
{}
TableArrayConformanceError::~TableArrayConformanceError ()
{}


TableConformanceError::TableConformanceError (const String& s)
: TableError (s + ": Table conformance error (#rows mismatch)")
{}
TableConformanceError::~TableConformanceError ()
{}


TableInvSort::TableInvSort ()
: TableError ("Invalid table sort")
{}
TableInvSort::TableInvSort (const String& s)
: TableError ("Invalid table sort: " + s)
{}
TableInvSort::~TableInvSort ()
{}


TableInvLogic::TableInvLogic ()
: TableError ("Tables in logical operation have different roots")
{}
TableInvLogic::~TableInvLogic ()
{}


TableInvExpr::TableInvExpr (const String& str)
: TableError ("Error in select expression: " + str)
{}
TableInvExpr::TableInvExpr (const String& name, const String& str)
: TableError ("Error in select expression: column " + name + " is invalid; " + str)
{}
TableInvExpr::~TableInvExpr ()
{}


TableVectorNonConform::TableVectorNonConform ()
: TableError ("Shapes of table vectors are not conformant")
{}
TableVectorNonConform::~TableVectorNonConform ()
{}


TableParseError::TableParseError (const String& s)
: TableError ("Parse error in table command: " + s)
{}
TableParseError::~TableParseError ()
{}
