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

#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableError::TableError (Category c)
: AipsError("Table error",c)
{}
TableError::TableError (const String& str,Category c)
: AipsError(str,c)
{}
TableError::~TableError () noexcept
{}


TableInternalError::TableInternalError (const String& str,Category c)
: TableError("Internal Table error: " + str,c)
{}
TableInternalError::~TableInternalError () noexcept
{}


TableDuplFile::TableDuplFile (const String& name,Category c)
: TableError("Table " + name + " already exists",c)
{}
TableDuplFile::TableDuplFile (const String& name, const String& msg,Category c)
: TableError("Table " + name + " already exists" + msg,c)
{}
TableDuplFile::~TableDuplFile () noexcept
{}


TableNoFile::TableNoFile (const String& name,Category c)
: TableError(name.empty()  ?  String("No table name given at open") :
	                      "Table " + name + " does not exist",c)
{}
TableNoFile::~TableNoFile () noexcept
{}


TableNoDir::TableNoDir (const String& name,Category c)
: TableError(name + " is not a directory",c)
{}
TableNoDir::~TableNoDir () noexcept
{}


TableNoDatFile::TableNoDatFile (const String& filename,Category c)
: TableError(filename.empty() ? String("No table name given at open") :
	                      "Table file " + filename + " does not exist",c)
{}
TableNoDatFile::~TableNoDatFile () noexcept
{}


TableDescNoName::TableDescNoName (Category c)
: TableError ("No name for table description",c)
{}
TableDescNoName::~TableDescNoName () noexcept
{}


TableInvOpt::TableInvOpt (const String& cl, const String& str, Category c)
: TableError ("Invalid " + cl + " option: " + str,c)
{}
TableInvOpt::~TableInvOpt () noexcept
{}


TableInvType::TableInvType (const String& tableName,
                            const String& tpin, const String& tpfil,Category c)
: TableError ("Table file " + tableName + "is incorrect: Expected type "
              + tpin + ", found " + tpfil, c)
{}
TableInvType::~TableInvType () noexcept
{}


TableInvColumnDesc::TableInvColumnDesc (const String& name, const String& msg,Category c)
: TableError("Invalid description of column " + name + ": " + msg,c)
{}
TableInvColumnDesc::~TableInvColumnDesc () noexcept
{}


TableInvHyperDesc::TableInvHyperDesc (const String& name, const String& msg,Category c)
: TableError("Invalid description of hypercolumn " + name + ": " + msg,c)
{}
TableInvHyperDesc::~TableInvHyperDesc () noexcept
{}


TableUnknownDesc::TableUnknownDesc (const String& name,Category c)
: TableError("ColumnDesc class " + name + " unknown to ColumnDesc::register",c)
{}
TableUnknownDesc::~TableUnknownDesc () noexcept
{}


TableInvDT::TableInvDT (Category c)
: TableError ("Invalid Table data type",c)
{}
TableInvDT::TableInvDT (const String& name,Category c)
: TableError ("Invalid Table data type when accessing column" + name,c)
{}
TableInvDT::~TableInvDT () noexcept
{}


TableInvOper::TableInvOper (Category c)
: TableError ("Invalid Table operation",c)
{}
TableInvOper::TableInvOper (const String& s,Category c)
: TableError ("Invalid Table operation: " + s,c)
{}
TableInvOper::~TableInvOper () noexcept
{}


TableArrayConformanceError::TableArrayConformanceError (const String& s,Category c)
: TableError (s + ": Table array conformance error",c)
{}
TableArrayConformanceError::~TableArrayConformanceError () noexcept
{}


TableConformanceError::TableConformanceError (const String& s,Category c)
: TableError (s + ": Table conformance error (#rows mismatch)",c)
{}
TableConformanceError::~TableConformanceError () noexcept
{}


TableInvSort::TableInvSort (Category c)
: TableError ("Invalid table sort",c)
{}
TableInvSort::TableInvSort (const String& s,Category c)
: TableError ("Invalid table sort: " + s,c)
{}
TableInvSort::~TableInvSort () noexcept
{}


TableInvLogic::TableInvLogic (Category c)
: TableError ("Tables in logical operation have different roots",c)
{}
TableInvLogic::~TableInvLogic () noexcept
{}


TableInvExpr::TableInvExpr (const String& str,Category c)
: TableError ("Error in select expression: " + str,c)
{}
TableInvExpr::TableInvExpr (const String& name, const String& str,Category c)
: TableError ("Error in select expression: column " + name + " is invalid; " + str,c)
{}
TableInvExpr::~TableInvExpr () noexcept
{}


TableVectorNonConform::TableVectorNonConform (Category c)
: TableError ("Shapes of table vectors are not conformant",c)
{}
TableVectorNonConform::~TableVectorNonConform () noexcept
{}


TableParseError::TableParseError (const String& s, int pos,
                                  const String& token, Category c)
  : TableError ("Error in TaQL command: " + s, c),
  itsPos   (pos),
  itsToken (token)
{}
TableParseError::~TableParseError () noexcept
{}


TableGramError::TableGramError (int pos, const String& token, Category c)
: TableError ("parse error at or near position " +
              String::toString(pos) + " '" + token + "'", c),
  itsPos   (pos),
  itsToken (token)
{}
TableGramError::~TableGramError () noexcept
{}

} //# NAMESPACE CASACORE - END

