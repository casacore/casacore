//# TableError.cc: Error classes for the table descriptor classes
//# Copyright (C) 1994,1995,1996,1997
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

rtti_imp_init(TableError);
rtti_imp_mbrf(TableError);
TableError::TableError ()
: AipsError("Table error")
{}
TableError::TableError (const String& str)
: AipsError(str)
{}
TableError::TableError (ExcpError* excp)
: AipsError(excp)
{
    TableError* tmp;
    PCAST(tmp,TableError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableError::~TableError ()
{}


rtti_imp_init(TableInternalError);
rtti_imp_mbrf(TableInternalError);
TableInternalError::TableInternalError (const String& str)
: TableError("Internal Table error: " + str)
{}
TableInternalError::TableInternalError (ExcpError* excp)
: TableError(excp)
{
    TableInternalError* tmp;
    PCAST(tmp,TableInternalError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInternalError::~TableInternalError ()
{}


rtti_imp_init(TableDuplFile);
rtti_imp_mbrf(TableDuplFile);
TableDuplFile::TableDuplFile (const String& name)
: TableError("Table " + name + " already exists")
{}
TableDuplFile::TableDuplFile (const String& name, const String& msg)
: TableError("Table " + name + " already exists" + msg)
{}
TableDuplFile::TableDuplFile (ExcpError* excp)
: TableError(excp)
{
    TableDuplFile* tmp;
    PCAST(tmp,TableDuplFile,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableDuplFile::~TableDuplFile ()
{}


rtti_imp_init(TableNoFile);
rtti_imp_mbrf(TableNoFile);
TableNoFile::TableNoFile (const String& name)
: TableError("Table " + name + " does not exist")
{}
TableNoFile::TableNoFile (ExcpError* excp)
: TableError(excp)
{
    TableNoFile* tmp;
    PCAST(tmp,TableNoFile,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableNoFile::~TableNoFile ()
{}


rtti_imp_init(TableDescNoName);
rtti_imp_mbrf(TableDescNoName);
TableDescNoName::TableDescNoName ()
: TableError ("No name for table description")
{}
TableDescNoName::TableDescNoName (ExcpError* excp)
: TableError(excp)
{
    TableDescNoName* tmp;
    PCAST(tmp,TableDescNoName,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableDescNoName::~TableDescNoName ()
{}


rtti_imp_init(TableInvOpt);
rtti_imp_mbrf(TableInvOpt);
TableInvOpt::TableInvOpt (const String& cl, const String& str)
: TableError ("Invalid " + cl + " option: " + str)
{}
TableInvOpt::TableInvOpt (ExcpError* excp)
: TableError(excp)
{
    TableInvOpt* tmp;
    PCAST(tmp,TableInvOpt,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvOpt::~TableInvOpt ()
{}


rtti_imp_init(TableInvType);
rtti_imp_mbrf(TableInvType);
TableInvType::TableInvType (const String& tpin, const String& tpfil)
: TableError ("Expected type " + tpin + ", found " + tpfil)
{}
TableInvType::TableInvType (ExcpError* excp)
: TableError(excp)
{
    TableInvType* tmp;
    PCAST(tmp,TableInvType,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvType::~TableInvType ()
{}


rtti_imp_init(TableInvColumnDesc);
rtti_imp_mbrf(TableInvColumnDesc);
TableInvColumnDesc::TableInvColumnDesc (const String& name, const String& msg)
: TableError("Invalid description of column " + name + ": " + msg)
{}
TableInvColumnDesc::TableInvColumnDesc (ExcpError* excp)
: TableError(excp)
{
    TableInvColumnDesc* tmp;
    PCAST(tmp,TableInvColumnDesc,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvColumnDesc::~TableInvColumnDesc ()
{}


rtti_imp_init(TableInvHyperDesc);
rtti_imp_mbrf(TableInvHyperDesc);
TableInvHyperDesc::TableInvHyperDesc (const String& name, const String& msg)
: TableError("Invalid description of hypercolumn " + name + ": " + msg)
{}
TableInvHyperDesc::TableInvHyperDesc (ExcpError* excp)
: TableError(excp)
{
    TableInvHyperDesc* tmp;
    PCAST(tmp,TableInvHyperDesc,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvHyperDesc::~TableInvHyperDesc ()
{}


rtti_imp_init(TableUnknownDesc);
rtti_imp_mbrf(TableUnknownDesc);
TableUnknownDesc::TableUnknownDesc (const String& name)
: TableError("ColumnDesc class " + name + " unknown to ColumnDesc::register")
{}
TableUnknownDesc::TableUnknownDesc (ExcpError* excp)
: TableError(excp)
{
    TableUnknownDesc* tmp;
    PCAST(tmp,TableUnknownDesc,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableUnknownDesc::~TableUnknownDesc ()
{}


rtti_imp_init(TableInvDT);
rtti_imp_mbrf(TableInvDT);
TableInvDT::TableInvDT ()
: TableError ("Invalid Table data type")
{}
TableInvDT::TableInvDT (const String& name)
: TableError ("Invalid Table data type when accessing column" + name)
{}
TableInvDT::TableInvDT (ExcpError* excp)
: TableError(excp)
{
    TableInvDT* tmp;
    PCAST(tmp,TableInvDT,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvDT::~TableInvDT ()
{}


rtti_imp_init(TableInvOper);
rtti_imp_mbrf(TableInvOper);
TableInvOper::TableInvOper ()
: TableError ("Invalid Table operation")
{}
TableInvOper::TableInvOper (const String& s)
: TableError ("Invalid Table operation: " + s)
{}
TableInvOper::TableInvOper (ExcpError* excp)
: TableError(excp)
{
    TableInvOper* tmp;
    PCAST(tmp,TableInvOper,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvOper::~TableInvOper ()
{}


rtti_imp_init(TableArrayConformanceError);
rtti_imp_mbrf(TableArrayConformanceError);
TableArrayConformanceError::TableArrayConformanceError (const String& s)
: TableError (s + ": Table array conformance error")
{}
TableArrayConformanceError::TableArrayConformanceError (ExcpError* excp)
: TableError(excp)
{
    TableArrayConformanceError* tmp;
    PCAST(tmp,TableArrayConformanceError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableArrayConformanceError::~TableArrayConformanceError ()
{}


rtti_imp_init(TableConformanceError);
rtti_imp_mbrf(TableConformanceError);
TableConformanceError::TableConformanceError (const String& s)
: TableError (s + ": Table conformance error (#rows mismatch)")
{}
TableConformanceError::TableConformanceError (ExcpError* excp)
: TableError(excp)
{
    TableConformanceError* tmp;
    PCAST(tmp,TableConformanceError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableConformanceError::~TableConformanceError ()
{}


rtti_imp_init(TableInvSort);
rtti_imp_mbrf(TableInvSort);
TableInvSort::TableInvSort ()
: TableError ("Invalid table sort")
{}
TableInvSort::TableInvSort (const String& s)
: TableError ("Invalid table sort: " + s)
{}
TableInvSort::TableInvSort (ExcpError* excp)
: TableError(excp)
{
    TableInvSort* tmp;
    PCAST(tmp,TableInvSort,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvSort::~TableInvSort ()
{}


rtti_imp_init(TableInvLogic);
rtti_imp_mbrf(TableInvLogic);
TableInvLogic::TableInvLogic ()
: TableError ("Tables in logical operation have different roots")
{}
TableInvLogic::TableInvLogic (ExcpError* excp)
: TableError(excp)
{
    TableInvLogic* tmp;
    PCAST(tmp,TableInvLogic,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvLogic::~TableInvLogic ()
{}


rtti_imp_init(TableInvExpr);
rtti_imp_mbrf(TableInvExpr);
TableInvExpr::TableInvExpr (const String& str)
: TableError ("Error in select expression: " + str)
{}
TableInvExpr::TableInvExpr (const String& name, const String& str)
: TableError ("Error in select expression: column " + name + " is invalid; " + str)
{}
TableInvExpr::TableInvExpr (ExcpError* excp)
: TableError(excp)
{
    TableInvExpr* tmp;
    PCAST(tmp,TableInvExpr,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableInvExpr::~TableInvExpr ()
{}


rtti_imp_init(TableVectorNonConform);
rtti_imp_mbrf(TableVectorNonConform);
TableVectorNonConform::TableVectorNonConform ()
: TableError ("Shapes of table vectors are not conformant")
{}
TableVectorNonConform::TableVectorNonConform (ExcpError* excp)
: TableError(excp)
{
    TableVectorNonConform* tmp;
    PCAST(tmp,TableVectorNonConform,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableVectorNonConform::~TableVectorNonConform ()
{}


rtti_imp_init(TableParseError);
rtti_imp_mbrf(TableParseError);
TableParseError::TableParseError (const String& s)
: TableError ("Parse error in table command: " + s)
{}
TableParseError::TableParseError (ExcpError* excp)
: TableError(excp)
{
    TableParseError* tmp;
    PCAST(tmp,TableParseError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TableParseError::~TableParseError ()
{}
