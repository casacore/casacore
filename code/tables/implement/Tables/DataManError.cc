//# DataManError.cc: Storage manager error classes
//# Copyright (C) 1994,1995,1996
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

#include <aips/Tables/DataManError.h>

// Implementation of DataManager error classes.

rtti_imp_init(DataManError);
rtti_imp_mbrf(DataManError);
DataManError::DataManError ()
: AipsError("Table DataManager error")
{ ; }
DataManError::DataManError (const String& str)
: AipsError("Table DataManager error: " + str)
{ ; }
DataManError::DataManError (ExcpError* excp)
: AipsError(excp)
{
    DataManError* tmp;
    PCAST(tmp,DataManError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManError::~DataManError ()
{ ; }


rtti_imp_init(DataManInternalError);
rtti_imp_mbrf(DataManInternalError);
DataManInternalError::DataManInternalError (const String& str)
: DataManError("Internal error: " + str)
{ ; }
DataManInternalError::DataManInternalError (ExcpError* excp)
: DataManError(excp)
{
    DataManInternalError* tmp;
    PCAST(tmp,DataManInternalError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManInternalError::~DataManInternalError ()
{ ; }


rtti_imp_init(DataManUnknownCtor);
rtti_imp_mbrf(DataManUnknownCtor);
DataManUnknownCtor::DataManUnknownCtor (const String& msg)
: DataManError(msg)
{}
DataManUnknownCtor::DataManUnknownCtor (ExcpError* excp)
: DataManError(excp)
{
    DataManUnknownCtor* tmp;
    PCAST(tmp,DataManUnknownCtor,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManUnknownCtor::~DataManUnknownCtor ()
{}


rtti_imp_init(DataManInvDT);
rtti_imp_mbrf(DataManInvDT);
DataManInvDT::DataManInvDT ()
: DataManError ("Invalid data type")
{ ; }
DataManInvDT::DataManInvDT (const String& name)
: DataManError ("Invalid data type when accessing column " + name)
{ ; }
DataManInvDT::DataManInvDT (ExcpError* excp)
: DataManError(excp)
{
    DataManInvDT* tmp;
    PCAST(tmp,DataManInvDT,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManInvDT::~DataManInvDT ()
{ ; }


rtti_imp_init(DataManInvOper);
rtti_imp_mbrf(DataManInvOper);
DataManInvOper::DataManInvOper ()
: DataManError ("Invalid operation")
{ ; }
DataManInvOper::DataManInvOper (const String& s)
: DataManError ("Invalid operation: " + s)
{ ; }
DataManInvOper::DataManInvOper (ExcpError* excp)
: DataManError(excp)
{
    DataManInvOper* tmp;
    PCAST(tmp,DataManInvOper,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManInvOper::~DataManInvOper ()
{ ; }


rtti_imp_init(DataManUnknownVirtualColumn);
rtti_imp_mbrf(DataManUnknownVirtualColumn);
DataManUnknownVirtualColumn::DataManUnknownVirtualColumn
                                            (const String& columnName,
					     const String& engineName)
: DataManError ("column " + columnName +
		" is unknown to virtual column engine " + engineName)
{ ; }
DataManUnknownVirtualColumn::DataManUnknownVirtualColumn (ExcpError* excp)
: DataManError(excp)
{
    DataManUnknownVirtualColumn* tmp;
    PCAST(tmp,DataManUnknownVirtualColumn,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
DataManUnknownVirtualColumn::~DataManUnknownVirtualColumn ()
{ ; }


rtti_imp_init(TSMError);
rtti_imp_mbrf(TSMError);
TSMError::TSMError (const String& s)
: DataManError ("TiledStMan: " + s)
{ ; }
TSMError::TSMError (ExcpError* excp)
: DataManError(excp)
{
    TSMError* tmp;
    PCAST(tmp,TSMError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
TSMError::~TSMError ()
{ ; }
