//# SortError.cc: Error classes for the sort class
//# Copyright (C) 1993,1994,1995
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

#include <aips/Utilities/SortError.h>

// Implementation of Sort error classes.

rtti_imp_init(SortError);
rtti_imp_mbrf(SortError);
SortError::SortError () : AipsError("Sort error")
{ ; }
SortError::SortError (const String& str) :
                            AipsError(str)
{ ; }
SortError::SortError (ExcpError* excp) : AipsError(excp)
{
    SortError* tmp;
    PCAST(tmp,SortError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
SortError::~SortError ()
{ ; }


rtti_imp_init(SortInvDT);
rtti_imp_mbrf(SortInvDT);
SortInvDT::SortInvDT () : SortError ("Invalid sort data type")
{ ; }
SortInvDT::SortInvDT (ExcpError* excp) : SortError(excp)
{
    SortInvDT* tmp;
    PCAST(tmp,SortInvDT,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
SortInvDT::~SortInvDT ()
{ ; }


rtti_imp_init(SortInvIncr);
rtti_imp_mbrf(SortInvIncr);
SortInvIncr::SortInvIncr () : SortError ("Sort increment < key Incr")
{ ; }
SortInvIncr::SortInvIncr (ExcpError* excp) : SortError(excp)
{
    SortInvIncr* tmp;
    PCAST(tmp,SortInvIncr,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
SortInvIncr::~SortInvIncr ()
{ ; }


rtti_imp_init(SortNoData);
rtti_imp_mbrf(SortNoData);
SortNoData::SortNoData () : SortError ("No data array given to constructor")
{ ; }
SortNoData::SortNoData (ExcpError* excp) : SortError(excp)
{
    SortNoData* tmp;
    PCAST(tmp,SortNoData,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
SortNoData::~SortNoData ()
{ ; }


rtti_imp_init(SortInvOpt);
rtti_imp_mbrf(SortInvOpt);
SortInvOpt::SortInvOpt () : SortError ("Invalid sort option given")
{ ; }
SortInvOpt::SortInvOpt (ExcpError* excp) : SortError(excp)
{
    SortInvOpt* tmp;
    PCAST(tmp,SortInvOpt,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}
SortInvOpt::~SortInvOpt ()
{ ; }
