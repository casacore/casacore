//# Error.cc: Base class for all AIPS++ errors (templated classes)
//# Copyright (C) 1993,1994,1995,1996,2000
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

#include <aips/Exceptions/Error.h>
#include <iostream.h>


//#  This template is for index errors  where the Index can be any type. It
//#  is derived from the class IndexError. This derivation allows the user
//#  to catch all index errors with one catch statement. The remainder of
//#  of the class is very similar to the AipsError class.
template<class t>
indexError<t>::indexError(t oI, const Char *str) : IndexError(str), oIndex(oI)
{}
template<class t>
indexError<t>::indexError(t oI, const String &str) : IndexError(str), oIndex(oI)
{}

template<class t>
indexError<t>::indexError(ExcpError *excp) : IndexError(excp)
{
    indexError<t> *tmp;
    PCAST(tmp,indexError<t>,excp);
    if (tmp) {
        oIndex = (*tmp).oIndex;
        _equal = True;
    } else {
        _equal = False;
    }
}

template<class t>
indexError<t>::~indexError()
    {}

//#  This template is for duplicate key errors  where the Key can be any type. It
//#  is derived from the class DuplError. This derivation allows the user
//#  to catch all index errors with one catch statement. The remainder of
//#  of the class is very similar to the AipsError class.
template<class t>
duplError<t>::duplError(ExcpError *excp) : DuplError(excp)
{
    duplError<t> *tmp;
    PCAST(tmp,duplError<t>,excp);
    if (tmp) {
        oKey = (*tmp).oKey;
        _equal = True;
    } else {
        _equal = False;
    }
}

template<class t>
duplError<t>::~duplError()
    {}


template<class t>
duplError<t>::duplError(t oI, const Char *str) : DuplError(str), oKey(oI)
{}
template<class t>
duplError<t>::duplError(t oI, const String &str) : DuplError(str), oKey(oI)
{}

