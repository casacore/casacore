//# TVecLogic.cc: Global functions for table vector logical operations
//# Copyright (C) 1994,1995,1997
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

#ifndef TABLES_TVECLOGIC_TCC
#define TABLES_TVECLOGIC_TCC

#include <casacore/tables/Tables/TVecLogic.h>
#include <casacore/tables/Tables/TVec.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define TVECLOGICOPER(NAME,OP) \
template<class T> \
Bool aips_name2(tabVecReptv,NAME) (const TabVecRep<T>& l, const TabVecRep<T>& r) \
{ \
    uInt nr = r.nelements(); \
    l.validateConformance(nr); \
    Bool retval = True; \
    for (uInt i=0; i<nr; i++) { \
        if (! (l.value(i) OP r.value(i))) { \
	    retval = False; \
	    break; \
        } \
    } \
    return retval; \
} \
template<class T> \
Bool aips_name2(tabVecRepvalr,NAME) (const TabVecRep<T>& tv, const T& val) \
{ \
    uInt nr = tv.nelements(); \
    Bool retval = True; \
    for (uInt i=0; i<nr; i++) { \
        if (! (tv.value(i) OP val)) { \
	    retval = False; \
	    break; \
        } \
    } \
    return retval; \
} \
template<class T> \
Bool aips_name2(tabVecRepvall,NAME) (const T& val, const TabVecRep<T>& tv) \
{ \
    uInt nr = tv.nelements(); \
    Bool retval = True; \
    for (uInt i=0; i<nr; i++) { \
        if (! (val OP tv.value(i))) { \
	    retval = False; \
	    break; \
        } \
    } \
    return retval; \
} \

TVECLOGICOPER(LE,<=)
TVECLOGICOPER(LT,<)
TVECLOGICOPER(GE,>=)
TVECLOGICOPER(GT,>)
TVECLOGICOPER(EQ,==)
TVECLOGICOPER(NE,!=)

} //# NAMESPACE CASACORE - END


#endif
