//# VSCEngine.cc: Base virtual column for a scalar column with any type
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

#ifndef TABLES_VSCENGINE_TCC
#define TABLES_VSCENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/VSCEngine.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
VSCEngine<T>::VSCEngine ()
: sourceName_p ("")
{}

template<class T>
VSCEngine<T>::VSCEngine (const String& sourceColumnName)
: sourceName_p (sourceColumnName)
{}

template<class T>
VSCEngine<T>::VSCEngine (const VSCEngine<T>& that)
: VirtualColumnEngine(),
  VirtualScalarColumn<T>(),
  sourceName_p (that.sourceName_p)
{}

template<class T>
VSCEngine<T>::~VSCEngine()
{}

template<class T>
String VSCEngine<T>::dataManagerType() const
    { return dataTypeId() + "VSCEngine"; }

// The column is in principle writable.
template<class T>
Bool VSCEngine<T>::isWritable() const
    { return True; }

template<class T>
DataManagerColumn* VSCEngine<T>::makeScalarColumn (const String& columnName,
						   int, const String&)
{
    if (sourceName_p.empty()) {
	sourceName_p = columnName;
    }else{
	if (columnName != sourceName_p) {
	    throw (DataManInvOper
		   ("VSCEngine with source column " + sourceName_p +
		    " bound to column " + columnName + "; must be the same"));
	}
    }
    return this;
}

} //# NAMESPACE CASACORE - END


#endif
