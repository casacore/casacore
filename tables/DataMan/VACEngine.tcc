//# VACEngine.cc: Base virtual column for an array column with any type
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

#ifndef TABLES_VACENGINE_TCC
#define TABLES_VACENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/VACEngine.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
VACEngine<T>::VACEngine ()
: sourceName_p ("")
{}

template<class T>
VACEngine<T>::VACEngine (const String& sourceColumnName)
: sourceName_p (sourceColumnName)
{}

template<class T>
VACEngine<T>::VACEngine (const VACEngine<T>& that)
: VirtualColumnEngine(),
  VirtualArrayColumn<T>(),
  sourceName_p (that.sourceName_p)
{}

template<class T>
VACEngine<T>::~VACEngine()
{}

template<class T>
String VACEngine<T>::dataManagerType() const
    { return dataTypeId() + "VACEngine"; }

// The column is in principle writable.
template<class T>
Bool VACEngine<T>::isWritable() const
    { return True; }

template<class T>
DataManagerColumn* VACEngine<T>::makeDirArrColumn (const String& columnName,
						   int dataType, const String& dataTypeId)
{
  return makeIndArrColumn (columnName, dataType, dataTypeId);
}
  
template<class T>
DataManagerColumn* VACEngine<T>::makeIndArrColumn (const String& columnName,
						   int, const String&)
{
    if (sourceName_p.empty()) {
	sourceName_p = columnName;
    }else{
	if (columnName != sourceName_p) {
	    throw (DataManInvOper
		   ("VACEngine with source column " + sourceName_p +
		    " bound to column " + columnName + "; must be the same"));
	}
    }
    return this;
}

} //# NAMESPACE CASACORE - END


#endif
