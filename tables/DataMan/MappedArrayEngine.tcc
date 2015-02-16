//# MappedArrayEngine.cc: Templated virtual column engine to map a table array
//# Copyright (C) 2005
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

#ifndef TABLES_MAPPEDARRAYENGINE_TCC
#define TABLES_MAPPEDARRAYENGINE_TCC

//# Includes
#include <casacore/tables/DataMan/MappedArrayEngine.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class S, class T>
MappedArrayEngine<S,T>::MappedArrayEngine (const String& virtualColumnName,
					   const String& storedColumnName)
: BaseMappedArrayEngine<S,T> (virtualColumnName, storedColumnName)
{}

template<class S, class T>
MappedArrayEngine<S,T>::MappedArrayEngine (const Record& spec)
: BaseMappedArrayEngine<S,T> ()
{
  if (spec.isDefined("SOURCENAME")  &&  spec.isDefined("TARGETNAME")) {
    setNames (spec.asString("SOURCENAME"), spec.asString("TARGETNAME"));
  }
}

template<class S, class T>
MappedArrayEngine<S,T>::MappedArrayEngine (const MappedArrayEngine<S,T>& that)
: BaseMappedArrayEngine<S,T> (that)
{}

template<class S, class T>
MappedArrayEngine<S,T>::~MappedArrayEngine()
{}

//# Clone the engine object.
template<class S, class T>
DataManager* MappedArrayEngine<S,T>::clone() const
{
  DataManager* dmPtr = new MappedArrayEngine<S,T> (*this);
  return dmPtr;
}


//# Return the type name of the engine (i.e. its class name).
template<class S, class T>
String MappedArrayEngine<S,T>::dataManagerType() const
{
  return className();
}
//# Return the class name.
//# Get the data type names using class ValType.
template<class S, class T>
String MappedArrayEngine<S,T>::className()
{
  return "MappedArrayEngine<" + valDataTypeId (static_cast<S*>(0)) + ","
                              + valDataTypeId (static_cast<T*>(0)) + ">";
}

template<class S, class T>
String MappedArrayEngine<S,T>::dataManagerName() const
{
  return virtualName();
}

template<class S, class T>
Record MappedArrayEngine<S,T>::dataManagerSpec() const
{
  Record spec;
  spec.define ("SOURCENAME", virtualName());
  spec.define ("TARGETNAME", storedName());
  return spec;
}

template<class S, class T>
DataManager* MappedArrayEngine<S,T>::makeObject (const String&,
						 const Record& spec)
{
  DataManager* dmPtr = new MappedArrayEngine<S,T>(spec);
  return dmPtr;
}
template<class S, class T>
void MappedArrayEngine<S,T>::registerClass()
{
  DataManager::registerCtor (className(), makeObject);
}

template<class S, class T>
void MappedArrayEngine<S,T>::mapOnGet (Array<S>& array, const Array<T>& target)
{
  convertArray (array, target);
}

template<class S, class T>
void MappedArrayEngine<S,T>::mapOnPut (const Array<S>& array, Array<T>& target)
{
  convertArray (target, array);
}

} //# NAMESPACE CASACORE - END


#endif
