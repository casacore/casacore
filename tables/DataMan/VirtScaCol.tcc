//# VirtScaCol.cc: Base virtual column data manager class
//# Copyright (C) 1994,1995,1996,1999
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

#ifndef TABLES_VIRTSCACOL_TCC
#define TABLES_VIRTSCACOL_TCC

//# Includes
#include <casacore/tables/DataMan/VirtScaCol.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
VirtualScalarColumn<T>::~VirtualScalarColumn()
{}

template<class T>
Bool VirtualScalarColumn<T>::isWritable() const
    { return False; }

template<class T>
int VirtualScalarColumn<T>::dataType() const
    { return ValType::getType (static_cast<T*>(0)); }

template<class T>
String VirtualScalarColumn<T>::dataTypeId() const
{
    return valDataTypeId (static_cast<T*>(0));
}

template<class T>
Bool VirtualScalarColumn<T>::canAccessScalarColumn (Bool& reask) const
{
    reask = False;
    return True;
}


//# Implement the get/put functions via a macro.
//# In principle they are not possible.
//# The implementation is done using global functions iso. specializations
//# to overcome the limitations of the SGI compiler (in May 1995).
#define VIRTUALSCALARCOLUMN_GETPUT(TP,NM) \
template<class T> \
void VirtualScalarColumn<T>::aips_name2(get,NM) (uInt rownr, TP* dataPtr) \
    { getVirtualScalarColumn (this, rownr, dataPtr); } \
template<class T> \
void VirtualScalarColumn<T>::aips_name2(put,NM) (uInt rownr, \
                                                 const TP* dataPtr) \
    { putVirtualScalarColumn (this, rownr, dataPtr); }

VIRTUALSCALARCOLUMN_GETPUT(Bool,BoolV)
VIRTUALSCALARCOLUMN_GETPUT(uChar,uCharV)
VIRTUALSCALARCOLUMN_GETPUT(Short,ShortV)
VIRTUALSCALARCOLUMN_GETPUT(uShort,uShortV)
VIRTUALSCALARCOLUMN_GETPUT(Int,IntV)
VIRTUALSCALARCOLUMN_GETPUT(uInt,uIntV)
VIRTUALSCALARCOLUMN_GETPUT(float,floatV)
VIRTUALSCALARCOLUMN_GETPUT(double,doubleV)
VIRTUALSCALARCOLUMN_GETPUT(Complex,ComplexV)
VIRTUALSCALARCOLUMN_GETPUT(DComplex,DComplexV)
VIRTUALSCALARCOLUMN_GETPUT(String,StringV)

template<class T>
void VirtualScalarColumn<T>::getOtherV (uInt rownr, void* dataPtr)
    { get (rownr, *(T*)dataPtr); }
template<class T>
void VirtualScalarColumn<T>::putOtherV (uInt rownr, const void* dataPtr)
    { put (rownr, *(const T*)dataPtr); }


//# Implement the generic functions as typed functions.
template<class T>
void VirtualScalarColumn<T>::getScalarColumnV (void* dataPtr)
    { getScalarColumn (*(Vector<T>*)dataPtr); }
template<class T>
void VirtualScalarColumn<T>::putScalarColumnV (const void* dataPtr)
    { putScalarColumn (*(const Vector<T>*)dataPtr); }
template<class T>
uInt VirtualScalarColumn<T>::getBlockV (uInt rownr, uInt nrmax, void* dataPtr)
    { return getBlock (rownr, nrmax, (T*)dataPtr); }
template<class T>
void VirtualScalarColumn<T>::putBlockV (uInt rownr, uInt nrmax, const void* dataPtr)
    { putBlock (rownr, nrmax, (const T*)dataPtr); }


//# Now implement the default implementations of the typed functions.

//# The default implementation of put throws an exception.
template<class T>
void VirtualScalarColumn<T>::put (uInt, const T&)
    { throwPut(); }

//# The default implementation of get/putScalarColumn handles its data using
//# get/putBlock.
template<class T>
void VirtualScalarColumn<T>::getScalarColumn (Vector<T>& vec)
{
    Bool deleteIt;
    T* data = vec.getStorage (deleteIt);
    uInt nrgot;
    uInt rownr=0;
    for (uInt nrtodo=vec.nelements(); nrtodo>0;) {
        nrgot = getBlock (rownr, nrtodo, data);
        nrtodo -= nrgot;
        data   += nrgot;
	rownr  += nrgot;
    }
    vec.putStorage (data, deleteIt);
}
template<class T>
void VirtualScalarColumn<T>::putScalarColumn (const Vector<T>& vec)
{
    Bool deleteIt;
    const T* data =vec.getStorage (deleteIt);
    putBlock (0, vec.nelements(), data);
    vec.freeStorage (data, deleteIt);
}

//# The default implementation of getBlock gets one value.
//# The default implementation of putBlock puts one value at a time.
template<class T>
uInt VirtualScalarColumn<T>::getBlock (uInt rownr, uInt nrmax, T* dataPtr)
{
    if (nrmax > 0) {
	get (rownr, *dataPtr);
	return 1;
    }
    return 0;
}
template<class T>
void VirtualScalarColumn<T>::putBlock (uInt rownr, uInt nrmax, const T* dataPtr)
{
    while (nrmax > 0) {
	put (rownr++, *dataPtr++);
	nrmax--;
    }
}

} //# NAMESPACE CASACORE - END


#endif
