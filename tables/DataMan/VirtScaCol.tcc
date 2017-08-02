//# VirtScaCol.tcc: Base virtual column data manager class
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
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
VirtualScalarColumn<T>::~VirtualScalarColumn()
{}

template<class T>
int VirtualScalarColumn<T>::dataType() const
    { return ValType::getType (static_cast<T*>(0)); }

template<class T>
String VirtualScalarColumn<T>::dataTypeId() const
{
    return valDataTypeId (static_cast<T*>(0));
}


//# Implement the get/put functions via a macro.
//# In principle they are not possible.
//# The implementation is done using global functions defined in the .h fie.
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
    { get (rownr, *static_cast<T*>(dataPtr)); }
template<class T>
void VirtualScalarColumn<T>::putOtherV (uInt rownr, const void* dataPtr)
    { put (rownr, *static_cast<const T*>(dataPtr)); }


//# Now implement the default implementations of the column access functions.
template<class T>
void VirtualScalarColumn<T>::getScalarColumnV (ArrayBase& dataPtr)
{
  Vector<T>& vec = static_cast<Vector<T>&>(dataPtr);
  uInt nr = vec.nelements();
  for (uInt rownr=0; rownr<nr; ++rownr) {
    get (rownr, vec[rownr]);
  }
}

template<class T>
void VirtualScalarColumn<T>::putScalarColumnV (const ArrayBase& dataPtr)
{
  const Vector<T>& vec = static_cast<const Vector<T>&>(dataPtr);
  uInt nr = vec.nelements();
  for (uInt rownr=0; rownr<nr; ++rownr) {
    put (rownr, vec[rownr]);
  }
}

template<class T>
void VirtualScalarColumn<T>::getScalarColumnCellsV (const RefRows& rownrs,
                                                    ArrayBase& dataPtr)
{
  Vector<T>& vec = static_cast<Vector<T>&>(dataPtr);
  RefRowsSliceIter iter(rownrs);
  uInt i=0;
  while (! iter.pastEnd()) {
    uInt rownr = iter.sliceStart();
    uInt end   = iter.sliceEnd();
    uInt incr  = iter.sliceIncr();
    while (rownr <= end) {
      getOtherV (rownr, &(vec[i]));
      rownr += incr;
      i++;
    }
    iter++;
  }
}

template<class T>
void VirtualScalarColumn<T>::putScalarColumnCellsV (const RefRows& rownrs,
                                                    const ArrayBase& dataPtr)
{
  const Vector<T>& vec = static_cast<const Vector<T>&>(dataPtr);
  RefRowsSliceIter iter(rownrs);
  uInt i=0;
  while (! iter.pastEnd()) {
    uInt rownr = iter.sliceStart();
    uInt end   = iter.sliceEnd();
    uInt incr  = iter.sliceIncr();
    while (rownr <= end) {
      putOtherV (rownr, &(vec[i]));
      rownr += incr;
      i++;
    }
    iter++;
  }
}

//# The default implementation of put throws an exception.
template<class T>
void VirtualScalarColumn<T>::put (uInt, const T&)
    { throwPut(); }


} //# NAMESPACE CASACORE - END


#endif
