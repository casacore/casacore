//# TiledFileAccess.cc: Tiled access to an array in a file
//# Copyright (C) 2001
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


#include <trial/Tables/TiledFileAccess.h>
#include <trial/Tables/TiledFileHelper.h>
#include <aips/Tables/TableError.h>
#include <aips/Containers/Record.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/Assert.h>


template<class T>
TiledFileAccess<T>::TiledFileAccess (const String& fileName,
				     Int64 fileOffset,
				     const IPosition& shape,
				     const IPosition& tileShape,
				     uInt maximumCacheSize,
				     Bool writable,
				     Bool canonical)
: itsCube     (0),
  itsTSM      (0),
  itsWritable (writable)
{
  DataType dtype = whatType ((T*)0);
  itsLocalPixelSize = ValType::getTypeSize (dtype);
  itsTSM  = new TiledFileHelper (fileName, shape, dtype, maximumCacheSize,
				 writable, canonical);
  itsCube = new TSMCube (itsTSM, itsTSM->file(), shape, tileShape, fileOffset);
}


template<class T>
TiledFileAccess<T>::~TiledFileAccess()
{
  delete itsCube;
  delete itsTSM;
}

template<class T>
Array<T> TiledFileAccess<T>::get (const Slicer& section)
{
  Array<T> arr;
  get (arr, section);
  return arr;
}

template<class T>
void TiledFileAccess<T>::get (Array<T>& buffer, const Slicer& section)
{
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  buffer.resize (shp);
  Bool deleteIt;
  T* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, False);
  buffer.putStorage (dataPtr, deleteIt);
}

template<class T>
void TiledFileAccess<T>::put (const Array<T>& buffer, const Slicer& section)
{
  AlwaysAssert (isWritable(), AipsError);
  IPosition start, end, stride;
  IPosition shp = section.inferShapeFromSource (itsCube->cubeShape(),
						start, end, stride);
  AlwaysAssert (shp.isEqual (buffer.shape()), AipsError);
  Bool deleteIt;
  const T* dataPtr = buffer.getStorage (deleteIt);
  itsCube->accessStrided (start, end, stride, (char*)dataPtr, 0,
  			  itsLocalPixelSize, True);
  buffer.freeStorage (dataPtr, deleteIt);
}


template<class T>
void TiledFileAccess<T>::setMaximumCacheSize (uInt nbytes)
{
  itsTSM->setMaximumCacheSize (nbytes);
}

template<class T>
uInt TiledFileAccess<T>::maximumCacheSize() const
{
  return itsTSM->maximumCacheSize();
}
