//# HDF5Lattice.tcc: this defines the HDF5Lattice class
//# Copyright (C) 2008
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or(at your
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

#ifndef LATTICES_HDF5LATTICE_TCC
#define LATTICES_HDF5LATTICE_TCC

#include <casacore/lattices/Lattices/HDF5Lattice.h>
#include <casacore/lattices/Lattices/HDF5LattIter.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/HDF5/HDF5Error.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice()
  {}

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const TiledShape& shape, const String& fileName,
			       const String& arrayName, const String& groupName)
  {
    itsFile = new HDF5File (fileName, ByteIO::New);
    makeArray (shape, arrayName, groupName);
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const TiledShape& shape)
  {
    Path fileName = File::newUniqueName(String("./"), String("HDF5Lattice"));
    itsFile = new HDF5File (fileName.absoluteName(), ByteIO::Scratch);
    makeArray (shape, "array", String());
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const TiledShape& shape,
			       const CountedPtr<HDF5File>& file,
			       const String& arrayName, const String& groupName)
  : itsFile (file)
  {
    makeArray (shape, arrayName, groupName);
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const String& fileName,
			       const String& arrayName, const String& groupName)
  {
    // Open for write if possible.
    if (File(fileName).isWritable()) {
      itsFile = new HDF5File(fileName, ByteIO::Update);
    } else {
      itsFile = new HDF5File(fileName);
    }
    openArray (arrayName, groupName);
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const CountedPtr<HDF5File>& file,
			       const String& arrayName, const String& groupName)
  : itsFile (file)
  {
    openArray (arrayName, groupName);
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::HDF5Lattice (const HDF5Lattice<T>& other)
  : Lattice<T>(),
    itsFile    (other.itsFile),
    itsGroup   (other.itsGroup),
    itsDataSet (other.itsDataSet)
  {
    DebugAssert (ok(), AipsError);
  }

  template<typename T>
  HDF5Lattice<T>::~HDF5Lattice()
  {
    flush();
  }

  template<typename T>
  HDF5Lattice<T>& HDF5Lattice<T>::operator= (const HDF5Lattice<T>& other)
  {
    if (this != &other) {
      itsFile    = other.itsFile;
      itsGroup   = other.itsGroup;
      itsDataSet = other.itsDataSet;
    }
    DebugAssert (ok(), AipsError);
    return *this;
  }

  template<typename T>
  Lattice<T>* HDF5Lattice<T>::clone() const
  {
    return new HDF5Lattice<T> (*this);
  }

  template<typename T>
  Bool HDF5Lattice<T>::isPersistent() const
  {
    return True;
  }

  template<typename T>
  Bool HDF5Lattice<T>::isPaged() const
  {
    return True;
  }

  template<typename T>
  Bool HDF5Lattice<T>::isWritable() const
  {
    // HDF5Lattice is writable if underlying file is already open for write
    // or if the underlying file is in principle writable.
    return itsFile->isWritable();
  }

  template <typename T> 
  String HDF5Lattice<T>::name (Bool stripPath) const 
  {
    Path path(itsFile->getName());
    if (!stripPath) {
      return path.absoluteName();
    } 
    return path.baseName();
  }

  template<typename T>
  IPosition HDF5Lattice<T>::shape() const
  {
    DebugAssert (ok(), AipsError);
    return itsDataSet->shape();
  }

  template<typename T>
  Bool HDF5Lattice<T>::doGetSlice (Array<T>& buffer, const Slicer& section)
  {
    buffer.resize (section.length());
    Bool deleteIt;
    T* data = buffer.getStorage (deleteIt);
    itsDataSet->get (section, data);
    buffer.putStorage (data, deleteIt);
    return False;
  }

  template<typename T>
  void HDF5Lattice<T>::doPutSlice (const Array<T>& sourceArray, 
				   const IPosition& where,
				   const IPosition& stride)
  {
    checkWritable();
    Bool deleteIt;
    const T* data = sourceArray.getStorage (deleteIt);
    const uInt arrDim = sourceArray.ndim();
    const uInt latDim = ndim();
    AlwaysAssert(arrDim <= latDim, AipsError);
    if (arrDim == latDim) {
      Slicer section(where, sourceArray.shape(), stride, Slicer::endIsLength); 
      itsDataSet->put (section, data);
    } else {
      Array<T> degenerateArr(sourceArray.addDegenerate(latDim-arrDim));
      Slicer section(where, degenerateArr.shape(), stride, Slicer::endIsLength);
      itsDataSet->put (section, data);
    } 
    sourceArray.freeStorage (data, deleteIt);
  }

  template<typename T>
  IPosition HDF5Lattice<T>::tileShape() const
  {
    return itsDataSet->tileShape();
  }

  template<typename T>
  uInt HDF5Lattice<T>::advisedMaxPixels() const
  {
    return tileShape().product();
  }

  template<typename T>
  IPosition HDF5Lattice<T>::doNiceCursorShape (uInt maxPixels) const
  {
    IPosition retval = tileShape();
    if (retval.product() > Int(maxPixels)) {
      retval = Lattice<T>::doNiceCursorShape(maxPixels);
    }
    return retval;
  }

  template<class T>
  void HDF5Lattice<T>::setCacheSizeInTiles (uInt howManyTiles)
  {
    itsDataSet->setCacheSize (howManyTiles);
  }

  template<class T>
  void HDF5Lattice<T>::setCacheSizeFromPath (const IPosition& sliceShape,
                                             const IPosition& windowStart,
                                             const IPosition& windowLength,
                                             const IPosition& axisPath)
  {
    itsDataSet->setCacheSize (TSMCube::calcCacheSize (itsDataSet->shape(),
                                                      itsDataSet->tileShape(),
                                                      False,
                                                      sliceShape, windowStart,
                                                      windowLength, axisPath,
                                                      0, 1));
  }

  template<typename T>
  T HDF5Lattice<T>::getAt (const IPosition& where) const
  {
    T value;
    itsDataSet->get (Slicer(where), &value);
    return value;
  }

  template<typename T>
  void HDF5Lattice<T>::putAt (const T& value, const IPosition& where)
  {
    itsDataSet->put (Slicer(where), &value);
  }

  template<typename T>
  Bool HDF5Lattice<T>::ok() const
  {
    return True;
  }

  template<typename T>
  void HDF5Lattice<T>::checkWritable() const
  {
    if (!isWritable()) {
      throw HDF5Error ("file " + itsFile->getName() + " is not writable");
    }
  }

  template<typename T>
  LatticeIterInterface<T>* HDF5Lattice<T>::makeIter (const LatticeNavigator& nav,
						     Bool useRef) const
  {
    return new HDF5LattIter<T>(*this, nav, useRef);
  }

  template <typename T>
  void HDF5Lattice<T>::openArray (const String& arrayName,
				  const String& groupName)
  {
    if (groupName.empty()) {
      // Use root group.
      itsGroup = new HDF5Group(*itsFile, "/", true);
    } else {
      itsGroup = new HDF5Group(*itsFile, groupName, true);
    }
    // Open the data set.
    itsDataSet = new HDF5DataSet (*itsGroup, arrayName, (const T*)0);
  }

  template <typename T>
  void HDF5Lattice<T>::makeArray (const TiledShape& shape,
				  const String& arrayName,
				  const String& groupName)
  {
    // Make sure the table is writable.
    checkWritable();
    if (groupName.empty()) {
      // Use root group.
      itsGroup = new HDF5Group(*itsFile, "/", true);
    } else {
      // Create group if not existing yet.
      itsGroup = new HDF5Group(*itsFile, groupName);
    }
    // Create the data set.
    itsDataSet = new HDF5DataSet (*itsGroup, arrayName, shape.shape(),
				  shape.tileShape(), (const T*)0);
  }

  template<typename T>
  void HDF5Lattice<T>::flush()
  {
    itsFile->flush();
  }

} //# NAMESPACE CASACORE - END

#endif
