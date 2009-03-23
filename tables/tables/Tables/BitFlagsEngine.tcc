//# BitFlagsEngine.tcc: Templated virtual column engine to map bit flags to a Bool
//# Copyright (C) 2009
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

//# Includes
#include <tables/Tables/BitFlagsEngine.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ColumnDesc.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/DataManError.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/ValTypeId.h>


namespace casa { //# NAMESPACE CASA - BEGIN

  template<typename T>
  BitFlagsEngine<T>::BitFlagsEngine (const String& virtualColumnName,
                                     const String& storedColumnName,
                                     T readMask, T writeMask)
  : BaseMappedArrayEngine<Bool,T> (virtualColumnName, storedColumnName),
    itsReadMask  (readMask),
    itsWriteMask (writeMask)
  {}

  template<typename T>
  BitFlagsEngine<T>::BitFlagsEngine (const Record& spec)
  : BaseMappedArrayEngine<Bool,T> (),
    itsReadMask  (T(0xffffffff)),
    itsWriteMask (1)
  {
    if (spec.isDefined("SOURCENAME")  &&  spec.isDefined("TARGETNAME")) {
      setNames (spec.asString("SOURCENAME"), spec.asString("TARGETNAME"));
      setProperties (spec);
    }
  }

  template<typename T>
  BitFlagsEngine<T>::BitFlagsEngine (const BitFlagsEngine<T>& that)
  : BaseMappedArrayEngine<Bool,T> (that),
    itsReadMask  (that.itsReadMask),
    itsWriteMask (that.itsWriteMask)
  {}

  template<typename T>
  BitFlagsEngine<T>::~BitFlagsEngine()
  {}

  //# Clone the engine object.
  template<typename T>
  DataManager* BitFlagsEngine<T>::clone() const
  {
    DataManager* dmPtr = new BitFlagsEngine<T> (*this);
    return dmPtr;
  }


  //# Return the type name of the engine (i.e. its class name).
  template<typename T>
  String BitFlagsEngine<T>::dataManagerType() const
  {
    return className();
  }
  //# Return the class name.
  //# Get the data type names using class ValType.
  template<typename T>
  String BitFlagsEngine<T>::className()
  {
    return "BitFlagsEngine<" + valDataTypeId (static_cast<T*>(0));
  }

  template<typename T>
  String BitFlagsEngine<T>::dataManagerName() const
  {
    return virtualName();
  }

  template<typename T>
  Record BitFlagsEngine<T>::dataManagerSpec() const
  {
    Record spec = getProperties();
    spec.define ("SOURCENAME", virtualName());
    spec.define ("TARGETNAME", storedName());
    return spec;
  }

  template<typename T>
  Record BitFlagsEngine<T>::getProperties() const
  {
    Record spec;
    spec.define ("READMASK",  Int(itsReadMask));
    spec.define ("WRITEMASK", Int(itsWriteMask));
    return spec;
  }

  template<typename T>
  void BitFlagsEngine<T>::setProperties (const Record& spec)
  {
    if (spec.isDefined("READMASK")) {
      itsReadMask  = T(spec.asInt("READMASK"));
    }
    if (spec.isDefined("WRITEMASK")) {
      itsWriteMask = T(spec.asInt("WRITEMASK"));
    }
  }

  template<typename T>
  DataManager* BitFlagsEngine<T>::makeObject (const String&,
                                              const Record& spec)
  {
    DataManager* dmPtr = new BitFlagsEngine<T>(spec);
    return dmPtr;
  }
  template<typename T>
  void BitFlagsEngine<T>::registerClass()
  {
    DataManager::registerCtor (className(), makeObject);
  }


  template<typename T>
  void BitFlagsEngine<T>::create (uInt initialNrrow)
  {
    BaseMappedArrayEngine<Bool,T>::create (initialNrrow);
    // Store the various parameters as keywords in this column.
    TableColumn thisCol (table(), virtualName());
    thisCol.rwKeywordSet().define ("_BitFlagsEngine_ReadMask",
				   Int(itsReadMask));
    thisCol.rwKeywordSet().define ("_BitFlagsEngine_WriteMask",
				   Int(itsWriteMask));
  }

  template<typename T>
  void BitFlagsEngine<T>::prepare()
  {
    BaseMappedArrayEngine<Bool,T>::prepare();
    ROTableColumn thisCol (table(), virtualName());
    itsReadMask  = T(thisCol.keywordSet().asInt("_BitFlagsEngine_ReadMask"));
    itsWriteMask = T(thisCol.keywordSet().asInt("_BitFlagsEngine_WriteMask"));
  }


  template<typename T>
  void BitFlagsEngine<T>::getArray (uInt rownr, Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().get (rownr, target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putArray (uInt rownr, const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().put (rownr, target);
  }

  template<typename T>
  void BitFlagsEngine<T>::getSlice (uInt rownr, const Slicer& slicer,
                                    Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().getSlice (rownr, slicer, target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putSlice (uInt rownr, const Slicer& slicer,
                                    const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().putSlice (rownr, slicer, target);
  }

  template<typename T>
  void BitFlagsEngine<T>::getArrayColumn (Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().getColumn (target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putArrayColumn (const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().putColumn (target);
  }

  template<typename T>
  void BitFlagsEngine<T>::getArrayColumnCells (const RefRows& rownrs,
                                               Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().getColumnCells (rownrs, target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putArrayColumnCells (const RefRows& rownrs,
                                               const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().putColumnCells (rownrs, target);
  }

  template<typename T>
  void BitFlagsEngine<T>::getColumnSlice (const Slicer& slicer,
                                          Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().getColumn (slicer, target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putColumnSlice (const Slicer& slicer,
                                          const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().putColumn (slicer, target);
  }

  template<typename T>
  void BitFlagsEngine<T>::getColumnSliceCells (const RefRows& rownrs,
                                               const Slicer& slicer,
                                               Array<Bool>& array)
  {
    Array<T> target(array.shape());
    roColumn().getColumnCells (rownrs, slicer, target);
    mapOnGet (array, target);
  }
  template<typename T>
  void BitFlagsEngine<T>::putColumnSliceCells (const RefRows& rownrs,
                                               const Slicer& slicer,
                                               const Array<Bool>& array)
  {
    Array<T> target(array.shape());
    mapOnPut (array, target);
    rwColumn().putColumnCells (rownrs, slicer, target);
  }

  template<typename T>
  void BitFlagsEngine<T>::mapOnGet (Array<Bool>& array,
                                    const Array<T>& stored)
  {
    ///    arrayTransform (stored, array, FlagsToBool(itsReadMask));
  }

  template<typename T>
  void BitFlagsEngine<T>::mapOnPut (const Array<Bool>& array,
                                    Array<T>& stored)
  {
    ///    arraayTransformInPlace (stored, array, BoolToFlags(itsWriteMask));
  }

} //# NAMESPACE CASA - END
