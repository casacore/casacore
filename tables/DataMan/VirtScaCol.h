//# VirtScaCol.h: Templated base class for virtual scalar column
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

#ifndef TABLES_VIRTSCACOL_H
#define TABLES_VIRTSCACOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Templated base class for virtual scalar column
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManagerColumn
//   <li> VirtualColumnEngine
// </prerequisite>

// <etymology>
// VirtualScalarColumn handles a virtual column containing a scalar.
// </etymology>

// <synopsis> 
// VirtualScalarColumn is the abstract base class to handle a scalar column
// for a virtual column engine.
// It is derived from DataManagerColumn and reimplements some
// virtual functions to make life easier for the derived classes.
// It does the following:
// <ul>
//  <li>
//   It implements the dataType function, so it is not needed to implement
//   that in derived classes.
//  <li>
//   It has a default implementation of False for function isWritable.
//   Thus by default virtual scalar columns are not writable, which will
//   often be the case. Only if a virtual scalar column can be writable,
//   it has to be implemented in the derived class.
//  <li>
//   Declare a get/put function with the template parameter as its argument.
//   The virtual functions get/putBoolV, etc. (defined in DataManagerColumn)
//   are by default implemented using this (templated) get/put function.
//   This allows for the default implementation of get/putBlock and
//   makes life easier for the implementor of a derived class.
//   However, the disadvantage of this is an extra virtual function call.
//   (E.g. for a Bool value the first one is getBoolV and the second
//    one get(T&), where T is Bool). If efficiency is really necessary,
//    getBoolV, etc. should also be implemented in the derived class.
//  <li>
//   In DataManagerColumn the functions get/putBlockV and get/putColumnV
//   are defined, which have a void* data argument. This is necessary
//   to handle arbitrary data types in the non-templated base class
//   DataManagerColumn.
//   In this templated VirtualScalarColumn class, virtual functions
//   get/putBlock and get/putColumn have been defined. They cast
//   the void* data argument to T&, so in a derived class no care has
//   to be taken for that cast.
//   Furthermore a default implementation of them has been made.
//   <ul>
//    <li> getBlock gets one value using function get.
//    <li> putBlock puts one value at the time using function put.
//    <li> getColumn uses function getBlock.
//    <li> putColumn uses function putBlock.
//   </ul>
//   If efficiency is an issue, these functions should be implemented
//   in the derived class.
// </ul>
// </synopsis> 

// <motivation>
// This class reimplements some virtual functions implemented by
// DataManagerColumn and types the data argument. In that way they are
// easier to implement in derived classes. Furthermore they allow
// default implementations.
// </motivation>

// <templating arg=T>
//  <li> default constructor
//  <li> copy constructor
//  <li> assignment operator
//  <li> <src>static String dataTypeId();   // unique name of the class</src>
// </templating>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class VirtualScalarColumnBase : public DataManagerColumn
{
public:
    // Create a column.
    VirtualScalarColumnBase()
        {}

    virtual ~VirtualScalarColumnBase();

    // By default no data can be put in a virtual column.
    virtual Bool isWritable() const;

protected:
    // The array access functions throw an exception.
    // <group>
    virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);
    virtual void putArrayV (rownr_t rownr, const ArrayBase& data);
    virtual void getArrayColumnV (ArrayBase& data);
    virtual void putArrayColumnV (const ArrayBase& data);
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
				       ArrayBase& data);
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
				       const ArrayBase& data);
    virtual void getSliceV (rownr_t rownr, const Slicer& slicer, ArrayBase& data);
    virtual void putSliceV (rownr_t rownr, const Slicer& slicer,
			    const ArrayBase& data);
    virtual void getColumnSliceV (const Slicer& slicer, ArrayBase& data);
    virtual void putColumnSliceV (const Slicer& slicer, const ArrayBase& data);
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer, ArrayBase& data);
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
				       const Slicer& slicer,
				       const ArrayBase& data);
    // </group>
};


template<class T>
class VirtualScalarColumn : public VirtualScalarColumnBase
{
public:

    // Create a column.
    VirtualScalarColumn()
	{;}

    // Frees up the storage.
    virtual ~VirtualScalarColumn();

    // Return the data type of the column.
    virtual int dataType() const;

    // Return the data type Id of the column.
    virtual String dataTypeId() const;

    // Let a derived class get the scalar value in the given row.
    virtual void get (rownr_t rownr, T& data) = 0;

    // Let a derived class put the scalar value into the given row.
    // The default implementation throws an exception.
    virtual void put (rownr_t rownr, const T& data);

private:
    // Implement the virtual functions defined in DataManagerColumn.
    // Get the scalar value in the given row.
    // <group>
    virtual void getBool     (rownr_t rownr, Bool* dataPtr);
    virtual void getuChar    (rownr_t rownr, uChar* dataPtr);
    virtual void getShort    (rownr_t rownr, Short* dataPtr);
    virtual void getuShort   (rownr_t rownr, uShort* dataPtr);
    virtual void getInt      (rownr_t rownr, Int* dataPtr);
    virtual void getuInt     (rownr_t rownr, uInt* dataPtr);
    virtual void getInt64    (rownr_t rownr, Int64* dataPtr);
    virtual void getfloat    (rownr_t rownr, float* dataPtr);
    virtual void getdouble   (rownr_t rownr, double* dataPtr);
    virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
    virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
    virtual void getString   (rownr_t rownr, String* dataPtr);
    // This function is the get for all non-standard data types.
    virtual  void getOther   (rownr_t rownr, void* dataPtr);
    // </group>

    // Implement the virtual functions defined in DataManagerColumn.
    // Put the scalar value into the given row.
    // <group>
    virtual void putBool     (rownr_t rownr, const Bool* dataPtr);
    virtual void putuChar    (rownr_t rownr, const uChar* dataPtr);
    virtual void putShort    (rownr_t rownr, const Short* dataPtr);
    virtual void putuShort   (rownr_t rownr, const uShort* dataPtr);
    virtual void putInt      (rownr_t rownr, const Int* dataPtr);
    virtual void putuInt     (rownr_t rownr, const uInt* dataPtr);
    virtual void putInt64    (rownr_t rownr, const Int64* dataPtr);
    virtual void putfloat    (rownr_t rownr, const float* dataPtr);
    virtual void putdouble   (rownr_t rownr, const double* dataPtr);
    virtual void putComplex  (rownr_t rownr, const Complex* dataPtr);
    virtual void putDComplex (rownr_t rownr, const DComplex* dataPtr);
    virtual void putString   (rownr_t rownr, const String* dataPtr);
    // This function is the put for all non-standard data types.
    virtual void putOther    (rownr_t rownr, const void* dataPtr);
    // </group>

    // Get all scalar values in the column.
    // The default implementation loops over the rows.
    virtual void getScalarColumnV (ArrayBase& dataPtr);

    // Put all scalar values in the column.
    // The default implementation loops over the rows.
    virtual void putScalarColumnV (const ArrayBase& dataPtr);

    // Get some scalar values in the column.
    // The default implementation loops over the rows.
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
					ArrayBase& dataPtr);

    // Put some scalar values in the column.
    // The default implementation loops over the rows.
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
					const ArrayBase& dataPtr);

private:
    // The object cannot be copied.
    VirtualScalarColumn (const VirtualScalarColumn<T>&);

    // The object cannot be assigned to.
    VirtualScalarColumn<T>& operator= (const VirtualScalarColumn<T>&);
};



// <summary>
// Global functions to get or put data of a virtual column
// </summary>
// <synopsis>
// </synopsis>
// <group name=get_putVirtualScalar>
template<class T>
inline void getVirtualScalar (VirtualScalarColumn<T>* col,
                              uInt rownr, T* dataPtr)
    { col->get (rownr, *dataPtr); }
inline void getVirtualScalar (DataManagerColumn* col,
                              uInt, void*)
    { col->throwGet(); }

template<class T>
inline void putVirtualScalar (VirtualScalarColumn<T>* col,
                              uInt rownr, const T* dataPtr)
    { col->put (rownr, *dataPtr); }
inline void putVirtualScalar (DataManagerColumn* col,
                              uInt, const void*)
    { col->throwPut(); }
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/VirtScaCol.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
