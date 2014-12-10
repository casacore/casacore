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
//#
//# $Id$

#ifndef TABLES_VIRTSCACOL_H
#define TABLES_VIRTSCACOL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;


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


template<class T>
class VirtualScalarColumn : public DataManagerColumn
{
public:

    // Create a column.
    VirtualScalarColumn()
	{;}

    // Frees up the storage.
    virtual ~VirtualScalarColumn();

    // Return the data type of the column.
    int dataType() const;

    // Return the data type Id of the column.
    String dataTypeId() const;

    // By default no data can be put in a virtual column.
    virtual Bool isWritable() const;

    // Get the scalar value in the given row.
    virtual void get (uInt rownr, T& data) = 0;

    // Put the scalar value into the given row.
    // The default implementation throws an exception.
    virtual void put (uInt rownr, const T& data);

protected:
    // The class can handle a get/putScalarColumn.
    Bool canAccessScalarColumn (Bool& reask) const;

    // Get all scalar values in the column.
    // The argument dataPtr is in fact a Vector<T>*, but a void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn getColumn function).
    virtual void getScalarColumn (Vector<T>& data);

    // Put all scalar values in the column.
    // The argument dataPtr is in fact a const Vector<T>*, but a const void*
    // is needed to be generic.
    // The vector pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn putColumn function).
    virtual void putScalarColumn (const Vector<T>& data);

    // Get scalars from the given row on with a maximum of nrmax values.
    // It returns the actual number of values got.
    // This can be used to get an entire column of scalars or to get
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a T*, but a void*
    // is needed to be generic. It must have length nrmax.
    virtual uInt getBlock (uInt rownr, uInt nrmax, T* dataPtr);

    // Put nrmax scalars from the given row on.
    // It returns the actual number of values put.
    // This can be used to put an entire column of scalars or to put
    // a part of a column (for a cache for example).
    // The argument dataPtr is in fact a const T*, but a const void*
    // is needed to be generic. It must have length nrmax.
    virtual void putBlock (uInt rownr, uInt nrmax, const T* dataPtr);


private:
    // Implement the virtual functions defined in DataManagerColumn.
    // Get the scalar value in the given row.
    // <group>
    void getBoolV     (uInt rownr, Bool* dataPtr);
    void getuCharV    (uInt rownr, uChar* dataPtr);
    void getShortV    (uInt rownr, Short* dataPtr);
    void getuShortV   (uInt rownr, uShort* dataPtr);
    void getIntV      (uInt rownr, Int* dataPtr);
    void getuIntV     (uInt rownr, uInt* dataPtr);
    void getfloatV    (uInt rownr, float* dataPtr);
    void getdoubleV   (uInt rownr, double* dataPtr);
    void getComplexV  (uInt rownr, Complex* dataPtr);
    void getDComplexV (uInt rownr, DComplex* dataPtr);
    void getStringV   (uInt rownr, String* dataPtr);
    // This function is the get for all non-standard data types.
    void getOtherV    (uInt rownr, void* dataPtr);
    // </group>

    // Implement the virtual functions defined in DataManagerColumn.
    // Put the scalar value into the given row.
    // <group>
    void putBoolV     (uInt rownr, const Bool* dataPtr);
    void putuCharV    (uInt rownr, const uChar* dataPtr);
    void putShortV    (uInt rownr, const Short* dataPtr);
    void putuShortV   (uInt rownr, const uShort* dataPtr);
    void putIntV      (uInt rownr, const Int* dataPtr);
    void putuIntV     (uInt rownr, const uInt* dataPtr);
    void putfloatV    (uInt rownr, const float* dataPtr);
    void putdoubleV   (uInt rownr, const double* dataPtr);
    void putComplexV  (uInt rownr, const Complex* dataPtr);
    void putDComplexV (uInt rownr, const DComplex* dataPtr);
    void putStringV   (uInt rownr, const String* dataPtr);
    // This function is the put for all non-standard data types.
    void putOtherV    (uInt rownr, const void* dataPtr);
    // </group>

    // Implement the virtual functions defined in DataManagerColumn.
    // Get all scalar values in the column.
    void getScalarColumnV (void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put all scalar values in the column.
    void putScalarColumnV (const void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Get scalars from the given row on with a maximum of nrmax values.
    uInt getBlockV (uInt rownr, uInt nrmax, void* dataPtr);

    // Implement the virtual functions defined in DataManagerColumn.
    // Put nrmax scalars from the given row on.
    void putBlockV (uInt rownr, uInt nrmax, const void* dataPtr);

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
// <group name=getVirtualScalarColumn>
template<class T>
inline void getVirtualScalarColumn (VirtualScalarColumn<T>* col,
                                    uInt rownr, T* dataPtr)
    { col->get (rownr, *dataPtr); }
inline void getVirtualScalarColumn (DataManagerColumn* col,
				    uInt, void*)
    { col->throwGet(); }

template<class T>
inline void putVirtualScalarColumn (VirtualScalarColumn<T>* col,
                                    uInt rownr, const T* dataPtr)
    { col->put (rownr, *dataPtr); }
inline void putVirtualScalarColumn (DataManagerColumn* col,
				    uInt, const void*)
    { col->throwPut(); }
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/VirtScaCol.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
