//# ISMColumn.h: A Column in the Incremental Storage Manager
//# Copyright (C) 1996,1997,1998,1999,2002
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

#ifndef TABLES_ISMCOLUMN_H
#define TABLES_ISMCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/tables/DataMan/ISMBase.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/OS/Conversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class ISMBucket;


// <summary>
// A Column in the Incremental Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=ISMBase>ISMBase</linkto>
// </prerequisite>

// <etymology>
// ISMColumn represents a Column in the Incremental Storage Manager.
// </etymology>

// <synopsis>
// ISMColumn handles the access to a column containing scalars or direct
// arrays of the various data types. It uses class <linkto class=ISMBucket>
// ISMBucket</linkto> to get and put the data into the correct bucket.
// When the value does not fit in the bucket, the bucket is split
// and the new bucket is added to the storage manager.
// <p>
// The object maintains a variable indicating the last row ever put.
// This is used to decide if a put of a value is valid until the
// end of the table or for that one row only. In this way it does not
// make any difference if rows are added before or after a value is put
// <br>
// A value put before or at the last row ever put will only affect that
// one row. The rows before and after it keep their original value. If
// needed that value is copied.
// <p>
// To optimize (especially sequential) access to the column, ISMColumn
// maintains the last value gotten and the rows for which it is valid.
// In this way a get does not need to access the data in the bucket.
// <p>
// ISMColumn use the static conversion functions in the
// <linkto class=Conversion>Conversion</linkto> framework to
// get/put the data in external format (be it canonical or local).
// Most data types are fixed length, but some are variable length
// (e.g. String). In external format variable length data is preceeded
// by its total length (which includes the length itself). This makes
// it possible to get the length of a data value without having to
// interpret it, which is easy when (re)moving a value. For this reason
// ISMColumn contains its own conversion functions for Strings.
// <p>
// ISMColumn also acts as the base class for more specialized ISM
// column classes (i.e. <linkto class=ISMIndColumn>ISMIndColumn</linkto>
// for indirect columns).
// In this way <linkto class=ISMBase>ISMBase</linkto> can hold a
// block of <src>ISMColumn*</src> for any column. Furthermore
// <src>ISMColumn</src> contains the hooks to allow a derived class
// to use other ISMColumn functions (e.g. there are "action" functions
// for a derived class to react on the duplication or removal of
// a data value (e.g. due to a bucket split).
// </synopsis> 

// <motivation>
// ISMColumn encapsulates all operations on an ISM Column.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class ISMColumn : public StManColumnBase
{
public:
    // Create a ISMColumn object with the given parent.
    // It initializes the various variables.
    // It keeps the pointer to its parent (but does not own it).
    ISMColumn (ISMBase* parent, int dataType, uInt colnr);

    ~ISMColumn();

    // Set the shape of an array in the column.
    virtual void setShapeColumn (const IPosition& shape);

    // Get the dimensionality of the item in the given row.
    // This is the same for all rows.
    virtual uInt ndim (rownr_t rownr);

    // Get the shape of the array in the given row.
    // This is the same for all rows.
    virtual IPosition shape (rownr_t rownr);

    // Let the column object initialize itself for a newly created table.
    // This is meant for a derived class.
    virtual void doCreate (ISMBucket*);

    // Let the column object initialize itself for an existing table.
    virtual void getFile (rownr_t nrrow);

    // Flush and optionally fsync the data.
    // This is meant for a derived class.
    virtual Bool flush (rownr_t nrrow, Bool fsync);

    // Resync the storage manager with the new file contents.
    // It resets the last rownr put.
    void resync (rownr_t nrrow);

    // Let the column reopen its data files for read/write access.
    virtual void reopenRW();

    // Get a scalar value in the given row.
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
    // </group>

    // Put a scalar value in the given row.
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
    // </group>

    // Get the scalar values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumn function).
    virtual void getScalarColumnV (ArrayBase& dataPtr);

    // Put the scalar values into the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn putColumn function).
    virtual void putScalarColumnV (const ArrayBase& dataPtr);

    // Get the scalar values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumnCells function).
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
                                        ArrayBase& dataPtr);

    // Get an array value in the given row.
    virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);

    // Put an array value in the given row.
    virtual void putArrayV (rownr_t rownr, const ArrayBase& dataPtr);

    // Add (newNrrow-oldNrrow) rows to the column and initialize
    // the new rows when needed.
    virtual void addRow (rownr_t newNrrow, rownr_t oldNrrow);

    // Remove the given row in the bucket from the column.
    void remove (rownr_t bucketRownr, ISMBucket* bucket, rownr_t bucketNrrow,
		 rownr_t newNrrow);

    // Get the function needed to read/write a uInt and rownr from/to
    // external format. This is used by other classes to read the length
    // of a variable data value.
    // <group>
    static Conversion::ValueFunction* getReaduInt   (Bool asCanonical);
    static Conversion::ValueFunction* getReadRownr  (Bool asCanonical);
    static Conversion::ValueFunction* getWriteuInt  (Bool asCanonical);
    static Conversion::ValueFunction* getWriteRownr (Bool asCanonical);
    // </group>

    // Give a derived class the opportunity to react on the duplication
    // of a value. It is used by ISMIndColumn.
    virtual void handleCopy (rownr_t rownr, const char* value);

    // Give a derived class the opportunity to react on the removal
    // of a value. It is used by ISMIndColumn.
    virtual void handleRemove (rownr_t rownr, const char* value);

    // Get the fixed length of the data value in a cell of this column
    // (0 = variable length).
    uInt getFixedLength() const;

    // Get the nr of elements in this data value.
    uInt nelements() const;


protected:
    // Test if the last value is invalid for this row.
    int isLastValueInvalid (Int64 rownr) const;

    // Get the value for this row.
    // Set the cache if the flag is set.
    void getValue (rownr_t rownr, void* value, Bool setCache);

    // Put the value for this row.
    void putValue (rownr_t rownr, const void* value);

    //# Declare member variables.
    // Pointer to the parent storage manager.
    ISMBase*          stmanPtr_p;
    // Length of column cell value in storage format (0 = variable length).
    // If 0, the value is always preceeded by a uInt giving the length.
    uInt              fixedLength_p;
    // Column sequence number of this column.
    uInt              colnr_p;
    // The shape of the column.
    IPosition         shape_p;
    // Number of elements in a value for this column.
    uInt              nrelem_p;
    // Number of values to be copied.
    // Normally this is nrelem_p, but for complex types it is 2*nrelem_p.
    // When local format is used, it is the number of bytes.
    uInt              nrcopy_p;
    // Cache for interval for which last value read is valid.
    // The last value is valid for startRow_p till endRow_p (inclusive).
    Int64             startRow_p;
    Int64             endRow_p;
    void*             lastValue_p;
    // The last row for which a value has been put.
    rownr_t           lastRowPut_p;
    // The size of the data type in local format.
    uInt              typeSize_p;
    // Pointer to a convert function for writing.
    Conversion::ValueFunction* writeFunc_p;
    // Pointer to a convert function for reading.
    Conversion::ValueFunction* readFunc_p;
    // Pointer to a compare function.
    ObjCompareFunc*   compareFunc_p;


private:
    // Forbid copy constructor.
    ISMColumn (const ISMColumn&);

    // Forbid assignment.
    ISMColumn& operator= (const ISMColumn&);

    // Initialize part of the object.
    // It is used by doCreate and getFile.
    void init();

    // Clear the object (used by destructor and init).
    void clear();

    // Put the value in all buckets from the given row on.
    void putFromRow (rownr_t rownr, const char* data, uInt lenData);

    // Put a data value into the bucket.
    // When it is at the first row of the bucket, it replaces the value.
    // Otherwise it is added.
    void putData (ISMBucket* bucket, rownr_t bucketStartRow,
		  rownr_t bucketNrrow, rownr_t bucketRownr,
		  const char* data, uInt lenData,
		  Bool afterLastRow, Bool canSplit);

    // Replace a value at the given offset in the bucket.
    // If the bucket is too small, it will be split (if allowed).
    void replaceData (ISMBucket* bucket, rownr_t bucketStartRow,
		      rownr_t bucketNrrow, rownr_t bucketRownr, uInt& offset,
		      const char* data, uInt lenData, Bool canSplit = True);

    // Add a value at the given index in the bucket.
    // If the bucket is too small, it will be split (if allowed).
    Bool addData (ISMBucket* bucket, rownr_t bucketStartRow,
		  rownr_t bucketNrrow, rownr_t bucketRownr, uInt inx,
		  const char* data, uInt lenData,
		  Bool afterLastRow = False, Bool canSplit = True);

    // Handle the duplicated values after a bucket split.
    void handleSplit (ISMBucket& bucket, const Block<Bool>& duplicated);

    // Compare the values.
    virtual Bool compareValue (const void* val1, const void* val2) const;

    // Handle a String in copying to/from external format.
    // <group>
    static size_t fromString (void* out, const void* in, size_t n,
                              Conversion::ValueFunction* writeLeng);
    static size_t toString (void* out, const void* in, size_t n,
                            Conversion::ValueFunction* readLeng);
    static size_t writeStringBE (void* out, const void* in, size_t n);
    static size_t readStringBE (void* out, const void* in, size_t n);
    static size_t writeStringLE (void* out, const void* in, size_t n);
    static size_t readStringLE (void* out, const void* in, size_t n);
    // </group>

    void getScaCol (Vector<Bool>&);
    void getScaCol (Vector<uChar>&);
    void getScaCol (Vector<Short>&);
    void getScaCol (Vector<uShort>&);
    void getScaCol (Vector<Int>&);
    void getScaCol (Vector<uInt>&);
    void getScaCol (Vector<Int64>&);
    void getScaCol (Vector<float>&);
    void getScaCol (Vector<double>&);
    void getScaCol (Vector<Complex>&);
    void getScaCol (Vector<DComplex>&);
    void getScaCol (Vector<String>&);

    void getScaColCells (const RefRows&, Vector<Bool>&);
    void getScaColCells (const RefRows&, Vector<uChar>&);
    void getScaColCells (const RefRows&, Vector<Short>&);
    void getScaColCells (const RefRows&, Vector<uShort>&);
    void getScaColCells (const RefRows&, Vector<Int>&);
    void getScaColCells (const RefRows&, Vector<uInt>&);
    void getScaColCells (const RefRows&, Vector<Int64>&);
    void getScaColCells (const RefRows&, Vector<float>&);
    void getScaColCells (const RefRows&, Vector<double>&);
    void getScaColCells (const RefRows&, Vector<Complex>&);
    void getScaColCells (const RefRows&, Vector<DComplex>&);
    void getScaColCells (const RefRows&, Vector<String>&);

    void putScaCol (const Vector<Bool>&);
    void putScaCol (const Vector<uChar>&);
    void putScaCol (const Vector<Short>&);
    void putScaCol (const Vector<uShort>&);
    void putScaCol (const Vector<Int>&);
    void putScaCol (const Vector<uInt>&);
    void putScaCol (const Vector<Int64>&);
    void putScaCol (const Vector<float>&);
    void putScaCol (const Vector<double>&);
    void putScaCol (const Vector<Complex>&);
    void putScaCol (const Vector<DComplex>&);
    void putScaCol (const Vector<String>&);
};


  inline int ISMColumn::isLastValueInvalid (Int64 rownr) const
{
    return rownr < startRow_p  ||  rownr > endRow_p;
}

inline uInt ISMColumn::getFixedLength() const
{
    return fixedLength_p;
}

inline uInt ISMColumn::nelements() const
{
    return nrelem_p;
}



} //# NAMESPACE CASACORE - END

#endif
