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
#include <casacore/tables/DataMan/StManColumn.h>
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


class ISMColumn : public StManColumn
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
    virtual uInt ndim (uInt rownr);

    // Get the shape of the array in the given row.
    // This is the same for all rows.
    virtual IPosition shape (uInt rownr);

    // Let the column object initialize itself for a newly created table.
    // This is meant for a derived class.
    virtual void doCreate (ISMBucket*);

    // Let the column object initialize itself for an existing table.
    virtual void getFile (uInt nrrow);

    // Flush and optionally fsync the data.
    // This is meant for a derived class.
    virtual Bool flush (uInt nrrow, Bool fsync);

    // Resync the storage manager with the new file contents.
    // It resets the last rownr put.
    void resync (uInt nrrow);

    // Let the column reopen its data files for read/write access.
    virtual void reopenRW();

    // Get a scalar value in the given row.
    // <group>
    virtual void getBoolV     (uInt rownr, Bool* dataPtr);
    virtual void getuCharV    (uInt rownr, uChar* dataPtr);
    virtual void getShortV    (uInt rownr, Short* dataPtr);
    virtual void getuShortV   (uInt rownr, uShort* dataPtr);
    virtual void getIntV      (uInt rownr, Int* dataPtr);
    virtual void getuIntV     (uInt rownr, uInt* dataPtr);
    virtual void getfloatV    (uInt rownr, float* dataPtr);
    virtual void getdoubleV   (uInt rownr, double* dataPtr);
    virtual void getComplexV  (uInt rownr, Complex* dataPtr);
    virtual void getDComplexV (uInt rownr, DComplex* dataPtr);
    virtual void getStringV   (uInt rownr, String* dataPtr);
    // </group>

    // Put a scalar value in the given row.
    // <group>
    virtual void putBoolV     (uInt rownr, const Bool* dataPtr);
    virtual void putuCharV    (uInt rownr, const uChar* dataPtr);
    virtual void putShortV    (uInt rownr, const Short* dataPtr);
    virtual void putuShortV   (uInt rownr, const uShort* dataPtr);
    virtual void putIntV      (uInt rownr, const Int* dataPtr);
    virtual void putuIntV     (uInt rownr, const uInt* dataPtr);
    virtual void putfloatV    (uInt rownr, const float* dataPtr);
    virtual void putdoubleV   (uInt rownr, const double* dataPtr);
    virtual void putComplexV  (uInt rownr, const Complex* dataPtr);
    virtual void putDComplexV (uInt rownr, const DComplex* dataPtr);
    virtual void putStringV   (uInt rownr, const String* dataPtr);
    // </group>

    // Get the scalar values in the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumn function).
    // <group>
    virtual void getScalarColumnBoolV     (Vector<Bool>* dataPtr);
    virtual void getScalarColumnuCharV    (Vector<uChar>* dataPtr);
    virtual void getScalarColumnShortV    (Vector<Short>* dataPtr);
    virtual void getScalarColumnuShortV   (Vector<uShort>* dataPtr);
    virtual void getScalarColumnIntV      (Vector<Int>* dataPtr);
    virtual void getScalarColumnuIntV     (Vector<uInt>* dataPtr);
    virtual void getScalarColumnfloatV    (Vector<float>* dataPtr);
    virtual void getScalarColumndoubleV   (Vector<double>* dataPtr);
    virtual void getScalarColumnComplexV  (Vector<Complex>* dataPtr);
    virtual void getScalarColumnDComplexV (Vector<DComplex>* dataPtr);
    virtual void getScalarColumnStringV   (Vector<String>* dataPtr);
    // </group>

    // Put the scalar values into the entire column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn putColumn function).
    // <group>
    virtual void putScalarColumnBoolV     (const Vector<Bool>* dataPtr);
    virtual void putScalarColumnuCharV    (const Vector<uChar>* dataPtr);
    virtual void putScalarColumnShortV    (const Vector<Short>* dataPtr);
    virtual void putScalarColumnuShortV   (const Vector<uShort>* dataPtr);
    virtual void putScalarColumnIntV      (const Vector<Int>* dataPtr);
    virtual void putScalarColumnuIntV     (const Vector<uInt>* dataPtr);
    virtual void putScalarColumnfloatV    (const Vector<float>* dataPtr);
    virtual void putScalarColumndoubleV   (const Vector<double>* dataPtr);
    virtual void putScalarColumnComplexV  (const Vector<Complex>* dataPtr);
    virtual void putScalarColumnDComplexV (const Vector<DComplex>* dataPtr);
    virtual void putScalarColumnStringV   (const Vector<String>* dataPtr);
    // </group>

    // Get the scalar values in some cells of the column.
    // The buffer pointed to by dataPtr has to have the correct length.
    // (which is guaranteed by the ScalarColumn getColumnCells function).
    // The default implementation loops through all rows.
    // <group>
    virtual void getScalarColumnCellsBoolV     (const RefRows& rownrs,
						Vector<Bool>* dataPtr);
    virtual void getScalarColumnCellsuCharV    (const RefRows& rownrs,
						Vector<uChar>* dataPtr);
    virtual void getScalarColumnCellsShortV    (const RefRows& rownrs,
						Vector<Short>* dataPtr);
    virtual void getScalarColumnCellsuShortV   (const RefRows& rownrs,
						Vector<uShort>* dataPtr);
    virtual void getScalarColumnCellsIntV      (const RefRows& rownrs,
						Vector<Int>* dataPtr);
    virtual void getScalarColumnCellsuIntV     (const RefRows& rownrs,
						Vector<uInt>* dataPtr);
    virtual void getScalarColumnCellsfloatV    (const RefRows& rownrs,
						Vector<float>* dataPtr);
    virtual void getScalarColumnCellsdoubleV   (const RefRows& rownrs,
						Vector<double>* dataPtr);
    virtual void getScalarColumnCellsComplexV  (const RefRows& rownrs,
						Vector<Complex>* dataPtr);
    virtual void getScalarColumnCellsDComplexV (const RefRows& rownrs,
						Vector<DComplex>* dataPtr);
    virtual void getScalarColumnCellsStringV   (const RefRows& rownrs,
						Vector<String>* dataPtr);
    // </group>

    // Get an array value in the given row.
    // <group>
    virtual void getArrayBoolV     (uInt rownr, Array<Bool>* dataPtr);
    virtual void getArrayuCharV    (uInt rownr, Array<uChar>* dataPtr);
    virtual void getArrayShortV    (uInt rownr, Array<Short>* dataPtr);
    virtual void getArrayuShortV   (uInt rownr, Array<uShort>* dataPtr);
    virtual void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
    virtual void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
    virtual void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
    virtual void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
    virtual void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
    virtual void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
    virtual void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
    // </group>

    // Put an array value in the given row.
    // <group>
    virtual void putArrayBoolV     (uInt rownr, const Array<Bool>* dataPtr);
    virtual void putArrayuCharV    (uInt rownr, const Array<uChar>* dataPtr);
    virtual void putArrayShortV    (uInt rownr, const Array<Short>* dataPtr);
    virtual void putArrayuShortV   (uInt rownr, const Array<uShort>* dataPtr);
    virtual void putArrayIntV      (uInt rownr, const Array<Int>* dataPtr);
    virtual void putArrayuIntV     (uInt rownr, const Array<uInt>* dataPtr);
    virtual void putArrayfloatV    (uInt rownr, const Array<float>* dataPtr);
    virtual void putArraydoubleV   (uInt rownr, const Array<double>* dataPtr);
    virtual void putArrayComplexV  (uInt rownr, const Array<Complex>* dataPtr);
    virtual void putArrayDComplexV (uInt rownr, const Array<DComplex>* dataPtr);
    virtual void putArrayStringV   (uInt rownr, const Array<String>* dataPtr);
    // </group>

    // Add (newNrrow-oldNrrow) rows to the column and initialize
    // the new rows when needed.
    virtual void addRow (uInt newNrrow, uInt oldNrrow);

    // Remove the given row in the bucket from the column.
    void remove (uInt bucketRownr, ISMBucket* bucket, uInt bucketNrrow,
		 uInt newNrrow);

    // Get the function needed to read/write a uInt from/to external format.
    // This is used by other classes to read the length of a variable
    // data value.
    // <group>
    static Conversion::ValueFunction* getReaduInt  (Bool asCanonical);
    static Conversion::ValueFunction* getWriteuInt (Bool asCanonical);
    // </group>

    // Give a derived class the opportunity to react on the duplication
    // of a value. It is used by ISMIndColumn.
    virtual void handleCopy (uInt rownr, const char* value);

    // Give a derived class the opportunity to react on the removal
    // of a value. It is used by ISMIndColumn.
    virtual void handleRemove (uInt rownr, const char* value);

    // Get the fixed length of the data value in a cell of this column
    // (0 = variable length).
    uInt getFixedLength() const;

    // Get the nr of elements in this data value.
    uInt nelements() const;


protected:
    // Test if the last value is invalid for this row.
    int isLastValueInvalid (Int rownr) const;

    // Get the value for this row.
    // Set the cache if the flag is set.
    void getValue (uInt rownr, void* value, Bool setCache);

    // Put the value for this row.
    void putValue (uInt rownr, const void* value);

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
    Int               startRow_p;
    Int               endRow_p;
    void*             lastValue_p;
    // The last row for which a value has been put.
    uInt              lastRowPut_p;
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
    void putFromRow (uInt rownr, const char* data, uInt lenData);

    // Put a data value into the bucket.
    // When it is at the first row of the bucket, it replaces the value.
    // Otherwise it is added.
    void putData (ISMBucket* bucket, uInt bucketStartRow,
		  uInt bucketNrrow, uInt bucketRownr,
		  const char* data, uInt lenData,
		  Bool afterLastRow, Bool canSplit);

    // Replace a value at the given offset in the bucket.
    // If the bucket is too small, it will be split (if allowed).
    void replaceData (ISMBucket* bucket, uInt bucketStartRow,
		      uInt bucketNrrow, uInt bucketRownr, uInt& offset,
		      const char* data, uInt lenData, Bool canSplit = True);

    // Add a value at the given index in the bucket.
    // If the bucket is too small, it will be split (if allowed).
    Bool addData (ISMBucket* bucket, uInt bucketStartRow,
		  uInt bucketNrrow, uInt bucketRownr, uInt inx,
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
};


inline int ISMColumn::isLastValueInvalid (Int rownr) const
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
