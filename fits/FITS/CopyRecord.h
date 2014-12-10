//# CopyRecord.h: Copy fields from a Record to a table or other record.
//# Copyright (C) 1995,1996,1997,1999,2000
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
//#
//# $Id$

#ifndef FITS_COPYRECORD_H
#define FITS_COPYRECORD_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

class Table;
class TableDesc;
class RecordInterface;
class RecordDesc;
class String;
template <class T> class Vector;
template <class T> class ScalarColumn;
template <class T> class ArrayColumn;

// <summary>
// Copies fields from a Record to columns of a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Record
//   <li> Table
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// This class should be generalized, and made better. It is the analog of
// RowCopier, i.e. it copies all the fields from some Record to certain
// columns of a table. The mapping from fields to columns occurs at
// construction of the CopyRecordToTable object.
// </motivation>

class CopyRecordToTable
{
public:
    // Set the mapping between fields and columns. In particular,
    // inputMap(fieldNumber) -> columnNumber.
    CopyRecordToTable(Table &outputTable, 
                      const RecordInterface &inputBuffer,
                      const Vector<Int> inputMap);

    // assignment constructor, reference semantics
    CopyRecordToTable(const CopyRecordToTable &other);

    ~CopyRecordToTable();

    // assignment operator, reference semantics
    CopyRecordToTable &operator=(const CopyRecordToTable &other);

    // Copy from the record (which must still exist) to the given row number
    // of the table (which must also still exist).
    void copy(uInt rownr);

private: 
    // We could just have a TableColumn for scalars, but we'd need all of
    // the array types anyway.
    PtrBlock<ScalarColumn<Bool> *> table_bool;
    PtrBlock<ScalarColumn<uChar> *> table_char;
    PtrBlock<ScalarColumn<Short> *> table_short;
    PtrBlock<ScalarColumn<Int> *> table_int;
    PtrBlock<ScalarColumn<Float> *> table_float;
    PtrBlock<ScalarColumn<Double> *> table_double;
    PtrBlock<ScalarColumn<Complex> *> table_complex;
    PtrBlock<ScalarColumn<DComplex> *> table_dcomplex;
    PtrBlock<ScalarColumn<String> *> table_string;
    PtrBlock<ArrayColumn<Bool> *> table_array_bool;
    PtrBlock<ArrayColumn<uChar> *> table_array_char;
    PtrBlock<ArrayColumn<Short> *> table_array_short;
    PtrBlock<ArrayColumn<Int> *> table_array_int;
    PtrBlock<ArrayColumn<Float> *> table_array_float;
    PtrBlock<ArrayColumn<Double> *> table_array_double;
    PtrBlock<ArrayColumn<Complex> *> table_array_complex;
    PtrBlock<ArrayColumn<DComplex> *> table_array_dcomplex;
    PtrBlock<ArrayColumn<String> *> table_array_string;
 
    Block<RORecordFieldPtr<Bool> > record_bool;
    Block<RORecordFieldPtr<uChar> > record_char;
    Block<RORecordFieldPtr<Short> > record_short;
    Block<RORecordFieldPtr<Int> > record_int;
    Block<RORecordFieldPtr<Float> > record_float;
    Block<RORecordFieldPtr<Double> > record_double;
    Block<RORecordFieldPtr<Complex> > record_complex;
    Block<RORecordFieldPtr<DComplex> > record_dcomplex;
    Block<RORecordFieldPtr<String> > record_string;
    Block<RORecordFieldPtr<Array<Bool> > > record_array_bool;
    Block<RORecordFieldPtr<Array<uChar> > > record_array_char;
    Block<RORecordFieldPtr<Array<Short> > > record_array_short;
    Block<RORecordFieldPtr<Array<Int> > > record_array_int;
    Block<RORecordFieldPtr<Array<Float> > > record_array_float;
    Block<RORecordFieldPtr<Array<Double> > > record_array_double;
    Block<RORecordFieldPtr<Array<Complex> > > record_array_complex;
    Block<RORecordFieldPtr<Array<DComplex> > > record_array_dcomplex;
    Block<RORecordFieldPtr<Array<String> > > record_array_string;

    void clearAll();

    // Undefined and inaccessible
    CopyRecordToTable();
};

//#!                        global functions

// This function probably doesn't belong here, but I'm not yet sure where it does belong.
// This function adds all the fields in recordDescription to tableDescription, prefixed by "prefix".
// If prefix is empty, nothing is prepended
// otherwise the string prefix + "_" is prepended to each RecordFieldPtr name in
// constructing the TableDesc
void addRecordDesc(TableDesc &tableDescription,
		   const RecordDesc &recordDescription,
		   const String &prefix);


// <summary>
// Copies fields between Records, possibly to fields with another name.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Record
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>

class CopyRecordToRecord
{
public:
    // Set the mapping between fields and columns. In particular,
    // inputMap(fieldNumber) -> outputFieldNumber.
    CopyRecordToRecord(RecordInterface &outputBuffer, 
                      const RecordInterface &inputBuffer,
                      const Vector<Int> inputMap);


    ~CopyRecordToRecord();

    // Copy from the record (which must still exist) to the 
    // output record (which must also still exist).
    void copy();

private:

    // Undefined and inaccessible
    CopyRecordToRecord();
    CopyRecordToRecord(const CopyRecordToRecord &);
    CopyRecordToRecord &operator=(const CopyRecordToRecord &);
 
    Block<RORecordFieldPtr<Bool> > in_record_bool;
    Block<RORecordFieldPtr<uChar> > in_record_char;
    Block<RORecordFieldPtr<Short> > in_record_short;
    Block<RORecordFieldPtr<Int> > in_record_int;
    Block<RORecordFieldPtr<Float> > in_record_float;
    Block<RORecordFieldPtr<Double> > in_record_double;
    Block<RORecordFieldPtr<Complex> > in_record_complex;
    Block<RORecordFieldPtr<DComplex> > in_record_dcomplex;
    Block<RORecordFieldPtr<String> > in_record_string;
    Block<RORecordFieldPtr<Array<Bool> > > in_record_array_bool;
    Block<RORecordFieldPtr<Array<uChar> > > in_record_array_char;
    Block<RORecordFieldPtr<Array<Short> > > in_record_array_short;
    Block<RORecordFieldPtr<Array<Int> > > in_record_array_int;
    Block<RORecordFieldPtr<Array<Float> > > in_record_array_float;
    Block<RORecordFieldPtr<Array<Double> > > in_record_array_double;
    Block<RORecordFieldPtr<Array<Complex> > > in_record_array_complex;
    Block<RORecordFieldPtr<Array<DComplex> > > in_record_array_dcomplex;
    Block<RORecordFieldPtr<Array<String> > > in_record_array_string;

    Block<RecordFieldPtr<Bool> > out_record_bool;
    Block<RecordFieldPtr<uChar> > out_record_char;
    Block<RecordFieldPtr<Short> > out_record_short;
    Block<RecordFieldPtr<Int> > out_record_int;
    Block<RecordFieldPtr<Float> > out_record_float;
    Block<RecordFieldPtr<Double> > out_record_double;
    Block<RecordFieldPtr<Complex> > out_record_complex;
    Block<RecordFieldPtr<DComplex> > out_record_dcomplex;
    Block<RecordFieldPtr<String> > out_record_string;
    Block<RecordFieldPtr<Array<Bool> > > out_record_array_bool;
    Block<RecordFieldPtr<Array<uChar> > > out_record_array_char;
    Block<RecordFieldPtr<Array<Short> > > out_record_array_short;
    Block<RecordFieldPtr<Array<Int> > > out_record_array_int;
    Block<RecordFieldPtr<Array<Float> > > out_record_array_float;
    Block<RecordFieldPtr<Array<Double> > > out_record_array_double;
    Block<RecordFieldPtr<Array<Complex> > > out_record_array_complex;
    Block<RecordFieldPtr<Array<DComplex> > > out_record_array_dcomplex;
    Block<RecordFieldPtr<Array<String> > > out_record_array_string;
};


} //# NAMESPACE CASACORE - END

#endif


