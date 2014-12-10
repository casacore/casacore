//# FITSTable.h: Simplified interface to FITS tables with Casacore Look and Feel.
//# Copyright (C) 1995,1996,1997,1999,2000,2001
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

#ifndef FITS_FITSTABLE_H
#define FITS_FITSTABLE_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
class FITSFieldCopier;
class TableDesc;
template<class T> class Vector;

// <summary>
// Simplified interface to FITS tables with Casacore Look and Feel.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> General knowledge of FITS binary and ASCII tables.
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// FITSTablular is an obstract base class which is used for read-only access to
// tabular FITS-like data structures.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1995/06/01">
//   <li> Eventually we'd like to be able to write the tables as well as read
//        them.
// </todo>

class FITSTabular
{
public:
    virtual ~FITSTabular();
    // isValid() returns False if this object isn't a valid Tabular data
    // structure.
    virtual Bool isValid() const = 0;
    // Returns keywords which are associated with the underlying FITS files.
    virtual const TableRecord &keywords() const = 0;
    // Returns the description of the underlying FITS table.
    virtual const RecordDesc &description() const = 0;
    // Returns any TUNITnnn associated with a column (the field names
    // are the column names, each field value is the TUNITnnn value for
    // that field).  Note that only those columns with a non-empty
    // TUNITnnn have an entry in the units() Record. 
    virtual const Record &units() const = 0;
    // Returns any TDISPnnn associated with a column (the field names
    // are the column names, each field value is the TDISPnnn value for
    // that field).  Note that only those columns with a non-empty
    // TDISPnnn have an entry in the displayFormats() Record.
    virtual const Record &displayFormats() const = 0;
    // Returns any TNULLnnn associated with a column (the field names
    // are the column names, each field value is the TNULLnnn value for
    // that field).  Note that only those columns with a specific entry for
    // TNULLnnn and which have not been promoted to doubles due TSCAL
    // and TZERO values will have an entry in the nulls() Record.
    // The meaning of TNULL is only defined for integer and byte columns. 
    // When a column is promoted to a double because of scaling,
    // any TNULL values will be assigned a value of NaN.
    virtual const Record &nulls() const = 0;

    // Returns True if we have advanced past the end of data.
    virtual Bool pastEnd() const = 0;

    // Advance the row if possible (guaranteed harmless if pastEnd() is True.
    virtual void next() = 0;

    // Reopen the table, default behavior is to do nothing, return False
    virtual Bool reopen(const String&) { return False; }

    // return the name
    virtual const String &name() const = 0;

    // Has the description changed since construction, default is False
    virtual Bool hasChanged() const { return False;}
    // reset the changed flag, default do nothing
    virtual void resetChangedFlag() {;}

    // Return the currentRow. This is guaranteed to be valid so long as only
    // member functions of this base class are called (so you can safely attach
    // RecordFieldPtr objects to it. The result is undefined if pastEnd() is True.
    virtual const Record &currentRow() const = 0;

    // Helper function for retrieving keywords from a native-FITS hdu.
    // If allKeywords is not True, some keywords will be excluded
    // from the list.  Currently the list of excluded keywords
    // includes TTYPEnnn, TFORMnnn, and TUNITnnn
    static TableRecord keywordsFromHDU(HeaderDataUnit &hdu,
				       Bool allKeywords = False);

    // Helper function for retrieving a description from a native-FITS hdu.
    static RecordDesc descriptionFromHDU(BinaryTableExtension &hdu);

    // Help function for retrieving any shape information from String columns
    // using the SubString convention.
    // Information is returned in a Record having named fields = all String
    // columns following those convention. Each of these fields is, in turn,
    // a sub-record having these three fields: NCHAR, NELEM, DELIM.  
    // If NELEM == -1 then there must have been a DELIM specified and
    // this field is a variable shaped string array where each element has
    // at most NCHAR and they are separated by DELIM (which is a String field here).
    // Otherwise, DELIM is not used and there are NCHAR per element for each
    // of NELEM in each cell for this column.
    static Record subStringShapeFromHDU(BinaryTableExtension &hdu);

    // Helper function for retrieving the TUNITnnn from a native-FITS hdu.
    static Record unitsFromHDU(BinaryTableExtension &hdu);

    // Helper function for retrieving the TDISPnnn from a native-FITS hdu.
    static Record displayFormatsFromHDU(BinaryTableExtension &hdu);

    // Helper function for retrieving the TNULLnnn from a native-FITS hdu.
    static Record nullsFromHDU(BinaryTableExtension &hdu);

    // Get a TableDesc appropriate to hold a FITSTabular
    // the keywords, description, units, displayFormats, and nulls are all used
    static TableDesc tableDesc(const FITSTabular &fitstabular);
};

// <summary>
// Attach a FITSTabular to a binary or ASCII table
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> General knowledge of FITS binary and ASCII tables.
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// FITSTable is a FITSTabular which is attached to a FITS table (on disk only
// presently), either Binary or ASCII.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1995/06/01">
//   <li>
// </todo>

class FITSTable : public FITSTabular
{
public:
    // this creates an invalid (isValid() return False) FITSTable
    // Its primary purpose is so that FITSTables can be created before
    // the file name is known.  reopen() is then used to open the file.
    FITSTable(uInt whichHDU=1, Bool allKeywords=False);

    // 0-relative HDU. It can never be zero by the FITS rules.
    // allKeywords is passed to FITSTabular::keywordsFromHDU
    // See the documentation for that function for a list of
    // excluded keywords when allKeywords is False.
    FITSTable(const String &fileName, uInt whichHDU=1,
	      Bool allKeywords = False);
    ~FITSTable() { clear_self();}

    // Has the end of file been reached yet
    virtual Bool eof() const {return io_p->eof();}

    // Attach this FITSTable to a new file name, same HDU# as at open time
    virtual Bool reopen(const String &fileName);
    virtual const String& name() const { return name_p;}

    virtual Bool isValid() const {return isValid_p;}

    virtual const TableRecord &keywords() const {return keywords_p;}
    virtual const RecordDesc &description() const {return description_p;}
    virtual const Record &units() const {return units_p;}
    virtual const Record &displayFormats() const {return disps_p;}
    virtual const Record &nulls() const {return nulls_p;}

    virtual Bool pastEnd() const;
    virtual void next();
    virtual const Record &currentRow() const;

    // single FITS tables know how many rows there are
    // unlike general FITSTabulars, which may not know
    // (e.g. if it is a FITSMultiTable)
    virtual uInt nrow() const {return raw_table_p->nrows();}

    // these tables should also know where they are
    virtual Int rownr() const {return row_nr_p;}

    // and it should be possible to move to a desired row
    // the rownr() member can be used to verify that a move 
    // was successful - this will happen if the requested row
    // was < rownr() or >= nrow() - i.e. movements backwards or
    // beyond the end of the file are not possible.
    virtual void move(Int torow);

    // the keywords from the Primary HDU
    virtual const TableRecord &primaryKeywords() const {return primaryKeys_p;}
protected:
    // SDFITSTable needs to make some keywords appear as
    // columns, this requires access to description_p, keywords_p, and
    // row_p.  However, its not something that typical FITSTable 
    // users will want.  Therefore, I've provided this protected
    // function for SDFITSTable to use so as to not have to provide
    // direct access to those data members at the public level.
    // The named keywords and values are appended to the end of
    // row_p and removed from keywords_p, description_p is modified
    // appropriately.  The returned value is False if any named
    // keyword did not appear in keywords_p (however, all named
    // keywords that DO appear in keywords_p will have been correctly
    // moved).
    Bool virtualColumns(const Vector<String>& keyNames); 
private:
    // Undefined and inaccessible. An alternative would be to use reference
    // semantics like Table.
    FITSTable(const FITSTable &);
    FITSTable &operator=(const FITSTable &);

    void fill_row();
    void clear_self();

    Bool isValid_p;

    String name_p;

    uInt hdu_nr_p;

    Int row_nr_p;
    BinaryTableExtension *raw_table_p;
    FitsInput *io_p;
    TableRecord keywords_p;
    TableRecord primaryKeys_p;
    RecordDesc description_p;
    Record row_p;
    Record units_p;
    Record disps_p;
    Record nulls_p;
    Record subStrShapes_p;
    Bool allKeys_p;
    // One per field in row_p, of the right type. i.e. casting required.
    uInt nfields_p;
    Block<void *> row_fields_p;
    Block<Int> field_types_p;
    Block<Bool> promoted_p;
    Block<Int> tdims_p;
    // these are used by VADESC columns
    Block<Int> vatypes_p;
    Block<void *> vaptr_p;
    // I had trouble making a Block<VADescFitsField>
    VADescFitsField *va_p;
    char *theheap_p;

    // It is necessary to read the PDA to get the primary keywords.
    // If there is any data there, the FITS classes do not provide any way to
    // just skip over them to get to the next HDU.  The only way to do that is
    // to actually read all of the data.  If there is no data, this step is
    // unnecessary and so this subroutine need only be called after the primary
    // keywords have been read AND the PDA has some data in it.  Closing the
    // FitsInput and reopening it is faster in most cases than reading in each
    // data value.
    void reopenAtFirstHDU(const String &name);
};

// <summary>
// Simplified interface to create and write to a FITS Binary Table
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> General knowledge of FITS binary and ASCII tables.
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
//
// <todo asof="1995/06/01">
//   <li> probably much
// </todo>

class FITSTableWriter
{
public:
    enum {DefaultMaxStringSize = 16};

    // You MUST have already written a first HDU to FitsOutput.
    // description contains the names and types of the table columns to be written.
    // The row is not rearranged (i.e. they are used in order) for alignment purposes.
    // Array columns must have fixed shape unless tdimColumns is used. Use the 
    // maxLengths record to indicate any string columns which should have a length other 
    // than the default value by providing an int field of the same name as the string 
    // field in this record.
    // The size of the table (nrows) must be given at creation.  Use extraKeywords to
    // indicate any keywords not automatically created.  The units record is used to
    // indicate the units for any column.  Provide a string field with the same name as
    // the column field in description.  If freeOutput is True, file must come from new
    // since it will be deleted upon destruction.  You might not want this to happen if
    // you are going to write many tables to the same fits file.  Use variableShapes to
    // signal which array columns have variable shape and use maxLengths to indicate
    // the maximum size of those variable shaped columns.  The variableShapes record
    // should contain a String corresponding to the longest TDIM value appropriate for
    // each variable shaped column.  The maxLengths record should contain an int field
    // for each variable shaped column which indicates the maximum number of elements
    // to be found in that column.  Unused values in any cell in the variable shaped
    // columns will be filled with zero.  These columns will use the SDFITS TDIM convention
    // where for column nnn there is a corresponding TDIMnnn column where
    // the values in each row are the true shape of the data in column nnn.
    // variableShapes appears as the last argument for backwards compatibility with
    // existing code.
    FITSTableWriter(FitsOutput *file, 
		    const RecordDesc &description,
		    const Record &maxLengths,
		    uInt nrows,
		    const Record &extraKeywords,
		    const Record &units,
		    Bool freeOutput = True,
		    const Record &variableShapes = Record());

    ~FITSTableWriter();

    // use this to set the value of the current row to be written
    RecordInterface &row() {return row_p;}

    // Write the current row()
    void write();

    // Don't delete this out from under us!
    FitsOutput *writer() {return writer_p;}

    // Returns a writer, with the first HDU filled in (set to null). The caller
    // is responsible for deleting the pointer returned from makeWriter.
    static FitsOutput *makeWriter(const String &fileName);
private:
    // Undefined and inaccessible
    FITSTableWriter();
    FITSTableWriter(const FITSTableWriter&);
    FITSTableWriter& operator=(const FITSTableWriter&);

    Bool delete_writer_p;
    FitsOutput *writer_p;
    uInt nrows_written_p;
    BinaryTableExtension *bintable_p;
    Record row_p;
    PtrBlock<FITSFieldCopier *> copiers_p;
};

// <summary>
// Simplified interface to create and write to FITS random groups
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> General knowledge of FITS binary and ASCII tables.
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// Like FITSTableWriter except that this must be the first HDU and
// all "columns" in the description must have the same type, i.e. float.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1995/06/01">
//   <li> probably much
// </todo>

class FITSGroupWriter
{
public:
    // Since this must always be the first HDU, there is no point in constructing it
    // with a FitsOutput.  description indicates the names of the random groups parameters.
    // nrows is a synonym for ngroups.  Use extraKeywords to
    // indicate any keywords not automatically created (SIMPLE, BITPIX, NAXIS*, EXTEND,
    // BLOCKED, GROUPS, PCOUNT, GOUNT, ORIGIN, END). If freeOutput is True, file will be
    // deleted by the destructor.  You might not want this to happen if
    // you are going to write any extensions to the same fits file.  You can get the
    // FitsOutput used here from write()
    FITSGroupWriter(const String &fileName,
		    const RecordDesc &description,
		    uInt nrows,
		    const Record &extraKeywords,
		    Bool freeOutput = True);

    ~FITSGroupWriter();

    // Set the values for the current group
    RecordInterface &row() {return row_p;}

    // Write the current group (row()).
    void write();

    // Don't delete this out from under us!
    FitsOutput *writer() {return writer_p;}
private:
    // Undefined and inaccessible
    FITSGroupWriter();
    FITSGroupWriter(const FITSGroupWriter&);
    FITSGroupWriter& operator=(const FITSGroupWriter&);

    Bool delete_writer_p;
    FitsOutput *writer_p;
    uInt nrows_written_p, nrows_total_p;
    PrimaryGroup<Float> *group_p;
    Record row_p;
    Int error_count_p;

    // Checks error status of writer_p and group_p. Cleans up and throws an exception if bad.
    void check_error(const char *extra_info = 0);
};

} //# NAMESPACE CASACORE - END

#endif
