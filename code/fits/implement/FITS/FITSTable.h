//# FITSTable.h: Simplified interface to FITS tables with AIPS++ Look and Feel.
//# Copyright (C) 1995,1996,1997
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

#if !defined(AIPS_FITS_TABLE_H)
#define AIPS_FITS_TABLE_H

#if defined(_AIX)
#pragma implementation ("FITSTable.h")
#endif

#include <aips/aips.h>
#include <aips/Containers/Record.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Containers/Block.h>
#include <aips/FITS/hdu.h>

class String;
class FitsInput;
class FitsOutput;
class FITSFieldCopier;
class TableDesc;
template<class T> class Vector;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
class ostream;
#endif

// <summary>
// Simplified interface to FITS tables with AIPS++ Look and Feel.
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
    // 0-relative HDU. It can never be zero by the FITS rules.
    // allKeywords is passed to FITSTabular::keywordsFromHDU
    // See the documentation for that function for a list of
    // excluded keywords when allKeywords is False.
    FITSTable(const String &fileName, uInt whichHDU=1,
	      Bool allKeywords = False);
    ~FITSTable();

    // Attach this FITSTable to a new file name, same HDU# as at open time
    virtual Bool reopen(const String &fileName);
    virtual const String& name() const { return name_p;}

    virtual Bool isValid() const;

    virtual const TableRecord &keywords() const;
    virtual const RecordDesc &description() const;
    virtual const Record &units() const;
    virtual const Record &displayFormats() const;
    virtual const Record &nulls() const;

    virtual Bool pastEnd() const;
    virtual void next();
    virtual const Record &currentRow() const;
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
    FITSTable();
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
    RecordDesc description_p;
    Record row_p;
    Record units_p;
    Record disps_p;
    Record nulls_p;
    Bool allKeys_p;
    // One per field in row_p, of the right type. i.e. casting required.
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
};

class FITSTableWriter
{
public:
    enum {DefaultMaxStringSize = 16};

    // You MUST have already written a null first HDU.
    FITSTableWriter(FitsOutput *file, 
		    // The row won't be rearranged so that, e.g., DOUBLEs are
		    // first for alignment purposes. You might want to do this
		    // yourself. Arrays must be fixed shape.
		    const RecordDesc &description,
		    // Change default with int field with same name as string
		    // field in description
		    const Record &maxStringLengths,
		    uInt nrows,
		    const Record &extraKeywords,
		    // the units, where available
		    const Record &units,
		    // You might want to write many fits files in the same
		    // physical file.  If True, "file" must come from new,
		    // since it will be deleted upon destruction.
		    Bool freeOutput = True);

    ~FITSTableWriter();

    RecordInterface &row() {return row_p;}
    // Eventually return how many rows have been written
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

// Write random groups. Basically like tablewriter, except:
//   1. must be first hdu
//   2. all "columns" must be the same type, i.e. float.
class FITSGroupWriter
{
public:
    // Must always be the first HDU, so no point constructing it with a
    // FitsOutput
    FITSGroupWriter(const String &fileName,
		    // All fields must be floating point at present, maybe
		    // eventually generalize to other BITPIX's. One (and only
		    // one) of the fields must be an array.
		    const RecordDesc &description,
		    // nrows is a synonym for ngroups
		    uInt nrows,
		    // Keywords other than those that will be added 
		    // automatically. (SIMPLE, BITPIX, NAXIS*, EXTEND,
		    // BLOCKED, GROUPS, PCOUNT, GCOUNT, ORIGIN, END).
		    const Record &extraKeywords,
		    // You might want to write many fits files in the same
		    // physical file.  If True, "file" must come from new,
		    // since it will be deleted upon destruction.
		    Bool freeOutput = True);

    ~FITSGroupWriter();

    RecordInterface &row() {return row_p;}
    // Eventually return how many rows have been written
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
#endif
