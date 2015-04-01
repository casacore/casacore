//# BinTable.h: The class BinaryTable converts a FITS binary table into a Casacore Table.
//# Copyright (C) 1995,1996,1999
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

#ifndef FITS_BINTABLE_H
#define FITS_BINTABLE_H


//# Includes

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// BinaryTable is used to translate a FITS binary table to a Casacore Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tBinTable">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> FitsInput
//   <li> HeaderDataUnit
//   <li> BinaryTableExtension
//   <li> Tables module
// </prerequisite>
//
// <etymology>
// BinaryTable inherits from the FITS BinaryTableExtension class and its
// primary use is to convert that class to a Casacore Table.  This explains
// it's use but not its name.  A better name should be found.
// </etymology>
//
// <synopsis> 
// The class starts with an already existing FitsInput object, which should
// be set at a BinaryTableExtension HDU.   Member functions provide a TableDesc 
// appropriate for the FITS data (to help in constructing a Casacore Table
// compatible with the BinaryTableExtension), a Table containing the
// current row of FITS data and a Table containing the next row of FITS data
// (which can be used to step through the FitsInput, copying each row
// using the RowCopier class), and a Table containin the entire FITS binary 
// table from the current row to the end of the table.
// </synopsis> 
//
// <motivation>
// We need a way to get FITS data into Casacore Tables.
// </motivation>
//
// <example>
// open a FitsInput from a disk file, if the HDU is a BinaryTableExtension,
// then instantiate a BinTable object and get the entire table.  A fair 
// amount of error checking has been eliminated from this example.
// <srcblock>
//    FitsInput infits("myFITSFile", FITS::Disk);
//    switch (infits.hdutype()) {
//       case FITS::BinaryTableHDU:
//          BinaryTable bintab(infits);
//          Table tab = bintable.fullTable("myTable");
//          break;
//    }
// </srcblock>
// There would obviously be other cases to the switch to deal with any
// other HDUs (e.g. skip them via infits.skip_hdu()).  The Table destructor
// would write "myTable" to disk.
// </example>
//
//
// <todo asof="1995/04/10">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> It would be nice to construct this directly from the BinaryTableExtension.
//   <li> When random access FITS becomes available, this needs to be able to deal with that.
//   <li> A corresponding class is needed for conversion from Casacore Tables to FITS.
//   <li> Throw exceptions rather than send messages to cout : however the entire FITS
//        module behaves this way, so it should all remain consistent.
//   <li> The following types of columns are not dealt with very well or at all
//          (Bit, Byte, 0-length columns).
//   <li> No attempt use any TDIM columns or keywords to shape arrays.
// </todo>

class BinaryTable : public BinaryTableExtension
{
public: 

    //   The only constructor is from a FitsInput, you can also optionally
    //   provide a FITS error handler.  If useMiriadSM is True, use
    //   the Miriad storage manager for all columns, otherwise AipsIO.
    //   If sdfits is True, all non-reserved and some reserved keyword
    //   are treated as if they were columns with constant values
    //   "virtual columns" in the sdfits convention.
    BinaryTable(FitsInput &,
		FITSErrorHandler errhandler = FITSError::defaultHandler, 
		Bool useMiriadSM = False, Bool sdfits = False);

    ~BinaryTable();

    // Get the full table, using the supplied arguments to construct the table.
    // The table will contain all data from the current row to the end of the
    // BinarTableExtension.If useMiriadSM is True, use the Miriad storage
    // manager for all columns, otherwise AipsIO.
    Table fullTable(const String& tabName, 
		    const Table::TableOption = Table::NewNoReplace,
		    Bool useMiriadSM = False);

    // This version  of the fullTable return a Memory based table
    // Its recommended if its being used as a temporary

    Table fullTable();

    //	Get an appropriate TableDesc (this is the same TableDesc used to 
    // construct any Table objects returned by this class.
    const TableDesc& getDescriptor();

    //	Return the Table keywords (this is the same TableRecord used
    //  in any Table objects returned by this class.
    TableRecord& getKeywords();

    // Get a Table with a single row, the current row of the FITS table.
    // The returned Table is a Scratch table.
    // The standard BinaryTableExtension manipulation functions are
    // available to position the FITS input at the desired location.
    const Table &thisRow();

    // Get a Table with a single row, the next row of the FITS table.
    // The returned Table is a Scratch table.
    // The FITS input is positioned to the next row and the values translated
    // and returned in a Table object.
    const Table &nextRow();


private:

    //# Data Members
    // This is the Scratch table containing the current row
    Table* currRowTab;
    // The number of elements for each column of the BinaryTableExtension
    Int *nelem;
    // This is a map from column number to column name
    SimpleOrderedMap<Int, String> *colNames;

    TableRecord kwSet;

    // These are used by any VADesc columns
    FITS::ValueType *vatypes_p;
    void **vaptr_p;
    VADescFitsField *va_p;
    char *theheap_p;

    // this is the function that fills each row in as needed
    void fillRow();
};


} //# NAMESPACE CASACORE - END

#endif
