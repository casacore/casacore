//# SDFITSTable.h : this defines SDFITSTable,  a FITSTable following the SD convention
//# Copyright (C) 1997,1999
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

#ifndef FITS_SDFITSTABLE_H
#define FITS_SDFITSTABLE_H


#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// SDFITSTable is a FITSTable which follows the Single Dish FITS Convention.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> FITSTable
// </prerequisite>
//
// <etymology>
// SDFITSTable is derived from FITSTable.  It contains additional
// checks and behaviour appropriate to the Single Dish FITS Convention
// hence this is a Single Dish FITS Table, or SDFITSTable.
// </etymology>
//
// <synopsis>
// This class behaves much like FITSTable.  It additionally verifies
// that the indicated HDU in the input FITS file follows the SDFITS
// convention (it has all of the required columns) and it treats
// keywords as virtual columns when appropriate.  These virtual
// columns will appear as fields in the currentRecord and description
// and will NOT appear in the keywords.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// It was useful to encapsulate this behaviour in a class so that
// the checks on a valid SDFITS table and the treatment of keywords
// as virtual columns would not need to appear everywhere it might
// be used.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="1997/01/14">
//   <li> everything
// </todo>

class SDFITSTable : public FITSTable
{
public:
    // the core keywords, UNKNOWN is not a core keyword,
    // NUM_CORE_KEYWORDS is a place holder
    enum CoreKeyword { OBJECT, TELESCOP, BANDWID, DATEOBS,
		       EXPOSURE, TSYS, NUM_CORE_KEYWORDS, 
		       UNKNOWN=NUM_CORE_KEYWORDS };

    // construct from a file
    SDFITSTable(const String &fileName, uInt whichHDU=1);

    // The destructor
    ~SDFITSTable();

    // Attach this SDFITSTable to a new file name, same HDU# as at open time
    virtual Bool reopen(const String &fileName);

    // is this a valid SDFITS file
    virtual Bool isSDFITS() const { return isSDFITS_p;}

    // translate to/from core keyword names to enumeration
    static CoreKeyword coreKeyword(const String& name);
    static String coreKeywordName(CoreKeyword kw);

private:
    Bool isSDFITS_p;

    // block of core keyword names
    static Block<String> kwNames;
    // kwNames initialization function
    static void init_kwNames();

    // check to see if the named keyword should
    // be turned into a column, all non-reserved keywords will
    // always be turned into a column.
    static Bool isSDFitsColumn(const String& name);

    // the array of keyword names
    // the regular FITSTable::reopen does nearly everything fine,
    // this function moves stuff out of the keywords and into the
    // output record as appropriate
    void sdfits_shuffle();

    // undefined an inaccessible
    SDFITSTable();
    SDFITSTable(const SDFITSTable &);
    SDFITSTable &operator=(const SDFITSTable &);
};

} //# NAMESPACE CASACORE - END

#endif


