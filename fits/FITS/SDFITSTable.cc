//# SDFITSTable.cc: defines SDFITSTable, a FITSTable following the SD convention
//# Copyright (C) 1997,1998,1999
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

//# Includes

#include <casacore/fits/FITS/SDFITSTable.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Block<String> SDFITSTable::kwNames;

void SDFITSTable::init_kwNames()
{
  if (kwNames.nelements() != (NUM_CORE_KEYWORDS+1)) {
    kwNames.resize(NUM_CORE_KEYWORDS+1);
    kwNames[OBJECT]   = FITS::ResWord.aname(FITS::OBJECT);
    kwNames[TELESCOP] = FITS::ResWord.aname(FITS::TELESCOP);
    kwNames[BANDWID]  = "BANDWID";
    kwNames[DATEOBS]  = FITS::ResWord.aname(FITS::DATE_OBS);
    kwNames[EXPOSURE] = "EXPOSURE";
    kwNames[TSYS]     = "TSYS";
    kwNames[UNKNOWN] = "";
  }
}

SDFITSTable::CoreKeyword SDFITSTable::coreKeyword(const String& name)
{
  init_kwNames();
  uint32_t i = 0;
  while (i < NUM_CORE_KEYWORDS && kwNames[i] != name) { i++; }
  return CoreKeyword(i);
}

String SDFITSTable::coreKeywordName(CoreKeyword kw)
{
  init_kwNames();
  return kwNames[kw];
}

SDFITSTable::SDFITSTable(const String& fileName, uint32_t whichHDU)
  : FITSTable(fileName, whichHDU), isSDFITS_p(false)
{
  // check for valid (core) SDFITS keywords, move keywords to columns
  sdfits_shuffle();
}

SDFITSTable::~SDFITSTable()
{ ; }

bool SDFITSTable::reopen(const String &fileName)
{
  bool result = FITSTable::reopen(fileName);
  if (result) sdfits_shuffle();
  return result;
}

bool SDFITSTable::isSDFitsColumn(const String& name)
{
  bool result;
  // if name is not reserved, return true
  if (!FITS::ResWord.isreserved(name.chars(), name.length())) {
    result = true;
  } else if (name != FITS::ResWord.aname(FITS::COMMENT) && 
	     name != FITS::ResWord.aname(FITS::DATAMAX) && 
	     name != FITS::ResWord.aname(FITS::DATAMIN) &&
	     name != FITS::ResWord.aname(FITS::EXTLEVEL) && 
	     name != FITS::ResWord.aname(FITS::EXTNAME) && 
	     name != FITS::ResWord.aname(FITS::EXTVER) &&
	     name != FITS::ResWord.aname(FITS::HISTORY) && 
	     name != FITS::ResWord.aname(FITS::REFERENC)) {
      // all of the above might reasonably be expected to be in
      // a FITS table as keywords which should remain keywords
      // Other (e.g. BITPIX, TFIELDS, etc) which describe the
      // table, are removed by FITSTable.  Everything else
      // is a keyword which should be treated as a virtual column.
      // DATAMAX and DATAMIN above, when they appear as keywords
      // in an sdfits table, refer to the entire table and hence
      // should remain as keywords and not virtual columns.
      // When they appear as true column, then they obviously
      // should remain true columns.
    result = true;
  } else {
    result = false;
  }
  return result;
}

void SDFITSTable::sdfits_shuffle()
{
  // if its already not valid, no sense going on
  if (isValid()) {
    // shift keywords to row
    Vector<String> virtCols(keywords().nfields());
    uint32_t virtCount = 0;
    uint32_t i;
    for (i=0;i<virtCols.nelements();i++) {
      // is it already duplicated in the row ?
      String kwName=keywords().name(i);
      // only move them if there is not already a column with the same name
      // true columns always take precedence
      if (isSDFitsColumn(kwName) && !currentRow().isDefined(kwName)) {
	virtCols(virtCount++) = kwName;
      }
    }
    // virtualColumns should never return false
    AlwaysAssert(virtualColumns(virtCols(Slice(0,virtCount))), AipsError);
    // check to see that all core keywords are in currentRow()
    // stopping when the first core keyword is NOT found
    isSDFITS_p = true;
    for (i=0;i<NUM_CORE_KEYWORDS && isSDFITS();i++) {
      if (!currentRow().isDefined(coreKeywordName(CoreKeyword(i)))) {
	isSDFITS_p = false;
      }
    }
  }
}

} //# NAMESPACE CASACORE - END

