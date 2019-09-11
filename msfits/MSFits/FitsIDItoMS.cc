//# FITSIDItoMS.cc: Convert a FITS-IDI binary table to an AIPS++ Table.
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//# 
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//# 
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//# 
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/msfits/MSFits/FitsIDItoMS.h> 
#include <casacore/casa/Arrays/ArrayIO.h> 
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h> 
#include <casacore/casa/Arrays/Matrix.h> 
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h> 
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>

#include <casacore/casa/OS/Directory.h>

#include <casacore/ms/MeasurementSets/MeasurementSet.h> 
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSHistoryColumns.h>
#include <casacore/ms/MeasurementSets/MSObsColumns.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/ms/MeasurementSets/MSTileLayout.h>

#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/measures/Measures/MeasTable.h>

#include <casacore/tables/Tables/Table.h> 
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ArrColDesc.h>      
#include <casacore/tables/Tables/ScaColDesc.h> 

#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ArrayColumn.h>           
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h> 
#include <casacore/tables/DataMan/StManAipsIO.h> 
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/Tables/RowCopier.h> 
#include <casacore/tables/DataMan/TiledColumnStMan.h>

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/tables/Tables/TableLock.h>

#include <casacore/casa/Utilities/Assert.h> 
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/fits/FITS/FITSSpectralUtil.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/fits/FITS/BinTable.h>
#include <casacore/tables/LogTables/NewFile.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/OS/File.h>
#include <casacore/casa/Quanta/MVTime.h>

#include <casacore/casa/iomanip.h>

#include <casacore/scimath/Mathematics/FFTPack.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//local debug switch 
int mydebug = 0;
#ifdef MYDEBUG
   mydebug = 1;
#endif 
   

// Returns the 0-based position of the key string in the map,
// which is a list of strings.  Looks for the "Which" occurrance
// of the key.
static Int getIndex(Vector<String>& map, const String& key, uInt which = 0)
{
  uInt count = 0;
  const uInt nMap = map.nelements();
  for (uInt i = 0; i < nMap; i++) {
    if (map(i) == key) {
      if (count == which) {
	return i;
      } else {
	count++;
      }
    }
  }
  return -1;
}

// Like getIndex, but only checks for containment, not exact identity
static Int getIndexContains(Vector<String>& map, const String& key, 
			    uInt which = 0)
{
  uInt count = 0;
  const uInt nMap = map.nelements();
  for (uInt i = 0; i < nMap; i++) {
    if (map(i).contains(key)) {
      if (count == which) {
	return i;
      } else {
	count++;
      }
    }
  }
  return -1;
}

Bool FITSIDItoMS1::firstMain = True; // initialize the class variable firstMain
Bool FITSIDItoMS1::firstSyscal = True; // initialize the class variable firstSyscal
Bool FITSIDItoMS1::firstWeather = True; // initialize the class variable firstWeather
Double FITSIDItoMS1::rdate = 0.; // initialize the class variable rdate
String FITSIDItoMS1::array_p = ""; // initialize the class variable array_p
std::map<Int,Int> FITSIDItoMS1::antIdFromNo; // initialize the class variable antIdFromNo
std::map<Int,Int> FITSIDItoMS1::digiLevels; // initialize the class variable digiLevels
Vector<Double> FITSIDItoMS1::effChBw;

//	
// Constructor
//	
FITSIDItoMS1::FITSIDItoMS1(FitsInput& fitsin, const String& correlat, const Int& obsType, const Bool& initFirstMain)
  : BinaryTableExtension(fitsin),
    itsNrMSKs(10),
    itsMSKC(itsNrMSKs," "),
    itsMSKN(itsNrMSKs," "),
    itsMSKV(itsNrMSKs," "),
    itsgotMSK(itsNrMSKs,False),
    ///infile_p(fitsin),
    itsObsType(obsType),
    itsCorrelat(correlat),
    msc_p(0)
{

  itsLog = new LogIO();

  //
  // Get some things to remember.
  //
  Int nfield = tfields();      // nr of fields in the FITS table
  itsNelem.resize(nfield);     // nrs of elements per field
  itsNelem = 0;
  itsIsArray.resize(nfield);   // array flags per field
  itsIsArray = False;          // assume scalar-type
  
  if(initFirstMain){
      firstMain = True;
      firstSyscal = True;
      firstWeather = True;
      weather_hasWater_p = False;
      weather_hasElectron_p = False;
      antIdFromNo.clear();
      digiLevels.clear();
      rdate = 0.;
  }
  
  //
  // Step 0: The mandatory and reserved FITS keywords have been read
  // and put into the data members by the BinaryTableExtension
  // constructor.
  //
  // Step 1: Now read the rest of the FITS keywords and put them
  // into the itsMSK... buffers (the ones with names like MSK*,
  // i.e. the MS-specific keywords) and into TableRecord itsKwSet
  // (the rest of the FITS keywords and EXTVER).
  //
  
  convertKeywords();      
  
  
  // 
  // Step 1a: Read the table.info from the MSK table keywords TYPE,
  // SUBTYPE and README, and clear the relevant MSK buffer entries.
  //
  for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
    if (itsgotMSK(ikey) && itsMSKC(ikey)==" ") {
      //
      // This is a table keyword.
      //
      if (itsMSKN(ikey) == "TYPE") {
	itsTableInfo.setType(itsMSKV(ikey));
      } else if (itsMSKN(ikey) == "SUBTYPE") {
	itsTableInfo.setSubType(itsMSKV(ikey));
      } else if (itsMSKN(ikey) == "README") {
	itsTableInfo.readmeAddLine(itsMSKV(ikey));
      }
      itsgotMSK(ikey) = False;
    }
  }
  
  //
  // Step 2: Convert the FITS field descriptions stored in the data
  // members, into TableColumn descriptions (part of itsTableDesc).
  // Also interpret the storage options contained in the MSKs (keys
  // UNIT, SHAPE and OPTIONS) and clear the relevant MSK buffer
  // entries.
  //
  
  describeColumns();
  
  //
  // Step 3: Convert the rest of the MSKs. The column-type keywords
  // are added to the TableColumn description and the table-type
  // ones are added to itsKwSet.
  //
  
  //convertMSKeywords();
  
  //
  // Step 3a: Move the table keywords from itsKwSet to itsTableDesc
  // and clean out itsKwSet.
  //
  itsTableDesc.rwKeywordSet().merge(itsKwSet,RecordInterface::RenameDuplicates);
  RecordDesc emptyDesc;
  itsKwSet.restructure(emptyDesc);
  
  //
  // Step 4: Create a single-row table, with the table
  // description just built. It will hold the "current" row and is
  // therefore called itsCurRowTab.
  //
  SetupNewTable newtab("", itsTableDesc, Table::Scratch);
  StManAipsIO stman;
  newtab.bindAll (stman);
    
  itsCurRowTab = Table(newtab, 1);

  //cout << "Created " << itsCurRowTab.tableName() << endl; 
  
  const Regex trailing(" *$"); // trailing blanks
  
  String extname(FITSIDItoMS1::extname());
  extname = extname.before(trailing);
  
  
  if(extname!="UV_DATA"){     
    //
    // Fill the one row of itsCurRowTab.
    //
    if (nrows() > 0) {
      //
      // Read the first row of the FITS table into memory.
      //
      read(1);
      //
      // Fill the single row in itsCurRowTab from memory.
      //
      fillRow();
    }
  }
}


void FITSIDItoMS1::fillRow()
{
    //
    // Loop over each field.
    //
    for (Int icol=0; icol<tfields(); icol++) {
	//		and switch on the FITS type
	TableColumn tabcol(itsCurRowTab, icol);
	switch (field(icol).fieldtype()) {
	    
	case FITS::LOGICAL:
	{
	    FitsField<FitsLogical> thisfield = *(FitsField<FitsLogical>* )&field(icol);
	    Vector<Bool> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Bool> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;
	
	case FITS::BIT:
	{
	    FitsField<FitsBit> thisfield = *(FitsField<FitsBit>* )&field(icol);
	    Vector<Bool> vec(itsNelem(icol));
	    for (uInt ie=0; ie<field(icol).nelements(); ie++) {
		vec(ie) = (int(thisfield(ie)));
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Bool> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;
	
	case FITS::BYTE:
	{
	    FitsField<unsigned char> thisfield = *(FitsField<unsigned char>* )&field(icol);
	    Vector<uChar> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<uChar> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::CHAR:
	case FITS::STRING:
	{
	    FitsField<char> thisfield = *(FitsField<char>* )&field(icol);
	    char* cptr = (char* )thisfield.data();
	    uInt length = thisfield.nelements();
	    if (itsIsArray(icol)) {
		//
		// Decode the string into a vector of strings.
		// Using whitespac,etc. as separator.
		//
		IPosition shp (tabcol.shapeColumn());
		uInt nr = shp.product();
		istringstream istr(String(cptr,length));
		Vector<String> vec;
		istr >> vec;
		ArrayColumn<String> arrcol(tabcol);
		if (vec.nelements() != nr) {
		    //
		    // Whitespace is too much; use newspace as separator.
		    // First look for the true end (remove trailing blanks).
		    // Remove leading and trailing [] if there.
		    //
		    while (length > 0 && 
			   (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
			length--;
		    }
		    if (length>1 && cptr[0] == '['  &&  cptr[length-1] == ']') {
			cptr++;
			length -= 2;
		    }
		    String str = String(cptr,length);
		    Vector<String> strvec = stringToVector (str, '\n');
		    vec.reference (strvec);
		    if (vec.nelements() != nr) {
			cerr << "**Error: " << vec.nelements()
			     << " values expected for column "
			     << tabcol.columnDesc().name()
			     << ", found " << nr << endl;
			vec.resize (nr, True);
		    }
		}
		arrcol.put(0,vec.reform(shp));
	    } else {
		//
		// Look for the true end (remove trailing blanks).
		//
		while (length > 0 && 
		       (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
		    length--;
		}
		String str = String(cptr,length);
		tabcol.putScalar(0,str);
	    }
	}
	break;

	case FITS::SHORT:
	{
	    FitsField<short> thisfield = *(FitsField<short>* )&field(icol);
	    Vector<Short> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Short> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::LONG:
	{
	    FitsField<FitsLong> thisfield = * (FitsField<FitsLong>* )&field(icol);
	    Vector<Int> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = (Int )thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Int> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::FLOAT:
	{
	    FitsField<float> thisfield = *(FitsField<float>* )&field(icol);
	    Vector<Float> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Float> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::DOUBLE:
	{
	    FitsField<double> thisfield = *(FitsField<double>* )&field(icol);
	    Vector<Double> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Double> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::COMPLEX:
	{
	    FitsField<Complex> thisfield = *(FitsField<Complex>* )&field(icol);
	    Vector<Complex> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<Complex> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::DCOMPLEX:
	{
	    FitsField<DComplex> thisfield = *(FitsField<DComplex>* )&field(icol);
	    Vector<DComplex> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
		vec(ie) = thisfield(ie);
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<DComplex> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	case FITS::ICOMPLEX:
	{
	    FitsField<IComplex> thisfield = *(FitsField<IComplex> *)&field(icol);
	    Vector<DComplex> vec(itsNelem(icol));
	    for (Int ie=0; ie<itsNelem(icol); ie++) {
	      const IComplex& val = thisfield(ie);
		vec(ie) = DComplex (val.real(), val.imag());
	    }
	    if (itsIsArray(icol)) {
		ArrayColumn<DComplex> arrcol(tabcol);
		arrcol.put(0,vec.reform(tabcol.shapeColumn()));
	    } else if (itsNelem(icol) == 1) {
		tabcol.putScalar(0,vec(0));
	    }
	}
	break;

	default:
	    // VADESC or NOVALUE (which shouldn't occur here
	    cerr << "Error: unrecognized table data type for field "
		 << icol << endl;
	    cerr << "That should not have happened" << endl;
	    continue;
	}      		// end of loop over switch
    }			// end of loop over fields
    
    //
    // Loop over all virtual columns if necessary.
    //
    if (itsKwSet.nfields() > 0) {
	for (uInt icol=0; icol<itsKwSet.nfields(); icol++) {
	    TableColumn tabcol(itsCurRowTab, itsKwSet.name(icol));
	    switch (itsKwSet.type(icol)) {
	    case TpBool:
		tabcol.putScalar(0,itsKwSet.asBool(icol));
		break;
	    case TpUChar:
		tabcol.putScalar(0,itsKwSet.asuChar(icol));
		break;
	    case TpShort:
		tabcol.putScalar(0,itsKwSet.asShort(icol));
		break;
	    case TpInt:
		tabcol.putScalar(0,itsKwSet.asInt(icol));
		break;
	    case TpUInt:
		tabcol.putScalar(0,itsKwSet.asuInt(icol));
		break;
	    case TpFloat:
		tabcol.putScalar(0,itsKwSet.asfloat(icol));
		break;
	    case TpDouble:
		tabcol.putScalar(0,itsKwSet.asdouble(icol));
		break;
	    case TpComplex:
		tabcol.putScalar(0,itsKwSet.asComplex(icol));
		break;
	    case TpDComplex:
		tabcol.putScalar(0,itsKwSet.asDComplex(icol));
		break;
	    case TpString:
		tabcol.putScalar(0,itsKwSet.asString(icol));
		break;
	    default:
		throw(AipsError("Impossible virtual column type"));
		break;
	    }
	}
    }
}


// The destructor

FITSIDItoMS1::~FITSIDItoMS1()
{
  if(!msc_p){
    delete msc_p;
  }
  delete itsLog;
}


Table FITSIDItoMS1::oldfullTable(const String& tabname)
{
    //
    // Prepare for the creation of a new table with the name requested
    // and with the same table description as itsCurRowTab.  
    //
    SetupNewTable newtab(tabname, getDescriptor(), Table::Scratch); 

    Int nRows = nrows();
    StandardStMan stanStMan (-nRows);
    newtab.bindAll(stanStMan);      

    //
    // Create an empty table with the proper number of rows.
    //
    Table full(newtab,nrows());

    // cout << "OFT Creating " << full.tableName() << endl;

    //
    // Create a row copier that will repeatedly copy the current row
    // in the single-row table itsCurRowTab to the full-size table.
    //
    RowCopier rowcop(full, itsCurRowTab);

    //
    // Loop over all rows remaining.
    //
    for (Int outrow = 0, infitsrow = currrow();
	 infitsrow < nrows(); 
	 outrow++, infitsrow++) {
	//
	// Copy the 0-th row from itsCurRowTab to the outrow-th row in
	// the full-sized table.
	//
	rowcop.copy(outrow, 0);
	//
	// Read the next input row, but don't read past the end of the
	// table.
	//
	if ((infitsrow+1) < nrows()) {
	    //
	    // Read the next row.
	    //
	    read(1);
	    //
	    // Write it in itsCurRowTab.
	    //
	    fillRow();
	}
    }

    //
    // Construct the table.info.
    //
    TableInfo& info = full.tableInfo();
    info = itsTableInfo;

    return full;
}


const TableDesc& FITSIDItoMS1::getDescriptor()
{
    return itsCurRowTab.tableDesc();
}


TableRecord& FITSIDItoMS1::getKeywords()
{
    return itsCurRowTab.rwKeywordSet();
}



const Table& FITSIDItoMS1::thisRow()
{
    return (itsCurRowTab);
}



const Table& FITSIDItoMS1::nextRow()
{
    //
    // Here, its user beware in reading past end of table i.e. just
    // the same way FITS works.
    //
    read(1);
    fillRow();
    return (itsCurRowTab);
}



//
// Convert part of the keywords in the FITS binary table extension
// header to a TableRecord (itsKwSet). MS-specific keywords, MSK...,
// are only scanned and buffered. They will be converted by
// convertMSKeywords().
//
void FITSIDItoMS1::convertKeywords()
{
    ConstFitsKeywordList& kwl = kwlist();
    kwl.first();
    const FitsKeyword* kw;
    Regex trailing(" *$"); // trailing blanks
    String kwname;
    
    //
    // Buffer for storing the MSK's, MS-specific FITS keywords.
    //
    uInt iMSK = 0; 

    //
    // Loop through the FITS keyword list.
    //
    while ((kw = kwl.next())) {

	kwname = kw->name();
	
	if (!kw->isreserved()) {
	    //
	    // Non-reserved keyword:
	    //
	    
	    //
	    // Get the name of the keyword.
	    // -- At present (1998, March 11) non-reserved FITS
	    // keywords are not recognised as indexed. The index is
	    // just considered part of the name. --
	    //
	    kwname = kw->name();

	    //
            // If the name already occurs in itsKwSet, issue a warning
            // and overwrite the old keyword.
	    //
	    if (itsKwSet.isDefined(kwname)) {
	      *itsLog << LogIO::WARN << "Duplicate keyword name : " << kwname
		      << " most recent occurrance takes precedence" << LogIO::POST;
	      itsKwSet.removeField(kwname);
	    }
	    
	    //
	    // Buffer the MS-specific keywords.
	    //
	    if (kwname(0,3)=="MSK") {
		iMSK = atoi(kwname.after(3).chars());
		if (iMSK > 0) {
		    if (iMSK > itsNrMSKs) {
			// Extend the MSK buffers with 10 elements.
			itsNrMSKs += 10;
			itsMSKC.resize(itsNrMSKs,True);
			itsMSKN.resize(itsNrMSKs,True);
			itsMSKV.resize(itsNrMSKs,True);
			itsgotMSK.resize(itsNrMSKs,True);
			for (uInt ikey=iMSK-1; ikey<itsNrMSKs; ikey++) {
			    itsgotMSK(ikey) = False;
			}
		    }
		    itsgotMSK(iMSK-1) = True;
		    //
		    // String values shorter than 8 characters are
		    // padded with blanks. Remove those.
		    //
		    String val = kw->asString();
		    val = val.before(trailing);
		    if (kwname(3,1)=="C") {
			itsMSKC(iMSK-1) = val;
		    } else if (kwname(3,1)=="N") {
			itsMSKN(iMSK-1) = val;
		    } else if (kwname(3,1)=="V") {
			itsMSKV(iMSK-1) = val;
		    } else {
			*itsLog << LogIO::WARN << "MSBinaryTable found unknown MSK keyword: "
				<< kwname << ". It will be ignored" << LogIO::POST;
		    }
		} else {
		    *itsLog << LogIO::WARN << "MSBinaryTable found unknown MSK keyword: "
			    << kwname << ". It will be ignored" << LogIO::POST;
		}
	    } 
	    else {
		
		// Add a keyword of the proper type to the keyword
		// list.
		//
		switch (kw->type()) {
		case FITS::NOVALUE: itsKwSet.define(kwname,"");
		    // NOVALUE fields become string keywords with an emtpy string.
		    *itsLog << LogIO::NORMAL << "FITS::NOVALUE found" << LogIO::POST;
		    break;
		case FITS::LOGICAL: itsKwSet.define(kwname, kw->asBool()); 
		    break;
		case FITS::CHAR: itsKwSet.define(kwname, kw->asString());
		    break;
		case FITS::STRING: itsKwSet.define(kwname, kw->asString());
		    break;
		case FITS::LONG: itsKwSet.define(kwname, kw->asInt());
		    break;
		case FITS::FLOAT: itsKwSet.define(kwname, kw->asFloat());
		    break;
		case FITS::DOUBLE: itsKwSet.define(kwname, kw->asDouble());
		    break;
		case FITS::COMPLEX: itsKwSet.define(kwname, kw->asComplex());
		    break;
		default:
		    *itsLog << LogIO::WARN << "Internal error: unrecognized table data type for keyword "
			    << kwname << " type = " << kw->type() << LogIO::POST;
		    continue;
		}
		
		//
		// Add any comment in.
		//
		itsKwSet.setComment(kwname, kw->comm());
	    }           

	} else {
	    //
	    // Reserved keywords are handled elsewhere.
	    //
	}	  	// end of if(!kw->isreserved())
    }			// end of loop over kw list

    //
    // Handle the version keyword; make it a table keyword. The
    // VERSION should be defined in a proper MSFITS file (written by
    // ms2fits), but if it is not, the version will get the value
    // FITS::minInt.
    //
    itsKwSet.define("VERSION", extver());
    
}



//
// Convert FITS field descriptions to TableColumn descriptions. Also
// take into account the storage options specified in the MSK's.
//
void FITSIDItoMS1::describeColumns()
{
    Int defaultOption = ColumnDesc::FixedShape;
    Int option = defaultOption;
    //
    // Loop over the fields in the FITS table.
    //
    Regex trailing(" *$"); // trailing blanks
    Int nfield = tfields();    // nr of fields in the FITS table

    ConstFitsKeywordList& kwl = kwlist();

    //
    // Get shape vector from MAXISn fields for UV_DATA extensions
    // and determine if there are WEIGHTS in the UV data
    //
    String extname(FITSIDItoMS1::extname());
    extname = extname.before(trailing);

    Vector<Int> maxis(0);
    if(extname=="UV_DATA"){
	const FitsKeyword* kw;
        String kwname;
        kwl.first();
        uInt ctr=0;

	weightypKwPresent_p = False;
	weightyp_p = "";
	nStokes_p = 1;
	nBand_p = 1;

        while((kw = kwl.next())){
	    kwname = kw->name();
	    if(kwname.at(0,5)=="MAXIS"){ 
		maxis.resize(++ctr,True);
		maxis(ctr-1)=kw->asInt();
//		cout << "**maxis=" << maxis << endl;
	    }
	    else if(kwname.at(0,7)=="NO_STKD"){
	        nStokes_p = kw->asInt();
//		cout << "**nStokes=" << nStokes_p << endl;
	    }
	    else if(kwname.at(0,7)=="NO_BAND"){
	        nBand_p = kw->asInt();
//		cout << "**nBand=" << nBand_p << endl;
	    }
	    else if(kwname.at(0,8)=="WEIGHTYP"){
	        weightypKwPresent_p = True;
		weightyp_p = kw->asString();
		weightyp_p.upcase();
		weightyp_p.trim();
		if(weightyp_p!="NORMAL" && weightyp_p!="CORRELAT"){
		  *itsLog << LogIO::WARN << "Found WEIGHTYP keyword with value \"" << weightyp_p
			  << "\" in UV_DATA table. Presently this keyword is ignored."
			  << LogIO::POST;
		}
	    }
	}

	if(maxis.nelements()>1){
	    if(maxis(1)==2){
		uv_data_hasWeights_p = False;
	    }
	    else if(maxis(1)==3){
		uv_data_hasWeights_p = True;
	    }
	    else{
		uv_data_hasWeights_p = False;
		*itsLog << LogIO::WARN << "Invalid value for MAXIS1 keyword in UV_DATA table "
			<< maxis(1) << " should be 2 or 3. Will try to continue ..." << LogIO::POST;
	    }
	}
	else{
	    uv_data_hasWeights_p = False;
	    *itsLog << LogIO::WARN << "Could not find MAXIS1 keyword in UV_DATA table. FITS IDI file probably invalid."
		    << LogIO::POST;
	}
    }

    for (Int icol=0; icol<nfield; icol++) {
	itsNelem(icol) = field(icol).nelements();
	
	//
	// Get the name of the field. (Names shorter than 8 characters
	// are padded with blanks. Remove those.)
	//
	String colname(ttype(icol));
	colname = colname.before(trailing);
///	cout << "Doing field " << icol << " with name " << colname << endl;

	//cout << "colname=" << colname << endl;

	if(extname=="UV_DATA" && colname=="FLUX"){
	  colname="DATA";
	}

	if(extname=="WEATHER" && colname=="WVR_H2O")
	  weather_hasWater_p = True;
	if(extname=="WEATHER" && colname=="IONOS_ELECTRON")
	  weather_hasElectron_p = True;

	//
	// Check if the name exists.
	//
	if (itsTableDesc.isColumn(colname)) {
	    //
	    // Yes, as a column name.  Append the column number to
	    // this name.
	    //
	    ostringstream newname;
	    newname << colname << "." << icol;
	    colname = newname.str();
	    //
	    // Issue a warning.
	    //
	    *itsLog << LogIO::WARN << "Duplicate column name : " << ttype(icol)
		    << " this occurance will be named " << colname << LogIO::POST;

	} else if (itsTableDesc.keywordSet().isDefined(colname)) {
	    //
	    // Yes, as a keyword name.  Rename the offending keyword;
	    // the column name takes precedence!
	    //
	    String newname = colname + "-keyword";
///	    cout << "Duplicate name (keyword&  column) : " << ttype(icol)
///		 << " keyword will be renamed " << newname << endl;
	    itsTableDesc.rwKeywordSet().renameField(newname, colname);
	}
	
	//
	// Get a shorthand Bool for array versus scalar.  
	//
	Bool isSHAPEd = False;
	String SHAPEstr = "()";
//	cout << colname << " is";
	if (field(icol).fieldtype() == FITS::CHAR
	    || field(icol).fieldtype() == FITS::STRING) {
//	    cout << " a String-type column";
	    //
	    // See whether MSK SHAPE is defined. If so: array.
	    //
	    for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
		if (itsgotMSK(ikey) && (itsMSKC(ikey)==colname)) {
		    if (itsMSKN(ikey) == "SHAPE") {
			isSHAPEd = True;
			SHAPEstr = itsMSKV(ikey);
			itsIsArray(icol) = True;
			cout << " (Array)";
			itsgotMSK(ikey) = False;
		    }
		}
	    }

	} else if (itsNelem(icol) > 1) {
	    // multi-element vector or other array
	    itsIsArray(icol) = True;
//	    cout << " a multi-element non-String-type Array column";

	} else {
//	    cout << " a non-String-type column";
	    //
	    // See whether MSK SHAPE is defined. If so: array.
	    //
	    for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
		if (itsgotMSK(ikey) && (itsMSKC(ikey)==colname)) {
		    if (itsMSKN(ikey) == "SHAPE") {
			SHAPEstr = itsMSKV(ikey);
			itsIsArray(icol) = True;
			isSHAPEd = True;
//			cout << " (0/1 element Vector)";
			itsgotMSK(ikey) = False;
		    }
		}
	    }
	}
//	cout << endl;

	//
	// Get the shape vector for arrays.
	//
	Int ndim = 1;
	IPosition shape(ndim);

	if (itsIsArray(icol)) {
	    //
	    // Array-type columns must get a shape defined. For
	    // matrices and higher-dimension arrays the shape is read
	    // from the FITS keyword TDIM, but for vectors it must be
	    // derived from the repeat count in the FITS keyword TFORM.
	    //

	    if(extname=="UV_DATA" && colname=="DATA")
	      {
		shape.resize(maxis(0));
		for (Int id=0; id<maxis(0); id++) {
		    shape(id) = maxis(id+1);
		}
//		cout << "   shape = " << shape << endl;
	      }

	    else
	      {

	    String dimstr(tdim(icol));
	    String formstr(tform(icol));
	    if (dimstr(0,1)=="(") {
		//
		// TDIM key given as a string (dim1,dim2,...). Decode
		// the substring inside the parentheses. Again,
		// strings shorter than 8 characters were padded with
		// blanks; remove those.
		// 
		dimstr = dimstr.before(trailing);
		dimstr = dimstr(1,dimstr.length()-2);
		Vector<String> dimvec(stringToVector(dimstr));
		ndim = dimvec.nelements();
		shape.resize(ndim);
		for (Int id=0; id<ndim; id++) {
		    shape(id) = atoi(dimvec(id).chars());
		}
//		cout << "   shape = " << shape << endl;

	    } else if (isSHAPEd) {
		//
		// Vector of strings or degenerated vector.  Use the
		// substring inside the parentheses as shape.
		//
		dimstr = SHAPEstr(1,SHAPEstr.length()-2);
		shape(0) = atoi(dimstr.chars());
//		cout << "   shape = " << shape << endl;

	    } else {
		//
		// This must be a normal Vector. Derive its length
		// from the repeat count in the FITS key TFORM. Again,
		// strings shorter than 8 characters were padded with
		// blanks; remove those.
		//
		formstr = formstr.before(trailing);
		formstr = formstr(0,formstr.length()-1);
		shape(0) = atoi(formstr.chars());
//		cout << "    shape = " << shape << endl;
	    }

	      }

	    //
	    // Set the option for the column description. Use the
	    // value of MSK OPTIONS if that is defined for this
	    // column. Otherwise set the default option.
	    //
	    option = defaultOption;
	    for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
		if (itsgotMSK(ikey) && (itsMSKC(ikey)==colname)) {
		    if (itsMSKN(ikey) == "OPTIONS") {
			if (itsMSKV(ikey) == "DIRECT") {
			    option = ColumnDesc::Direct;
			    *itsLog << LogIO::DEBUG1 << "found MSK OPTIONS = DIRECT for "
				    << colname << LogIO::POST;
			} else {
			    *itsLog << LogIO::WARN << "Invalid MSK OPTIONS = "
				    << itsMSKV(ikey) << " is ignored." << LogIO::POST;
			}
			itsgotMSK(ikey) = False;
		    }
		}
	    }

	} else {
///	    cout << colname << " is a scalar" << endl;

	}
		
	//
	// Add a column to the table descriptor.
	//
	
	//
        // Switch on the type of column.
	//
	switch (field(icol).fieldtype()) {
	    
	case FITS::BIT:
	    // BIT stored as LOGICAL.
	    
	case FITS::LOGICAL:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Bool>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Bool>(colname,""));
	    }
	    break;
	    
	case FITS::BYTE:
	    // BYTE stored as uChar.
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<uChar>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<uChar>(colname,""));
	    }
	    break;
	    
	case FITS::SHORT:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Short>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Short>(colname,""));
	    }
	    break;
	    
	case FITS::LONG:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Int>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Int>(colname,""));
	    }
	    break;
	    
	case FITS::CHAR: 
	case FITS::STRING:
	    // was: A CHAR and STRING type is always a string, never an array.
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<String>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<String>(colname,""));
	    }
	    break;
	    
	case FITS::FLOAT:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Float>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Float>(colname,""));
	    }
	    break;
	    
	case FITS::DOUBLE:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Double>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Double>(colname,""));
	    }
	    break;
	    
	case FITS::COMPLEX:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<Complex>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<Complex>(colname,""));
	    }
	    break;
	    
	case FITS::ICOMPLEX:
	    // ICOMPLEX is promoted to DCOMPLEX so no precision is lost.
	    
	case FITS::DCOMPLEX:
	    if (itsIsArray(icol)) {
		itsTableDesc.addColumn(ArrayColumnDesc<DComplex>
				       (colname,"",shape,option));
	    } else {
		itsTableDesc.addColumn(ScalarColumnDesc<DComplex>(colname,""));
	    }
	    break;
	    
	default:
	    // VADESC or NOVALUE should not happen in a table.
	    *itsLog << LogIO::WARN << "Internal error: column " << icol
		    << " has untranslatable type " << field(icol).fieldtype() << LogIO::POST;
	    continue;
	}		// end of switch on FITS type
	
	//
	// Set the comment string if appropriate.  (I don't really
	// understand, why it must be icol+1, but only that way the
	// comments are associated with the proper column names.)
	//
	if (kwl(FITS::TTYPE, icol+1)) {
///	    cout << "icol    = " << icol << endl;
///	    cout << "colname = " << colname << endl;
///	    cout << "comment = " << kwl(FITS::TTYPE,icol+1)->comm() << endl;
	    itsTableDesc.rwColumnDesc(colname).comment() = kwl(FITS::TTYPE,icol+1)->comm();
	}

	//
	// Attach associated information.
	//
	// Units. Again, strings shorter than 8 characters were padded
	// with blanks; remove those.
	//
	String unitstr(tunit(icol));
	unitstr = unitstr.before(trailing);
	itsTableDesc.rwColumnDesc(colname).rwKeywordSet()
	    .define("UNIT", unitstr);

    }	//		end of loop over columns


/*
    //
    // For a MeasurementSet main table we will work with tiled storage
    // managers. Define the hypercolumns needed.
    //


    //String extname(FITSIDItoMS1::extname());
    //extname = extname.before(trailing);
    if (extname == "UV_DATA") {
	//
	// Define the tiled hypercube for the data and flag columns.
	//
	itsTableDesc.defineHypercolumn(
	    "TiledData",7,
	    //stringToVector(MS::columnName(MS::DATA)+","+
	    //		   MS::columnName(MS::FLAG)));

	    stringToVector(MS::columnName(MS::DATA)));

				 
	//
	// Define the tiled hypercube for the UVW column.
	//
	itsTableDesc.defineHypercolumn(
	    "TiledUVW",2,
	    stringToVector(MS::columnName(MS::UVW)));
	
    }
*/
}



//
// Convert the MS-specific keywords in the FITS binary table extension
// header to a TableRecord (itsKwSet).
//
void FITSIDItoMS1::convertMSKeywords()
{
    for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
	if (itsgotMSK(ikey)) {
	    if (itsMSKC(ikey) == " ") {
		//
		// Convert to a table keyword.
		//
///		cout << "defining table keyword " << itsMSKN(ikey) << endl;
		itsKwSet.define(itsMSKN(ikey), itsMSKV(ikey));
	    } else {
		//
		// Convert to a column keyword.
		//
///		cout << "defining column keyword " << itsMSKC(ikey)
///		     << "." << itsMSKN(ikey) << endl;
		itsTableDesc.rwColumnDesc(itsMSKC(ikey))
		    .rwKeywordSet().define(itsMSKN(ikey), itsMSKV(ikey));
	    }
	}
    }
}


void FITSIDItoMS1::getAxisInfo()
{
  *itsLog << LogOrigin("FitsIDItoMS", "getAxisInfo");
  // Extracts the axis related info. from the MAXISn fields and 
  // returns them in the form of arrays.
  const Regex trailing(" *$"); // trailing blanks
  
  ConstFitsKeywordList& kwl = kwlist();
  
  //
  // Get shape vector from MAXISn fields for UV_DATA extensions
  // Also, get associated CTYPEn CRPIXn CRVALn CDELTn values
  //
  String extname(FITSIDItoMS1::extname());
  extname = extname.before(trailing);
  Vector<Int> maxis(0);
  
  if(extname=="UV_DATA"){
    Int nAxis = 0;
    uInt imaxis = 0;
    uInt idx = 0;
    //    Bool setMAXIS = False;
    const FitsKeyword* kw;
    String kwname;
    kwl.first();
    
    
    //while((kw = kwl.next())&& setMAXIS == False) 
    while((kw = kwl.next())){
      kwname = kw->name();
      //cout << "kwname1=" << kwname <<endl;
      //cout << "kwname.length=" << kwname.length() <<endl;
      //cout << "idx=" << idx <<endl;
      if(kwname == "MAXIS"){
	nAxis = kw->asInt();
	//cout << "nAxis=" << nAxis << endl;;
        //	setMAXIS = True;
      }
    }
    if (nAxis < 1) {
      throw(AipsError( "UV_DATA has no axes!"));
    }
    nPixel_p.resize(nAxis);
    refVal_p.resize(nAxis);
    refPix_p.resize(nAxis);
    delta_p.resize(nAxis);
    coordType_p.resize(nAxis);
    
    kwl.first();
    while((kw = kwl.next())){
      kwname = kw->name();
      idx = kw->index() - 1; 
      //cout << "kwname=" << kwname <<endl;
      // Note: MAXISn are non-reserved FITS keywords
      // so 'n' does NOT recongnized as index. 
      if(kwname.at(0,5)=="MAXIS" && kwname.length()>5){ 
	nPixel_p(imaxis++)=kw->asInt();
      }
      if(kwname.at(0,5)=="CTYPE"){ 
	coordType_p(idx)=kw->asString(); 
	coordType_p(idx)=coordType_p(idx).before(trailing);
      }
      else if(kwname.at(0,5)=="CDELT"){    
	delta_p(idx)=kw->asDouble();
      }
      else if(kwname.at(0,5)=="CRPIX"){
	refPix_p(idx)=kw->asDouble();
      }
      else if(kwname.at(0,5)=="CRVAL" ){
	refVal_p(idx)=kw->asDouble();
      }
      else 
	{}
    }
    

    /**
      for(Int ctr=0;ctr<nAxis;ctr++)
	{
          //coordType_p(ctr) = priGroup_p.ctype(ctr);
          //coordType_p(ctr) = coordType_p(ctr).before(trailing);
          //refVal_p(ctr) = static_cast<Double>(priGroup_p.crval(ctr));
          //refPix_p(ctr) = static_cast<Double>(priGroup_p.crpix(ctr));
          //delta_p(ctr) = static_cast<Double>(priGroup_p.cdelt(ctr));	  
          coordType_p(ctr) = ctype(ctr);
          coordType_p(ctr) = coordType_p(ctr).before(trailing);
          refVal_p(ctr) = static_cast<Double>(crval(ctr));
          refPix_p(ctr) = static_cast<Double>(crpix(ctr));
          delta_p(ctr) = static_cast<Double>(cdelt(ctr));	  
	}
      **/

//    cout << "nPixel_p=" << nPixel_p << endl;
//    cout << "coordType_p=" << coordType_p << endl;
//    cout << "refVal_p=" << refVal_p << endl;
//    cout << "refPix_p=" << refPix_p << endl;
//    cout << "delta_p=" << delta_p << endl;
  }
  
  // Check if required axes are there
  if (getIndex(coordType_p, "COMPLEX") < 0) {
    *itsLog << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "STOKES") < 0) {
    *itsLog << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "FREQ") < 0) {
    *itsLog << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "RA") < 0) && 
      (getIndex(coordType_p, "RA---SIN") < 0) && 
      (getIndex(coordType_p, "RA---NCP") < 0) && 
      (getIndex(coordType_p, "RA---SCP") < 0)) {
    *itsLog << "Data does not have a RA axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "DEC") < 0) && 
      (getIndex(coordType_p, "DEC--SIN") < 0) && 
      (getIndex(coordType_p, "DEC--NCP") < 0) && 
      (getIndex(coordType_p, "DEC--SCP") < 0)) {
    *itsLog << "Data does not have a DEC axis" << LogIO::EXCEPTION;
  }

    
  // Sort out the order of the polarizations and find the sort indices
  // to put them in 'standard' order: PP,PQ,QP,QQ
  const uInt iPol = getIndex(coordType_p, "STOKES");
  const uInt numCorr = nPixel_p(iPol);
  corrType_p.resize(numCorr); 
  for (uInt i = 0; i < numCorr; i++) {
    // note: 1-based ref pix
    corrType_p(i) = ifloor(refVal_p(iPol) +
			   (i+1-refPix_p(iPol))*delta_p(iPol)+0.5);
    // convert AIPS-convention Stokes description to Casacore enum
//    cout << "corrType_p="<< corrType_p(i) <<endl;
    switch (corrType_p(i)) {
    case -8:
      corrType_p(i) = Stokes::YX; break;
    case -7:
      corrType_p(i) = Stokes::XY; break;
    case -6:
      corrType_p(i) = Stokes::YY; break;
    case -5:
      corrType_p(i) = Stokes::XX; break;
    case -4:
      corrType_p(i) = Stokes::LR; break;
    case -3:
      corrType_p(i) = Stokes::RL; break;
    case -2:
      corrType_p(i) = Stokes::LL; break;
    case -1:
      corrType_p(i) = Stokes::RR; break;
    case 1:
      corrType_p(i) = Stokes::XX; break;  //not correct but for a quick fix for sma data
      //corrType_p(i) = Stokes::I; break; 
    case 2:
      corrType_p(i) = Stokes::Q; break;
    case 3:
      corrType_p(i) = Stokes::U; break;
    case 4:
      corrType_p(i) = Stokes::V; break;
     
    default: 
      if (corrType_p(i) < -8 || corrType_p(i) > 4) {
	*itsLog << "Unknown Correlation type: " << corrType_p(i) 
		<< LogIO::EXCEPTION;
      }
    }
  }

  Vector<Int> tmp(corrType_p.copy());
  // Sort the polarizations to standard order. Could probably use
  // GenSortIndirect here.
  GenSort<Int>::sort(corrType_p);
  corrIndex_p.resize(numCorr);
  // Get the sort indices to rearrange the data to standard order
  for (uInt i = 0; i < numCorr; i++) {
    for (uInt j = 0; j < numCorr; j++) {
      if (corrType_p(j) == tmp(i)) corrIndex_p[i] = j;
    }
  }

  // Figure out the correlation products from the polarizations
  corrProduct_p.resize(2, numCorr); corrProduct_p = 0;
  for (uInt i = 0; i < numCorr; i++) {
    const Stokes::StokesTypes cType = Stokes::type(corrType_p(i));
    Fallible<Int> receptor = Stokes::receptor1(cType);
    if (receptor.isValid()) {
      corrProduct_p(0,i) = receptor;
//      cout << "corrProcut_p(0,"<< i <<")=" << corrProduct_p(0,i);
    } else {
      *itsLog << "Cannot deduce receptor 1 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
    receptor = Stokes::receptor2(cType);
    if (receptor.isValid()) {
      corrProduct_p(1,i) = receptor;
//      cout << "corrProcut_p(1,"<< i <<")=" << corrProduct_p(1,i);
    } else {
      *itsLog << "Cannot deduce receptor 2 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
  }

  // Save the object name, we may need it (for single source fits)
  const FitsKeyword* kwp;
  object_p = (kwp=kw(FITS::OBJECT)) ? kwp->asString() : "unknown";
  object_p=object_p.before(trailing);
  // Save the array name
  if(array_p=="" || array_p=="unknown"){
    array_p = (kwp=kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
    array_p=array_p.before(trailing);
  }
  if(array_p=="" || array_p=="unknown"){
    array_p = (kwp=kw("ARRNAM")) ? kwp->asString() : "unknown";
    array_p=array_p.before(trailing);
  }

  // Save the RA/DEC epoch (for ss fits)
  epoch_p = (kwp=kw(FITS::EPOCH)) ? kwp->asFloat() : 2000.0;

  // Get the spectral information
  freqsys_p = MFrequency::TOPO;
  restfreq_p = 0.0;
  Record header;
  Vector<String> ignore;
  Bool ok = FITSKeywordUtil::getKeywords(header, kwlist(), ignore);
  if (ok) {
    Int spectralAxis;
    Double referenceChannel, referenceFrequency, deltaFrequency;
    Vector<Double> frequencies;
    MDoppler::Types velPref;
    // Many of the following aren't used since they have been obtained
    // in other ways.
    ok = FITSSpectralUtil::fromFITSHeader(spectralAxis,
					  referenceChannel,
					  referenceFrequency,
					  deltaFrequency,
					  frequencies,
					  freqsys_p,
					  velPref,
					  restfreq_p,
					  *itsLog,
					  header);
  }

}

void FITSIDItoMS1::setupMeasurementSet(const String& MSFileName, Bool useTSM, 
				       Bool mainTbl, Bool addCorrMod,
				       Bool addSyscal, Bool addWeather) {
  
  Int nCorr = 0;
  Int nChan = 0;
  Int nIF_p = 0;

  String telescop;

  if(mainTbl) {
    nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
    nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
    nIF_p = getIndex(coordType_p,"BAND");
    if (nIF_p>=0) {
      nIF_p=nPixel_p(nIF_p);
    } else {
      nIF_p=1;
    }
    // determine telescop here
  } 
  //cout << "===> nIF_p=" << nIF_p <<endl; 
  // Make the MS table
  TableDesc td = MS::requiredTableDesc();
  
  // Even though we know the data is going to be the same shape throughout I'll
  // still create a column that has a variable shape as this will permit MS's
  // with other shapes to be appended.
  MS::addColumnToDesc(td, MS::DATA, 2);
  Vector<String> tiledDataNames;
  String hcolName=String("Tiled")+String("DATA");
  td.defineHypercolumn(hcolName, 3,
		       stringToVector("DATA"));
  tiledDataNames.resize(1);
  tiledDataNames[0] = hcolName;
  
  // Add this optional column (random group fits can have a
  // weight per visibility) if the FITS IDI data actually contains it
  if(uv_data_hasWeights_p){
    MS::addColumnToDesc(td, MS::WEIGHT_SPECTRUM, 2);
    MS::addColumnToDesc(td, MS::SIGMA_SPECTRUM, 2);
  }
  
  if(mainTbl && useTSM) {
    td.defineHypercolumn("TiledDATA",3,
			 stringToVector(MS::columnName(MS::DATA)));
    td.defineHypercolumn("TiledFlag",3,
			 stringToVector(MS::columnName(MS::FLAG)));
    td.defineHypercolumn("TiledFlagCategory",4,
			 stringToVector(MS::columnName(MS::FLAG_CATEGORY)));
    if(uv_data_hasWeights_p){
      td.defineHypercolumn("TiledWgtSpectrum",3,
			   stringToVector(MS::columnName(MS::WEIGHT_SPECTRUM)));
    }
    td.defineHypercolumn("TiledUVW",2,
			 stringToVector(MS::columnName(MS::UVW)));
    td.defineHypercolumn("TiledWgt",2,
			 stringToVector(MS::columnName(MS::WEIGHT)));
    td.defineHypercolumn("TiledSigma", 2,
			 stringToVector(MS::columnName(MS::SIGMA)));
  }
  SetupNewTable newtab(MSFileName, td, Table::New);
  
  // Set the default Storage Manager to be the Incr one
  uInt cache_val=32768;
  IncrementalStMan incrStMan ("ISMData",cache_val);
  newtab.bindAll(incrStMan, True);

  // Choose an appropriate tileshape
  IPosition dataShape(2, nCorr, nChan);
  IPosition tshape = MSTileLayout::tileShape(dataShape, itsObsType, telescop);

  if(tshape.nelements() != 3){
    throw(AipsError("TileShape has to have 3 elememts ") );
  }  

  IPosition tileShape(tshape);

  if(mainTbl){
    IncrementalStMan incrStMan0("Array_ID",cache_val);
    newtab.bindColumn(MS::columnName(MS::ARRAY_ID), incrStMan0);
    IncrementalStMan incrStMan1("EXPOSURE",cache_val);
    newtab.bindColumn(MS::columnName(MS::EXPOSURE), incrStMan1);
    IncrementalStMan incrStMan2("FEED1",cache_val);
    newtab.bindColumn(MS::columnName(MS::FEED1), incrStMan2);
    IncrementalStMan incrStMan3("FEED2",cache_val);
    newtab.bindColumn(MS::columnName(MS::FEED2), incrStMan3);
    IncrementalStMan incrStMan4("FIELD_ID",cache_val);
    newtab.bindColumn(MS::columnName(MS::FIELD_ID), incrStMan4);
    IncrementalStMan incrStMan6("INTERVAL",cache_val);
    newtab.bindColumn(MS::columnName(MS::INTERVAL), incrStMan6);
    IncrementalStMan incrStMan7("OBSERVATION_ID",cache_val);
    newtab.bindColumn(MS::columnName(MS::OBSERVATION_ID), incrStMan7);
    IncrementalStMan incrStMan8("PROCESSOR_ID",cache_val);
    newtab.bindColumn(MS::columnName(MS::PROCESSOR_ID), incrStMan8);
    IncrementalStMan incrStMan9("SCAN_NUMBER",cache_val);
    newtab.bindColumn(MS::columnName(MS::SCAN_NUMBER), incrStMan9);
    IncrementalStMan incrStMan10("STATE_ID",cache_val);
    newtab.bindColumn(MS::columnName(MS::STATE_ID), incrStMan10);
    IncrementalStMan incrStMan11("TIME",cache_val);
    newtab.bindColumn(MS::columnName(MS::TIME), incrStMan11);
    IncrementalStMan incrStMan12("TIME_CENTROID",cache_val);
    newtab.bindColumn(MS::columnName(MS::TIME_CENTROID), incrStMan12);
  
    // Bind FLAG_ROW, ANTENNA1, ANTENNA2 and DATA_DESC_ID to the standardStMan 
    // as they may change sufficiently frequently to make the
    // incremental storage manager inefficient for these columns.
    
    StandardStMan aipsStMan0("ANTENNA1", cache_val);
    newtab.bindColumn(MS::columnName(MS::ANTENNA1), aipsStMan0);
    StandardStMan aipsStMan1("ANTENNA2", cache_val);
    newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan1);
    StandardStMan aipsStMan2("DATA_DESC_ID", cache_val);
    newtab.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan2);
    StandardStMan aipsStMan3("FLAG_ROW",cache_val/4);
    newtab.bindColumn(MS::columnName(MS::FLAG_ROW), aipsStMan3);
    
    
    TiledShapeStMan tiledStMan1f("TiledFlag",tileShape);
    TiledShapeStMan tiledStMan1fc("TiledFlagCategory",
				  IPosition(4,tileShape(0),tileShape(1),1,
					    tileShape(2)));
    TiledShapeStMan tiledStMan2("TiledWgtSpectrum",tileShape);
    TiledColumnStMan tiledStMan3("TiledUVW",IPosition(2,3,1024));
    TiledShapeStMan tiledStMan4("TiledWgt", 
				IPosition(2,tileShape(0),tileShape(2)));
    TiledShapeStMan tiledStMan5("TiledSigma", 
				IPosition(2,tileShape(0),tileShape(2)));
    
    // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
    
    TiledShapeStMan tiledStMan1Data("TiledDATA",tileShape);
    
    newtab.bindColumn(MS::columnName(MS::DATA), tiledStMan1Data);
    
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan1f);
    newtab.bindColumn(MS::columnName(MS::FLAG_CATEGORY),tiledStMan1fc);
    if(uv_data_hasWeights_p){
      newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),tiledStMan2);
    }
    
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStMan3);
    newtab.bindColumn(MS::columnName(MS::WEIGHT),tiledStMan4);
    newtab.bindColumn(MS::columnName(MS::SIGMA),tiledStMan5);
    
  }
  // avoid lock overheads by locking the table permanently
  TableLock lock(TableLock::AutoLocking);
  MeasurementSet ms(newtab,lock);
  //MeasurementSet ms(newtab);

  // create all subtables
  // we make new tables with 0 rows
  Table::TableOption option=Table::New;
  // Set up the subtables for the UVFITS MS
  ms.createDefaultSubtables(option);
 
// Since the MS SOURCE table is presently not filled,
// its creation is commented out here.
//   // add the optional Source sub table to allow for 
//   // specification of the rest frequency
//   TableDesc sourceTD=MSSource::requiredTableDesc();
//   SetupNewTable sourceSetup(ms.sourceTableName(),sourceTD,option);
//   ms.rwKeywordSet().defineTable(MS::keywordName(MS::SOURCE),
//  				 Table(sourceSetup,0));

  if(addCorrMod){
    cout << "Correlator model table setup needs to be inplemented." << endl;
  }

  if(addSyscal){
    TableDesc td = MSSysCal::requiredTableDesc();
    MSSysCal::addColumnToDesc(td, MSSysCal::TSYS);
    SetupNewTable tabSetup(ms.sysCalTableName(), td, option);
    ms.rwKeywordSet().defineTable(MS::keywordName(MS::SYSCAL),
				  Table(tabSetup));
  }

  if(addWeather){
    TableDesc td = MSWeather::requiredTableDesc();
    MSWeather::addColumnToDesc(td, MSWeather::DEW_POINT);
    if(weather_hasWater_p)
      MSWeather::addColumnToDesc(td, MSWeather::H2O);
    if(weather_hasElectron_p)
      MSWeather::addColumnToDesc(td, MSWeather::IONOS_ELECTRON);
    MSWeather::addColumnToDesc(td, MSWeather::PRESSURE);
    MSWeather::addColumnToDesc(td, MSWeather::TEMPERATURE);
    MSWeather::addColumnToDesc(td, MSWeather::WIND_DIRECTION);
    MSWeather::addColumnToDesc(td, MSWeather::WIND_SPEED);
    SetupNewTable tabSetup(ms.weatherTableName(), td, option);
    ms.rwKeywordSet().defineTable(MS::keywordName(MS::WEATHER),
				  Table(tabSetup));
  }

  // update the references to the subtable keywords
  ms.initRefs();
 
  { // Set the TableInfo
    TableInfo& info(ms.tableInfo());
    info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
    info.setSubType(String("FITS-IDI"));
    info.readmeAddLine
      ("This is a measurement set Table holding astronomical observations");
  }

  ms_p=ms;
  msc_p=new MSColumns(ms_p);
}

// Van Vleck relationship
static Double rho_2(Double r)
{
  return sin((C::pi * r) / 2);
}

// Fred Schwab's rational approximation for 2-bit sampling with n =
// 3.336 and optimally chosen v_0.  See VLBA Correlator Memo 75.
static Double rho_4(Double r)
{
  Double rr = r*r;
  Double num, den;

  num = 1.1329552 - 3.1056902 * rr + 2.9296994 * rr*rr - 0.90122460 * rr*rr*rr;
  den = 1 - 2.7056559 * rr + 2.5012473 * rr*rr - 0.73985978 * rr*rr*rr;

  return (num / den) * r;
}

void FITSIDItoMS1::fillMSMainTable(const String& MSFileName, Int& nField, Int& nSpW)
{

  // Get access to the MS columns
  //MSColumns& msc(*msc_p);

  
  MeasurementSet ms(MSFileName,Table::Update);
  MSColumns msc(ms);
  if(!firstMain){
    ms_p = ms;
    msc_p = new MSColumns(ms_p); 
  }

  const Regex trailing(" *$"); // trailing blanks

  // get the random group parameter names
  Int tFields; //(nParams)
  Int nRows;  //(nGroups)
  Int MSnRows; MSnRows = msc.nrow();
  Vector<Int> scans; scans=0;
  msc.scanNumber().getColumn(scans); 
  //cout << msc.scanNumber().getColumn() <<endl;
  //cout << msc.scanNumber(0)<<endl;

  tFields = tfields();
  nRows = nrows();
  //cout << "tFields=" << tFields << endl;
  //cout << "nrows=" << nRows << endl;
  //cout << "msnrows=" << MSnRows << endl; 
  //cout << "scanNumber=" << nScan<< endl;

  Vector<String> tType(tFields);

  for (Int i=0; i < tFields; i++) {
    tType(i) = ttype(i); 
    tType(i) = tType(i).before(trailing);
  }

  //cout << "tType=" << tType(0) << endl;

  //cout << "STOKES POS=" << getIndex(coordType_p,"STOKES") << endl;
  //cout << "FREQ POS=" << getIndex(coordType_p,"FREQ") << endl;

  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  Int nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
  //cout << "nCorr=" << nCorr << endl;
  //cout << "nChan=" << nChan << endl;

  Matrix<Complex> vis(nCorr,nChan);
  Matrix<Float> sigmaSpec(nCorr, nChan);
  Matrix<Float> weightSpec(nCorr, nChan);

  Vector<Float> tmp(nChan + 1);
  Vector<Float> work(3 * (nChan + 1) + 15);
  Bool worksave;
  Float *workptr = work.getStorage(worksave);
  FFTPack::costi(nChan + 1, workptr);

  const Int nCat = 3; // three initial categories
  // define the categories
  Vector<String> cat(nCat);
  cat(0)="FLAG_CMD";
  cat(1)="ORIGINAL"; 
  cat(2)="USER"; 
  msc.flagCategory().rwKeywordSet().define("CATEGORY",cat);
  Cube<Bool> flagCat(nCorr,nChan,nCat,False);
  Matrix<Bool> flag = flagCat.xyPlane(0); // references flagCat's storage

  // find out the indices for U, V and W, there are several naming schemes
  Int iU,iV,iW;
  iU = getIndexContains(tType,"UU"); 
  iV = getIndexContains(tType,"VV");
  iW = getIndexContains(tType,"WW");
  if (iU < 0 || iV < 0 || iW < 0) {
    throw(AipsError("FitsIDItoMS: Cannot find UVW information"));
  }
  
  // get index for baseline
  Int iBsln = getIndex(tType, "BASELINE");
  // get indices for time
  Int iTime0 = getIndex(tType, "DATE");
  Int iTime1 = getIndex(tType, "TIME");
  // get index for source
  Int iSource = getIndex(tType, "SOURCE_ID"); 
  if (iSource < 0)
    iSource = getIndex(tType, "SOURCE");
  // get index for Freq
  Int iFreq = getIndex(tType, "FREQID");
  // get index for FLUX
  Int iFlux = getIndex(tType, "FLUX");
  // get index for Integration time
  Int iInttim = getIndex(tType, "INTTIM"); 
  // get index for weight
  Int iWeight = getIndex(tType, "WEIGHT");

  /*
  cout << "iU=" << iU << endl;
  cout << "iV=" << iV << endl;
  cout << "iW=" << iW << endl;
  cout << "iBsln=" << iBsln << endl;
  cout << "iTime0=" << iTime0 << endl;
  cout << "iTime1=" << iTime1 << endl;
  cout << "iSource=" << iSource << endl;
  cout << "iFreq=" << iFreq << endl;
  cout << "iFlux=" << iFlux << endl;
  */

  receptorAngle_p.resize(1);
  nAnt_p=0;
  *itsLog << LogIO::NORMAL << "Reading and writing visibility data"<< LogIO::POST;

  Int row=-1;
  Double startTime;
  Float interval;
  startTime=0.0; interval=1;

  ProgressMeter meter(0.0, nRows*1.0, "FITS-IDI Filler", "Rows copied", "",
 		      "", True,  nRows/100);

  Vector<Double> uvw(3); // Move this temporary out of the loop
  Vector<Float> _uvw(3); 
  Int lastSpW;
  lastSpW=-1;
  Int putrow = -1;
  //  Double lastTime=0;
  //  Bool lastRowFlag=False;
  Int nScan = 0;

  if (firstMain) {
    putrow = -1; 
  } else {
    putrow = MSnRows - 1; 
    nScan = scans(putrow) + 1;      
  }
  //cout << "scanNumber=" << nScan<< endl;

  for (Int trow=0; trow<nRows; trow++) {
    // Read next row and
    // get time in MJD seconds
    const Double JDofMJD0=2400000.5;
    read(1);
    
    //
    //get actual Time0 data value from field array,
    //then multiply by scale factor and add offset.
    //
    Double time;
    memcpy(&time, (static_cast<Double *>(data_addr[iTime0])), sizeof(Double));
    time *= tscal(iTime0);
    time += tzero(iTime0);  
    time -= JDofMJD0;
    //cout << "TIME=" << time << endl; 

    if (iTime1>=0){
      Double time1;
      memcpy(&time1, (static_cast<Double *>(data_addr[iTime1])), sizeof(Double));
      time1 *= tscal(iTime1);
      time1 += tzero(iTime1); 
      time += time1;
    }
    //cout << "TIME=" << time << endl; 

    Int _baseline;
    Float baseline;
    memcpy(&_baseline, (static_cast<Int *>(data_addr[iBsln])), sizeof(Int));
    baseline=static_cast<Float>(_baseline); 
    baseline *= tscal(iBsln);
    baseline += tzero(iBsln); 
    //cout << "BASELINE=" << baseline << endl; 

    if(field(iU).fieldtype() == FITS::FLOAT) {
      uvw(0) = *static_cast<Float *>(data_addr[iU]);
    } else {
      uvw(0) = *static_cast<Double *>(data_addr[iU]);
    }    
    uvw(0) *= tscal(iU);
    uvw(0) += tzero(iU); 
    //cout << "uvw(0)=" << uvw(0) << endl; 

    if(field(iV).fieldtype() == FITS::FLOAT) {
      uvw(1) = *static_cast<Float *>(data_addr[iV]);
    } else {
      uvw(1) = *static_cast<Double *>(data_addr[iV]);
    }
    uvw(1) *= tscal(iV);
    uvw(1) += tzero(iV); 
    //cout << "uvw(1)=" << uvw(1) << endl; 

    if(field(iW).fieldtype() == FITS::FLOAT) {
      uvw(2) = *static_cast<Float *>(data_addr[iW]);
    } else {
      uvw(2) = *static_cast<Double *>(data_addr[iW]);
    }
    uvw(2) *= tscal(iW);
    uvw(2) += tzero(iW); 
    //cout << "uvw(2)=" << uvw(2) << endl; 

    time  *= C::day; 
    //    cout << "TIME=" << setprecision(11) << time << endl; 

    if (row<0) {
      startTime = time;
      if (firstMain){
	startTime_p = startTime;
	//	cout << "startTime is set to " << startTime << endl;
      }
    }

    // If integration time is available, use it:
    if (iInttim > -1) {
      memcpy(&interval, (static_cast<Float *>(data_addr[iInttim])), sizeof(Float));
      interval *= tscal(iInttim);
      // cout << "INTTIM=" << setprecision(11) << interval << endl; 
    } else {
      // make a guess at the integration time
      if (row<0) {
	*itsLog << LogIO::WARN << "UV_DATA table contains no integration time information. Will try to derive it from TIME." 
		<< LogIO::POST;
      }
      if (time > startTime) {
	interval=time-startTime;
	msc.interval().fillColumn(interval);
	msc.exposure().fillColumn(interval);
	startTime = DBL_MAX; // do this only once
      }
    }

    if(trow==nRows-1){
      lastTime_p = time+interval;
    }

    Int array = Int(100.0*(baseline - Int(baseline)+0.001));
    Int ant1 = Int(baseline)/256; 
    Int ant2 = Int(baseline) - ant1*256; 
    if(antIdFromNo.find(ant1) != antIdFromNo.end()){
    	ant1 = antIdFromNo[ant1];
    }
    else{
    	*itsLog << LogIO::SEVERE << "Inconsistent input dataset: unknown ANTENNA_NO "
    			<< ant1 << " in baseline used in UV_DATA table." << LogIO::EXCEPTION;
    }
    if(antIdFromNo.find(ant2) != antIdFromNo.end()){
    	ant2 = antIdFromNo[ant2];
    }
    else{
    	*itsLog << LogIO::SEVERE << "Inconsistent input dataset: unknown ANTENNA_NO "
    			<< ant2 << " in baseline used in UV_DATA table." << LogIO::EXCEPTION;
    }
    nAnt_p = max(nAnt_p,ant1+1);
    nAnt_p = max(nAnt_p,ant2+1);

    Bool doConjugateVis = False;

    if(ant1>ant2){ // swap indices and multiply UVW by -1
      Int tant = ant1;
      ant1 = ant2;
      ant2 = tant;
      uvw *= -1.;
      doConjugateVis = True;
    }

    // Convert U,V,W from units of seconds to meters
    uvw *= C::c;

    Int count = 0;

    Float test_baseline;
    memcpy(&test_baseline,fitsrow,sizeof(Int));
    //cout << "*****TEST_BASELINE FITS=" << test_baseline << endl;

    memcpy(&test_baseline,table,sizeof(Int));
    //cout << "*****TEST_BASELINE TABLE=" << test_baseline << endl;

    Int new_baseline;
    memcpy(&new_baseline,static_cast<Int *>(data_addr[iBsln]),sizeof(Int));
    //cout << "*****NEW_BASELINE ADDR=" << new_baseline << endl;
        

    Float visReal = 0.;
    Float visImag = 0.;
    Float visWeight = 1.;

    Int nIF_p = 0;
    nIF_p = getIndex(coordType_p,"BAND");
    if (nIF_p>=0) {
      nIF_p=nPixel_p(nIF_p);
    } else {
      nIF_p=1;
    }
    
    //cout <<"ifnomax ="<<max(1,nIF_p)<<endl;

    for (Int ifno=0; ifno<max(1,nIF_p); ifno++) {
      // BANDs go to separate rows in the MS
      ms.addRow();
      row++;
      putrow++;
 
      for (Int chan=0; chan<nChan; chan++) {
	for (Int pol=0; pol<nCorr; pol++) {
           
          memcpy(&visReal, (static_cast<Float *>(data_addr[iFlux])) + count++, sizeof(Float));
//          if(count<9){
//	    cout << "COUNT=" << count <<"ifno="<< ifno <<"chan="<< chan<<"pol="<< pol  << " corrindex " << corrIndex_p[pol] << endl;
//	  }
	  //visReal *= tscal(iFlux);
	  //visReal += tzero(iFlux); 

	  memcpy(&visImag, (static_cast<Float *>(data_addr[iFlux])) + count++, sizeof(Float));     
	  //visImag *= tscal(iFlux);
	  //visImag += tzero(iFlux); 
//	  if(count<10){
//	    cout<<"    visReal="<< visReal << "visImag="<< visImag<<endl;
//	  }

	  if (uv_data_hasWeights_p) {
	    memcpy(&visWeight, (static_cast<Float *>(data_addr[iFlux])) + count++, sizeof(Float));
	  } else if (iWeight>=0) {
	    memcpy(&visWeight, (static_cast<Float *>(data_addr[iWeight])) + ifno * nStokes_p + pol, sizeof(Float));
	  }

	  //const Float wt = priGroup_p(count++); 

	  const Int p = corrIndex_p[pol];

 	  if (visWeight <= 0.0) {
	    weightSpec(p, chan) = -visWeight;
	    flag(p, chan) = True;
	  } else {                            
	    weightSpec(p, chan) = visWeight;
	    flag(p, chan) = False;
	  }

	  if(doConjugateVis){ // need a conjugation to follow the ant1<=ant2 rule
	    vis(p, chan) = Complex(visReal, visImag); // NOTE: this means no conjugation of visibility because of FITS-IDI convention!
	  }
	  else{
	    vis(p, chan) = Complex(visReal, -visImag); // NOTE: conjugation of visibility!
	                                               // FITS-IDI convention is conjugate of AIPS and CASA convention!
	  }
 	}
      }

      // Apply digital corrections to data correlated with the DiFX
      // correlator as these have not been applied at the correlator.
      // Various constants have been taken from VLBA Scientific Memo
      // 12 as the DiFX tries to emulate the original VLBA FX
      // correlator as closely as possible.  Note that DiFX doesn't
      // suffer from saturation so the saturation correction is
      // omitted here.  DiFX currently doesn't support Hanning
      // weighting.
      //
      // The correction applied here matches what the AIPS FITLD task
      // does for DiFX-correlated data as closely as possible.  Two
      // notable differences in the implementation.  This code simply
      // uses the FFTPACK cosine tranform where FITLD implements its
      // own cosine tranform based on an FFT.  And this code simply
      // uses the expressions for rho_2 and rho_4 given in the
      // literature instead of using a lookup table.
      if (itsCorrelat == "DIFX") {
	const double A = 5.36;
	const Double H = 0.87890625;
	Double bfacta, bfactc;
	Double Rm, gamma, alfa;
	Double (*rho)(Double) = NULL;

	if (digiLevels[ant1] == 4 && digiLevels[ant2] == 4) {
	  Rm = 4.3048;
	  alfa = 0.882518;
	  gamma = 3.335875 * 64.0 / 63.0;
	  rho = rho_4;
	} else if (digiLevels[ant1] == 2 && digiLevels[ant2] == 2) {
	  Rm = 1.0;
	  alfa = 2.0 / C::pi;
	  gamma = 1.0 * 64.0 / 63.0;
	  rho = rho_2;
	} else if ((digiLevels[ant1] == 2 && digiLevels[ant2] == 4) ||
		   (digiLevels[ant1] == 4 && digiLevels[ant2] == 2)) {
	  Rm = 5.8784;
	  alfa = 0.882518;
	  gamma = 3.335875 * 64.0 / 63.0;
	} else {
	  // Unsupported.  Assume a large number of levels (alpha =
	  // 1.0) and trust the user is going to normalize the
	  // visibilities (e.g. by running the accor task in CASA).
	  Rm = 1.0 / (A * H);
	  alfa = 1.0;
	  gamma = 1.0;
	}

	bfactc = (gamma*gamma) / (A * Rm * alfa * H);
	bfacta = (gamma*gamma) / (A * Rm * H);

	if (ant1 != ant2 || rho == NULL) {
	  // Cross-correlations
	  vis *= Complex(bfactc);
	} else {
	  // Auto-correlations
	  for (Int p=0; p<nCorr; p++) {
	    if (corrProduct_p(0, p) == corrProduct_p(1, p)) {
	      Bool tmpsave;
	      Float *tmpptr;

	      for (Int chan=0; chan<nChan; chan++)
		tmp(chan) = bfacta * vis(p, chan).real();

	      if (abs(tmp(0)) < 1e-20)
		continue;

	      // Extrapolate spectrum as this point has been thrown
	      // away by the correlator.
	      tmp(nChan) = 2 * tmp(nChan-1) - tmp(nChan-2);

	      // Cosine transform to lag domain
	      tmpptr = tmp.getStorage(tmpsave);
	      FFTPack::cost(nChan + 1, tmpptr, workptr);
	      tmp.putStorage(tmpptr, tmpsave);

	      // Apply digital correction.
	      for (Int chan = 1; chan<nChan; chan++) {
		Float wt = 1.0 - ((Float)chan / nChan);
		tmp(chan) = (wt * tmp(0)) * rho(tmp(chan) / (wt * tmp(0)));
	      }
	      tmp(nChan) = 0.0;

	      // Cosine trandorm back to frequency domain
	      tmpptr = tmp.getStorage(tmpsave);
	      FFTPack::cost(nChan + 1, tmpptr, workptr);
	      tmp.putStorage(tmpptr, tmpsave);

	      for (Int chan=0; chan<nChan; chan++)
		vis(p, chan) = tmp(chan) / (2*nChan);
	    } else {
	      for (Int chan=0; chan<nChan; chan++)
		vis(p, chan) *= Complex(bfactc);
	    }
	  }
	}
      }

      if (weightyp_p == "CORRELAT")
	vis /= weightSpec;

      // determine the spectralWindowId
      Int spW = ifno;
      if (iFreq>=0) {
	memcpy(&spW, (static_cast<Int *>(data_addr[iFreq])), sizeof(Int));
	spW *= (Int)tscal(iFreq);
	spW += (Int)tzero(iFreq); 
	spW--; // make 0-based
	if (nIF_p>0) {
	  spW *=nIF_p; 
	  spW+=ifno;
	}
      }
      if (spW!=lastSpW) {
	msc.dataDescId().put(putrow,spW);
	nSpW = max(nSpW, spW+1);
	lastSpW=spW;
      }

      for (Int chan=0; chan<nChan; chan++) {
	for (Int pol=0; pol<nCorr; pol++) {
	  const Int p = corrIndex_p[pol];

	  if (ant1 == ant2)
	    weightSpec(p, chan) *= interval * effChBw(spW);
	  else
	    weightSpec(p, chan) *= 2 * interval * effChBw(spW);

	  if (weightSpec(p, chan) > 0.0)
	    sigmaSpec(p, chan) = 1.0f / sqrt(weightSpec(p, chan));
	  else
	    sigmaSpec(p, chan) = 1.0f;
	}
      }

      // fill in values for all the unused columns
      msc.feed1().put(putrow,0);
      msc.feed2().put(putrow,0);
      msc.flagRow().put(putrow,False);
      msc.processorId().put(putrow,-1);
      msc.observationId().put(putrow,0);
      msc.stateId().put(putrow,-1);

      Vector<Float> tmp(nCorr);
      tmp=1.0;
      msc.sigma().put(putrow,tmp);
      tmp=0.0;
      msc.weight().put(putrow,tmp);

      msc.interval().put(putrow,interval);
      msc.exposure().put(putrow,interval);
      msc.scanNumber().put(putrow,nScan);

      msc.data().put(putrow,vis);

      const Vector<Float> sigma = partialMedians(sigmaSpec, IPosition(1, 1));
      const Vector<Float> weight = partialMedians(weightSpec, IPosition(1, 1));
      msc.sigma().put(putrow,sigma);
      msc.weight().put(putrow,weight);

      if(uv_data_hasWeights_p){
	msc.sigmaSpectrum().put(putrow,sigmaSpec);
	msc.weightSpectrum().put(putrow,weightSpec); 
      }

      msc.flag().put(putrow,flag);
      msc.flagCategory().put(putrow,flagCat);

      Bool rowFlag=allEQ(flag,True);
      msc.flagRow().put(putrow,rowFlag);

      msc.antenna1().put(putrow,ant1);
      msc.antenna2().put(putrow,ant2);
      msc.arrayId().put(putrow,array);
      msc.time().put(putrow,time);
      msc.timeCentroid().put(putrow,time+interval/2.);
      msc.uvw().put(putrow,uvw);
      
      // store the sourceId 
      Int sourceId = 0;
      if (iSource>=0) {
 	// make 0-based
        memcpy(&sourceId, (static_cast<Int *>(data_addr[iSource])), sizeof(Int));
        sourceId *= (Int)tscal(iSource);
        sourceId += (Int)tzero(iSource); 
 	sourceId--; // make 0-based
      }
      msc.fieldId().put(putrow,sourceId);
      nField = max(nField, sourceId+1);
    } // end for(ifno=0 ...
    meter.update((trow+1)*1.0);
  } // end for(trow=0 ...

  // fill the receptorAngle with defaults, just in case there is no AN table
  receptorAngle_p=0;

  // Free FFTPack work buffer
  work.putStorage(workptr, worksave);
}

// fill Observation table
void FITSIDItoMS1::fillObsTables() {
  const Regex trailing(" *$"); // trailing blanks
  const FitsKeyword* kwp;
  Vector<Double> times(2);
  
  if(firstMain) {
    ms_p.observation().addRow();
    String observer;
    observer = (kwp=kw(FITS::OBSERVER)) ? kwp->asString() : "unknown";
    observer=observer.before(trailing);
    MSObservationColumns msObsCol(ms_p.observation());
    msObsCol.observer().put(0,observer);
    String obscode;
    obscode = (kwp=kw("OBSCODE")) ? kwp->asString() : "";
    obscode=obscode.before(trailing);
    msObsCol.project().put(0,obscode);
    String telescope= (kwp=kw(FITS::TELESCOP)) ? kwp->asString() : array_p;
    telescope=telescope.before(trailing);  
    if(telescope=="" || telescope=="unknown"){
      telescope= (kwp=kw("ARRNAM")) ? kwp->asString() : "unknown";
      telescope=telescope.before(trailing);  
    } 
    msObsCol.telescopeName().put(0,telescope);
    msObsCol.scheduleType().put(0, "");
   
  //String date;
  //date = (kwp=kw(FITS::DATE_OBS)) ? kwp->asString() : "";
  //if (date=="") {
    // try FITS::DATE instead 
    //  (but this will find DATE-MAP which may not be correct...)
  //  date = (kwp=kw(FITS::DATE)) ? kwp->asString() : "";
  //}

    times(0)=startTime_p;
    times(1)=lastTime_p; // time in the last record of the input data
  //cout << "times=" << times(0) << "," << times(1)<< endl;
    msObsCol.timeRange().put(0,times);
    msObsCol.releaseDate().put(0,times(0));  //just use TIME_RANGE for now
    msObsCol.flagRow().put(0,False);
  } else {
    times = msc_p->observation().timeRange()(0);
    MSObservationColumns msObsCol(ms_p.observation());
    times(1)=lastTime_p;
    msObsCol.timeRange().put(0,times);
  }   
  // Store all keywords from the first HISTORY keyword onwards in History table
  String date;
  date = (kwp=kw(FITS::DATE_OBS)) ? kwp->asString() : "";
  if (date=="") {
    date = (kwp=kw(FITS::DATE)) ? kwp->asString() : "";
  }
  if (date=="") date = "2000-01-01";
  MVTime timeVal;
  MEpoch::Types epochRef;
  FITSDateUtil::fromFITS(timeVal,epochRef,date,"UTC");
  Double time=timeVal.second();

  String history = (kwp=kw(FITS::HISTORY)) ? kwp->comm(): "";
  history = history.before(trailing);
  MSHistoryColumns msHisCol(ms_p.history());
  Int row=-1;
  while (history!="") {
    ms_p.history().addRow(); row++;
    msHisCol.observationId().put(row,0);
    msHisCol.time().put(row,time);
    msHisCol.priority().put(row,"NORMAL");
    msHisCol.origin().put(row,"FITSIDItoMS1::fillObsTables");
    msHisCol.application().put(row,"ms");
    msHisCol.message().put(row,history);
    history = (kwp=nextkw()) ? kwp->comm(): "";
    history = history.before(trailing);
  }
}

void FITSIDItoMS1::fillAntennaTable()
{
  *itsLog << LogOrigin("FitsIDItoMS", "fillAntennaTable");
  const Regex trailing(" *$"); // trailing blanks
  TableRecord btKeywords=getKeywords();
  
  Int nAnt=nrows();
  receptorAngle_p.resize(2*nAnt);
  Vector<Double> arrayXYZ(3);
  arrayXYZ=0.0;
  if(!btKeywords.isDefined("ARRAYX")||!btKeywords.isDefined("ARRAYY")||
     !btKeywords.isDefined("ARRAYZ")) {
    throw(AipsError("FITSIDItoMS: Illegal ANTENNA file: no antenna positions"));
  }
   arrayXYZ(0)=getKeywords().asdouble("ARRAYX");
   arrayXYZ(1)=getKeywords().asdouble("ARRAYY");
   arrayXYZ(2)=getKeywords().asdouble("ARRAYZ");

   *itsLog << LogIO::NORMAL << "number of antennas = "<<nAnt<<LogIO::POST;
   *itsLog << LogIO::NORMAL << "array ref pos = "<<arrayXYZ<<LogIO::POST;

   String srdate;
   if(btKeywords.isDefined("RDATE")) {
     srdate=btKeywords.asString("RDATE");
   }
   Double gst=0.0;
   if(btKeywords.isDefined("GSTIA0")) {
     gst=btKeywords.asdouble("GSTIA0")*C::degree;
   }
   Double degpdy=0.0;
   if(btKeywords.isDefined("DEGPDY")) {
     degpdy=btKeywords.asdouble("DEGPDY");
   }
   String timsys="TAI";
   if (btKeywords.isDefined("TIMSYS")) {
     timsys=btKeywords.asString("TIMSYS");
     timsys=timsys.before(trailing);
   }
   if (btKeywords.isDefined("TIMESYS")) { // TIMESYS overrides TIMSYS
     timsys=btKeywords.asString("TIMESYS");
     timsys=timsys.before(trailing);
   }
   String frame="GEOCENTRIC";
   if (btKeywords.isDefined("FRAME")) {
     String myframe = btKeywords.asString("FRAME");
     myframe=myframe.before(trailing);
     if(myframe != frame){ // presently the only defined value is the default
       *itsLog << LogIO::WARN << "FRAME keyword in ARRAY_GEOMETRY table has unrecognized value \"" 
	       << myframe << "\", will assume GEOCENTRIC." << LogIO::POST;
     }
   }

   MVTime timeVal;
   MEpoch::Types epochRef;
   FITSDateUtil::fromFITS(timeVal,epochRef,srdate,timsys);
   // convert to canonical form
   timsys=MEpoch::showType(epochRef);
   rdate=timeVal.second(); // MJD seconds
   String arrnam="unknown";
   if (btKeywords.isDefined("ARRNAM")) {
     arrnam=btKeywords.asString("ARRNAM");
     arrnam=arrnam.before(trailing);
     if(array_p=="" || array_p=="unknown"){
       array_p = arrnam;
     }
     else{
       if(array_p != arrnam){
	 *itsLog << LogIO::WARN << "Conflicting observatory names: found "
		 << arrnam << " and " << array_p << LogIO::POST;
       }
     }
   }
   if ((array_p=="" || array_p=="unknown") && btKeywords.isDefined("TELESCOP")) {
     arrnam=btKeywords.asString("TELESCOP");
     arrnam=arrnam.before(trailing);
     array_p = arrnam;
   }

   // store the time and frame keywords 
   ms_p.antenna().rwKeywordSet().define(String("RDATE"),rdate);
   ms_p.antenna().rwKeywordSet().define(String("GSTIA0"),gst);
   ms_p.antenna().rwKeywordSet().define(String("DEGPDY"),degpdy);
   ms_p.antenna().rwKeywordSet().define(String("TIMESYS"),timsys);
   ms_p.antenna().rwKeywordSet().define(String("FRAME"),frame);

   //save value to set time reference frame elsewhere
   timsys_p=timsys;

   Float diameter=0.; // default (meaning "not set")

   Table anTab=oldfullTable("");

   MSAntennaColumns& ant(msc_p->antenna());
   ScalarColumn<String> name(anTab,"ANNAME");
   ArrayColumn<Double> antXYZ(anTab,"STABXYZ");
   ArrayColumn<Float> dantXYZ(anTab,"DERXYZ");
   // following is for space-born telescope
   //ScalarColumn<Int> orbp(anTab,"ORBPARM");
   ScalarColumn<Int> anNo(anTab,"NOSTA");
   ScalarColumn<Int> mntid(anTab,"MNTSTA");
   ArrayColumn<Float> offset(anTab,"STAXOF");
   ScalarColumn<Float> diam;
   if(anTab.tableDesc().isColumn("DIAMETER")){
     diam.attach(anTab,"DIAMETER"); // this column is optional
   }
   else{
     if (arrnam=="ATCA") diameter=22.;
     if (arrnam=="SMA")  diameter=6.;
     *itsLog << LogIO::WARN 
	     << "ARRAY_GEOMETRY input table does not contain dish DIAMETER column.\n Will assume default diameter for TELESCOPE " 
	     << arrnam << " which is " << diameter <<" m." << LogIO::POST; 
   }

   // All "VLBI" (==arrayXYZ<1000) requires y-axis reflection:
   //  (ATCA looks like "VLBI" in UVFITS, but is already correct)
   //Bool doVLBIRefl= ((array_p!="ATCA") && allLE(abs(arrayXYZ),1000.0));     


   // continue definition of antenna number to antenna id mapping 
   for (Int inRow=0; inRow<nAnt; inRow++) {
     Int ii = anNo(inRow);
     if(antIdFromNo.find(ii) == antIdFromNo.end()){
       Int sz = antIdFromNo.size();
       antIdFromNo[ii] = sz; // append assuming uniqueness
       *itsLog << LogIO::NORMAL << "   antenna_no " << ii << " -> antenna ID " << antIdFromNo[ii] << endl;
     }
   }

   // add antenna info to table (TT)
   ant.setPositionRef(MPosition::ITRF);

   // take into account that the ANTENNA table may already contain rows 
   Int newRows = antIdFromNo.size() - ms_p.antenna().nrow();
   ms_p.antenna().addRow(newRows);

   Int row=0;
   for (Int i=0; i<newRows; i++) {
     
     row = antIdFromNo[anNo(i)];

     if(diam.isNull()){ // no DIAMETER column available
       ant.dishDiameter().put(row,diameter);
     }
     else{
       ant.dishDiameter().put(row,diam(i));
     }
     String mount;
     switch (mntid(i)) {
     case 0: mount="ALT-AZ"; break;
     case 1: mount="EQUATORIAL"; break;
     case 2: mount="X-Y"; break;
     case 3: mount="ORBITING"; break;
     case 4: mount="ALT-AZ+NASMYTH-R"; break;
     case 5: mount="ALT-AZ+NASMYTH-L"; break;
     default: mount="UNKNOWN"; break;
     }
     ant.flagRow().put(row,False);
     ant.mount().put(row,mount);
     String temps = String::toString(anNo(i));
     if(anNo(i)<10){
       temps = String("0")+temps;
     }
     if(name(i)==""){
       ant.name().put(row,String("ANT")+temps);
       ant.station().put(row,arrnam+":"+temps);     
     }
     else{
       ant.name().put(row, name(i));
       ant.station().put(row,name(i));
     }
     Vector<Float> tempf=offset(i);
     Vector<Double> tempd(3);
     for (Int j=0; j<3; j++) tempd[j]=tempf[j];
     ant.offset().put(row,tempd);

     ant.type().put(row,"GROUND-BASED");

     // Do UVFITS-dependent position corrections:
     // ArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
     Vector<Double> corXYZ=antXYZ(i);
/***
     // If nec, rotate coordinates out of local VLA frame to ITRF
     if ( doVLARot ) corXYZ=product(posRot,corXYZ);
 
     // If nec, reflect y-coord to yield right-handed geocentric:
     if ( doVLBIRefl ) corXYZ(1)=-corXYZ(1);
***/
     ant.position().put(row,arrayXYZ+corXYZ);

   }
   // store these items in non-standard keywords for now
   ant.name().rwKeywordSet().define("ARRAY_NAME",arrnam);
   ant.position().rwKeywordSet().define("ARRAY_POSITION",arrayXYZ);

}

void FITSIDItoMS1::fillFeedTable() {
  const Regex trailing(" *$"); // trailing blanks
  MSFeedColumns& msfc(msc_p->feed());

  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* fkw;
  String kwname;
  kwl.first();
  Int nIF = 1;
  Int nPcal = 0;
  while ((fkw = kwl.next())){
    kwname = fkw->name();
    if (kwname == "NO_STKD") {
      //noSTKD = fkw->asInt();
      //cout << kwname << "=" << noSTKD << endl;
    }
    if (kwname == "STK_1") {
      //firstSTK = fkw->asInt();
      //cout << kwname << "=" << firstSTK << endl;
    }
    if (kwname == "NO_BAND") {
      nIF = fkw->asInt();
      //cout << kwname << "=" << nIF << endl;
    }
    if (kwname == "NOPCAL") {
      nPcal = fkw->asInt();
      //cout << kwname << "=" << nPcal << endl;
    }
  }

  //access fitsidi AN table
  Table anTab = oldfullTable("");
  ScalarColumn<Double> time(anTab, "TIME");
  ScalarColumn<Float> timeint;
  ScalarColumn<Double> timeintd;
  try{
    timeint.attach(anTab, "TIME_INTERVAL");
  }
  catch(AipsError&){
    timeintd.attach(anTab, "TIME_INTERVAL");
    *itsLog << LogIO::NORMAL << "Note: this ANTENNA table uses double precision for TIME_INTERVAL. Convention is single."
	    << LogIO::POST;
  }    
  ScalarColumn<String> name(anTab, "ANNAME");
  ScalarColumn<Int> anNo(anTab, "ANTENNA_NO");
  ScalarColumn<Int> array(anTab, "ARRAY");
  ScalarColumn<Int> fqid(anTab, "FREQID");
  ScalarColumn<Int> digLev(anTab, "NO_LEVELS");
  ScalarColumn<String> poltya(anTab, "POLTYA");
  ScalarColumn<String> poltyb(anTab, "POLTYB");

  // if the values for all bands are the same, POLAA, POLAB, POLCALA and POLCALB can be scalar
  Bool POLAisScalar = False;
  ArrayColumn<Float> polaa;
  ArrayColumn<Float> polab;
  ScalarColumn<Float> polaaS;
  ScalarColumn<Float> polabS;
  try{
    polaa.attach(anTab, "POLAA");
    polab.attach(anTab, "POLAB");
  }
  catch(AipsError& x){
    polaaS.attach(anTab, "POLAA");
    polabS.attach(anTab, "POLAB");
    POLAisScalar = True;
    *itsLog << LogIO::WARN << "Treating POLAA and POLAB columns in input ANTENNA table as scalar," 
	    << endl << " i.e. using same value for all bands." << LogIO::POST;
  }

  ArrayColumn<Float> polcala;
  ArrayColumn<Float> polcalb;
  if(nPcal > 0){
    if(anTab.tableDesc().isColumn("POLCALA") && anTab.tableDesc().isColumn("POLCALB")){
      polcala.attach(anTab, "POLCALA");
      polcalb.attach(anTab, "POLCALB");
    }
    else{
      *itsLog << LogIO::WARN << "POLCALA and/or POLCALB column is missing in ANTENNA table." << LogIO::POST;
    }
  }

  //  ArrayColumn<Float> beamfwhm(anTab, "BEAMFWHM"); // this column is optional and there is presently
                                                    // no place for this information in the MS
  Matrix<Complex> polResponse(2,2); 
  polResponse=0.; polResponse(0,0)=polResponse(1,1)=1.;
  Matrix<Double> offset(2,2); offset=0.;
  Int nAnt = anTab.nrow();
  //cout <<"nAnt="<<nAnt<<", nIF="<< nIF;
  Vector<Double> position(3); position=0.;
  Vector<String> polType(2);
  receptorAngle_p = 0;
  receptorAngle_p.resize(2*nAnt*nIF);
  if(POLAisScalar){
    Vector<Float> polanga(nAnt); 
    Vector<Float> polangb(nAnt); 
    polaaS.getColumn(polanga); 
    polabS.getColumn(polangb); 
    for(Int i=0; i<nAnt; i++){
      for(Int j=0; j<nIF; j++){
	Int k = (i*nIF + j);
	receptorAngle_p(2*k+0)=static_cast<Double>(polanga[i])*C::degree;
	receptorAngle_p(2*k+1)=static_cast<Double>(polangb[i])*C::degree;
      }
    }
  }
  else{
    Matrix<Float> polanga(nIF,nAnt); 
    Matrix<Float> polangb(nIF,nAnt); 
    polaa.getColumn(polanga); 
    polab.getColumn(polangb); 
    for(Int i=0; i<nAnt; i++){
      for(Int j=0; j<nIF; j++){
	Int k = (i*nIF + j);
	receptorAngle_p(2*k+0)=static_cast<Double>(polanga(j,i))*C::degree;
	receptorAngle_p(2*k+1)=static_cast<Double>(polangb(j,i))*C::degree;
      }
    }
  }
  //Double testval=1.0;

  // start/continue definition of antenna number to ID mapping in case this table is read before the MS Antenna table is filled
  for (Int inRow=0; inRow<nAnt; inRow++) {
    Int ii = anNo(inRow);
    if(antIdFromNo.find(ii) == antIdFromNo.end()){
      Int sz = antIdFromNo.size();
      antIdFromNo[ii] = sz; // append assuming uniqueness
       *itsLog << LogIO::NORMAL << "   antenna_no " << ii << " -> antenna ID " << antIdFromNo[ii] << endl;
    }
  }

  // record number of digitizer levels for each antenna
  for (Int inRow=0; inRow<nAnt; inRow++) {
    digiLevels[antIdFromNo[anNo(inRow)]] = digLev(inRow);
    if (itsCorrelat == "DIFX" && digLev(inRow) != 2 && digLev(inRow) != 4) {
      *itsLog << LogIO::SEVERE << "unsupported number of digitizer levels for ANTENNA_NO " << anNo(inRow) << "."
	      << endl << " Digital corrections and amplitude scaling will not be applied to"
	      << endl << " baselines involving this antenna." << LogIO::POST;
    }
  }

  // fill the feed table
  Int outRow=-1;
  for (Int inRow=0; inRow<nAnt; inRow++) {
    for (Int inIF=0; inIF<nIF; inIF++) { 
      Int k = inRow*nIF + inIF;
      ms_p.feed().addRow(); outRow++;
      if(antIdFromNo.find(anNo(inRow)) != antIdFromNo.end()){
    	msfc.antennaId().put(outRow,antIdFromNo[anNo(inRow)]);
      }
      else{
    	*itsLog << LogIO::SEVERE << "Internal error: no mapping for ANTENNA_NO "
				<< anNo(inRow) << LogIO::EXCEPTION;
      }
      msfc.beamId().put(outRow,-1);
      msfc.feedId().put(outRow,0); // only one feed ID == 0
      if(!timeint.isNull()){
    	 msfc.interval().put(outRow,timeint(inRow)*C::day);
      }
      else{ // use the double version instead
    	 msfc.interval().put(outRow,timeintd(inRow)*C::day);
      }
     //    msfc.phasedFeedId().put(outRow,-1);
     //msfc.spectralWindowId().put(outRow,-1); // all
      msfc.spectralWindowId().put(outRow,inIF); 
     //msfc.time().put(outRow,0.);
      msfc.time().put(outRow, time(inRow)*C::day);
      msfc.numReceptors().put(outRow,2);
      msfc.beamOffset().put(outRow,offset);
      polType(0) = poltya(inRow);
      polType(1) = poltyb(inRow);
      msfc.polarizationType().put(outRow,polType);
      msfc.polResponse().put(outRow,polResponse);
      msfc.position().put(outRow,position);
     //msfc.receptorAngle().put(outRow,receptorAngle_p(Slice(2*outRow,2)));
      msfc.receptorAngle().put(outRow,receptorAngle_p(Slice(2*k,2)));
    }
  }

}

// method for filling Spectral window table.
void FITSIDItoMS1::fillSpectralWindowTable()
{
  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());

  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* kw;
  String kwname;
  Int nCorr = 1;
  Int nIF_p = 0;
  Int nChan = 0;
  Double zeroRefFreq = 0.0;
  Double refChan = 0.0;
  kwl.first();
  while ((kw = kwl.next()))
    {
      kwname = kw->name();
      if (kwname == "NO_STKD") {
        nCorr = kw->asInt();
      }
      if (kwname == "NO_BAND") {
        nIF_p = kw->asInt();
        //cout << "nIF_p=" << nIF_p << endl;
      }
      if (kwname == "NO_CHAN") {
        nChan = kw->asInt();
        //cout << "nChan=" << nChan << endl;
      }
      if (kwname == "REF_FREQ") {
        zeroRefFreq =  kw->asDouble();
        //cout << "zeroRefFreq=" << zeroRefFreq << endl;
      } 
      if (kwname == "REF_PIXL") {
        refChan = kw->asDouble();
        //cout << "refChan=" << refChan << endl;
      }
    }

//  Int iFreq = getIndex(coordType_p, "FREQ");
//  Int nChan = nPixel_p(iFreq);
//  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
 
  // fill out the polarization info (only single entry allowed in fits input)
  corrType_p = 1;
  corrProduct_p=0;
  ms_p.polarization().addRow();
  msPol.numCorr().put(0,nCorr);
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  msPol.flagRow().put(0,False);

  //access fitsidi FQ table 
  //Table fqTab=bt.fullTable("",Table::Scratch);
  Table fqTab=oldfullTable("");
  Int nRow=fqTab.nrow();
  ScalarColumn<Int> colFqid(fqTab,"FREQID");
  Matrix<Double> ifFreq(nIF_p,nRow);
  Matrix<Float> chWidth(nIF_p,nRow);
  Matrix<Float> totalBandwidth(nIF_p,nRow);
  Matrix<Int> sideband(nIF_p,nRow);
  //cout << "nIF_p=" << nIF_p << endl;
  //cout << "nRow=" << nRow << endl;


  Int nSpW = nIF_p;
  effChBw.resize(nSpW);

  // The type of the column changes according to the number of entries
  if (nIF_p==1) {

    ScalarColumn<Double> colIFFreq(fqTab, "BANDFREQ");
    ScalarColumn<Float> colChWidth(fqTab, "CH_WIDTH"); 
    ScalarColumn<Float> colTotalBW(fqTab,"TOTAL_BANDWIDTH");
    ScalarColumn<Int> colSideBand(fqTab, "SIDEBAND");

    for (Int i=0; i<nRow; i++) {
      ifFreq(0,i)=colIFFreq(i);
      chWidth(0,i)=colChWidth(i);
      totalBandwidth(0,i)=colTotalBW(i);
      sideband(0,i) = colSideBand(i);
    }
  } else {
    ArrayColumn<Double> colIFFreq(fqTab, "BANDFREQ");
    ArrayColumn<Float> colChWidth(fqTab, "CH_WIDTH");
    ArrayColumn<Float> colTotalBW(fqTab, "TOTAL_BANDWIDTH");
    ArrayColumn<Int> colSideBand(fqTab, "SIDEBAND");

    colIFFreq.getColumn(ifFreq);
    colChWidth.getColumn(chWidth);
    colTotalBW.getColumn(totalBandwidth);
    colSideBand.getColumn(sideband);
  }

  for (Int spw=0; spw<nSpW; spw++) {
    ms_p.spectralWindow().addRow();
    ms_p.dataDescription().addRow();
    msDD.spectralWindowId().put(spw,spw);
    //msDD.polarizationId().put(spw,0);
    msDD.flagRow().put(spw,False);
    Int ifc=0;
    Int freqGroup = 0;
    if (nIF_p>0) {
      ifc=spw%nIF_p;
      freqGroup = spw/nIF_p;
    }
    Int fqRow=spw/max(1,nIF_p);
    //if (fqRow != colFrqSel(fqRow)-1)
    if (fqRow != colFqid(fqRow)-1)
      *itsLog << LogIO::WARN << "Trouble interpreting FQ table, ids may be wrong" << LogIO::POST;
    msSpW.name().put(spw,"none");
    msSpW.ifConvChain().put(spw,ifc);
    msSpW.numChan().put(spw,nChan);
    //Double refChan = refPix_p(iFreq);
    //Double refFreq=refVal_p(iFreq)+ifFreq(ifc,fqRow);
    Double refFreq=zeroRefFreq+ifFreq(ifc,fqRow);
    Double chanBandwidth=chWidth(ifc,fqRow);
    Vector<Double> chanFreq(nChan),resolution(nChan);
    for (Int i=0; i < nChan; i++) {
      chanFreq(i)= refFreq + (i+1-refChan) * chanBandwidth;
    }
    effChBw(spw)=abs(chanBandwidth);
    resolution=abs(chanBandwidth);
    msSpW.chanFreq().put(spw,chanFreq);
    msSpW.chanWidth().put(spw,resolution);
    msSpW.effectiveBW().put(spw,resolution);
    msSpW.refFrequency().put(spw,refFreq);
    msSpW.resolution().put(spw,resolution);
    msSpW.totalBandwidth().put(spw,totalBandwidth(ifc,fqRow));
    msSpW.netSideband().put(spw,sideband(ifc, fqRow));
    msSpW.freqGroup().put(spw,freqGroup);
    msSpW.freqGroupName().put(spw,"none");
    msSpW.flagRow().put(spw,False);
    // set the reference frames for frequency
    freqsys_p = MFrequency::TOPO;
    msSpW.measFreqRef().put(spw,freqsys_p);
  }
}                                            

// method to fill Field Table
void FITSIDItoMS1::fillFieldTable()
{
  *itsLog << LogOrigin("FitsIDItoMS()", "fillFieldTable");
  MSFieldColumns& msField(msc_p->field());
  //Table suTab=bt.fullTable("",Table::Scratch);
  Table suTab=oldfullTable("");

  //access the columns in source FITS-IDI subtable
  ScalarColumn<Int> id;
  if(suTab.tableDesc().isColumn("SOURCE_ID")){
    id.attach(suTab, "SOURCE_ID");
  }
  else if(suTab.tableDesc().isColumn("ID_NO.")){
    *itsLog << LogIO::WARN << "No SOURCE_ID column in input SOURCE table. Using deprecated ID_NO column."
	    << LogIO::POST;
    id.attach(suTab, "ID_NO.");
  }
  else{
    throw(AipsError("No SOURCE_ID column in input SOURCE table."));
  }

  ScalarColumn<String> name(suTab,"SOURCE");
  ScalarColumn<Int> qual(suTab,"QUAL");
  ScalarColumn<String> code(suTab,"CALCODE");
  ScalarColumn<Int> fqid(suTab,"FREQID");

  // if the values are the same for all bands, the flux, alpha, freqoff, sysvel, and restfreq columns can be scalar
  ArrayColumn<Float> iflux;
  ArrayColumn<Float> qflux;
  ArrayColumn<Float> uflux;
  ArrayColumn<Float> vflux;
  ArrayColumn<Float> alpha;
  ArrayColumn<Float> foffset;  
  ArrayColumn<Double> foffsetD;  
  ArrayColumn<Double> sysvel;
  ArrayColumn<Double> restfreq;

  ScalarColumn<Float> ifluxS;
  ScalarColumn<Float> qfluxS;
  ScalarColumn<Float> ufluxS;
  ScalarColumn<Float> vfluxS;
  ScalarColumn<Float> alphaS;
  ScalarColumn<Float> foffsetS;
  ScalarColumn<Double> foffsetSD;
  ScalarColumn<Double> sysvelS;
  ScalarColumn<Double> restfreqS;

  try{ // try array column first
    iflux.attach(suTab,"IFLUX"); // I (Jy)
    qflux.attach(suTab,"QFLUX"); // Q 
    uflux.attach(suTab,"UFLUX"); // U 
    vflux.attach(suTab,"VFLUX"); // V 
    alpha.attach(suTab,"ALPHA"); // sp. index
    try{
      foffset.attach(suTab,"FREQOFF"); // fq. offset  
    }
    catch(AipsError& x){
      foffsetD.attach(suTab,"FREQOFF"); // fq. offset  
      *itsLog << LogIO::WARN << "Column FREQOFF is Double but should be Float." << LogIO::POST;
    }
    sysvel.attach(suTab,"SYSVEL"); // sys vel. (m/s)  
    restfreq.attach(suTab,"RESTFREQ"); // rest freq. (hz)  
  }
  catch(AipsError& x){
    ifluxS.attach(suTab,"IFLUX"); // I (Jy)
    qfluxS.attach(suTab,"QFLUX"); // Q 
    ufluxS.attach(suTab,"UFLUX"); // U 
    vfluxS.attach(suTab,"VFLUX"); // V 
    alphaS.attach(suTab,"ALPHA"); // sp. index  
    try{
      foffsetS.attach(suTab,"FREQOFF"); // fq. offset  
    }
    catch(AipsError x){
      foffsetSD.attach(suTab,"FREQOFF"); // fq. offset  
      *itsLog << LogIO::WARN << "Column FREQOFF is Double but should be Float." << LogIO::POST;
    }
    sysvelS.attach(suTab,"SYSVEL"); // sys vel. (m/s)  
    restfreqS.attach(suTab,"RESTFREQ"); // rest freq. (hz)  
    *itsLog << LogIO::WARN << "Treating ?FLUX, ALPHA, FREQOFF, SYSVEL, and RESTFREQ columns in input SOURCE table as scalar,"
	    << endl << " i.e. using same value for all bands." << LogIO::POST;
  }      

  ScalarColumn<Double> ra(suTab,"RAEPO");    //degrees
  ScalarColumn<Double> dec(suTab,"DECEPO");  //degrees
  ScalarColumn<String> equinox;
  ScalarColumn<Double> epoch; //years, alternative for equinox
  if(suTab.tableDesc().isColumn("EQUINOX")){
    equinox.attach(suTab,"EQUINOX"); // string
  }
  else if(suTab.tableDesc().isColumn("EPOCH")){
    epoch.attach(suTab,"EPOCH");
  }
  ScalarColumn<Double> raapp(suTab,"RAAPP");    //degrees
  ScalarColumn<Double> decapp(suTab,"DECAPP");  //degrees
  ScalarColumn<String> veltype(suTab,"VELTYP"); //   
  ScalarColumn<String> veldef(suTab,"VELDEF"); //   
  ScalarColumn<Double> pmra(suTab,"PMRA");   //deg/day
  ScalarColumn<Double> pmdec(suTab,"PMDEC"); //deg/day
  ScalarColumn<Float> pllx(suTab,"PARALLAX"); //arcsec 

  //if (Int(suTab.nrow())<nField) {
  //  *itsLog << LogIO::NORMAL
  //     << "Input Source id's not sequential, adding empty rows in output"
  //     << LogIO::POST;
  //}
  Int outRow=-1;
  //cout << "epoch(fillFieldTable) = " << epoch(0) << endl;
  // set the DIRECTION MEASURE REFERENCE for appropriate columns
  MDirection::Types epochRefZero=MDirection::J2000;
  if(equinox.isNull()){
    if (nearAbs(epoch(0),1950.0,0.01)) {
      epochRefZero=MDirection::B1950;
    }
  }
  else{
    if (equinox(0).contains("1950.0B")) {
      epochRefZero=MDirection::B1950;
    }
  }    
  msc_p->setDirectionRef(epochRefZero);
  for (Int inRow=0; inRow<(Int)suTab.nrow(); inRow++) {
    if (id(inRow) < 1) {
      *itsLog << LogIO::WARN
	      << "Input source id < 1, invalid source id!" << LogIO::POST;     
    }
    Int fld = id(inRow)-1;
    // temp. fix for wrong source id in sma data
    if (fld == -2) fld = fld + 2 ; 

    // add empty rows until the row number in the output matches the source id
    while (fld > outRow) {
      // Append a flagged, empty row to the FIELD table
      ms_p.field().addRow();
      outRow++;
      Vector<MDirection> nullDir(1);
      nullDir(0).set(MVDirection(0.0,0.0), MDirection::Ref(epochRefZero));
      msField.phaseDirMeasCol().put(outRow,nullDir);
      msField.delayDirMeasCol().put(outRow,nullDir);
      msField.referenceDirMeasCol().put(outRow,nullDir);
      msField.flagRow().put(outRow,True);
    }

    msField.sourceId().put(fld,-1); // source table not yet filled in
    msField.code().put(fld,code(inRow));
    msField.name().put(fld,name(inRow));
    Int numPoly = 0;
    //cout << "pmra = "<< pmra(inRow)<< endl;
    //cout << "pmdec = "<< pmdec(inRow) << endl;
    //cout << "abs(pmra) = "<< abs(pmra(inRow)) << endl;
    //cout << "abs(pmdec) = "<< abs(pmdec(inRow)) <<endl;

    if (!nearAbs(pmra(inRow), 0.0) || !nearAbs(pmdec(inRow), 0.0)) { 
      if (abs(pmra(inRow)) > 1000.0 || abs(pmdec(inRow)) > 1000.0) {
        *itsLog << LogIO::WARN << " unreasonably large proper motion parameter(s)" 
		<< " (> 1000 deg/day)! Check input data." << LogIO::POST;
      }
      else { 
        numPoly = 1;
      }
    }

    // The code below will write the direction in B1950 or J2000 coordinates if
    // the direction is constant. However it will use apparent Coordinates (I
    // am not sure if this means APP, JTRUE, BTRUE or what), if the proper
    // motion is non-zero. In all cases the time will be the date of the start 
    // of the observation.
    MDirection::Types epochRef=MDirection::APP;
    MVDirection refDir;
    if (numPoly == 0) {
      if(equinox.isNull()){
	if (near(epoch(inRow),2000.0,0.01)) {
	  epochRef = MDirection::J2000;
	} else if (numPoly == 0 && nearAbs(epoch(inRow),1950.0,0.01)) {
	  epochRef = MDirection::B1950;
	} else {
	  *itsLog << " Cannot handle epoch in SU table: "
		  << epoch(inRow) << LogIO::EXCEPTION;
	}
      }
      else{ // have equinox
	if (equinox(inRow).contains("J2000")) {
	  epochRef = MDirection::J2000;
	} else if (numPoly == 0 && equinox(inRow).contains("1950.0B")) {
	  epochRef = MDirection::B1950;
	} else {
	  *itsLog << " Cannot handle equinox in SU table: "
		  << equinox(inRow) << LogIO::EXCEPTION;
	}
      }
      refDir = MVDirection(ra(inRow)*C::degree,dec(inRow)*C::degree);
    } else {

      // for numPoly!=0, use apparent direction.
      // Need to define 'frame' to use APP.  frame needs the observatory
      // positions and time of observation (can be obtained from RDATE in
      // Fits key). At this momonet, use J2000.
      epochRef = MDirection::J2000;  
      refDir = MVDirection(raapp(inRow)*C::degree,decapp(inRow)*C::degree);
    }
    Vector<MDirection> radecMeas(numPoly+1);
    radecMeas(0).set(refDir, MDirection::Ref(epochRef));
    if (numPoly==1) {
      radecMeas(1).set(MVDirection(pmra(inRow)*C::degree/C::day,
                                   pmdec(inRow)*C::degree/C::day),
                       MDirection::Ref(epochRef));
    }

    // time will be filled later (once Observation Table is filled) 
    // at this moment just put 0.0.
 
    msField.time().put(fld, 0.0);
    msField.numPoly().put(fld,numPoly);
    msField.delayDirMeasCol().put(fld,radecMeas);
    msField.phaseDirMeasCol().put(fld,radecMeas);
    msField.referenceDirMeasCol().put(fld,radecMeas);
    msField.flagRow().put(fld,False);
  }
}

Bool FITSIDItoMS1::fillCorrelatorModelTable()
{

  *itsLog << LogOrigin("FitsIDItoMS()", "fillCorrelatorModelTable");
//  MSCorrelatorModelColumns& msCorrMod(msc_p->correlatorModel());
  *itsLog << LogIO::WARN <<  "not yet implemented" << LogIO::POST;
  return False;

}

Bool FITSIDItoMS1::fillSysCalTable()
{
  *itsLog << LogOrigin("FitsIDItoMS()", "fillSysCalTable");
  MSSysCalColumns& msSysCal(msc_p->sysCal());

  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* fkw;
  String kwname;
  kwl.first();
  Int nIF = 1;
  while ((fkw = kwl.next())){
    kwname = fkw->name();
    if (kwname == "NO_BAND") {
      nIF = fkw->asInt();
    }
  }

  Int nVal=nrows();
  Bool dualPol=False;
  Bool TSYSisScalar=False;

  Table tyTab = oldfullTable("");
  ScalarColumn<Double> time(tyTab, "TIME");
  ScalarColumn<Float> timeint(tyTab, "TIME_INTERVAL");
  ScalarColumn<Int> anNo(tyTab, "ANTENNA_NO");
  ArrayColumn<Float> tsys_1;
  ArrayColumn<Float> tsys_2;
  ScalarColumn<Float> tsys_1S;
  ScalarColumn<Float> tsys_2S;
  try{
    tsys_1.attach(tyTab, "TSYS_1");
    if(tyTab.tableDesc().isColumn("TSYS_2")) {
      tsys_2.attach(tyTab, "TSYS_2"); // this column is optional
      dualPol=True;
    }
  }
  catch(AipsError&){
    tsys_1S.attach(tyTab, "TSYS_1");
    if(tyTab.tableDesc().isColumn("TSYS_2")) {
      tsys_2S.attach(tyTab, "TSYS_2"); // this column is optional
      dualPol=True;
    }
    TSYSisScalar=True;
    *itsLog << LogIO::WARN << "Treating TSYS_1 and TSYS_2 columns in input SYSTEM_TEMPERATURE table as scalar,"
	    << endl << " i.e. using same value for all bands." << LogIO::POST;
  }
  Vector<Float> tsys(dualPol ? 2 : 1);

  Int outRow=-1;
  for (Int inRow=0; inRow<nVal; inRow++) {
    for (Int inIF=0; inIF<nIF; inIF++) {
      ms_p.sysCal().addRow(); outRow++;
      if (antIdFromNo.find(anNo(inRow)) != antIdFromNo.end()) {
	msSysCal.antennaId().put(outRow, antIdFromNo[anNo(inRow)]);
      } else {
    	*itsLog << LogIO::SEVERE << "Internal error: no mapping for ANTENNA_NO "
				<< anNo(inRow) << LogIO::EXCEPTION;
      }
      msSysCal.feedId().put(outRow,0); // only one feed ID == 0
      msSysCal.time().put(outRow,time(inRow)*C::day + rdate);
      msSysCal.interval().put(outRow,timeint(inRow)*C::day);
      msSysCal.spectralWindowId().put(outRow,inIF);
      if (TSYSisScalar) {
	tsys(0)=tsys_1S(inRow);
	if (dualPol)
	  tsys(1)=tsys_2S(inRow);
      } else {
	tsys(0)=tsys_1(inRow)(IPosition(1,inIF));
	if (dualPol)
	  tsys(1)=tsys_2(inRow)(IPosition(1,inIF));
      }
      msSysCal.tsys().put(outRow,tsys);
    }
  }

  return True;
}

Bool FITSIDItoMS1::fillFlagCmdTable()
{
  *itsLog << LogOrigin("FitsIDItoMS()", "fillFlagCmdTable");
  MSFlagCmdColumns& msFlagCmd(msc_p->flagCmd());

  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* fkw;
  String kwname;
  kwl.first();
  Int noSTKD = 0;
  Int firstSTK = -1;
  while ((fkw = kwl.next())){
    kwname = fkw->name();
    if (kwname == "NO_STKD") {
      noSTKD = fkw->asInt();
      //cout << kwname << "=" << noSTKD << endl;
    }
    if (kwname == "STK_1") {
      firstSTK = fkw->asInt();
      //cout << kwname << "=" << firstSTK << endl;
    }
  }

  const char *stokes[] = { "RR", "LL", "RL", "LR", "XX", "YY", "XY", "YX" };
  if (firstSTK >= 0 || noSTKD < 1 || noSTKD > 4 || firstSTK - noSTKD < -9) {
    *itsLog << LogIO::SEVERE << "Unsupported stokes STK_1 " << firstSTK
	    << "NO_STKD" << noSTKD << LogIO::EXCEPTION;
  }

  Int nVal=nrows();

  Table flagTab = oldfullTable("");
  ScalarColumn<Int> srcid(flagTab, "SOURCE_ID");
  ScalarColumn<Int> array(flagTab, "ARRAY");
  ArrayColumn<Int> ants(flagTab, "ANTS");
  ScalarColumn<Int> fqid(flagTab, "FREQID");
  ArrayColumn<Float> timerang(flagTab, "TIMERANG");
  Bool BANDSisScalar = False;
  ArrayColumn<Int> bands;
  ScalarColumn<Int> bandsS;
  ArrayColumn<Int> chans(flagTab, "CHANS");
  ArrayColumn<Int> pflags(flagTab, "PFLAGS");
  ScalarColumn<String> reason(flagTab, "REASON");
  ScalarColumn<Int> severity(flagTab, "SEVERITY");

  try {
    bands.attach(flagTab, "BANDS");
  }
  catch(AipsError& x){
    bandsS.attach(flagTab, "BANDS");
    BANDSisScalar = True;
  }

  Int outRow=-1;
  for (Int inRow=0; inRow<nVal; inRow++) {
    // Check whether flag specification is supported; skip row if it isn't.
    if (array(inRow) != 0) {
      *itsLog << LogIO::SEVERE << "Flagging by array number not supported"
	      << LogIO::POST;
      continue;
    }
    if (fqid(inRow) != -1 && fqid(inRow) != 0) {
      *itsLog << LogIO::SEVERE << "Flagging by frequency setup not supported"
	      << LogIO::POST;
      continue;
    }

    // Check antenna numbers; fail if there are inconsistencies.
    Int ant1 = ants(inRow)(IPosition(1, 0));
    Int ant2 = ants(inRow)(IPosition(1, 1));
    if (ant1 != 0 && antIdFromNo.find(ant1) == antIdFromNo.end()) {
    	*itsLog << LogIO::SEVERE << "No mapping for antenna "
				<< ant1 << LogIO::EXCEPTION;
    }
    if (ant2 != 0 && antIdFromNo.find(ant2) == antIdFromNo.end()) {
    	*itsLog << LogIO::SEVERE << "No mapping for antenna "
				<< ant2 << LogIO::EXCEPTION;
    }

    // Check channel numbers; fail if there are inconsistencies.
    Int chan1 = chans(inRow)(IPosition(1, 0));
    Int chan2 = chans(inRow)(IPosition(1, 1));
    if (chan1 > chan2 || chan1 < -1 || chan2 < -1) {
    	*itsLog << LogIO::SEVERE << "incorrect channel range "
		<< chan1 << "-" << chan2 <<  LogIO::EXCEPTION;
    }

    ms_p.flagCmd().addRow(); outRow++;
    Double time = (timerang(inRow)(IPosition(1, 0)) + timerang(inRow)(IPosition(1, 1))) / 2;
    Double interval = timerang(inRow)(IPosition(1, 1)) - timerang(inRow)(IPosition(1, 0));
    msFlagCmd.time().put(outRow, time * C::day + rdate);
    msFlagCmd.interval().put(outRow, interval * C::day);
    msFlagCmd.type().put(outRow, "FLAG");
    msFlagCmd.reason().put(outRow, reason(inRow));
    msFlagCmd.level().put(outRow, 0);
    msFlagCmd.severity().put(outRow, severity(inRow));
    msFlagCmd.applied().put(outRow, 0);

    // Build command string
    ostringstream cmd;

    // antenna selection
    ant1 = (ant1 == 0) ? -1 : antIdFromNo[ant1];
    ant2 = (ant2 == 0) ? -1 : antIdFromNo[ant2];
    if (ant1 != -1) {
      cmd << "antenna='" << ant1;
      if (ant2 != -1)
	cmd << "&" << ant2;
      cmd << "'";
    }

    // field selection
    if (srcid(inRow) != 0) {
      if (cmd.str().size() > 0)
	cmd << " ";
      cmd << "field='" << srcid(inRow) - 1 << "'";
    }

    // spw selection
    ostringstream spw;
    Vector<Int> bandsV;
    Bool needSpw = False;
    if (BANDSisScalar)
      bandsV = Vector<Int>(1, bandsS(inRow));
    else
      bandsV = bands(inRow);
    for (int band = 0; band < bandsV.shape()(0); band++) {
      if (bandsV[band]) {
	if (spw.str().size() > 0)
	  spw << ",";
	spw << band;
      } else {
	needSpw = True;
      }
    }
    if (!needSpw)
      spw.str("*");
    if (needSpw || chan1 > 0) {
      if (cmd.str().size() > 0)
	cmd << " ";
      cmd << "spw='" << spw.str();
      if (chan1 > 0) {
	cmd << ":"<< chan1 - 1 << "~" << chan2 - 1 << "'";
      }
      cmd << "'";
    }

    // correlation selection
    ostringstream corr;
    Vector<Int> pflagsV = pflags(inRow);
    Bool needCorr = False;
    for (int stk = 0; stk < noSTKD; stk++) {
      if (pflagsV[stk]) {
	if (corr.str().size())
	  corr << ",";
	corr << stokes[stk - firstSTK - 1];
      } else {
	needCorr = True;
      }
    }
    if (needCorr) {
      if (cmd.str().size() > 0)
	cmd << " ";
      cmd << "correlation='" << corr.str() << "'";
    }

    msFlagCmd.command().put(outRow, cmd.str());
  }

  return True;
}

Bool FITSIDItoMS1::fillWeatherTable()
{
  *itsLog << LogOrigin("FitsIDItoMS()", "fillWeatherTable");
  MSWeatherColumns& msWeather(msc_p->weather());

  Int nVal=nrows();

  Table wxTab = oldfullTable("");
  ScalarColumn<Double> time(wxTab, "TIME");
  ScalarColumn<Float> timeint(wxTab, "TIME_INTERVAL");
  ScalarColumn<Int> anNo(wxTab, "ANTENNA_NO");
  ScalarColumn<Float> temperature(wxTab, "TEMPERATURE");
  ScalarColumn<Float> pressure(wxTab, "PRESSURE");
  ScalarColumn<Float> dewpoint(wxTab, "DEWPOINT");
  ScalarColumn<Float> wind_velocity(wxTab, "WIND_VELOCITY");
  ScalarColumn<Float> wind_direction(wxTab, "WIND_DIRECTION");
  ScalarColumn<Float> wvr_h2o;
  if(weather_hasWater_p)
    wvr_h2o.attach(wxTab, "WVR_H2O");
  ScalarColumn<Float> ionos_electron;
  if(weather_hasElectron_p)
    wvr_h2o.attach(wxTab, "IONOS_ELECTRON");

  Int outRow=-1;
  for (Int inRow=0; inRow<nVal; inRow++) {
      ms_p.weather().addRow(); outRow++;
      if (antIdFromNo.find(anNo(inRow)) != antIdFromNo.end()) {
	msWeather.antennaId().put(outRow, antIdFromNo[anNo(inRow)]);
      } else {
    	*itsLog << LogIO::SEVERE << "Internal error: no mapping for ANTENNA_NO "
				<< anNo(inRow) << LogIO::EXCEPTION;
      }
      msWeather.time().put(outRow,time(inRow)*C::day + rdate);
      msWeather.interval().put(outRow,timeint(inRow)*C::day);
      msWeather.dewPoint().put(outRow,dewpoint(inRow)+273.15);
      msWeather.pressure().put(outRow,pressure(inRow));	// hPa == millibar
      msWeather.temperature().put(outRow,temperature(inRow)+273.15);
      msWeather.windDirection().put(outRow,wind_direction(inRow)*C::pi/180.0);
      msWeather.windSpeed().put(outRow,wind_velocity(inRow));
      if(weather_hasWater_p)
	msWeather.H2O().put(outRow,wvr_h2o(inRow));
      if(weather_hasElectron_p)
	msWeather.ionosElectron().put(outRow,wvr_h2o(inRow));
  }

  return True;
}

Bool FITSIDItoMS1::handleGainCurve()
{

  *itsLog << LogOrigin("FitsIDItoMS()", "handleGainCurve");
  // convert the GAIN_CURVE table to a calibration table
  *itsLog << LogIO::WARN <<  "not yet implemented" << LogIO::POST;
  return False;

}

Bool FITSIDItoMS1::handlePhaseCal()
{

  *itsLog << LogOrigin("FitsIDItoMS()", "handlePhaseCal");
  // convert the PHASE-CAL table to a calibration table
  *itsLog << LogIO::WARN <<  "not yet implemented" << LogIO::POST;
  return False;

}

Bool FITSIDItoMS1::handleModelComps()
{

  *itsLog << LogOrigin("FitsIDItoMS()", "handleModelComps");
  // make the content of the MODEL_COMPS table available in the MS (t.b.d.)
  *itsLog << LogIO::WARN <<  "not yet implemented" << LogIO::POST;
  return False;

}


void FITSIDItoMS1::fixEpochReferences() {
  *itsLog << LogOrigin("FitsIDItoMS()", "fixEpochReferences");
  if (timsys_p=="IAT") timsys_p="TAI";
  if (timsys_p=="UTC" || timsys_p=="TAI") {
    if (timsys_p=="UTC") msc_p->setEpochRef(MEpoch::UTC, False);
    if (timsys_p=="TAI") msc_p->setEpochRef(MEpoch::TAI, False);
  } else {
    if (timsys_p!="")
      *itsLog << LogIO::SEVERE << "Unhandled time reference frame: "<<timsys_p<<LogIO::POST;
  }
}

void FITSIDItoMS1::updateTables(const String& MStmpDir)
{
  const Vector<Double> obsTime = msc_p->observation().timeRange()(0);
  //update polarization table
  //this should be a polarization table in _tmp directory  
  //This is expected to call after getAxisInfo.
  String MSFileName;
  MSFileName = MStmpDir + "/FREQUENCY";
  MeasurementSet mssub(MSFileName,Table::Update);
  ms_p = mssub;
  msc_p = new MSColumns(ms_p);
  MSPolarizationColumns& msPol(msc_p->polarization());
  msPol.corrType().put(0,corrType_p);
  msPol.corrProduct().put(0,corrProduct_p);
  delete msc_p;

  //update time in the field table
  MSFileName = MStmpDir + "/SOURCE";
  MeasurementSet mssub2(MSFileName,Table::Update);
  ms_p = mssub2;
  msc_p = new MSColumns(ms_p);
  Int nrow = ms_p.field().nrow();
  MSFieldColumns& msFld(msc_p->field());
  
  for (Int row = 0; row < nrow; row++) { 
    msFld.time().put(row,obsTime(0)); 
    //    cout << "update: obsTime=" << obsTime(0) << endl;
  }
    
  delete msc_p;

  // update time in the feed table
  MSFileName = MStmpDir + "/ANTENNA";
  MeasurementSet mssub3(MSFileName,Table::Update);
  ms_p = mssub3;
  msc_p = new MSColumns(ms_p);
  nrow = ms_p.feed().nrow();
  MSFeedColumns& msFeed(msc_p->feed());
  
  for (Int row = 0; row < nrow; row++) { 
    // cout << "update: feed time =" << msFeed.time()(row) << ", rdate " << rdate  << endl;
    msFeed.time().put(row, msFeed.time()(row) + rdate); // add the reference date  
    // cout << "update: feed time new =" << msFeed.time()(row)  << endl;
  }
    
  delete msc_p;

  msc_p = 0;

} 

bool FITSIDItoMS1::readFitsFile(const String& msFile)
{

  *itsLog << LogOrigin("FitsIDItoMS()", "readFitsFile");

  

  Int nField=0, nSpW=0;
  
  String tmpPolTab;

  const Regex trailing(" *$"); // trailing blanks 
  String extname(FITSIDItoMS1::extname());
  extname=extname.before(trailing);
  
  *itsLog << LogIO::NORMAL << "Found binary table " << extname << LogIO::POST;
  
  if(extname=="UV_DATA") 
    { 
      
      String tmpdir = msFile + "_tmp";
      getAxisInfo();
      
      if(firstMain){
	Bool useTSM=True;
	Bool mainTbl=True;
	
	setupMeasurementSet(msFile, useTSM, mainTbl);
	
	fillMSMainTable(msFile, nField, nSpW);
	fillObsTables();
	
	fixEpochReferences();
	
	updateTables(tmpdir); 
	
	firstMain=False;
      }
      else{
	fillMSMainTable(msFile, nField, nSpW);
	fillObsTables();
      }
    }
  
  else{
    Bool useTSM=False;
    Bool mainTbl=False;
    Bool addCorrMode=False;
    Bool addSyscal=False;
    Bool addWeather=False;

    if (firstSyscal && extname == "SYSTEM_TEMPERATURE") {
      addSyscal=True;
      firstSyscal=False;
    }

    if (firstWeather && extname == "WEATHER") {
      addWeather=True;
      firstWeather=False;
    }

    setupMeasurementSet(msFile, useTSM, mainTbl, addCorrMode, addSyscal,
			addWeather);
    
    Bool success = True; // for the optional tables, we have a return value permitting us
                         // to skip them if they cannot be read

    if(extname=="ARRAY_GEOMETRY") fillAntennaTable();
    else if (extname=="SOURCE") fillFieldTable();
    else if (extname=="FREQUENCY") fillSpectralWindowTable();
    else if (extname=="ANTENNA") fillFeedTable();
    else if (extname=="INTERFEROMETER_MODEL") success =  fillCorrelatorModelTable();
    else if (extname=="SYSTEM_TEMPERATURE") success = fillSysCalTable();
    else if (extname=="FLAG") success =  fillFlagCmdTable(); 
    else if (extname=="GAIN_CURVE") success =  handleGainCurve();
    else if (extname=="PHASE-CAL") success =  handlePhaseCal(); 
    else if (extname=="WEATHER")  success =  fillWeatherTable(); 
    else if (extname=="MODEL_COMPS") success = handleModelComps();
    else if(extname =="BASELINE"
	    || extname =="BANDPASS"
	    || extname =="CALIBRATION"
	    ){
      *itsLog << LogIO::WARN << "FITS-IDI table " << extname 
	      << " not yet supported. Will ignore it." << LogIO::POST;
      return False;
    }
    else {
      *itsLog << LogIO::WARN << "Extension " << extname 
	      << " not part of the FITS-IDI convention. Will ignore it." << LogIO::POST;
      return False;
    }  
    if(!success){
      *itsLog << LogIO::WARN << "The optional FITS-IDI table " << extname 
	      << " could not be read. Will ignore it." << LogIO::POST;
      return False;
    }
  }

  return True;

} 







} //# NAMESPACE CASACORE - END

