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

#include <msfits/MSFits/FitsIDItoMS.h> //
#include <casa/Arrays/ArrayIO.h> //
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/IPosition.h> //
#include <casa/Arrays/Matrix.h> //
#include <casa/Arrays/MatrixMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Slice.h> 
#include <casa/Containers/Record.h>
#include <casa/Exceptions/Error.h>
#include <fits/FITS/fitsio.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <ms/MeasurementSets/MeasurementSet.h> //
#include <ms/MeasurementSets/MSAntennaColumns.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MSDataDescColumns.h>
#include <ms/MeasurementSets/MSFeedColumns.h>
#include <ms/MeasurementSets/MSFieldColumns.h>
#include <ms/MeasurementSets/MSHistoryColumns.h>
#include <ms/MeasurementSets/MSObsColumns.h>
#include <ms/MeasurementSets/MSPolColumns.h>
#include <ms/MeasurementSets/MSSpWindowColumns.h>

#include <measures/Measures/MDirection.h>
#include <measures/Measures/MDoppler.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MeasData.h>
#include <measures/Measures/Stokes.h>
#include <measures/Measures/MeasTable.h>

#include <tables/Tables/Table.h> 
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/ArrColDesc.h> //     
#include <tables/Tables/ScaColDesc.h> //

#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ArrayColumn.h>           
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ColumnDesc.h> //
#include <tables/Tables/StManAipsIO.h> //
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/IncrementalStMan.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <tables/Tables/RowCopier.h> //
#include <tables/Tables/TiledColumnStMan.h>

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableInfo.h>
#include <tables/Tables/TableLock.h>

#include <casa/Utilities/Assert.h> //
#include <casa/Utilities/Regex.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/Fallible.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <fits/FITS/FITSSpectralUtil.h>
#include <fits/FITS/FITSDateUtil.h>
#include <fits/FITS/BinTable.h>
#include <tables/LogTables/NewFile.h>
#include <casa/System/ProgressMeter.h>
#include <casa/sstream.h>
#include <casa/stdio.h>

#include <casa/OS/File.h>
#include <casa/Quanta/MVTime.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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

//initialize UV_DATA flag
Bool FITSIDItoMS1::firstMain = True;

//	
// Constructor
//	
/* 
FITSIDItoMS1::FITSIDItoMS1(FitsInput& fitsin) : 
  BinaryTableExtension(fitsin), infile_p(fitsin), firstMain(True)
{
  //firstMain=True;
  //  infile_p=fitsin;
}
*/


FITSIDItoMS1::FITSIDItoMS1(FitsInput& fitsin)
: BinaryTableExtension(fitsin),
  itsNrMSKs(10),
  itsMSKC(itsNrMSKs," "),
  itsMSKN(itsNrMSKs," "),
  itsMSKV(itsNrMSKs," "),
  itsgotMSK(itsNrMSKs,False),
  infile_p(fitsin), 
  msc_p(0)
{
  /*
    // is there a heap
    if (pcount()) {
	// yes, must read the entire table in at once so that
	// we can have access to the heap as we step through the table
	read(nrows());
	if (notnull(theap())) {
	    uInt heapOffset = theap() - rowsize()*nrows();
	    // Skip to the start of the heap
	    // I don't see any way except to read these bogus bytes
	    Block<Char> junk(heapOffset);
	    ExtensionHeaderDataUnit::read(junk.storage(), heapOffset);
	}
	theheap_p = new char [pcount()];
	AlwaysAssert(theheap_p, AipsError);
	ExtensionHeaderDataUnit::read(theheap_p, pcount());
  */


/*
	// and do some initial decoding of the VADesc related stuff
	vatypes_p = new FITS::ValueType [ncols()];
	AlwaysAssert(vatypes_p, AipsError);
	vaptr_p = new void * [ncols()];
	AlwaysAssert(vaptr_p, AipsError);
	va_p = new VADescFitsField [ncols()];
	AlwaysAssert(va_p, AipsError);
	for (Int i=0;i<ncols();++i) {
	    vaptr_p[i] = 0;
	    if (field(i).fieldtype() == FITS::VADESC) {
		int maxsize;
		FITS::parse_vatform(tform(i), vatypes_p[i], maxsize);
		bind(i, va_p[i]);
		if (vatypes_p[i] == FITS::NOVALUE) {
		    cerr << "Error in VA desc format for column " 
			 << i << " : " << tform(i) << endl;
		} else {
		    switch (vatypes_p[i]) {
		    case FITS::LOGICAL: 
			vaptr_p[i] = (void *)(new FitsLogical[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::BIT: 
			{
			    Int nbytes = maxsize / 8;
			    if (maxsize % 8) nbytes++;
			    maxsize = nbytes;
			}
			// fall throught to BYTE for the actual allocation
		    case FITS::BYTE: 
			vaptr_p[i] = (void *)(new uChar[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::SHORT: 
			vaptr_p[i] = (void *)(new Short[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::LONG: 
			vaptr_p[i] = (void *)(new FitsLong[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::CHAR: 
			vaptr_p[i] = (void *)(new Char[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::FLOAT: 
			vaptr_p[i] = (void *)(new Float[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::DOUBLE:
			vaptr_p[i] = (void *)(new Double[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::COMPLEX:
			vaptr_p[i] = (void *)(new Complex[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    case FITS::DCOMPLEX:
			vaptr_p[i] = (void *)(new DComplex[maxsize]);
			AlwaysAssert(vaptr_p[i], AipsError);
			break;
		    default: 
			cerr << "Impossible VADesc type in column " 
			     << i << " : " << vatypes_p[i] << endl;
			break;
		    }
		}
	    } else {
		vatypes_p[i] = FITS::NOVALUE;
	    }
	}

    }

*/




    //
    // Get some things to remember.
    //
    Int nfield = tfields();      // nr of fields in the FITS table
    itsNelem.resize(nfield);     // nrs of elements per field
    itsNelem = 0;
    itsIsArray.resize(nfield);   // array flags per field
    itsIsArray = False;          // assume scalar-type

    
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
///		cout << "found MSK TYPE    = " << itsMSKV(ikey) << endl;
	    } else if (itsMSKN(ikey) == "SUBTYPE") {
		itsTableInfo.setSubType(itsMSKV(ikey));
///		cout << "found MSK SUBTYPE = " << itsMSKV(ikey) << endl;
	    } else if (itsMSKN(ikey) == "README") {
		itsTableInfo.readmeAddLine(itsMSKV(ikey));
///		cout << "found MSK README  = " << itsMSKV(ikey) << endl;
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
    // Step 4: Create a single-row scratch table, with the table
    // description just built. It will hold the "current" row and is
    // therefore called itsCurRowTab.
    //
    SetupNewTable newtab("", itsTableDesc, Table::Scratch);
    StManAipsIO stman;
    newtab.bindAll (stman);

    
    itsCurRowTab = Table(newtab, 1);

    const Regex trailing(" *$"); // trailing blanks
  
    String extname(FITSIDItoMS1::extname());
    extname = extname.before(trailing);

                
    if(extname!="UV_DATA")
      {     
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
    else 
      {
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


//	The destructor

FITSIDItoMS1::~FITSIDItoMS1()
{
  //delete infile_p;
  delete msc_p;
}


Table FITSIDItoMS1::oldfullTable(const String& tabname)
{
    //
    // Prepare for the creation of a new table with the name requested
    // and with the same table description as itsCurRowTab.  
    //
    SetupNewTable newtab(tabname, getDescriptor(), Table::NewNoReplace); 

    Int nRows = nrows();
    StandardStMan stanStMan (-nRows);
    newtab.bindAll (stanStMan);      

    //
    // Create an empty table with the proper number of rows.
    //
    Table full(newtab,nrows());

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

/*
Table FITSIDItoMS1::fullTable(const String& tabname, 
			      const Table::TableOption taboptn,
			      Bool useIncrSM)
{
   SetupNewTable newtab(tabname, getDescriptor(), taboptn);
   if (useIncrSM) {
       IncrementalStMan stman ("ISM");
       newtab.bindAll(stman);
   }

    //		and actually create the table
    Table full(newtab,nrows());
    RowCopier rowcop(full, itsCurRowTab);
    //			loop over all rows remaining
    for (Int outrow = 0, infitsrow = currrow(); infitsrow < nrows(); 
	 outrow++, infitsrow++) {
	rowcop.copy(outrow, 0);
	//		don't read past the end of the table
	if ((infitsrow+1) < nrows()) {
	    if (!theheap_p) read(1);
	    else ++(*this);
	    fillRow();
	}
    }		// end of loop over rows
    return full;
}
*/

Table FITSIDItoMS1::createTable(const String& tabname)
{
    //
    // Prepare for the creation of a new table with the name requested
    // and with the same table description as itsCurRowTab.  
    //
    SetupNewTable newtab(tabname, getDescriptor(), Table::NewNoReplace); 

    Int nRows = nrows();
    StandardStMan stanStMan (-nRows);
    newtab.bindAll (stanStMan);      

    //
    // Create an empty table with the proper number of rows.
    //
    Table full(newtab,nrows());
    //return full;


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



Table FITSIDItoMS1::fillTable(const String& tabname)
{
    //
    // Open the MAIN table in Update mode and add appropriate number of rows.
    //
    Table newtab(tabname,Table::Update);
    newtab.addRow(nrows());

    //
    // Create a row copier that will repeatedly copy the current row
    // in the single-row table itsCurRowTab to the full-size table.
    //
    RowCopier rowcop(newtab, itsCurRowTab);

    //
    // Loop over all rows remaining.
    //
    //for (Int outrow = 0, infitsrow = currrow();
    //	 infitsrow < nrows(); 
    //	 outrow++, infitsrow++) {

    for (Int outrow = newtab.nrow()-nrows(), infitsrow = currrow();
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
    TableInfo& info = newtab.tableInfo();
    info = itsTableInfo;

    return newtab;
}



Table FITSIDItoMS1::createMainTable(const String& tabname)
{
    //
    // Create MAIN table with the same table description as itsCurRowTab.  
    //
    SetupNewTable newtab(tabname, getDescriptor(), Table::NewNoReplace); 

    Int nRows = nrows();
    StandardStMan stanStMan (-nRows);
    newtab.bindAll (stanStMan);
    
    //
    // For the main MS table, we'd better work with incremental and
    // tiled storage managers.
    //
    Regex trailing(" *$"); // trailing blanks
    String extname(FITSIDItoMS1::extname());
    extname = extname.before(trailing);
    if (extname == "UV_DATA") {
        //                                           
        // Find the tile shape for the data.
        //
	Vector<Int> datashape = getDescriptor()
	    .columnDesc(MS::columnName(MS::DATA))
	    .shape()
	    .asVector();
	Int nChanPerTile = (datashape(1) + 7) / 8;
        Int nRowsPerTile = 16384/(datashape(0)*nChanPerTile);
        Int nTilesInRow = (nRows+nRowsPerTile-1)/nRowsPerTile;
        nRowsPerTile = (nRows+nTilesInRow-1)/nTilesInRow;
	IPosition tileShape (3,datashape(0),nChanPerTile,nRowsPerTile);
	cout << "**tileShape=" << tileShape << endl;
	//
	// Create the storage managers.
	// The StandardStMan will contain about 1024 rows per bucket.
	//
	IncrementalStMan incrStMan ("ISMData");
	TiledShapeStMan  tiledStMan("TiledData", tileShape);
 
	//
	// Bind all columns to the incrStMan except ANTENNA1,
	// ANTENNA2, UVW, SPECTRAL_WINDOW_ID, DATA and FLAG.
	// ANTENNA{1,2}, SPECTRAL_WINDOW_ID, and UVW are bound to the
	// StandardStMan.
	// SPECTRAL_WINDOW_ID, DATA, and FLAG are bound to the
	// tiledStMan.
	//
	newtab.bindAll (incrStMan);
	//
	//newtab.bindColumn(MS::columnName(MS::ANTENNA1),stanStMan);
	//newtab.bindColumn(MS::columnName(MS::ANTENNA2),stanStMan);
	//newtab.bindColumn(MS::columnName(MS::SPECTRAL_WINDOW_ID),stanStMan);
	//newtab.bindColumn(MS::columnName(MS::UVW),stanStMan);
	//
	//newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan);
	//newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan);
    }

    Table maintab(newtab);
    return maintab;
}



Table FITSIDItoMS1::fillMainTable(const String& tabname)
{
    //
    // Open the MAIN table in Update mode and add appropriate number of rows.
    //
    Table maintab(tabname,Table::Update);
    maintab.addRow(nrows());

    //cout << "nrows=" << maintab.nrow() << "\n";
    //
    // Create a row copier that will repeatedly copy the current row
    // in the single-row table itsCurRowTab to the MAIN table.
    //
    RowCopier rowcop(maintab, itsCurRowTab);

    //
    // Loop over all rows remaining.
    //
    //for (Int outrow = 0, infitsrow = currrow();
    //	 infitsrow < nrows(); 
    //	 outrow++, infitsrow++) {

    for (Int outrow = maintab.nrow()-nrows(), infitsrow = currrow();
	 infitsrow < nrows(); 
	 outrow++, infitsrow++) { 

      //cout << "infitsrow=" << infitsrow << "\n";

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
    TableInfo& info = maintab.tableInfo();
    info = itsTableInfo;

    return maintab;
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
	//if(kw->isreserved()) cout << "reserved kwname=" << kwname << endl;
	//else cout << "kwname=" << kwname << endl;
	
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

///	    cout << "doing keyword " << kwname << endl;
	    //
            // If the name already occurs in itsKwSet, issue a warning
            // and overwrite the old keyword.
	    //
	    if (itsKwSet.isDefined(kwname)) {
		cout << "Duplicate keyword name : " << kwname
		     << " most recent occurrance takes precedence" << endl;
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
			cout << "MSBinaryTable found unknown MSK keyword: "
			     << kwname << ". It will be ignored" << endl;
		    }
		} else {
		    cout << "MSBinaryTable found unknown MSK keyword: "
			 << kwname << ". It will be ignored" << endl;
		}
	    } else {

		// Add a keyword of the proper type to the keyword
		// list.
		//
		switch (kw->type()) {
		case FITS::NOVALUE: itsKwSet.define(kwname,"");
		    // NOVALUE fields become string keywords with an emtpy string.
		    cout << "FITS::NOVALUE found" << endl;
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
		    cerr << "Error: unrecognized table data type for keyword "
			 << kwname << " type = " << kw->type() << endl;
		    cerr << "That should not have happened" << endl;
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
///   cout << "defined table keyword VERSION = " << extver() << endl;
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
    //
    String extname(FITSIDItoMS1::extname());
    extname = extname.before(trailing);

    Vector<Int> maxis(0);
    if(extname=="UV_DATA") 
      {
        const FitsKeyword* kw;
        String kwname;
        kwl.first();
        uInt ctr=0;

        while((kw = kwl.next())) 
	  {
	    kwname = kw->name();
	    if(kwname.at(0,5)=="MAXIS")
	      { 
		maxis.resize(++ctr,True);
		maxis(ctr-1)=kw->asInt();
		cout << "**maxis=" << maxis << endl;
	      }
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

	if(extname=="UV_DATA" && colname=="FLUX")
	  colname="DATA";

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
	    cout << "Duplicate column name : " << ttype(icol)
		 << " this occurance will be named " << colname << endl;

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
	Bool isString = False;
	Bool isSHAPEd = False;
	String SHAPEstr = "()";
	cout << colname << " is";
	if (field(icol).fieldtype() == FITS::CHAR
	    || field(icol).fieldtype() == FITS::STRING) {
	    isString = True;
	    cout << " a String-type column";
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
	    cout << " a multi-element non-String-type Array column";

	} else {
	    cout << " a non-String-type column";
	    //
	    // See whether MSK SHAPE is defined. If so: array.
	    //
	    for (uInt ikey=0; ikey<itsNrMSKs; ikey++) {
		if (itsgotMSK(ikey) && (itsMSKC(ikey)==colname)) {
		    if (itsMSKN(ikey) == "SHAPE") {
			SHAPEstr = itsMSKV(ikey);
			itsIsArray(icol) = True;
			isSHAPEd = True;
			cout << " (0/1 element Vector)";
			itsgotMSK(ikey) = False;
		    }
		}
	    }
	}
	cout << endl;

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
		cout << "   shape = " << shape << endl;
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
		cout << "   shape = " << shape << endl;

	    } else if (isSHAPEd) {
		//
		// Vector of strings or degenerated vector.  Use the
		// substring inside the parentheses as shape.
		//
		dimstr = SHAPEstr(1,SHAPEstr.length()-2);
		shape(0) = atoi(dimstr.chars());
		cout << "   shape = " << shape << endl;

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
		cout << "    shape = " << shape << endl;
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
///			    cout << "found MSK OPTIONS = DIRECT for "
///				 << colname << endl;
			} else {
			    cout << "Invalid MSK OPTIONS = "
				 << itsMSKV(ikey) << " is ignored." << endl;
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
	    cerr << "Error: column " << icol
		 << " has untranslatable type " << field(icol).fieldtype()
		 << " This should NEVER happen " << endl;
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
  //itsLog << LogOrigin("MSFitsInput", "getPrimaryGroupAxisInfo");
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

  if(extname=="UV_DATA") 
    {
      Int nAxis = 0;
      uInt imaxis = 0;
      uInt idx = 0;
      Bool setMAXIS = False;
      const FitsKeyword* kw;
      String kwname;
      kwl.first();


      //while((kw = kwl.next())&& setMAXIS == False) 
      while(kw = kwl.next()) 
        {
          kwname = kw->name();
          //cout << "kwname1=" << kwname <<endl;
          //cout << "kwname.length=" << kwname.length() <<endl;
          //cout << "idx=" << idx <<endl;
          if(kwname == "MAXIS") 
            {
              nAxis = kw->asInt();
              cout << "nAxis=" << nAxis << endl;;
              setMAXIS = True;
            }
         }
      if (nAxis < 1) {
      	cout << "Data has no axes!" << LogIO::EXCEPTION;
      }
      nPixel_p.resize(nAxis);
      refVal_p.resize(nAxis);
      refPix_p.resize(nAxis);
      delta_p.resize(nAxis);
      coordType_p.resize(nAxis);

      kwl.first();
      while((kw = kwl.next()))
        {
          kwname = kw->name();
          idx = kw->index() - 1; 
          //cout << "kwname=" << kwname <<endl;
          // Note: MAXISn are non-reserved FITS keywords
          // so 'n' does NOT recongnized as index. 
          if(kwname.at(0,5)=="MAXIS" && kwname.length()>5)
            { 
	      nPixel_p(imaxis++)=kw->asInt();
	    }
           if(kwname.at(0,5)=="CTYPE")
            { 
              coordType_p(idx)=kw->asString(); 
              coordType_p(idx)=coordType_p(idx).before(trailing);
            }
          else if(kwname.at(0,5)=="CDELT")
            {    
              delta_p(idx)=kw->asDouble();
            }
          else if(kwname.at(0,5)=="CRPIX")
            {
              refPix_p(idx)=kw->asDouble();
            }
          else if(kwname.at(0,5)=="CRVAL" ) 
            {
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

      cout << "nPixel_p=" << nPixel_p << endl;
      cout << "coordType_p=" << coordType_p << endl;
      cout << "refVal_p=" << refVal_p << endl;
      cout << "refPix_p=" << refPix_p << endl;
      cout << "delta_p=" << delta_p << endl;
    }

  // Check if required axes are there
  if (getIndex(coordType_p, "COMPLEX") < 0) {
    cout << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "STOKES") < 0) {
    cout << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "FREQ") < 0) {
    cout << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "RA") < 0) && 
      (getIndex(coordType_p, "RA---SIN") < 0) && 
      (getIndex(coordType_p, "RA---NCP") < 0) && 
      (getIndex(coordType_p, "RA---SCP") < 0)) {
    cout << "Data does not have a RA axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "DEC") < 0) && 
      (getIndex(coordType_p, "DEC--SIN") < 0) && 
      (getIndex(coordType_p, "DEC--NCP") < 0) && 
      (getIndex(coordType_p, "DEC--SCP") < 0)) {
    cout << "Data does not have a DEC axis" << LogIO::EXCEPTION;
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
    // convert AIPS-convention Stokes description to aips++ enum
    cout << "corrType_p="<< corrType_p(i) <<endl;
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
	cout << "Unknown Correlation type: " << corrType_p(i) 
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
      cout << "corrProcut_p(0,"<< i <<")=" << corrProduct_p(0,i);
    } else {
      cout << "Cannot deduce receptor 1 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
    receptor = Stokes::receptor2(cType);
    if (receptor.isValid()) {
      corrProduct_p(1,i) = receptor;
      cout << "corrProcut_p(1,"<< i <<")=" << corrProduct_p(1,i);
    } else {
      cout << "Cannot deduce receptor 2 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
  }

  // Save the object name, we may need it (for single source fits)
  const FitsKeyword* kwp;
  object_p = (kwp=kw(FITS::OBJECT)) ? kwp->asString() : "unknown";
  object_p=object_p.before(trailing);
  // Save the array name
  array_p = (kwp=kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
  array_p=array_p.before(trailing);
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
					  itsLog,
					  header);
  }

/*
  if (nAxis < 1) {
    itsLog << "Data has no axes!" << LogIO::EXCEPTION;
  }
  nPixel_p.resize(nAxis);
  refVal_p.resize(nAxis);
  refPix_p.resize(nAxis);
  delta_p.resize(nAxis);
  coordType_p.resize(nAxis);
  for (Int i = 0; i < nAxis; i++)  {
    nPixel_p(i) = priGroup_p.dim(i);
    if (nPixel_p(i) < 0) {
      itsLog << "Axes " << i << " cannot have a negative value" 
	     << LogIO::EXCEPTION;
    }
    coordType_p(i) = priGroup_p.ctype(i);
    coordType_p(i) = coordType_p(i).before(trailing);
    refVal_p(i) = static_cast<Double>(priGroup_p.crval(i));
    refPix_p(i) = static_cast<Double>(priGroup_p.crpix(i));
    delta_p(i) = static_cast<Double>(priGroup_p.cdelt(i));
  }
  // Check if required axes are there
  if (getIndex(coordType_p, "COMPLEX") < 0) {
    itsLog << "Data does not have a COMPLEX axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "STOKES") < 0) {
    itsLog << "Data does not have a STOKES axis" << LogIO::EXCEPTION;
  }
  if (getIndex(coordType_p, "FREQ") < 0) {
    itsLog << "Data does not have a FREQ axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "RA") < 0) && 
      (getIndex(coordType_p, "RA---SIN") < 0) && 
      (getIndex(coordType_p, "RA---NCP") < 0) && 
      (getIndex(coordType_p, "RA---SCP") < 0)) {
    itsLog << "Data does not have a RA axis" << LogIO::EXCEPTION;
  }
  if ((getIndex(coordType_p, "DEC") < 0) && 
      (getIndex(coordType_p, "DEC--SIN") < 0) && 
      (getIndex(coordType_p, "DEC--NCP") < 0) && 
      (getIndex(coordType_p, "DEC--SCP") < 0)) {
    itsLog << "Data does not have a DEC axis" << LogIO::EXCEPTION;
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
    // convert AIPS-convention Stokes description to aips++ enum
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
    default: 
      if (corrType_p(i) < 0) {
	itsLog << "Unknown Correlation type: " << corrType_p(i) 
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
    } else {
      itsLog << "Cannot deduce receptor 1 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
    receptor = Stokes::receptor2(cType);
    if (receptor.isValid()) {
      corrProduct_p(1,i) = receptor;
    } else {
      itsLog << "Cannot deduce receptor 2 for correlations of type: " 
	     << Stokes::name(cType) << LogIO::EXCEPTION;
    }
  }
  // Save the object name, we may need it (for single source fits)
  const FitsKeyword* kwp;
  object_p = (kwp=priGroup_p.kw(FITS::OBJECT)) ? kwp->asString() : "unknown";
  object_p=object_p.before(trailing);
  // Save the array name
  array_p = (kwp=priGroup_p.kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
  array_p=array_p.before(trailing);
  // Save the RA/DEC epoch (for ss fits)
  epoch_p = (kwp=priGroup_p.kw(FITS::EPOCH)) ? kwp->asFloat() : 2000.0;

  // Get the spectral information
  freqsys_p = MFrequency::TOPO;
  restfreq_p = 0.0;
  Record header;
  Vector<String> ignore;
  Bool ok = FITSKeywordUtil::getKeywords(header, priGroup_p.kwlist(), ignore);
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
					  itsLog,
					  header);
  }

*/
}

//void FITSIDItoMS1::setupMeasurementSet(const String& MSFileName, Bool useTSM) {
void FITSIDItoMS1::setupMeasurementSet(const String& MSFileName, Bool useTSM, 
Bool mainTbl) {

   Int nCorr = 0;
   Int nChan = 0;
   Int nIF_p = 0;

  if(mainTbl) {
    nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
    nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
    nIF_p = getIndex(coordType_p,"BAND");
    if (nIF_p>=0) {
      nIF_p=nPixel_p(nIF_p);
    } else {
      nIF_p=1;
    }
  } 
  //cout << "===> nIF_p=" << nIF_p <<endl; 
  // Make the MS table
  TableDesc td = MS::requiredTableDesc();
  
  // Even though we know the data is going to be the same shape throughout I'll
  // still create a column that has a variable shape as this will permit MS's
  // with other shapes to be appended.
  MS::addColumnToDesc(td, MS::DATA, 2);

  
  // add this optional column because random group fits has a
  // weight per visibility
  MS::addColumnToDesc(td, MS::WEIGHT_SPECTRUM, 2);
  
  if(mainTbl) {
  if (useTSM) {
    td.defineHypercolumn("TiledData",3,
 			 stringToVector(MS::columnName(MS::DATA)));
    td.defineHypercolumn("TiledFlag",3,
 			 stringToVector(MS::columnName(MS::FLAG)));
    td.defineHypercolumn("TiledFlagCategory",4,
 			 stringToVector(MS::columnName(MS::FLAG_CATEGORY)));
    td.defineHypercolumn("TiledWeight",3,
 			 stringToVector(MS::columnName(MS::WEIGHT_SPECTRUM)));
    td.defineHypercolumn("TiledUVW",2,
 			 stringToVector(MS::columnName(MS::UVW)));
  }
  }
  //if(firstMain)
    SetupNewTable newtab(MSFileName, td, Table::New);
  //else
  //Table maintab(tabname,Table::Update);
  //Table maintab(MSFileName,Table::Update);
  
  // Set the default Storage Manager to be the Incr one
  IncrementalStMan incrStMan ("ISMData");
  newtab.bindAll(incrStMan, True);
  // bind ANTENNA2 to the standardStMan as it changes every row
  StandardStMan aipsStMan;
  newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
 
  //newtab.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan);
 
// TT
  if(mainTbl) {
  if (useTSM) {
    Int tileSize=nChan/10+1;
    // make the tile about 128k big
    TiledShapeStMan tiledStMan1("TiledData",
 				IPosition(3,nCorr,tileSize,
 					  16384/nCorr/tileSize));
    TiledShapeStMan tiledStMan1f("TiledFlag",
  				 IPosition(3,nCorr,tileSize,
 					   16384/nCorr/tileSize));
    TiledShapeStMan tiledStMan1fc("TiledFlagCategory",
				  IPosition(4,nCorr,tileSize,1,
 					   16384/nCorr/tileSize));
    TiledShapeStMan tiledStMan2("TiledWeight",
 				IPosition(3,nCorr, tileSize,
 					  16384/nCorr/tileSize));
    TiledColumnStMan tiledStMan3("TiledUVW",
 				 IPosition(2,3,1024));
    // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
    newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan1);
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan1f);
    newtab.bindColumn(MS::columnName(MS::FLAG_CATEGORY),tiledStMan1fc);
    newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),tiledStMan2);
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStMan3);
  } else {
    newtab.bindColumn(MS::columnName(MS::DATA),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::FLAG),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM),aipsStMan);
    newtab.bindColumn(MS::columnName(MS::UVW),aipsStMan);
  }
  }
  // avoid lock overheads by locking the table permanently
  TableLock lock(TableLock::PermanentLocking);
  MeasurementSet ms(newtab,lock);
  //MeasurementSet ms(newtab);

  // create all subtables
  // we make new tables with 0 rows
  Table::TableOption option=Table::New;
  // Set up the subtables for the UVFITS MS
  ms.createDefaultSubtables(option);
 
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


void FITSIDItoMS1::fillMSMainTable(const String& MSFileName, Int& nField, Int& nSpW)
{
  //itsLog << LogOrigin("MSFitsInput", "fillMSMainTable");
  // Get access to the MS columns
  //MSColumns& msc(*msc_p);

  
  //if(!firstMain)
  //  {
      MeasurementSet ms(MSFileName,Table::Update);
      MSColumns msc(ms);
      //  }
  if(!firstMain)
    {
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
  cout << "tFields=" << tFields << endl;
  cout << "nrows=" << nRows << endl;
  cout << "msnrows=" << MSnRows << endl; 
  //cout << "scanNumber=" << nScan<< endl;

  Vector<String> tType(tFields);

  for (Int i=0; i < tFields; i++) {
    tType(i) = ttype(i); 
    tType(i) = tType(i).before(trailing);
  }

  cout << "tType=" << tType(0) << endl;

  cout << "STOKES POS=" << getIndex(coordType_p,"STOKES") << endl;
  cout << "FREQ POS=" << getIndex(coordType_p,"FREQ") << endl;

  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  Int nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
  cout << "nCorr=" << nCorr << endl;
  cout << "nChan=" << nChan << endl;

  Matrix<Complex> vis(nCorr,nChan);
  Vector<Float> sigma(nCorr);
  Matrix<Float> weightSpec(nCorr, nChan);
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
    throw(AipsError("MSFitsInput: Cannot find UVW information"));
  }
  
  // get index for baseline
  Int iBsln = getIndex(tType, "BASELINE");
  // get indices for time
  Int iTime0 = getIndex(tType, "DATE");
  Int iTime1 = getIndex(tType, "TIME");
  // get index for source
  Int iSource = getIndex(tType, "SOURCE");
  // get index for Freq
  Int iFreq = getIndex(tType, "FREQID");
  // get index for FLUX
  Int iFlux = getIndex(tType, "FLUX");

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
  //itsLog << LogIO::NORMAL << "Reading and writing " << nGroups
  //	 << " visibility data"<< LogIO::POST;
  cout << "Reading and writing visibility data" << endl;
  Int row=-1;
  Double startTime, interval;
  startTime=0.0; interval=1;

  ProgressMeter meter(0.0, nRows*1.0, "FITS-IDI Filler", "Rows copied", "",
 		      "", True,  nRows/100);

  Vector<Double> uvw(3); // Move this temporary out of the loop
  Vector<Float> _uvw(3); 
  Int lastAnt1, lastAnt2, lastArray, lastSpW, lastSourceId;
  lastAnt1=-1; lastAnt2=-1; lastArray=-1; lastSpW=-1; lastSourceId=-1;
  Int putrow = -1;
  Double lastTime=0;
  Bool lastRowFlag=False;
  Float lastWeight=0.0;
  Int nScan = 0;

  if (firstMain) {
    putrow = -1; 
  } else {
    putrow = MSnRows - 1; 
    nScan = scans(putrow) + 1;      
  }
  cout << "scanNumber=" << nScan<< endl;

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

    if (iTime1>=0)
      {
	Double time1;
	memcpy(&time1, (static_cast<Double *>(data_addr[iTime1])), sizeof(Double));
	time1 *= tscal(iTime1);
	time1 += tzero(iTime1); 
	time += time1;
      }
    //cout << "TIME=" << time << endl; 

    Long _baseline;
    Float baseline;
    memcpy(&_baseline, (static_cast<Long *>(data_addr[iBsln])), sizeof(Long));
    baseline=static_cast<Float>(_baseline); 
    baseline *= tscal(iBsln);
    baseline += tzero(iBsln); 
    //cout << "BASELINE=" << baseline << endl; 

    memcpy(&(_uvw(0)), (static_cast<Float *>(data_addr[iU])), sizeof(Float));
    uvw(0)=static_cast<Double>(_uvw(0));
    uvw(0) *= tscal(iU);
    uvw(0) += tzero(iU); 
    //cout << "uvw(0)=" << uvw(0) << endl; 

    memcpy(&(_uvw(1)), (static_cast<Float *>(data_addr[iV])), sizeof(Float));
    uvw(1)=static_cast<Double>(_uvw(1));
    uvw(1) *= tscal(iV);
    uvw(1) += tzero(iV); 
    //cout << "uvw(1)=" << uvw(1) << endl; 

    memcpy(&(_uvw(2)), (static_cast<Float *>(data_addr[iW])), sizeof(Float));
    uvw(2)=static_cast<Double>(_uvw(2));
    //uvw(2) = *(static_cast<Int *>((*(fld[iW])).data()));
    uvw(2) *= tscal(iW);
    uvw(2) += tzero(iW); 
    //cout << "uvw(2)=" << uvw(2) << endl; 

    time  *= C::day; 
    //cout << "TIME=" << time << endl; 

    // make a guess at the integration time
    //if (row<0) startTime = time;
    if (row<0) {
      startTime = time;
      if (firstMain) startTime_p = startTime;
    }
    if (time > startTime) {
      interval=time-startTime;
      msc.interval().fillColumn(interval);
      msc.exposure().fillColumn(interval);
      startTime = DBL_MAX; // do this only once
    }

    Int array = Int(100.0*(baseline - Int(baseline)+0.001));
    Int ant1 = Int(baseline)/256; 
    nAnt_p = max(nAnt_p,ant1);
    Int ant2 = Int(baseline) - ant1*256; 
    nAnt_p = max(nAnt_p,ant2);
    ant1--; ant2--; // make 0-based

    if(mydebug) cout << "baseline=" << baseline << endl;
    
    // Convert U,V,W from units of seconds to meters
    uvw *= C::c;

    Int count = 0;

    /*
    double flux[2*nCorr*nChan*4];
    for(uInt ctr=0;ctr<(2*nCorr*nChan*4);ctr++)
      {
	memcpy(&(flux[ctr]), (fitsrow + (fitsrowsize-(2*nCorr*nChan*4)) + (sizeof(Double)*ctr)), sizeof(Double));
	cout << flux[ctr] << " ";
      }
    */

       
    /* 
    Float test_uu;
    memcpy(&test_uu,fitsrow,sizeof(Float));
    cout << "TEST_UU=" << test_uu << endl;

    memcpy(&test_uu,table,sizeof(Float));
    cout << "TEST_UU=" << test_uu << endl;
    */

    Float test_baseline;
    memcpy(&test_baseline,fitsrow,sizeof(Long));
    //cout << "*****TEST_BASELINE FITS=" << test_baseline << endl;

    memcpy(&test_baseline,table,sizeof(Long));
    //cout << "*****TEST_BASELINE TABLE=" << test_baseline << endl;

    Long new_baseline;
    memcpy(&new_baseline,static_cast<Long *>(data_addr[iBsln]),sizeof(Long));
    //cout << "*****NEW_BASELINE ADDR=" << new_baseline << endl;
        

    Float visReal;
    Float visImag;

    //***temporal fix  
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
      //ms_p.addRow();
      ms.addRow();
      row++;
      putrow++;
 
      for (Int chan=0; chan<nChan; chan++) {
 	for (Int pol=0; pol<nCorr; pol++) {
           
          //memcpy(&visReal, (static_cast<Float *>(data_addr[iFlux])) + (sizeof(Float)*count++), sizeof(Float));
          memcpy(&visReal, (static_cast<Float *>(data_addr[iFlux])) + count++, sizeof(Float));
          if (mydebug) cout << "COUNT=" << count <<"ifno="<< ifno <<"chan="<< chan<<"pol="<< pol  << endl;
	  //visReal *= tscal(iFlux);
	  //visReal += tzero(iFlux); 

	  //memcpy(&visImag, (static_cast<Float *>(data_addr[iFlux])) + (sizeof(Float)*count++), sizeof(Float));     
	  memcpy(&visImag, (static_cast<Float *>(data_addr[iFlux])) + count++, sizeof(Float));     
	  //visImag *= tscal(iFlux);
	  //visImag += tzero(iFlux); 
          //cout<<"visReal="<< visReal << "visImag="<< visImag<<endl;

	  //const Float wt = priGroup_p(count++); 

	  const Int p = corrIndex_p[pol];
/*
 	  if (wt < 0.0) {
	    weightSpec(p, chan) = -wt;
	    flag(p, chan) = True;
	  } else {                            
	    weightSpec(p, chan) = wt;
	    flag(p, chan) = False;
	  }
*/
	  vis(p, chan) = Complex(visReal, visImag);
 	}
      }


      // fill in values for all the unused columns
      //if (row==0 && putrow==0) {
      if (row==0) {
 	msc.exposure().put(row,interval);
 	msc.feed1().put(row,0);
 	msc.feed2().put(row,0);
 	msc.flagRow().put(row,False);
 	lastRowFlag=False;
 	msc.interval().put(row,interval);
 	//msc.scanNumber().put(row,0);
 	//msc.scanNumber().put(row,nScan);
 	msc.processorId().put(row,-1);
 	msc.observationId().put(row,0);
 	msc.stateId().put(row,-1);
 	Vector<Float> tmp(nCorr); tmp=1.0;
 	msc.sigma().put(row,tmp);
 	msc.weight().put(row,tmp);
 	lastWeight=1.0;
      }

      //cout << "$$$$$$$$$$$$$$$$"<<endl;
      //cout << "row, putrow at 2432="<< row <<","<< putrow << endl;
      //cout << "nScan="<< nScan << endl;
      //cout << "$$$$$$$$$$$$$$$$"<<endl;
     

      msc.scanNumber().put(putrow,nScan);

      //msc.data().put(row,vis);
      msc.data().put(putrow,vis);
      // single channel case: make weight and weightSpectrum identical.
      // multichannel case: weight should not be used.
      if (nChan==1) { 
 	const Vector<Float> weight(weightSpec.column(0).copy()); 
 	if (weight(0)!=lastWeight) {
 	  //msc.weight().put(row,weight);
 	  msc.weight().put(putrow,weight);
 	  lastWeight=weight(0);
 	}
      }
      //msc.weightSpectrum().put(row,weightSpec); 
      //msc.flag().put(row,flag);
      //msc.flagCategory().put(row,flagCat);
      msc.weightSpectrum().put(putrow,weightSpec); 
      msc.flag().put(putrow,flag);
      msc.flagCategory().put(putrow,flagCat);
      Bool rowFlag=allEQ(flag,True);
      if (rowFlag!=lastRowFlag) {
 	//msc.flagRow().put(row,rowFlag);
 	msc.flagRow().put(putrow,rowFlag);
 	lastRowFlag=rowFlag;
      }

      if (ant1!=lastAnt1) {
 	//msc.antenna1().put(row,ant1);
 	msc.antenna1().put(putrow,ant1);
 	lastAnt1=ant1;
      }
      if (array!=lastArray) {
 	//msc.arrayId().put(row,array);
 	msc.arrayId().put(putrow,array);
 	lastArray=array;
      }
      // Always put antenna2 since it is bound to the
      // aipsStMan and is assumed to change every
      // row
      //msc.antenna2().put(row,ant2);
      msc.antenna2().put(putrow,ant2);
      if (time!=lastTime) {
 	//msc.time().put(row,time);
 	//msc.timeCentroid().put(row,time);
 	msc.time().put(putrow,time);
 	msc.timeCentroid().put(putrow,time);
 	lastTime=time;
        lastTime_p=lastTime;
        //cout << "lastTime_p="<< lastTime <<endl;

      }
      //msc.uvw().put(row,uvw);
      msc.uvw().put(putrow,uvw);
      
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
 	//msc.dataDescId().put(row,spW);
 	msc.dataDescId().put(putrow,spW);
 	nSpW = max(nSpW, spW+1);
 	lastSpW=spW;
      }
    
      // store the sourceId 
      Int sourceId = 0;
      if (iSource>=0) {
 	// make 0-based
        memcpy(&sourceId, (static_cast<Int *>(data_addr[iSource])), sizeof(Int));
        sourceId *= (Int)tscal(iSource);
        sourceId += (Int)tzero(iSource); 
 	sourceId--; // make 0-based
      }
      if (sourceId!=lastSourceId) {
 	//msc.fieldId().put(row,sourceId);
 	msc.fieldId().put(putrow,sourceId);
 	nField = max(nField, sourceId+1);
 	lastSourceId=sourceId;
      }
    }
    meter.update((trow+1)*1.0);
  }

  // fill the receptorAngle with defaults, just in case there is no AN table
  receptorAngle_p=0;
  // set the Measure References

  /*
  for(Int ctr=0;ctr<ncols();ctr++)
    {
      cout << "**fld[" << ctr << "]=" << *(fld[ctr]) << endl;
    }
  */

/*
  //itsLog << LogOrigin("MSFitsInput", "fillMSMainTable");
  // Get access to the MS columns
  MSColumns& msc(*msc_p);
  const Regex trailing(" *$"); // trailing blanks

  // get the random group parameter names
  Int nParams;
  Int nGroups;
  nParams= priGroup_p.pcount(); 
  nGroups = priGroup_p.gcount(); 
  Vector<String> pType(nParams);
  for (Int i =0; i < nParams; i++) {
    pType(i) = priGroup_p.ptype(i); 
    pType(i) = pType(i).before(trailing);
  }

  Int nCorr = nPixel_p(getIndex(coordType_p,"STOKES"));
  Int nChan = nPixel_p(getIndex(coordType_p,"FREQ"));
  
  Matrix<Complex> vis(nCorr,nChan);
  Vector<Float> sigma(nCorr);
  Matrix<Float> weightSpec(nCorr, nChan);
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
  iU = getIndexContains(pType,"UU"); 
  iV = getIndexContains(pType,"VV");
  iW = getIndexContains(pType,"WW");
  if (iU < 0 || iV < 0 || iW < 0) {
    throw(AipsError("MSFitsInput: Cannot find UVW information"));
  }
  // get index for baseline
  Int iBsln = getIndex(pType, "BASELINE");
  // get indices for time
  Int iTime0 = getIndex(pType, "DATE",0);
  Int iTime1 = getIndex(pType, "DATE",1);
  // get index for source
  Int iSource = getIndex(pType, "SOURCE");
  // get index for Freq
  Int iFreq = getIndex(pType, "FREQSEL");

  receptorAngle_p.resize(1);
  nAnt_p=0;
  itsLog << LogIO::NORMAL << "Reading and writing " << nGroups
     << " visibility groups"<< LogIO::POST;
  Int row=-1;
  Double startTime, interval;
  startTime=0.0; interval=1;

  ProgressMeter meter(0.0, nGroups*1.0, "UVFITS Filler", "Groups copied", "",
 		      "", True,  nGroups/100);

  Vector<Double> uvw(3); // Move this temporary out of the loop
  Int lastAnt1, lastAnt2, lastArray, lastSpW, lastSourceId;
  lastAnt1=-1; lastAnt2=-1; lastArray=-1; lastSpW=-1; lastSourceId=-1;
  Double lastTime=0;
  Bool lastRowFlag=False;
  Float lastWeight=0.0;
  for (Int group=0; group<nGroups; group++) {
    // Read next group and
    // get time in MJD seconds
    const Double JDofMJD0=2400000.5;
    priGroup_p.read();
    Double time = priGroup_p.parm(iTime0); 
    time -= JDofMJD0;
    if (iTime1>=0) time += priGroup_p.parm(iTime1);
    Float baseline = priGroup_p.parm(iBsln); 
    uvw(0) = priGroup_p.parm(iU);
    uvw(1) = priGroup_p.parm(iV);
    uvw(2) = priGroup_p.parm(iW);
    time  *= C::day; 

    // make a guess at the integration time
    if (row<0) startTime = time;
    if (time > startTime) {
      interval=time-startTime;
      msc.interval().fillColumn(interval);
      msc.exposure().fillColumn(interval);
      startTime = DBL_MAX; // do this only once
    }

    Int array = Int(100.0*(baseline - Int(baseline)+0.001));
    Int ant1 = Int(baseline)/256; 
    nAnt_p = max(nAnt_p,ant1);
    Int ant2 = Int(baseline) - ant1*256; 
    nAnt_p = max(nAnt_p,ant2);
    ant1--; ant2--; // make 0-based
    
    // Convert U,V,W from units of seconds to meters
    uvw *= C::c;

    Int count = 0;

    for (Int ifno=0; ifno<max(1,nIF_p); ifno++) {
      // IFs go to separate rows in the MS
      ms_p.addRow(); 
      row++;
      for (Int chan=0; chan<nChan; chan++) {
 	for (Int pol=0; pol<nCorr; pol++) {
 	  const Float visReal = priGroup_p(count++);
 	  const Float visImag = priGroup_p(count++);
 	  const Float wt = priGroup_p(count++); 
	  const Int p = corrIndex_p[pol];
 	  if (wt < 0.0) {
	    weightSpec(p, chan) = -wt;
	    flag(p, chan) = True;
	  } else {
	    weightSpec(p, chan) = wt;
	    flag(p, chan) = False;
	  }
	  vis(p, chan) = Complex(visReal, visImag);
 	}
      }
      // fill in values for all the unused columns
      if (row==0) {
 	msc.exposure().put(row,interval);
 	msc.feed1().put(row,0);
 	msc.feed2().put(row,0);
 	msc.flagRow().put(row,False);
 	lastRowFlag=False;
 	msc.interval().put(row,interval);
 	msc.scanNumber().put(row,0);
 	msc.processorId().put(row,-1);
 	msc.observationId().put(row,0);
 	msc.stateId().put(row,-1);
 	Vector<Float> tmp(nCorr); tmp=1.0;
 	msc.sigma().put(row,tmp);
 	msc.weight().put(row,tmp);
 	lastWeight=1.0;
      }
      msc.data().put(row,vis);
      // single channel case: make weight and weightSpectrum identical.
      // multichannel case: weight should not be used.
      if (nChan==1) { 
 	const Vector<Float> weight(weightSpec.column(0).copy()); 
 	if (weight(0)!=lastWeight) {
 	  msc.weight().put(row,weight);
 	  lastWeight=weight(0);
 	}
      }
      msc.weightSpectrum().put(row,weightSpec); 
      msc.flag().put(row,flag);
      msc.flagCategory().put(row,flagCat);
      Bool rowFlag=allEQ(flag,True);
      if (rowFlag!=lastRowFlag) {
 	msc.flagRow().put(row,rowFlag);
 	lastRowFlag=rowFlag;
      }

      if (ant1!=lastAnt1) {
 	msc.antenna1().put(row,ant1);
 	lastAnt1=ant1;
      }
      if (array!=lastArray) {
 	msc.arrayId().put(row,array);
 	lastArray=array;
      }
      // Always put antenna2 since it is bound to the
      // aipsStMan and is assumed to change every
      // row
      msc.antenna2().put(row,ant2);
      if (time!=lastTime) {
 	msc.time().put(row,time);
 	msc.timeCentroid().put(row,time);
 	lastTime=time;
      }
      msc.uvw().put(row,uvw);
      
      // determine the spectralWindowId
      Int spW = ifno;
      if (iFreq>=0) {
 	spW = (Int)priGroup_p.parm(iFreq) - 1; // make 0-based
 	if (nIF_p>0) {
 	  spW *=nIF_p; 
 	  spW+=ifno;
 	}
      }
      if (spW!=lastSpW) {
 	msc.dataDescId().put(row,spW);
 	nSpW = max(nSpW, spW+1);
 	lastSpW=spW;
      }
    
      // store the sourceId 
      Int sourceId = 0;
      if (iSource>=0) {
 	// make 0-based
 	sourceId = (Int)priGroup_p.parm(iSource) - 1; 
      }
      if (sourceId!=lastSourceId) {
 	msc.fieldId().put(row,sourceId);
 	nField = max(nField, sourceId+1);
 	lastSourceId=sourceId;
      }
    }
    meter.update((group+1)*1.0);
  }
  // fill the receptorAngle with defaults, just in case there is no AN table
  receptorAngle_p=0;
  // set the Measure References
*/
}

// fill Observation table
void FITSIDItoMS1::fillObsTables() {
  const Regex trailing(" *$"); // trailing blanks
  const FitsKeyword* kwp;
  Vector<Double> times(2);
  
  if(firstMain) {
    ms_p.observation().addRow();
    String observer;
    observer = (kwp=kw(FITS::OBSERVER)) ? kwp->asString() : "";
    observer=observer.before(trailing);
    MSObservationColumns msObsCol(ms_p.observation());
    msObsCol.observer().put(0,observer);
    String telescope= (kwp=kw(FITS::TELESCOP)) ? kwp->asString() : "unknown";
    telescope=telescope.before(trailing);  
    msObsCol.telescopeName().put(0,telescope);
    msObsCol.scheduleType().put(0, "");
    msObsCol.project().put(0, "");
   
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

//void FITSIDItoMS1::fillAntennaTable(BinaryTable& bt)
void FITSIDItoMS1::fillAntennaTable()
{
  //itsLog << LogOrigin("MSFitsInput()", "fillAntennaTable");
  const Regex trailing(" *$"); // trailing blanks
  TableRecord btKeywords=getKeywords();
  
 /**
  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* kw;
  String kwname;
  kwl.first();
  while ((kw = kwl.next())) 
    {
      kwname = kw->name(); 
      if (kwname == "NO_BAND") {
        Int val = kw->asInt();
        cout << kwname << "=" << val << endl;
      }
      cout << "test="<< kwname<<endl;
        
    }
 **/
  
  
  //cout << "nAnt_p=" << nAnt_p << endl;
  //cout << "nrows=" << nrows() << endl;

  /** 
  if (nAnt_p>nrows()) {
    cout << "Not all antennas found in antenna table:"
       << " expected " << nAnt_p << ", found " << nrows()
       << LogIO::EXCEPTION;
  }
  **/
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

   // itsLog << LogIO::NORMAL << "number of antennas ="<<nAnt<<LogIO::POST;
   // itsLog << LogIO::NORMAL << "array ref pos:"<<arrayXYZ<<LogIO::POST;

   // Since we cannot write these quantities, we cannot rely upon
   // their presence in any UVFITS file that we read:
   Double rdate=0.0;
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

   //cout << "srdate=" << srdate <<endl;
   //cout << "gst="<< gst << endl;

   MVTime timeVal;
   MEpoch::Types epochRef;
   FITSDateUtil::fromFITS(timeVal,epochRef,srdate,timsys);
   // convert to canonical form
   timsys=MEpoch::showType(epochRef);
   rdate=timeVal.second(); // MJD seconds
   String arrnam="Unknown";
   if (btKeywords.isDefined("ARRNAM")) {
     arrnam=btKeywords.asString("ARRNAM"); 
   }

   // store the time keywords 
   ms_p.antenna().rwKeywordSet().define(String("RDATE"),rdate);
   ms_p.antenna().rwKeywordSet().define(String("GSTIA0"),gst);
   ms_p.antenna().rwKeywordSet().define(String("DEGPDY"),degpdy);
   ms_p.antenna().rwKeywordSet().define(String("TIMSYS"),timsys);

   //save value to set time reference frame elsewhere
   timsys_p=timsys;
   // Fill in some likely values
   //Float diameter=25;
   Float diameter=6; // defalut (For SMA)  
   if (array_p=="ATCA") diameter=22;
   if (array_p=="VLBA") diameter=25;

   //Table anTab=fullTable("",Table::Scratch);
   Table anTab=oldfullTable("");

   // This is for UVFITS
   ///   MSAntennaColumns& ant(msc_p->antenna());
   //ROScalarColumn<String> name(anTab,"ANNAME");
   //ROScalarColumn<Int> id(anTab,"NOSTA");
   //ROScalarColumn<Int> mountType(anTab,"MNTSTA");
   //ROScalarColumn<Float> offset(anTab,"STAXOF");  
   //ROScalarColumn<Float> polangleA(anTab,"POLAA");
   //ROScalarColumn<Float> polangleB(anTab,"POLAB"); 
   //ROArrayColumn<Double> antXYZ(anTab,"STABXYZ");

   // This is for FITS-IDI (TT)
   MSAntennaColumns& ant(msc_p->antenna());
   ROScalarColumn<String> name(anTab,"ANNAME");
   ROArrayColumn<Double> antXYZ(anTab,"STABXYZ");
   ROArrayColumn<Float> dantXYZ(anTab,"DERXYZ");
   // following is for space-born telescope
   //ROScalarColumn<Int> orbp(anTab,"ORBPARM");
   ROScalarColumn<Int> stid(anTab,"NOSTA");
   ROScalarColumn<Int> mntid(anTab,"MNTSTA");
   ROArrayColumn<Float> offset(anTab,"STAXOF");

   // All "VLBI" (==arrayXYZ<1000) requires y-axis reflection:
   //  (ATCA looks like "VLBI" in UVFITS, but is already correct)
   //Bool doVLBIRefl= ((array_p!="ATCA") && allLE(abs(arrayXYZ),1000.0));     

 // add antenna info to table (TT)
   ant.setPositionRef(MPosition::ITRF);
   Int row=ms_p.antenna().nrow()-1;
   for (Int i=0; i<nAnt; i++) {
     ms_p.antenna().addRow(); row++;
     ant.dishDiameter().put(row,diameter);
     String mount;
     switch (mntid(i)) {
     case 0: mount="ALT-AZ"; break;
     case 1: mount="EQUATORIAL"; break;
     case 2: mount="X-Y"; break;
     case 3: mount="ORBITING"; break;
     case 4: mount="BIZARRE"; break;
     default: mount="UNKNOWN"; break;
     }
     ant.flagRow().put(row,False);
     ant.mount().put(row,mount);
     ant.name().put(row,String::toString(stid(i)));
     //Vector<Double> offsets(3); offsets=0.; offsets(0)=offset(i);
     //ant.offset().put(row,offset);
     ant.station().put(row,name(i));
     ant.type().put(row,"GROUND-BASED");

     // Do UVFITS-dependent position corrections:
     // ROArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
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

/******** from MSFitsInput class 
   // Prepare handling of UVFITS Antenna position coord conventions:
   // VLA requires rotation of local coords:
   Bool doVLARot=(array_p=="VLA");
   // initialize rotation matrix with zero rotation
   Matrix<Double> posRot=Rot3D(0,0.0);  
   if ( doVLARot ) {
     // Array position for VLA from aips may be wrong, so use
     //  authoritative position from measures (station positions
     //  are from on-line system and are relative to this)
     MPosition vlaCentre;
     AlwaysAssert(MeasTable::Observatory(vlaCentre, "VLA"), AipsError);
     arrayXYZ = vlaCentre.getValue().getValue();
     // Form rotation around Z axis by VLA longitude=atan(arrayY/arrayX)
     Double vlaLong=atan2(arrayXYZ(1),arrayXYZ(0));
     posRot=Rot3D(2,vlaLong);  // Applied to each ant position below
   }
   // All "VLBI" (==arrayXYZ<1000) requires y-axis reflection: 
   //  (ATCA looks like "VLBI" in UVFITS, but is already correct)
   Bool doVLBIRefl= ((array_p!="ATCA") && allLE(abs(arrayXYZ),1000.0));

   // add antenna info to table
   ant.setPositionRef(MPosition::ITRF);
   Int row=ms_p.antenna().nrow()-1;
   for (Int i=0; i<nAnt; i++) {
     ms_p.antenna().addRow(); row++;
     ant.dishDiameter().put(row,diameter); 
     String mount;
     switch (mountType(i)) {
     case 0: mount="ALT-AZ"; break;
     case 1: mount="EQUATORIAL"; break;
     case 2: mount="X-Y"; break;
     case 3: mount="ORBITING"; break;
     case 4: mount="BIZARRE"; break;
     default: mount="UNKNOWN"; break;
     }
     ant.flagRow().put(row,False);
     ant.mount().put(row,mount);
     ant.name().put(row,String::toString(id(i)));
     Vector<Double> offsets(3); offsets=0.; offsets(0)=offset(i);
     ant.offset().put(row,offsets);
     ant.station().put(row,name(i));
     ant.type().put(row,"GROUND-BASED");

     // Do UVFITS-dependent position corrections:
     // ROArrayColumn antXYZ(i) may need coord transform; do it in corXYZ:
     Vector<Double> corXYZ=antXYZ(i);
     // If nec, rotate coordinates out of local VLA frame to ITRF
     if ( doVLARot ) corXYZ=product(posRot,corXYZ);

     // If nec, reflect y-coord to yield right-handed geocentric:
     if ( doVLBIRefl ) corXYZ(1)=-corXYZ(1);

     ant.position().put(row,arrayXYZ+corXYZ);
     // store the angle for use in the feed table
     receptorAngle_p(2*i+0)=polangleA(i)*C::degree;
     receptorAngle_p(2*i+1)=polangleB(i)*C::degree;
   }

   // store these items in non-standard keywords for now
   ant.name().rwKeywordSet().define("ARRAY_NAME",arrnam);
   ant.position().rwKeywordSet().define("ARRAY_POSITION",arrayXYZ);
*******/
}
void FITSIDItoMS1::fillFeedTable() {
  MSFeedColumns& msfc(msc_p->feed());

  // find out the POLARIZATION_TYPE
  // In the fits files we handle there can be only a single, uniform type
  // of polarization so the following should work.
/**
  MSPolarizationColumns& msPolC(msc_p->polarization());
  Int numCorr=msPolC.numCorr()(0);
  Vector<String> rec_type(2); rec_type="";
  if (corrType_p(0)>=Stokes::RR && corrType_p(numCorr-1)<=Stokes::LL) {
    rec_type(0)="R"; rec_type(1)="L";
  }
  if (corrType_p(0)>=Stokes::XX && corrType_p(numCorr-1)<=Stokes::YY) {
    rec_type(0)="X"; rec_type(1)="Y";
  }
**/
  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* kw;
  String kwname;
  kwl.first();
  Int noSTKD = 1;
  Int firstSTK = 0;
  Int nIF = 1;
  while ((kw = kwl.next()))
    {
      kwname = kw->name();
      if (kwname == "NO_STKD") {
        noSTKD = kw->asInt();
        cout << kwname << "=" << noSTKD << endl;
      }
      if (kwname == "STK_1") {
        firstSTK = kw->asInt();
        cout << kwname << "=" << firstSTK << endl;
      }
      if (kwname == "NO_BAND") {
        nIF = kw->asInt();
        cout << kwname << "=" << nIF << endl;
      }

    }

  //access fitsidi AN table
  Table anTab = oldfullTable("");
  ROScalarColumn<Double> time(anTab, "TIME");
  ROScalarColumn<Float> timeint(anTab, "TIME_INTERVAL");
  ROScalarColumn<String> name(anTab, "ANNAME");
  ROScalarColumn<Int> anNo(anTab, "ANTENNA_NO");
  ROScalarColumn<Int> array(anTab, "ARRAY");
  ROScalarColumn<Int> fqid(anTab, "FREQID");
  ROScalarColumn<Int> digLev(anTab, "NO_LEVELS");
  ROScalarColumn<String> poltya(anTab, "POLTYA");
  ROArrayColumn<Float> polaa(anTab, "POLAA");
  ROArrayColumn<Float> polcala(anTab, "POLCALA");
  ROScalarColumn<String> poltyb(anTab, "POLTYB");
  ROArrayColumn<Float> polab(anTab, "POLAB");
  ROArrayColumn<Float> polcalb(anTab, "POLCALB");

  Matrix<Complex> polResponse(2,2); 
  polResponse=0.; polResponse(0,0)=polResponse(1,1)=1.;
  Matrix<Double> offset(2,2); offset=0.;
  Int nAnt = anTab.nrow();
  //cout <<"nAnt="<<nAnt<<", nIF="<< nIF;
  Matrix<Float> polanga(nIF,nAnt); 
  Matrix<Float> polangb(nIF,nAnt); 
  Vector<Double> position(3); position=0.;
  Vector<String> polType(2);
  polaa.getColumn(polanga); 
  polab.getColumn(polangb); 
  //Double testval=1.0;
  receptorAngle_p = 0;
  receptorAngle_p.resize(2*nAnt*nIF);
  for (Int i=0; i<nAnt; i++) {
    for (Int j=0; j<nIF; j++) {

    //cout << "testval is used"<< endl;
    //receptorAngle_p(2*i+0)=testval*C::degree;
    //receptorAngle_p(2*i+1)=testval*C::degree;

      Int k = (i*nIF + j);
      receptorAngle_p(2*k+0)=static_cast<Double>(polanga(j,i))*C::degree;
      receptorAngle_p(2*k+1)=static_cast<Double>(polangb(j,i))*C::degree;
    }
  }

  // fill the feed table
  Int outRow=-1;
  for (Int inRow=0; inRow<nAnt; inRow++) {
    for (Int inIF=0; inIF<nIF; inIF++) { 
      Int k = inRow*nIF + inIF;
      ms_p.feed().addRow(); outRow++;
      msfc.antennaId().put(outRow,anNo(inRow)-1);
      msfc.beamId().put(outRow,-1);
      msfc.feedId().put(outRow,0);
      msfc.interval().put(outRow,timeint(inRow)*C::day);
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

/**
  MSFeedColumns& msfc(msc_p->feed());

  // find out the POLARIZATION_TYPE
  // In the fits files we handle there can be only a single, uniform type
  // of polarization so the following should work.
  MSPolarizationColumns& msPolC(msc_p->polarization());
  Int numCorr=msPolC.numCorr()(0);
  Vector<String> rec_type(2); rec_type="";
  if (corrType_p(0)>=Stokes::RR && corrType_p(numCorr-1)<=Stokes::LL) {
    rec_type(0)="R"; rec_type(1)="L";
  }
  if (corrType_p(0)>=Stokes::XX && corrType_p(numCorr-1)<=Stokes::YY) {
    rec_type(0)="X"; rec_type(1)="Y";
  }

  Matrix<Complex> polResponse(2,2); 
  polResponse=0.; polResponse(0,0)=polResponse(1,1)=1.;
  Matrix<Double> offset(2,2); offset=0.;
  Vector<Double> position(3); position=0.;

  // fill the feed table
  Int row=-1;
  for (Int ant=0; ant<nAnt_p; ant++) {
    ms_p.feed().addRow(); row++;
    msfc.antennaId().put(row,ant);
    msfc.beamId().put(row,-1);
    msfc.feedId().put(row,0);
    msfc.interval().put(row,DBL_MAX);
    //    msfc.phasedFeedId().put(row,-1);
    msfc.spectralWindowId().put(row,-1); // all
    //msfc.time().put(row,0.);
    msfc.time().put(row, time(ant)*C::day);
    msfc.numReceptors().put(row,2);
    msfc.beamOffset().put(row,offset);
    msfc.polarizationType().put(row,rec_type);
    msfc.polResponse().put(row,polResponse);
    msfc.position().put(row,position);
    //msfc.receptorAngle().put(row,receptorAngle_p(Slice(2*ant,2)));
  }
**/
}

// method for filling Spectral window table.
// copied from MSFitsInput
//void MSFitsInput::fillSpectralWindowTable(BinaryTable& bt, Int nSpW)
//void FITSIDItoMS1::fillSpectralWindowTable(Int nSpW)
void FITSIDItoMS1::fillSpectralWindowTable()
{
  // itsLog << LogOrigin("MSFitsInput()", "fillSpectralWindowTable");
  MSSpWindowColumns& msSpW(msc_p->spectralWindow());
  MSDataDescColumns& msDD(msc_p->dataDescription());
  MSPolarizationColumns& msPol(msc_p->polarization());

  ConstFitsKeywordList& kwl = kwlist();
  const FitsKeyword* kw;
  String kwname;
  Int nCorr = 1;
  Int firstSTK = 0;
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
      if (kwname == "STK_1") {
        firstSTK = kw->asInt();
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
  ROScalarColumn<Int> colFqid(fqTab,"FREQID");
  Matrix<Double> ifFreq(nIF_p,nRow);
  Matrix<Float> chWidth(nIF_p,nRow);
  Matrix<Float> totalBandwidth(nIF_p,nRow);
  Matrix<Int> sideband(nIF_p,nRow);
  cout << "nIF_p=" << nIF_p << endl;
  cout << "nRow=" << nRow << endl;


  Int nSpW = nIF_p;

  // The type of the column changes according to the number of entries
  if (nIF_p==1) {

    ROScalarColumn<Double> colIFFreq(fqTab, "BANDFREQ");
    ROScalarColumn<Float> colChWidth(fqTab, "CH_WIDTH"); 
    ROScalarColumn<Float> colTotalBW(fqTab,"TOTAL_BANDWIDTH");
    ROScalarColumn<Int> colSideBand(fqTab, "SIDEBAND");
    //ROScalarColumn<Int> BaseBandCH(fqTab, "BB_CHAN"); 

    for (Int i=0; i<nRow; i++) {
      ifFreq(0,i)=colIFFreq(i);
      chWidth(0,i)=colChWidth(i);
      totalBandwidth(0,i)=colTotalBW(i);
      sideband(0,i) = colSideBand(i);
    }
  } else {
    ROArrayColumn<Double> colIFFreq(fqTab, "BANDFREQ");
    ROArrayColumn<Float> colChWidth(fqTab, "CH_WIDTH");
    ROArrayColumn<Float> colTotalBW(fqTab, "TOTAL_BANDWIDTH");
    ROArrayColumn<Int> colSideBand(fqTab, "SIDEBAND");
    ROArrayColumn<Int> BaseBandCH(fqTab, "BB_CHAN"); 

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
      itsLog << LogIO::SEVERE << "Trouble interpreting FQ table, id's may be wrong" << LogIO::POST;
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
//void MSFitsInput::fillFieldTable(BinaryTable& bt, Int nField)
//void FITSIDItoMS1::fillFieldTable(Int nField)
void FITSIDItoMS1::fillFieldTable()
{
  // itsLog << LogOrigin("MSFitsInput()", "fillFieldTable");
  MSFieldColumns& msField(msc_p->field());
  //Table suTab=bt.fullTable("",Table::Scratch);
  Table suTab=oldfullTable("");

  //access the columns in source FITS-IDI subtable
  ROScalarColumn<Int> id(suTab,"ID_NO.");
  ROScalarColumn<String> name(suTab,"SOURCE");
  ROScalarColumn<Int> qual(suTab,"QUAL");
  ROScalarColumn<String> code(suTab,"CALCODE");
  ROScalarColumn<Int> fqid(suTab,"FREQID");
  ROArrayColumn<Float> iflux(suTab,"IFLUX"); // I (Jy)
  ROArrayColumn<Float> qflux(suTab,"QFLUX"); // Q 
  ROArrayColumn<Float> uflux(suTab,"UFLUX"); // U 
  ROArrayColumn<Float> vflux(suTab,"VFLUX"); // V 
  ROArrayColumn<Float> alpha(suTab,"ALPHA"); // sp. index  
  ROArrayColumn<Double> foffset(suTab,"FREQOFF"); // fq. offset  
  ROScalarColumn<Double> ra(suTab,"RAEPO");    //degrees
  ROScalarColumn<Double> dec(suTab,"DECEPO");  //degrees
  ROScalarColumn<Double> epoch(suTab,"EPOCH"); //years
  ROScalarColumn<Double> raapp(suTab,"RAAPP");    //degrees
  ROScalarColumn<Double> decapp(suTab,"DECAPP");  //degrees
  ROArrayColumn<Double> sysvel(suTab,"SYSVEL"); // sys vel. (m/s)  
  ROScalarColumn<String> veltype(suTab,"VELTYP"); //   
  ROScalarColumn<String> veldef(suTab,"VELDEF"); //   
  ROScalarColumn<Double> pmra(suTab,"PMRA");   //deg/day
  ROScalarColumn<Double> pmdec(suTab,"PMDEC"); //deg/day
  ROScalarColumn<Float> pllx(suTab,"PARALLAX"); //arcsec 

  //if (Int(suTab.nrow())<nField) {
  //  itsLog << LogIO::NORMAL
  //     << "Input Source id's not sequential, adding empty rows in output"
  //     << LogIO::POST;
  //}
  Int outRow=-1;
  //cout << "epoch(fillFieldTable) = " << epoch(0) << endl;
  // set the DIRECTION MEASURE REFERENCE for appropriate columns
  MDirection::Types epochRefZero=MDirection::J2000;
  if (nearAbs(epoch(0),1950.0,0.01)) {
    epochRefZero=MDirection::B1950;
  }
  msc_p->setDirectionRef(epochRefZero);
  for (Int inRow=0; inRow<(Int)suTab.nrow(); inRow++) {
    if (id(inRow) < 1) {
      itsLog << LogIO::WARN
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
        itsLog << LogIO::WARN << " unreasonably large proper motion parameter(s)" 
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
      if (near(epoch(inRow),2000.0,0.01)) {
        epochRef = MDirection::J2000;
      } else if (numPoly == 0 && nearAbs(epoch(inRow),1950.0,0.01)) {
        epochRef = MDirection::B1950;
      } else {
        itsLog << " Cannot handle epoch in SU table: "
               << epoch(fld) << LogIO::EXCEPTION;
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

    // ** following will be filled later (once Observation Table is filled) 
    // at this moment just put 0.0.
    // For FITS-IDI, this ref time equals to RDATE in sec?
    // 
    // Get the time from the observation subtable. I have assumed that this bit
    // of the observation table has been filled by now.
    //const Vector<Double> obsTimes = msc_p->observation().timeRange()(0);
 
    msField.time().put(fld, 0.0);
    msField.numPoly().put(fld,numPoly);
    msField.delayDirMeasCol().put(fld,radecMeas);
    msField.phaseDirMeasCol().put(fld,radecMeas);
    msField.referenceDirMeasCol().put(fld,radecMeas);
    msField.flagRow().put(fld,False);
  }
}

void FITSIDItoMS1::fixEpochReferences() {
  //itsLog << LogOrigin("MSFitsInput()", "fixEpochReferences");
  if (timsys_p=="IAT") timsys_p="TAI";
  if (timsys_p=="UTC" || timsys_p=="TAI") {
    if (timsys_p=="UTC") msc_p->setEpochRef(MEpoch::UTC, False);
    if (timsys_p=="TAI") msc_p->setEpochRef(MEpoch::TAI, False);
  } else {
    if (timsys_p!="")
      cout << LogIO::SEVERE << "Unhandled time reference frame: "<<timsys_p<<LogIO::POST;
    //itsLog << LogIO::SEVERE << "Unhandled time reference frame: "<<timsys_p<<LogIO::POST;
  }
}

//void FITSIDItoMS1::updateTables(const String& MSFileName)
void FITSIDItoMS1::updateTables(const String& MStmpDir)
//void FITSIDItoMS1::updateTables(const String& MSMain, const String& MStmpDir)
{
  const Vector<Double> obsTime = msc_p->observation().timeRange()(0);
  cout << "check PT 1"<<endl; 
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
  //delete msc_p;

  //update time in the field table
  MSFileName = MStmpDir + "/SOURCE";
  //MSMainSubFileName = MSMain + "/OBSERVATION";
  MeasurementSet mssub2(MSFileName,Table::Update);
  //MeasurementSet msobs(MSMainSubFileName,Table::Update);
  ms_p = mssub2;
  msc_p = new MSColumns(ms_p);
  Int nrow = ms_p.field().nrow();
  
  
  MSFieldColumns& msFld(msc_p->field());
  
  for (Int row = 0; row < nrow; row++) { 
    msFld.time().put(row,obsTime(0)); 
    //cout << "update:obsTime=" << obsTime(0);
  }
    
  //delete msc_p;

} 

void FITSIDItoMS1::readFitsFile(const String& msFile)
{

  //itsLog << LogOrigin("MSFitsInput", "readFitsFile");
  Int nField=0, nSpW=0;
  
  String tmpPolTab;

  const Regex trailing(" *$"); // trailing blanks 
  String extname(FITSIDItoMS1::extname());
  extname=extname.before(trailing);
  
  /*
  if(infile_p.hdutype() == "NotAHDU")
    {
      cout << "reading special record" << endl;
      read_sp();
    }
  

  else
    {
*/
      cout << "rectype=" << infile_p.rectype() << endl;
      cout << "Found binary table of type " << extname << endl;
      
      //if(extname=="UV_DATA" && firstMain)
        if(extname=="UV_DATA") 
	{ 
	  	  
          String tmpdir = msFile + "_tmp";
	  getAxisInfo();

	  if(firstMain)
	    {
	      Bool useTSM=True;
              Bool mainTbl=True;

	      setupMeasurementSet(msFile, useTSM, mainTbl);
              //createMainTable(msFile);

	      fillMSMainTable(msFile, nField, nSpW);
	      fillObsTables();

	 
	      fixEpochReferences();
	              
              cout << "updating subtables ..."<< endl;
              updateTables(tmpdir); 

	      firstMain=False;
	    }
          else
            {
	  
	      fillMSMainTable(msFile, nField, nSpW);
	      fillObsTables();
	     //skip();
          }
	}

      else
	{
	  Bool useTSM=False;
          Bool mainTbl=False;
	  setupMeasurementSet(msFile, useTSM, mainTbl);
	   
	  if(extname=="ARRAY_GEOMETRY") fillAntennaTable();
          else if (extname=="SOURCE") fillFieldTable();
          else if (extname=="FREQUENCY") fillSpectralWindowTable();
          else if (extname=="ANTENNA") fillFeedTable();

	  /*
	    else if(extname=="")  ;
	    else oldfullTable("")  ;
	  */
   
	  /*
	    //handle empty extensions
	    if(fitsdatasize()) skip();     
	  */
             
	  //fullTable("", Table::Scratch);
	  oldfullTable("");

	  /*
	    else if (type.contains("") && !haveSpW) 
	    {
	    haveSpW=True;
	    fillSpectralWindowTable(binTab, nSpW);
	    } 

	    else if (type.contains("SU") && !haveField) 
	    {
	    haveField=True;
	    fillFieldTable(binTab, nField);
	    } 
	  */

	  /*      
	    if (!haveSpW) 
	    {
	    // single freq. case
	    fillSpectralWindowTable();
	    }

	    if (!haveField) 
	    {
	    // single source case
	    fillFieldTable(nField);
	    }
	  
	    fixEpochReferences();
	    
	    if (!haveAn) 
	    {
	    itsLog << "Cannot find an AN Table. This is required." << LogIO::EXCEPTION;
	    }

	    fillFeedTable();
	  */        
	}

/*
  itsLog << LogOrigin("MSFitsInput", "readFitsFile");
  Int nField=0, nSpW=0;

  getPrimaryGroupAxisInfo();

  Bool useTSM=True;
  setupMeasurementSet(msFile_p, useTSM);
          
  // fill the OBSERVATION table
  fillObsTables();
          
  // fill the main table
  fillMSMainTable(nField, nSpW);

  // now handle the BinaryTable extensions for the subtables
  Bool haveAn=False, haveField=False, haveSpW=False;

  while (infile_p->rectype() != FITS::EndOfFile && !infile_p->err()) {
    if (infile_p->hdutype() != FITS::BinaryTableHDU) {
      itsLog << LogIO::NORMAL << "Skipping unhandled extension" << LogIO::POST;
      infile_p->skip_hdu();
    } else {
      BinaryTable binTab(*infile_p);
      // see if we can recognize the type
      String type=binTab.extname();
      itsLog << LogIO::NORMAL << "Found binary table of type " << type 
	 << " following data" << LogIO::POST;
      //itsLog << binTab <<LogIO::POST;
      if (type.contains("AN") && !haveAn) {
	haveAn=True;
	fillAntennaTable(binTab);
      } else if (type.contains("FQ") && !haveSpW) {
	haveSpW=True;
	fillSpectralWindowTable(binTab, nSpW);
      } else if (type.contains("SU") && !haveField) {
	haveField=True;
	fillFieldTable(binTab, nField);
      } else {
	itsLog << LogIO::NORMAL 
	   << "Skipping table, duplicate or unrecognized type: "
	   << type << LogIO::POST;
	binTab.fullTable("", Table::Scratch); // infile.skip_hdu();
      }
    }
  }
  if (!haveSpW) {
    // single freq. case
    fillSpectralWindowTable();
  }

  if (!haveField) {
    // single source case
    fillFieldTable(nField);
  }
  fixEpochReferences();

  if (!haveAn) {
    itsLog << "Cannot find an AN Table. This is required." << LogIO::EXCEPTION;
  }
  fillFeedTable();
*/

} 







} //# NAMESPACE CASA - END

