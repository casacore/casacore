//# Bintable.cc:  this defines BinaryTable, which converts FITS binary tables to Casacore Tables
//# Copyright (C) 1994-1999,2000,2001,2003
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

//# Includes
#include <casacore/fits/FITS/BinTable.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/MemoryStMan.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/RowCopier.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/stdio.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Bool isSDFitsColumn(FITS::ReservedName name) {
    if (name == FITS::AUTHOR || name == FITS::CDELT || name == FITS::CROTA ||
	name == FITS::CRPIX || name == FITS::CRVAL || name == FITS::CTYPE ||
	name == FITS::DATE || name == FITS::DATE_OBS || name == FITS::EPOCH ||
	name == FITS::EQUINOX || name == FITS::INSTRUME || 
	name == FITS::OBJECT || name == FITS::OBSERVER ||
	name == FITS::ORIGIN || name == FITS::TELESCOP) 
	return True;
    else
	return False;
}
	
	
//   The constructor

BinaryTable::BinaryTable(FitsInput& fitsin, FITSErrorHandler errhandler, 
			 Bool useIncrSM, Bool sdfits) :
    BinaryTableExtension(fitsin, errhandler), currRowTab(0), nelem(0), 
    colNames(0), vatypes_p(0), vaptr_p(0), va_p(0), theheap_p(0)
{
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
	// and do some initial decoding of the VADesc related stuff
	uInt ncol = ncols();
	vatypes_p = new FITS::ValueType [ncol];
	AlwaysAssert(vatypes_p, AipsError);
	vaptr_p = new void * [ncol];
	AlwaysAssert(vaptr_p, AipsError);
	va_p = new VADescFitsField [ncol];
	AlwaysAssert(va_p, AipsError);
	for (uInt i=0;i<ncol;++i) {
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

//		this table descriptor is not kept, it is used 
//		during construction only
   TableDesc td;
//		build the keywords of the currRowTab
//		loop through the FITS kw list
   ConstFitsKeywordList &kwl = kwlist();
   kwl.first();
   const FitsKeyword *kw;
   String kwname;
   // will hold the index portion for indexed keywords, this should be
   // more than enough space
   char index[8];
   while ((kw = kwl.next())) {
       if (!kw->isreserved() || (sdfits && isSDFitsColumn(kw->kw().name()))) {
	   // Get the kw name and remove the trailing spaces
           kwname = kw->name();
	   kwname.rtrim(' ');
	   // if it is indexed, add the index to the keyword
	   if (kw->isindexed()) {
	       sprintf(index,"%i",kw->index());
	       kwname = kwname + String(index);
	   }
	   //		first check if this keyword exists in kwSet
	   if (kwSet.isDefined(kwname)) {
	       //		Issue a warning and remove the old one
	       cout << "Duplicate keyword name : " << kwname
		   << " most recent occurrance takes precedence" << endl;
	       kwSet.removeField(kwname);
	   }
	   switch (kw->type()) {
	       //		put NOVALUE fields in as string keywords with an emtpy string
	   case FITS::NOVALUE: kwSet.define(kwname,"");
	       break;
	   case FITS::LOGICAL: kwSet.define(kwname, kw->asBool()); 
	       break;
	   case FITS::CHAR: kwSet.define(kwname, kw->asString());
	       break;
	   case FITS::STRING: kwSet.define(kwname, kw->asString());
	       break;
	   case FITS::LONG: kwSet.define(kwname, kw->asInt());
	       break;
	   case FITS::FLOAT: kwSet.define(kwname, kw->asFloat());
	       break;
	   case FITS::DOUBLE: kwSet.define(kwname, kw->asDouble());
	       break;
	   case FITS::COMPLEX: kwSet.define(kwname, kw->asComplex());
	       break;
			       //		the above types are all that should be present as keywords
	   default:
	       cerr << "Error: unrecognized table data type for keyword "
		    << kwname << " type = " << kw->type() << endl;
	       cerr << "That should not have happened" << endl;
	       continue;
	   }		// end of switch on kw->type()
	   // add any comment in
	   kwSet.setComment(kwname, kw->comm());
       }	  	// end of if(!kw->isreserved())
   }			// end of loop over kw list

   //		get some things to remember
   Int nfield = (Int ) tfields();
   nelem = new Int[nfield];
   colNames = new SimpleOrderedMap<Int, String>("");

   AlwaysAssert(nelem, AipsError);
   //		loop over the number of fields in the FITS table
   for (Int i=0; i < nfield; i++) {
       nelem[i] = field(i).nelements();
       //		check if the column name exists
       String colname(ttype(i));
       //               remove trailing spaces
       colname.rtrim(' ');
       if (td.isColumn(colname)) {
	   //		issue a warning, append column number to this name
	   ostringstream newname;
	   newname << colname << "." << i;
	   //		str gives the space to cptr, which must be deleted
	   colname = newname.str();
	   cout << "Duplicate column name : " << ttype(i)
	       << " this occurance will be named " << colname << endl;
       } else if (td.keywordSet().isDefined(colname)) {
	   //          rename the offending keyword, 
	   //          the column name takes precedence!
	   String newname = colname + "-keyword";
// 	   cout << "Duplicate name (keyword & column) : " << ttype(i)
// 	       << " keyword will be renamed " << newname << endl;
	   td.rwKeywordSet().renameField(newname, colname);
       }
       //		enter the name in the colNames map
       colNames->define(i, colname);
       //		get a shorthand Bool for array versus scalar
       //               NOTE: VADESC are always assumed to be array columns
       //               but that fact is ignored by isArray - but thats ok,
       //               it is not used in that case.
       Bool isArray = (nelem[i] > 1 && field(i).fieldtype() != FITS::CHAR
			     && field(i).fieldtype() != FITS::STRING);
       //		switch on the type of column
       switch (field(i).fieldtype()) {
	   // BIT stored as LOGICAL
       case FITS::BIT:
       case FITS::LOGICAL:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Bool>(colname,"",
						  IPosition(1,nelem[i]),
						  ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Bool>(colname,""));
	   }
	   break;
	   //		BYTE stored as uChar
       case FITS::BYTE:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<uChar>(colname,"",
						   IPosition(1,nelem[i]),
						   ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<uChar>(colname,""));
	   }
	   break;
       case FITS::SHORT:
	   if (isArray) {
	       td.addColumn(ArrayColumnDesc<Short>(colname,"",
						   IPosition(1,nelem[i]),
						   ColumnDesc::Direct));
	   } else {
	       td.addColumn(ScalarColumnDesc<Short>(colname,""));
	   }
	   break;
       case FITS::LONG:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Int>(colname,"",
						 IPosition(1,nelem[i]),
						 ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Int>(colname,""));
	   }
	   break;
       case FITS::CHAR: 
       case FITS::STRING:
	   //		a CHAR and STRING type is always a string, never an array	
	   td.addColumn(ScalarColumnDesc<String>(colname,""));
	   break;
       case FITS::FLOAT:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Float>(colname,"",
						   IPosition(1,nelem[i]),
						   ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Float>(colname,""));
	   }
	   break;
       case FITS::DOUBLE:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Double>(colname,"",
						    IPosition(1,nelem[i]),
						    ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Double>(colname,""));
	   }
	   break;
       case FITS::COMPLEX:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Complex>(colname,"",
						     IPosition(1,nelem[i]),
						     ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Complex>(colname,""));
	   }
	   break;
	   //		ICOMPLEX is promoted to DCOMPLEX so no precision is lost
       case FITS::ICOMPLEX:
       case FITS::DCOMPLEX:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<DComplex>(colname,"",
						      IPosition(1,nelem[i]),
						      ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<DComplex>(colname,""));
	   }
	   break;
       case FITS::VADESC:
	   {
	       // there MUST be a heap at this point
	       AlwaysAssert(theheap_p, AipsError);
	       switch (vatypes_p[i]) {
	       case FITS::BIT:
	       case FITS::LOGICAL: 
		   td.addColumn(ArrayColumnDesc<Bool>(colname,"")); break;
	       case FITS::BYTE: 
		   td.addColumn(ArrayColumnDesc<uChar>(colname,"")); break;
	       // shorts are promoted to LONGs
	       case FITS::SHORT: 
	       case FITS::LONG:
		   td.addColumn(ArrayColumnDesc<Int>(colname,"")); break;
	       // an array of chars is just a scalar String
	       case FITS::CHAR: 
		   td.addColumn(ScalarColumnDesc<String>(colname,"")); break;
	       case FITS::FLOAT: 
		   td.addColumn(ArrayColumnDesc<Float>(colname,"")); break;
	       case FITS::DOUBLE:
		   td.addColumn(ArrayColumnDesc<Double>(colname,"")); break;
	       case FITS::COMPLEX: 
		   td.addColumn(ArrayColumnDesc<Complex>(colname,"")); break;
	       case FITS::DCOMPLEX:
		   td.addColumn(ArrayColumnDesc<DComplex>(colname,"")); break;
	       default:
		   cerr << "Error:: column " << i
			<< " has impossible type for variable array descriptor "
			<< vatypes_p[i] << endl;
		   break;
	       }
	   }
	   break;
	   //	NOVALUE should not happen in a table
       default:
	   cerr << "Error: column " << i
		<< " has untranslatable type " << field(i).fieldtype()
		<< " This should NEVER happen " << endl;
	   continue;
       }		// end of switch on FITS type
       //		set the comment string if appropriate
       if (kwl(FITS::TTYPE, i)) 
	   td.rwColumnDesc(colname).comment() = kwl(FITS::TTYPE,i)->comm();
       //		for 0 element fields, set ZEROELEM keyword, Bool=True
       if (nelem[i]==0) td.rwColumnDesc(colname).rwKeywordSet().define("ZEROELEM", True);
       //		attach associated information
       //		Units
       td.rwColumnDesc(colname).rwKeywordSet().define("TUNIT", tunit(i));
       //		display format
       td.rwColumnDesc(colname).rwKeywordSet().define("TDISP", tunit(i));
       //		TDIM array dimension convention, this should really be
       //		done above !!
       td.rwColumnDesc(colname).rwKeywordSet().define("TDIM", tdim(i));
       //		For integer types, add TNULL, gives the undefined value
       //		for integer types
       if (field(i).fieldtype() == FITS::SHORT ||
	   field(i).fieldtype() == FITS::LONG)
	   td.rwColumnDesc(colname).rwKeywordSet().define("TNULL", tnull(i));
   }	//		end of loop over columns

   // add the virtual columns if sdfits is true
   // all of these should be scalars
   if (sdfits) {
       // first, remove duplicates - true columns take precedence
       Vector<String> duplicates(kwSet.nfields());
       uInt count = 0;
       uInt field;
       for (field=0;field<kwSet.nfields();field++) {
	   if (td.isColumn(kwSet.name(field))) {
	       duplicates(count) = kwSet.name(field);
	       count++;
	   }
       }
       for (field=0;field<count;field++) {
	   kwSet.removeField(duplicates(field));
       }
       for (field=0;field<kwSet.nfields();field++) {	   
	   switch (kwSet.type(field)) {
	   case TpBool:
	       td.addColumn(ScalarColumnDesc<Bool>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpUChar:
	       td.addColumn(ScalarColumnDesc<uChar>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpShort:
	       td.addColumn(ScalarColumnDesc<Short>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpInt:
	       td.addColumn(ScalarColumnDesc<Int>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpUInt:
	       td.addColumn(ScalarColumnDesc<uInt>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpFloat:
	       td.addColumn(ScalarColumnDesc<Float>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpDouble:
	       td.addColumn(ScalarColumnDesc<Double>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpComplex:
	       td.addColumn(ScalarColumnDesc<Complex>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpDComplex:
	       td.addColumn(ScalarColumnDesc<Complex>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   case TpString:
	       td.addColumn(ScalarColumnDesc<String>(kwSet.name(field),kwSet.comment(field)));
	       break;
	   default:
	       throw(AipsError("Impossible virtual column type"));
	       break;
	   }
       }
   } else {
       // no virtual columns, put any keywords in td and
       // clean out kwSet
       td.rwKeywordSet().merge(kwSet,RecordInterface::RenameDuplicates);
       RecordDesc emptyDesc;
       kwSet.restructure(emptyDesc);
   }
   //		now, create currRowTable
   SetupNewTable newtab("", td, Table::Scratch);
   if (useIncrSM) {
       IncrementalStMan stman ("ISM");
       newtab.bindAll(stman);
   }

   //		and finally create the Table
   currRowTab = new Table(newtab, Table::Memory, 1);

   //		OK, fill the one row of CurrRowTab
   if (nrows() > 0) {
       // if we don't have a heap, read a row
       if (!theheap_p) read(1);
       fillRow();
   }
}

void BinaryTable::fillRow()
{
    //		loop over each field
    for (Int j=0;j<tfields(); j++) {
	//		and switch on the FITS type
	TableColumn tabcol(*currRowTab, (*colNames)(j));
	switch (field(j).fieldtype()) {
	case FITS::LOGICAL:
            {
		FitsField<FitsLogical> thisfield = 
		    *(FitsField<FitsLogical> *)&field(j);
		Vector<Bool> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		if (nelem[j] > 1) {
		    ArrayColumn<Bool> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::BIT:
            {
		FitsField<FitsBit> thisfield =
		    *(FitsField<FitsBit> *)&field(j);
		Vector<Bool> vec(nelem[j]);
		for (uInt k=0;k<field(j).nelements();k++) {
		    vec(k) = (int(thisfield(k)));
		}
		if (nelem[j] > 1) {
		    ArrayColumn<Bool> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::BYTE:
            {
		FitsField<unsigned char> thisfield = 
		    *(FitsField<unsigned char> *)&field(j);
		Vector<uChar> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		if (nelem[j] > 1) {
		    ArrayColumn<uChar> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::CHAR:
	case FITS::STRING:
            {
		FitsField<char> thisfield = 
		    *(FitsField<char> *)&field(j);
		// look for the true end of the string
		char * cptr = (char *)thisfield.data();
		uInt length = thisfield.nelements();
		while (length > 0 && 
		       (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
		  length--;
		}
		tabcol.putScalar(0,String(cptr, length));
            }
            break;
	case FITS::SHORT:
            {
		FitsField<short> thisfield = 
		    *(FitsField<short> *)&field(j);
		Vector<Short> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		//			any scaling should happen here
		if (nelem[j] > 1) {
		    ArrayColumn<Short> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::LONG:
            {
		FitsField<FitsLong> thisfield = 
		    *(FitsField<FitsLong> *)&field(j);
		Vector<Int> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = (Int )thisfield(k);
		}
		//			any scaling should happen here
		if (nelem[j] > 1) {
		    ArrayColumn<Int> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::FLOAT:
            {
		FitsField<float> thisfield = 
		    *(FitsField<float> *)&field(j);
		Vector<Float> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    Vector<Double> dvec(nelem[j]);
		    convertArray(dvec, vec);
		    dvec *= tscal(j); 
		    dvec += tzero(j);
		    convertArray(vec, dvec);
		} else if (tzero(j) != 0) {
		    vec += (Float )tzero(j);
		}
		if (nelem[j] > 1) {
		    ArrayColumn<Float> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::DOUBLE:
            {
		FitsField<double> thisfield = 
		    *(FitsField<double> *)&field(j);
		Vector<Double> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    vec *= tscal(j); 
		    vec += tzero(j);
		} else if (tzero(j) != 0) {
		    vec += (Double )tzero(j);
		}
		if (nelem[j] > 1) {
		    ArrayColumn<Double> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::COMPLEX:
            {
		FitsField<Complex> thisfield = 
		    *(FitsField<Complex> *)&field(j);
		Vector<Complex> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    vec *= Complex(tscal(j),0); 
		    vec += Complex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec += Complex(tzero(j),0);
		}
		if (nelem[j] > 1) {
		    ArrayColumn<Complex> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::DCOMPLEX:
            {
		FitsField<DComplex> thisfield = 
		    *(FitsField<DComplex> *)&field(j);
		Vector<DComplex> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = thisfield(k);
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    vec *= DComplex(tscal(j),0) ; 
		    vec += DComplex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec += DComplex(tzero(j));
		}
		if (nelem[j] > 1) {
		    ArrayColumn<DComplex> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::ICOMPLEX:
            {
		FitsField<IComplex> thisfield = 
		    *(FitsField<IComplex> *)&field(j);
		Vector<DComplex> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		  const IComplex& icm = thisfield(k);
		    vec(k) = DComplex (icm.real(), icm.imag());
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    vec *= DComplex(tscal(j),0); 
		    vec += DComplex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec += DComplex(tzero(j));
		}
		if (nelem[j] > 1) {
		    ArrayColumn<DComplex> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	case FITS::VADESC:
	    { 
		FitsVADesc thisva = va_p[j]();
		// its a pity so many copies seem to be necessary
		// one to copy the heap into the local version of the
		// desired type
		// the second to hold and scale the values in a Casacore type
		// and finally the copy actually placed in the table
		switch (vatypes_p[j]) {
		case FITS::LOGICAL:
		    {
			FitsLogical *vptr = (FitsLogical *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Bool> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			ArrayColumn<Bool> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::BIT:
		    {
			uChar *vptr = (uChar *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			// assumes 8 bits per uChar
			Int whichByte = -1;
			Vector<Bool> vec(thisva.num());
			uChar mask = 0200;
			for (Int k=0;k<thisva.num();k++) {
			    if (k%8 == 0) whichByte++;
			    vec(k) = (vptr[whichByte] & (mask >> k%8));
			}
			ArrayColumn<Bool> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::BYTE:
		    {
			uChar *vptr = (uChar *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<uChar> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			ArrayColumn<uChar> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::CHAR:
		    {
			Char *vptr = (Char *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			tabcol.putScalar(0,String(vptr, thisva.num()));
		    }
		    break;
		case FITS::SHORT:
		    {
			Short *vptr = (Short *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Int> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// any scaling should happen here
			ArrayColumn<Int> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::LONG:
		    {
			FitsLong *vptr = (FitsLong *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Int> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// any scaling should happen here
			ArrayColumn<Int> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::FLOAT:
		    {
			Float *vptr = (Float *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Float> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// scale as appropriate
			if (tscal(j) != 1) {
			    Vector<Double> dvec(thisva.num());
			    convertArray(dvec, vec);
			    dvec *= tscal(j);
			    dvec += tzero(j);
			    convertArray(vec, dvec);
			} else if (tzero(j) != 0) {
			    vec += (Float )tzero(j);
			}
			ArrayColumn<Float> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::DOUBLE:
		    {
			Double *vptr = (Double *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Double> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// scale as appropriate
			if (tscal(j) != 1) {
			    vec *= tscal(j);
			    vec += tzero(j);
			} else if (tzero(j) != 0) {
			    vec += (Double )tzero(j);
			}
			ArrayColumn<Double> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::COMPLEX:
		    {
			Complex *vptr = (Complex *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<Complex> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// scale as appropriate
			if (tscal(j) != 1) {
			    vec *= Complex(tscal(j),0);
			    vec += Complex(tzero(j),0);
			} else if (tzero(j) != 0) {
			    vec += Complex(tzero(j),0);
			}
			ArrayColumn<Complex> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		case FITS::DCOMPLEX:
		    {
			DComplex *vptr = (DComplex *)(vaptr_p[j]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			Vector<DComplex> vec(thisva.num());
			for (Int k=0;k<thisva.num();k++) {
			    vec(k) = vptr[k];
			}
			// scale as appropriate
			if (tscal(j) != 1) {
			    vec *= DComplex(tscal(j),0);
			    vec += DComplex(tzero(j),0);
			} else if (tzero(j) != 0) {
			    vec += DComplex(tzero(j),0);
			}
			ArrayColumn<DComplex> arrcol(tabcol);
			arrcol.put(0,vec);
		    }
		    break;
		default:
		    cerr << "Error unrecognized variable array type for field "
			 << j << " type : " << vatypes_p[j] << endl;
		    break;
		}
	    }
	    break;
	default:
	    // NOVALUE (which shouldn't occur here
            cerr << "Error: unrecognized table data type for field "
		<< j << endl;
            cerr << "That should not have happened" << endl;
            continue;
	}			// end of loop over switch
    }			// end of loop over fields

    // loop over all virtual columns if necessary
    if (kwSet.nfields() > 0) {
	for (uInt field=0;field<kwSet.nfields();field++) {
	    TableColumn tabcol(*currRowTab, kwSet.name(field));
	    switch (kwSet.type(field)) {
	    case TpBool:
		tabcol.putScalar(0,kwSet.asBool(field));
		break;
	    case TpUChar:
		tabcol.putScalar(0,kwSet.asuChar(field));
		break;
	    case TpShort:
		tabcol.putScalar(0,kwSet.asShort(field));
		break;
	    case TpInt:
		tabcol.putScalar(0,kwSet.asInt(field));
		break;
	    case TpUInt:
		tabcol.putScalar(0,kwSet.asuInt(field));
		break;
	    case TpFloat:
		tabcol.putScalar(0,kwSet.asfloat(field));
		break;
	    case TpDouble:
		tabcol.putScalar(0,kwSet.asdouble(field));
		break;
	    case TpComplex:
		tabcol.putScalar(0,kwSet.asComplex(field));
		break;
	    case TpDComplex:
		tabcol.putScalar(0,kwSet.asDComplex(field));
		break;
	    case TpString:
		tabcol.putScalar(0,kwSet.asString(field));
		break;
	    default:
		throw(AipsError("Impossible virtual column type"));
		break;
	    }
	}
    }
}

//	The destructor

BinaryTable::~BinaryTable()
{
    if (vaptr_p) {
	for (Int i=0;i<ncols();++i) {
	    if (vaptr_p[i]) {
		switch (vatypes_p[i]) {
		case FITS::LOGICAL: delete [] (FitsLogical *)vaptr_p[i]; break;
		case FITS::BIT: delete [] (uChar *)vaptr_p[i]; break;
		case FITS::BYTE: delete [] (uChar *)vaptr_p[i]; break;
		case FITS::SHORT: delete [] (Short *)vaptr_p[i]; break;
		case FITS::LONG: delete [] (FitsLong *)vaptr_p[i]; break;
		case FITS::CHAR: delete [] (Char *)vaptr_p[i]; break;
		case FITS::FLOAT: delete [] (Float *)vaptr_p[i]; break;
		case FITS::DOUBLE: delete [] (Double *)vaptr_p[i]; break;
		case FITS::COMPLEX: delete [] (Complex *)vaptr_p[i]; break;
		case FITS::DCOMPLEX: delete [] (DComplex *)vaptr_p[i]; break;
		  // nothing else should happen, should probably throw an exception
		default: break;
		}
	    }
	}
    }
    delete [] vatypes_p;
    delete [] vaptr_p;
    delete [] va_p;
    delete [] theheap_p;

    delete currRowTab;
    delete [] nelem;
    delete colNames;
    currRowTab = 0;
    nelem = 0;
    colNames = 0;
}

Table BinaryTable::fullTable(const String& tabname, 
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
    RowCopier rowcop(full, *currRowTab);
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

Table BinaryTable::fullTable()
{
   SetupNewTable newtab("", getDescriptor(), Table::Scratch);
   MemoryStMan stman ("MemSM");
       newtab.bindAll(stman);
    //		and actually create the table
    Table *full= new Table(newtab,Table::Memory, nrows());
    RowCopier rowcop(*full, *currRowTab);
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
    return *full;
}


const TableDesc& BinaryTable::getDescriptor()
{
    return currRowTab->tableDesc();
}

TableRecord& BinaryTable::getKeywords()
{
    return currRowTab->rwKeywordSet();
}

const Table &BinaryTable::thisRow()
{
    return (*currRowTab);
}

const Table &BinaryTable::nextRow()
{
    //		here, its user beware in reading past end of table
    //		i.e. just the same way FITS works.
    if (!theheap_p) read(1);
    else ++(*this);
    fillRow();
    return (*currRowTab);
}

} //# NAMESPACE CASACORE - END

