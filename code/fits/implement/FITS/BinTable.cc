//#
//# Copyright (C) 1994 Associated Universities, Inc. Washington DC, USA.,1995,1996,1997
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

#include <trial/FITS/BinTable.h>
#include <aips/FITS/fits.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/RowCopier.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/Regex.h>
#include <strstream.h>
#include <stdio.h>

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

BinaryTable::BinaryTable(FitsInput& fitsin, ostream& output, Bool useIncrSM,
			 Bool sdfits) :
    BinaryTableExtension(fitsin, output), currRowTab(0), nelem(0), colNames(0)
{
//		this table descriptor is not kept, it is used 
//		during construction only
   TableDesc td;
//		build the keywords of the currRowTab
//		loop through the FITS kw list
   ConstFitsKeywordList &kwl = kwlist();
   kwl.first();
   const FitsKeyword *kw;
   Regex trailing(" *$"); // trailing blanks
   String kwname;
   // will hold the index portion for indexed keywords, this should be
   // more than enough space
   char index[8];
   while (kw = kwl.next()) {
       if (!kw->isreserved() || (sdfits && isSDFitsColumn(kw->kw().name()))) {
	   // Get the kw name and remove the trailing spaces
           kwname = kw->name();
	   kwname = kwname.before(trailing);
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
       colname  = colname.before(trailing);
       if (td.isColumn(colname)) {
	   //		issue a warning, append column number to this name
	   ostrstream newname;
	   newname << colname << "." << i << ends;
	   //		str gives the space to cptr, which must be deleted
	   char * cptr = newname.str();
	   colname = cptr;
	   delete cptr;
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
       Bool isArray = ToBool(nelem[i] > 1 && field(i).fieldtype() != FITS::CHAR
			     && field(i).fieldtype() != FITS::STRING);
       //		switch on the type of column
       switch (field(i).fieldtype()) {
       case FITS::LOGICAL: 
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<Bool>(colname,"",
						  IPosition(1,nelem[i]),
						  ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<Bool>(colname,""));
	   }
	   break;
	   //		BIT, BYTE stored as uChar
       case FITS::BIT: 
	   //			assumes 8 bits per uChar
	   nelem[i] = ((nelem[i] % 8) == 0) ?
	       (nelem[i] % 8) : (nelem[i] % 8) + 1;
       case FITS::BYTE:
	   if (isArray) {
               td.addColumn(ArrayColumnDesc<uChar>(colname,"",
						   IPosition(1,nelem[i]),
						   ColumnDesc::Direct));
	   } else {
               td.addColumn(ScalarColumnDesc<uChar>(colname,""));
	   }
	   break;
	   //		SHORTs are promoted to LONGs
       case FITS::SHORT:
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
	   //		VADESC still needs to be dealt with, NOVALUE should not
	   //		happen in a table
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
	       td.addColumn(ScalarColumnDesc<Bool>(kwSet.name(field),""));
	       break;
	   case TpUChar:
	       td.addColumn(ScalarColumnDesc<uChar>(kwSet.name(field),""));
	       break;
	   case TpShort:
	       td.addColumn(ScalarColumnDesc<Short>(kwSet.name(field),""));
	       break;
	   case TpInt:
	       td.addColumn(ScalarColumnDesc<Int>(kwSet.name(field),""));
	       break;
	   case TpUInt:
	       td.addColumn(ScalarColumnDesc<uInt>(kwSet.name(field),""));
	       break;
	   case TpFloat:
	       td.addColumn(ScalarColumnDesc<Float>(kwSet.name(field),""));
	       break;
	   case TpDouble:
	       td.addColumn(ScalarColumnDesc<Double>(kwSet.name(field),""));
	       break;
	   case TpComplex:
	       td.addColumn(ScalarColumnDesc<Complex>(kwSet.name(field),""));
	       break;
	   case TpDComplex:
	       td.addColumn(ScalarColumnDesc<Complex>(kwSet.name(field),""));
	       break;
	   case TpString:
	       td.addColumn(ScalarColumnDesc<String>(kwSet.name(field),""));
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

    // For some reason at the present time we need to shape all the direct
    // array columns after newtab is created but before a table is created.
    for (uInt j=0; j<tfields(); j++) {
	if (td[(*colNames)(j)].isArray()) {
	    // Every array is a direct array in BinaryTable
	    newtab.setShapeColumn((*colNames)(j), IPosition(1,nelem[j]));
	}
    }

   //		and finally create the Table
   currRowTab = new Table(newtab, 1);

   //		OK, fill the one row of CurrRowTab
   if (nrows() > 0) {
       read(1);
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
		Vector<uChar> vec(nelem[j]);
		//			assumes 8 bits per uChar
		for (Int k=0;k<field(j).nelements();k++) {
		    uChar mask = 1 << (k % 8);
		    vec(k%8) = ((vec(k%8) & ~mask) | thisfield(k) << (k % 8));
		}
		if (nelem[j] > 1) {
		    ArrayColumn<uChar> arrcol(tabcol);
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
		Vector<Int> vec(nelem[j]);
		for (Int k=0;k<nelem[j];k++) {
		    vec(k) = (Short )thisfield(k);
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
		    convertArray(dvec.ac(), vec.ac());
		    dvec.ac() *= tscal(j); 
		    dvec.ac() += tzero(j);
		    convertArray(vec.ac(), dvec.ac());
		} else if (tzero(j) != 0) {
		    vec.ac() += (Float )tzero(j);
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
		    vec.ac() *= tscal(j); 
		    vec.ac() += tzero(j);
		} else if (tzero(j) != 0) {
		    vec.ac() += (Double )tzero(j);
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
		    vec.ac() *= Complex(tscal(j),0); 
		    vec.ac() += Complex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec.ac() += Complex(tzero(j));
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
		    vec.ac() *= DComplex(tscal(j),0) ; 
		    vec.ac() += DComplex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec.ac() += DComplex(tzero(j));
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
		    vec(k) = thisfield(k);
		}
		//			Scale as appropriate
		if (tscal(j) != 1) {
		    vec.ac() *= DComplex(tscal(j),0); 
		    vec.ac() += DComplex(tzero(j),0);
		} else if (tzero(j) != 0) {
		    vec.ac() += DComplex(tzero(j));
		}
		if (nelem[j] > 1) {
		    ArrayColumn<DComplex> arrcol(tabcol);
		    arrcol.put(0,vec);
		} else if (nelem[j] == 1) {
		    tabcol.putScalar(0,vec(0));
		}
            }
            break;
	default:
	    //			VARDESC and NOVALUE (which shouldn't occur here
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
    for (uInt outrow = 0, infitsrow = currrow(); infitsrow < nrows(); 
	 outrow++, infitsrow++) {
	rowcop.copy(outrow, 0);
	//		don't read past the end of the table
	if ((infitsrow+1) < nrows()) {
	    read(1);
	    fillRow();
	}
    }		// end of loop over rows
    return full;
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
    read(1);
    fillRow();
    return (*currRowTab);
}
