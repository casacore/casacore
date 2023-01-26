//# FITSTable.h: Simplified interface to FITS tables with Casacore Look and Feel.
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/fits/FITS/FITSTable.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/OS/Path.h>

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>

#include <casacore/casa/Utilities/ValType.h>

#include <casacore/casa/Arrays/Array.h>

#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Just returns the scalar type.
static DataType fitsDataType(FITS::ValueType fitsType)
{
    switch(fitsType) {
    case FITS::BIT:	 
    case FITS::LOGICAL: return TpBool;
    case FITS::CHAR:     return TpString;
    case FITS::BYTE:	 return TpUChar;
    case FITS::SHORT:    return TpShort;
    case FITS::LONG:     return TpInt;
    case FITS::FLOAT:    return TpFloat;
    case FITS::DOUBLE:   return TpDouble;
    case FITS::COMPLEX:  return TpComplex;
    case FITS::ICOMPLEX:  // ICOMPLEX promoted to DComplex so no precision is lost
    case FITS::DCOMPLEX: return TpDComplex;
    case FITS::STRING: //  return TpString; // FITS just has character arrays,
			 // not strings
    default:
	return TpOther; // VADESC will trigger this
    }
}

// takes a pointer to an array of chars and returns
// its length   A null character terminates the string
// and trailing spaces are not considered as part of the
// string
uint32_t charLength(const char *cptr, uint32_t maxLength)
{
    uint32_t length = 0;
    // watch for any null termination
    while (length < maxLength && cptr[length] != '\0') {
	length++;
    }
    // don't count any trailing spaces
    while (length > 0 && cptr[length-1] == ' ') {
	length--;
    }
    return length;
}

FITSTabular::~FITSTabular()
{
    // Nothing
}

TableRecord FITSTabular::keywordsFromHDU(HeaderDataUnit &hdu,
					 bool allKeywords)
{
    // Setup the keywords.
    //     First, delete the old ones
    TableRecord keywords;
    //     Now add in all the keywords from this HDU
    hdu.firstkw();
    const FitsKeyword *key = hdu.currkw();
    bool noValue = false;

    String name;
    FITS::ReservedName kwname;
    while (key) {
      name = key->name();
      kwname = key->kw().name();
      // skip certain keywords if allKeywords is not true
      if (!allKeywords && key->isreserved() && 
	  (kwname == FITS::BITPIX || kwname == FITS::GCOUNT || kwname == FITS::NAXIS ||
           kwname == FITS::PCOUNT || kwname == FITS::TBCOL || 
           kwname == FITS::TDIM || kwname == FITS::TDISP ||
           kwname == FITS::TFIELDS || kwname == FITS::THEAP ||
           kwname == FITS::TFORM || kwname == FITS::TNULL ||
           kwname == FITS::TSCAL || kwname == FITS::TTYPE || 
           kwname == FITS::TUNIT || kwname == FITS::TZERO ||
           kwname == FITS::XTENSION)) {
	key = hdu.nextkw();
	continue;
      }
      if (key->isindexed()) {
	ostringstream num;
	num << key->index();
	name += String(num);
      }

      switch (key->type()) {
      case FITS::LOGICAL : 
	keywords.define(name,key->asBool());
	break;
      case FITS::STRING : 
	{
	    const char * cptr = key->asString();
	    uint32_t length = charLength(cptr, key->valStrlen());
	    keywords.define(name,String(cptr,length));
	}
	break;
      case FITS::FLOAT : 
	keywords.define(name,key->asFloat());
	break;
      case FITS::DOUBLE : 
	keywords.define(name,key->asDouble());
	break;
      case FITS::LONG : 
	keywords.define(name,key->asInt());
	break;
      case FITS::COMPLEX : 
	keywords.define(name,key->asComplex());
	break;
      case FITS::DCOMPLEX : 
	keywords.define(name,key->asDComplex());
	break;
      case FITS::NOVALUE : 
	noValue = true;
	break;
      default:
	throw(AipsError("FITSTablular::keywordsFromHDU() - unknown keyword type"
			" (cannot happen!)"));
      }
      // Don't comment keywords without a value (e.g. END).
      if (!noValue) {
	keywords.setComment(name, key->comm());
      }
      key = hdu.nextkw();
    }
    return keywords;
}

RecordDesc FITSTabular::descriptionFromHDU(
    BinaryTableExtension &hdu)
{
    RecordDesc description;
    uint32_t ncol = hdu.ncols();

    // this is needed here
    Record subStringInfo = subStringShapeFromHDU(hdu);

    IPosition shape;
    for (uint32_t i=0; i < ncol; i++) {
	DataType type = fitsDataType(hdu.field(i).fieldtype());
	shape.resize(hdu.field(i).dims());
	for (uint32_t j=0; j<shape.nelements(); j++) {
	    shape(j) = hdu.field(i).dim(j);
	}
	String colname(hdu.ttype(i));
        colname.rtrim(' ');
	// watch for VADESC columns
	if (hdu.field(i).fieldtype() == FITS::VADESC) {
	    // variable array descriptor
	    FITS::ValueType ftype;
	    int maxelem;
	    FITS::parse_vatform(hdu.tform(i), ftype, maxelem);
	    type = fitsDataType(ftype);
	    shape.resize(0);
	}
	// obvious Scalar
	if (shape.nelements() == 1 && shape.product() == 1) {
	    description.addField(colname, type);
	} else if (type == TpString) {
	    // TpString is the only known special case
	    // is this a substring convention
	    if (subStringInfo.isDefined(colname)) {
		const Record info(subStringInfo.asRecord(colname));
		int32_t nelem = info.asInt("NELEM");
		if (nelem > 0) {
		    // fixed shape
		    description.addField(colname, type, IPosition(1,nelem));
		} else {
		    // variable shape
		    description.addField(colname, asArray(type));
		}
	    } else {
		// Scalar
		description.addField(colname, type);
	    }
	} else {
	    // Array
	    if (shape.nelements() == 0) {
		// variable shapped array
		// leave shape off, make sure this is an array DataType
		// this makes this a variable sized array
		description.addField(colname, asArray(type));
	    } else {
		description.addField(colname, type, shape);
	    }
	}
    }
    return description;
}

Record FITSTabular::subStringShapeFromHDU(BinaryTableExtension &hdu)
{
    Record subStringShapes;
    uint32_t ncol = hdu.ncols();

    Regex trailing(" *$"); // trailing blanks
    for (uint32_t i=0; i < ncol; i++) {
	String colname(hdu.ttype(i));
        colname.rtrim(' ');
	String tform(hdu.tform(i));
        tform.rtrim(' ');
	// Look for the sub-string convention, described in appendix C of
	// Cotton, Tody, and Pence.  Its in the TFIELD for this column.
	// This probably could (should?) happen in FitsField<char>.
	// But I understand this better so do it here.
	if (tform.matches(Regex("^.*A:SSTR[0-9]+(/[0-9]+)?$"))) {
	    Record info;
	    String sstr = tform.after(tform.find("SSTR")+3);
            String::size_type slinx = sstr.find('/');
            if (slinx != String::npos) {
		// two integers separate by a the slash
                int32_t maxChars = atol(sstr.before(slinx).chars());
		int32_t delim = atol(sstr.after(slinx).chars());
		info.define("NCHAR", maxChars);
		info.define("NELEM", -1);
		info.define("DELIM", String(char(delim)));
	    } else {
		// it must be just an integer at this point
		int32_t nchars = atol(sstr.chars());
		// fixed shape String array
		// determine the shape given nchars
		int32_t nelem = hdu.field(i).nelements() / nchars;
		if (nelem < 1) nelem = 1;
		info.define("NCHAR", nchars);
		info.define("NELEM", nelem);
		info.define("DELIM", String(char('\0')));
	    }
	    subStringShapes.defineRecord(colname, info);
	}
    }
    return subStringShapes;
}

Record FITSTabular::unitsFromHDU(BinaryTableExtension &hdu)
{
    Record units;
    uint32_t ncol = hdu.ncols();

    for (uint32_t i=0; i < ncol; i++) {
	String colname(hdu.ttype(i));
	colname.rtrim(' ');
	String unitval(hdu.tunit(i));
	unitval.rtrim(' ');
	if (!unitval.empty()) units.define(colname, unitval);
    }
    return units;
}

Record FITSTabular::displayFormatsFromHDU(BinaryTableExtension &hdu)
{
    Record disps;
    uint32_t ncol = hdu.ncols();

    for (uint32_t i=0; i < ncol; i++) {
	String colname(hdu.ttype(i));
	colname.rtrim(' ');
	String dispval(hdu.tdisp(i));
	dispval.rtrim(' ');
	if (!dispval.empty()) disps.define(colname, dispval);
    }
    return disps;
}

Record FITSTabular::nullsFromHDU(BinaryTableExtension &hdu)
{
    Record nulls;
    uint32_t ncol = hdu.ncols();

    // strangely, the first arg to hdu.kw is a reference
    // to a FITS::ReservedName (not even a const reference)
    // So, since I can't put FITS::TNULL there, I need to
    // make it a variable, first.  Argh.
    FITS::ReservedName tnull = FITS::TNULL;
    for (uint32_t i=0; i < ncol; i++) {
	// only if column is BYTE, SHORT, LONG and
	// tscal == 1.0 and tzero = 0.0, i.e. no promotion will be done
	// also, make sure it has a TNULL keyword in the original
	// FITS since hdu.tnull(i) returns the minimum int32_t even
	// when no TNULL keyword is present.
	if (hdu.kw(tnull, i) &&
	    (hdu.field(i).fieldtype() == FITS::BYTE ||
	     hdu.field(i).fieldtype() == FITS::SHORT ||
	     hdu.field(i).fieldtype() == FITS::LONG) &&
	    hdu.tscal(i) == 1.0 && hdu.tzero(i) == 0.0) {
	    String colname(hdu.ttype(i));
	    colname.rtrim(' ');
	    nulls.define(colname, (hdu.kw(tnull,i))->asInt());
	}
    }
    return nulls;
}

TableDesc FITSTabular::tableDesc(const FITSTabular &fitstabular)
{
    // construct the output table given the description in the first
    // row of infits
    TableDesc td;
    td.rwKeywordSet() = fitstabular.keywords();
    RecordDesc desc = fitstabular.description();
    Record units = fitstabular.units();
    Record disps = fitstabular.displayFormats();
    Record nulls = fitstabular.nulls();

    for (uint32_t i=0;i<desc.nfields();i++) {
	if (!desc.isArray(i)) {
	    // it shouldn't be necessary to set the default value here
	    // but it seem to be 
	    switch (desc.type(i)) {
	    case TpBool:
		{ 
		    ScalarColumnDesc<bool> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefBool());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<bool>(desc.name(i), desc.comment(i)));
		break;
	    case TpChar:
		{ 
		    ScalarColumnDesc<char> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefChar());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<char>(desc.name(i), desc.comment(i)));
		break;
	    case TpUChar:
		{ 
		    ScalarColumnDesc<unsigned char> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefUChar());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<unsigned char>(desc.name(i), desc.comment(i)));
		break;
	    case TpShort:
		{ 
		    ScalarColumnDesc<int16_t> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefShort());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<int16_t>(desc.name(i), desc.comment(i)));
		break;
	    case TpInt:
		{ 
		    ScalarColumnDesc<int32_t> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefInt());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<int32_t>(desc.name(i), desc.comment(i)));
		break;
	    case TpFloat:
		{ 
		    ScalarColumnDesc<float> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefFloat());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<float>(desc.name(i), desc.comment(i)));
		break;
	    case TpDouble:
		{ 
		    ScalarColumnDesc<double> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefDouble());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<double>(desc.name(i), desc.comment(i)));
		break;
	    case TpComplex:
		{ 
		    ScalarColumnDesc<Complex> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefComplex());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<Complex>(desc.name(i), desc.comment(i)));
		break;
	    case TpDComplex:
		{ 
		    ScalarColumnDesc<DComplex> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefDComplex());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<DComplex>(desc.name(i), desc.comment(i)));
		break;
	    case TpString:
		{ 
		    ScalarColumnDesc<String> scd(desc.name(i), desc.comment(i));
		    scd.setDefault(ValType::undefString());
		    td.addColumn(scd);
		}
		// td.addColumn(ScalarColumnDesc<String>(desc.name(i), desc.comment(i)));
		break;
	    default:
		cerr << "Unrecognized scalar column data type in column " <<
		    desc.name(i) << " : " << desc.type(i) << endl;
		break;
	    }
	} else {
	    bool fixedShape = true;
	    IPosition shape = desc.shape(i);
	    int32_t options = 0;
	    if (shape.nelements() == 1 && shape(0) == -1) {
		fixedShape = false;
	    } else {
		options = ColumnDesc::FixedShape;
	    }
	    // this division between direct and indirect is arbitrary
	    // I wonder what a good division is?
	    if (fixedShape && shape.product() <= 20) {
		options = ColumnDesc::Direct;
	    } 
	    switch (desc.type(i)) {
	    case TpBool:
	    case TpArrayBool:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<bool>(desc.name(i), desc.comment(i),
						       shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<bool>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpChar:
	    case TpArrayChar:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<char>(desc.name(i), desc.comment(i),
						       shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<char>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpUChar:
	    case TpArrayUChar:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<unsigned char>(desc.name(i), desc.comment(i),
							shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<unsigned char>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpShort:
	    case TpArrayShort:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<int16_t>(desc.name(i), desc.comment(i),
							shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<int16_t>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpInt:
	    case TpArrayInt:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<int32_t>(desc.name(i), desc.comment(i),
						      shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<int32_t>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpFloat:
	    case TpArrayFloat:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<float>(desc.name(i), desc.comment(i),
							shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<float>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpDouble:
	    case TpArrayDouble:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<double>(desc.name(i), desc.comment(i),
							 shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<double>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpComplex:
	    case TpArrayComplex:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<Complex>(desc.name(i), desc.comment(i),
							  shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<Complex>(desc.name(i), desc.comment(i)));
		}
		break;
	    case TpDComplex:
	    case TpArrayDComplex:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<DComplex>(desc.name(i), desc.comment(i),
							   shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<DComplex>(desc.name(i), desc.comment(i)));
			}
		break;
	    case TpString:
	    case TpArrayString:
		if (fixedShape) {
		    td.addColumn(ArrayColumnDesc<String>(desc.name(i), desc.comment(i),
							 shape, options));
		} else {
		    td.addColumn(ArrayColumnDesc<String>(desc.name(i), desc.comment(i)));
		}
		break;
	    default:
		cerr << "Unrecognized array column data type in column " <<
		    desc.name(i) << " : " << desc.type(i) << endl;
		break;
	    }
	}
	// add in any units, displayFormats, and nulls, if available
	if (units.isDefined(desc.name(i))) {
	    td.rwColumnDesc(desc.name(i)).rwKeywordSet().define("UNIT",
								units.asString(desc.name(i)));
	}
	if (disps.isDefined(desc.name(i))) {
	    td.rwColumnDesc(desc.name(i)).rwKeywordSet().define("DISP",
								disps.asString(desc.name(i)));
	}
	if (nulls.isDefined(desc.name(i))) {
	    td.rwColumnDesc(desc.name(i)).rwKeywordSet().define("NULL",
								nulls.asInt(desc.name(i)));
	}
    }
    return td;
}

FITSTable::FITSTable(uint32_t whichHDU, bool allKeywords)
    : hdu_nr_p(whichHDU), row_nr_p(-1), raw_table_p(0), io_p(0),
      row_p(RecordInterface::Variable), allKeys_p(allKeywords),
      nfields_p(0), row_fields_p(0), field_types_p(0), vatypes_p(0),
      vaptr_p(0), va_p(0), theheap_p(0)
{
    isValid_p = false;
}

FITSTable::FITSTable(const String &fileName, uint32_t whichHDU, 
		     bool allKeywords)
    : hdu_nr_p(whichHDU), row_nr_p(-1), raw_table_p(0), io_p(0), 
      row_p(RecordInterface::Variable), allKeys_p(allKeywords), 
      nfields_p(0), row_fields_p(0), field_types_p(0), vatypes_p(0), 
      vaptr_p(0), va_p(0), theheap_p(0)
{
    isValid_p = reopen(fileName);
}

bool FITSTable::reopen(const String &fileName)
{
    clear_self();

    // use the Path class so that ~ is parsed if present in the file name
    Path filePath(fileName);
    io_p = new FitsInput(filePath.expandedName().chars(), FITS::Disk);
    AlwaysAssert(io_p, AipsError);
    if (io_p->err() || io_p->eof()) {
	return false;
    }
    // construct the primary HDU keywords record
    if (io_p->hdutype() != FITS::PrimaryArrayHDU) return false;

    switch (io_p->datatype()) {
    case FITS::BYTE:
	{
	    BytePrimaryArray pa(*io_p);
	    primaryKeys_p = FITSTabular::keywordsFromHDU(pa, allKeys_p);
	    if (pa.nelements()) reopenAtFirstHDU(filePath.expandedName().chars());
	}
    break;
    case FITS::SHORT:
	{
	    ShortPrimaryArray pa(*io_p);
	    primaryKeys_p = FITSTabular::keywordsFromHDU(pa, allKeys_p);
	    if (pa.nelements()) reopenAtFirstHDU(filePath.expandedName().chars());
	}
    break;
    case FITS::LONG:
	{
	    LongPrimaryArray pa(*io_p);
	    primaryKeys_p = FITSTabular::keywordsFromHDU(pa, allKeys_p);
	    if (pa.nelements()) reopenAtFirstHDU(filePath.expandedName().chars());
	}
    break;
    case FITS::FLOAT:
	{
	    FloatPrimaryArray pa(*io_p);
	    primaryKeys_p = FITSTabular::keywordsFromHDU(pa, allKeys_p);
	    if (pa.nelements()) reopenAtFirstHDU(filePath.expandedName().chars());
	}
    break;
    case FITS::DOUBLE:
	{
	    DoublePrimaryArray pa(*io_p);
	    primaryKeys_p = FITSTabular::keywordsFromHDU(pa, allKeys_p);
	    if (pa.nelements()) reopenAtFirstHDU(filePath.expandedName().chars());
	}
    break;
    default:
	return false;
    }

    uint32_t i;
    for (i=1; i < hdu_nr_p && !io_p->err(); i++) {
	io_p->skip_hdu();
    }

    if (io_p->err() || io_p->eof()) {
	return false;
    }
    
    // OK; we have a valid HDU
    if (io_p->hdutype() == FITS::BinaryTableHDU) {
	raw_table_p = new BinaryTableExtension(*io_p);
    } else if (io_p->hdutype() == FITS::AsciiTableHDU) {
	raw_table_p = new AsciiTableExtension(*io_p);
    } else {
	return false;
    }
    AlwaysAssert(raw_table_p, AipsError);
    keywords_p = FITSTabular::keywordsFromHDU(*raw_table_p, allKeys_p);
    description_p = FITSTabular::descriptionFromHDU(*raw_table_p);
    units_p = FITSTabular::unitsFromHDU(*raw_table_p);
    disps_p = FITSTabular::displayFormatsFromHDU(*raw_table_p);
    nulls_p = FITSTabular::nullsFromHDU(*raw_table_p);
    subStrShapes_p = FITSTabular::subStringShapeFromHDU(*raw_table_p);

    // resize some things based on the number of fields in the description
    nfields_p = description_p.nfields();
    row_fields_p.resize(nfields_p);
    field_types_p.resize(nfields_p);
    promoted_p.resize(nfields_p);
    tdims_p.resize(nfields_p);
    promoted_p = false;
    tdims_p = -1;
    bool anyPromoted = false;
    bool anyReshaped = false;
    // look for fields to promote and TDIMnnn columns, extracting (nnn-1)
    for (i=0;i<nfields_p;i++) {
	DataType type = description_p.type(i);
	if ((raw_table_p->tscal(i) != 1.0 || raw_table_p->tzero(i) != 0.0)
	    && (type == TpUChar || type == TpArrayUChar ||
		type == TpShort || type == TpArrayShort ||
		type == TpInt   || type == TpArrayInt)) {
	    promoted_p[i] = true;
	    anyPromoted = true;
	} 
	if (description_p.name(i).matches(Regex("^TDIM[0-9]+$")) && 
	    description_p.type(i) == TpString) {
            String tdim = description_p.name(i);
            tdim = tdim.after(3);
	    int32_t which = atol(tdim.chars())-1;
	    if (which >= 0 && which < int32_t(tdims_p.nelements())) {
		anyReshaped = true;
		tdims_p[which] = i;
	    } 
	}
    }
    // it actually gets redone here so that we can preserve the order
    // of things in the description
    if (anyPromoted || anyReshaped) {
	RecordDesc newDesc;
	for (i=0;i<nfields_p;i++) {
	    DataType type = description_p.type(i);
	    IPosition shape = description_p.shape(i);
	    if (promoted_p[i] == true) {
		if (isArray(type)) {
		    type = TpArrayDouble;
		} else {
		    type = TpDouble;
		}
	    }
	    if (tdims_p[i] >= 0) {
		// this is really a variable shaped array
		if (!isArray(type)) {
		    // but its stored here as a scalar - must
		    // be just one element - promote it to an
		    // an array type
		    type = asArray(type);
		}
		// so we don't specify a shape here.
		newDesc.addField(description_p.name(i), type);
	    } else {
		if (isArray(type)) {
		    // add a field of the appropriate shape
		    newDesc.addField(description_p.name(i), type, shape);
		} else {
		    newDesc.addField(description_p.name(i), type);
		}
	    }
      	}
	description_p = newDesc;
    }

    row_p.restructure(description_p);

    // read the heap (and the rest of the table, since this is a
    // sequential access file only) if one is present
    if (raw_table_p->pcount()) {
	raw_table_p->read(raw_table_p->nrows());
	if (raw_table_p->notnull(raw_table_p->theap())) {
	    uint32_t heapOffset = raw_table_p->theap() - 
		raw_table_p->rowsize()*raw_table_p->nrows();
	    // skip to the start of the heap
	    // I don't see any way except to read these bogus bytes
	    Block<char> junk(heapOffset);
	    raw_table_p->ExtensionHeaderDataUnit::read(junk.storage(), 
						       heapOffset);
	}
	theheap_p = new char [raw_table_p->pcount()];
	AlwaysAssert(theheap_p, AipsError);
	raw_table_p->ExtensionHeaderDataUnit::read(theheap_p, 
						   raw_table_p->pcount());
    } else {
	// just read one row, assuming there are any rows to read
	if (raw_table_p->nrows()) raw_table_p->read(1);
    }
    row_nr_p++;

    // Setup the record fields (one time only)
    for (i=0; i < nfields_p; i++) {
	switch( description_p.type(i)) {
	case TpBool: 
	    row_fields_p[i] = new RecordFieldPtr<bool>(row_p, i);
	    break;
	case TpArrayBool:
	    row_fields_p[i] = new RecordFieldPtr<Array<bool> >(row_p, i);
	    break;
	case TpUChar:
	    row_fields_p[i] = new RecordFieldPtr<unsigned char>(row_p, i);
	    break;
	case TpArrayUChar:
	    row_fields_p[i] = new RecordFieldPtr<Array<unsigned char> >(row_p, i);
	    break;
	case TpShort:
	    row_fields_p[i] = new RecordFieldPtr<int16_t>(row_p, i);
	    break;
	case TpArrayShort:
	    row_fields_p[i] = new RecordFieldPtr<Array<int16_t> >(row_p, i);
	    break;
	case TpInt:
	    row_fields_p[i] = new RecordFieldPtr<int32_t>(row_p, i);
 	    break;
	case TpArrayInt:
	    row_fields_p[i] = new RecordFieldPtr<Array<int32_t> >(row_p, i);
	    break;
	case TpFloat:
	    row_fields_p[i] = new RecordFieldPtr<float>(row_p, i);
	    break;
	case TpArrayFloat:
	    row_fields_p[i] = new RecordFieldPtr<Array<float> >(row_p, i);
	    break;
	case TpDouble:
	    row_fields_p[i] = new RecordFieldPtr<double>(row_p, i);
	    break;
	case TpArrayDouble:
	    row_fields_p[i] = new RecordFieldPtr<Array<double> >(row_p, i);
	    break;
	case TpComplex:
	    row_fields_p[i] = new RecordFieldPtr<Complex>(row_p, i);
            break;
	case TpArrayComplex:
	    row_fields_p[i] = new RecordFieldPtr<Array<Complex> >(row_p, i);
	    break;
	case TpDComplex:
	    row_fields_p[i] = new RecordFieldPtr<DComplex>(row_p, i);
	    break;
	case TpArrayDComplex:
	    row_fields_p[i] = new RecordFieldPtr<Array<DComplex> >(row_p, i);
	    break;
	case TpString:
	    row_fields_p[i] = new RecordFieldPtr<String>(row_p, i);
	    break;
	case TpArrayString:
	    row_fields_p[i] = new RecordFieldPtr<Array<String> >(row_p, i);
	    break;
	default:
	    throw(AipsError("FITSTable::reopen() - unknown field type"));
	}
	AlwaysAssert(row_fields_p[i] != 0, AipsError);
	field_types_p[i] = description_p.type(i);
    }

    name_p = fileName;
    // set up things necessary fo VADESC cols
    // this is only necessary if a heap exists
    if (theheap_p) {
        int32_t ncols = raw_table_p->ncols();
	vatypes_p.resize(raw_table_p->ncols());
	vaptr_p.resize(raw_table_p->ncols());
	va_p = new VADescFitsField [ncols];
	AlwaysAssert(va_p, AipsError);
	for (i=0;i<uint32_t(raw_table_p->ncols());i++) {
	    vaptr_p[i] = 0;
	    vatypes_p[i] = FITS::NOVALUE;
	    if (raw_table_p->field(i).fieldtype() == FITS::VADESC) {
		int maxsize;
		FITS::ValueType vtype;
		FITS::parse_vatform(raw_table_p->tform(i),
				    vtype, maxsize);
		vatypes_p[i] = vtype;
		raw_table_p->bind(i, va_p[i]);
		if (vatypes_p[i] == FITS::NOVALUE) {
		    throw(AipsError("FITSTable::reopen() - invalid VADESC format"));
		}
		switch (vatypes_p[i]) {
		case FITS::LOGICAL: 
		    vaptr_p[i] = (void *)(new FitsLogical[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::BIT: 
		    {
			int32_t nbytes = maxsize / 8;
			if (maxsize % 8) nbytes++;
			maxsize = nbytes;
		    }
		    // fall through to BYTE for actual allocation
		    CASACORE_FALLTHROUGH;
		case FITS::BYTE: 
		    vaptr_p[i] = (void *)(new unsigned char[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::SHORT: 
		    vaptr_p[i] = (void *)(new int16_t[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::LONG: 
		    vaptr_p[i] = (void *)(new FitsLong[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::CHAR: 
		    vaptr_p[i] = (void *)(new char[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::FLOAT: 
		    vaptr_p[i] = (void *)(new float[maxsize]);
		    AlwaysAssert(vaptr_p[i], AipsError);
		    break;
		case FITS::DOUBLE:
		    vaptr_p[i] = (void *)(new double[maxsize]);
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
	}
    }
	
    if (description_p.nfields() > 0 && raw_table_p->nrows()) {
	fill_row();
    }
    isValid_p = true;

    return true;
}


void FITSTable::next()
{
    // first, read a row or step to the next row
    row_nr_p++;
    if (row_nr_p >= raw_table_p->nrows()) {
	return; // Don't read past the end, this row is already filled
    }
    // Use the native FITS classes
    if (!theheap_p) raw_table_p->read(1);
    else ++(*raw_table_p);
    if (isValid()) fill_row();
}

// What an ugly function! Simplify somehow!
void FITSTable::fill_row()
{
    // fill the current row into the Row object.
    for (uint32_t i=0; i < nfields_p; i++) {
	// get the scaling factors
	double zero, scale;
	zero = raw_table_p->tzero(i);
	scale = raw_table_p->tscal(i);
	// get any necessary shapes
	IPosition shape;
	if (isArray(DataType(field_types_p[i])) && 
	    description_p.shape(i).nelements()==1 &&
	    description_p.shape(i)(0) == -1) {
	    // this is only worth doing for arrays which can be reshaped
	    if (tdims_p[i] >= 0) {
		// get the shape from the tdim column
		// get it from the raw fits since it might not have been filled yet
		int32_t tdfield = tdims_p[i];
		DebugAssert(field_types_p[tdfield] == TpString, AipsError);
		FitsField<char> &fitsRef = 
		    (FitsField<char> &)(raw_table_p->field(tdfield));
		// look for the true end of the string
		char * cptr = (char *)fitsRef.data();
		uint32_t length = fitsRef.nelements();
		while (length > 0 && 
		       (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
		    length--;
		}
		String tdim((char *)fitsRef.data(), length);
		// remove the surrounding parenthesis
		tdim = tdim.after(tdim.find('('));
		tdim = tdim.before(tdim.find(')'));
		// count up the number of commas
		int32_t ncommas = tdim.freq(',');
		shape.resize(ncommas+1, false);
		for (int32_t j=0;j<ncommas;j++) {
                    String::size_type inx = tdim.find(',');
                    String field = tdim.before(inx);
		    tdim = tdim.after(inx);
		    shape(j) = atol(field.chars());
		}
		// the final field is left
		shape(ncommas) = atol(tdim.chars());
	    } else {
		// this must be a VADesc, that's the only way this can ever happen
		// unless, of course, its a CHAR array using the sub-string convention
		if (raw_table_p->field(i).fieldtype() != FITS::CHAR) {
		    DebugAssert(raw_table_p->field(i).fieldtype() == FITS::VADESC, AipsError);
		    FitsVADesc thisva = va_p[i]();
		    shape = IPosition(1, thisva.num());
		}
	    }
	}
	switch (raw_table_p->field(i).fieldtype()) {
	case FITS::LOGICAL:
	{
	    FitsField<FitsLogical> &fitsRef = 
		(FitsField<FitsLogical> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpBool) {
		RecordFieldPtr<bool> &rowRef =
		    *((RecordFieldPtr<bool> *)row_fields_p[i]);
		*rowRef = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
		RecordFieldPtr<Array<bool> > &rowRef =
		    *((RecordFieldPtr<Array<bool> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		bool *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::BIT:
	{
	    FitsField<FitsBit> &fitsRef = 
		(FitsField<FitsBit> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpBool) {
		RecordFieldPtr<bool> &rowRef =
		    *((RecordFieldPtr<bool> *)row_fields_p[i]);
		(*rowRef) = (int(fitsRef()));
	    } else {
		DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
		RecordFieldPtr<Array<bool> > &rowRef =
		    *((RecordFieldPtr<Array<bool> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		bool *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    data[n] = (int(fitsRef(n)));
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::CHAR:
	{
	    String fieldName = description_p.name(i);
	    if (!subStrShapes_p.isDefined(fieldName)) {
		DebugAssert(field_types_p[i] == TpString, AipsError);
		FitsField<char> &fitsRef = 
		    (FitsField<char> &)(raw_table_p->field(i));
		RecordFieldPtr<String> &rowRef =
		    *((RecordFieldPtr<String> *)row_fields_p[i]);
		char * cptr = (char *)fitsRef.data();
		uint32_t length = charLength(cptr, fitsRef.nelements());
		(*rowRef) = String(cptr, length);
	    } else {
		DebugAssert(field_types_p[i] == TpArrayString, AipsError);
		FitsField<char> &fitsRef = 
		    (FitsField<char> &)(raw_table_p->field(i));
		RecordFieldPtr<Array<String> > &rowRef = 
		    *((RecordFieldPtr<Array<String> > *)row_fields_p[i]);
		char * cptr = (char *)fitsRef.data();
		uint32_t length = charLength(cptr, fitsRef.nelements());
		String rawValue(cptr, length);
		// figure out a way to cache this to make it faster
		Record info(subStrShapes_p.asRecord(fieldName));
		int32_t nels = info.asInt("NELEM");
		int32_t nchar = info.asInt("NCHAR");
		Vector<String> result;
		if (nels > 0) {
		    // fixed shape
		    result.resize(nels);
		    // and just do them all
		    int32_t curr = 0;
		    for (int32_t z=0;z<nels;z++) {
			result(z) = rawValue.at(curr, nchar);
			curr += nchar;
		    }
		} else {
		    // variable shape
		    // count the number of delimiters and add 1
		    String delim(info.asString("DELIM"));
		    nels = rawValue.freq(delim) + 1;
		    result.resize(nels);
		    (*rowRef).resize(result.shape());
		    for (int32_t z=0;z<(nels-1);z++) {
                        String::size_type inx = rawValue.find(delim);
			result(z) = rawValue.before(inx);
			rawValue = rawValue.after(inx+delim.size()-1);
		    }
		    // the last one remains
		    result(nels-1) = rawValue;
		}
		// now put the result intot the output record field
		(*rowRef) = result;
	    }
	}
	break;
	case FITS::BYTE:
	{
	    FitsField<unsigned char> &fitsRef = 
		(FitsField<unsigned char> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpUChar) {
		RecordFieldPtr<unsigned char> &rowRef =
		    *((RecordFieldPtr<unsigned char> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		if (promoted_p[i]) {
		    DebugAssert(field_types_p[i] == TpArrayDouble ||
				field_types_p[i] == TpDouble, AipsError);
		    if (field_types_p[i] == TpArrayDouble) {
			RecordFieldPtr<Array<double> > &rowRef =
			    *((RecordFieldPtr<Array<double> > *)row_fields_p[i]);
			// does this need to be reshaped?
			int32_t n = raw_table_p->field(i).nelements();
			if (tdims_p[i] >= 0) {
			    (*rowRef).resize(shape);
			    n = shape.product();
			}
			bool deleteIt;
			double *data = (*rowRef).getStorage(deleteIt);
			while (n) {
			    n--;
			    data[n] = double(fitsRef(n));
			}
			(*rowRef).putStorage(data, deleteIt);
			*rowRef *= scale;
			*rowRef += zero;
		    } else {
			RecordFieldPtr<double> &rowRef =
			    *((RecordFieldPtr<double> *)row_fields_p[i]);
			(*rowRef) = double(fitsRef())*scale + zero;
		    }
		} else {
		    DebugAssert(field_types_p[i] == TpArrayUChar, AipsError);
		    RecordFieldPtr<Array<unsigned char> > &rowRef =
			*((RecordFieldPtr<Array<unsigned char> > *)row_fields_p[i]);
		    // does this need to be reshaped?
		    int32_t n = raw_table_p->field(i).nelements();
		    if (tdims_p[i] >= 0) {
			(*rowRef).resize(shape);
			n = shape.product();
		    }
		    bool deleteIt;
		    unsigned char *data = (*rowRef).getStorage(deleteIt);
		    while (n) {
			n--;
			data[n] = fitsRef(n);
		    }
		    (*rowRef).putStorage(data, deleteIt);
		}
	    }
	}
	break;
	case FITS::SHORT:
	{
	    FitsField<short> &fitsRef = 
		(FitsField<short> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpShort) {
		RecordFieldPtr<int16_t> &rowRef =
		    *((RecordFieldPtr<int16_t> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		if (promoted_p[i]) {
		    DebugAssert(field_types_p[i] == TpArrayDouble ||
				field_types_p[i] == TpDouble, AipsError);
		    if (field_types_p[i] == TpArrayDouble) {
			RecordFieldPtr<Array<double> > &rowRef =
			    *((RecordFieldPtr<Array<double> > *)row_fields_p[i]);
			// does this need to be reshaped?
			int32_t n = raw_table_p->field(i).nelements();
			if (tdims_p[i] >= 0) {
			    (*rowRef).resize(shape);
			    n = shape.product();
			}
			bool deleteIt;
			double *data = (*rowRef).getStorage(deleteIt);
			while (n) {
			    n--;
			    data[n] = double(fitsRef(n));
			}
			(*rowRef).putStorage(data, deleteIt);
			*rowRef *= scale;
			*rowRef += zero;
		    } else {
			RecordFieldPtr<double> &rowRef =
			    *((RecordFieldPtr<double> *)row_fields_p[i]);
			(*rowRef) = double(fitsRef())*scale + zero;
		    }
		} else {
		    DebugAssert(field_types_p[i] == TpArrayShort, AipsError);
		    RecordFieldPtr<Array<int16_t> > &rowRef =
			*((RecordFieldPtr<Array<int16_t> > *)row_fields_p[i]);
		    // does this need to be reshaped?
		    int32_t n = raw_table_p->field(i).nelements();
		    if (tdims_p[i] >= 0) {
			(*rowRef).resize(shape);
			n = shape.product();
		    }
		    bool deleteIt;
		    int16_t *data = (*rowRef).getStorage(deleteIt);
		    while (n) {
			n--;
			data[n] = fitsRef(n);

		    }
		    (*rowRef).putStorage(data, deleteIt);
		}
	    }
	}
	break;
	case FITS::LONG:
	{
	    FitsField<FitsLong> &fitsRef = 
		(FitsField<FitsLong> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpInt) {
		RecordFieldPtr<int32_t> &rowRef =
		    *((RecordFieldPtr<int32_t> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		if (promoted_p[i]) {
		    DebugAssert(field_types_p[i] == TpArrayDouble ||
				field_types_p[i] == TpDouble, AipsError);
		    if (field_types_p[i] == TpArrayDouble) {
			RecordFieldPtr<Array<double> > &rowRef =
			    *((RecordFieldPtr<Array<double> > *)row_fields_p[i]);
			// does this need to be reshaped?
			int32_t n = raw_table_p->field(i).nelements();
			if (tdims_p[i] >= 0) {
			    (*rowRef).resize(shape);
			    n = shape.product();
			}
			bool deleteIt;
			double *data = (*rowRef).getStorage(deleteIt);
			while (n) {
			    n--;
			    data[n] = double(fitsRef(n));
			}
			(*rowRef).putStorage(data, deleteIt);
			*rowRef *= scale;
			*rowRef += zero;
		    } else {
			RecordFieldPtr<double> &rowRef =
			    *((RecordFieldPtr<double> *)row_fields_p[i]);
			(*rowRef) = double(fitsRef())*scale + zero;
		    }
		} else {
		    DebugAssert(field_types_p[i] == TpArrayInt, AipsError);
		    RecordFieldPtr<Array<int32_t> > &rowRef =
			*((RecordFieldPtr<Array<int32_t> > *)row_fields_p[i]);
		    // does this need to be reshaped?
		    int32_t n = raw_table_p->field(i).nelements();
		    if (tdims_p[i] >= 0) {
			(*rowRef).resize(shape);
			n = shape.product();
		    }
		    bool deleteIt;
		    int32_t *data = (*rowRef).getStorage(deleteIt);
		    while (n) {
			n--;
			data[n] = fitsRef(n);
		    }
		    (*rowRef).putStorage(data, deleteIt);
		}
	    }
	}
	break;
	case FITS::FLOAT:
	{
	    FitsField<float> &fitsRef = 
		(FitsField<float> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpFloat) {
		RecordFieldPtr<float> &rowRef =
		    *((RecordFieldPtr<float> *)row_fields_p[i]);
		(*rowRef) = float(fitsRef()*scale + zero);
	    } else {
		DebugAssert(field_types_p[i] == TpArrayFloat, AipsError);
		RecordFieldPtr<Array<float> > &rowRef =
		    *((RecordFieldPtr<Array<float> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		float *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
		*rowRef *= float(scale);
		*rowRef += float(zero);
	    }
	}
	break;
	case FITS::DOUBLE:
	{
	    FitsField<double> &fitsRef = 
		(FitsField<double> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpDouble) {
		RecordFieldPtr<double> &rowRef =
		    *((RecordFieldPtr<double> *)row_fields_p[i]);
		(*rowRef) = fitsRef()*scale + zero;
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
		RecordFieldPtr<Array<double> > &rowRef =
		    *((RecordFieldPtr<Array<double> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		double *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
		*rowRef *= scale;
		*rowRef += zero;
	    }
	}
	break;
	case FITS::COMPLEX:
	{
	    FitsField<Complex> &fitsRef = 
		(FitsField<Complex> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpComplex) {
		RecordFieldPtr<Complex> &rowRef =
		    *((RecordFieldPtr<Complex> *)row_fields_p[i]);
		const Complex& val = fitsRef();
		(*rowRef) = Complex (val.real() * scale + zero,
				     val.imag() * scale + zero);
	    } else {
		DebugAssert(field_types_p[i] == TpArrayComplex, AipsError);
		RecordFieldPtr<Array<Complex> > &rowRef =
		    *((RecordFieldPtr<Array<Complex> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		Complex *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    const Complex& val = fitsRef(n);
		    data[n] = Complex (val.real() * scale + zero,
				       val.imag() * scale + zero);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::DCOMPLEX:
	{
	    FitsField<DComplex> &fitsRef = 
		(FitsField<DComplex> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpDComplex) {
		RecordFieldPtr<DComplex> &rowRef =
		    *((RecordFieldPtr<DComplex> *)row_fields_p[i]);
		const DComplex& val = fitsRef();
		(*rowRef) = DComplex (val.real() * scale + zero,
				      val.imag() * scale + zero);
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDComplex, AipsError);
		RecordFieldPtr<Array<DComplex> > &rowRef =
		    *((RecordFieldPtr<Array<DComplex> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		DComplex *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    const DComplex& val = fitsRef(n);
		    data[n] = DComplex (val.real() * scale + zero,
					val.imag() * scale + zero);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::ICOMPLEX:
	{
	    FitsField<IComplex> &fitsRef = 
		(FitsField<IComplex> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpDComplex) {
		RecordFieldPtr<DComplex> &rowRef =
		    *((RecordFieldPtr<DComplex> *)row_fields_p[i]);
		const IComplex& val = fitsRef();
		(*rowRef) = DComplex (val.real() * scale + zero,
				      val.imag() * scale + zero);
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDComplex, AipsError);
		RecordFieldPtr<Array<DComplex> > &rowRef =
		    *((RecordFieldPtr<Array<DComplex> > *)row_fields_p[i]);
		// does this need to be reshaped?
		int32_t n = raw_table_p->field(i).nelements();
		if (tdims_p[i] >= 0) {
		    (*rowRef).resize(shape);
		    n = shape.product();
		}
		bool deleteIt;
		DComplex *data = (*rowRef).getStorage(deleteIt);
		while (n) {
		    n--;
		    const IComplex& val = fitsRef(n);
		    data[n] = DComplex (val.real() * scale + zero,
					val.imag() * scale + zero);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::VADESC: 
	    {
		FitsVADesc thisva = va_p[i]();
		switch (vatypes_p[i]) {
		case FITS::LOGICAL:
		    {
			FitsLogical *vptr = (FitsLogical *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
			RecordFieldPtr<Array<bool> > &rowRef = 
			    *((RecordFieldPtr<Array<bool> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			bool *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::BIT:
		    {
			unsigned char *vptr = (unsigned char *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
			RecordFieldPtr<Array<bool> > &rowRef = 
			    *((RecordFieldPtr<Array<bool> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			bool *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			int32_t whichByte = n/8 - 1;
			if (n%8) whichByte++;
			unsigned char mask = 0200;
			while (n) {
			    n--;
			    if (n%8 == 7) whichByte--;
			    data[n] = (vptr[whichByte] & (mask >> n%8));
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::CHAR:
		    {
			// the sub string convention can't be used here
			char *vptr = (char *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpString, AipsError);
			RecordFieldPtr<String> &rowRef = 
			    *((RecordFieldPtr<String> *) row_fields_p[i]);
			uint32_t length = charLength(vptr, thisva.num());
			(*rowRef) = String(vptr, length);
		    }
		    break;
		case FITS::BYTE:
		    {
			unsigned char *vptr = (unsigned char *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			if (promoted_p[i]) {
			    DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
			    RecordFieldPtr<Array<double> > &rowRef =
				*((RecordFieldPtr<Array<double> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    double *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = double(vptr[n]);
			    }
			    (*rowRef).putStorage(data, deleteIt);
			    *rowRef *= scale;
			    *rowRef += zero;
			} else {
			    DebugAssert(field_types_p[i] == TpArrayUChar, AipsError);
			    RecordFieldPtr<Array<unsigned char> > &rowRef = 
				*((RecordFieldPtr<Array<unsigned char> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    unsigned char *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = vptr[n];
			    }
			    (*rowRef).putStorage(data, deleteIt);
			}
		    }
		    break;
		case FITS::SHORT:
		    {
			int16_t *vptr = (int16_t *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			if (promoted_p[i]) {
			    DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
			    RecordFieldPtr<Array<double> > &rowRef =
				*((RecordFieldPtr<Array<double> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    double *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = double(vptr[n]);
			    }
			    (*rowRef).putStorage(data, deleteIt);
			    *rowRef *= scale;
			    *rowRef += zero;
			} else {
			    DebugAssert(field_types_p[i] == TpArrayShort, AipsError);
			    RecordFieldPtr<Array<int16_t> > &rowRef = 
				*((RecordFieldPtr<Array<int16_t> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    int16_t *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = vptr[n];
			    }
			    (*rowRef).putStorage(data, deleteIt);
			}
		    }
		    break;
		case FITS::LONG:
		    {
			FitsLong *vptr = (FitsLong *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			if (promoted_p[i]) {
			    DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
			    RecordFieldPtr<Array<double> > &rowRef =
				*((RecordFieldPtr<Array<double> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    double *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = double(vptr[n]);
			    }
			    (*rowRef).putStorage(data, deleteIt);
			    *rowRef *= scale;
			    *rowRef += zero;
			} else {
			    DebugAssert(field_types_p[i] == TpArrayInt, AipsError);
			    RecordFieldPtr<Array<int32_t> > &rowRef = 
				*((RecordFieldPtr<Array<int32_t> > *) row_fields_p[i]);
			    // need to shape the output array
			    (*rowRef).resize(shape);
			    bool deleteIt;
			    int32_t *data = (*rowRef).getStorage(deleteIt);
			    int32_t n = shape.product();
			    while (n) {
				n--;
				data[n] = vptr[n];
			    }
			    (*rowRef).putStorage(data, deleteIt);
			}
		    }
		    break;
		case FITS::FLOAT:
		    {
			float *vptr = (float *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayFloat, AipsError);
			RecordFieldPtr<Array<float> > &rowRef = 
			    *((RecordFieldPtr<Array<float> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			float *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
			*rowRef *= float(scale);
			*rowRef += float(zero);
		    }
		    break;
		case FITS::DOUBLE:
		    {
			double *vptr = (double *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
			RecordFieldPtr<Array<double> > &rowRef = 
			    *((RecordFieldPtr<Array<double> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			double *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
			*rowRef *= scale;
			*rowRef += zero;
		    }
		    break;
		case FITS::COMPLEX:
		    {
			Complex *vptr = (Complex *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayComplex, AipsError);
			RecordFieldPtr<Array<Complex> > &rowRef = 
			    *((RecordFieldPtr<Array<Complex> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			Complex *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			while (n) {
			    n--;
			    const Complex& val = vptr[n];
			    data[n] = Complex (val.real() * scale + zero,
					       val.imag() * scale + zero);
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::DCOMPLEX:
		    {
			DComplex *vptr = (DComplex *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayDComplex, AipsError);
			RecordFieldPtr<Array<DComplex> > &rowRef = 
			    *((RecordFieldPtr<Array<DComplex> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(shape);
			bool deleteIt;
			DComplex *data = (*rowRef).getStorage(deleteIt);
			int32_t n = shape.product();
			while (n) {
			    n--;
			    const DComplex& val = vptr[n];
			    data[n] = DComplex (val.real() * scale + zero,
						val.imag() * scale + zero);
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		default:
		    throw(AipsError("FITSTable::fillrow() - unexpected variable array type"));
		    break;
		}
	    }
	    break;
        default:
	    throw(AipsError("FITSTable::fill_row() - unknown data type"));
	}
    }
}

void FITSTable::clear_self()
{
    row_nr_p = -1;

    delete raw_table_p;
    raw_table_p = 0;

    delete io_p;
    io_p = 0;

    uint32_t i;
    for (i=0; i < nfields_p; i++) {
	switch( field_types_p[i]) {
	case TpBool: 
	    delete (RecordFieldPtr<bool> *)row_fields_p[i];
	    break;
	case TpArrayBool:
	    delete (RecordFieldPtr<Array<bool> > *)row_fields_p[i]; 
	    break;
	case TpUChar:
	    delete (RecordFieldPtr<unsigned char> *)row_fields_p[i];
	    break;
	case TpArrayUChar:
	    delete (RecordFieldPtr<Array<unsigned char> > *)row_fields_p[i];
	    break;
	case TpShort:
	    delete (RecordFieldPtr<int16_t> *)row_fields_p[i];
	    break;
	case TpArrayShort:
	    delete (RecordFieldPtr<Array<int16_t> > *)row_fields_p[i];
	    break;
	case TpInt:
	    delete (RecordFieldPtr<int32_t> *)row_fields_p[i];
 	    break;
	case TpArrayInt:
	    delete (RecordFieldPtr<Array<int32_t> > *)row_fields_p[i];
	    break;
	case TpFloat:
	    delete (RecordFieldPtr<float> *)row_fields_p[i];
	    break;
	case TpArrayFloat:
	    delete (RecordFieldPtr<Array<float> > *)row_fields_p[i];
	    break;
	case TpDouble:
	    delete (RecordFieldPtr<double> *)row_fields_p[i];
	    break;
	case TpArrayDouble:
	    delete (RecordFieldPtr<Array<double> > *)row_fields_p[i];
	    break;
	case TpComplex:
	    delete (RecordFieldPtr<Complex> *)row_fields_p[i];
            break;
	case TpArrayComplex:
	    delete (RecordFieldPtr<Array<Complex> > *)row_fields_p[i];
	    break;
	case TpDComplex:
	    delete (RecordFieldPtr<DComplex> *)row_fields_p[i];
	    break;
	case TpArrayDComplex:
	    delete (RecordFieldPtr<Array<DComplex> > *)row_fields_p[i];
	    break;
	case TpString:
	    delete (RecordFieldPtr<String> *)row_fields_p[i];
	    break;
	case TpArrayString:
	    delete (RecordFieldPtr<Array<String> > *)row_fields_p[i];
	    break;
	default:
	    throw(AipsError("FITSTable::clear_self() - unknown field type"));
	}
	row_fields_p[i] = 0;
    }
    for (i=0;i<vatypes_p.nelements();i++) {
	if (vaptr_p[i]) {
	    switch (vatypes_p[i]) {
	    case FITS::LOGICAL: delete [] (FitsLogical *)vaptr_p[i]; break;
	    case FITS::BIT: delete [] (unsigned char *)vaptr_p[i]; break;
	    case FITS::BYTE: delete [] (unsigned char *)vaptr_p[i]; break;
	    case FITS::CHAR: delete [] (char *)vaptr_p[i]; break;
	    case FITS::SHORT: delete [] (int16_t *)vaptr_p[i]; break;
	    case FITS::LONG: delete [] (FitsLong *)vaptr_p[i]; break;
	    case FITS::FLOAT: delete [] (float *)vaptr_p[i]; break;
	    case FITS::DOUBLE: delete [] (double *)vaptr_p[i]; break;
	    case FITS::COMPLEX: delete [] (Complex *)vaptr_p[i]; break;
	    case FITS::DCOMPLEX: delete [] (DComplex *)vaptr_p[i]; break;
	    }
	}
    }
    nfields_p = 0;
    vatypes_p.resize(0);
    vaptr_p.resize(0);
    delete [] va_p;
    va_p = 0;
    delete [] theheap_p;
    theheap_p = 0;
    row_fields_p.resize(0);
    RecordDesc tmp;
    description_p = tmp;
    row_p.restructure(tmp);
    description_p = tmp;
    keywords_p.restructure(tmp);
    units_p.restructure(tmp);
    disps_p.restructure(tmp);
    nulls_p.restructure(tmp);
    subStrShapes_p.restructure(tmp);
    name_p = "";
    isValid_p = false;
}

const Record &FITSTable::currentRow() const
{
    return row_p;
}

void FITSTable::move(int32_t torow) {
    // we can only move within the table and only ahead
    // if this table contains no rows, moving is impossible, just return
    if (nrow() == 0) return;
    if (torow < rownr()) torow = rownr();
    if (torow >= int32_t(nrow())) torow = int32_t(nrow()) - 1;
    // if we are already there, just return
    if (torow == rownr()) return;

    // use the native FITS classes to move
    while (row_nr_p < torow) {
	row_nr_p++;
	if (!theheap_p) raw_table_p->read(1);
	else ++(*raw_table_p);
    }
    // and fill this row
    if (isValid()) fill_row();
}

bool FITSTable::pastEnd() const
{
    return ((isValid() && row_nr_p >= raw_table_p->nrows()) || ! isValid());
}

bool FITSTable::virtualColumns(const Vector<String>& keyNames)
{
    // move keyNames
    bool result = true;
    for (uint32_t i=0;i<keyNames.nelements();i++) {
	int32_t fieldNumber = keywords_p.fieldNumber(keyNames(i));
	if (fieldNumber >= 0) {
	    switch (keywords_p.type(fieldNumber)) {
	    case TpBool:
		{
		    bool value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpUChar:
		{
		    unsigned char value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpShort:
		{
		    int16_t value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpInt:
		{
		    int32_t value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpUInt:
		{
		    uint32_t value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpFloat:
		{
		    float value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpDouble:
		{
		    double value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpComplex:
		{
		    Complex value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpDComplex:
		{
		    DComplex value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpString:
		{
		    String value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    default:
		// this should never happen since the contents of
		// keywords_p should always be one of the previous types
		throw(AipsError("FITSTable::virtualColumns() invalid type in FITS keyword"));
		break;
	    }
	    row_p.setComment(keyNames(i), 
			     keywords_p.comment(keyNames(i)));
	    // it should now be safe to delete this keyword
	    keywords_p.removeField(keyNames(i));
	} else {
	    // not found in keywords_p
	    result = false;
	}
    }
    // reset description
    description_p = row_p.description();
    return result;
}

void FITSTable::reopenAtFirstHDU(const String &name) {
    delete io_p;
    io_p = 0;
    io_p = new FitsInput(name.chars(), FITS::Disk);
    AlwaysAssert(io_p, AipsError);
    // no need to check for err here, presumably
    io_p->skip_hdu();
}

} //# NAMESPACE CASACORE - END

