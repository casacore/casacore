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
//# $Id$

#include <trial/FITS/FITSTable.h>

#include <aips/Arrays/Vector.h>
#include <aips/Containers/RecordField.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/FITS/fits.h>
#include <aips/FITS/fitsio.h>
#include <aips/FITS/hdu.h>

#include <aips/Arrays/Array.h>

#include <strstream.h>

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

FITSTabular::~FITSTabular()
{
    // Nothing
}

TableRecord FITSTabular::keywordsFromHDU(HeaderDataUnit &hdu,
					 Bool allKeywords)
{
    // Setup the keywords.
    //     First, delete the old ones
    TableRecord keywords;
    //     Now add in all the keywords from this HDU
    hdu.firstkw();
    const FitsKeyword *key = hdu.currkw();
    Bool noValue = False;

    String name;
    while (key) {
      name = key->name();
      // skip certain keywords if allKeywords is not True
      if (!allKeywords && key->isreserved() && 
	  (key->kw().name() == FITS::TTYPE ||
	   key->kw().name() == FITS::TFORM ||
	   key->kw().name() == FITS::TUNIT)) {
	key = hdu.nextkw();
	continue;
      }
      if (key->isindexed()) {
	ostrstream num;
	num << key->index();
	name += String(num);
      }

      switch (key->type()) {
      case FITS::LOGICAL : 
	keywords.define(name,key->asBool());
	break;
      case FITS::STRING : 
	// need to remvoe any trailing spaces
	{
	    const char * cptr = key->asString();
	    uInt length = key->valStrlen();
	    while (length > 0 && 
		   (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
	      length--;
	    }
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
	noValue = True;
	break;
      default:
	throw(AipsError("FITSTable::reopen() - unknown keyword type"
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
    uInt ncol = hdu.ncols();

    Regex trailing(" *$"); // trailing blanks
    IPosition shape;
    for (uInt i=0; i < ncol; i++) {
	DataType type = fitsDataType(hdu.field(i).fieldtype());
	shape.resize(hdu.field(i).dims());
	for (uInt j=0; j<shape.nelements(); j++) {
	    shape(j) = hdu.field(i).dim(j);
	}
	String colname(hdu.ttype(i));
	colname = colname.before(trailing);
	// watch for VADESC columns
	if (hdu.field(i).fieldtype() == FITS::VADESC) {
	    // variable array descriptor
	    FITS::ValueType ftype;
	    int maxelem;
	    FITS::parse_vatform(hdu.tform(i), ftype, maxelem);
	    type = fitsDataType(ftype);
	    shape.resize(0);
	}
	// TpString is always a scalar column, it typically comes from
	// an array of FITS::CHAR
	if (type == TpString || 
	    (shape.nelements() == 1 && shape.product() == 1)) {
	    // Scalar
	    description.addField(colname, type);
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

Record FITSTabular::unitsFromHDU(BinaryTableExtension &hdu)
{
    Record units;
    uInt ncol = hdu.ncols();

    Regex trailing(" *$"); // trailing blanks
    for (uInt i=0; i < ncol; i++) {
	String colname(hdu.ttype(i));
	colname = colname.before(trailing);
	String unitval(hdu.tunit(i));
	unitval = unitval.before(trailing);
	if (!unitval.empty()) units.define(colname, unitval);
    }
    return units;
}

FITSTable::FITSTable(const String &fileName, uInt whichHDU, 
		     Bool allKeywords)
    : hdu_nr_p(whichHDU), row_nr_p(-1), raw_table_p(0), io_p(0), 
      row_p(RecordInterface::Variable), allKeys_p(allKeywords), 
      row_fields_p(0), field_types_p(0), vatypes_p(0), vaptr_p(0),
      va_p(0), theheap_p(0)
{
    isValid_p = reopen(fileName);
}

FITSTable::~FITSTable()
{
    clear_self();
}

Bool FITSTable::reopen(const String &fileName)
{
    clear_self();

    io_p = new FitsInput(fileName.chars(), FITS::Disk);
    if (io_p->err()) {
	return False;
    }

    for (uInt i=0; i < hdu_nr_p && !io_p->err(); i++) {
	io_p->skip_hdu();
    }

    if (io_p->err()) {
	return False;
    }
    
    // OK; we have a valid HDU
    if (io_p->hdutype() == FITS::BinaryTableHDU) {
	raw_table_p = new BinaryTableExtension(*io_p);
    } else if (io_p->hdutype() == FITS::AsciiTableHDU) {
	raw_table_p = new AsciiTableExtension(*io_p);
    } else {
	return False;
    }
    AlwaysAssert(raw_table_p, AipsError);
    keywords_p = FITSTabular::keywordsFromHDU(*raw_table_p, allKeys_p);
    description_p = FITSTabular::descriptionFromHDU(*raw_table_p);
    units_p = FITSTabular::unitsFromHDU(*raw_table_p);

    row_p.restructure(description_p);

    // read the heap (and the rest of the table, since this is a
    // sequential access file only) if one is present
    if (raw_table_p->pcount()) {
	raw_table_p->read(raw_table_p->nrows());
	if (raw_table_p->notnull(raw_table_p->theap())) {
	    uInt heapOffset = raw_table_p->theap() - 
		raw_table_p->rowsize()*raw_table_p->nrows();
	    // skip to the start of the heap
	    // I don't see any way except to read these bogus bytes
	    Block<Char> junk(heapOffset);
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
    uInt n = description_p.nfields();
    row_fields_p.resize(n);
    field_types_p.resize(n);
    for (i=0; i < n; i++) {
	switch( description_p.type(i)) {
	case TpBool: 
	    row_fields_p[i] = new RecordFieldPtr<Bool>(row_p, i);
	    break;
	case TpArrayBool:
	    row_fields_p[i] = new RecordFieldPtr<Array<Bool> >(row_p, i);
	    break;
	case TpUChar:
	    row_fields_p[i] = new RecordFieldPtr<uChar>(row_p, i);
	    break;
	case TpArrayUChar:
	    row_fields_p[i] = new RecordFieldPtr<Array<uChar> >(row_p, i);
	    break;
	case TpShort:
	    row_fields_p[i] = new RecordFieldPtr<Short>(row_p, i);
	    break;
	case TpArrayShort:
	    row_fields_p[i] = new RecordFieldPtr<Array<Short> >(row_p, i);
	    break;
	case TpInt:
	    row_fields_p[i] = new RecordFieldPtr<Int>(row_p, i);
 	    break;
	case TpArrayInt:
	    row_fields_p[i] = new RecordFieldPtr<Array<Int> >(row_p, i);
	    break;
	case TpFloat:
	    row_fields_p[i] = new RecordFieldPtr<Float>(row_p, i);
	    break;
	case TpArrayFloat:
	    row_fields_p[i] = new RecordFieldPtr<Array<Float> >(row_p, i);
	    break;
	case TpDouble:
	    row_fields_p[i] = new RecordFieldPtr<Double>(row_p, i);
	    break;
	case TpArrayDouble:
	    row_fields_p[i] = new RecordFieldPtr<Array<Double> >(row_p, i);
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
	vatypes_p.resize(raw_table_p->ncols());
	vaptr_p.resize(raw_table_p->ncols());
	va_p = new VADescFitsField [raw_table_p->ncols()];
	AlwaysAssert(va_p, AipsError);
	for (i=0;i<uInt(raw_table_p->ncols());i++) {
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
			Int nbytes = maxsize / 8;
			if (maxsize % 8) nbytes++;
			maxsize = nbytes;
		    }
		    // fall through to BYTE for actual allocation
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
	}
    }
	
    if (description_p.nfields() > 0 && raw_table_p->nrows()) {
	fill_row();
    }
    isValid_p = True;

    return True;
}


Bool FITSTable::isValid() const
{
    return isValid_p;
}


const TableRecord &FITSTable::keywords() const
{
    return keywords_p;
}

const RecordDesc &FITSTable::description() const
{
    return description_p;
}

const Record &FITSTable::units() const
{
    return units_p;
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
    uInt n = row_fields_p.nelements();
    for (uInt i=0; i < n; i++) {
	switch (raw_table_p->field(i).fieldtype()) {
	case FITS::LOGICAL:
	{
	    FitsField<FitsLogical> &fitsRef = 
		(FitsField<FitsLogical> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpBool) {
		RecordFieldPtr<Bool> &rowRef =
		    *((RecordFieldPtr<Bool> *)row_fields_p[i]);
		*rowRef = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
		RecordFieldPtr<Array<Bool> > &rowRef =
		    *((RecordFieldPtr<Array<Bool> > *)row_fields_p[i]);
		Bool deleteIt;
		Bool *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
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
		RecordFieldPtr<Bool> &rowRef =
		    *((RecordFieldPtr<Bool> *)row_fields_p[i]);
		(*rowRef) = ToBool(int(fitsRef()));
	    } else {
		DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
		RecordFieldPtr<Array<Bool> > &rowRef =
		    *((RecordFieldPtr<Array<Bool> > *)row_fields_p[i]);
		Bool deleteIt;
		Bool *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = ToBool(int(fitsRef(n)));
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	// FITS::CHAR are intepreted to be scalar String values
	case FITS::CHAR:
	{
	    DebugAssert(field_types_p[i] == TpString, AipsError);
	    FitsField<char> &fitsRef = 
		(FitsField<char> &)(raw_table_p->field(i));
	    RecordFieldPtr<String> &rowRef =
		*((RecordFieldPtr<String> *)row_fields_p[i]);
	    // look for the true end of the string
	    char * cptr = (char *)fitsRef.data();
	    uInt length = fitsRef.nelements();
	    while (length > 0 && 
		   (cptr[length-1] == '\0' || cptr[length-1] == ' ')) {
		length--;
	    }
	    (*rowRef) = String((char *)fitsRef.data(), length);
	}
	break;
	case FITS::BYTE:
	{
	    FitsField<unsigned char> &fitsRef = 
		(FitsField<unsigned char> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpUChar) {
		RecordFieldPtr<uChar> &rowRef =
		    *((RecordFieldPtr<uChar> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayUChar, AipsError);
		RecordFieldPtr<Array<uChar> > &rowRef =
		    *((RecordFieldPtr<Array<uChar> > *)row_fields_p[i]);
		Bool deleteIt;
		uChar *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::SHORT:
	{
	    FitsField<short> &fitsRef = 
		(FitsField<short> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpShort) {
		RecordFieldPtr<Short> &rowRef =
		    *((RecordFieldPtr<Short> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayShort, AipsError);
		RecordFieldPtr<Array<Short> > &rowRef =
		    *((RecordFieldPtr<Array<Short> > *)row_fields_p[i]);
		Bool deleteIt;
		Short *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::LONG:
	{
	    FitsField<FitsLong> &fitsRef = 
		(FitsField<FitsLong> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpInt) {
		RecordFieldPtr<Int> &rowRef =
		    *((RecordFieldPtr<Int> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayInt, AipsError);
		RecordFieldPtr<Array<Int> > &rowRef =
		    *((RecordFieldPtr<Array<Int> > *)row_fields_p[i]);
		Bool deleteIt;
		Int *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::FLOAT:
	{
	    FitsField<float> &fitsRef = 
		(FitsField<float> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpFloat) {
		RecordFieldPtr<Float> &rowRef =
		    *((RecordFieldPtr<Float> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayFloat, AipsError);
		RecordFieldPtr<Array<Float> > &rowRef =
		    *((RecordFieldPtr<Array<Float> > *)row_fields_p[i]);
		Bool deleteIt;
		Float *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
	    }
	}
	break;
	case FITS::DOUBLE:
	{
	    FitsField<double> &fitsRef = 
		(FitsField<double> &)(raw_table_p->field(i));
	    if (field_types_p[i] == TpDouble) {
		RecordFieldPtr<Double> &rowRef =
		    *((RecordFieldPtr<Double> *)row_fields_p[i]);
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
		RecordFieldPtr<Array<Double> > &rowRef =
		    *((RecordFieldPtr<Array<Double> > *)row_fields_p[i]);
		Bool deleteIt;
		Double *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
		}
		(*rowRef).putStorage(data, deleteIt);
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
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayComplex, AipsError);
		RecordFieldPtr<Array<Complex> > &rowRef =
		    *((RecordFieldPtr<Array<Complex> > *)row_fields_p[i]);
		Bool deleteIt;
		Complex *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
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
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDComplex, AipsError);
		RecordFieldPtr<Array<DComplex> > &rowRef =
		    *((RecordFieldPtr<Array<DComplex> > *)row_fields_p[i]);
		Bool deleteIt;
		DComplex *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
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
		(*rowRef) = fitsRef();
	    } else {
		DebugAssert(field_types_p[i] == TpArrayDComplex, AipsError);
		RecordFieldPtr<Array<DComplex> > &rowRef =
		    *((RecordFieldPtr<Array<DComplex> > *)row_fields_p[i]);
		Bool deleteIt;
		DComplex *data = (*rowRef).getStorage(deleteIt);
		Int n = raw_table_p->field(i).nelements();
		while (n) {
		    n--;
		    data[n] = fitsRef(n);
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
			RecordFieldPtr<Array<Bool> > &rowRef = 
			    *((RecordFieldPtr<Array<Bool> > *) row_fields_p[i]);\
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Bool *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::BIT:
		    {
			uChar *vptr = (uChar *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayBool, AipsError);
			RecordFieldPtr<Array<Bool> > &rowRef = 
			    *((RecordFieldPtr<Array<Bool> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Bool *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			Int whichByte = n/8 - 1;
			if (n%8) whichByte++;
			uChar mask = 0200;
			while (n) {
			    n--;
			    if (n%8 == 7) whichByte--;
			    data[n] = ToBool(vptr[whichByte] & (mask >> n%8));
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::CHAR:
		    {
			Char *vptr = (Char *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpString, AipsError);
			RecordFieldPtr<String> &rowRef = 
			    *((RecordFieldPtr<String> *) row_fields_p[i]);
			// look for the true end of the string
			uInt length = thisva.num();
			while (length > 0 && 
			       (vptr[length-1] == '\0' || vptr[length-1] == ' ')) {
			    length--;
			}
			(*rowRef) = String(vptr, length);
		    }
		    break;
		case FITS::BYTE:
		    {
			uChar *vptr = (uChar *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayUChar, AipsError);
			RecordFieldPtr<Array<uChar> > &rowRef = 
			    *((RecordFieldPtr<Array<uChar> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			uChar *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::SHORT:
		    {
			Short *vptr = (Short *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayShort, AipsError);
			RecordFieldPtr<Array<Short> > &rowRef = 
			    *((RecordFieldPtr<Array<Short> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Short *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::LONG:
		    {
			FitsLong *vptr = (FitsLong *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayInt, AipsError);
			RecordFieldPtr<Array<Int> > &rowRef = 
			    *((RecordFieldPtr<Array<Int> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Int *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::FLOAT:
		    {
			Float *vptr = (Float *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayFloat, AipsError);
			RecordFieldPtr<Array<Float> > &rowRef = 
			    *((RecordFieldPtr<Array<Float> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Float *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
		    }
		    break;
		case FITS::DOUBLE:
		    {
			Double *vptr = (Double *)(vaptr_p[i]);
			FITS::f2l(vptr, (void *)(theheap_p + thisva.offset()),
				  thisva.num());
			DebugAssert(field_types_p[i] == TpArrayDouble, AipsError);
			RecordFieldPtr<Array<Double> > &rowRef = 
			    *((RecordFieldPtr<Array<Double> > *) row_fields_p[i]);
			// need to shape the output array
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Double *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
			}
			(*rowRef).putStorage(data, deleteIt);
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
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			Complex *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
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
			(*rowRef).resize(IPosition(1,thisva.num()));
			Bool deleteIt;
			DComplex *data = (*rowRef).getStorage(deleteIt);
			Int n = thisva.num();
			while (n) {
			    n--;
			    data[n] = vptr[n];
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

    uInt n = row_fields_p.nelements();
    for (uInt i=0; i < n; i++) {
	switch( field_types_p[i]) {
	case TpBool: 
	    delete (RecordFieldPtr<Bool> *)row_fields_p[i];
	    break;
	case TpArrayBool:
	    delete (RecordFieldPtr<Array<Bool> > *)row_fields_p[i]; 
	    break;
	case TpUChar:
	    delete (RecordFieldPtr<uChar> *)row_fields_p[i];
	    break;
	case TpArrayUChar:
	    delete (RecordFieldPtr<Array<uChar> > *)row_fields_p[i];
	    break;
	case TpShort:
	    delete (RecordFieldPtr<Short> *)row_fields_p[i];
	    break;
	case TpArrayShort:
	    delete (RecordFieldPtr<Array<Short> > *)row_fields_p[i];
	    break;
	case TpInt:
	    delete (RecordFieldPtr<Int> *)row_fields_p[i];
 	    break;
	case TpArrayInt:
	    delete (RecordFieldPtr<Array<Int> > *)row_fields_p[i];
	    break;
	case TpFloat:
	    delete (RecordFieldPtr<Float> *)row_fields_p[i];
	    break;
	case TpArrayFloat:
	    delete (RecordFieldPtr<Array<Float> > *)row_fields_p[i];
	    break;
	case TpDouble:
	    delete (RecordFieldPtr<Double> *)row_fields_p[i];
	    break;
	case TpArrayDouble:
	    delete (RecordFieldPtr<Array<Double> > *)row_fields_p[i];
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
	default:
	    throw(AipsError("FITSTable::clear_self() - unknown field type"));
	}
	row_fields_p[i] = 0;
    }
    for (i=0;i<vatypes_p.nelements();i++) {
	if (vaptr_p[i]) {
	    switch (vatypes_p[i]) {
	    case FITS::LOGICAL: delete [] (FitsLogical *)vaptr_p[i]; break;
	    case FITS::BIT: delete [] (uChar *)vaptr_p[i]; break;
	    case FITS::BYTE: delete [] (uChar *)vaptr_p[i]; break;
	    case FITS::CHAR: delete [] (Char *)vaptr_p[i]; break;
	    case FITS::SHORT: delete [] (Short *)vaptr_p[i]; break;
	    case FITS::LONG: delete [] (FitsLong *)vaptr_p[i]; break;
	    case FITS::FLOAT: delete [] (Float *)vaptr_p[i]; break;
	    case FITS::DOUBLE: delete [] (Double *)vaptr_p[i]; break;
	    case FITS::COMPLEX: delete [] (Complex *)vaptr_p[i]; break;
	    case FITS::DCOMPLEX: delete [] (DComplex *)vaptr_p[i]; break;
	    }
	}
    }
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
    name_p = "";
    isValid_p = False;
}

const Record &FITSTable::currentRow() const
{
    return row_p;
}

Bool FITSTable::pastEnd() const
{
    return ToBool((isValid() && row_nr_p >= raw_table_p->nrows()) || ! isValid());
}

Bool FITSTable::virtualColumns(const Vector<String>& keyNames)
{
    // move keyNames
    Bool result = True;
    for (uInt i=0;i<keyNames.nelements();i++) {
	Int fieldNumber = keywords_p.fieldNumber(keyNames(i));
	if (fieldNumber >= 0) {
	    switch (keywords_p.type(fieldNumber)) {
	    case TpBool:
		{
		    Bool value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpUChar:
		{
		    uChar value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpShort:
		{
		    Short value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpInt:
		{
		    Int value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpUInt:
		{
		    uInt value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpFloat:
		{
		    Float value;
		    keywords_p.get(keyNames(i), value);
		    row_p.define(keyNames(i), value);
		}
		break;
	    case TpDouble:
		{
		    Double value;
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
	    result = False;
	}
    }
    // reset description
    description_p = row_p.description();
    return result;
}
