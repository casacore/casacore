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
    case FITS::ICOMPLEX:
    case FITS::COMPLEX:  return TpComplex;
    case FITS::DCOMPLEX: return TpDComplex;
    case FITS::STRING: //  return TpString; // FITS just has character arrays,
			 // not strings
    default:
	return TpOther;
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
    uInt count = 0;

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
	keywords.define(name,key->asString());
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
	// TpString is always a scalar column, it typically comes from
	// an array of FITS::CHAR
	if (type == TpString || 
	    (shape.nelements() == 1 && shape.product() == 1)) {
	    // Scalar
	    description.addField(colname, type);
	} else {
	    // Array
	    description.addField(colname, type, shape);
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
    : row_nr_p(-1), raw_table_p(0), io_p(0), row_fields_p(0), 
      hdu_nr_p(whichHDU), row_p(RecordInterface::Variable),
      field_types_p(0), allKeys_p(allKeywords)
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
    if (description_p.nfields() > 0) {
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
    if (isValid()) fill_row();
}

// What an ugly function! Simplify somehow!
void FITSTable::fill_row()
{
    row_nr_p++;
    if (row_nr_p >= raw_table_p->nrows()) {
	return; // Don't read past the end
    }

    // Use the native FITS classes
    raw_table_p->read(1);

    // And now fill it into the Row object.
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
	    cout << "not found : " << keyNames(i) << endl;
	}
    }
    // reset description
    description_p = row_p.description();
    return result;
}
