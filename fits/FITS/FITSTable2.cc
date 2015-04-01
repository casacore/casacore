//# FITSTable.h: Simplified interface to FITS tables with Casacore Look and Feel.
//# Copyright (C) 1995,1996,1997,1998,2000,2001,2003
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

#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/fits/FITS/FITSFieldCopier.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/OS/Path.h>

#include <casacore/casa/Arrays/Array.h>

#include <casacore/casa/sstream.h>

#include <casacore/casa/stdio.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt sizeofStringField(const RecordDesc &description, const Record &sizes,
		       uInt whichField)
{
    Int size = FITSTableWriter::DefaultMaxStringSize;
    AlwaysAssert(description.type(whichField) == TpString, AipsError);
    String name = description.name(whichField);
    Int which = sizes.fieldNumber(name);
    if (which >= 0) {
        sizes.get(which, size);
    }
    return size;
}


FITSTableWriter::FITSTableWriter(FitsOutput *file, 
				 const RecordDesc &description,
				 const Record &maxLengths,
				 uInt nrows,
				 const Record &extraKeywords,
				 const Record &units,
				 Bool freeOutput,
				 const Record &variableShapes)
  : delete_writer_p(freeOutput), writer_p(file), nrows_written_p(0), bintable_p(0),
    row_p(description), copiers_p(0)
{
    uInt nfields = description.nfields();
    Int sizeInBytes = 0;
    FitsKeywordList columns;
    uInt i;
    uInt thisColumn = 1;
    Block<Int> fieldMap(nfields,-1);
    Block<Int> tdimMap(nfields,-1);
    Block<Int> fieldSizes(nfields,0);
    for (i=0; i < nfields; i++) {
      const char *comment = 0;
      if (description.comment(i) != "") {
	  comment = description.comment(i).chars();
      }
      Bool hasVariableShape = 
	  (variableShapes.fieldNumber(description.name(i)) >= 0) &&
	  (maxLengths.fieldNumber(description.name(i)) >= 0);
      Int size = 1;
      String repeat = "1"; // Always write, even for scalars
      String code = "X";
      switch (description.type(i)) {
      case TpArrayBool: 
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpBool:
	  sizeInBytes += size*1;
	  code = "L";
	  break;

      case TpArrayUChar:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpUChar:
	  sizeInBytes += size*1;
	  code = "B";
	  break;

      case TpArrayShort:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpShort:
	  sizeInBytes += size*2;
	  code = "I";
	  break;

      case TpArrayInt:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpInt:
	  sizeInBytes += size*4;
	  code = "J";
	  break;

      case TpArrayFloat:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpFloat:
	  sizeInBytes += size*4;
	  code = "E";
	  break;

      case TpArrayDouble:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpDouble:
	  sizeInBytes += size*8;
	  code = "D";
	  break;

      case TpArrayComplex:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpComplex:
	  sizeInBytes += size*8;
	  code = "C";
	  break;

      case TpArrayDComplex:
	  if (hasVariableShape) {
	      size = maxLengths.asInt(description.name(i));
	  } else {
	      size = description.shape(i).product();
	  }
	  {
	      ostringstream buffer;
	      buffer << size;
	      repeat = String(buffer);
	  }
      case TpDComplex:
	  sizeInBytes += size*16;
	  code = "M";
	  break;

      case TpString:
	  {
	      uInt stringlen = sizeofStringField(description, maxLengths, i);
	      sizeInBytes += stringlen;
	      ostringstream buffer;
	      buffer << stringlen;
	      repeat = String(buffer);
	      code = "A";
	  }
	  break;

      case TpArrayString:
	  throw(AipsError("Arrays of strings are not yet supported"));
	  break;
      default:
	  throw(AipsError("Invalid type"));
      }

      columns.mk(thisColumn, FITS::TTYPE, description.name(i).chars(), comment);
      columns.mk(thisColumn, FITS::TFORM, (repeat + code).chars());
      IPosition shape = description.shape(i);
      if (shape.nelements() > 1 && !hasVariableShape) {
	  ostringstream buffer;
	  buffer << "(";
	  for (uInt j=0; j<shape.nelements(); j++) {
	      buffer << shape(j);
	      if (j != shape.nelements()-1) {buffer << ",";}
	  }
	  buffer << ")";
	  String s(buffer);
	  columns.mk(thisColumn, FITS::TDIM, s.chars());
      } 	  
      // see if there are units for this column
      if (units.isDefined(description.name(i)) &&
	  units.dataType(description.name(i)) == TpString) {
	  String unitString(units.asString(description.name(i)));
	  if (unitString != String(""))
	      columns.mk(thisColumn, FITS::TUNIT, unitString.chars());
      }
      fieldMap[i] = thisColumn-1;
      fieldSizes[i] = size;
      if (hasVariableShape) {
	  // add the TDIM column for the previous column
	  String sampleTdim = variableShapes.asString(description.name(i));
	  {
	      ostringstream buffer;
	      buffer << sampleTdim.length();
	      repeat = String(buffer);
	      sizeInBytes += sampleTdim.length();
	  }
	  code = "A";
	  char tdimColName[8];
	  sprintf(tdimColName,"TDIM%03i",thisColumn);
	  thisColumn++;
	  String tdimComment = "Shape of " + description.name(i) + " column.";
	  columns.mk(thisColumn, FITS::TTYPE, tdimColName, tdimComment.chars());
	  columns.mk(thisColumn, FITS::TFORM, (repeat + code).chars());
	  tdimMap[i] = thisColumn-1;
      }
      thisColumn++;
    }

    AlwaysAssert(sizeInBytes > 0, AipsError);

    // OK, now we can start filling in the header
    FitsKeywordList kw;
    kw.mk(FITS::XTENSION,"BINTABLE","Binary Table Extension");
    kw.mk(FITS::BITPIX,8,"Character Information");
    kw.mk(FITS::NAXIS,2,"Two-dimensional table");
    kw.mk(1,FITS::NAXIS,sizeInBytes,"Number of bytes per row");
    kw.mk(2,FITS::NAXIS,Int(nrows),"Number of rows");
    kw.mk(FITS::PCOUNT,0,"No random parameters");
    kw.mk(FITS::GCOUNT,1,"Only one group");
    kw.mk(FITS::TFIELDS,Int(thisColumn-1),"Number of columns");
    kw.spaces();
    // The following are too specific.
    //    kw.mk(FITS::EXTNAME,"SINGLE DISH","Single Dish FITS convention");
    //    kw.mk(FITS::EXTVER,1,"Version");
    kw.spaces();

    // Write the users keywords next
    uInt nkeys = extraKeywords.nfields();
    for (i=0; i<nkeys; i++) {
        String name = extraKeywords.name(i);
	const char *comment = 0;
	if (extraKeywords.comment(i) != "") {
	    comment = extraKeywords.comment(i).chars();
	}
	switch (extraKeywords.type(i)) {
	case TpBool:
	    {
	        Bool val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val, comment);
	  }
	  break;
	case TpInt:
	    {
	        Int val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val, comment);
	  }
	  break;
	case TpFloat:
	    {
	        Float val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val, comment);
	  }
	  break;
	case TpDouble:
	    {
	        Double val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val, comment);
	  }
	  break;
	case TpComplex:
	    {
	        Complex val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val.real(), val.imag(), comment);
	  }
	  break;
	case TpDComplex:
	    {
	        DComplex val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val.real(), val.imag(), comment);
	    }
	    break;
	case TpString:
	    {
	        String val;
		extraKeywords.get(i, val);
		kw.mk(name.chars(), val.chars(), comment);
	    }
	    break;
	default:
	    throw(AipsError("Invalid type"));
	    break;
	}
    }
    kw.spaces();

    // Now add in the column keywords
    FitsKeyword *k;
    columns.first();
    while (columns.next()) {
        k = new FitsKeyword(*columns.curr());
        kw.insert(*k);
    }
    kw.spaces();
    kw.end();

    bintable_p = new BinaryTableExtension(kw);

    AlwaysAssert(!bintable_p->err(), AipsError);
    bintable_p->write_hdr(*writer_p);

    // OK, now we can attach the copiers. We could make a template function
    // for this to avoid replicating code.
    copiers_p.resize(nfields);
    for (i=0; i<nfields; i++) {
	Int whichField = fieldMap[i];
	Int whichTdim = tdimMap[i];
        switch (description.type(i)) {
	case TpBool:
	  {
	    RORecordFieldPtr<Bool> *rptr = new RORecordFieldPtr<Bool>(row_p, i);
	    FitsField<FitsLogical> *fptr = new FitsField<FitsLogical>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Bool,FitsLogical>(rptr, fptr);
	  }
	    break;
	case TpUChar:
	  {
	    RORecordFieldPtr<uChar> *rptr = new RORecordFieldPtr<uChar>(row_p, i);
	    FitsField<uChar> *fptr = new FitsField<uChar>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<uChar,uChar>(rptr, fptr);
	  }
	    break;
	case TpShort:
	  {
	    RORecordFieldPtr<Short> *rptr = new RORecordFieldPtr<Short>(row_p, i);
	    FitsField<Short> *fptr = new FitsField<Short>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Short,Short>(rptr, fptr);
	  }
	    break;
	case TpInt:
	  {
	    RORecordFieldPtr<Int> *rptr = new RORecordFieldPtr<Int>(row_p, i);
	    FitsField<FitsLong> *fptr = new FitsField<FitsLong>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Int,FitsLong>(rptr, fptr);
	  }
	    break;
	case TpFloat:
	  {
	    RORecordFieldPtr<Float> *rptr = new RORecordFieldPtr<Float>(row_p, i);
	    FitsField<float> *fptr = new FitsField<float>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Float,float>(rptr, fptr);
	  }
	    break;
	case TpDouble:
	  {
	    RORecordFieldPtr<Double> *rptr = new RORecordFieldPtr<Double>(row_p, i);
	    FitsField<double> *fptr = new FitsField<double>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Double,double>(rptr, fptr);
	  }
	    break;
	case TpComplex:
	  {
	    RORecordFieldPtr<Complex> *rptr = new RORecordFieldPtr<Complex>(row_p, i);
	    FitsField<Complex> *fptr = new FitsField<Complex>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<Complex,Complex>(rptr, fptr);
	  }
	    break;
	case TpDComplex:
	  {
	    RORecordFieldPtr<DComplex> *rptr = new RORecordFieldPtr<DComplex>(row_p, i);
	    FitsField<DComplex> *fptr = new FitsField<DComplex>;
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new ScalarFITSFieldCopier<DComplex,DComplex>(rptr, fptr);
	  }
	    break;
	case TpString:
	  {
	    RORecordFieldPtr<String> *rptr = new RORecordFieldPtr<String>(row_p, i);
	    FitsField<char> *fptr = 
	      new FitsField<char>(sizeofStringField(description, 
						    maxLengths, i));
	    bintable_p->bind(whichField, *fptr);
	    copiers_p[i] = new StringFITSFieldCopier(rptr, fptr);
	  }
	    break;
	case TpArrayBool:
	  {
	    RORecordFieldPtr<Array<Bool> > *rptr = new RORecordFieldPtr<Array<Bool> >(row_p, i);
	    FitsField<FitsLogical> *fptr = 
	      new FitsField<FitsLogical>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Bool,FitsLogical> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Bool,FitsLogical> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayUChar:
	  {
	    RORecordFieldPtr<Array<uChar> > *rptr = new RORecordFieldPtr<Array<uChar> >(row_p, i);
	    FitsField<uChar> *fptr = 
	      new FitsField<uChar>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<uChar,uChar> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<uChar,uChar> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayShort:
	  {
	    RORecordFieldPtr<Array<Short> > *rptr = new RORecordFieldPtr<Array<Short> >(row_p, i);
	    FitsField<Short> *fptr = 
	      new FitsField<Short>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Short,Short> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Short,Short> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayInt:
	  {
	    RORecordFieldPtr<Array<Int> > *rptr = new RORecordFieldPtr<Array<Int> >(row_p, i);
	    FitsField<FitsLong> *fptr = 
	      new FitsField<FitsLong>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Int,FitsLong> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Int,FitsLong> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayFloat:
	  {
	    RORecordFieldPtr<Array<Float> > *rptr = new RORecordFieldPtr<Array<Float> >(row_p, i);
	    FitsField<Float> *fptr = 
	      new FitsField<Float>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Float,Float> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Float,Float> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayDouble:
	  {
	    RORecordFieldPtr<Array<Double> > *rptr = new RORecordFieldPtr<Array<Double> >(row_p, i);
	    FitsField<Double> *fptr = 
	      new FitsField<Double>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Double,Double> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Double,Double> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayComplex:
	  {
	    RORecordFieldPtr<Array<Complex> > *rptr = new RORecordFieldPtr<Array<Complex> >(row_p, i);
	    FitsField<Complex> *fptr = 
		new FitsField<Complex>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<Complex,Complex> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<Complex,Complex> (rptr, fptr);
	    }
	  }
	    break;
	case TpArrayDComplex:
	  {
	    RORecordFieldPtr<Array<DComplex> > *rptr = new RORecordFieldPtr<Array<DComplex> >(row_p, i);
	    FitsField<DComplex> *fptr = 
	      new FitsField<DComplex>(fieldSizes[i]);
	    bintable_p->bind(whichField, *fptr);
	    if (whichTdim >= 0) {
		FitsField<char> *tdirptr =
		    new FitsField<char>(variableShapes.asString(description.name(i)).length());
		bintable_p->bind(whichTdim, *tdirptr);
		copiers_p[i] = new VariableArrayFITSFieldCopier<DComplex,DComplex> (rptr, fptr, tdirptr);
	    } else {
		copiers_p[i] = new ArrayFITSFieldCopier<DComplex,DComplex> (rptr, fptr);
	    }
	  }
	    break;
	default:
	    AlwaysAssert(0, AipsError);
	}
	AlwaysAssert(copiers_p[i], AipsError);
    }
}

FITSTableWriter::~FITSTableWriter()
{
    if (delete_writer_p) {
        delete writer_p;
    }
    uInt nfields = row_p.description().nfields();
    for (uInt i=0; i<nfields; i++) {
        delete copiers_p[i];
    }
    copiers_p.resize(0);
    delete bintable_p;
}

void FITSTableWriter::write()
{
    uInt nfields = row_p.description().nfields();
    bintable_p->set_next(1);
    for (uInt i=0; i<nfields; i++) {
        copiers_p[i]->copyToFITS();
    }
    bintable_p->write(*writer_p);
}

FitsOutput *FITSTableWriter::makeWriter(const String &fileName)
{
    const char *name = Path(fileName).expandedName().chars();
    FitsOutput *file = new FitsOutput(name, FITS::Disk);
    FitsKeywordList st;

    st.mk(FITS::SIMPLE,True,"Standard FITS format");
    st.mk(FITS::BITPIX,8,"Character Information");
    st.mk(FITS::NAXIS,0,"No image data array present");
    st.mk(FITS::EXTEND,True,"Extension exists");
    st.spaces();
    st.comment("The first data is in the HDU following this one");
    st.spaces();
    st.end();
    PrimaryArray<unsigned char> hdu1(st);
    hdu1.write_hdr(*file);

    return file;
}

} //# NAMESPACE CASACORE - END

