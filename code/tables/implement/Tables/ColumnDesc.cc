//# ColumnDesc.cc: Envelope class for description of a table column
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScaRecordColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/SubTabDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Containers/RecordDesc.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/DataType.h>
#include <aips/IO/AipsIO.h>
#include <aips/Tables/TableError.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


ColumnDesc::ColumnDesc (const BaseColumnDesc& cold)
: colPtr_p   (cold.clone()),
  allocated_p(True)
{}

ColumnDesc::ColumnDesc (const ColumnDesc& that)
: colPtr_p   (that.colPtr_p),
  allocated_p(True)
{
    if (colPtr_p != 0)
	colPtr_p = colPtr_p->clone();
}

ColumnDesc::ColumnDesc (BaseColumnDesc* bcdp)
: colPtr_p   (bcdp),
  allocated_p(False)
{}

ColumnDesc::~ColumnDesc()
{
    if (allocated_p) {
	delete colPtr_p;
    }
}


ColumnDesc& ColumnDesc::operator= (const ColumnDesc& that)
{
    if (this != &that) {
	if (allocated_p) {
	    delete colPtr_p;
	}
	colPtr_p = that.colPtr_p;
	if (colPtr_p != 0) {
	    colPtr_p = colPtr_p->clone();
	}
	allocated_p = True;
    }
    return *this;
}


Bool ColumnDesc::operator== (const ColumnDesc& that) const
{
    if (dataType() != that.dataType())
	return False;
    if (options() != that.options())
	return False;
    if (ndim() != that.ndim())
	return False;
    if (isScalar() && that.isScalar())
	return True;
    if (isArray() && that.isArray())
	return True;
    if (isTable() && that.isTable())
	return True;
    return False;
}

Bool ColumnDesc::operator!= (const ColumnDesc& that) const
{
    return (*this==that  ?  False : True);
}


Bool ColumnDesc::isFixedShape() const
{
    if (isScalar()) {
	return True;
    }
    if ((options() & ColumnDesc::FixedShape)  ==  ColumnDesc::FixedShape) {
	return True;
    }
    return False;
}


DataType ColumnDesc::trueDataType() const
{
    DataType dtype = dataType();
    if (! isArray()) {
        return dtype;
    }
    switch (dtype) {
    case TpBool:
	return TpArrayBool;
    case TpChar:
	return TpArrayChar;
    case TpUChar:
	return TpArrayUChar;
    case TpShort:
	return TpArrayShort;
    case TpUShort:
	return TpArrayUShort;
    case TpInt:
	return TpArrayInt;
    case TpUInt:
	return TpArrayUInt;
    case TpFloat:
	return TpArrayFloat;
    case TpDouble:
	return TpArrayDouble;
    case TpComplex:
	return TpArrayComplex;
    case TpDComplex:
	return TpArrayDComplex;
    case TpString:
	return TpArrayString;
    default:
	AlwaysAssert (False, AipsError);
    }
    return TpOther;
}


// Return the column name.
const String& ColumnDesc::name() const
    { return colPtr_p->name(); }


//# Put into AipsIO.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void ColumnDesc::putFile (AipsIO& ios, const String& parentTableName) const
{
    ios << (uInt)1;                  // class version 1
    //# First write the exact column type, then its data.
    ios << colPtr_p->className();
    colPtr_p->putFile (ios, parentTableName);
}

//# Get from AipsIO.
void ColumnDesc::getFile (AipsIO& ios, Bool tableIsWritable,
			  const String& parentTableName)
{
    //# First register all subclasses if not done yet.
    if (!registrationDone_p) {
	registerColumnDesc();
	registrationDone_p = True;
    }
    uInt version;
    ios >> version;
    String tp;
    ios >> tp;
    if (allocated_p) {
	delete colPtr_p;
    }
    allocated_p = True;
    colPtr_p = registerMap(tp)(tp);
    if (colPtr_p == 0) {
	throw (AllocError ("ColumnDesc(AipsIO&)",1));
    }
    colPtr_p->getFile (ios, tableIsWritable, parentTableName);
}


ostream& operator<< (ostream& ios, const ColumnDesc& cd)
{
    cd.show (ios);
    return ios;
}
void ColumnDesc::show() const
{
    show (cout);
}
void ColumnDesc::show (ostream& os) const
{
    colPtr_p->show (os);
    os << "   #keywords=" << keywordSet().nfields() << endl;
    os << keywordSet().description();
}


//# Initialize the static variables for the class registration.
Bool ColumnDesc::registrationDone_p = False;
SimpleOrderedMap<String, BaseColumnDesc* (*)(const String&)>
                      ColumnDesc::registerMap (ColumnDesc::unknownColumnDesc);

//# The default "ctor" function for unknown types.
BaseColumnDesc* ColumnDesc::unknownColumnDesc (const String& name)
{
    throw (TableUnknownDesc(name));
    return 0;
}






//# Register the "static AipsIO constructors" of all XColumnDesc classes.
void ColumnDesc::registerColumnDesc()
{
    ScalarColumnDesc<Bool>     scdb (registerMap);
    ScalarColumnDesc<uChar>    scduc(registerMap);
    ScalarColumnDesc<Short>    scds (registerMap);
    ScalarColumnDesc<uShort>   scdus(registerMap);
    ScalarColumnDesc<Int>      scdi (registerMap);
    ScalarColumnDesc<uInt>     scdui(registerMap);
    ScalarColumnDesc<float>    scdf (registerMap);
    ScalarColumnDesc<double>   scdd (registerMap);
    ScalarColumnDesc<Complex>  scdcx(registerMap);
    ScalarColumnDesc<DComplex> scddx(registerMap);
    ScalarColumnDesc<String>   scdst(registerMap);
    ScalarRecordColumnDesc     srcd (registerMap);

    ArrayColumnDesc<Bool>     acdb (registerMap);
    ArrayColumnDesc<uChar>    acduc(registerMap);
    ArrayColumnDesc<Short>    acds (registerMap);
    ArrayColumnDesc<uShort>   acdus(registerMap);
    ArrayColumnDesc<Int>      acdi (registerMap);
    ArrayColumnDesc<uInt>     acdui(registerMap);
    ArrayColumnDesc<float>    acdf (registerMap);
    ArrayColumnDesc<double>   acdd (registerMap);
    ArrayColumnDesc<Complex>  acdcx(registerMap);
    ArrayColumnDesc<DComplex> acddx(registerMap);
    ArrayColumnDesc<String>   acdst(registerMap);

    SubTableDesc std(registerMap);
}
