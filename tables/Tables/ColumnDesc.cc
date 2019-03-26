//# ColumnDesc.cc: Envelope class for description of a table column
//# Copyright (C) 1994,1995,1996,1997,1998,2001,2016
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

#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SubTabDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Initialize the statics.
Mutex ColumnDesc::theirMutex;

  
ColumnDesc::ColumnDesc (const BaseColumnDesc& cold)
: colPtr_p   (cold.clone()),
  allocated_p(True)
{}

ColumnDesc::ColumnDesc (const ColumnDesc& that)
: colPtr_p   (that.colPtr_p),
  allocated_p(True)
{
  if (colPtr_p != 0) {
      colPtr_p = colPtr_p->clone();
  }
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
    return !(*this == that);
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
    case TpInt64:
	return TpArrayInt64;
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


AipsIO& operator<< (AipsIO& ios, const ColumnDesc& cd)
{
    cd.putFile (ios, TableAttr());
    return ios;
}

AipsIO& operator>> (AipsIO& ios, ColumnDesc& cd)
{
    cd.getFile(ios, TableAttr());
    return ios;
}

//# Put into AipsIO.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void ColumnDesc::putFile (AipsIO& ios, const TableAttr& parentAttr) const
{
    ios << (uInt)1;                  // class version 1
    //# First write the exact column type, then its data.
    ios << colPtr_p->className();
    colPtr_p->putFile (ios, parentAttr);
}

//# Get from AipsIO.
void ColumnDesc::getFile (AipsIO& ios, const TableAttr& parentAttr)
{
    uInt version;
    ios >> version;
    String tp;
    ios >> tp;
    if (allocated_p) {
	delete colPtr_p;
    }
    // If tp is not in the map, (tp, unknownColumnDesc) is added and called (throws).
    ColumnDesc::ColumnDescCtor* cdFunc = getCtor(tp);
    colPtr_p = (*cdFunc)(tp);
    allocated_p = True;
    colPtr_p->getFile (ios, parentAttr);
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
    if (colPtr_p) {
        colPtr_p->show (os);
        os << "   #keywords=" << keywordSet().nfields() << endl;
        os << keywordSet().description();
    } else {
        os << "ColumnDesc is empty" << endl;
    }
}


//# Register a mapping.
void ColumnDesc::registerCtor (const String& name,
                               ColumnDesc::ColumnDescCtor* func)
{
    ScopedMutexLock lock(theirMutex);
    getRegisterMap().insert (std::make_pair(name, func));
}

//# Get a ColumnDesc constructor.
//# Return default function if undefined.
ColumnDesc::ColumnDescCtor* ColumnDesc::getCtor (const String& name)
{
  std::map<String, ColumnDesc::ColumnDescCtor*>& regMap = getRegisterMap();
  std::map<String, ColumnDesc::ColumnDescCtor*>::iterator iter = regMap.find (name);
  if (iter == regMap.end()) {
    throw;
  }
  return iter->second;
}

std::map<String, ColumnDesc::ColumnDescCtor*>& ColumnDesc::getRegisterMap()
{
  static std::map<String, ColumnDesc::ColumnDescCtor*> regMap(initRegisterMap());
  return regMap;
}

//# Register the main "static constructors" of all XColumnDesc classes.
// No locking since private and only called by ctor of static member init.
std::map<String, ColumnDesc::ColumnDescCtor*> ColumnDesc::initRegisterMap()
{
  std::map<String, ColumnDesc::ColumnDescCtor*> regMap;

  ScalarColumnDesc<Bool>     scdb("x");
  regMap.insert (std::make_pair(scdb.className(), &scdb.makeDesc));
  ScalarColumnDesc<uChar>    scduc("x");
  regMap.insert (std::make_pair(scduc.className(), &scduc.makeDesc));
  ScalarColumnDesc<Short>    scds("x");
  regMap.insert (std::make_pair(scds.className(), &scds.makeDesc));
  ScalarColumnDesc<uShort>   scdus("x");
  regMap.insert (std::make_pair(scdus.className(), &scdus.makeDesc));
  ScalarColumnDesc<Int>      scdi("x");
  regMap.insert (std::make_pair(scdi.className(), &scdi.makeDesc));
  ScalarColumnDesc<uInt>     scdui("x");
  regMap.insert (std::make_pair(scdui.className(), &scdui.makeDesc));
  ScalarColumnDesc<Int64>    scdi64("x");
  regMap.insert (std::make_pair(scdi64.className(), &scdi64.makeDesc));
  ScalarColumnDesc<float>    scdf("x");
  regMap.insert (std::make_pair(scdf.className(), &scdf.makeDesc));
  ScalarColumnDesc<double>   scdd("x");
  regMap.insert (std::make_pair(scdd.className(), &scdd.makeDesc));
  ScalarColumnDesc<Complex>  scdcx("x");
  regMap.insert (std::make_pair(scdcx.className(), &scdcx.makeDesc));
  ScalarColumnDesc<DComplex> scddx("x");
  regMap.insert (std::make_pair(scddx.className(), &scddx.makeDesc));
  ScalarColumnDesc<String>   scdst("x");
  regMap.insert (std::make_pair(scdst.className(), &scdst.makeDesc));

  ScalarRecordColumnDesc     srcd ("x");
  regMap.insert (std::make_pair(srcd.className(), &srcd.makeDesc));

  ArrayColumnDesc<Bool>     acdb("x");
  regMap.insert (std::make_pair(acdb.className(), &acdb.makeDesc));
  ArrayColumnDesc<uChar>    acduc("x");
  regMap.insert (std::make_pair(acduc.className(), &acduc.makeDesc));
  ArrayColumnDesc<Short>    acds("x");
  regMap.insert (std::make_pair(acds.className(), &acds.makeDesc));
  ArrayColumnDesc<uShort>   acdus("x");
  regMap.insert (std::make_pair(acdus.className(), &acdus.makeDesc));
  ArrayColumnDesc<Int>      acdi("x");
  regMap.insert (std::make_pair(acdi.className(), &acdi.makeDesc));
  ArrayColumnDesc<uInt>     acdui("x");
  regMap.insert (std::make_pair(acdui.className(), &acdui.makeDesc));
  ArrayColumnDesc<Int64>    acdi64("x");
  regMap.insert (std::make_pair(acdi64.className(), &acdi64.makeDesc));
  ArrayColumnDesc<float>    acdf("x");
  regMap.insert (std::make_pair(acdf.className(), &acdf.makeDesc));
  ArrayColumnDesc<double>   acdd("x");
  regMap.insert (std::make_pair(acdd.className(), &acdd.makeDesc));
  ArrayColumnDesc<Complex>  acdcx("x");
  regMap.insert (std::make_pair(acdcx.className(), &acdcx.makeDesc));
  ArrayColumnDesc<DComplex> acddx("x");
  regMap.insert (std::make_pair(acddx.className(), &acddx.makeDesc));
  ArrayColumnDesc<String>   acdst("x");
  regMap.insert (std::make_pair(acdst.className(), &acdst.makeDesc));

  SubTableDesc std("x", "", TableDesc());
  regMap.insert (std::make_pair(std.className(), &std.makeDesc));

  return regMap;
}

} //# NAMESPACE CASACORE - END
