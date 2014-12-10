//# SerialHelper: a helper class for (un)serializing a Function object
//# Copyright (C) 2002
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
//#
//# $Id$
#include <casacore/scimath/Functionals/SerialHelper.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

const String SerialHelper::FUNCTYPE("functype");
const String SerialHelper::gtype[] = {
    "Bool", "Byte", "Short", "Int", "Float", 
    "Double", "Complex", "DComplex", "String"
};

Bool SerialHelper::getFuncType(String& ftype) const 
{
    if (! gr.isDefined(FUNCTYPE)) return False;

    try {
       ftype = gr.asString(RecordFieldId(FUNCTYPE));
       if(!ftype.size() ){
	   throw InvalidSerializationError("Empty value for functype field");
        }
    } catch (AipsError (x)) {
	throw InvalidSerializationError("Wrong type for functype field");
    }
    return True;
}

void SerialHelper::checkFuncType(const String& ftype) const 
{
    String thistype;
    if (! getFuncType(thistype))
	throw InvalidSerializationError("No functype field defined");
    if (ftype != thistype) 
	throw InvalidSerializationError(String("Wrong functype (need ") +
					     ftype + ", found " + thistype);
}

template <> void getArrayVal<Bool>(Bool& val,     Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpBool :
           val = gr.asBool(RecordFieldId(name));
           break;
        case TpArrayBool :
           {
           Array<Bool> tmp = gr.asArrayBool(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpBool," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<Short>(Short& val,    Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpShort :
           val = gr.asShort(RecordFieldId(name));
           break;
        case TpArrayShort :
           {
           Array<Short> tmp = gr.asArrayShort(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpShort," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<Int>(Int& val,      Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpInt :
           val = gr.asInt(RecordFieldId(name));
           break;
        case TpArrayInt :
           {
           Array<Int> tmp = gr.asArrayInt(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpInt," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<Float>(Float& val,    Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpFloat :
           val = gr.asFloat(RecordFieldId(name));
           break;
        case TpArrayFloat :
           {
           Array<Float> tmp = gr.asArrayFloat(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpFloat," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<Double>(Double& val,   Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpDouble :
           val = gr.asDouble(RecordFieldId(name));
           break;
        case TpArrayDouble :
           {
           Array<Double> tmp = gr.asArrayDouble(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpDouble," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<Complex>(Complex& val,  Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpComplex :
           val = gr.asComplex(RecordFieldId(name));
           break;
        case TpArrayComplex :
           {
           Array<Complex> tmp = gr.asArrayComplex(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpComplex," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<DComplex>(DComplex& val, Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpDComplex :
           val = gr.asDComplex(RecordFieldId(name));
           break;
        case TpArrayDComplex :
           {
           Array<DComplex> tmp = gr.asArrayDComplex(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpDComplex," + 
					     " found record)");
           break;
    }
}
template <> void getArrayVal<String>(String& val,   Int, const Record& gr, 
			                  const String& name, uInt index)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    switch(gr.dataType(RecordFieldId(name))){
        case TpString :
           val = gr.asString(RecordFieldId(name));
           break;
        case TpArrayString :
           {
           Array<String> tmp = gr.asArrayString(RecordFieldId(name));
           val = tmp(IPosition(tmp.nelements(), index));
           }
           break;
        default :
	   throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need TpString," + 
					     " found record)");
           break;
    }
}

template <> void getArray<Bool>(Array<Bool>& val,     Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayBool) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array bool," + 
					     " found record)");
    val = gr.asArrayBool(RecordFieldId(name));
}
template <> void getArray<Short>(Array<Short>& val,    Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayShort) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array short," + 
					     " found record)");
    val = gr.asArrayShort(RecordFieldId(name));
}
template <> void getArray<Int>(Array<Int>& val,      Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayInt) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array int," + 
					     " found record)");
    val = gr.asArrayInt(RecordFieldId(name));
}
template <> void getArray<Float>(Array<Float>& val,    Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayFloat) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array float," + 
					     " found record)");
    val = gr.asArrayFloat(RecordFieldId(name));
}
template <> void getArray<Double>(Array<Double>& val,   Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayDouble) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array double," + 
					     " found record)");
    val = gr.asArrayDouble(RecordFieldId(name));
}
template <> void getArray<Complex>(Array<Complex>& val,  Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayComplex) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array complex," + 
					     " found record)");
    val = gr.asArrayComplex(RecordFieldId(name));
}
template <> void getArray<DComplex>(Array<DComplex>& val, Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayDComplex) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array dcomplex," + 
					     " found record)");
    val = gr.asArrayDComplex(RecordFieldId(name));
}
template <> void getArray<String>(Array<String>& val,   Int, const Record& gr, 
			                      const String& name)
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpArrayString) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array string," + 
					     " found record)");
    val = gr.asArrayString(RecordFieldId(name));
}


void SerialHelper::get(Bool &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtBOOL, gr, name, index);
}

void SerialHelper::get(String &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtSTRING, gr, name, index);
}

//  void SerialHelper::get(Byte &val, const String& name, uInt index) const
//  {
//      getArrayVal(val, Array::BYTE, gr, name, index);
//  }

void SerialHelper::get(Short &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtSHORT, gr, name, index);
}

void SerialHelper::get(Int &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtINT, gr, name, index);
}

void SerialHelper::get(Float &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtFLOAT, gr, name, index);
}

void SerialHelper::get(Double &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtDOUBLE, gr, name, index);
}

void SerialHelper::get(Complex &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtCOMPLEX, gr, name, index);
}

void SerialHelper::get(DComplex &val, const String& name, uInt index) const
{
    getArrayVal(val, SerialHelper::shtDCOMPLEX, gr, name, index);
}

void SerialHelper::get(Array<Bool> &val, const String& name) const
{
    getArray(val, SerialHelper::shtBOOL, gr, name);
}

void SerialHelper::get(Array<Short> &val, const String& name) const
{
    getArray(val, SerialHelper::shtSHORT, gr, name);
}

void SerialHelper::get(Array<Int> &val, const String& name) const
{
    getArray(val, SerialHelper::shtINT, gr, name);
}

void SerialHelper::get(Array<Float> &val, const String& name) const
{
    getArray(val, SerialHelper::shtFLOAT, gr, name);
}

void SerialHelper::get(Array<Double> &val, const String& name) const
{
    getArray(val, SerialHelper::shtDOUBLE, gr, name);
}

void SerialHelper::get(Array<Complex> &val, const String& name) const
{
    getArray(val, SerialHelper::shtCOMPLEX, gr, name);
}

void SerialHelper::get(Array<DComplex> &val, const String& name) const
{
    getArray(val, SerialHelper::shtDCOMPLEX, gr, name);
}

void SerialHelper::get(Array<String> &val, const String& name) const
{
    getArray(val, SerialHelper::shtSTRING, gr, name);
}

void SerialHelper::get(Record &val, const String& name) const
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    //std::cerr << name << " "<< gr.dataType(RecordFieldId(name)) << endl;
    if (gr.dataType(RecordFieldId(name)) != TpRecord) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need record," + 
					     " found array)");
    val = gr.asRecord(RecordFieldId(name));
}
/*

template <class V>
void getArrayVal(V &val, Int gtype, const Record& gr, 
		      const String& name, uInt index) 
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    if (gr.dataType(RecordFieldId(name)) != TpArray) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array," + 
					     " found record)");
    Array gv = gr.get(RecordFieldId(name));
    if (! gv.get(RecordFieldId(val), index))
	throw InvalidSerializationError(String("Wrong type for ") + 
					     name + " field (need " + 
					     SerialHelper::gtype[gtype] + 
					     "found " + 
				   SerialHelper::gtype[gv.elementType()]);
}
*/

/*
template <class V>
void getArray(Array<V> &val, Int gtype, const Record& gr,
		   const String& name) 
{
    if (! gr.isDefined(name)) throw FieldNotFoundError(name);
    if (gr.dataType(RecordFieldId(name)) != TpArray) 
	throw InvalidSerializationError(String("Wrong type for ") + name +
					     " field (need array," + 
					     " found record)");
    Array gv = gr.get(RecordFieldId(name));
    if (! gv.get(RecordFieldId(val)))
	throw InvalidSerializationError(String("Wrong type for ") + 
					     name + " field (need " + 
					     SerialHelper::gtype[gtype] + 
					     "found " + 
				   SerialHelper::gtype[gv.elementType()]);
}
*/

} //# NAMESPACE CASACORE - END

