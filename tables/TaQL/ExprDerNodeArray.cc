//# ExprDerArrayNode.cc: Nodes representing constant arrays in table select expression tree
//# Copyright (C) 1997,1998,1999,2000
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

#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Quanta/MVTime.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeArrayConstBool::TableExprNodeArrayConstBool
                                                 (const Array<bool>& val)
: TableExprNodeArray (NTBool, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstBool::TableExprNodeArrayConstBool
                                                 (const MArray<bool>& val)
: TableExprNodeArray (NTBool, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstBool::~TableExprNodeArrayConstBool()
{}
MArray<bool> TableExprNodeArrayConstBool::getArrayBool (const TableExprId&)
    { return value_p; }


TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<uint64_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<int64_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<uint32_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<int32_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<uint16_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<int16_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const Array<unsigned char>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<uint64_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<int64_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<uint32_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<int32_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<uint16_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<int16_t>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::TableExprNodeArrayConstInt
                                                 (const MArray<unsigned char>& val)
: TableExprNodeArray (NTInt, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstInt::~TableExprNodeArrayConstInt()
{}
MArray<int64_t> TableExprNodeArrayConstInt::getArrayInt
                                                 (const TableExprId&)
    { return value_p; }
MArray<double> TableExprNodeArrayConstInt::getArrayDouble
                                                 (const TableExprId&)
{
    MArray<double> arr;
    arr.fill (value_p);
    return arr;
}
MArray<DComplex> TableExprNodeArrayConstInt::getArrayDComplex
                                                 (const TableExprId&)
{
    MArray<DComplex> arr;
    arr.fill (value_p);
    return arr;
}

TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<double>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<float>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const Array<int64_t>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const MArray<double>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const MArray<float>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDouble::TableExprNodeArrayConstDouble
                                                 (const MArray<int64_t>& val)
: TableExprNodeArray (NTDouble, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDouble::~TableExprNodeArrayConstDouble()
{}
MArray<double> TableExprNodeArrayConstDouble::getArrayDouble
                                                 (const TableExprId&)
    { return value_p; }
MArray<DComplex> TableExprNodeArrayConstDouble::getArrayDComplex
                                                 (const TableExprId&)
{
    MArray<DComplex> arr;
    arr.fill (value_p);
    return arr;
}

TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<DComplex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<Complex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<double>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const Array<int64_t>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const MArray<DComplex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const MArray<Complex>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const MArray<double>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::TableExprNodeArrayConstDComplex
                                                 (const MArray<int64_t>& val)
: TableExprNodeArray (NTComplex, OtLiteral, val.shape())
{
    value_p.fill (val);
}
TableExprNodeArrayConstDComplex::~TableExprNodeArrayConstDComplex()
{}
MArray<DComplex> TableExprNodeArrayConstDComplex::getArrayDComplex
                                                 (const TableExprId&)
    { return value_p; }

TableExprNodeArrayConstString::TableExprNodeArrayConstString
                                                 (const Array<String>& val)
: TableExprNodeArray (NTString, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstString::TableExprNodeArrayConstString
                                                 (const MArray<String>& val)
: TableExprNodeArray (NTString, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstString::~TableExprNodeArrayConstString()
{}
MArray<String> TableExprNodeArrayConstString::getArrayString
                                                 (const TableExprId&)
    { return value_p; }

TableExprNodeArrayConstDate::TableExprNodeArrayConstDate
                                                 (const Array<MVTime>& val)
: TableExprNodeArray (NTDate, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDate::TableExprNodeArrayConstDate
                                                 (const MArray<MVTime>& val)
: TableExprNodeArray (NTDate, OtLiteral, val.shape()),
  value_p            (val)
{}
TableExprNodeArrayConstDate::~TableExprNodeArrayConstDate()
{}
MArray<double> TableExprNodeArrayConstDate::getArrayDouble (const TableExprId&)
{
    MArray<double> arr;
    arr.fill (value_p);
    return arr;
}
MArray<MVTime> TableExprNodeArrayConstDate::getArrayDate (const TableExprId&)
    { return value_p; }

} //# NAMESPACE CASACORE - END

