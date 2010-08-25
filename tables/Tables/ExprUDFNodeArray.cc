//# ExprUDFNodeArray.cc: Class representing an array UDF in select expression
//# Copyright (C) 2010
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
#include <tables/Tables/ExprUDFNodeArray.h>

namespace casa { //# NAMESPACE CASA - BEGIN
  
  TableExprUDFNodeArray::TableExprUDFNodeArray (UDFBase* udf,
                                                const TableExprNodeSet&)
    : TableExprNodeArray (udf->dataType(), OtFunc),
      itsUDF (udf)
  {}

  TableExprUDFNodeArray::~TableExprUDFNodeArray()
  {
    delete itsUDF;
  }

  Array<Bool>     TableExprUDFNodeArray::getArrayBool    (const TableExprId& id)
    { return itsUDF->getArrayBool (id); }
  Array<Int64>    TableExprUDFNodeArray::getArrayInt     (const TableExprId& id)
    { return itsUDF->getArrayInt (id); }
  Array<Double>   TableExprUDFNodeArray::getArrayDouble  (const TableExprId& id)
    { return itsUDF->getArrayDouble (id); }
  Array<DComplex> TableExprUDFNodeArray::getArrayDComplex(const TableExprId& id)
    { return itsUDF->getArrayDComplex (id); }
  Array<String>   TableExprUDFNodeArray::getArrayString  (const TableExprId& id)
    { return itsUDF->getArrayString (id); }
  Array<MVTime>   TableExprUDFNodeArray::getArrayDate    (const TableExprId& id)
    { return itsUDF->getArrayDate (id); }

} //# NAMESPACE CASA - END
