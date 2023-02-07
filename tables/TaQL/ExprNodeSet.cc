//# ExprNodeSet.cc: Classes representing a set in table select expression
//# Copyright (C) 1997,1999,2000,2001,2003
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

#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeSet::TableExprNodeSet()
: TableExprNodeRep (NTNumeric, VTSet, OtUndef, Constant),
  itsSingle        (True),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (True)
{}

TableExprNodeSet::TableExprNodeSet (const IPosition& indices)
: TableExprNodeRep (NTInt, VTSet, OtUndef, Constant),
  itsSingle        (True),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (False)
{
    uInt n = indices.size();
    itsElems.resize (n);
    for (uInt i=0; i<n; i++) {
      itsElems[i].reset (new TableExprNodeSetElemSingle
                         (TableExprNode (Int64(indices(i)))));
    }
}

TableExprNodeSet::TableExprNodeSet (const Slicer& indices)
: TableExprNodeRep (NTInt, VTSet, OtUndef, Constant),
  itsSingle        (False),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (False)
{
    uInt n = indices.ndim();
    itsElems.resize (n);
    for (uInt i=0; i<n; i++) {
        TableExprNode start;
        TableExprNode end;
        if (indices.start()(i) != Slicer::MimicSource) {
          start = TableExprNode (Int64(indices.start()(i)));
        }
        if (indices.end()(i) != Slicer::MimicSource) {
          end = TableExprNode (Int64(indices.end()(i)));
        }
        TableExprNode incr (Int64(indices.stride()(i)));
        itsElems[i].reset (new TableExprNodeSetElemDiscrete (start, end, incr));
    }
}

TableExprNodeSet::TableExprNodeSet (const Vector<rownr_t>& rownrs,
                                    const TableExprNodeSet& set)
: TableExprNodeRep (set.dataType(), VTSet, OtUndef, Constant),
  itsElems         (rownrs.size() * set.size()),
  itsSingle        (set.isSingle()),
  itsDiscrete      (set.isDiscrete()),
  itsBounded       (set.isBounded()),
  itsCheckTypes    (False)
{
    // Fill in all values.
    size_t nrel = set.size();
    for (rownr_t i=0; i<rownrs.size(); i++) {
        for (size_t j=0; j<nrel; j++) {
          itsElems[j+i*nrel] = set[j]->evaluate (rownrs[i]);
        }
    }
    setUnit (set.unit());
}

TableExprNodeSet::TableExprNodeSet (const TableExprNodeSet& that)
: TableExprNodeRep (that),
  itsElems         (that.itsElems),
  itsSingle        (that.itsSingle),
  itsDiscrete      (that.itsDiscrete),
  itsBounded       (that.itsBounded),
  itsCheckTypes    (that.itsCheckTypes)
{}

TableExprNodeSet::~TableExprNodeSet()
{}

void TableExprNodeSet::add (const TENSEBShPtr& elemIn, Bool adaptType)
{
    // Convert a constant mid-width interval to a normal interval.
    TENSEBShPtr elem (elemIn);
    if (elem->isConstant()  &&  elem->isMidWidth()) {
      elem = elem->evaluate (TableExprId(0));
    }
    size_t n = itsElems.size();
    itsElems.resize (n+1);
    itsElems[n] = elem;
    // Set and adapt unit as needed.
    if (unit().empty()) {
        setUnit (elem->unit());
    }
    // See if the set properties change.
    if (! elem->isSingle()) {
        itsSingle = False;
        if (! elem->isDiscrete()) {
            itsDiscrete = False;
            itsBounded  = False;
        } else {
            if (elem->end() == 0) {
                // Note that an undefined start defaults to 0, this is bounded.
                itsBounded = False;
            }
        }
    }
    if (n == 0) {
        dtype_p = elem->dataType();
    } else if (adaptType) {
        // Determine the highest data type.
        // Note: using OtEQ works well for all types (including dates).
        dtype_p = TableExprNodeBinary::getDT (dtype_p, elem->dataType(),
                                              OtEQ);
    }
    fillExprType (itsElems[n].get());
}

void TableExprNodeSet::adaptSetUnits (const Unit& unit)
{
    if (! unit.empty()) {
        for (size_t i=0; i<itsElems.size(); i++) {
            itsElems[i]->adaptSetUnits (unit);
        }
        setUnit (unit);
    }
}

void TableExprNodeSet::checkEqualDataTypes() const
{
    if (itsCheckTypes) {
        for (size_t i=0; i<itsElems.size(); i++) {
            if (itsElems[i]->dataType() != dtype_p) {
                throw TableInvExpr ("Set elements must have equal data types");
            }
        }
    }
}

void TableExprNodeSet::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    for (size_t j=0; j<itsElems.size(); j++) {
        itsElems[j]->show (os, indent+2);
    }
}

void TableExprNodeSet::flattenTree (std::vector<TableExprNodeRep*>& nodes)
{
    nodes.push_back (this);
    for (size_t j=0; j<itsElems.size(); j++) {
        itsElems[j]->flattenTree (nodes);
    }
}

Bool TableExprNodeSet::hasArrays() const
{
    //# Check if a value is an array?
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        const TableExprNodeSetElemBase& elem = *itsElems[i];
        if (elem.start()  &&  elem.start()->valueType() == VTArray) {
            return True;
        }
        if (elem.end()  &&  elem.end()->valueType() == VTArray) {
            return True;
        }
        if (elem.increment()  &&  elem.increment()->valueType() == VTArray) {
            return True;
        }
    }
    return False;
}

TENShPtr TableExprNodeSet::setOrArray() const
{
    // A set where elements have different unit types cannot be turned
    // into an array.
    if (! unit().empty()) {
        Quantity q(1., unit());
        size_t n = size();
        for (size_t i=0; i<n; i++) {
              if (! itsElems[i]->unit().empty()) {
                  if (! q.isConform (itsElems[i]->unit())) {
                    return TENShPtr(new TableExprNodeSet (*this));
                  }
              }
        }
        // No different units, so adapt elements to the unit.
        for (size_t i=0; i<n; i++) {
            itsElems[i]->adaptSetUnits (unit());
        }
    }
    // If discrete, all start values should be filled in.
    if (itsDiscrete) {
        size_t n = size();
        for (size_t i=0; i<n; i++) {
            if (itsElems[i]->start() == 0) {
                throw (TableInvExpr ("no start value in discrete interval"));
            }
        }
    }
    // If the set is bounded, it can be converted to an array.
    if (itsBounded) {
        // If it is const, that can be done immediately.
        if (isConstant()) {
            return toConstArray();
        }
        // Set ndim and shape if those are constant.
    }
    TableExprNodeSet* set = new TableExprNodeSet (*this);
    TENShPtr pset(set);
    if (itsBounded) {
        // Set the type to VTArray; the getArray functions
        // will convert the set to an array for each row.
        // Set the shape and/or dimensionaly if constant.
        set->setValueType (VTArray);
        set->setShape();
    }
    return pset;
}

void TableExprNodeSet::setShape()
{
  // Only sets with single elements can have a size.
  if (!itsSingle) {
    return;
  }
  // Scalar elements form a vector.
  if (!hasArrays()) {
    ndim_p = 1;
    shape_p = IPosition (1, size());
  }
  // See if all elements have the same shape.
  // If not, leave the shape empty.
  // Do the same for dimensionality.
  IPosition shp (itsElems[0]->shape());
  uInt ndim = shp.size();
  for (size_t i=1; i<itsElems.size(); ++i) {
    IPosition shp2 (itsElems[i]->shape());
    if (!shp2.isEqual(shp)) {
      shp.resize(0);
    }
    if (shp2.size() != ndim) {
      ndim = 0;
    }
  }
  // The set has one more axis.
  if (ndim > 0) {
    ndim_p = ndim+1;
  }
  if (!shp.empty()) {
    shape_p = shp;
    shape_p.append (IPosition(1,size()));
  }
}
  
TENShPtr TableExprNodeSet::toConstArray() const
{
    // Construct the correct const array object.
    TENShPtr tsnptr;
    switch (dataType()) {
    case NTBool:
      tsnptr.reset (new TableExprNodeArrayConstBool (toArray<Bool>(0)));
      break;
    case NTInt:
      tsnptr.reset (new TableExprNodeArrayConstInt (toArray<Int64>(0)));
      break;
    case NTDouble:
      tsnptr.reset (new TableExprNodeArrayConstDouble (toArray<Double>(0)));
      break;
    case NTComplex:
      tsnptr.reset (new TableExprNodeArrayConstDComplex (toArray<DComplex>(0)));
      break;
    case NTString:
      tsnptr.reset (new TableExprNodeArrayConstString (toArray<String>(0)));
      break;
    case NTDate:
      tsnptr.reset (new TableExprNodeArrayConstDate (toArray<MVTime>(0)));
      break;
    default:
      TableExprNode::throwInvDT ("TableExprNodeSet::toConstArray");
    }
    tsnptr->setUnit (unit());
    return tsnptr;
}

MArray<Bool> TableExprNodeSet::getArrayBool (const TableExprId& id)
{
  return toArray<Bool> (id);
}
MArray<Int64> TableExprNodeSet::getArrayInt (const TableExprId& id)
{
  return toArray<Int64> (id);
}
MArray<Double> TableExprNodeSet::getArrayDouble (const TableExprId& id)
{
  return toArray<Double> (id);
}
MArray<DComplex> TableExprNodeSet::getArrayDComplex (const TableExprId& id)
{
  return toArray<DComplex> (id);
}
MArray<String> TableExprNodeSet::getArrayString (const TableExprId& id)
{
  return toArray<String> (id);
}
MArray<MVTime> TableExprNodeSet::getArrayDate (const TableExprId& id)
{
  return toArray<MVTime> (id);
}

Bool TableExprNodeSet::contains (const TableExprId& id, Bool value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchBool (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
Bool TableExprNodeSet::contains (const TableExprId& id, Int64 value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchInt (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
Bool TableExprNodeSet::contains (const TableExprId& id, Double value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDouble (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
Bool TableExprNodeSet::contains (const TableExprId& id, DComplex value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDComplex (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
Bool TableExprNodeSet::contains (const TableExprId& id, String value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchString (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
Bool TableExprNodeSet::contains (const TableExprId& id, MVTime value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDate (&result, &value, 1, id);
        if (result) break;
    }
    return result;
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<Bool>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Bool* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchBool (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<Int64>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Int64* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchInt (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<Double>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Double* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDouble (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<DComplex>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const DComplex* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDComplex (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<String>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchString (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::contains (const TableExprId& id,
                                         const MArray<MVTime>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const MVTime* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        itsElems[i]->matchDate (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}

} //# NAMESPACE CASACORE - END

