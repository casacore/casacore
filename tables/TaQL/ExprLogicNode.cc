//# ExprLogicNode.cc: Nodes representing scalar logical operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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

#include <casacore/tables/TaQL/ExprLogicNode.h>
#include <casacore/tables/TaQL/ExprNodeSetOpt.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <float.h>                     // for DBL_MAX
#include <limits.h>                     // for DBL_MAX


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Implement the comparison operators for each data type.

TableExprNodeEQBool::TableExprNodeEQBool (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQBool::getBool (const TableExprId& id)
{
    return lnode_p->getBool(id) == rnode_p->getBool(id);
}

TableExprNodeEQInt::TableExprNodeEQInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQInt::getBool (const TableExprId& id)
{
    return lnode_p->getInt(id) == rnode_p->getInt(id);
}

TableExprNodeEQDouble::TableExprNodeEQDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQDouble::getBool (const TableExprId& id)
{
    return lnode_p->getDouble(id) == rnode_p->getDouble(id);
}

TableExprNodeEQDComplex::TableExprNodeEQDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQDComplex::getBool (const TableExprId& id)
{
    return lnode_p->getDComplex(id) == rnode_p->getDComplex(id);
}

TableExprNodeEQString::TableExprNodeEQString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQString::getBool (const TableExprId& id)
{
    return lnode_p->getString(id) == rnode_p->getString(id);
}

TableExprNodeEQRegex::TableExprNodeEQRegex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQRegex::getBool (const TableExprId& id)
{
    return rnode_p->getRegex(id).match (lnode_p->getString(id));
}

TableExprNodeEQDate::TableExprNodeEQDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtEQ)
{}
Bool TableExprNodeEQDate::getBool (const TableExprId& id)
{
    return lnode_p->getDate(id) == rnode_p->getDate(id);
}


TableExprNodeNEBool::TableExprNodeNEBool (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEBool::getBool (const TableExprId& id)
{
    return lnode_p->getBool(id) != rnode_p->getBool(id);
}

TableExprNodeNEInt::TableExprNodeNEInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEInt::getBool (const TableExprId& id)
{
    return lnode_p->getInt(id) != rnode_p->getInt(id);
}

TableExprNodeNEDouble::TableExprNodeNEDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEDouble::getBool (const TableExprId& id)
{
    return lnode_p->getDouble(id) != rnode_p->getDouble(id);
}

TableExprNodeNEDComplex::TableExprNodeNEDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEDComplex::getBool (const TableExprId& id)
{
    return lnode_p->getDComplex(id) != rnode_p->getDComplex(id);
}

TableExprNodeNEString::TableExprNodeNEString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEString::getBool (const TableExprId& id)
{
    return lnode_p->getString(id) != rnode_p->getString(id);
}

TableExprNodeNERegex::TableExprNodeNERegex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNERegex::getBool (const TableExprId& id)
{
    return ! rnode_p->getRegex(id).match (lnode_p->getString(id));
}

TableExprNodeNEDate::TableExprNodeNEDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNE)
{}
Bool TableExprNodeNEDate::getBool (const TableExprId& id)
{
    return lnode_p->getDate(id) != rnode_p->getDate(id);
}


TableExprNodeGTInt::TableExprNodeGTInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGT)
{}
Bool TableExprNodeGTInt::getBool (const TableExprId& id)
{
    return lnode_p->getInt(id) > rnode_p->getInt(id);
}

TableExprNodeGTDouble::TableExprNodeGTDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGT)
{}
Bool TableExprNodeGTDouble::getBool (const TableExprId& id)
{
    return lnode_p->getDouble(id) > rnode_p->getDouble(id);
}

TableExprNodeGTDComplex::TableExprNodeGTDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGT)
{}
Bool TableExprNodeGTDComplex::getBool (const TableExprId& id)
{
    return lnode_p->getDComplex(id) > rnode_p->getDComplex(id);
}

TableExprNodeGTString::TableExprNodeGTString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGT)
{}
Bool TableExprNodeGTString::getBool (const TableExprId& id)
{
    return lnode_p->getString(id) > rnode_p->getString(id);
}

TableExprNodeGTDate::TableExprNodeGTDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGT)
{}
Bool TableExprNodeGTDate::getBool (const TableExprId& id)
{
    return lnode_p->getDate(id) > rnode_p->getDate(id);
}


TableExprNodeGEInt::TableExprNodeGEInt (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGE)
{}
Bool TableExprNodeGEInt::getBool (const TableExprId& id)
{
    return lnode_p->getInt(id) >= rnode_p->getInt(id);
}

TableExprNodeGEDouble::TableExprNodeGEDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGE)
{}
Bool TableExprNodeGEDouble::getBool (const TableExprId& id)
{
    return lnode_p->getDouble(id) >= rnode_p->getDouble(id);
}

TableExprNodeGEDComplex::TableExprNodeGEDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGE)
{}
Bool TableExprNodeGEDComplex::getBool (const TableExprId& id)
{
    return lnode_p->getDComplex(id) >= rnode_p->getDComplex(id);
}

TableExprNodeGEString::TableExprNodeGEString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGE)
{}
Bool TableExprNodeGEString::getBool (const TableExprId& id)
{
    return lnode_p->getString(id) >= rnode_p->getString(id);
}

TableExprNodeGEDate::TableExprNodeGEDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtGE)
{}
Bool TableExprNodeGEDate::getBool (const TableExprId& id)
{
    return lnode_p->getDate(id) >= rnode_p->getDate(id);
}


TableExprNodeINInt::TableExprNodeINInt (const TableExprNodeRep& node,
                                        Bool)
: TableExprNodeBinary (NTBool, node, OtIN)
{}
void TableExprNodeINInt::optimize()
{
  doOptimize (rnode_p);
}
void TableExprNodeINInt::doOptimize (TENShPtr& rnode)
{
  if (rnode->isConstant()  &&  rnode->valueType() == VTArray) {
    // Convert a constant array for faster lookup.
    MArray<Int64> values = rnode->getArrayInt(0);
    Array<Int64> arr(values.array());
    if (values.hasMask()) {
      // Remove masked elements.
      arr.reference (values.flatten());
    }
    // Use an unordered_map for fast lookup.
    rnode.reset (new TableExprNodeSetOptUSet<Int64> (*rnode, arr));
  }
}
Bool TableExprNodeINInt::getBool (const TableExprId& id)
{
    Int64 val = lnode_p->getInt (id);
    return rnode_p->contains (id, val);
}

TableExprNodeINDouble::TableExprNodeINDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtIN)
{}
void TableExprNodeINDouble::optimize()
{
  doOptimize (rnode_p);
}
void TableExprNodeINDouble::doOptimize (TENShPtr& rnode)
{
  if (rnode->isConstant()  &&  rnode->valueType() == VTSet) {
    TableExprNodeSet& set = dynamic_cast<TableExprNodeSet&>(*rnode);
    if (!set.isSingle()  &&  !set.isDiscrete()) {
      rnode = TableExprNodeSetOptContSetBase<Double>::transform (set);
    }
  }
}
Bool TableExprNodeINDouble::getBool (const TableExprId& id)
{
    return rnode_p->contains (id, lnode_p->getDouble (id));
}

TableExprNodeINDComplex::TableExprNodeINDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtIN)
{}
Bool TableExprNodeINDComplex::getBool (const TableExprId& id)
{
    return rnode_p->contains (id, lnode_p->getDComplex (id));
}

TableExprNodeINString::TableExprNodeINString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtIN)
{}
void TableExprNodeINString::optimize()
{
  doOptimize (rnode_p);
}
void TableExprNodeINString::doOptimize (TENShPtr& rnode)
{
  if (rnode->isConstant()) {
    if (rnode->valueType() == VTSet) {
      // A constant set with continuous intervals can be made faster.
      TableExprNodeSet& set = dynamic_cast<TableExprNodeSet&>(*rnode);
      if (!set.isSingle()  &&  !set.isDiscrete()) {
        rnode = TableExprNodeSetOptContSetBase<String>::transform (set);
      }
    } else if (rnode->valueType() == VTArray) {
      // Convert a constant array to an unordered_set for faster lookup.
      MArray<String> values = rnode->getArrayString(0);
      Array<String> arr(values.array());
      if (values.hasMask()) {
        // Remove masked elements.
        arr.reference (values.flatten());
      }
      rnode.reset (new TableExprNodeSetOptUSet<String> (*rnode, arr));
    }
  }
}
Bool TableExprNodeINString::getBool (const TableExprId& id)
{
    return rnode_p->contains (id, lnode_p->getString (id));
}

TableExprNodeINDate::TableExprNodeINDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtIN)
{}
void TableExprNodeINDate::optimize()
{
  doOptimize (rnode_p);
}
void TableExprNodeINDate::doOptimize (TENShPtr& rnode)
{
  if (rnode->isConstant()  &&  rnode->valueType() == VTSet) {
    TableExprNodeSet& set = dynamic_cast<TableExprNodeSet&>(*rnode);
    if (!set.isSingle()  &&  !set.isDiscrete()) {
      rnode = TableExprNodeSetOptContSetBase<Double>::transform (set);
    }
  }
}
Bool TableExprNodeINDate::getBool (const TableExprId& id)
{
    return rnode_p->contains (id, lnode_p->getDate (id));
}


TableExprNodeOR::TableExprNodeOR (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtOR)
{}
Bool TableExprNodeOR::getBool (const TableExprId& id)
{
    return lnode_p->getBool(id) || rnode_p->getBool(id);
}


TableExprNodeAND::TableExprNodeAND (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtAND)
{}
Bool TableExprNodeAND::getBool (const TableExprId& id)
{
    return lnode_p->getBool(id) && rnode_p->getBool(id);
}


TableExprNodeNOT::TableExprNodeNOT (const TableExprNodeRep& node)
: TableExprNodeBinary (NTBool, node, OtNOT)
{}
Bool TableExprNodeNOT::getBool (const TableExprId& id)
{
  return ! lnode_p->getBool(id);
}



void TableExprNodeEQDouble::ranges (Block<TableExprRange>& blrange)
{
    Double dval = 0;
    TENShPtr tsncol = 0;
    //# We can store a range if there is a scalar column and constant
    //# (left or right).
    if (lnode_p->operType()  == TableExprNodeRep::OtColumn
    &&  lnode_p->valueType() == TableExprNodeRep::VTScalar
    &&  rnode_p->operType()  == TableExprNodeRep::OtLiteral) {
        tsncol = lnode_p;
        dval = rnode_p->getDouble (0);
    }else{
        if (rnode_p->operType()  == TableExprNodeRep::OtColumn
        &&  rnode_p->valueType() == TableExprNodeRep::VTScalar
        &&  lnode_p->operType()  == TableExprNodeRep::OtLiteral) {
            tsncol = rnode_p;
            dval = lnode_p->getDouble (0);
        }
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange,
                                   dynamic_cast<TableExprNodeColumn*>(tsncol.get()),
                                   dval, dval);
}

void TableExprNodeGEDouble::ranges (Block<TableExprRange>& blrange)
{
    Double st = 0;
    Double end = 0;
    TENShPtr tsncol = 0;
    //# We can store a range if there is a scalar column and constant
    //# (left or right).
    if (lnode_p->operType()  == TableExprNodeRep::OtColumn
    &&  lnode_p->valueType() == TableExprNodeRep::VTScalar
    &&  rnode_p->operType()  == TableExprNodeRep::OtLiteral) {
        tsncol = lnode_p;
        st = rnode_p->getDouble (0);
        end = DBL_MAX;
    }else{
        if (rnode_p->operType()  == TableExprNodeRep::OtColumn
        &&  rnode_p->valueType() == TableExprNodeRep::VTScalar
        &&  lnode_p->operType()  == TableExprNodeRep::OtLiteral) {
            tsncol = rnode_p;
            end = lnode_p->getDouble (0);
            st = -DBL_MAX;
        }
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange,
                                   dynamic_cast<TableExprNodeColumn*>(tsncol.get()),
                                   st, end);
}

void TableExprNodeGTDouble::ranges (Block<TableExprRange>& blrange)
{
    Double st = 0;
    Double end = 0;
    TENShPtr tsncol = 0;
    //# We can store a range if there is a scalar column and constant
    //# (left or right).
    if (lnode_p->operType()  == TableExprNodeRep::OtColumn
    &&  lnode_p->valueType() == TableExprNodeRep::VTScalar
    &&  rnode_p->operType()  == TableExprNodeRep::OtLiteral) {
        tsncol = lnode_p;
        st = rnode_p->getDouble (0);
        end = DBL_MAX;
    }else{
        if (rnode_p->operType()  == TableExprNodeRep::OtColumn
        &&  lnode_p->valueType() == TableExprNodeRep::VTScalar
        &&  lnode_p->operType()  == TableExprNodeRep::OtLiteral) {
            tsncol = rnode_p;
            end = lnode_p->getDouble (0);
            st = -DBL_MAX;
        }
    }
    //# Now create a range (if possible).
    //# The cast is harmless, since it is surely that object type.
    TableExprNodeRep::createRange (blrange,
                                   dynamic_cast<TableExprNodeColumn*>(tsncol.get()),
                                   st, end);
}


//# Or two blocks of ranges.
void TableExprNodeOR::ranges (Block<TableExprRange>& blrange)
{
    //# Get the ranges of the children.
    Block<TableExprRange> left,right;
    lnode_p->ranges (left);
    rnode_p->ranges (right);
    //# Now or the ranges.
    //# If a column appears in one, but not in the other it needs
    //# to be removed. Only equal columns can be combined and what
    //# gets created is a superset of the original blocks.
    blrange.resize(0,True);
    size_t nr=0;
    for (size_t i=0; i<left.nelements(); i++) {
        for (size_t j=0; j<right.nelements(); j++) {
            if (right[j].getColumn().columnDesc().name() ==
                                left[i].getColumn().columnDesc().name()) {
                blrange.resize(nr+1, True);
                blrange[nr] = left[i];
                blrange[nr].mixOr (right[j]);
                nr++;
            }
        }
    }
}


//# And two blocks of ranges.
void TableExprNodeAND::ranges (Block<TableExprRange>& blrange)
{
    //# Get the ranges of the children.
    Block<TableExprRange> other;
    lnode_p->ranges (blrange);
    rnode_p->ranges (other);
    //# If one of them is empty (which means all), return the other.
    if (other.nelements() == 0) {
        return;
    }
    if (blrange.nelements() == 0) {
        blrange = other;
        return;
    }
    //# Now and the ranges.
    //# First handle one and intersect its ranges with matching
    //# column names in the other one.
    //# Keep a vector with flags for non-processed other ones.
    Vector<Int> vec(other.nelements());
    vec = 0;
    for (size_t i=0; i<blrange.nelements(); i++) {
        for (size_t j=0; j<other.nelements(); j++) {
            if (other[j].getColumn().columnDesc().name() ==
                                blrange[i].getColumn().columnDesc().name()) {
                blrange[i].mixAnd (other[j]);
                vec(j) = 1;
            }
        }
    }
    //# Now add the non-processed other ones to the result.
    size_t nr = blrange.nelements();
    for (size_t i=0; i<other.nelements(); i++) {
        if (vec(i) == 0) {
            blrange.resize(nr+1, True);
            blrange[nr++] = other[i];
        }
    }
}

} //# NAMESPACE CASACORE - END

