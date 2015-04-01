//# ExprConeNode.cc: Class representing a cone search in table select expression
//# Copyright (C) 2005
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

#include <casacore/tables/TaQL/ExprConeNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprConeNode::TableExprConeNode (FunctionType ftype, NodeDataType dtype,
				      const TableExprNodeSet& source,
				      uInt origin)
: TableExprFuncNode (ftype, dtype, VTScalar, source),
  origin_p          (origin)
{}

TableExprConeNode::~TableExprConeNode()
{}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// When one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprConeNode::fillNode
                                   (TableExprConeNode* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
  return TableExprFuncNode::fillNode (thisNode, nodes, dtypeOper);
}

// Fill the children pointers of a node.
void TableExprConeNode::fillChildNodes (TableExprConeNode* thisNode,
					PtrBlock<TableExprNodeRep*>& nodes,
					const Block<Int>& dtypeOper)
{
  TableExprFuncNode::fillChildNodes (thisNode, nodes, dtypeOper);
}

void TableExprConeNode::tryToConst()
{
}

Bool TableExprConeNode::getBool (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::anyconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First ANYCONE argument must have 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 3 != 0) {
	throw TableInvExpr("Second ANYCONE argument "
			   "must have multiple of 3 values");
      }
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra  = src[0];
      const double dec = src[1];
      Bool res = False;
      for (uInt i=0; i<coneArr.nelements(); i+=3) {
	const double raCone  = cone[i];
	const double decCone = cone[i+1];
	const double radius  = cone[i+2];
	if (cos(radius) <= sin(decCone) * sin(dec) +
	                   cos(decCone) * cos(dec) * cos(raCone-ra)) {
	  res = True;
	  break;
	}
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  case TableExprFuncNode::conesFUNC:
    {
      Array<double> srcArr  = operands_p[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First CONES argument must have 2 values");
      }
      Array<double> coneArr = operands_p[1]->getArrayDouble(id);
      if (coneArr.nelements() != 3) {
	throw TableInvExpr("Second CONES argument "
			   "must have multiple of 3 values");
      }
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra      = src[0];
      const double dec     = src[1];
      const double raCone  = cone[0];
      const double decCone = cone[1];
      const double radius  = cone[2];
      Bool res = cos(radius) <= sin(decCone) * sin(dec) +
	                        cos(decCone) * cos(dec) * cos(raCone - ra);
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  case TableExprFuncNode::anycone3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First ANYCONE argument must have 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 2 != 0) {
	throw TableInvExpr("Second ANYCONE3 argument "
			   "must have multiple of 2 values");
      }
      // Radius can be a single value or an array.
      int nrrad = 1;
      double radval;
      const double* rad = 0;
      Array<double> radArr;
      if ( operands()[2]->valueType() == VTArray) {
	radArr = operands()[2]->getArrayDouble(id);
	nrrad = radArr.nelements();
      } else {
	radval = operands()[2]->getDouble(id);
	rad = &radval;
      }
      Bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
	rad = radArr.getStorage (deleteRad);
      }
      const double ra  = src[0];
      const double dec = src[1];
      Bool res = False;
      for (uInt i=0; i<coneArr.nelements(); i+=2) {
	const double raCone  = cone[i];
	const double decCone = cone[i+1];
	double dist = sin(decCone) * sin(dec) +
	              cos(decCone) * cos(dec) * cos(raCone-ra);
	for (Int k=0; k<nrrad; k++) {
	  const double radius  = rad[k];
	  if (cos(radius) <= dist) {
	    res = True;
	    break;
	  }
	}
	if (res) break;
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      if (rad != &radval) {
	radArr.freeStorage (rad, deleteRad);
      }
      return res;
    }
  case TableExprFuncNode::cones3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First CONES argument "
			   "must have 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 2 != 0) {
	throw TableInvExpr("Second CONES3 argument "
			   "must have multiple of 2 values");
      }
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra      = src[0];
      const double dec     = src[1];
      const double raCone  = cone[0];
      const double decCone = cone[1];
      const double radius  = operands()[2]->getDouble(id);
      Bool res = cos(radius) <= sin(decCone) * sin(dec) +
	                        cos(decCone) * cos(dec) * cos(raCone - ra);
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  default:
    throw (TableInvExpr ("TableExprConeNode::getBool, "
			 "unknown function"));
  }
  return True;
}

Int64 TableExprConeNode::getInt (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::findconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First FINDCONE argument "
			   "must have 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 3 != 0) {
	throw TableInvExpr("Second FINDCONE argument "
			   "must have multiple of 3 values");
      }
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra  = src[0];
      const double dec = src[1];
      Int res = -1;
      for (uInt i=0; i<coneArr.nelements(); i+=3) {
	const double raCone  = cone[i];
	const double decCone = cone[i+1];
	const double radius  = cone[i+2];
	if (cos(radius) <= sin(decCone) * sin(dec) +
	                   cos(decCone) * cos(dec) * cos(raCone-ra)) {
	  res = origin_p + i/3;
	  break;
	}
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  case TableExprFuncNode::findcone3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First FINDCONE argument "
			   "must have 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 2 != 0) {
	throw TableInvExpr("Second FINDCONE3 argument "
			   "must have multiple of 2 values");
      }
      // Radius can be a single value or an array.
      int nrrad = 1;
      double radval;
      const double* rad = 0;
      Array<double> radArr;
      if ( operands()[2]->valueType() == VTArray) {
	radArr = operands()[2]->getArrayDouble(id);
	nrrad = radArr.nelements();
      } else {
	radval = operands()[2]->getDouble(id);
	rad = &radval;
      }
      Bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
	rad = radArr.getStorage (deleteRad);
      }
      const double ra  = src[0];
      const double dec = src[1];
      Int res = -1;
      for (uInt i=0; i<coneArr.nelements(); i+=2) {
	const double raCone  = cone[i];
	const double decCone = cone[i+1];
	double dist = sin(decCone) * sin(dec) +
	              cos(decCone) * cos(dec) * cos(raCone-ra);
	for (Int k=0; k<nrrad; k++) {
	  const double radius  = rad[k];
	  if (cos(radius) <= dist) {
	    res = origin_p + k + nrrad*i/2;
	    break;
	  }
	}
	if (res >= 0) break;
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      if (rad != &radval) {
	radArr.freeStorage (rad, deleteRad);
      }
      return res;
    }
  default:
    throw (TableInvExpr ("TableExprConeNode::getDouble, "
			 "unknown function"));
  }
  return 0;
}


TableExprNodeRep::NodeDataType TableExprConeNode::checkOperands
                                 (Block<Int>& dtypeOper,
				  ValueType& resVT, Block<Int>&,
				  FunctionType fType,
				  PtrBlock<TableExprNodeRep*>& nodes)
{
  int nrarg = 3;
  switch (fType) {
  // The 2 argument cone functions accept arrays only.
  // The result is a Bool scalar or array.
  case TableExprFuncNode::conesFUNC:
  case TableExprFuncNode::anyconeFUNC:
  case TableExprFuncNode::findconeFUNC:
    nrarg = 2;      // fall through
  // The 3 argument cone functions accept a scalar or array as the 3rd argument.
  // The result is a Bool scalar or array.
  case TableExprFuncNode::cones3FUNC:
  case TableExprFuncNode::anycone3FUNC:
  case TableExprFuncNode::findcone3FUNC:
    {
      checkNumOfArg (nrarg, nrarg, nodes);
      for (Int i=0; i<2; i++) {
	if (nodes[i]->valueType() != VTArray) {
	  throw TableInvExpr
	    ("First 2 arguments of CONE functions must be double arrays");
	}
      }
      // Result is a scalar or array.
      resVT = VTScalar;
      // Check the number of elements in the position node.
      Int nvalPos = findNelem (nodes[0]);
      Int nvalCone = findNelem (nodes[1]);
      // findcone returns an index value as integer.
      // This is a scalar if there is one source.
      if (fType == findconeFUNC  ||  fType == findcone3FUNC) {
	if (nvalPos != 2) {
	  resVT = VTArray;
	}
	return checkDT (dtypeOper, NTReal, NTInt, nodes);
      }    
      // cones returns an array if there is more than one cone or radius.
      if (fType == conesFUNC) {
	if (nvalCone != 3) {
	  resVT = VTArray;
	}
      } else if (fType == cones3FUNC) {
	if (nvalCone != 2  ||  nodes[2]->valueType() != VTScalar) {
	  resVT = VTArray;
	}
      }
      return checkDT (dtypeOper, NTReal, NTBool, nodes);
    }
  default:
    throw (TableInvExpr ("TableExprConeNode::checkOperands, "
			 "function not contained in switch statement"));
  }
}

Int TableExprConeNode::findNelem (const TableExprNodeRep* node)
{
  Int nelem = -1;
  if (node->valueType() == VTSet) {
    const TableExprNodeSet* set = dynamic_cast<const TableExprNodeSet*>(node);
    AlwaysAssert (set, AipsError);
    TableExprNodeRep* arr = set->setOrArray();
    if (arr->valueType() != VTArray) {
      throw TableInvExpr ("CONES argument is a non-array set");
    }
    nelem = arr->shape().product();
    delete arr;
  } else {
    nelem = node->shape().product();
  }
  return nelem;
}



TableExprConeNodeArray::TableExprConeNodeArray (TableExprFuncNode::FunctionType ftype,
                                                NodeDataType dtype,
                                                const TableExprNodeSet& source,
                                                uInt origin)
  : TableExprFuncNodeArray (ftype, dtype, VTArray, source, TaQLStyle()),
  origin_p               (origin)
{
  ndim_p = -1;
}

TableExprConeNodeArray::~TableExprConeNodeArray()
{}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
// When one of the nodes is a constant, convert its type if
// it does not match the other one.
TableExprNodeRep* TableExprConeNodeArray::fillNode
                                   (TableExprConeNodeArray* thisNode,
				    PtrBlock<TableExprNodeRep*>& nodes,
				    const Block<Int>& dtypeOper)
{
  return TableExprFuncNodeArray::fillNode (thisNode, nodes, dtypeOper);
}

// Fill the children pointers of a node.
void TableExprConeNodeArray::fillChildNodes (TableExprConeNodeArray* thisNode,
                                             PtrBlock<TableExprNodeRep*>& nodes,
                                             const Block<Int>& dtypeOper)
{
  TableExprFuncNode::fillChildNodes (thisNode->getChild(), nodes, dtypeOper);
}

void TableExprConeNodeArray::tryToConst()
{
}

Array<Bool> TableExprConeNodeArray::getArrayBool (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::conesFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() % 2 != 0) {
	throw TableInvExpr("First CONES argument "
			   "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 3 != 0) {
	throw TableInvExpr("Second CONES argument "
			   "must have multiple of 3 values");
      }
      // The result shape is a matrix (#cones, #sources).
      Int nsrc  = srcArr.nelements() / 2;
      Int ncone = coneArr.nelements() / 3;
      Array<Bool> resArr(IPosition(2,ncone,nsrc));
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      Bool* res = resArr.data();
      for (uInt j=0; j<srcArr.nelements(); j+=2) {
	const double ra  = src[j];
	const double sindec = sin(src[j+1]);
	const double cosdec = cos(src[j+1]);
	for (uInt i=0; i<coneArr.nelements(); i+=3) {
	  const double raCone  = cone[i];
	  const double decCone = cone[i+1];
	  const double radius  = cone[i+2];
	  *res++ = cos(radius) <= sin(decCone) * sindec +
	                          cos(decCone) * cosdec * cos(raCone-ra);
	}
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return resArr;
    }
  case TableExprFuncNode::cones3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() != 2) {
	throw TableInvExpr("First CONES3 argument "
			   "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 2 != 0) {
	throw TableInvExpr("Second CONES3 argument "
			   "must have multiple of 2 values");
      }
      // Radius can be a single value or an array.
      int nrrad = 1;
      double radval;
      const double* rad = 0;
      Array<double> radArr;
      if ( operands()[2]->valueType() == VTArray) {
	radArr = operands()[2]->getArrayDouble(id);
	nrrad = radArr.nelements();
      } else {
	radval = operands()[2]->getDouble(id);
	rad = &radval;
      }
      // The result shape is a cube (#radii, #cones, #sources).
      Int nsrc  = srcArr.nelements() / 2;
      Int ncone = coneArr.nelements() / 2;
      Bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
	rad = radArr.getStorage (deleteRad);
      }
      Array<Bool> resArr(IPosition(3,nrrad,ncone,nsrc));
      Bool* res = resArr.data();
      for (uInt j=0; j<srcArr.nelements(); j+=2) {
	const double ra  = src[j];
	const double dec = src[j+1];
	for (uInt i=0; i<coneArr.nelements(); i+=2) {
	  const double raCone  = cone[i];
	  const double decCone = cone[i+1];
	  double dist = sin(decCone) * sin(dec) +
	                cos(decCone) * cos(dec) * cos(raCone-ra);

	  for (Int k=0; k<nrrad; k++) {
	    const double radius = rad[k];
	    *res++ = cos(radius) <= dist;
	  }
	}
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      if (rad != &radval) {
	radArr.freeStorage (rad, deleteRad);
      }
      return resArr;
    }
  default:
    throw (TableInvExpr ("TableExprConeNodeArray::getArrayBool, "
			 "unknown function"));
  }
}

Array<Int64> TableExprConeNodeArray::getArrayInt (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::findconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() % 2 != 0) {
	throw TableInvExpr("First FINDCONE argument "
			   "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 3 != 0) {
	throw TableInvExpr("Second FINDCONE argument "
			   "must have multiple of 3 values");
      }
      // The result shape is the source array shape.
      IPosition shpc = srcArr.shape();
      IPosition shp;
      if (shpc.nelements() > 1  &&  shpc[0] == 2) {
	shp = shpc.getLast (shpc.nelements() - 1);
      } else {
	shp = shpc;
	shp[0] = shp[0] / 2;
      }
      Array<Int64> resArr(shp);
      Bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      Int64* res = resArr.data();
      for (uInt j=0; j<srcArr.nelements(); j+=2) {
	const double ra  = src[j];
	const double dec = src[j+1];
	*res = -1;
	for (uInt i=0; i<coneArr.nelements(); i+=3) {
	  const double raCone  = cone[i];
	  const double decCone = cone[i+1];
	  const double radius  = cone[i+2];
	  if (cos(radius) <= sin(decCone) * sin(dec) +
	                     cos(decCone) * cos(dec) * cos(raCone-ra)) {
	    *res = origin_p + i/3;
	    break;
	  }
	}
	res++;
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return resArr;
    }
  case TableExprFuncNode::findcone3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id);
      if (srcArr.nelements() % 2 != 0) {
	throw TableInvExpr("First FINDCONE argument "
			   "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id);
      if (coneArr.nelements() % 2 != 0) {
	throw TableInvExpr("Second FINDCONE3 argument "
			   "must have multiple of 2 values");
      }
      // Radius can be a single value or an array.
      int nrrad = 1;
      double radval;
      const double* rad = 0;
      Array<double> radArr;
      if ( operands()[2]->valueType() == VTArray) {
	radArr = operands()[2]->getArrayDouble(id);
	nrrad = radArr.nelements();
      } else {
	radval = operands()[2]->getDouble(id);
	rad = &radval;
      }
      // The result shape is the source array shape.
      IPosition shpc = srcArr.shape();
      IPosition shp;
      if (shpc.nelements() > 1  &&  shpc[0] == 2) {
	shp = shpc.getLast (shpc.nelements() - 1);
      } else {
	shp = shpc;
	shp[0] = shp[0] / 2;
      }
      Bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
	rad = radArr.getStorage (deleteRad);
      }
      Array<Int64> resArr(shp);
      Int64* res = resArr.data();
      for (uInt j=0; j<srcArr.nelements(); j+=2) {
	const double ra  = src[j];
	const double dec = src[j+1];
	*res = -1;
	for (uInt i=0; i<coneArr.nelements(); i+=2) {
	  const double raCone  = cone[i];
	  const double decCone = cone[i+1];
	  const double radius  = cone[i+2];
	  double dist = sin(decCone) * sin(dec) +
	                cos(decCone) * cos(dec) * cos(raCone-ra);
	  for (Int k=0; k<nrrad; k++) {
	    if (cos(radius) <= dist) {
	      *res = origin_p + k + nrrad*i/2;
	      break;
	    }
	  }
	  if (*res >= 0) break;
	}
	res++;
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      if (rad != &radval) {
	radArr.freeStorage (rad, deleteRad);
      }
      return resArr;
    }
  default:
    throw (TableInvExpr ("TableExprConeNodeArray::getArrayDouble, "
			 "unknown function"));
  }
}


} //# NAMESPACE CASACORE - END
