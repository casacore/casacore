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

#include <casacore/tables/TaQL/ExprConeNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprConeNode::TableExprConeNode (FunctionType ftype, NodeDataType dtype,
                                      const TableExprNodeSet& source,
                                      const vector<TENShPtr>& nodes,
                                      const Block<int32_t>& dtypeOper,
                                      uint32_t origin)
  : TableExprFuncNode (ftype, dtype, VTScalar, source, nodes, dtypeOper),
    origin_p          (origin)
{}

TableExprConeNode::~TableExprConeNode()
{}

bool TableExprConeNode::getBool (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::anyconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First ANYCONE argument must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
      if (coneArr.nelements() % 3 != 0) {
        throw TableInvExpr("Second ANYCONE argument "
                           "must have multiple of 3 values");
      }
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra  = src[0];
      const double dec = src[1];
      bool res = false;
      for (size_t i=0; i<coneArr.nelements(); i+=3) {
        const double raCone  = cone[i];
        const double decCone = cone[i+1];
        const double radius  = cone[i+2];
        if (cos(radius) <= (sin(decCone) * sin(dec) +
                            cos(decCone) * cos(dec) * cos(raCone-ra))) {
          res = true;
          break;
        }
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  case TableExprFuncNode::conesFUNC:
    {
      Array<double> srcArr  = operands_p[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First CONES argument must have multiple of 2 values");
      }
      Array<double> coneArr = operands_p[1]->getArrayDouble(id).array();
      if (coneArr.nelements() != 3) {
        throw TableInvExpr("Second CONES argument "
                           "must have multiple of 3 values");
      }
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra      = src[0];
      const double dec     = src[1];
      const double raCone  = cone[0];
      const double decCone = cone[1];
      const double radius  = cone[2];
      bool res = (cos(radius) <= (sin(decCone) * sin(dec) +
                                  cos(decCone) * cos(dec) * cos(raCone - ra)));
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  case TableExprFuncNode::anycone3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First ANYCONE argument must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
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
        radArr = operands()[2]->getArrayDouble(id).array();
        nrrad = radArr.nelements();
      } else {
        radval = operands()[2]->getDouble(id);
        rad = &radval;
      }
      bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
        rad = radArr.getStorage (deleteRad);
      }
      const double ra  = src[0];
      const double dec = src[1];
      bool res = false;
      for (size_t i=0; i<coneArr.nelements(); i+=2) {
        const double raCone  = cone[i];
        const double decCone = cone[i+1];
        double dist = (sin(decCone) * sin(dec) +
                       cos(decCone) * cos(dec) * cos(raCone-ra));
        for (int32_t k=0; k<nrrad; k++) {
          const double radius  = rad[k];
          if (cos(radius) <= dist) {
            res = true;
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
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First CONES argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
      if (coneArr.nelements() % 2 != 0) {
        throw TableInvExpr("Second CONES3 argument "
                           "must have multiple of 2 values");
      }
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra      = src[0];
      const double dec     = src[1];
      const double raCone  = cone[0];
      const double decCone = cone[1];
      const double radius  = operands()[2]->getDouble(id);
      bool res = (cos(radius) <= (sin(decCone) * sin(dec) +
                                  cos(decCone) * cos(dec) * cos(raCone - ra)));
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return res;
    }
  default:
    throw (TableInvExpr ("TableExprConeNode::getBool, "
                         "unknown function"));
  }
  return true;
}

int64_t TableExprConeNode::getInt (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::findconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First FINDCONE argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
      if (coneArr.nelements() % 3 != 0) {
        throw TableInvExpr("Second FINDCONE argument "
                           "must have multiple of 3 values");
      }
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double ra  = src[0];
      const double dec = src[1];
      int64_t res = -1;
      for (size_t i=0; i<coneArr.nelements(); i+=3) {
        const double raCone  = cone[i];
        const double decCone = cone[i+1];
        const double radius  = cone[i+2];
        if (cos(radius) <= (sin(decCone) * sin(dec) +
                            cos(decCone) * cos(dec) * cos(raCone-ra))) {
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
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() != 2) {
        throw TableInvExpr("First FINDCONE argument "
                           "must have multiple 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
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
        radArr = operands()[2]->getArrayDouble(id).array();
        nrrad = radArr.nelements();
      } else {
        radval = operands()[2]->getDouble(id);
        rad = &radval;
      }
      bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
        rad = radArr.getStorage (deleteRad);
      }
      const double ra  = src[0];
      const double dec = src[1];
      int64_t res = -1;
      for (size_t i=0; i<coneArr.nelements(); i+=2) {
        const double raCone  = cone[i];
        const double decCone = cone[i+1];
        double dist = (sin(decCone) * sin(dec) +
                       cos(decCone) * cos(dec) * cos(raCone-ra));
        for (int32_t k=0; k<nrrad; k++) {
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
    throw (TableInvExpr ("TableExprConeNode::getInt, "
                         "unknown function"));
  }
  return 0;
}


TableExprNodeRep::NodeDataType TableExprConeNode::checkOperands
                                 (Block<int32_t>& dtypeOper,
                                  ValueType& resVT, Block<int32_t>&,
                                  FunctionType fType,
                                  const vector<TENShPtr>& nodes)
{
  int nrarg = 3;
  switch (fType) {
  // The 2 argument cone functions accept arrays only.
  // The result is a bool scalar or array.
  case TableExprFuncNode::conesFUNC:
  case TableExprFuncNode::anyconeFUNC:
  case TableExprFuncNode::findconeFUNC:
    nrarg = 2;      // fall through
  // The 3 argument cone functions accept a scalar or array as the 3rd argument.
  // The result is a bool scalar or array.
  case TableExprFuncNode::cones3FUNC:
  case TableExprFuncNode::anycone3FUNC:
  case TableExprFuncNode::findcone3FUNC:
    {
      checkNumOfArg (nrarg, nrarg, nodes);
      for (int32_t i=0; i<2; i++) {
        if (nodes[i]->valueType() != VTArray) {
          throw TableInvExpr
            ("First 2 arguments of CONE functions must be double arrays");
        }
      }
      // Result is a scalar or array.
      resVT = VTScalar;
      // Check the number of elements in the position node.
      int32_t nvalPos = findNelem (nodes[0]);
      int32_t nvalCone = findNelem (nodes[1]);
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

int32_t TableExprConeNode::findNelem (const TENShPtr& node)
{
  int64_t nelem = -1;
  if (node->valueType() == VTSet) {
    const TableExprNodeSet* set =
      dynamic_cast<const TableExprNodeSet*>(node.get());
    AlwaysAssert (set, AipsError);
    TENShPtr arr = set->setOrArray();
    if (arr->valueType() != VTArray) {
      throw TableInvExpr ("CONES argument is a non-array set");
    }
    nelem = arr->shape().product();
  } else {
    nelem = node->shape().product();
  }
  return nelem;
}



TableExprConeNodeArray::TableExprConeNodeArray (TableExprFuncNode::FunctionType ftype,
                                                NodeDataType dtype,
                                                const TableExprNodeSet& source,
                                                const vector<TENShPtr>& nodes,
                                                const Block<int32_t>& dtypeOper,
                                                uint32_t origin)
  : TableExprFuncNodeArray (ftype, dtype, VTArray, source,
                            nodes, dtypeOper, TaQLStyle()),
    origin_p               (origin)
{
  ndim_p = -1;
}

TableExprConeNodeArray::~TableExprConeNodeArray()
{}

MArray<bool> TableExprConeNodeArray::getArrayBool (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::conesFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() % 2 != 0) {
        throw TableInvExpr("First CONES argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
      if (coneArr.nelements() % 3 != 0) {
        throw TableInvExpr("Second CONES argument "
                           "must have multiple of 3 values");
      }
      // The result shape is a matrix (#cones, #sources).
      int32_t nsrc  = srcArr.nelements() / 2;
      int32_t ncone = coneArr.nelements() / 3;
      Array<bool> resArr(IPosition(2,ncone,nsrc));
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      bool* res = resArr.data();
      for (size_t j=0; j<srcArr.nelements(); j+=2) {
        const double ra  = src[j];
        const double sindec = sin(src[j+1]);
        const double cosdec = cos(src[j+1]);
        for (size_t i=0; i<coneArr.nelements(); i+=3) {
          const double raCone  = cone[i];
          const double decCone = cone[i+1];
          const double radius  = cone[i+2];
          *res++ = cos(radius) <= (sin(decCone) * sindec +
                                   cos(decCone) * cosdec * cos(raCone-ra));
        }
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return MArray<bool>(resArr);
    }
  case TableExprFuncNode::cones3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() % 2 != 0) {
        throw TableInvExpr("First CONES3 argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
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
        radArr = operands()[2]->getArrayDouble(id).array();
        nrrad = radArr.nelements();
      } else {
        radval = operands()[2]->getDouble(id);
        rad = &radval;
      }
      // The result shape is a cube (#radii, #cones, #sources).
      int32_t nsrc  = srcArr.nelements() / 2;
      int32_t ncone = coneArr.nelements() / 2;
      bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      if (rad != &radval) {
        rad = radArr.getStorage (deleteRad);
      }
      Array<bool> resArr(IPosition(3,nrrad,ncone,nsrc));
      bool* res = resArr.data();
      for (size_t j=0; j<srcArr.nelements(); j+=2) {
        const double ra  = src[j];
        const double dec = src[j+1];
        for (size_t i=0; i<coneArr.nelements(); i+=2) {
          const double raCone  = cone[i];
          const double decCone = cone[i+1];
          double dist = (sin(decCone) * sin(dec) +
                         cos(decCone) * cos(dec) * cos(raCone-ra));

          for (int32_t k=0; k<nrrad; k++) {
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
      return MArray<bool>(resArr);
    }
  default:
    throw (TableInvExpr ("TableExprConeNodeArray::getArrayBool, "
                         "unknown function"));
  }
}

MArray<int64_t> TableExprConeNodeArray::getArrayInt (const TableExprId& id)
{
  switch (funcType()) {
  case TableExprFuncNode::findconeFUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() % 2 != 0) {
        throw TableInvExpr("First FINDCONE argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
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
      Array<int64_t> resArr(shp);
      bool deleteSrc, deleteCone;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      int64_t* res = resArr.data();
      for (size_t j=0; j<srcArr.nelements(); j+=2) {
        const double ra  = src[j];
        const double dec = src[j+1];
        *res = -1;
        for (size_t i=0; i<coneArr.nelements(); i+=3) {
          const double raCone  = cone[i];
          const double decCone = cone[i+1];
          const double radius  = cone[i+2];
          if (cos(radius) <= (sin(decCone) * sin(dec) +
                              cos(decCone) * cos(dec) * cos(raCone-ra))) {
            *res = origin_p + i/3;
            break;
          }
        }
        res++;
      }
      srcArr.freeStorage (src, deleteSrc);
      coneArr.freeStorage (cone, deleteCone);
      return MArray<int64_t>(resArr);
    }
  case TableExprFuncNode::findcone3FUNC:
    {
      Array<double> srcArr  = operands()[0]->getArrayDouble(id).array();
      if (srcArr.nelements() % 2 != 0) {
        throw TableInvExpr("First FINDCONE argument "
                           "must have multiple of 2 values");
      }
      Array<double> coneArr = operands()[1]->getArrayDouble(id).array();
      if (coneArr.nelements() % 2 != 0) {
        throw TableInvExpr("Second FINDCONE3 argument "
                           "must have multiple of 2 values");
      }
      // Radius can be a single value or an array.
      int nrrad = 1;
      Array<double> radArr;
      if ( operands()[2]->valueType() == VTArray) {
        radArr = operands()[2]->getArrayDouble(id).array();
        nrrad = radArr.nelements();
      } else {
        radArr.resize(IPosition(1,1));
        radArr.data()[0] = operands()[2]->getDouble(id);
        nrrad = 1;
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
      bool deleteSrc, deleteCone, deleteRad;
      const double* src  = srcArr.getStorage (deleteSrc);
      const double* cone = coneArr.getStorage (deleteCone);
      const double* rad = radArr.getStorage (deleteRad);
      Array<int64_t> resArr(shp);
      int64_t* res = resArr.data();
      for (size_t j=0; j<srcArr.nelements(); j+=2) {
        const double ra  = src[j];
        const double dec = src[j+1];
        *res = -1;
        for (size_t i=0; i<coneArr.nelements(); i+=2) {
          const double raCone  = cone[i];
          const double decCone = cone[i+1];
          double dist = (sin(decCone) * sin(dec) +
                         cos(decCone) * cos(dec) * cos(raCone-ra));
          for (int32_t k=0; k<nrrad; k++) {
            if (cos(rad[k]) <= dist) {
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
      radArr.freeStorage (rad, deleteRad);
      return MArray<int64_t>(resArr);
    }
  default:
    throw (TableInvExpr ("TableExprConeNodeArray::getArrayInt, "
                         "unknown function"));
  }
}


} //# NAMESPACE CASACORE - END
