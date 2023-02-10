//# TableParseFunc.cc: Class handling TaQL functions
//# Copyright (C) 1994-2022
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

#include <casacore/tables/TaQL/TableParseFunc.h>
#include <casacore/tables/TaQL/TableParseQuery.h>
#include <casacore/tables/TaQL/TaQLJoin.h>
#include <casacore/tables/TaQL/ExprConeNode.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN  

  TableExprNode TableParseFunc::makeFuncNode (TableParseQuery* tpq,
                                              const String& fname,
                                              const TableExprNodeSet& arguments,
                                              const Vector<int>& ignoreFuncs,
                                              const TableExprInfo& tabin,
                                              const TaQLStyle& style)
  {
    TableExprInfo tabInfo(tabin);
    String name = fname;
    // See if something like xx.func is given.
    // xx can be a shorthand or a user defined function library.
    Vector<String> parts = stringToVector (name, '.');
    TableParsePair tabPair;
    if (tpq  &&  parts.size() == 2) {
      // See if xx is a shorthand. If so, use that table.
      tabPair = tpq->tableList().findTable (parts[0], False);
      if (! tabPair.table().isNull()) {
        tabInfo = tabPair.getTableInfo();
        name = parts[1];
      }
    }
    //# Determine the function type.
    TableExprFuncNode::FunctionType ftype = findFunc (name,
                                                      arguments.size(),
                                                      ignoreFuncs);
    if (ftype == TableExprFuncNode::NRFUNC) {
      // The function can be a user defined one (or unknown).
      return makeUDFNode (tpq, name, arguments, tabInfo, style);
    }
    if (tabInfo.isJoinTable()) {
      if (ftype == TableExprFuncNode::rowidFUNC) {
        if (arguments.size() > 0) {
          throw TableInvExpr("Function rowid cannot have arguments") ;
        }
        return new TaQLJoinRowid (tabInfo, tpq->joins()[tabPair.joinIndex()]);
      } else if (ftype == TableExprFuncNode::rownrFUNC) {
        throw TableInvExpr("Function rownr cannot be used on a join table");
      }
    }
    try {
      // The axes of reduction functions such as SUMS can be given as a set or as
      // individual values. Turn it into an Array object.
      uInt axarg = 1;
      switch (ftype) {
      case TableExprFuncNode::arrfractilesFUNC:
      case TableExprFuncNode::runfractileFUNC:
      case TableExprFuncNode::boxfractileFUNC:
        axarg = 2;    // fall through!!
      case TableExprFuncNode::arrsumsFUNC:
      case TableExprFuncNode::arrproductsFUNC:
      case TableExprFuncNode::arrsumsqrsFUNC:
      case TableExprFuncNode::arrminsFUNC:
      case TableExprFuncNode::arrmaxsFUNC:
      case TableExprFuncNode::arrmeansFUNC:
      case TableExprFuncNode::arrvariances0FUNC:
      case TableExprFuncNode::arrvariances1FUNC:
      case TableExprFuncNode::arrstddevs0FUNC:
      case TableExprFuncNode::arrstddevs1FUNC:
      case TableExprFuncNode::arravdevsFUNC:
      case TableExprFuncNode::arrrmssFUNC:
      case TableExprFuncNode::arrmediansFUNC:
      case TableExprFuncNode::arranysFUNC:
      case TableExprFuncNode::arrallsFUNC:
      case TableExprFuncNode::arrntruesFUNC:
      case TableExprFuncNode::arrnfalsesFUNC:
      case TableExprFuncNode::runsumFUNC:
      case TableExprFuncNode::runproductFUNC:
      case TableExprFuncNode::runsumsqrFUNC:
      case TableExprFuncNode::runminFUNC:
      case TableExprFuncNode::runmaxFUNC:
      case TableExprFuncNode::runmeanFUNC:
      case TableExprFuncNode::runvariance0FUNC:
      case TableExprFuncNode::runvariance1FUNC:
      case TableExprFuncNode::runstddev0FUNC:
      case TableExprFuncNode::runstddev1FUNC:
      case TableExprFuncNode::runavdevFUNC:
      case TableExprFuncNode::runrmsFUNC:
      case TableExprFuncNode::runmedianFUNC:
      case TableExprFuncNode::runanyFUNC:
      case TableExprFuncNode::runallFUNC:
      case TableExprFuncNode::runntrueFUNC:
      case TableExprFuncNode::runnfalseFUNC:
      case TableExprFuncNode::boxsumFUNC:
      case TableExprFuncNode::boxproductFUNC:
      case TableExprFuncNode::boxsumsqrFUNC:
      case TableExprFuncNode::boxminFUNC:
      case TableExprFuncNode::boxmaxFUNC:
      case TableExprFuncNode::boxmeanFUNC:
      case TableExprFuncNode::boxvariance0FUNC:
      case TableExprFuncNode::boxvariance1FUNC:
      case TableExprFuncNode::boxstddev0FUNC:
      case TableExprFuncNode::boxstddev1FUNC:
      case TableExprFuncNode::boxavdevFUNC:
      case TableExprFuncNode::boxrmsFUNC:
      case TableExprFuncNode::boxmedianFUNC:
      case TableExprFuncNode::boxanyFUNC:
      case TableExprFuncNode::boxallFUNC:
      case TableExprFuncNode::boxntrueFUNC:
      case TableExprFuncNode::boxnfalseFUNC:
      case TableExprFuncNode::arrayFUNC:
      case TableExprFuncNode::transposeFUNC:
      case TableExprFuncNode::areverseFUNC:
      case TableExprFuncNode::diagonalFUNC:
        if (arguments.size() >= axarg) {
          TableExprNodeSet parms;
          // Add first argument(s) to the parms.
          for (uInt i=0; i<axarg; i++) {
            parms.add (arguments[i]);
          }
          // Now handle the axes arguments.
          // They can be given as a set or as individual scalar values.
          Bool axesIsArray = False;
          if (arguments.size() == axarg) {
            // No axes given. Add default one for transpose, etc..
            axesIsArray = True;
            if (ftype == TableExprFuncNode::transposeFUNC  ||
                ftype == TableExprFuncNode::areverseFUNC   ||
                ftype == TableExprFuncNode::diagonalFUNC) {
              // Add an empty vector if no arguments given.
              TableExprNodeSetElem arg((TableExprNode(Vector<Int>())));
              parms.add (arg);
            }
          } else if (arguments.size() == axarg+1
                     &&  arguments[axarg]->isSingle()) {
            // A single set given; see if it is an array.
            const TENSEBShPtr& arg = arguments[axarg];
            if (arg->start()->valueType() == TableExprNodeRep::VTArray) {
              parms.add (arg);
              axesIsArray = True;
            }
          }
          if (!axesIsArray) {
            // Combine all axes in a single set and add to parms.
            TableExprNodeSet axes;
            for (uInt i=axarg; i<arguments.size(); i++) {
              const TENSEBShPtr arg = arguments[i];
              const TENShPtr& rep = arg->start();
              if (!rep  ||  !arg->isSingle()
                  ||  rep->valueType() != TableExprNodeRep::VTScalar
                  ||  (rep->dataType() != TableExprNodeRep::NTInt
                       &&  rep->dataType() != TableExprNodeRep::NTDouble)) {
                throw TableInvExpr ("Axes/shape arguments " +
                                    String::toString(i+1) +
                                    " are not one or more scalars"
                                    " or a single bounded range");
              }
              axes.add (arg);
            }
            parms.add (TableExprNodeSetElem(axes.setOrArray()));
          }
          return TableExprNode::newFunctionNode (ftype, parms, tabInfo, style);
        }
        break;
      case TableExprFuncNode::conesFUNC:
      case TableExprFuncNode::anyconeFUNC:
      case TableExprFuncNode::findconeFUNC:
      case TableExprFuncNode::cones3FUNC:
      case TableExprFuncNode::anycone3FUNC:
      case TableExprFuncNode::findcone3FUNC:
        return TableExprNode::newConeNode (ftype, arguments, style.origin());
      default:
        break;
      }
      return TableExprNode::newFunctionNode (ftype, arguments, tabInfo, style);
    } catch (const std::exception& x) {
      String err (x.what());
      if (err.size() > 28  &&  err.before(28) == "Error in select expression: ") {
        err = err.from(28);
      }
      throw TableInvExpr ("Erroneous use of function " + name + " - " + err);
    }
  }


  TableExprNode TableParseFunc::makeUDFNode (TableParseQuery* sel,
                                             const String& name,
                                             const TableExprNodeSet& arguments,
                                             const TableExprInfo& tabInfo,
                                             const TaQLStyle& style)
  {
    Vector<String> parts = stringToVector (name, '.');
    if (parts.size() == 1) {
      // No ., thus no UDF but a builtin function.
      throw TableInvExpr ("TaQL function " + name + " is unknown; "
                          "use 'show func' to see all functions");
    }
    TableExprNode udf;
    if (sel) {
      if (parts.size() > 2) {
        // At least 3 parts; see if the first part is a table shorthand.
        TableParsePair tabPair = sel->tableList().findTable (parts[0], False);
        if (! tabPair.table().isNull()) {
          udf = TableExprNode::newUDFNode (name.substr(parts[0].size() + 1),
                                           arguments, tabPair.getTableInfo(),
                                           style);
        }
      }
    }
    // If not created, use the full name and given (i.e. first) table.
    if (udf.isNull()) {
      udf = TableExprNode::newUDFNode (name, arguments, tabInfo, style);
    }
    // A UDF might create table column nodes, so add it to applySelNodes_p.
    if (sel) {
      sel->addApplySelNode (udf);
    }
    return udf;
  }


  TableExprFuncNode::FunctionType TableParseFunc::findFunc
  (const String& name,
   uInt narguments,
   const Vector<Int>& ignoreFuncs)
  {
    //# Determine the function type.
    //# Use the function name in lower case.
    //# Error if functype in ignoreFuncs or if ignoreFuncs is not empty and
    //# the function is an aggregate one.
    TableExprFuncNode::FunctionType ftype = TableExprFuncNode::piFUNC;
    String funcName (name);
    funcName.downcase();
    if (funcName == "pi") {
      ftype = TableExprFuncNode::piFUNC;
    } else if (funcName == "e") {
      ftype = TableExprFuncNode::eFUNC;
    } else if (funcName == "c") {
      ftype = TableExprFuncNode::cFUNC;
    } else if (funcName == "near") {
      ftype = TableExprFuncNode::near2FUNC;
      if (narguments == 3) {
        ftype = TableExprFuncNode::near3FUNC;
      }
    } else if (funcName == "nearabs") {
      ftype = TableExprFuncNode::nearabs2FUNC;
      if (narguments == 3) {
        ftype = TableExprFuncNode::nearabs3FUNC;
      }
    } else if (funcName == "sin") {
      ftype = TableExprFuncNode::sinFUNC;
    } else if (funcName == "sinh") {
      ftype = TableExprFuncNode::sinhFUNC;
    } else if (funcName == "cos") {
      ftype = TableExprFuncNode::cosFUNC;
    } else if (funcName == "cosh") {
      ftype = TableExprFuncNode::coshFUNC;
    } else if (funcName == "exp") {
      ftype = TableExprFuncNode::expFUNC;
    } else if (funcName == "log"  ||  funcName == "ln") {
      ftype = TableExprFuncNode::logFUNC;
    } else if (funcName == "log10") {
      ftype = TableExprFuncNode::log10FUNC;
    } else if (funcName == "sqrt") {
      ftype = TableExprFuncNode::sqrtFUNC;
    } else if (funcName == "pow") {
      ftype = TableExprFuncNode::powFUNC;
    } else if (funcName == "conj") {
      ftype = TableExprFuncNode::conjFUNC;
    } else if (funcName == "square"  ||  funcName == "sqr") {
      ftype = TableExprFuncNode::squareFUNC;
    } else if (funcName == "cube") {
      ftype = TableExprFuncNode::cubeFUNC;
    } else if (funcName == "min") {
      ftype = TableExprFuncNode::minFUNC;
      if (narguments == 1) {
        ftype = TableExprFuncNode::arrminFUNC;
      }
    } else if (funcName == "max") {
      ftype = TableExprFuncNode::maxFUNC;
      if (narguments == 1) {
        ftype = TableExprFuncNode::arrmaxFUNC;
      }
    } else if (funcName == "norm") {
      ftype = TableExprFuncNode::normFUNC;
    } else if (funcName == "abs"  ||  funcName == "amplitude"  ||
               funcName == "ampl") {
      ftype = TableExprFuncNode::absFUNC;
    } else if (funcName == "arg"  ||  funcName == "phase") {
      ftype = TableExprFuncNode::argFUNC;
    } else if (funcName == "real") {
      ftype = TableExprFuncNode::realFUNC;
    } else if (funcName == "imag") {
      ftype = TableExprFuncNode::imagFUNC;
    } else if (funcName == "int"  ||  funcName == "integer") {
      ftype = TableExprFuncNode::intFUNC;
    } else if (funcName == "asin") {
      ftype = TableExprFuncNode::asinFUNC;
    } else if (funcName == "acos") {
      ftype = TableExprFuncNode::acosFUNC;
    } else if (funcName == "atan") {
      ftype = TableExprFuncNode::atanFUNC;
    } else if (funcName == "atan2") {
      ftype = TableExprFuncNode::atan2FUNC;
    } else if (funcName == "tan") {
      ftype = TableExprFuncNode::tanFUNC;
    } else if (funcName == "tanh") {
      ftype = TableExprFuncNode::tanhFUNC;
    } else if (funcName == "sign") {
      ftype = TableExprFuncNode::signFUNC;
    } else if (funcName == "round") {
      ftype = TableExprFuncNode::roundFUNC;
    } else if (funcName == "floor") {
      ftype = TableExprFuncNode::floorFUNC;
    } else if (funcName == "ceil") {
      ftype = TableExprFuncNode::ceilFUNC;
    } else if (funcName == "fmod") {
      ftype = TableExprFuncNode::fmodFUNC;
    } else if (funcName == "complex"  ||  funcName == "formcomplex") {
      ftype = TableExprFuncNode::complexFUNC;
    } else if (funcName == "sum") {
      ftype = TableExprFuncNode::arrsumFUNC;
    } else if (funcName == "sums") {
      ftype = TableExprFuncNode::arrsumsFUNC;
    } else if (funcName == "runningsum") {
      ftype = TableExprFuncNode::runsumFUNC;
    } else if (funcName == "boxedsum") {
      ftype = TableExprFuncNode::boxsumFUNC;
    } else if (funcName == "product") {
      ftype = TableExprFuncNode::arrproductFUNC;
    } else if (funcName == "products") {
      ftype = TableExprFuncNode::arrproductsFUNC;
    } else if (funcName == "runningproduct") {
      ftype = TableExprFuncNode::runproductFUNC;
    } else if (funcName == "boxedproduct") {
      ftype = TableExprFuncNode::boxproductFUNC;
    } else if (funcName == "sumsqr"  ||  funcName == "sumsquare") {
      ftype = TableExprFuncNode::arrsumsqrFUNC;
    } else if (funcName == "sumsqrs"  ||  funcName == "sumsquares") {
      ftype = TableExprFuncNode::arrsumsqrsFUNC;
    } else if (funcName == "runningsumsqr"  ||  funcName == "runningsumsquare") {
      ftype = TableExprFuncNode::runsumsqrFUNC;
    } else if (funcName == "boxedsumsqr"  ||  funcName == "boxedsumsquare") {
      ftype = TableExprFuncNode::boxsumsqrFUNC;
    } else if (funcName == "mins") {
      ftype = TableExprFuncNode::arrminsFUNC;
    } else if (funcName == "runningmin") {
      ftype = TableExprFuncNode::runminFUNC;
    } else if (funcName == "boxedmin") {
      ftype = TableExprFuncNode::boxminFUNC;
    } else if (funcName == "maxs") {
      ftype = TableExprFuncNode::arrmaxsFUNC;
    } else if (funcName == "runningmax") {
      ftype = TableExprFuncNode::runmaxFUNC;
    } else if (funcName == "boxedmax") {
      ftype = TableExprFuncNode::boxmaxFUNC;
    } else if (funcName == "mean"  ||  funcName == "avg") {
      ftype = TableExprFuncNode::arrmeanFUNC;
    } else if (funcName == "means"  ||  funcName == "avgs") {
      ftype = TableExprFuncNode::arrmeansFUNC;
    } else if (funcName == "runningmean"  ||  funcName == "runningavg") {
      ftype = TableExprFuncNode::runmeanFUNC;
    } else if (funcName == "boxedmean"  ||  funcName == "boxedavg") {
      ftype = TableExprFuncNode::boxmeanFUNC;
    } else if (funcName == "variance") {
      ftype = TableExprFuncNode::arrvariance0FUNC;
    } else if (funcName == "variances") {
      ftype = TableExprFuncNode::arrvariances0FUNC;
    } else if (funcName == "runningvariance") {
      ftype = TableExprFuncNode::runvariance0FUNC;
    } else if (funcName == "boxedvariance") {
      ftype = TableExprFuncNode::boxvariance0FUNC;
    } else if (funcName == "samplevariance") {
      ftype = TableExprFuncNode::arrvariance1FUNC;
    } else if (funcName == "samplevariances") {
      ftype = TableExprFuncNode::arrvariances1FUNC;
    } else if (funcName == "runningsamplevariance") {
      ftype = TableExprFuncNode::runvariance1FUNC;
    } else if (funcName == "boxedsamplevariance") {
      ftype = TableExprFuncNode::boxvariance1FUNC;
    } else if (funcName == "stddev") {
      ftype = TableExprFuncNode::arrstddev0FUNC;
    } else if (funcName == "stddevs") {
      ftype = TableExprFuncNode::arrstddevs0FUNC;
    } else if (funcName == "runningstddev") {
      ftype = TableExprFuncNode::runstddev0FUNC;
    } else if (funcName == "boxedstddev") {
      ftype = TableExprFuncNode::boxstddev0FUNC;
    } else if (funcName == "samplestddev") {
      ftype = TableExprFuncNode::arrstddev1FUNC;
    } else if (funcName == "samplestddevs") {
      ftype = TableExprFuncNode::arrstddevs1FUNC;
    } else if (funcName == "runningsamplestddev") {
      ftype = TableExprFuncNode::runstddev1FUNC;
    } else if (funcName == "boxedsamplestddev") {
      ftype = TableExprFuncNode::boxstddev1FUNC;
    } else if (funcName == "avdev") {
      ftype = TableExprFuncNode::arravdevFUNC;
    } else if (funcName == "avdevs") {
      ftype = TableExprFuncNode::arravdevsFUNC;
    } else if (funcName == "runningavdev") {
      ftype = TableExprFuncNode::runavdevFUNC;
    } else if (funcName == "boxedavdev") {
      ftype = TableExprFuncNode::boxavdevFUNC;
    } else if (funcName == "rms") {
      ftype = TableExprFuncNode::arrrmsFUNC;
    } else if (funcName == "rmss") {
      ftype = TableExprFuncNode::arrrmssFUNC;
    } else if (funcName == "runningrms") {
      ftype = TableExprFuncNode::runrmsFUNC;
    } else if (funcName == "boxedrms") {
      ftype = TableExprFuncNode::boxrmsFUNC;
    } else if (funcName == "median") {
      ftype = TableExprFuncNode::arrmedianFUNC;
    } else if (funcName == "medians") {
      ftype = TableExprFuncNode::arrmediansFUNC;
    } else if (funcName == "runningmedian") {
      ftype = TableExprFuncNode::runmedianFUNC;
    } else if (funcName == "boxedmedian") {
      ftype = TableExprFuncNode::boxmedianFUNC;
    } else if (funcName == "fractile") {
      ftype = TableExprFuncNode::arrfractileFUNC;
    } else if (funcName == "fractiles") {
      ftype = TableExprFuncNode::arrfractilesFUNC;
    } else if (funcName == "runningfractile") {
      ftype = TableExprFuncNode::runfractileFUNC;
    } else if (funcName == "boxedfractile") {
      ftype = TableExprFuncNode::boxfractileFUNC;
    } else if (funcName == "any") {
      ftype = TableExprFuncNode::arranyFUNC;
    } else if (funcName == "anys") {
      ftype = TableExprFuncNode::arranysFUNC;
    } else if (funcName == "runningany") {
      ftype = TableExprFuncNode::runanyFUNC;
    } else if (funcName == "boxedany") {
      ftype = TableExprFuncNode::boxanyFUNC;
    } else if (funcName == "all") {
      ftype = TableExprFuncNode::arrallFUNC;
    } else if (funcName == "alls") {
      ftype = TableExprFuncNode::arrallsFUNC;
    } else if (funcName == "runningall") {
      ftype = TableExprFuncNode::runallFUNC;
    } else if (funcName == "boxedall") {
      ftype = TableExprFuncNode::boxallFUNC;
    } else if (funcName == "ntrue") {
      ftype = TableExprFuncNode::arrntrueFUNC;
    } else if (funcName == "ntrues") {
      ftype = TableExprFuncNode::arrntruesFUNC;
    } else if (funcName == "runningntrue") {
      ftype = TableExprFuncNode::runntrueFUNC;
    } else if (funcName == "boxedntrue") {
      ftype = TableExprFuncNode::boxntrueFUNC;
    } else if (funcName == "nfalse") {
      ftype = TableExprFuncNode::arrnfalseFUNC;
    } else if (funcName == "nfalses") {
      ftype = TableExprFuncNode::arrnfalsesFUNC;
    } else if (funcName == "runningnfalse") {
      ftype = TableExprFuncNode::runnfalseFUNC;
    } else if (funcName == "boxednfalse") {
      ftype = TableExprFuncNode::boxnfalseFUNC;
    } else if (funcName == "array") {
      ftype = TableExprFuncNode::arrayFUNC;
    } else if (funcName == "transpose") {
      ftype = TableExprFuncNode::transposeFUNC;
    } else if (funcName == "reversearray"  ||  funcName == "areverse") {
      ftype = TableExprFuncNode::areverseFUNC;
    } else if (funcName == "diagonal"  ||  funcName == "diagonals") {
      ftype = TableExprFuncNode::diagonalFUNC;
    } else if (funcName == "resize") {
      ftype = TableExprFuncNode::resizeFUNC;
    } else if (funcName == "isnan") {
      ftype = TableExprFuncNode::isnanFUNC;
    } else if (funcName == "isinf") {
      ftype = TableExprFuncNode::isinfFUNC;
    } else if (funcName == "isfinite") {
      ftype = TableExprFuncNode::isfiniteFUNC;
    } else if (funcName == "isdefined") {
      ftype = TableExprFuncNode::isdefFUNC;
    } else if (funcName == "isnull") {
      ftype = TableExprFuncNode::isnullFUNC;
    } else if (funcName == "iscolumn") {
      ftype = TableExprFuncNode::iscolFUNC;
    } else if (funcName == "iskeyword") {
      ftype = TableExprFuncNode::iskeyFUNC;
    } else if (funcName == "ndim") {
      ftype = TableExprFuncNode::ndimFUNC;
    } else if (funcName == "nelements"  ||  funcName == "count") {
      ftype = TableExprFuncNode::nelemFUNC;
    } else if (funcName == "shape") {
      ftype = TableExprFuncNode::shapeFUNC;
    } else if (funcName == "strlength" ||  funcName == "len") {
      ftype = TableExprFuncNode::strlengthFUNC;
    } else if (funcName == "upcase"    ||  funcName == "upper"  ||
               funcName == "toupper"   ||  funcName == "to_upper") {
      ftype = TableExprFuncNode::upcaseFUNC;
    } else if (funcName == "downcase"  ||  funcName == "lower"  ||
               funcName == "tolower"   ||  funcName == "to_lower") {
      ftype = TableExprFuncNode::downcaseFUNC;
    } else if (funcName == "capitalize") {
      ftype = TableExprFuncNode::capitalizeFUNC;
    } else if (funcName == "reversestring"  ||  funcName == "sreverse") {
      ftype = TableExprFuncNode::sreverseFUNC;
    } else if (funcName == "trim") {
      ftype = TableExprFuncNode::trimFUNC;
    } else if (funcName == "ltrim") {
      ftype = TableExprFuncNode::ltrimFUNC;
    } else if (funcName == "rtrim") {
      ftype = TableExprFuncNode::rtrimFUNC;
    } else if (funcName == "substr"  ||  funcName == "substring") {
      ftype = TableExprFuncNode::substrFUNC;
    } else if (funcName == "replace") {
      ftype = TableExprFuncNode::replaceFUNC;
    } else if (funcName == "regex") {
      ftype = TableExprFuncNode::regexFUNC;
    } else if (funcName == "pattern") {
      ftype = TableExprFuncNode::patternFUNC;
    } else if (funcName == "sqlpattern") {
      ftype = TableExprFuncNode::sqlpatternFUNC;
    } else if (funcName == "datetime") {
      ftype = TableExprFuncNode::datetimeFUNC;
    } else if (funcName == "mjdtodate") {
      ftype = TableExprFuncNode::mjdtodateFUNC;
    } else if (funcName == "mjd") {
      ftype = TableExprFuncNode::mjdFUNC;
    } else if (funcName == "date") {
      ftype = TableExprFuncNode::dateFUNC;
    } else if (funcName == "time") {
      ftype = TableExprFuncNode::timeFUNC;
    } else if (funcName == "year") {
      ftype = TableExprFuncNode::yearFUNC;
    } else if (funcName == "month") {
      ftype = TableExprFuncNode::monthFUNC;
    } else if (funcName == "day") {
      ftype = TableExprFuncNode::dayFUNC;
    } else if (funcName == "cmonth") {
      ftype = TableExprFuncNode::cmonthFUNC;
    } else if (funcName == "weekday"   ||  funcName == "dow") {
      ftype = TableExprFuncNode::weekdayFUNC;
    } else if (funcName == "cweekday"   ||  funcName == "cdow") {
      ftype = TableExprFuncNode::cdowFUNC;
    } else if (funcName == "week") {
      ftype = TableExprFuncNode::weekFUNC;
    } else if (funcName == "cdatetime"  ||  funcName == "ctod") {
      ftype = TableExprFuncNode::ctodFUNC;
    } else if (funcName == "cdate") {
      ftype = TableExprFuncNode::cdateFUNC;
    } else if (funcName == "ctime") {
      ftype = TableExprFuncNode::ctimeFUNC;
    } else if (funcName == "string"  ||  funcName == "str") {
      ftype = TableExprFuncNode::stringFUNC;
    } else if (funcName == "hms") {
      ftype = TableExprFuncNode::hmsFUNC;
    } else if (funcName == "dms") {
      ftype = TableExprFuncNode::dmsFUNC;
    } else if (funcName == "hdms") {
      ftype = TableExprFuncNode::hdmsFUNC;
    } else if (funcName == "rand") {
      ftype = TableExprFuncNode::randFUNC;
    } else if (funcName == "rownumber"  ||  funcName == "rownr") {
      ftype = TableExprFuncNode::rownrFUNC;
    } else if (funcName == "rowid") {
      ftype = TableExprFuncNode::rowidFUNC;
    } else if (funcName == "iif") {
      ftype = TableExprFuncNode::iifFUNC;
    } else if (funcName == "angdist"  ||  funcName == "angulardistance") {
      ftype = TableExprFuncNode::angdistFUNC;
    } else if (funcName == "angdistx"  ||  funcName == "angulardistancex") {
      ftype = TableExprFuncNode::angdistxFUNC;
    } else if (funcName == "normangle") {
      ftype = TableExprFuncNode::normangleFUNC;
    } else if (funcName == "cones") {
      ftype = TableExprConeNode::conesFUNC;
      if (narguments == 3) {
        ftype = TableExprConeNode::cones3FUNC;
      }
    } else if (funcName == "anycone") {
      ftype = TableExprConeNode::anyconeFUNC;
      if (narguments == 3) {
        ftype = TableExprConeNode::anycone3FUNC;
      }
    } else if (funcName == "findcone") {
      ftype = TableExprConeNode::findconeFUNC;
      if (narguments == 3) {
        ftype = TableExprConeNode::findcone3FUNC;
      }
    } else if (funcName == "bool"  ||  funcName == "boolean") {
      ftype = TableExprFuncNode::boolFUNC;
    } else if (funcName == "nullarray") {
      ftype = TableExprFuncNode::nullarrayFUNC;
    } else if (funcName == "marray") {
      ftype = TableExprFuncNode::marrayFUNC;
    } else if (funcName == "arraydata") {
      ftype = TableExprFuncNode::arrdataFUNC;
    } else if (funcName == "mask"  ||  funcName == "arraymask") {
      ftype = TableExprFuncNode::arrmaskFUNC;
    } else if (funcName == "negatemask") {
      ftype = TableExprFuncNode::negatemaskFUNC;
    } else if (funcName == "replacemasked") {
      ftype = TableExprFuncNode::replmaskedFUNC;
    } else if (funcName == "replaceunmasked") {
      ftype = TableExprFuncNode::replunmaskedFUNC;
    } else if (funcName == "flatten"  ||  funcName == "arrayflatten") {
      ftype = TableExprFuncNode::arrflatFUNC;
    } else if (funcName == "countall") {
      ftype = TableExprFuncNode::countallFUNC;
    } else if (funcName == "gcount") {
      ftype = TableExprFuncNode::gcountFUNC;
    } else if (funcName == "gfirst") {
      ftype = TableExprFuncNode::gfirstFUNC;
    } else if (funcName == "glast") {
      ftype = TableExprFuncNode::glastFUNC;
    } else if (funcName == "gmin") {
      ftype = TableExprFuncNode::gminFUNC;
    } else if (funcName == "gmins") {
      ftype = TableExprFuncNode::gminsFUNC;
    } else if (funcName == "gmax") {
      ftype = TableExprFuncNode::gmaxFUNC;
    } else if (funcName == "gmaxs") {
      ftype = TableExprFuncNode::gmaxsFUNC;
    } else if (funcName == "gsum") {
      ftype = TableExprFuncNode::gsumFUNC;
    } else if (funcName == "gsums") {
      ftype = TableExprFuncNode::gsumsFUNC;
    } else if (funcName == "gproduct") {
      ftype = TableExprFuncNode::gproductFUNC;
    } else if (funcName == "gproducts") {
      ftype = TableExprFuncNode::gproductsFUNC;
    } else if (funcName == "gsumsqr"  ||  funcName == "gsumsquare") {
      ftype = TableExprFuncNode::gsumsqrFUNC;
    } else if (funcName == "gsumsqrs"  ||  funcName == "gsumsquares") {
      ftype = TableExprFuncNode::gsumsqrsFUNC;
    } else if (funcName == "gmean"  ||  funcName == "gavg") {
      ftype = TableExprFuncNode::gmeanFUNC;
    } else if (funcName == "gmeans"  ||  funcName == "gavgs") {
      ftype = TableExprFuncNode::gmeansFUNC;
    } else if (funcName == "gvariance") {
      ftype = TableExprFuncNode::gvariance0FUNC;
    } else if (funcName == "gvariances") {
      ftype = TableExprFuncNode::gvariances0FUNC;
    } else if (funcName == "gsamplevariance") {
      ftype = TableExprFuncNode::gvariance1FUNC;
    } else if (funcName == "gsamplevariances") {
      ftype = TableExprFuncNode::gvariances1FUNC;
    } else if (funcName == "gstddev") {
      ftype = TableExprFuncNode::gstddev0FUNC;
    } else if (funcName == "gstddevs") {
      ftype = TableExprFuncNode::gstddevs0FUNC;
    } else if (funcName == "gsamplestddev") {
      ftype = TableExprFuncNode::gstddev1FUNC;
    } else if (funcName == "gsamplestddevs") {
      ftype = TableExprFuncNode::gstddevs1FUNC;
    } else if (funcName == "grms") {
      ftype = TableExprFuncNode::grmsFUNC;
    } else if (funcName == "grmss") {
      ftype = TableExprFuncNode::grmssFUNC;
    } else if (funcName == "gany") {
      ftype = TableExprFuncNode::ganyFUNC;
    } else if (funcName == "ganys") {
      ftype = TableExprFuncNode::ganysFUNC;
    } else if (funcName == "gall") {
      ftype = TableExprFuncNode::gallFUNC;
    } else if (funcName == "galls") {
      ftype = TableExprFuncNode::gallsFUNC;
    } else if (funcName == "gntrue") {
      ftype = TableExprFuncNode::gntrueFUNC;
    } else if (funcName == "gntrues") {
      ftype = TableExprFuncNode::gntruesFUNC;
    } else if (funcName == "gnfalse") {
      ftype = TableExprFuncNode::gnfalseFUNC;
    } else if (funcName == "gnfalses") {
      ftype = TableExprFuncNode::gnfalsesFUNC;
    } else if (funcName == "ghist"  ||  funcName == "ghistogram") {
      ftype = TableExprFuncNode::ghistFUNC;
    } else if (funcName == "gaggr"  ||  funcName == "gstack") {
      ftype = TableExprFuncNode::gaggrFUNC;
    } else if (funcName == "growid") {
      ftype = TableExprFuncNode::growidFUNC;
    } else if (funcName == "gmedian") {
      ftype = TableExprFuncNode::gmedianFUNC;
    } else if (funcName == "gfractile") {
      ftype = TableExprFuncNode::gfractileFUNC;
    } else {
      // unknown name can be a user-defined function.
      ftype = TableExprFuncNode::NRFUNC;
    }
    // Functions to be ignored are incorrect.
    Bool found;
    linearSearch (found, ignoreFuncs, Int(ftype), ignoreFuncs.size());
    if (found  ||  (!ignoreFuncs.empty()  &&
                    ftype >= TableExprFuncNode::FirstAggrFunc)) {
      throw (TableInvExpr ("Function '" + funcName +
                           "' can only be used in TaQL"));
    }
    return ftype;
  }


} //# NAMESPACE CASACORE - END
