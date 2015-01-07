//# TaQLNodeVisitor.h: Class to visit the nodes in the raw TaQL parse tree
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

#ifndef TABLES_TAQLNODEVISITOR_H
#define TABLES_TAQLNODEVISITOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/TaQL/TaQLNodeDer.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to visit the nodes in the raw TaQL parse tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTableGram">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNode>TaQLNode</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis> 
// TaQLNodeVisitor is the abstract base class for classes that want to
// visit a TaQLNode tree, i.e. traverse the tree.
// Each visit results in a TaQLNodeResult object which acts as the basis
// for the actual result object.
// <br>
// A specialization of TaQLNodeVisitor (e.g. class
// <linkto class=TaQLNodeHandler>TaQLNodeHandler</linkto> needs to implement
// the various visitXXNode functions. A visit function will process a node
// which usually means visiting its children, etc..
// </synopsis>

// <motivation>
// The visitor design pattern separates the tree from the way it is processed.
// In this way any handler can be created. For instance, a query optimizer
// could be a future other handler.
// </motivation>

class TaQLNodeVisitor
{
public:
  virtual ~TaQLNodeVisitor();

  // Define the functions to visit each node type.
  // <group>
  virtual TaQLNodeResult visitConstNode    (const TaQLConstNodeRep& node) = 0;
  virtual TaQLNodeResult visitRegexNode    (const TaQLRegexNodeRep& node) = 0;
  virtual TaQLNodeResult visitUnaryNode    (const TaQLUnaryNodeRep& node) = 0;
  virtual TaQLNodeResult visitBinaryNode   (const TaQLBinaryNodeRep& node) = 0;
  virtual TaQLNodeResult visitMultiNode    (const TaQLMultiNodeRep& node) = 0;
  virtual TaQLNodeResult visitFuncNode     (const TaQLFuncNodeRep& node) = 0;
  virtual TaQLNodeResult visitRangeNode    (const TaQLRangeNodeRep& node) = 0;
  virtual TaQLNodeResult visitIndexNode    (const TaQLIndexNodeRep& node) = 0;
  virtual TaQLNodeResult visitKeyColNode   (const TaQLKeyColNodeRep& node) = 0;
  virtual TaQLNodeResult visitTableNode    (const TaQLTableNodeRep& node) = 0;
  virtual TaQLNodeResult visitColNode      (const TaQLColNodeRep& node) = 0;
  virtual TaQLNodeResult visitColumnsNode  (const TaQLColumnsNodeRep& node) = 0;
  virtual TaQLNodeResult visitJoinNode     (const TaQLJoinNodeRep& node) = 0;
  virtual TaQLNodeResult visitGroupNode    (const TaQLGroupNodeRep& node) = 0;
  virtual TaQLNodeResult visitSortKeyNode  (const TaQLSortKeyNodeRep& node) = 0;
  virtual TaQLNodeResult visitSortNode     (const TaQLSortNodeRep& node) = 0;
  virtual TaQLNodeResult visitLimitOffNode (const TaQLLimitOffNodeRep& node) = 0;
  virtual TaQLNodeResult visitGivingNode   (const TaQLGivingNodeRep& node) = 0;
  virtual TaQLNodeResult visitUpdExprNode  (const TaQLUpdExprNodeRep& node) = 0;
  virtual TaQLNodeResult visitSelectNode   (const TaQLSelectNodeRep& node) = 0;
  virtual TaQLNodeResult visitUpdateNode   (const TaQLUpdateNodeRep& node) = 0;
  virtual TaQLNodeResult visitInsertNode   (const TaQLInsertNodeRep& node) = 0;
  virtual TaQLNodeResult visitDeleteNode   (const TaQLDeleteNodeRep& node) = 0;
  virtual TaQLNodeResult visitCountNode    (const TaQLCountNodeRep& node) = 0;
  virtual TaQLNodeResult visitCalcNode     (const TaQLCalcNodeRep& node) = 0;
  virtual TaQLNodeResult visitCreTabNode   (const TaQLCreTabNodeRep& node) = 0;
  virtual TaQLNodeResult visitColSpecNode  (const TaQLColSpecNodeRep& node) = 0;
  virtual TaQLNodeResult visitRecFldNode   (const TaQLRecFldNodeRep& node) = 0;
  virtual TaQLNodeResult visitUnitNode     (const TaQLUnitNodeRep& node) = 0;
  // </group>

protected:
  // A convenience function to visit the given node using this visitor.
  TaQLNodeResult visitNode (const TaQLNode& node)
    { return node.visit (*this); }
};

} //# NAMESPACE CASACORE - END

#endif
