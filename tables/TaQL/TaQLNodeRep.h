//# TaQLNodeRep.h: Representation of a node in the raw TaQL parse tree
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

#ifndef TABLES_TAQLNODEREP_H
#define TABLES_TAQLNODEREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TaQLNodeResult.h>
#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/casa/BasicSL/String.h>
#include <iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declaration.
class AipsIO;
class TaQLNodeVisitor;

// <summary>
// Representation of a node in the raw TaQL parse tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNode>TaQLNode</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// TaQLNode/TaQLNodeRep form an envelope/letter pair.
// TaQLNodeRep is the abstract base class for all classes used in the
// raw TaQL parse tree
// (e.g. <linkto class=TaQLConstNodeRep>TaQLConstNodeRep</linkto>).
// </synopsis> 

// <motivation>
// The envelope/letter idiom (aka counted referencing) is a nice means
// to pass an object around by value, so to ensure that an object is deleted
// in case of an exception.
// Furthermore it makes copying an object very cheap and memory
// management straightforward.
// </motivation>

class TaQLNodeRep
{
public:
  // Define the various derived types (to be stored with AipsIO).
  //# They are easier to use than an enum.
  //# Do not change these definitions, since these values are stored in files.
  // <group>
  #define TaQLNode_Null     char(0)
  #define TaQLNode_Const    char(1)
  #define TaQLNode_Unary    char(2)
  #define TaQLNode_Binary   char(3)
  #define TaQLNode_Multi    char(4)
  #define TaQLNode_Func     char(5)
  #define TaQLNode_Range    char(6)
  #define TaQLNode_Index    char(7)
  #define TaQLNode_KeyCol   char(8)
  #define TaQLNode_Table    char(9)
  #define TaQLNode_Col      char(10)
  #define TaQLNode_Columns  char(11)
  #define TaQLNode_Join     char(12)
  #define TaQLNode_SortKey  char(13)
  #define TaQLNode_Sort     char(14)
  #define TaQLNode_LimitOff char(15)
  #define TaQLNode_Giving   char(16)
  #define TaQLNode_UpdExpr  char(17)
  #define TaQLNode_Select   char(18)
  #define TaQLNode_Update   char(19)
  #define TaQLNode_Insert   char(20)
  #define TaQLNode_Delete   char(21)
  #define TaQLNode_Calc     char(22)
  #define TaQLNode_CreTab   char(23)
  #define TaQLNode_ColSpec  char(24)
  #define TaQLNode_RecFld   char(25)
  #define TaQLNode_Unit     char(26)
  #define TaQLNode_Regex    char(27)
  #define TaQLNode_Count    char(28)
  #define TaQLNode_Groupby  char(29)
  // </group>

  // Constructor for derived classes specifying the type.
  explicit TaQLNodeRep (int nodeType);

  virtual ~TaQLNodeRep();

  // Increment the reference count.
  static TaQLNodeRep* link (TaQLNodeRep* rep)
  {
    if (rep) ++rep->itsCount;
    return rep;
  }

  // Decrement the reference count.
  // Delete the letter if no more references.
  static void unlink (TaQLNodeRep* rep)
  {
    if (rep  &&  --rep->itsCount == 0) delete rep;
  }

  // Get the node type of the derived class.
  char nodeType() const
    { return itsNodeType; }

  // Get the TaQL style.
  const TaQLStyle& style() const
    { return itsStyle; }

  // Visit a node for tree traversal.
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const = 0;

  // Print the object in an ostream.
  virtual void show (std::ostream& os) const = 0;

  // Save the object.
  virtual void save (AipsIO& aio) const = 0;

  // Check the data type string and return its standard form.
  static String checkDataType (const String&);

private:
  // Letter objects cannot be copied.
  // <group>
  TaQLNodeRep (const TaQLNodeRep&);
  TaQLNodeRep& operator= (const TaQLNodeRep&);
  // </group>

  int       itsCount;
  char      itsNodeType;
  TaQLStyle itsStyle;
};


} //# NAMESPACE CASACORE - END

#endif
