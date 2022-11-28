//# TaQLNodeResult.h: Classes holding the result of a node tree visit
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

#ifndef TABLES_TAQLNODERESULT_H
#define TABLES_TAQLNODERESULT_H

//# Includes
#include <casacore/casa/aips.h>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Abstract base class to hold the result of a visit to the node tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTableGram">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeVisitor>TaQLNodeVisitor</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// TaQLNodeResultRep is the abstract base class for classes holding
// values filled by visitors to the raw TaQL parse tree. Visitors are
// classes derived from <linkto class=TaQLNodeVisitor>TaQLNodeVisitor</linkto>
// which traverse the parse tree.
// TaQLNodeResultRep is the counted referenced letter class in the envelope
// class <linkto class=TaQLNodeResult>TaQLNodeResult</linkto>.
// </synopsis>

class TaQLNodeResultRep
{
public:
  // Default constructor.
  TaQLNodeResultRep()
  {}

  // Destructor.
  virtual ~TaQLNodeResultRep() = default;

  // Letter objects cannot be copied.
  // <group>
  TaQLNodeResultRep (const TaQLNodeResultRep&) = delete;
  TaQLNodeResultRep& operator= (const TaQLNodeResultRep&) = delete;
  // </group>
};


// <summary>
// Envelope class to hold the result of a visit to the node tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTableGram">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeVisitor>TaQLNodeVisitor</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// TaQLNodeResult is the envelope class for classes holding
// values filled by visitors to the raw TaQL parse tree. Visitors are
// classes derived from <linkto class=TaQLNodeVisitor>TaQLNodeVisitor</linkto>
// which traverse the parse tree.
// The counted referenced letter base class for the envelope is
// class <linkto class=TaQLNodeResultRep>TaQLNodeResultRep</linkto>.
// </synopsis>
class TaQLNodeResult
{
public:
  // Default constructor has no letter.
  TaQLNodeResult()
  {}

  // Wrap the given pointer in a shared_ptr.
  TaQLNodeResult (TaQLNodeResultRep* rep)
    : itsRep (rep)
  {}

  // Does the envelope hold a letter?
  Bool isValid() const
    { return itsRep.get(); }

  // Get the actual underlying object.
  const TaQLNodeResultRep& getRep() const
    { return *(itsRep.get()); }

private:
  std::shared_ptr<TaQLNodeResultRep> itsRep;
};

} //# NAMESPACE CASACORE - END

#endif
