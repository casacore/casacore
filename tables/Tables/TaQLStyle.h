//# TaQLStyle.h: Class with static members defining the TaQL style
//# Copyright (C) 2006
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

#ifndef TABLES_TAQLSTYLE_H
#define TABLES_TAQLSTYLE_H

//# Includes
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Class with static members defining the TaQL style.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis>
// Originally TaQL was developed to use the Glish style of indexing.
// This meant 1-based indices, axes in Fortran order, and end is inclusive
// in start:end.
// On the other hand the Python style is the opposite.
// In order to let the user choose between styles, one can define the
// style in a TaQL command.
// The default style is Glish.
// </synopsis> 

class TaQLStyle
{
public:
  // Default style is Glish.
  TaQLStyle (uInt origin=1)
    : itsOrigin  (origin),
      itsEndExcl (False),
      itsCOrder  (False)
    {}

  // Reset to the default Glish style.
  void reset()
    { set ("GLISH"); }

  // Set the style according to the (case-insensitive) value.
  // Possible values are Glish, Python, Base0, Base1, FortranOrder, Corder,
  // InclEnd, and ExclEnd.
  void set (const String& value);

  // Get the various style values.
  // <group>
  uInt origin() const
    { return itsOrigin; }
  Bool isEndExcl() const
    { return itsEndExcl; }
  Bool isCOrder() const
    { return itsCOrder; }
  // </group>

private:
  uInt itsOrigin;
  Bool itsEndExcl;
  Bool itsCOrder;
};


} //# NAMESPACE CASA - END

#endif
