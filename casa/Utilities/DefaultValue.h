//# DefaultValue.h: fill a variable with its default value.
//# Copyright (C) 1995,1996,2000
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
//#
//# $Id$

#ifndef CASA_DEFAULTVALUE_H
#define CASA_DEFAULTVALUE_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A templated function which sets a variable to a default value.
// </summary>

// <use visibility=export>

// <reviewed reviewer="syang" date="1996/03/14" tests="tDefaultValue.cc" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>
//
// <etymology>
// The DefaultValue function name is derived from its use to fill a data type
// with a default value, usually zero.
// </etymology>
//
// <synopsis>
// The DefaultValue function is passed an instance of a data type and the 
// variable is filled with a default value.  The majority of classes may 
// use the templated version here.  Special classes may use their own 
// non-templated specializations as demonstrated in 
// ../Utilities/test/tDefaultValue.cc.
// </synopsis>
//
// <example>
// <srcblock>
// Int foo = 35;
// defaultValue(foo);
// AlwaysAssert(foo == 0, AipsError);
// Array<Float> bar;
// defaultValue(bar);
// AlwaysAssert(allEQ(bar, 0.0f), AipsError);
// </srcblock>
// A special class may need its own implementation:
// <srcblock>
// void defaultValue(MySpecialClass &val){
//  // make a default value be all zeros
//  val.operator()(IPosition(2,3,4)) = Table.keywords().defaultval();
// };
// </srcblock>
// </example>
//
// <motivation>
// We needed a common way of setting all objects to zero or some 
// null/default value.  Specializing a templated function seemed the only way
// to reach everyone.
// </motivation>
//
// <templating arg=T>
//    <li> constructor T(Int)
//    <li> assignment operator (copy semantics)
// </templating>
//
// <thrown>
//    <li> none
// </thrown>
//
// <todo asof="1996/02/22">
//   <li> none
// </todo>

// <group name=defval>

template <class T> inline void defaultValue(T &theValue)
{
  theValue = T(0);
}

// </group>


} //# NAMESPACE CASACORE - END

#endif
