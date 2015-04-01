//# BasicSL.h: Basic standard library related classes.
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


#ifndef CASA_BASICSL_H
#define CASA_BASICSL_H

#include <casacore/casa/aips.h>

#include <casacore/casa/BasicSL/Complexfwd.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/IComplex.h>
#include <casacore/casa/BasicSL/RegexBase.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/BasicSL/STLMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary> Classes and global functions for standard library use </summary>

// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <synopsis>
//
// This module is a bag of related standard library classes and
// global functions.
//
// The following functionality is available:
// <ul>
//  <li> Forward declarations
//       <linkto group="Complexfwd.h#Complexfwd">
//       Complexfwd</linkto>
//       for complex numbers.
//  <li> Class <linkto group="Complex.h#Complex_desc">
//       Complex</linkto>
//       to offer single and double precision complex numbers.
//  <li> Class <linkto class=IComplex:description>
//       IComplex</linkto>
//       for integer complex numbers.
//  <li> Value <linkto class=Dummy_Constants_class:description>
//       Constants</linkto>
//       to offer mathematical and numerical constants.
//  <li> Class <linkto class=RegexBase:description>
//       RegexBase</linkto>
//       to offer abstract interface to regular expressions for
//       <linkto class=String>String</linkto>.
//  <li> Class <linkto class=String:description>
//       String</linkto>
//       for handling character strings.
//  <li> STLMath.h has some functions doing math on std::vector objects.
//  <li> STLIO.h has some functions showing a container on std::ostream or LogIO.
// </ul>
//
// <note role=tip> You may want to look at the individual header files
// to see whether you might not prefer to include only the header
// files you really need; it may be more efficient to do so.
// </note>
//
// </synopsis>

//# <todo asof="2005/06/08">
//#   <li>
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

