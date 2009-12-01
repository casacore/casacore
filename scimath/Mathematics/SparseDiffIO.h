//# SparseDiffIO.h: Output for SparseDiff objects
//# Copyright (C) 2007
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
//# $Id: SparseDiffIO.h,v 1.1 2007/11/16 04:34:46 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFFIO_H
#define SCIMATH_SPARSEDIFFIO_H


//# Includes
#include <casa/aips.h>
#include <casa/iosfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declarations
  template <class T> class SparseDiff;

  // <summary>
  // Implements all IO operators and functions for SparseDiff.
  // </summary>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tSparseDiff" demos="">
  // </reviewed>
  //
  // <prerequisite>
  // <li> <linkto class=SparseDiff>SparseDiff</linkto> class
  // </prerequisite>
  //
  // <etymology>
  // Implements all IO operators and functions for SparseDiff.
  // </etymology>
  //
  // <todo asof="2001/08/12">
  //  <li> Nothing I know of
  // </todo>
 
  // <group name="SparseDiff IO operations">
  template<class T>
  ostream &operator << (ostream &os, const SparseDiff<T> &ad);
  // </group>



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Mathematics/SparseDiffIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
