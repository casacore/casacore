//# MSSelUtil2.h: templated helper function for MSSelector
//# Copyright (C) 1997,1999,2000,2001
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
//# $Id: HostInfoDarwin.h 21521 2014-12-10 08:06:42Z gervandiepen $

#ifndef MS_MSSELUTIL2_H
#define MS_MSSELUTIL2_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
template <class T> class MSSelUtil2
{
  public:
  // reorder data from 3d (corr,chan,row) to 4d (corr,chan,ifr,time)
  static void reorderData(Array<T>& data,
                          const Vector<Int>& ifrSlot,
                          Int nIfr, 
                          const Vector<Int>& timeSlot, 
                          Int nTime,
                          const T& defvalue);

  // reorder data from 4d (corr,chan,ifr,time) to 3d (corr,chan,row) 
  static void reorderData(Array<T>& data, 
                          const Matrix<Int>& rowIndex,
                          Int nRow);

  // average data (with flags & weights applied) over it's last axis (time or
  // row), return in data (overwritten), dataFlag gives new flags.
  static void timeAverage(Array<Bool>& dataFlag, Array<T>& data, 
                          const Array<Bool>& flag, const Array<Float>& weight);

};
} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/ms/MSSel/MSSelUtil2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
