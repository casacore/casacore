//# NewMSSelUtil.h: this defines NewMSSelUtil, a helper class for NewMSSelector
//# Copyright (C) 1998,1999,2000
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

#if !defined(TRIAL_NEWMSSELUTIL_H)
#define TRIAL_NEWMSSELUTIL_H

#include <aips/aips.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Complex.h>

// <summary> 
// Helper class for NewMSFlagger with templated static function
// </summary>
// <synopsis> 
// Helper class for NewMSFlagger/DOms with templated static function to difference
// data in one of two directions.
// </synopsis> 
//<visibility=local>

template <class T> class NewMSSelUtil
{
public:
  // Compute the absolute difference of the data, subtracting
  // either the previous value (window==2) or the average over
  // the window (window>2). If doMedian==True is specified, the
  // median difference over the window is returned for window>2.
  // Takes flagging into account.
  // diffAxis==2,3: row or time, diffAxis==1: channel
  // Handles 3d and 4d data arrays.
  static Array<Float> diffData(const Array<T>& data,
			       const Array<Bool>& flag,
			       const Array<Bool>& flagRow,
			       Int diffAxis,
			       Int window,
			       Bool doMedian=False);
};

// <summary> 
// Helper class for NewMSSelector/DOms with templated static functions
// </summary>
// <synopsis> 
// Helper class for NewMSSelector/DOms with templated static functions to 
// reorder data to include or exclude an interferometer axis.
// </synopsis> 
//<visibility=local>
template <class T> class NewMSSelUtil2
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
};
#endif





