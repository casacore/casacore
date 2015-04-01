//# LatticeHistProgress.h: progress meter for LatticeHistograms
//# Copyright (C) 1996,1997,1999,2000
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

#ifndef LATTICES_LATTICEHISTPROGRESS_H
#define LATTICES_LATTICEHISTPROGRESS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class ProgressMeter;


// <summary> Provides a progress meter for the <src>LatticeHistograms</src> class </summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto module=Lattices>LatticeProgress</linkto>
// </prerequisite>
//
// <etymology>
// Display a progress meter for the class  <src>LatticeHistograms</src>
// </etymology>
//
// <synopsis>
//   Progress meters can be displayed by the <src>LatticeApply</src> class
//   which is used by <src>LatticeHistograms</src> in order to optimally iterate
//   through the image.  To do this,  one must derive a
//   class from <src>LatticeProgress</src>. <src>LatticeApply</src> calls 
//   methods declared in <src>LatticeProgress</src> and  implemented in 
//   the derived class.
// </synopsis>
//
// <motivation>
//  I like progress meters !
// </motivation>
//
// <todo asof="1998/01/10">
// </todo>
  
class LatticeHistProgress : public LatticeProgress
{   
public:
 
// Constructor makes a null object
    LatticeHistProgress() : itsMeter(0) {};
 
// Destructor deletes the ProgressMeter pointer
    virtual ~LatticeHistProgress();
 
// Initialize this object.  Here we create the ProgressMeter
// This function is called by the <src>init</src> in LatticeProgress
    virtual void initDerived();
  
// Tell the number of steps done so far.
    virtual void nstepsDone (uInt nsteps);
 
// The process has ended so clean things up.
    virtual void done();
 
private:
    ProgressMeter* itsMeter;
};



} //# NAMESPACE CASACORE - END

#endif



