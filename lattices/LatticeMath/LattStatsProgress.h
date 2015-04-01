//# LattStatsProgress.h: progress meter for LatticeStatistics
//# Copyright (C) 1996,1997,1998,1999
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

#ifndef LATTICES_LATTSTATSPROGRESS_H
#define LATTICES_LATTSTATSPROGRESS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/LatticeProgress.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class ProgressMeter;

// <summary> Provides a progress meter for the <src>LatticeStatistics</src> class </summary>
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
// Display a progress meter for the class  <src>LatticeStatistics</src>
// </etymology>
//
// <synopsis>
//   Progress meters can be displayed by the <src>LatticeApply</src> class
//   which is used by <src>LatticeStatistics</src> in order to optimally iterate
//   through the lattice.  To do this,  one must derive a
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
 

class LattStatsProgress : public LatticeProgress
{
public:

	// Constructor makes a null object
    LattStatsProgress() : _meter(), _currentStep(0) {};

    // Destructor deletes the ProgressMeter pointer
    virtual ~LattStatsProgress();

    // increment the current step (postfix version)
    void operator++(Int);

    // Initialize this object.  Here we create the ProgressMeter
    // This function is called by the <src>init</src> in LatticeProgress
    virtual void initDerived();

    // Tell the number of steps done so far.
    virtual void nstepsDone (uInt nsteps);

    // The process has ended so clean things up.
    virtual void done();

private:
    CountedPtr<ProgressMeter> _meter;
    uInt _currentStep;
};




} //# NAMESPACE CASACORE - END

#endif
