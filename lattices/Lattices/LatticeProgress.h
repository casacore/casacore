//# LatticeProgress.h: Abstract base class to monitor progress in lattice operations
//# Copyright (C) 1997
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

#ifndef LATTICES_LATTICEPROGRESS_H
#define LATTICES_LATTICEPROGRESS_H
 

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Vector;
class IPosition;

// <summary>
// Abstract base class to monitor progress in lattice operations
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This is an abstract base class for classes to monitor the
// progress of an operation on a Lattice. The default implementation
// offered by this class does nothing.
// However, a derived class could show the progress using for example
// a <linkto class=ProgressMeter>ProgressMeter</linkto>. A derived
// class should override the virtual functions from this class.
//
// The user of the LatticeProgress object should first call
// function <src>init</src> with the total number of steps
// that are to be done.   Thereafter, after each step has been
// executed, function <src>nstepsDone</src> should be called
// after each step.  Finally, function <src>done</src> should
// be called.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// Since operations on Lattices can take a while, it can be useful
// to show the progress. However, making module Lattices dependent on
// the class ProgressMeter sounded bad. This abstract class serves
// as a bridge between the Lattice module and the ProgressMeter class
// (or any other class showing the progress).
// </motivation>
//
//# <todo asof="1997/08/01">   
//#   <li> 
//# </todo>


class LatticeProgress
{
public:
    LatticeProgress()
	: itsExpectedNsteps(0) {}

    virtual ~LatticeProgress();

// Initialize the process.
// It sets the expected number of steps and 
// calls initDerived, so a derived class can initialize itself.
    void init (uInt expectedNsteps);

// Tell the number of steps done so far.
// The default implementation does nothing. A derived class
// should call the ProgressMeter function <src>update</src>
    virtual void nstepsDone (uInt nsteps);

// The process has ended.
    virtual void done();

// Recovers the expected number of total steps.
    uInt expectedNsteps() const
        { return itsExpectedNsteps; }

protected:
    // Let a derived class initialize itself.
    // This function is called by <src>init</src>.
    // The derived class should create the <src>ProgressMeter</src>
    // in here.
    virtual void initDerived();

private:
    uInt itsExpectedNsteps;
};



} //# NAMESPACE CASACORE - END

#endif
