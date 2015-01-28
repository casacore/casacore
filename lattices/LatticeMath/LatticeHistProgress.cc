//# LatticeHistProgress.cc:  progress meter for LatticeHistograms
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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
//

#include <casacore/lattices/LatticeMath/LatticeHistProgress.h>

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/BasicSL/String.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeHistProgress::~LatticeHistProgress()
{
    delete itsMeter;
}

void LatticeHistProgress::initDerived()
//
// Initialize meter
//
{
    delete itsMeter;

// The expectedNSteps function will return the number of
// expected steps.  The number of expected steps
// is set by calling LatticeProgress::init which then
// calls this initDerived function
// 
    itsMeter = new ProgressMeter(0.0, Double(expectedNsteps()), String("Generate Storage Image"),
                                 String("Accumulation Iterations"), String(""), String(""),
                                 True, max(1,Int(expectedNsteps()/20)));
}

void LatticeHistProgress::nstepsDone (uInt nsteps)
//
// Update the meter with the number of steps taken so far
{
    itsMeter->update (nsteps);
}

void LatticeHistProgress::done()
{   
    delete itsMeter;
    itsMeter = 0;
}
 



} //# NAMESPACE CASACORE - END

