//# LatticeCleanProgress.h: Abstract base class to monitor progress in lattice operations
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef LATTICES_LATTICECLEANPROGRESS_H
#define LATTICES_LATTICECLEANPROGRESS_H
 
//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Vector;
template <class T> class Matrix;
class PGPlotter;

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
// The user of the LatticeCleanProgress object should first call
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


class LatticeCleanProgress {
public:
  LatticeCleanProgress(PGPlotter* pgplotter=0);

  virtual ~LatticeCleanProgress();
  

  // Print and plot the information.
  // Currently, not all information is utilized.
  Bool info(const Bool lastcall,
	    const Int iteration,
	    const Int numberIterations,
	    const Vector<Float>& maxima,
	    const Block<IPosition>& posMaximum,
	    const Float strengthOptimum,
	    const Int optimumScale,
	    const IPosition& positionOptimum,
	    const Float& totalFlux,
	    const Vector<Float>& totalFluxScale,
	    const Bool resetBase=False);
  
protected:

private:

  // initizalize the arrays and such
  void initialize(const uInt nScales, 
		  const Float& maxResidual, 
		  const uInt numIterations);

  // As the iterations trickle in, we will from time to time
  // need to make the Matrices larger.  Increase to 2*n+1
  void resizeDataStorage();

  // this will redraw the plot with a new scale;
  // if plotMatrices = False, just draw the boxes,
  // else, replot all past data.
  //
  void basicSetUp(Bool plotMatrices = False);

  // Note: you MUST call  basicSetUp before calling this.
  void plotOne(const Int iteration, 
               const Vector<Float>& resid, const Vector<Float>& flux);

  PGPlotter* itsPgplotter;

  Vector<Float> iterationNumber;
  Matrix<Float> maxResiduals;
  Matrix<Float> posResiduals;
  Matrix<Float> negResiduals;
  Matrix<Float> totalFluxesPer;
  Vector<Float> totalFluxes;
  uInt currentIndex;
  uInt currentTotalIterations;
  Float currentFluxScale;
  Float currentMinFluxScale;
  Float currentMaxResidual;
  Float currentMinResidual;

  Float logMinRes;
  Float logMaxRes;
  Float deltaY;
  Float xMin;
  Float xMax;
  
  Float fluxScaleJump;
  Float residScaleJump;

  Float forbidden;

  Vector<Float> baseFluxes;

};



} //# NAMESPACE CASACORE - END

#endif
