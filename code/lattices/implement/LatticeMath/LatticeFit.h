//# LatticeFit.h: Fit every line of pixels parallel to any axis in a Lattice.
//# Copyright (C) 1994,1995,1999,2000
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

#if !defined(AIPS_LATTICE_FIT_H)
#define AIPS_LATTICE_FIT_H

#include <aips/aips.h>
#include <aips/Lattices/Lattice.h>
#include <trial/Fitting/LinearFit.h>

class GlishSysEventSource;

// <summary> 
// Fit every line of pixels parallel to any axis in a Lattice.
// </summary>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class=LinearFit>LinearFit</linkto>
//   <li> <linkto class=Lattice>Lattice</linkto>
// </prerequisite>
//
// <synopsis> 

// For every line in the lattice parallel to axis number <src>whichAxis</src>
// (often axis number 2, typically the frequency axis in a spectral line cube)
// independently fit the functions in fitter at the positions where
// <src>fitMask</src> is true. Presently the fit parameters, other than the last
// one(s) in fitter, are lost.
//
// If <src>returnResiduals</src> is True, return data-fit, otherwise return the
// fit.  For baseline and continuum subtraction, returnResiduals would normally
// be True.
//
// If a GlishSysEventSource is provided, the progress of the fit will be
// monitored with progress events, which contains a double precision number
// with a value between 0.0 and 1.0.
// </synopsis> 
//
// <example>
// Suppose one wanted to subtract a linear polynomial from every spectrum (3d
// axis) in an image. One could do this as follows:
// <srcBlock>
//    Image<Float> myImage("myimage"); // Get the image
//    uInt nchan = myImage.shape()(2); // 0 relative axis number
//    // Set up the fitter
//    Polynomial<Float> constant(0);  constant.setCoefficient(0, 1.0);
//    Polynomial<Float> linear(1);    linear.setCoefficient(1, 1.0);
//    LinearFitSVD<Float> fitter;
//    LinearComb<Float,Float> combination;
//    combination.addFunction(constant);
//    combination.addFunction(linear);
//    fitter.setFunction(combination);
//    Vector<Float> fittedParameters,
//
//    // Set up a mask indicating what channels we want to fit over. We want
//    // to fit over all channels.
//    Vector<Bool> fitMask(nchan); fitMask = True;
//
//    // Do the fit. True means subtract the fit from the model. In this case,
//    // We overwrite the input with the output.
//    baselineFit(myImage, fittedParameters,fitter, myImage, 2, fitMask, True);
// </srcBlock>
// </example>
//
// <motivation>
// Baseline fitting/continuum subtraction are important functions. This
// function essentially implements the IMLIN algorithm.
// </motivation>
//
// <todo asof="1995/09/01">
//   <li> Save the model parameters in an (optional) other lattice.
//   <li> Use logging classes, rather than the raw GlishSysEventSource.
//   <li> Allow per-pixel weights.
//   <li> Allow non-linear as well as linear LSQ fits.
// </todo>

// <linkfrom anchor="Baseline fitting" modules="Fitting"
// Related <here>fitting functions</here.
// </linkfrom>

// <group name="baselineFit">
uInt baselineFit(Lattice<Float> &outImage,
		 Vector<Float> &fittedParameters,
		 LinearFit<Float> &fitter, 
		 const Lattice<Float> &inImage,
		 uInt whichAxis,
		 const Vector<Bool> &fitMask,
		 Bool returnResiduals,
		 GlishSysEventSource *eventStream=0);
// </group>

#endif
