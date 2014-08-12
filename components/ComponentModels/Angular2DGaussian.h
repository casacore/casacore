//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#ifndef COMPONENTS_ANGULAR2DGAUSSIAN_H
#define COMPONENTS_ANGULAR2DGAUSSIAN_H

#include <casa/aips.h>
#include <scimath/Mathematics/GaussianBeam.h>

namespace casa {

class Angular2DGaussian : public GaussianBeam {
public:

	// create a beam with all quantities zero (a null beam).
	Angular2DGaussian()
        {}

	// Construct a beam from a set of Quantities. If minor > major
	// an exception is thrown. If any units are not angular, an
	// exception is thrown
	Angular2DGaussian(
		const Quantity& major, const Quantity& minor,
		const Quantity& pa
	)
          : GaussianBeam (major, minor, pa)
        {}

	// Construct a beam from a 3-Vector of Quantities representing
	// the major axis, the minor axis and the position angle (in that order).
	// If parms[1] > parms[0] (minor axis > major axis),
	// an exception is thrown. If any units are not angular, an
	// exception is thrown
	Angular2DGaussian(
		const Vector<Quantity>& parms
	)
          : GaussianBeam (parms)
        {}

	Angular2DGaussian(const GaussianBeam& other)
          : GaussianBeam (other)
        {}

	// Deconvolve the parameters of a source Gaussian from a this GaussianBeam
	// to give the deconvolved Gaussian source.  The return is True if the model appears
	// to be a point source and the output model will be set to
	// the parameters of the beam.
	Bool deconvolve(
		Angular2DGaussian& deconvolvedSize,
		const Angular2DGaussian& convolvedSize
	) const;
};



} //# end namespace

#endif

