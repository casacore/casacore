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

#ifndef COMPONENTS_GAUSSIANBEAM_H
#define COMPONENTS_GAUSSIANBEAM_H

#include <casa/aips.h>
#include <casa/Quanta/Quantum.h>

namespace casa {
	// so the typedef that comes next works
	class GaussianBeam;

	// for when we want the same thing, but it really doesn't represent a beam
	// but eg. a source
	typedef GaussianBeam Angular2DGaussian;

// <summary>
// Represents a Gaussian restoring beam associated with an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// A Gaussian Beam.
// </etymology>

// <synopsis>
// This class represents a Gaussian restoring beam associated with
// a deconvolved image.
// </synopsis>
//
// <example>

// </example>


// <motivation>
// Restoring beams are used many places in image analysis tasks.
// </motivation>

// <todo>
// </todo>

class GaussianBeam {
public:

	static const GaussianBeam NULL_BEAM;

	// create a beam with all quantities zero (a null beam).
	GaussianBeam();


	// Construct a beam from a set of Quantities. If minor > major
	// an exception is thrown. If any units are not angular, an
	// exception is thrown
	GaussianBeam(
		const Quantity& major, const Quantity& minor,
		const Quantity& pa
	);

	// Construct a beam from a 3-Vector of Quantities representing
	// the major axis, the minor axis and the position angle (in that order).
	// If parms[1] > parms[0] (minor axis > major axis),
	// an exception is thrown. If any units are not angular, an
	// exception is thrown
	GaussianBeam(
		const Vector<Quantity>& parms
	);

	GaussianBeam(const GaussianBeam& other);

	~GaussianBeam();

	GaussianBeam& operator=(const GaussianBeam& other);

	Bool operator==(const GaussianBeam& other) const;

	Bool operator!=(const GaussianBeam& other) const;

	// returns the major axis in the same units as it had at construction
	const Quantity& getMajor() const;

	// returns the value portion of the major axis in the specified units
	Double getMajor(const Unit& u) const;

	// returns the minor axis in the same units as it had at construction
	const Quantity& getMinor() const;

	// returns the value portion of the minor axis in the specified units
	Double getMinor(const Unit& u) const;

	// returns the position angle's value as it was at construction,
	// unless <src>unwrap</src> is True, in which case the value of the angle
	// returned will be between -90 and 90 degrees (but with unit the same
	// as it had when this object was constructed).
	Quantity getPA(const Bool unwrap=True) const;

	// returns the value portion of the position angle in the specified units
	Double getPA(const Unit& u, const Bool unwrap=True) const;

	// returns the beam area in the specified <src>unit</src>, which much conform to
	// solid angle units.
	Double getArea(const Unit& unit) const;

	// is this object a null beam (ie is either its major and/or minor axis zero)?
	Bool isNull() const;

	// returns GassianBeam.
	static const String& className();

	Record toRecord() const;

	void setMajorMinor(const Quantity& majAx, const Quantity& minAx);

	void setPA(const Quantity& pa);

	static GaussianBeam fromRecord(const Record& rec);

	// convert this object to a three-Vector of (major FWHM, minor FWHM, and pa).
	// If <src>unwrap</src> is True, the returned pa will fall between -90 and +90
	// degrees.
	Vector<Quantity> toVector(const Bool unwrap=True) const;

	// convert stored Quantities to the specified units
	void convert(
		const String& majUnit, const String& minUnit,
		const String& paUnit
	);

	// Deconvolve the parameters of a source Gaussian from a this GaussianBeam
	// to give the deconvolved Gaussian source.  The return is True if the model appears
	// to be a point source and the output model will be set to
	// the parameters of the beam.
	Bool deconvolve(
		Angular2DGaussian& deconvolvedSize,
		const Angular2DGaussian& convolvedSize
	) const;

private:
	Quantity _major, _minor, _pa;

};



ostream &operator<<(ostream &os, const GaussianBeam& beam);

LogIO &operator<<(LogIO &os, const GaussianBeam& beam);

Bool near(
	const GaussianBeam& left, const GaussianBeam& other,
	const Double relWidthTol, const Quantity& absPaTol
);

}

#endif

