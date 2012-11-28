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

#ifndef IMAGES_IMAGEBEAMSET_H
#define IMAGES_IMAGEBEAMSET_H

#include <casa/aips.h>
#include <components/ComponentModels/GaussianBeam.h>
#include <measures/Measures/Stokes.h>
#include <map>

namespace casa {

// <summary>
// Represents a set of restoring beams associated with an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// A Set of Beams associated with an Image.
// </etymology>

// <synopsis>
// This class represents a set of restoring beams associated with
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

class ImageBeamSet {
public:

	enum AxisType {
		SPECTRAL,
		POLARIZATION
	};

	// Construct an empty beam set.
	ImageBeamSet();

	// Construct a beam set from an array of beams with the
	// representing the specified image axes. The number of
	// dimensions in the <src>beams</src> Array must match
	// the number of elements in the <src>axes</src> Vector.
	ImageBeamSet(
		const Array<GaussianBeam>& beams,
		const Vector<AxisType>& axes
	);

	// construct an ImageBeamSet representing a single beam
	ImageBeamSet(const GaussianBeam& beam);

	//create an ImageBeamSet of the specified shape with all
	// GaussianBeams initialized to null beams.
	ImageBeamSet(const IPosition& shape, const Vector<AxisType>& axes);

	//create an ImageBeamSet of the specified shape with all
	// GaussianBeams initialized to <src>beam</src>.
	ImageBeamSet(
		const GaussianBeam& beam, const IPosition& shape,
		const Vector<AxisType>& axes
	);

	ImageBeamSet(const ImageBeamSet& other);

	~ImageBeamSet();

	ImageBeamSet& operator=(const ImageBeamSet& other);

	const GaussianBeam &operator()(const IPosition &) const;

    Array<GaussianBeam> operator[] (uInt i) const;

    const Array<GaussianBeam>& operator() (
    	const IPosition &start,
    	const IPosition &end
    ) const;

    Bool operator== (const ImageBeamSet& other) const;

    Bool operator!= (const ImageBeamSet& other) const;

    // get the axis types associated with the beam array.
	const Vector<AxisType>& getAxes() const;

	// get the single global beam. If there are multiple beams,
	// an exception is thrown.
	const GaussianBeam& getBeam() const;

	// get the beam at the specified location
	const GaussianBeam& getBeam(
		const IPosition& position,
		const Vector<AxisType>& axes=Vector<AxisType>(0)
	) const;

	// set the beam at a given location. Optionally, the axis types can
	// be supplied so one can specify the position using a different axis
	// ordering than the current object has. If specified, <src>axes</src>
	// must have the same number of elements as the dimensionality of the
	// current beam set and all axes of the current beam set must be
	// elements.
	void setBeam(
		const GaussianBeam& beam, const IPosition& position
	);

	// set the beams from the specified beginning to specified ending positions.
	void setBeams(
		const IPosition& begin, const IPosition& end,
		const Array<GaussianBeam>& beams
	);

	// does this beam set contain only a single beam?
	Bool hasSingleBeam() const;

	// does this beam set contain multiple beams?
	Bool hasMultiBeam() const;

	static const String& className();

	// return the size of the beam array.
	size_t size() const;

	// resize the beam array. If pos has a different dimensionality than
	// the current beam array, an exception is thrown.
	void resize(const IPosition& pos);

	// get the beam array
	const Array<GaussianBeam>& getBeams() const;

	// set the beam array.
	void setBeams(const Array<GaussianBeam>& beams);

	// get the number of elements in the beam array
	size_t nelements() const;

	// is the beam array empty?
	Bool empty() const;

	// get the shape of the beam array
	IPosition shape() const;

	// get the number of dimensions in the beam array
	size_t ndim() const;

	// set all beams to the same value
	void set(const GaussianBeam& beam);

	// get the beam in the set which has the smallest area
	GaussianBeam getMinAreaBeam() const;

	// get the beam in the set which has the largest area
	GaussianBeam getMaxAreaBeam() const;

	// get the position of the beam with the maximum area
	IPosition getMaxAreaBeamPosition() const;

	// get the position of the beam with the minimum area
	IPosition getMinAreaBeamPosition() const;

	// get maximal area beam and its position in the _beams array for
	// the given polarization. Use polarization=-1 if the image has no
	// polarization axis to do stats over all the beams.
	GaussianBeam getMaxAreaBeamForPol(
		IPosition& pos, const Int polarization
	) const;

	// get minimal area beam and its position in the _beams array for
	// the given polarization. Use polarization=-1 if the image has no
	// polarization axis to do stats over all the beams.
	GaussianBeam getMinAreaBeamForPol(
		IPosition& pos, const Int polarization
	) const;

	// get median area beam and its position in the _beams array for
	// the given polarization. Use polarization=-1 if the image has no
	// polarization axis to do stats over all the beams.
	GaussianBeam getMedianAreaBeamForPol(
		IPosition& pos, const Int polarization
	) const;

	// get the axis number for the specified type. Return -1 if there is no such axis
	Int getAxis(const AxisType type) const;

private:

	typedef std::map<AxisType, uInt> AxesMap;
	static const String _DEFAULT_AREA_UNIT;

	Array<GaussianBeam> _beams;
	Vector<AxisType> _axes;
	Array<Double> _areas;
	String _areaUnit;
	GaussianBeam _minBeam, _maxBeam;
	IPosition _minBeamPos, _maxBeamPos;
	vector<IPosition> _maxStokesMap, _minStokesMap,
		_medianStokesMap;
	AxesMap _axesMap;

	IPosition _truePosition(
		const IPosition& position,
		const Vector<AxisType>& axes
	) const;

	static void _checkForDups(const Vector<AxisType>& axes);

	void _checkAxisTypeSize(
		const Vector<AxisType>& axes
	) const;

	void _makeStokesMaps(
		const Bool beamsAreIdentical,
		const Int affectedStokes=-1
	);

	uInt _nStokes() const;

	uInt _nChannels() const;

	static std::map<AxisType, uInt> _setAxesMap(
		const Vector<AxisType>& axisTypes
	);

	void _calculateAreas();

	GaussianBeam _getBeamForPol(
		IPosition& pos, const vector<IPosition>& map,
		const Int polarization
	) const;

};

ostream &operator<<(ostream &os, const ImageBeamSet& beamSet);

}

#endif

