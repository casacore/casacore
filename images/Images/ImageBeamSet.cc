//# Copyright (C) 1995,1997,1998,1999,2000,2001,2002,2003
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
#include <casa/Arrays/ArrayMath.h>
// #include <casa/Utilities/GenSort.h>
#include <images/Images/ImageBeamSet.h>

// debug only
#include <casa/Arrays/ArrayIO.h>

namespace casa {

const String ImageBeamSet::_DEFAULT_AREA_UNIT = "arcsec2";

ImageBeamSet::ImageBeamSet()
	: _beams(Array<GaussianBeam>()), _axes(Vector<AxisType>(0)),
	  _areas(Array<Double>()), _areaUnit(_DEFAULT_AREA_UNIT),
	  _minBeam(GaussianBeam::NULL_BEAM),
	  _maxBeam(GaussianBeam::NULL_BEAM), _minBeamPos(IPosition(0)),
	  _maxBeamPos(IPosition(0)), _maxStokesMap(0), _minStokesMap(0),
	  _medianStokesMap(0), _axesMap() {
}

ImageBeamSet::ImageBeamSet(
	const Array<GaussianBeam>& beams,
	const Vector<AxisType>& axes
) : _beams(beams), _axes(axes),
	_maxStokesMap(0), _minStokesMap(0),
	_medianStokesMap(0), _axesMap(_setAxesMap(axes)) {
	_checkForDups(axes);
	_checkAxisTypeSize(axes);
	_calculateAreas();
	_makeStokesMaps(False);
}

ImageBeamSet::ImageBeamSet(const GaussianBeam& beam)
 : _beams(IPosition(1, 1), beam),
   _axes(0),
   _areas(
	    IPosition(1, 1),
	    beam.getArea(_DEFAULT_AREA_UNIT)
	),
   _areaUnit(_DEFAULT_AREA_UNIT), _minBeam(beam),
   _maxBeam(beam), _minBeamPos(1, 0), _maxBeamPos(1, 0),
   _maxStokesMap(0), _minStokesMap(0),
   _medianStokesMap(0), _axesMap() {
}

ImageBeamSet::ImageBeamSet(
	const IPosition& shape, const Vector<AxisType>& axes
) : _beams(shape), _axes(axes), _areas(shape, 0.0),
	_areaUnit(_DEFAULT_AREA_UNIT),
	_minBeam(GaussianBeam::NULL_BEAM),
	_maxBeam(GaussianBeam::NULL_BEAM), _minBeamPos(shape.size(), 0),
	_maxBeamPos(shape.size(), 0),
	_axesMap(_setAxesMap(axes)) {
	_checkForDups(axes);
	_checkAxisTypeSize(axes);
	_makeStokesMaps(True);
}

ImageBeamSet::ImageBeamSet(
	const GaussianBeam& beam, const IPosition& shape,
	const Vector<AxisType>& axes
) : _beams(shape, beam), _axes(axes),
	_areas(shape, beam.getArea(_DEFAULT_AREA_UNIT)),
	_areaUnit(_DEFAULT_AREA_UNIT), _minBeam(beam),
	_maxBeam(beam), _minBeamPos(shape.size(), 0),
	_maxBeamPos(shape.size(), 0), _maxStokesMap(0),
	_minStokesMap(0),
	_medianStokesMap(0), _axesMap(_setAxesMap(axes)) {
	_checkForDups(axes);
	_checkAxisTypeSize(axes);
	_makeStokesMaps(True);
}

ImageBeamSet::ImageBeamSet(const ImageBeamSet& other)
  : _beams(other._beams), _axes(other._axes),
    _areas(other._areas), _areaUnit(other._areaUnit),
    _minBeam(other._minBeam), _maxBeam(other._maxBeam),
    _minBeamPos(other._minBeamPos),
    _maxBeamPos(other._maxBeamPos),
    _maxStokesMap(other._maxStokesMap),
    _minStokesMap(other._minStokesMap),
    _medianStokesMap(other._medianStokesMap),
    _axesMap(other._axesMap) {}

ImageBeamSet::~ImageBeamSet() {}

ImageBeamSet& ImageBeamSet::operator=(
	const ImageBeamSet& other
) {
	if (this != &other) {
		_beams.assign(other._beams);
		_axes.assign(other._axes);
		_areas.assign(other._areas);
		_areaUnit = other._areaUnit;
		_minBeam = other._minBeam;
		_maxBeam = other._maxBeam;
		_minBeamPos.resize(other._minBeamPos.size());
		_minBeamPos = other._minBeamPos;
		_maxBeamPos.resize(other._maxBeamPos.size());
		_maxBeamPos = other._maxBeamPos;
		if (
			_maxStokesMap.size() > 0 && other._maxStokesMap.size() > 0
			&& _maxStokesMap.begin()->size() != other._maxStokesMap.begin()->size()
		) {
			_maxStokesMap.resize(0);
			_minStokesMap.resize(0);
			_medianStokesMap.resize(0);
		}
		_maxStokesMap = other._maxStokesMap;
		_minStokesMap = other._minStokesMap;
		_medianStokesMap = other._medianStokesMap;
		_axesMap = other._axesMap;
	}
	return *this;
}

const GaussianBeam& ImageBeamSet::operator()(
	const IPosition& pos
) const {
	return _beams(pos);
}

Array<GaussianBeam> ImageBeamSet::operator[](uInt i) const {
	return _beams[i];
}

const Array<GaussianBeam>& ImageBeamSet::operator()(
	const IPosition &start,
	const IPosition &end
) const {
	const Array<GaussianBeam>& beams = _beams(start, end);
	return beams;
}

Bool ImageBeamSet::operator== (const ImageBeamSet& other) const {
	return this == &other
		|| (
			allTrue(_axes == other._axes)
			&& allTrue(_beams == other._beams)
		);
}

Bool ImageBeamSet::operator!= (const ImageBeamSet& other) const {
	return ! (*this == other);
}

const Vector<ImageBeamSet::AxisType>& ImageBeamSet::getAxes() const {
	return _axes;
}

// get the beam at the specified location
const GaussianBeam& ImageBeamSet::getBeam(
	const IPosition& position, const Vector<AxisType>& axes
) const {
	return _beams(_truePosition(position, axes));
}

const GaussianBeam& ImageBeamSet::getBeam() const {
	if (_beams.nelements() > 1) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": This object contains multiple beams, not a single beam";
		throw AipsError(oss.str());
	}
	else if (_beams.nelements() == 0) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": This object contains no beams.";
		throw AipsError(oss.str());
	}
	return *(_beams.begin());
}

Bool ImageBeamSet::hasSingleBeam() const {
	return _beams.nelements() == 1;
}

Bool ImageBeamSet::hasMultiBeam() const {
	return _beams.nelements() > 1;
}

const String& ImageBeamSet::className() {
	static const String c = "ImageBeamSet";
	return c;
}

IPosition ImageBeamSet::_truePosition(
	const IPosition& position, const Vector<AxisType>& axes
) const {
	if (axes.size() == 0) {
		return position;
	}
	if (ndim() != axes.size()) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": Inconsistent size for axes Vector";
		throw AipsError(oss.str());
	}
	if (allTrue(axes == _axes)) {
		return position;
	}
	_checkForDups(axes);
	IPosition truePos(position.size(), 0);
	for (uInt i=0; i<axes.size(); i++) {
		Bool found = False;
		for (uInt j=0; j<_axes.size(); j++) {
			if (axes[i] == _axes[j]) {
				truePos[j] = position[i];
				found = True;
				break;
			}
			if (! found) {
				ostringstream oss;
				oss << className() << "::" << __FUNCTION__
					<< ": Inconsistent axes types";
				throw AipsError(oss.str());
			}
		}
	}
	if (truePos > _beams.shape() - 1) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": Inconsistent position specification";
		throw AipsError(oss.str());
	}
	return truePos;
}

void ImageBeamSet::_checkForDups(const Vector<AxisType>& axes) {
	for (
		Vector<AxisType>::const_iterator iter = axes.begin();
		iter != axes.end(); iter++
	) {
		Vector<AxisType>::const_iterator jiter = iter;
		jiter++;
		while (jiter != axes.end()) {
			if (*iter == *jiter) {
				ostringstream oss;
				oss << className() << "::" << __FUNCTION__
					<< ": Duplicate axes entry not permitted";
				throw AipsError(oss.str());
			}
			jiter++;
		}
	}
}

void ImageBeamSet::resize(const IPosition& pos) {
	if (pos.nelements() != _beams.ndim()) {
		throw AipsError(
			"An ImageBeamSet object cannot be resized to a different dimensionality."
		);
	}
	_beams.resize(pos);
	_calculateAreas();
}

size_t ImageBeamSet::size() const {
	return _beams.size();
}

const Array<GaussianBeam>& ImageBeamSet::getBeams() const {
	return _beams;
}

void ImageBeamSet::setBeams(const Array<GaussianBeam>& beams) {
    if (beams.ndim() != _axes.size()) {
		throw AipsError(
			"Beam array dimensionality is not equal to number of axes."
		);
	}
	_beams.assign(beams);
	_calculateAreas();
	_makeStokesMaps(False);
}

void ImageBeamSet::setBeams(
	const IPosition& begin, const IPosition& end,
	const Array<GaussianBeam>& beams
) {
	_beams(begin, end) = beams;
	_calculateAreas();
	// FIXME there is a more efficient way to make the stokes
	// maps in this case
	_makeStokesMaps(False);
}

size_t ImageBeamSet::nelements() const {
	return _beams.nelements();
}

Bool ImageBeamSet::empty() const {
	return _beams.empty();
}

IPosition ImageBeamSet::shape() const {
	return _beams.shape();
}

size_t ImageBeamSet::ndim() const {
	return _beams.ndim();
}

void ImageBeamSet::set(const GaussianBeam& beam) {
	_beams.set(beam);
	_minBeam = beam;
	_maxBeam = beam;
	_minBeamPos = IPosition(_beams.ndim(), 0);
	_maxBeamPos = IPosition(_beams.ndim(), 0);
	_makeStokesMaps(True);
}

void ImageBeamSet::setBeam(
	const GaussianBeam& beam, const IPosition& position
) {
	_beams(position) = beam;
	Double area = beam.getArea(_areaUnit);
	_areas(position) = area;
	if (area < _areas(_maxBeamPos)) {
		_minBeam = beam;
		_minBeamPos = position;
	}
	if (area > _areas(_maxBeamPos)) {
		_maxBeam = beam;
		_maxBeamPos = position;
	}
	if (_axesMap.find(POLARIZATION) != _axesMap.end()) {
		_makeStokesMaps(False, position[_axesMap.find(POLARIZATION)->second]);
	}
}

GaussianBeam ImageBeamSet::getMaxAreaBeam() const {
	return _maxBeam;
}

GaussianBeam ImageBeamSet::getMinAreaBeam() const {
	return _minBeam;
}

IPosition ImageBeamSet::getMaxAreaBeamPosition() const {
	return _maxBeamPos;
}

IPosition ImageBeamSet::getMinAreaBeamPosition() const {
	return _minBeamPos;
}

GaussianBeam ImageBeamSet::getMaxAreaBeamForPol(
	IPosition& pos, const Int polarization
) const {
	return _getBeamForPol(pos, _maxStokesMap, polarization);

}

GaussianBeam ImageBeamSet::getMinAreaBeamForPol(
	IPosition& pos, const Int polarization
) const {
	return _getBeamForPol(pos, _minStokesMap, polarization);
}

GaussianBeam ImageBeamSet::getMedianAreaBeamForPol(
	IPosition& pos, const Int polarization
) const {
		return _getBeamForPol(pos, _medianStokesMap, polarization);
}

Int ImageBeamSet::getAxis(const AxisType type) const {
	AxesMap::const_iterator x = _axesMap.find(type);
	return x == _axesMap.end() ? -1 : x->second;
}

GaussianBeam ImageBeamSet::_getBeamForPol(
	IPosition& pos, const vector<IPosition>& map,
	const Int polarization
) const {
	uInt nStokes = _nStokes();
	uInt mypol = nStokes == 0 && polarization < 0 ? 0 : polarization;
	if (nStokes == 0 && polarization >= 0) {
		throw AipsError(
			className() + "::" + __FUNCTION__
			+ ": This beam set has no polarization axis"
		);
	}
	if (mypol != 0 && mypol >= nStokes) {
		throw AipsError(
			className() + "::" + __FUNCTION__
			+ ": polarization=" + String::toString(polarization)
			+ " must be less than number of polarizations="
			+ String::toString(nStokes)
		);
	}
	pos = map[mypol];
	return _beams(pos);
}


void ImageBeamSet::_checkAxisTypeSize(const Vector<AxisType>& axes) const {
	if (_beams.ndim() != axes.size()) {
		ostringstream oss;
		oss << className() << "::" << __FUNCTION__
			<< ": Inconsistent beams Array dimensionality and axes Vector size";
		throw AipsError(oss.str());
	}
}

void ImageBeamSet::_makeStokesMaps(
	const Bool beamsAreIdentical,
	const Int affectedStokes
) {
	uInt nStokes = _nStokes();
	if (nStokes == 0) {
		_maxStokesMap.resize(1);
		_minStokesMap.resize(1);
		_medianStokesMap.resize(1);
	}
	else if (_maxStokesMap.size() != nStokes) {
		_maxStokesMap.resize(nStokes);
		_minStokesMap.resize(nStokes);
		_medianStokesMap.resize(nStokes);
	}
	if (_axesMap.find(SPECTRAL) == _axesMap.end()) {
		IPosition pos(1, 0);
		for (uInt i=0; i<nStokes; i++) {
			if (affectedStokes < 0 || affectedStokes == (Int)i) {
				pos[0] = i;
				_maxStokesMap[i] = pos;
				_minStokesMap[i] = pos;
				_medianStokesMap[i] = pos;
			}
		}
	}
	else if (beamsAreIdentical) {
		IPosition pos(2, 0);
		for (uInt i=0; i<nStokes; i++) {
			if (affectedStokes < 0 || affectedStokes == (Int)i) {
				pos[_axesMap[POLARIZATION]] = i;
				_minStokesMap[i] = pos;
				_maxStokesMap[i] = pos;
				_medianStokesMap[i] = pos;
			}
		}
	}
	else if (nStokes == 0) {
		// we still need the maps even if there are no stokes
		IPosition pos(1);
		IPosition minPos, maxPos;
		_minStokesMap[0] = _minBeamPos;
		_maxStokesMap[0] = _maxBeamPos;
		// get the median
		Vector<uInt> indices;
		GenSortIndirect<Double>::sort(indices, _areas);
		uInt nChan = _beams.size();
		uInt k = nChan % 2 == 0
			? nChan/2 - 1 : nChan/2;
		_medianStokesMap[0] = IPosition(1, indices[k]);
	}
	else {
		// nStokes and nChannels both > 0
		uInt spectralAxis = _axesMap.find(SPECTRAL)->second;
		uInt stokesAxis = _axesMap.find(POLARIZATION)->second;
		uInt nChan = _beams.shape()[spectralAxis];
		IPosition shape(2);
		shape[spectralAxis] = nChan;
		shape[stokesAxis] = 1;
		Array<Double> beamAreas(shape);
		// WARN assumes a maximum _beams dimensionality of 2,
		// which is OK for now but if this class is extended
		// to allow more than two dimensional _beams this will
		// have to change.
		IPosition start(2);
		IPosition end(2);
		start[spectralAxis] = 0;
		end[spectralAxis] = nChan - 1;
		Double minArea, maxArea;
		IPosition minPos, maxPos;
		IPosition pos(2);
		for (uInt i=0; i<nStokes; i++) {
			if (affectedStokes < 0 || (Int)i == affectedStokes) {
				start[stokesAxis] = i;
				end[stokesAxis] = i;
				beamAreas = _areas(
					Slicer(start, end, Slicer::endIsLast)
				);
				pos[stokesAxis] = i;
				minMax(minArea, maxArea, minPos, maxPos, beamAreas);
				pos[spectralAxis] = minPos[spectralAxis];
				_minStokesMap[i] = pos;
				pos[spectralAxis] = maxPos[spectralAxis];
				_maxStokesMap[i] = pos;
				// get the median
				Vector<uInt> indices;
				GenSortIndirect<Double>::sort(indices, beamAreas);
				uInt k = nChan % 2 == 0
					? nChan/2 - 1 : nChan/2;
				pos[spectralAxis] = indices[k];
				_medianStokesMap[i] = pos;
			}
		}
	}
}

uInt ImageBeamSet::_nStokes() const {
	return _axesMap.find(POLARIZATION) == _axesMap.end()
		? 0
		: _beams.shape()[_axesMap.find(POLARIZATION)->second];
}

ImageBeamSet::AxesMap ImageBeamSet::_setAxesMap(
	const Vector<AxisType>& axisTypes
) {
	std::map<ImageBeamSet::AxisType, uInt> mymap;
	uInt count = 0;
	for (
		Vector<AxisType>::const_iterator iter=axisTypes.begin();
		iter != axisTypes.end(); iter++, count++
	) {
		mymap[*iter] = count;
	}
	return mymap;
}

void ImageBeamSet::_calculateAreas() {
	_areaUnit = _beams.begin()->getMajor().getUnit();
	_areaUnit = Quantity(Quantity(1, _areaUnit)*Quantity(1, _areaUnit)).getUnit();
	_areas.resize(_beams.shape());
	Array<Double>::iterator iareas = _areas.begin();
	for (
		Array<GaussianBeam>::const_iterator ibeam=_beams.begin();
		ibeam!=_beams.end(); ibeam++, iareas++
	) {
		*iareas = ibeam->getArea(_areaUnit);
	}
	Double minArea, maxArea;
	minMax(minArea, maxArea, _minBeamPos, _maxBeamPos, _areas);
	_minBeam = _beams(_minBeamPos);
	_maxBeam = _beams(_maxBeamPos);
}

ostream &operator<<(ostream &os, const ImageBeamSet& beamSet) {
	os << beamSet.getBeams();
	return os;
}



}
