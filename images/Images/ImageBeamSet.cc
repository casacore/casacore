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
#include <casa/Quanta/QLogical.h>
#include <images/Images/ImageBeamSet.h>
#include <coordinates/Coordinates/CoordinateSystem.h>

// debug only
//#include <casa/Arrays/ArrayIO.h>

namespace casa {

const String ImageBeamSet::_DEFAULT_AREA_UNIT = "arcsec2";

ImageBeamSet::ImageBeamSet() :
	_beams      (0, 0),
	_areaUnit   (_DEFAULT_AREA_UNIT),
	_minBeam    (GaussianBeam::NULL_BEAM),
	_maxBeam    (GaussianBeam::NULL_BEAM),
	_minBeamPos (2, 0), _maxBeamPos(2, 0) {}

ImageBeamSet::ImageBeamSet(const Matrix<GaussianBeam>& beams) :
	_beams(beams) {
	_calculateAreas();
}

ImageBeamSet::ImageBeamSet(const GaussianBeam& beam) :
	_beams      (1, 1, beam),
	_areas      (1, 1, beam.getArea(_DEFAULT_AREA_UNIT)),
	_areaUnit   (_DEFAULT_AREA_UNIT),
	_minBeam    (beam),
	_maxBeam    (beam),
	_minBeamPos (2, 0),
	_maxBeamPos (2, 0) {}

ImageBeamSet::ImageBeamSet(
	uInt nchan, uInt nstokes, const GaussianBeam& beam
) :
	_beams(max(1u, nchan), max(1u, nstokes), beam),
	_areas      (_beams.shape(), beam.getArea(_DEFAULT_AREA_UNIT)),
	_areaUnit   (_DEFAULT_AREA_UNIT),
	_minBeam    (beam),
	_maxBeam    (beam),
	_minBeamPos (2, 0),
	_maxBeamPos (2, 0) {}

ImageBeamSet::ImageBeamSet(const ImageBeamSet& other) :
	_beams      (other._beams),
	_areas      (other._areas),
	_areaUnit   (other._areaUnit),
	_minBeam    (other._minBeam),
	_maxBeam    (other._maxBeam),
	_minBeamPos (other._minBeamPos),
	_maxBeamPos (other._maxBeamPos) {}

ImageBeamSet::~ImageBeamSet() {}

ImageBeamSet& ImageBeamSet::operator=(const ImageBeamSet& other) {
	if (this != &other) {
		_beams.assign(other._beams);
		_areas.assign(other._areas);
		_areaUnit   = other._areaUnit;
		_minBeam    = other._minBeam;
		_maxBeam    = other._maxBeam;
		_minBeamPos = other._minBeamPos;
		_maxBeamPos = other._maxBeamPos;
	}
	return *this;
}

const GaussianBeam& ImageBeamSet::getBeam(Int chan, Int stokes) const {
	if (nchan() <= 1) {
		chan = 0;
	}
	if (nstokes() <= 1) {
		stokes = 0;
	}
	// Note that chan=-1 can only be given if nchan()==1.
	AlwaysAssert(
		chan >=0 && chan < Int(nchan()) && stokes >= 0 && stokes < Int(nstokes()),
		AipsError
	);
	return _beams(chan, stokes);
}

Bool ImageBeamSet::operator==(const ImageBeamSet& other) const {
	return (
		this == &other
		|| (
			_beams.shape() == other._beams.shape()
			&& allEQ(_beams, other._beams)
		)
	);
}

Bool ImageBeamSet::operator!=(const ImageBeamSet& other) const {
	return !(*this == other);
}

const GaussianBeam& ImageBeamSet::getBeam() const {
	if (size() > 1) {
		throw AipsError(
			String(className()) + "::" + __FUNCTION__
			+ ": This object contains multiple beams, "
			"not a single beam"
		);
	}
	else if (empty()) {
		throw AipsError(
			String(className()) + "::" + __FUNCTION__
			+ ": This object is empty."
		);
	}
	return _beams(0, 0);
}

const String& ImageBeamSet::className() {
	static const String c = "ImageBeamSet";
	return c;
}

void ImageBeamSet::resize(uInt nchan, uInt nstokes) {
	_beams.resize(max(1u, nchan), max(1u, nstokes));
	_calculateAreas();
}

void ImageBeamSet::setBeams(const Matrix<GaussianBeam>& beams) {
	// Resize the beams if needed.
	// A beam axis can be extended if its length is 0 or 1.
	Int nch = nchan();
	Int beamNchan = beams.shape()[0];
	if (nch <= 1) {
		nch = beamNchan;
	}
	Int nst = nstokes();
	Int beamNstokes = beams.shape()[1];
	if (nst <= 1) {
		nst = beams.shape()[1];
	}
	AlwaysAssert(
		(beamNchan == nch || beamNchan == 1)
		&& (beamNstokes == nst || beamNstokes == 1),
		AipsError
	);
	// Determine the step size in the given beams.
	Int incrChan = (beamNchan == 1 ? 0 : 1);
	Int incrStokes = (beamNstokes == 1 ? 0 : 1);
	// Set the beam set to the given beams.
	_beams.resize(nch, nst);
	Int js = 0;
	for (Int is = 0; is < nst; ++is, js += incrStokes) {
		Int jc = 0;
		for (Int ic = 0; ic < nch; ++ic, jc += incrChan) {
			_beams(ic, is) = beams(jc, js);
		}
	}
	_calculateAreas();
}

void ImageBeamSet::set(const GaussianBeam& beam) {
	_beams = beam;
	_areas = beam.getArea(_areaUnit);
	_minBeam = beam;
	_maxBeam = beam;
	_minBeamPos = 0;
	_maxBeamPos = 0;
}

void ImageBeamSet::setBeam(Int chan, Int stokes, const GaussianBeam& beam) {
	AlwaysAssert(
		Int(chan) < _beams.shape()[0] && Int(stokes) < _beams.shape()[1],
		AipsError
	);
	if (chan >= 0 && stokes >= 0) {
		_beams(chan, stokes) = beam;
		IPosition location(2, chan, stokes);
		if (location == _maxBeamPos || location == _minBeamPos) {
			// we are overwriting the max or min beam, so we need to
			// determine the new max or min
			_calculateAreas();
		}
		else {
			Double area = beam.getArea(_areaUnit);
			_areas(chan, stokes) = area;
			if (area < _areas(_minBeamPos)) {
				_minBeam = beam;
				_minBeamPos = location;
			}
			if (area > _areas(_maxBeamPos)) {
				_maxBeam = beam;
				_maxBeamPos = location;
			}
		}
	}
	else if (chan < 0 && stokes < 0) {
		*this = ImageBeamSet(beam);
	}
	else if (chan < 0) {
		_beams(IPosition(2, 0, stokes), IPosition(2, nchan()-1, stokes)) = beam;
		if (_maxBeamPos[0] == chan || _minBeamPos[0] == chan) {
			// we are overwriting the max or min beam, so we need to recalculate
			// the areas
			_calculateAreas();
		}
		else {
			Double area = beam.getArea(_areaUnit);
			_areas(IPosition(2, 0, stokes), IPosition(2, nchan()-1, stokes)) = area;
			if (area < _areas(_minBeamPos)) {
				_minBeam = beam;
				_minBeamPos = IPosition(2, 0, stokes);
			}
			if (area > _areas(_maxBeamPos)) {
				_maxBeam = beam;
				_maxBeamPos = IPosition(2, 0, stokes);
			}
		}
	}
	else {
		// chan >=0 && stokes < 0
		_beams(IPosition(2, chan, 0), IPosition(2, chan, nstokes()-1)) = beam;
		if (_maxBeamPos[1] == stokes || _minBeamPos[1] == stokes) {
			// we are overwriting the max or min beam, so we need to recalculate
			// the areas
			_calculateAreas();
		}
		else {
			Double area = beam.getArea(_areaUnit);
			_areas(IPosition(2, chan, 0), IPosition(2, chan, nstokes()-1)) = area;
			if (area < _areas(_minBeamPos)) {
				_minBeam = beam;
				_minBeamPos = IPosition(2, chan, 0);
			}
			if (area > _areas(_maxBeamPos)) {
				_maxBeam = beam;
				_maxBeamPos = IPosition(2, chan, 0);
			}
		}
	}
}

const GaussianBeam& ImageBeamSet::getMaxAreaBeamForPol(IPosition& pos,
		uInt stokes) const {
	pos.resize(2);
	// If single Stokes, use the maximum itself.
	if (nstokes() <= 1) {
		pos = _maxBeamPos;
		return _maxBeam;
	}
	AlwaysAssert(stokes < nstokes(), AipsError);
	// Determine location of maximum area for given Stokes.
	Double mina, maxa;
	IPosition minPos;
	minMax(
		mina, maxa, minPos, pos,
		_areas(IPosition(2, 0, stokes), IPosition(2, nchan() - 1, stokes))
	);
	pos[1] = stokes;
	return _beams(pos);
}

const GaussianBeam& ImageBeamSet::getMinAreaBeamForPol(IPosition& pos,
		uInt stokes) const {
	pos.resize(2);
	// If single Stokes, use the minimum itself.
	if (nstokes() <= 1) {
		pos = _minBeamPos;
		return _minBeam;
	}
	AlwaysAssert(stokes < nstokes(), AipsError);
	// Determine location of minimum area for given Stokes.
	Double mina, maxa;
	IPosition maxPos;
	minMax(mina, maxa, pos, maxPos,
			_areas(IPosition(2, 0, stokes), IPosition(2, nchan() - 1, stokes)));
	pos[1] = stokes;
	return _beams(pos);
}

const GaussianBeam& ImageBeamSet::getMedianAreaBeamForPol(
	IPosition& pos, uInt stokes
) const {
	pos.resize(2);
	pos = _beams.shape() - 1;
	if (nstokes() > 1) {
		pos[1] = stokes;
	}
	AlwaysAssert(pos[1] >= 0 && pos[1] < _beams.shape()[1], AipsError);
	if (nchan() == 1) {
		return _beams(0, pos[1]);
	}
	// Do an indirect sort to find the location of the median.
	Vector<uInt> indices;
	GenSortIndirect<Double>::sort(indices,
			_areas(IPosition(2, 0, pos[1]), IPosition(2, nchan() - 1, pos[1])));
	pos[0] = indices[indices.size() / 2];
	return _beams(pos[0], pos[1]);
}

GaussianBeam ImageBeamSet::getCommonBeam() const {
	if (empty()) {
		throw AipsError("This beam set is empty.");
	}
	if (allTrue(_beams == GaussianBeam::NULL_BEAM)) {
		throw AipsError("All beams are null.");
	}
	if (allTrue(_beams == _beams(IPosition(2, 0)))) {
		return _beams(IPosition(2, 0));
	}
	BeamIter end = _beams.end();
	Bool largestBeamWorks = True;
	Angular2DGaussian junk;
	GaussianBeam problemBeam;
	Double myMajor = _maxBeam.getMajor("arcsec");
	Double myMinor = _maxBeam.getMinor("arcsec");

	for (
		BeamIter iBeam = _beams.begin();
		iBeam != end; iBeam++
	) {
		if (*iBeam != _maxBeam && !iBeam->isNull()) {
			myMajor = max(myMajor, iBeam->getMajor("arcsec"));
			myMinor = max(myMinor, iBeam->getMinor("arcsec"));
			try {
				if (iBeam->deconvolve(junk, _maxBeam)) {
					largestBeamWorks = False;
					problemBeam = *iBeam;
				}
			}
			catch (const AipsError& x) {
				largestBeamWorks = False;
				problemBeam = *iBeam;
			}
		}
	}
	if (largestBeamWorks) {
		return _maxBeam;
	}

	// transformation 1, rotate so one of the ellipses' major axis lies
	// along the x axis. Ellipse A is _maxBeam, ellipse B is problemBeam,
	// ellipse C is our wanted ellipse

	Double tB1 = problemBeam.getPA("rad", True) - _maxBeam.getPA("rad", True);

	if (abs(tB1) == C::pi / 2) {
		Bool maxHasMajor = _maxBeam.getMajor("arcsec")
				>= problemBeam.getMajor("arcsec");
		// handle the situation of right angles explicitly because things blow up otherwise
		Quantity major =
				maxHasMajor ? _maxBeam.getMajor() : problemBeam.getMajor();
		Quantity minor =
				maxHasMajor ? problemBeam.getMajor() : _maxBeam.getMajor();
		Quantity pa =
				maxHasMajor ? _maxBeam.getPA(True) : problemBeam.getPA(True);
		return GaussianBeam(major, minor, pa);
	}

	Double aA1 = _maxBeam.getMajor("arcsec");
	Double bA1 = _maxBeam.getMinor("arcsec");
	Double aB1 = problemBeam.getMajor("arcsec");
	Double bB1 = problemBeam.getMinor("arcsec");

	// transformation 2: Squeeze along the x axis and stretch along y axis so
	// ellipse A becomes a circle, preserving its area
	Double aA2 = sqrt(aA1 * bA1);
	Double bA2 = aA2;
	Double p = aA2 / aA1;
	Double q = bA2 / bA1;

	// ellipse B's parameters after transformation 2
	Double aB2, bB2, tB2;

	_transformEllipseByScaling(aB2, bB2, tB2, aB1, bB1, tB1, p, q);

	// Now the enclosing transformed ellipse, C, has semi-major axis equal to aB2,
	// minor axis is aA2 == bA2, and the pa is tB2
	Double aC2 = aB2;
	Double bC2 = aA2;
	Double tC2 = tB2;

	// Now reverse the transformations, first transforming ellipse C by shrinking the coordinate
	// system of transformation 2 yaxis and expanding its xaxis to return to transformation 1.

	Double aC1, bC1, tC1;
	_transformEllipseByScaling(aC1, bC1, tC1, aC2, bC2, tC2, 1 / p, 1 / q);

	// now rotate by _maxBeam.getPA() to get the untransformed enclosing ellipse

	Double aC = aC1;
	Double bC = bC1;
	Double tC = tC1 + _maxBeam.getPA("rad", True);

	// confirm that we can indeed convolve both beams with the enclosing ellipse
	GaussianBeam newMaxBeam = GaussianBeam(Quantity(aC, "arcsec"),
			Quantity(bC, "arcsec"), Quantity(tC, "rad"));
	// Sometimes (due to precision issues I suspect), the found beam has to be increased slightly
	// so our deconvolving method doesn't fail
	Bool ok = False;
	while (!ok) {
		try {
			if (_maxBeam.deconvolve(junk, newMaxBeam)) {
				throw AipsError();
			}
			if (problemBeam.deconvolve(junk, newMaxBeam)) {
				throw AipsError();
			}
			ok = True;
		}
		catch (const AipsError& x) {
			// deconvolution issues, increase the enclosing beam size slightly
			aC *= 1.001;
			bC *= 1.001;
			newMaxBeam = GaussianBeam(Quantity(aC, "arcsec"),
					Quantity(bC, "arcsec"), Quantity(tC, "rad"));
		}
	}
	// create a new beam set to run this method on, replacing _maxBeam with ellipse C

	ImageBeamSet newBeamSet(*this);
	Array<GaussianBeam> newBeams = _beams.copy();
	newBeams(_maxBeamPos) = newMaxBeam;
	newBeamSet.setBeams(newBeams);

	return newBeamSet.getCommonBeam();
}

const GaussianBeam ImageBeamSet::getSmallestMinorAxisBeam() const {
	BeamIter ibend = _beams.end();
	Bool found = False;
	Quantity minAxis;
	GaussianBeam res = *(_beams.begin());
	for (
		BeamIter ibeam = _beams.begin();
		ibeam != ibend; ++ibeam
	) {
		if (found) {
			Quantity test = ibeam->getMinor();
			if (
				test < minAxis
				|| (
					test == minAxis
					&& ibeam->getArea(_DEFAULT_AREA_UNIT) < res.getArea(_DEFAULT_AREA_UNIT)
				)
			) {
				minAxis = test;
				res = *ibeam;
			}
		}
		else if (! ibeam->isNull()) {
			minAxis = ibeam->getMinor();
			res = *ibeam;
			found = True;
		}
	}
	return res;
}

void ImageBeamSet::_transformEllipseByScaling(Double& transformedMajor,
		Double& transformedMinor, Double& transformedPA, Double major,
		Double minor, Double pa, Double xScaleFactor, Double yScaleFactor) {
	Double mycos = cos(pa);
	Double mysin = sin(pa);
	Double cos2 = mycos * mycos;
	Double sin2 = mysin * mysin;
	Double major2 = major * major;
	Double minor2 = minor * minor;
	Double a = cos2 / (major2) + sin2 / (minor2);
	Double b = -2 * mycos * mysin * (1 / (major2) - 1 / (minor2));
	Double c = sin2 / (major2) + cos2 / (minor2);

	Double xs = xScaleFactor * xScaleFactor;
	Double ys = yScaleFactor * yScaleFactor;

	Double r = a / xs;
	Double s = b * b / (4 * xs * ys);
	Double t = c / ys;

	Double u = r - t;
	Double u2 = u * u;

	Double f1 = u2 + 4 * s;
	Double f2 = sqrt(f1) * abs(u);

	Double j1 = (f2 + f1) / f1 / 2;
	Double j2 = (-f2 + f1) / f1 / 2;

	Double k1 = (j1 * r + j1 * t - t) / (2 * j1 - 1);
	Double k2 = (j2 * r + j2 * t - t) / (2 * j2 - 1);

	Double c1 = sqrt(1 / k1);
	Double c2 = sqrt(1 / k2);

	if (c1 == c2) {
		// the transformed ellipse is a circle
		transformedMajor = sqrt(k1);
		transformedMinor = transformedMajor;
		transformedPA = 0;
	} else if (c1 > c2) {
		// c1 is the major axis and so j1 is the solution for the corresponding pa
		// of the transformed ellipse
		transformedMajor = c1;
		transformedMinor = c2;
		transformedPA = (pa >= 0 ? 1 : -1) * acos(sqrt(j1));
	} else {
		// c2 is the major axis and so j2 is the solution for the corresponding pa
		// of the transformed ellipse
		transformedMajor = c2;
		transformedMinor = c1;
		transformedPA = (pa >= 0 ? 1 : -1) * acos(sqrt(j2));
	}
}

void ImageBeamSet::_calculateAreas() {
	_areas.resize(_beams.shape());
	if (!_beams.empty()) {
		_areaUnit = _beams.begin()->getMajor().getUnit();
		_areaUnit =
				(Quantity(Quantity(1, _areaUnit) * Quantity(1, _areaUnit)).getUnit());
		Array<Double>::iterator iareas = _areas.begin();
		BeamIter ibend = _beams.end();
		for (
			BeamIter ibeam = _beams.begin();
			ibeam != ibend; ++ibeam, ++iareas
		) {
			*iareas = ibeam->getArea(_areaUnit);
		}
		Double minArea, maxArea;
		minMax(minArea, maxArea, _minBeamPos, _maxBeamPos, _areas);
		_minBeam = _beams(_minBeamPos);
		_maxBeam = _beams(_maxBeamPos);
	}
}

ostream &operator<<(ostream &os, const ImageBeamSet& beamSet) {
	os << beamSet.getBeams();
	return os;
}

ImageBeamSet ImageBeamSet::subset(const Slicer& slicer,
		const CoordinateSystem& csys) const {
	// This beamset can be used if it has a single beam.
	if (nelements() < 2) {
		return *this;
	}
	// Determine the relevant axis numbers in the coordinate system.
	Int axes[2];
	axes[0] = csys.spectralAxisNumber();
	axes[1] = csys.polarizationAxisNumber();
	IPosition ss(slicer.start());
	IPosition se(slicer.end());
	IPosition si(slicer.stride());
	// If the beamset has no or a single freq or stokes, adjust the slicer.
	IPosition beamss(2, 0), beamse(2, 0), beamsi(2, 1);
	for (Int i = 0; i < 2; ++i) {
		if (axes[i] >= 0 && _beams.shape()[i] > 1) {
			AlwaysAssert(_beams.shape()[i] > se[axes[i]], AipsError);
			beamss[i] = ss[axes[i]];
			beamse[i] = se[axes[i]];
			beamsi[i] = si[axes[i]];
		}
	}
	return ImageBeamSet(_beams(beamss, beamse, beamsi));
}

Bool ImageBeamSet::equivalent(const ImageBeamSet& that) const {
	if (empty() || that.empty()) {
		return empty() == that.empty();
	}
	uInt nc1 = nchan();
	uInt np1 = nstokes();
	uInt nc2 = that.nchan();
	uInt np2 = that.nstokes();
	if (!(nc1 == nc2 || nc1 == 1 || nc2 == 1)
			|| !(np1 == np2 || np1 == 1 || np2 == 1)) {
		return False;      // shapes mismatch
	}
	uInt nc = max(nc1, nc2);
	uInt np = max(np1, np2);
	uInt incrc1 = (nc1 == 1 ? 0 : 1);
	uInt incrp1 = (np1 == 1 ? 0 : 1);
	uInt incrc2 = (nc2 == 1 ? 0 : 1);
	uInt incrp2 = (np2 == 1 ? 0 : 1);
	uInt c1 = 0, p1 = 0, c2 = 0, p2 = 0;
	for (uInt p = 0; p < np; ++p) {
		for (uInt c = 0; c < nc; ++c, c1 += incrc1, c2 += incrc2) {
			if (_beams(c1, p1) != that._beams(c2, p2)) {
				return False;  // mismatch in a beam
			}
		}
		c1 = c2 = 0;
		p1 += incrp1;
		p2 += incrp2;
	}
	return True;
}
}
