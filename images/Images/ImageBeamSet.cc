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

#include <images/Images/ImageBeamSet.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <casa/Quanta/QLogical.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>

#include <iomanip>

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

GaussianBeam ImageBeamSet::getMedianAreaBeam() const {
	Vector<uInt> indices;
	IPosition shape = _beams.shape();
	if (shape[0] > 1 && shape[1] > 1) {
		GenSortIndirect<Double>::sort(indices, Vector<Double>(_areas.tovector()));
		return _beams.tovector()[indices[indices.size()/2]];
	}
	else {
		GenSortIndirect<Double>::sort(indices, _areas);
		GaussianBeam medbeam = shape[0] > 1
			? _beams(indices[indices.size()/2], 0)
			: _beams(0, indices[indices.size()/2]);
		return medbeam;
	}
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

ImageBeamSet ImageBeamSet::fromRecord(const Record& rec) {
	ThrowIf(
		! rec.isDefined("nChannels"),
		"no nChannels field found"
	);
	ThrowIf(
		! rec.isDefined("nStokes"),
		"no nStokes field found"
	);
	uInt nchan = rec.asuInt("nChannels");
	ImageBeamSet beams(nchan, rec.asuInt("nStokes"));
	uInt count = 0;
	uInt chan = 0;
	uInt stokes = 0;
	Array<GaussianBeam>::const_iterator iterend = beams.getBeams().end();
	for (
		Array<GaussianBeam>::const_iterator iter =
		beams.getBeams().begin(); iter != iterend; ++iter, ++count
	) {
		String field = "*" + String::toString(count);
		ThrowIf(
			! rec.isDefined(field),
			"Field " + field + " is not defined"
		);
		beams.setBeam(
			chan, stokes,
			GaussianBeam::fromRecord(rec.asRecord(field))
		);
		if (++chan == nchan) {
			chan = 0;
			stokes++;
		}
	}
	return beams;
}

Record ImageBeamSet::toRecord() const {
	Record perPlaneBeams;
	perPlaneBeams.define("nChannels", nchan());
	perPlaneBeams.define("nStokes", nstokes());
	Record rec;
	uInt count = 0;
	const Array<GaussianBeam>& beams = getBeams();
	Array<GaussianBeam>::const_iterator iterEnd = beams.end();
	for (
		Array<GaussianBeam>::const_iterator iter=beams.begin();
		iter!=iterEnd; ++iter, ++count
	) {
		ThrowIf(
			iter->isNull(),
            "Invalid per plane beam found"
        );
		Record rec = iter->toRecord();
		perPlaneBeams.defineRecord("*" + String::toString(count), rec);
	}
	return perPlaneBeams;
}

void ImageBeamSet::summarize(
	LogIO& log, Bool verbose, const CoordinateSystem& csys
) const {
	ostream& os = log.output();
	Unit u("deg");
	for (
		Matrix<GaussianBeam>::const_iterator iter = _beams.begin();
		iter != _beams.end(); iter++
	) {
		if (
			iter->getMajor("deg") < 1/3600
			|| iter->getMinor("deg") < 1/3600
		) {
			u = Unit("mas");
			break;
		}
		if (
			iter->getMajor("deg") < 1.0
			|| iter->getMinor("deg") < 1.0
		) {
			u = Unit("arcsec");
		}
	}
	Bool hasSpectral = csys.hasSpectralAxis();
	Bool hasStokes = csys.hasPolarizationCoordinate();
	log.output() << "Restoring Beams " << endl;
	const SpectralCoordinate *spCoord = 0;
	IPosition beamsShape = _beams.shape();
	uInt chanWidth = 0;
	uInt freqWidth = 0;
	uInt freqPrec = 0;
	uInt velPrec = 0;
	uInt velWidth = 0;
	uInt polWidth = 3;
	uInt typeWidth = 6;
	Bool myverbose = verbose || ! hasSpectral || (hasSpectral && beamsShape[0] <= 3);
	const StokesCoordinate *polCoord = hasStokes
		? &csys.stokesCoordinate()
		: 0;
	if (hasSpectral) {
		spCoord = &csys.spectralCoordinate();
		chanWidth = max(4, Int(log10(beamsShape[0])) + 1);
		// yes these really should be separated because width applies only to the first.
		ostringstream x;
		Double freq;
		spCoord->toWorld(freq, 0);
		if (spCoord->pixelValues().size() > 0) {
			freqPrec = 6;
			velPrec = 3;
		}
		else {
			Double inc = spCoord->increment()[0];
			freqPrec = Int(abs(log10(inc/freq))) + 1;
			Double vel0, vel1;
			spCoord->pixelToVelocity(vel0, 0);
			spCoord->pixelToVelocity(vel1, 1);
			if (abs(vel0-vel1) > 10) {
				velPrec = 0;
			}
			else {
				velPrec = Int(abs(log10(abs(vel0-vel1)))) + 2;
			}
		}
		x << scientific << std::setprecision(freqPrec) << freq;
		freqWidth = x.str().length();
		velWidth = velPrec + 5;
		if (myverbose) {
			os << std::setw(chanWidth) << "Chan" << " ";
			os << std::setw(freqWidth)
				<< "Freq" << " ";
			os << std::setw(velWidth)
				<< "Vel";
		}
		else {
			if (hasStokes) {
				os << std::setw(polWidth) << "Pol" << " ";
			}
			os << std::setw(typeWidth) << "Type" << " ";
			os << std::setw(chanWidth) << "Chan" << " ";
			os << std::setw(freqWidth)
				<< "Freq" << " ";
			os << std::setw(velWidth)
				<< "Vel" << endl;
		}
	}
	if (myverbose) {
		if (hasStokes) {
			os << " ";
			os << std::setw(polWidth) << "Pol";
		}
		os << endl;
		Int stokesPos = hasStokes
			? hasSpectral
				? 1 : 0
				: -1;
		IPosition axisPath = hasSpectral && hasStokes
				? IPosition(2, 1, 0)
						: IPosition(1, 0);
        ArrayPositionIterator iter(beamsShape, axisPath, False);
        while (! iter.pastEnd()) {
            const IPosition pos = iter.pos();
			if (hasSpectral) {
				_chanInfoToStream(
					os, spCoord, pos[0], chanWidth,
					freqPrec, velWidth, velPrec
				);
			}
			if (hasStokes) {
				Stokes::StokesTypes stokes;
				polCoord->toWorld(stokes, pos[stokesPos]);
				os << std::setw(polWidth) << Stokes::name(stokes)
				<< " ";
			}
			_beamToStream(os, _beams(pos), u);
			os << endl;
            iter.next();
		}
	}
	else {
		uInt mymax = hasStokes ? nstokes() : 1;
		for (uInt i=0; i<mymax; i++) {
			String stokesString;
			if (hasStokes) {
				Stokes::StokesTypes stokes;
				polCoord->toWorld(stokes, i);
				stokesString = Stokes::name(stokes);
			}
			for (uInt j=0; j<3; j++) {
				String aggType;
				GaussianBeam beam;
				IPosition pos;
				switch (j) {
					case 0: {
						aggType = "Max";
						beam = getMaxAreaBeamForPol(pos, hasStokes? i : -1);
						break;
					}
					case 1: {
						aggType = "Min";
						beam = getMinAreaBeamForPol(pos, hasStokes ? i : -1);
						break;
					}
					case 2: {
						aggType = "Median";
						beam = getMedianAreaBeamForPol(
							pos, hasStokes ? i : -1
						);
						break;
					}
					default: {
						ThrowCc("Logic error: Unhandled aggregate type");
					}
				}
				if (hasStokes) {
					os << std::setw(polWidth) << stokesString << " ";
				}
				os << std::setw(typeWidth) << aggType << " ";
				_chanInfoToStream(
					os, spCoord, pos[0], chanWidth, freqPrec,
					velWidth, velPrec
				);
				_beamToStream(os, beam, u);
				os << endl;
			}
		}
	}
}

void ImageBeamSet::_chanInfoToStream(
	ostream& os, const SpectralCoordinate *spCoord,
	const uInt chan, const uInt chanWidth, const uInt freqPrec,
	const uInt velWidth, const uInt velPrec
) {
	os << std::fixed << std::setw(chanWidth)
		<< chan << " ";
	Double freq;
	spCoord->toWorld(freq, chan);
	os << scientific << std::setprecision(freqPrec)
		<< freq << " ";
	Double vel;
	spCoord->pixelToVelocity(vel, chan);
	os << std::setw(velWidth) << fixed
		<< std::setprecision(velPrec) << vel << " ";
}

void ImageBeamSet::_beamToStream(
	ostream& os, const GaussianBeam& beam,
	const Unit& unit
) {
	Quantity majAx = beam.getMajor();
	majAx.convert(unit);
	Quantity minAx = beam.getMinor();
	minAx.convert(unit);
	Quantity pa = beam.getPA(True);
	pa.convert("deg");
	os << fixed << std::setprecision(2) << std::setw(7) <<  majAx
		<< " x " << std::setw(7) << minAx << " pa=" << std::setw(6) << pa;
}

}
