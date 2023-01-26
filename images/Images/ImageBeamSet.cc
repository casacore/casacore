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

#include <casacore/images/Images/ImageBeamSet.h>

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>

#include <iomanip>

namespace casacore {

const String ImageBeamSet::_DEFAULT_AREA_UNIT = "arcsec2";

ImageBeamSet::ImageBeamSet() :
    _beams      (0, 0),
    _areaUnit   (_DEFAULT_AREA_UNIT),
    _minBeam    (GaussianBeam::NULL_BEAM),
    _maxBeam    (GaussianBeam::NULL_BEAM),
    _minBeamPos (2, 0), _maxBeamPos(2, 0) {}

ImageBeamSet::ImageBeamSet(const Matrix<GaussianBeam>& beams) 
: _beams(beams) {
    ThrowIf(
        beams.empty(),
        "The number of channels and/or the number of stokes in "
        "the beams matrix is zero, which is not permitted"
    );
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
    uint32_t nchan, uint32_t nstokes, const GaussianBeam& beam
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

const GaussianBeam& ImageBeamSet::getBeam(int32_t chan, int32_t stokes) const {
    if (nchan() <= 1) {
        chan = 0;
    }
    if (nstokes() <= 1) {
        stokes = 0;
    }
    // Note that chan=-1 can only be given if nchan()==1.
    AlwaysAssert(
        chan >=0 && chan < int32_t(nchan()) && stokes >= 0 && stokes < int32_t(nstokes()),
        AipsError
    );
    return _beams(chan, stokes);
}

bool ImageBeamSet::operator==(const ImageBeamSet& other) const {
    return (
        this == &other
        || (
            _beams.shape() == other._beams.shape()
            && allEQ(_beams, other._beams)
        )
    );
}

bool ImageBeamSet::operator!=(const ImageBeamSet& other) const {
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

void ImageBeamSet::resize(uint32_t nchan, uint32_t nstokes) {
    _beams.resize(max(1u, nchan), max(1u, nstokes));
    _calculateAreas();
}

void ImageBeamSet::setBeams(const Matrix<GaussianBeam>& beams) {
    // Resize the beams if needed.
    // A beam axis can be extended if its length is 0 or 1.
    int32_t nch = nchan();
    int32_t beamNchan = beams.shape()[0];
    if (nch <= 1) {
        nch = beamNchan;
    }
    int32_t nst = nstokes();
    int32_t beamNstokes = beams.shape()[1];
    if (nst <= 1) {
        nst = beams.shape()[1];
    }
    AlwaysAssert(
        (beamNchan == nch || beamNchan == 1)
        && (beamNstokes == nst || beamNstokes == 1),
        AipsError
    );
    // Determine the step size in the given beams.
    int32_t incrChan = (beamNchan == 1 ? 0 : 1);
    int32_t incrStokes = (beamNstokes == 1 ? 0 : 1);
    // Set the beam set to the given beams.
    _beams.resize(nch, nst);
    int32_t js = 0;
    for (int32_t is = 0; is < nst; ++is, js += incrStokes) {
        int32_t jc = 0;
        for (int32_t ic = 0; ic < nch; ++ic, jc += incrChan) {
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

void ImageBeamSet::setBeam(int32_t chan, int32_t stokes, const GaussianBeam& beam) {
    AlwaysAssert(
        int32_t(chan) < _beams.shape()[0] && int32_t(stokes) < _beams.shape()[1],
        AipsError
    );
    if (chan < 0 && stokes < 0) {
        *this = ImageBeamSet(beam);
    }
    else if (chan >= 0 && stokes >= 0) {
        IPosition location(2, chan, stokes);
        _replaceBeam(
            beam, location, location,
            _maxBeamPos == location || _minBeamPos[1] == location
        );
    }
    else if (chan < 0) {
        // && stokes >= 0
        _replaceBeam(
            beam, IPosition(2, 0, stokes), IPosition(2, nchan()-1, stokes),
            _maxBeamPos[1] == stokes || _minBeamPos[1] == stokes
        );
    }
    else {
        // chan >=0 && stokes < 0
        _replaceBeam(
            beam, IPosition(2, chan, 0), IPosition(2, chan, nstokes()-1),
            _maxBeamPos[0] == chan || _minBeamPos[0] == chan
        );
    }
}

void ImageBeamSet::_replaceBeam(
    const GaussianBeam& beam, const IPosition& location1,
    const IPosition& location2, bool overwriteMaxMin
) {
    _beams(location1, location2) = beam;
    if (overwriteMaxMin) {
        // we are overwriting the max or min beam, so we need to recalculate
        // the areas
        _calculateAreas();
    }
    else {
        const auto area = beam.getArea(_areaUnit);
        _areas(location1, location2) = area;
        if (area < _areas(_minBeamPos)) {
            _minBeam = beam;
            _minBeamPos = location1;
        }
        if (area > _areas(_maxBeamPos)) {
            _maxBeam = beam;
            _maxBeamPos = location1;
        }
    }
}

const GaussianBeam& ImageBeamSet::getMaxAreaBeamForPol(IPosition& pos,
        uint32_t stokes) const {
    pos.resize(2);
    // If single Stokes, use the maximum itself.
    if (nstokes() <= 1) {
        pos = _maxBeamPos;
        return _maxBeam;
    }
    AlwaysAssert(stokes < nstokes(), AipsError);
    // Determine location of maximum area for given Stokes.
    double mina, maxa;
    IPosition minPos;
    minMax(
        mina, maxa, minPos, pos,
        _areas(IPosition(2, 0, stokes), IPosition(2, nchan() - 1, stokes))
    );
    pos[1] = stokes;
    return _beams(pos);
}

const GaussianBeam& ImageBeamSet::getMinAreaBeamForPol(IPosition& pos,
        uint32_t stokes) const {
    pos.resize(2);
    // If single Stokes, use the minimum itself.
    if (nstokes() <= 1) {
        pos = _minBeamPos;
        return _minBeam;
    }
    AlwaysAssert(stokes < nstokes(), AipsError);
    // Determine location of minimum area for given Stokes.
    double mina, maxa;
    IPosition maxPos;
    minMax(mina, maxa, pos, maxPos,
            _areas(IPosition(2, 0, stokes), IPosition(2, nchan() - 1, stokes)));
    pos[1] = stokes;
    return _beams(pos);
}

const GaussianBeam& ImageBeamSet::getMedianAreaBeamForPol(
    IPosition& pos, uint32_t stokes
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
    Vector<uint32_t> indices;
    GenSortIndirect<double,uint32_t>::sort(indices,
            _areas(IPosition(2, 0, pos[1]), IPosition(2, nchan() - 1, pos[1])));
    pos[0] = indices[indices.size() / 2];
    return _beams(pos[0], pos[1]);
}

GaussianBeam ImageBeamSet::getMedianAreaBeam() const {
    Vector<uint32_t> indices;
    IPosition shape = _beams.shape();
    if (shape[0] > 1 && shape[1] > 1) {
      GenSortIndirect<double,uint32_t>::sort(indices, Vector<double>(_areas.tovector()));
        return _beams.tovector()[indices[indices.size()/2]];
    }
    else {
      GenSortIndirect<double,uint32_t>::sort(indices, _areas);
        GaussianBeam medbeam = shape[0] > 1
            ? _beams(indices[indices.size()/2], 0)
            : _beams(0, indices[indices.size()/2]);
        return medbeam;
    }
}

const GaussianBeam ImageBeamSet::getSmallestMinorAxisBeam() const {
    BeamIter ibend = _beams.end();
    bool found = false;
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
            found = true;
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
        Array<double>::iterator iareas = _areas.begin();
        BeamIter ibend = _beams.end();
        for (
            BeamIter ibeam = _beams.begin();
            ibeam != ibend; ++ibeam, ++iareas
        ) {
            *iareas = ibeam->getArea(_areaUnit);
        }
        double minArea, maxArea;
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
    int32_t axes[2];
    axes[0] = csys.spectralAxisNumber();
    axes[1] = csys.polarizationAxisNumber();
    IPosition ss(slicer.start());
    IPosition se(slicer.end());
    IPosition si(slicer.stride());
    // If the beamset has no or a single freq or stokes, adjust the slicer.
    IPosition beamss(2, 0), beamse(2, 0), beamsi(2, 1);
    for (int32_t i = 0; i < 2; ++i) {
        if (axes[i] >= 0 && _beams.shape()[i] > 1) {
            AlwaysAssert(_beams.shape()[i] > se[axes[i]], AipsError);
            beamss[i] = ss[axes[i]];
            beamse[i] = se[axes[i]];
            beamsi[i] = si[axes[i]];
        }
    }
    return ImageBeamSet(_beams(beamss, beamse, beamsi));
}

bool ImageBeamSet::equivalent(const ImageBeamSet& that) const {
    if (empty() || that.empty()) {
        return empty() == that.empty();
    }
    uint32_t nc1 = nchan();
    uint32_t np1 = nstokes();
    uint32_t nc2 = that.nchan();
    uint32_t np2 = that.nstokes();
    if (!(nc1 == nc2 || nc1 == 1 || nc2 == 1)
            || !(np1 == np2 || np1 == 1 || np2 == 1)) {
        return false;      // shapes mismatch
    }
    uint32_t nc = max(nc1, nc2);
    uint32_t np = max(np1, np2);
    uint32_t incrc1 = (nc1 == 1 ? 0 : 1);
    uint32_t incrp1 = (np1 == 1 ? 0 : 1);
    uint32_t incrc2 = (nc2 == 1 ? 0 : 1);
    uint32_t incrp2 = (np2 == 1 ? 0 : 1);
    uint32_t c1 = 0, p1 = 0, c2 = 0, p2 = 0;
    for (uint32_t p = 0; p < np; ++p) {
        for (uint32_t c = 0; c < nc; ++c, c1 += incrc1, c2 += incrc2) {
            if (_beams(c1, p1) != that._beams(c2, p2)) {
                return false;  // mismatch in a beam
            }
        }
        c1 = c2 = 0;
        p1 += incrp1;
        p2 += incrp2;
    }
    return true;
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
    auto nchan = rec.asuInt("nChannels");
    if (nchan == 0) {
        // provide backward compatibility for records written with 0 channels
        nchan = 1;
    }
    auto nstokes = rec.asuInt("nStokes");
    if (nstokes == 0) {
        // provide backward compatibility for records written with 0 stokes
        nstokes = 1;
    }
    uint32_t count = 0;
    uint32_t chan = 0;
    uint32_t stokes = 0;
    Matrix<GaussianBeam> beams(nchan, nstokes);
    auto iterend = beams.end();
    for (
        auto iter = beams.begin(); iter != iterend; ++iter, ++count
    ) {
        String field = "*" + String::toString(count);
        ThrowIf(
            ! rec.isDefined(field),
            "Field " + field + " is not defined"
        );
            *iter = GaussianBeam::fromRecord(rec.asRecord(field));
        if (++chan == nchan) {
            chan = 0;
            ++stokes;
        }
    }
    return ImageBeamSet(beams);
}

Record ImageBeamSet::toRecord() const {
    Record perPlaneBeams;
    perPlaneBeams.define("nChannels", nchan());
    perPlaneBeams.define("nStokes", nstokes());
    Record rec;
    uint32_t count = 0;
    const Array<GaussianBeam>& beams = getBeams();
    for (const auto& beam: beams) {
        ThrowIf(
            beam.isNull(),
            "Invalid per plane beam found"
        );
        Record rec = beam.toRecord();
        perPlaneBeams.defineRecord("*" + String::toString(count), rec);
        ++count;
    }
    return perPlaneBeams;
}

void ImageBeamSet::rotate(const Quantity& angle, bool unwrap) {
    ThrowIf(
        ! angle.isConform("rad"),
        "Quantity is not an angle"
    );
    Matrix<GaussianBeam>::iterator iter = _beams.begin();
    Matrix<GaussianBeam>::iterator end = _beams.end();
    while(iter != end) {
        iter->setPA(iter->getPA(true) + angle, unwrap);
        ++iter;
    }
    _minBeam.setPA(_minBeam.getPA() + angle, unwrap);
    _maxBeam.setPA(_maxBeam.getPA() + angle, unwrap);
}

void ImageBeamSet::summarize(
    LogIO& log, bool verbose, const CoordinateSystem& csys
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
    bool hasSpectral = csys.hasSpectralAxis();
    bool hasStokes = csys.hasPolarizationCoordinate();
    log.output() << "Restoring Beams " << endl;
    const SpectralCoordinate *spCoord = 0;
    IPosition beamsShape = _beams.shape();
    uint32_t chanWidth = 0;
    uint32_t freqWidth = 0;
    uint32_t freqPrec = 0;
    uint32_t velPrec = 0;
    uint32_t velWidth = 0;
    uint32_t polWidth = 3;
    uint32_t typeWidth = 6;
    bool myverbose = verbose || ! hasSpectral || (hasSpectral && beamsShape[0] <= 3);
    const StokesCoordinate *polCoord = hasStokes
        ? &csys.stokesCoordinate()
        : 0;
    if (hasSpectral) {
        spCoord = &csys.spectralCoordinate();
        chanWidth = max(4, int32_t(log10(beamsShape[0])) + 1);
        // yes these really should be separated because width applies only to the first.
        ostringstream x;
        double freq;
        spCoord->toWorld(freq, 0);
        if (spCoord->pixelValues().size() > 0) {
            freqPrec = 6;
            velPrec = 3;
        }
        else {
            double inc = spCoord->increment()[0];
            freqPrec = int32_t(abs(log10(inc/freq))) + 1;
            double vel0, vel1;
            spCoord->pixelToVelocity(vel0, 0);
            spCoord->pixelToVelocity(vel1, 1);
            if (abs(vel0-vel1) > 10) {
                velPrec = 0;
            }
            else {
                velPrec = int32_t(abs(log10(abs(vel0-vel1)))) + 2;
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
        int32_t stokesPos = hasStokes
            ? hasSpectral
                ? 1 : 0
                : -1;
        IPosition axisPath = hasSpectral && hasStokes
                ? IPosition(2, 1, 0)
                        : IPosition(1, 0);
        ArrayPositionIterator iter(beamsShape, axisPath, false);
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
        uint32_t mymax = hasStokes ? nstokes() : 1;
        for (uint32_t i=0; i<mymax; i++) {
            String stokesString;
            if (hasStokes) {
                Stokes::StokesTypes stokes;
                polCoord->toWorld(stokes, i);
                stokesString = Stokes::name(stokes);
            }
            for (uint32_t j=0; j<3; j++) {
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

const Quantum<Matrix<double>> ImageBeamSet::getAreas() const {
    return Quantum<Matrix<double>>(_areas, _areaUnit);
}

const std::map<String, Quantum<Matrix<double>>> ImageBeamSet::paramMatrices(
    const Unit& majminUnit, const Unit& paUnit
) const {
    const auto& beams = getBeams();
    IPosition shape(beams.shape());
    Matrix<double> mymaj(shape);
    Matrix<double> mymin(shape);
    Matrix<double> pa(shape);
    auto majIter = mymaj.begin();
    auto minIter = mymin.begin();
    auto paIter = pa.begin();
    for (const auto& beam: beams) {
        ThrowIf(
            beam.isNull(),
            "Invalid per plane beam found"
        );
        *majIter = beam.getMajor(majminUnit);
        *minIter = beam.getMinor(majminUnit);
        *paIter = beam.getPA(paUnit, false);
        ++majIter;
        ++minIter;
        ++paIter;
    }
    std::map<String, Quantum<Matrix<double>>> mymap;
    mymap["major"] = Quantum<Matrix<double>>(mymaj, majminUnit);
    mymap["minor"] = Quantum<Matrix<double>>(mymin, majminUnit);
    mymap["pa"] = Quantum<Matrix<double>>(pa, paUnit);
    return mymap;
}

void ImageBeamSet::_chanInfoToStream(
    ostream& os, const SpectralCoordinate *spCoord,
    const uint32_t chan, const uint32_t chanWidth, const uint32_t freqPrec,
    const uint32_t velWidth, const uint32_t velPrec
) {
    os << std::fixed << std::setw(chanWidth)
        << chan << " ";
    double freq;
    spCoord->toWorld(freq, chan);
    os << scientific << std::setprecision(freqPrec)
        << freq << " ";
    double vel;
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
    Quantity pa = beam.getPA(true);
    pa.convert("deg");
    os << fixed << std::setprecision(4) << std::setw(9) <<  majAx
        << " x " << std::setw(9) << minAx << " pa=" << std::setw(8) << pa;
}

}
