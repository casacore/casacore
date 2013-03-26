//# ImageBeamSet.cc: Collection of image beams in frequency and frequency
//# Copyright (C) 2012
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
#include <images/Images/ImageBeamSet.h>
#include <coordinates/Coordinates/CoordinateSystem.h>

// debug only
///#include <casa/Arrays/ArrayIO.h>

namespace casa {

  const String ImageBeamSet::_DEFAULT_AREA_UNIT = "arcsec2";

  ImageBeamSet::ImageBeamSet()
    : _areaUnit   (_DEFAULT_AREA_UNIT),
      _minBeam    (GaussianBeam::NULL_BEAM),
      _maxBeam    (GaussianBeam::NULL_BEAM),
      _minBeamPos (2,0),
      _maxBeamPos (2,0)
  {}

  ImageBeamSet::ImageBeamSet(const Matrix<GaussianBeam>& beams)
    : _beams(beams)
  {
    _calculateAreas();
  }

  ImageBeamSet::ImageBeamSet(const GaussianBeam& beam)
    : _beams      (1, 1, beam),
      _areas      (1, 1, beam.getArea(_DEFAULT_AREA_UNIT)),
      _areaUnit   (_DEFAULT_AREA_UNIT),
      _minBeam    (beam),
      _maxBeam    (beam),
      _minBeamPos (2, 0),
      _maxBeamPos (2, 0)
{}

  ImageBeamSet::ImageBeamSet(uInt nchan, uInt nstokes)
    : _beams      (nchan, nstokes),
      _areas      (nchan, nstokes, 0.),
      _areaUnit   (_DEFAULT_AREA_UNIT),
      _minBeam    (GaussianBeam::NULL_BEAM),
      _maxBeam    (GaussianBeam::NULL_BEAM),
      _minBeamPos (2, 0),
      _maxBeamPos (2, 0)
  {}

  ImageBeamSet::ImageBeamSet(const IPosition& shape)
    : _areaUnit   (_DEFAULT_AREA_UNIT),
      _minBeam    (GaussianBeam::NULL_BEAM),
      _maxBeam    (GaussianBeam::NULL_BEAM),
      _minBeamPos (2, 0),
      _maxBeamPos (2, 0)
  {
    AlwaysAssert (shape.size() == 2, AipsError);
    _beams.resize (shape);
    _areas.resize (shape);
  }

  ImageBeamSet::ImageBeamSet(uInt nchan, uInt nstokes,
                             const GaussianBeam& beam)

    : _beams      (nchan, nstokes, beam),
      _areas      (nchan, nstokes, beam.getArea(_DEFAULT_AREA_UNIT)),
      _areaUnit   (_DEFAULT_AREA_UNIT),
      _minBeam    (beam),
      _maxBeam    (beam),
      _minBeamPos (2, 0),
      _maxBeamPos (2, 0)
  {}

  ImageBeamSet::ImageBeamSet(const ImageBeamSet& other)
    : _beams      (other._beams),
      _areas      (other._areas),
      _areaUnit   (other._areaUnit),
      _minBeam    (other._minBeam),
      _maxBeam    (other._maxBeam),
      _minBeamPos (other._minBeamPos),
      _maxBeamPos (other._maxBeamPos)
  {}

  ImageBeamSet::~ImageBeamSet()
  {}

  ImageBeamSet& ImageBeamSet::operator=(const ImageBeamSet& other)
  {
    if (this != &other) {
      _beams.assign (other._beams);
      _areas.assign (other._areas);
      _areaUnit   = other._areaUnit;
      _minBeam    = other._minBeam;
      _maxBeam    = other._maxBeam;
      _minBeamPos = other._minBeamPos;
      _maxBeamPos = other._maxBeamPos;
    }
    return *this;
  }

  const GaussianBeam& ImageBeamSet::getBeam(Int chan, Int stokes) const
  {
    if (nchan()   == 1)  chan  = 0;
    if (nstokes() == 1) stokes = 0;
    // Note that chan=-1 can only be given if nchan()==1.
    AlwaysAssert (chan >=0  &&  chan < Int(nchan())  &&
                  stokes >= 0  &&  stokes < Int(nstokes()), AipsError);
    return _beams(chan, stokes);
  }

  Bool ImageBeamSet::operator== (const ImageBeamSet& other) const
  {
    return (this == &other  ||  (_beams.shape() == other._beams.shape()  &&
                                 allEQ(_beams, other._beams)));
  }

  Bool ImageBeamSet::operator!= (const ImageBeamSet& other) const
  {
    return ! (*this == other);
  }

  const GaussianBeam& ImageBeamSet::getBeam() const
  {
    if (_beams.nelements() > 1) {
      throw AipsError(String(className()) + "::" + __FUNCTION__ +
                      ": This object contains multiple beams, "
                      "not a single beam");
    }
    return _beams(0,0);
  }

  const String& ImageBeamSet::className() {
    static const String c = "ImageBeamSet";
    return c;
  }

  void ImageBeamSet::resize(uInt nchan, uInt nstokes)
  {
    _beams.resize(nchan, nstokes);
    _calculateAreas();
  }

  void ImageBeamSet::setBeams(const Matrix<GaussianBeam>& beams)
  {
    // Resize the beams if needed.
    // A beam axis can be extended if its length is 0 or 1.
    Int nch = nchan();
    Int beamNchan = beams.shape()[0];
    if (nch <= 1) nch = beamNchan;
    Int nst = nstokes();
    Int beamNstokes = beams.shape()[1];
    if (nst <= 1) nst = beams.shape()[1];
    AlwaysAssert ((beamNchan   == nch  ||  beamNchan   == 1)  &&
                  (beamNstokes == nst  ||  beamNstokes == 1), AipsError);
    // Determine the step size in the given beams.
    Int incrChan   = (beamNchan   == 1  ?  0 : 1);
    Int incrStokes = (beamNstokes == 1  ?  0 : 1);
    // Set the beam set to the given beams.
    _beams.resize (nch, nst);
    Int js = 0;
    for (Int is=0; is<nst; ++is, js+=incrStokes) { 
      Int jc = 0;
      for (Int ic=0; ic<nch; ++ic, jc+=incrChan) { 
        _beams(ic,is) = beams(jc,js);
      }
    }
    _calculateAreas();
  }

  void ImageBeamSet::set(const GaussianBeam& beam)
  {
    _beams      = beam;
    _areas      = beam.getArea(_areaUnit);
    _minBeam    = beam;
    _maxBeam    = beam;
    _minBeamPos = 0;
    _maxBeamPos = 0;
  }

  void ImageBeamSet::setBeam(uInt chan, uInt stokes,
                             const GaussianBeam& beam)
  {
    AlwaysAssert (Int(chan) < _beams.shape()[0]  &&
                  Int(stokes) < _beams.shape()[1], AipsError);
    _beams(chan, stokes) = beam;
    Double area = beam.getArea(_areaUnit);
    _areas(chan, stokes) = area;
    if (area < _areas(_maxBeamPos)) {
      _minBeam    = beam;
      _minBeamPos = IPosition(2, chan, stokes);
    }
    if (area > _areas(_maxBeamPos)) {
      _maxBeam    = beam;
      _maxBeamPos = IPosition(2, chan, stokes);
    }
  }

  const GaussianBeam& ImageBeamSet::getMaxAreaBeamForPol(IPosition& pos,
                                                         uInt stokes) const
  {
    pos.resize(2);
    // If single Stokes, use the maximum itself.
    if (nstokes() == 1) {
      pos = _maxBeamPos;
      return _maxBeam;
    }
    AlwaysAssert (stokes < nstokes(), AipsError);
    // Determine location of maximum area for given Stokes.
    Double mina, maxa;
    IPosition minPos;
    minMax (mina, maxa, minPos, pos,
            _areas(IPosition(2,0,stokes), IPosition(2,nchan()-1, stokes)));
    pos[1] = stokes;
    return _beams(pos);
}

  const GaussianBeam& ImageBeamSet::getMinAreaBeamForPol(IPosition& pos,
                                                         uInt stokes) const
  {
    pos.resize(2);
    // If single Stokes, use the minimum itself.
    if (nstokes() == 1) {
      pos = _minBeamPos;
      return _minBeam;
    }
    AlwaysAssert (stokes < nstokes(), AipsError);
    // Determine location of minimum area for given Stokes.
    Double mina, maxa;
    IPosition maxPos;
    minMax (mina, maxa, pos, maxPos,
            _areas(IPosition(2,0,stokes), IPosition(2,nchan()-1,stokes)));
    pos[1] = stokes;
    return _beams(pos);
}

  const GaussianBeam& ImageBeamSet::getMedianAreaBeamForPol(IPosition& pos,
                                                            uInt stokes) const
  {
    pos.resize(2);
    pos = _beams.shape() - 1;
    if (nstokes() > 1) {
      pos[1] = stokes;
    }
    AlwaysAssert (pos[1] >= 0  &&  pos[1] < _beams.shape()[1], AipsError);
    if (nchan() == 1) {
      return _beams(0, pos[1]);
    }
    // Do an indirect sort to find the location of the median.
    Vector<uInt> indices;
    GenSortIndirect<Double>::sort(indices,
                                  _areas(IPosition(2,0,pos[1]),
                                         IPosition(2,nchan()-1,pos[1])));
    pos[0] = indices[indices.size()/2];
    return _beams(pos[0], pos[1]);
  }

  void ImageBeamSet::_calculateAreas()
  {
    _areas.resize (_beams.shape());
    if (! _beams.empty()) {
      _areaUnit = _beams.begin()->getMajor().getUnit();
      _areaUnit = (Quantity(Quantity(1, _areaUnit) *
                            Quantity(1, _areaUnit)).getUnit());
      Array<Double>::iterator             iareas = _areas.begin();
      Array<GaussianBeam>::const_iterator ibend  = _beams.end();
      for (Array<GaussianBeam>::const_iterator ibeam=_beams.begin();
           ibeam!=ibend; ++ibeam, ++iareas) {
        *iareas = ibeam->getArea(_areaUnit);
      }
      Double minArea, maxArea;
      minMax(minArea, maxArea, _minBeamPos, _maxBeamPos, _areas);
      _minBeam = _beams(_minBeamPos);
      _maxBeam = _beams(_maxBeamPos);
    }
  }

  ostream &operator<<(ostream &os, const ImageBeamSet& beamSet)
  {
    os << beamSet.getBeams();
    return os;
  }

  ImageBeamSet ImageBeamSet::subset(const Slicer& slicer,
                                    const CoordinateSystem& csys) const
  {
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
    IPosition beamss(2,0), beamse(2,0), beamsi(2,1);
    for (Int i=0; i<2; ++i) {
      if (axes[i] >= 0  &&  _beams.shape()[i] > 1) {
        AlwaysAssert (_beams.shape()[i] > se[axes[i]], AipsError);
        beamss[i] = ss[axes[i]];
        beamse[i] = se[axes[i]];
        beamsi[i] = si[axes[i]];
      }
    }
    return ImageBeamSet (_beams(beamss, beamse, beamsi));
  }

  Bool ImageBeamSet::equivalent (const ImageBeamSet& that) const
  {
    if (empty() != that.empty()) {
      return False;      // mismatch; only one has a beam.
    } else if (empty()) {
      return True;       // match; both have no beam.
    }
    uInt nc1 = nchan();
    uInt np1 = nstokes();
    uInt nc2 = that.nchan();
    uInt np2 = that.nstokes();
    if (!(nc1 == nc2  ||  nc1 == 1  ||  nc2 == 1)  ||
        !(np1 == np2  ||  np1 == 1  ||  np2 == 1)) {
      return False;      // shapes mismatch
    }
    uInt nc = max(nc1,nc2);
    uInt np = max(np1,np2);
    uInt incrc1 = (nc1==1 ? 0:1);
    uInt incrp1 = (np1==1 ? 0:1);
    uInt incrc2 = (nc2==1 ? 0:1);
    uInt incrp2 = (np2==1 ? 0:1);
    uInt c1=0, p1=0, c2=0, p2=0;
    for (uInt p=0; p<np; ++p) {
      for (uInt c=0; c<nc; ++c, c1+=incrc1, c2+=incrc2) {
        if (_beams(c1,p1) != that._beams(c2,p2)) {
          return False;  // mismatch in a beam
        }
      }
      c1 = c2 = 0;
      p1 += incrp1;
      p2 += incrp2;
    }
    return True;
  }
  
} // end namespace
