//# SpectralEstimate.cc: Get an initial estimate for spectral lines
//# Copyright (C) 2001,2002,2003,2004
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

//# Includes
#include <components/SpectralComponents/SpectralEstimate.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicMath/Math.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constructors
SpectralEstimate::SpectralEstimate(const uInt maxpar) :
  useWindow_p(False), rms_p(0), cutoff_p(0),
  windowLow_p(0), windowEnd_p(0),
  regionLow_p(0), regionEnd_p(0),
  q_p(2), sigmin_p(0),
  deriv_p(0), slist_p(maxpar), lprof_p(0) {
  setQ();
}

SpectralEstimate::SpectralEstimate(const Double rms,
				   const Double cutoff, const Double minsigma,
				   const uInt maxpar) :
  useWindow_p(False), rms_p(rms), cutoff_p(cutoff),
  windowLow_p(0), windowEnd_p(0),
  regionLow_p(0), regionEnd_p(0),
  q_p(2), sigmin_p(minsigma),
  deriv_p(0), slist_p(maxpar), lprof_p(0) {
  setQ();
}


SpectralEstimate::SpectralEstimate(const SpectralEstimate &other) :
  useWindow_p(other.useWindow_p), rms_p(other.rms_p), cutoff_p(other.cutoff_p),
  windowLow_p(other.windowLow_p), windowEnd_p(other.windowEnd_p),
  regionLow_p(other.regionLow_p), regionEnd_p(other.regionEnd_p),
  q_p(other.q_p), sigmin_p(other.sigmin_p),
  deriv_p(0), slist_p(other.slist_p), lprof_p(other.lprof_p) {
  setQ(q_p);
  deriv_p = new Double[lprof_p];
  for (uInt i=0; i<lprof_p; i++) deriv_p[i] = other.deriv_p[i];
}

SpectralEstimate::~SpectralEstimate() {
  delete [] deriv_p; deriv_p = 0; lprof_p = 0;
}

SpectralEstimate &SpectralEstimate::operator=(const SpectralEstimate &other) {
  if (this != &other) {
    useWindow_p = other.useWindow_p; 
    rms_p = other.rms_p; 
    cutoff_p = other.cutoff_p;
    windowLow_p = other.windowLow_p;
    windowEnd_p = other.windowEnd_p;
    regionLow_p = other.regionLow_p;
    regionEnd_p = other.regionEnd_p;
    q_p = other.q_p;
    sigmin_p = other.sigmin_p;
    deriv_p = 0;
    slist_p = other.slist_p;
    lprof_p = other.lprof_p;
    setQ(q_p);
    deriv_p = new Double[lprof_p];
    for (uInt i=0; i<lprof_p; i++) deriv_p[i] = other.deriv_p[i];
  };
  return *this;
}

void SpectralEstimate::setRMS(const Double rms) {
  rms_p = abs(rms);
}

void SpectralEstimate::setCutoff(const Double cutoff) {
  cutoff_p = max(0.0, cutoff);
}

void SpectralEstimate::setMinSigma(const Double minsigma) {
  sigmin_p = max(0.0, minsigma);
}

void SpectralEstimate::setQ(const uInt q) {
  q_p = max(1, Int(q));
  a_p = 90.0/(q_p*(q_p+1)*(4*q_p*q_p-1)*(2*q_p+3));
  b_p = (q_p*(q_p+1))/3.0;
}

void SpectralEstimate::setRegion(const Int lo, const Int hi) {
  regionLow_p = regionEnd_p = 0;
  if (hi > lo) {
    regionLow_p = lo;
    regionEnd_p = hi;
  };
}

void SpectralEstimate::setWindowing(const Bool win) {
  useWindow_p = win;
}

void SpectralEstimate::setMaxN(const uInt maxpar) {
  slist_p.set(maxpar);
}

} //# NAMESPACE CASA - END


//# Cater for Double and Float
#ifdef AIPS_NO_TEMPLATE_SRC
#include <components/SpectralComponents/Spectral2Estimate.tcc>

namespace casa { //# NAMESPACE CASA - BEGIN
template SpectralList const & SpectralEstimate::estimate<Float>(Vector<Float> const &, Vector<Float> *);
template SpectralList const & SpectralEstimate::estimate<Float>(Vector<Float> const &, Vector<Float> const &);
template SpectralElement SpectralEstimate::convertElement<Float>(Vector<Float> const &,
                                                                 SpectralElement const &) const;
template void SpectralEstimate::findga<Float>(Vector<Float> const &); 
template uInt SpectralEstimate::window<Float>(Vector<Float> const &);
template void SpectralEstimate::findc2<Float>(Vector<Float> const &); 

template SpectralList const & SpectralEstimate::estimate<Double>(Vector<Double> const &, Vector<Double> *);
template SpectralList const & SpectralEstimate::estimate<Double>(Vector<Double> const &, Vector<Double> const &);
template SpectralElement SpectralEstimate::convertElement<Double>(Vector<Double> const &,
                                                                 SpectralElement const &) const;
template void SpectralEstimate::findga<Double>(Vector<Double> const &);
template uInt SpectralEstimate::window<Double>(Vector<Double> const &);
template void SpectralEstimate::findc2<Double>(Vector<Double> const &);
} //# NAMESPACE CASA - END
#endif
