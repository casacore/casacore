//# tSpectralFit.cc -- test SpectralElement; SpectralEstimate and SpectralFit
//# Copyright (C) 2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Random.h>
#include <trial/Wnbt/SpectralElement.h>
#include <trial/Wnbt/SpectralEstimate.h>
#include <trial/Wnbt/SpectralFit.h>
#include <aips/iostream.h>

int main() {
  // get estimates
  {
    cout << "Test SpectralEstimate" << endl;
    cout << "---------------------------------------------------" << endl;

    const Int NPAR = 9;
    const Int NPTS = 100;
    ///    Int q = 1;
    Int r;
    Int n = NPTS;
    Int np = NPAR;
    ///    Double rms = 0.5;
    ///    Double cutoff= 1.0;
    ///    Double minsig = 0.5;
    Double par[NPAR];
    ///    Double w[NPTS];
    Vector<Float> y(NPTS);

    par[0] = 5.0; par[1] = 45.0; par[2] = 1.1;
    par[3] = 4.0; par[4] = 50.0; par[5] = 1.1;
    par[6] = 6.0; par[7] = 55.0; par[8] = 1.1;
    for (Int i = 0; i < n; i++) {
      y(i) = 0.0;
      for (Int j = 0; j < np; j += 3) {
	if (par[j] != 0.0) {
	  Double a = par[j];
	  Double c = (par[j+1] - (Double) i);
	  Double s = par[j+2];
	  y(i) += a * exp( -0.5 * c / s * c / s );
	};
      };
    };
    cout << "Made spectrum with components: " << endl;
    for (uInt i=0; i<3; i++) {
      cout << SpectralElement(SpectralElement::GAUSSIAN,
			      par[3*i+0], par[3*i+1], par[3*i+2]) << endl;
    };
    SpectralEstimate est;
    r = est.estimate(y);
    cout << "Found: " << r << " estimates" << endl;
    for (Int i=0; i<r; i++) {
      cout << "Estimate " << i << ": " << est.element(i) << endl;
    };
  }

  // test fitter
  {
    // Number of components
    const uInt ncomp = 4;
    // Component data
    Double ampl[ncomp] = { 1, 2.5, 0.5, 5 };
    Double center[ncomp] = { 0.25, 0.5, 0.75, 0.3 };
    Double sigma[ncomp] = { 2, 4, 8, 3 };
    MLCG genit;
    Normal noise(&genit, 0.0, 0.25);
    Vector<Double> freq(1024);
    for (uInt i=0; i<1024; i++) freq(i) = 1400 + i*10.0/1024.0;
    
    try {
      cout << "Test SpectralFit" << endl;
      cout << "---------------------------------------------------" << endl;
      
      SpectralElement el[ncomp] = {
	SpectralElement(SpectralElement::GAUSSIAN, ampl[0],
			center[0]*10.+1400., sigma[0]*10./1024.),
	SpectralElement(SpectralElement::GAUSSIAN, ampl[1],
			center[1]*10.+1400., sigma[1]*10./1024.),
	SpectralElement(SpectralElement::GAUSSIAN, ampl[2],
			center[2]*10.+1400., sigma[2]*10./1024.),
	SpectralElement(SpectralElement::GAUSSIAN, ampl[3],
			center[3]*10.+1400., sigma[3]*10./1024.) };
      
      cout << "Spectral elements: " << endl;
      for (uInt j=0; j<ncomp; j++) cout << el[j] << endl;
      cout << "---------------------------------------------------" << endl;
      
      Vector<Double> dat(1024);
      for (uInt i=0; i<1024; i++) {
	dat(i) = 0;
	for (uInt j=0; j<ncomp; j++) {
	  dat(i) += el[j].getAmpl()*exp(-(freq(i)-el[j].getCenter())*
					(freq(i)-el[j].getCenter())*log(16.0)/
					el[j].getSigma()/
					el[j].getSigma());
	};
	dat(i) += noise();
      };
      cout << "Data for frequencies " << freq(300) << " - " << freq(315) <<
	endl;
      for (uInt i=300; i<316; i++) cout << freq(i) << ": " << dat(i) << endl;
      cout << "---------------------------------------------------" << endl;
      
      cout << "Specify fitter on the 4 gaussians:" << endl;
      SpectralFit fitter;
      for (uInt i=0; i<ncomp; i++) fitter.addFitElement(el[i]);
      for (uInt i=0; i<fitter.getNElements(); i++) {
	cout << fitter.getElement(i) << endl;
      };
      cout << "---------------------------------------------------" << endl;
      cout << "Execute the fitter, can do (1:Y): " <<
	fitter.fit(freq, dat) << endl;
      cout << "The results: " << endl;
      for (uInt i=0; i<fitter.getNElements(); i++) {
	cout << fitter.getElement(i) << endl;
      };
      cout << "---------------------------------------------------" << endl;
      
      cout << "Different start values: " << endl;
      for (uInt i=0; i<ncomp; i++) {
	el[i].setAmpl(0.9*el[i].getAmpl());
	el[i].setSigma(0.95*el[i].getSigma());
	fitter.setFitElement(i, el[i]);
      };
      for (uInt i=0; i<fitter.getNElements(); i++) {
	cout << fitter.getElement(i) << endl;
      };
      cout << "Execute the fitter, can do (1:Y): " <<
	fitter.fit(freq, dat) << endl;
      cout << "The results: " << endl;
      for (uInt i=0; i<fitter.getNElements(); i++) {
	cout << fitter.getElement(i) << endl;
      };
      
      cout << "---------------------------------------------------" << endl;
      cout << "Differences: " << endl;
      Vector<Double> xdat(1024);
      Double mx(-1e6);
      Double mn(1e6);
      Double avg(0);
      Double sg(0);
      for (uInt i=0; i<1024; i++) {
	xdat(i) = dat(i);
	for (uInt j=0; j<fitter.getNElements(); j++) {
	  xdat(i) -= fitter.getElement(j).getAmpl()*
	    exp(-(freq(i)-fitter.getElement(j).getCenter())*
		(freq(i)-fitter.getElement(j).getCenter())*log(16.0)/
		fitter.getElement(j).getSigma()/
		fitter.getElement(j).getSigma());
	};
	mx = (xdat(i)>mx) ? xdat(i) : mx;
	mn = (xdat(i)<mn) ? xdat(i) : mn;
	avg += xdat(i);
	sg += xdat(i)*xdat(i);
      };
      avg /= 1024.;
      sg = sqrt(sg/1024./1023.);
      cout << "Min difference: " << mn <<
	", max: " << mx << ", average: " << avg <<
	", sigma: " << sg << endl;
      cout << "---------------------------------------------------" << endl;
      
    } catch (AipsError x) {
      cout << x.getMesg() << endl;
    };
  }
  
  exit(0);
}
