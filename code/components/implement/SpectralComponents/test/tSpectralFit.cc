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
#include <trial/Wnbt/SpectralElement.h>
#include <trial/Wnbt/SpectralEstimate.h>
#include <trial/Wnbt/SpectralFit.h>
#include <trial/Tasking/PGPlotter.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Random.h>
#include <aips/Inputs/Input.h>
#include <aips/iostream.h>

int main(int argc, char **argv) {
      
  // Inputs
  Input inputs(1);
  inputs.version("$Id$");
  inputs.create("plotter", "", "plotter"); 
  inputs.readArguments(argc, argv);
  const String device = inputs.getString("plotter"); 

  PGPlotter plotter(device, 3, 3);
  // get estimates
  {
    cout << "Test SpectralEstimate" << endl;
    cout << "---------------------------------------------------" << endl;

    const Int NPAR = 9;
    const Int NPTS = 100;
    uInt q = 2;
    Int r;
    Int n = NPTS;
    Int np = NPAR;
    Double rms = 0.5;
    Double cutoff= 1.0;
    Double minsig = 0.5;
    Double par[NPAR];
    Vector<Float> y(NPTS);
    Vector<Float> xy(NPTS);

    par[0] = 5.0; par[1] = 45.0; par[2] = 1.1;
    par[3] = 4.0; par[4] = 50.0; par[5] = 1.1;
    par[6] = 6.0; par[7] = 55.0; par[8] = 1.1;
    for (Int i = 0; i<n; i++) {
      y(i) = 0.0;
      xy(i) = i;
      for (Int j = 0; j < np; j += 3) {
	if (par[j] != 0.0) {
	  Double a = par[j];
	  Double c = (par[j+1] - Double(i));
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
    plotter.env(xy(0), xy(n-1), -1, 12, 0, 0);
    plotter.lab("", "", "Noiseless data with 3 components and "
		"estimate residuals");
    plotter.line(xy, y);
    {
      SpectralEstimate est(rms, cutoff, minsig);
      Vector<Float> deriv(n);
      r = est.estimate(y, &deriv);
      cout << "Found (using rms, cutoff, minsig, q): (" <<
	rms << ", " << cutoff << ", " << minsig << ", " << q << ") " <<
	r << " estimates" << endl;
      for (Int i=0; i<r; i++) {
	cout << "Estimate " << i << ": " << est.element(i) << endl;
      };
      Vector<Float> res(NPTS);
      Float rmn = 1e6;
      Float rmx = -1e6;
      for (Int i=0; i<n; i++) {
	res(i) = y(i);
	for (Int j = 0; j < r; j++) {
	  Double a = est.element(j).getAmpl();
	  Double c = (est.element(j).getCenter() - Double(i));
	  Double s = est.element(j).getSigma();
	  res(i) -= a * exp( -log(16.0) * c / s * c / s );
	  rmn = min(rmn, res(i));
	  rmx = max(rmx, res(i));
	};
      };
      plotter.sci(2);
      plotter.line(xy, res);
      plotter.env(xy(0), xy(n-1), rmn, rmx, 0, 0);
      plotter.sci(1);
      plotter.lab("", "", "Noiseless spectrum with 2nd derivative");
      plotter.line(xy, y);
      plotter.sci(2);
      plotter.line(xy, deriv);
      plotter.env(xy(0), xy(n-1), -1, 12, 0, 0);
      plotter.sci(1);
      plotter.lab("", "", "Noiseless spectrum with estimates");
      plotter.line(xy, y);
      plotter.sci(2);
      for (Int j=0; j<r; j++) {
	for (Int i = 0; i<n; i++) {
	  Double a = est.element(j).getAmpl();
	  Double c = (est.element(j).getCenter() - Double(i));
	  Double s = est.element(j).getSigma();
	  res(i) = a * exp( -log(16.0) * c / s * c / s );
	};
	plotter.line(xy, res);
      };
    }
    {
      SpectralEstimate est(rms, cutoff, minsig);
      est.setWindowing(True);
      r = est.estimate(y);
      cout << "Window (using rms, cutoff, minsig, q): (" <<
	rms << ", " << cutoff << ", " << minsig << ", " << q << ") " <<
	r << " estimates" << endl;
      for (Int i=0; i<r; i++) {
	cout << "Estimate " << i << ": " << est.element(i) << endl;
      };
    }
    {
      q = 1;
      SpectralEstimate est(rms, cutoff, minsig);
      est.setQ(q);
      r = est.estimate(y);
      cout << "Found (using rms, cutoff, minsig, q): (" <<
	rms << ", " << cutoff << ", " << minsig << ", " << q << ") " <<
	r << " estimates" << endl;
      for (Int i=0; i<r; i++) {
	cout << "Estimate " << i << ": " << est.element(i) << endl;
      };
    }
    {
      q = 1;
      rms = cutoff = minsig = 0.0;
      SpectralEstimate est(rms, cutoff, minsig);
      est.setQ(q);
      r = est.estimate(y);
      cout << "Found (using rms, cutoff, minsig, q): (" <<
	rms << ", " << cutoff << ", " << minsig << ", " << q << ") " <<
	r << " estimates" << endl;
      for (Int i=0; i<r; i++) {
	cout << "Estimate " << i << ": " << est.element(i) << endl;
      };
    }
    {
      q = 2;
      rms = cutoff = minsig = 0.0;
      SpectralEstimate est(rms, cutoff, minsig);
      est.setQ(q);
      r = est.estimate(y);
      cout << "Found (using rms, cutoff, minsig, q): (" <<
	rms << ", " << cutoff << ", " << minsig << ", " << q << ") " <<
	r << " estimates" << endl;
      for (Int i=0; i<r; i++) {
	cout << "Estimate " << i << ": " << est.element(i) << endl;
      };
    }
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
    Normal noise(&genit, 0.0, 0.1);
    Vector<Double> freq(1024);
    Vector<Float> ffreq(1024);
    for (uInt i=0; i<1024; i++) {
      freq(i) = 1400 + i*10.0/1024.0;
      ffreq(i) = freq(i);
    };

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
      Vector<Float> fdat(1024);
      for (uInt i=0; i<1024; i++) {
	dat(i) = 0;
	for (uInt j=0; j<ncomp; j++) {
	  dat(i) += el[j].getAmpl()*exp(-(freq(i)-el[j].getCenter())*
					(freq(i)-el[j].getCenter())*log(16.0)/
					el[j].getSigma()/
					el[j].getSigma());
	};
	dat(i) += noise();
	fdat(i) = dat(i);
      };
      cout << "Data for frequencies " << freq(300) << " - " << freq(315) <<
	endl;
      for (uInt i=300; i<316; i++) cout << freq(i) << ": " << dat(i) << endl;
      cout << "---------------------------------------------------" << endl;
      
      plotter.env(ffreq(0), ffreq(1023), -1, 12, 0, 0);
      plotter.sci(1);
      plotter.lab("", "", "Synthetic spectra and residual after fit");
      plotter.line(ffreq, fdat);

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
      Vector<Float> fxdat(1024);
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
	  fxdat(i) = xdat(i);
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
      plotter.sci(2);
      plotter.line(ffreq, fxdat);
      cout << "---------------------------------------------------" << endl;
      
      cout << "---------------------------------------------------" << endl;
      cout << "Estimates: " << endl;
      SpectralEstimate mest(0.1, 0.5);
      mest.setQ(5);
      Int mr = mest.estimate(fdat);
      cout << "Found " << mr << " estimates" << endl;
      for (Int i=0; i<mr; i++) {
	cout << "Estimate " << i << ": " << mest.element(i) << endl;
      };
      plotter.env(ffreq(0), ffreq(1023), -1, 12, 0, 0);
      plotter.sci(1);
      plotter.lab("", "", "Synthetic spectra and residual after estimates");
      plotter.line(ffreq, fdat);
      for (uInt i=0; i<1024; i++) {
	xdat(i) = 0;
	for (Int j = 0; j < mr; j++) {
	  Double a = mest.element(j).getAmpl();
	  Double c = (mest.element(j).getCenter() - Double(i));
	  Double s = mest.element(j).getSigma();
	  xdat(i) += a * exp( -log(16.0) * c / s * c / s );
	};
	mx = (xdat(i)>mx) ? xdat(i) : mx;
	mn = (xdat(i)<mn) ? xdat(i) : mn;
      };
      plotter.sci(2);
      plotter.line(ffreq, fxdat);
      plotter.env(ffreq(0), ffreq(1023), -1, 12, 0, 0);
      plotter.sci(1);
      plotter.lab("", "", "Synthetic spectrum with estimates");
      plotter.line(ffreq, fdat);
      plotter.sci(2);
      for (Int j=0; j<mr; j++) {
	for (Int i = 0; i<1024; i++) {
	  Double a = mest.element(j).getAmpl();
	  Double c = (mest.element(j).getCenter() - Double(i));
	  Double s = mest.element(j).getSigma();
	  fxdat(i) = a * exp( -log(16.0) * c / s * c / s );
	};
	plotter.line(ffreq, fxdat);
      };
      plotter.env(ffreq(0), ffreq(1023), -1, 12, 0, 0);
      
      cout << "---------------------------------------------------" << endl;
      
    } catch (AipsError x) {
      cout << x.getMesg() << endl;
    };
  }
  
  exit(0);
}
