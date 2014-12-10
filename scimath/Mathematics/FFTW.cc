//# Copyright (C) 1994,1995,1996,1997,1998,1999,2003
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


#include <casacore/scimath/Mathematics/FFTW.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/HostInfo.h>

#ifdef HAVE_FFTW3
# include <fftw3.h>
#endif

#include <iostream>


namespace casacore {

  volatile Bool FFTW::is_initialized_fftw = False;
  Mutex FFTW::theirMutex;


#ifdef HAVE_FFTW3

  class FFTWPlan
  {
  public:
    explicit FFTWPlan (fftw_plan plan)
      : itsPlan(plan)
    {}
    ~FFTWPlan()
      { fftw_destroy_plan(itsPlan); }
    fftw_plan getPlan()
      { return itsPlan; }
  private:
    FFTWPlan (const FFTWPlan&);
    FFTWPlan& operator= (const FFTWPlan&);
    fftw_plan itsPlan;
  };

  class FFTWPlanf
  {
  public:
    explicit FFTWPlanf (fftwf_plan plan)
      : itsPlan(plan)
    {}
    ~FFTWPlanf()
      { fftwf_destroy_plan(itsPlan); }
    fftwf_plan getPlan()
      { return itsPlan; }
  private:
    FFTWPlanf (const FFTWPlanf&);
    FFTWPlanf& operator= (const FFTWPlanf&);
    fftwf_plan itsPlan;
  };

    

  FFTW::FFTW()
    : itsPlanR2Cf  (0),
      itsPlanR2C   (0),
      itsPlanC2Rf  (0),
      itsPlanC2R   (0),
      itsPlanC2CFf (0),
      itsPlanC2CF  (0),
      itsPlanC2CBf (0),
      itsPlanC2CB  (0)
  {
    if (!is_initialized_fftw) {
      ScopedMutexLock lock(theirMutex);
      if (!is_initialized_fftw) {
        int numCPUs = HostInfo::numCPUs();
        int nthreads = 1;
        // cerr << "Number of threads is " << numCPUs << endl;
        if (numCPUs > 1) {
          nthreads = numCPUs;
        }
      
        //    std::cout << "init threads " << fftwf_init_threads() << std::endl;
        //    std::cout << "init threads " << fftw_init_threads() << std::endl;
#ifdef HAVE_FFTW3_THREADS
        fftwf_init_threads();
        fftw_init_threads();
        fftwf_plan_with_nthreads(nthreads);
        fftw_plan_with_nthreads(nthreads);
#endif
        is_initialized_fftw = True;
      }
    }
    //    std::cerr << "will use " << nthreads << " threads " << std::endl;

    flags = FFTW_ESTIMATE;  
    
    //flags = FFTW_MEASURE;  // std::cerr << "Will FFTW_MEASURE..." << std::endl;
    //flags = FFTW_PATIENT;   std::cerr << "Will FFTW_PATIENT..." << std::endl;
    //flags = FFTW_EXHAUSTIVE;   std::cerr << "Will FFTW_EXHAUSTIVE..." << std::endl;
  }

  FFTW::~FFTW()
  {
    delete itsPlanR2Cf;
    delete itsPlanR2C;
    delete itsPlanC2Rf;
    delete itsPlanC2R;
    delete itsPlanC2CFf;
    delete itsPlanC2CF;
    delete itsPlanC2CBf;
    delete itsPlanC2CB;
    // We cannot deinitialize FFTW as in the following because
    // there may be other instances of this class around
    // Could do it when keeping a static counter, but must be made thread-safe.
#if 0
    fftw_cleanup();
    fftwf_cleanup();
    fftw_cleanup_threads();
    fftwf_cleanup_threads();
#endif
  }
  

  void FFTW::plan_r2c(const IPosition &size, Float *in, Complex *out) 
  {
    delete itsPlanR2Cf;
    itsPlanR2Cf = new FFTWPlanf
      (fftwf_plan_dft_r2c(size.nelements(),
                          size.asVector().data(),
                          in,
                          reinterpret_cast<fftwf_complex *>(out), 
                          flags));
  }

  void FFTW::plan_r2c(const IPosition &size, Double *in, DComplex *out) 
  {
    delete itsPlanR2C;
    itsPlanR2C = new FFTWPlan
      (fftw_plan_dft_r2c(size.nelements(),
                         size.asVector().data(),
                         in,
                         reinterpret_cast<fftw_complex *>(out), 
                         flags));
  }

  void FFTW::plan_c2r(const IPosition &size, Complex *in, Float *out) {
    delete itsPlanC2Rf;
    itsPlanC2Rf = new FFTWPlanf
      (fftwf_plan_dft_c2r(size.nelements(),
                          size.asVector().data(),
                          reinterpret_cast<fftwf_complex *>(in),
                          out, 
                          flags));

  }

  void FFTW::plan_c2r(const IPosition &size, DComplex *in, Double *out) {
    delete itsPlanC2R;
    itsPlanC2R = new FFTWPlan
      (fftw_plan_dft_c2r(size.nelements(),
                         size.asVector().data(),
                         reinterpret_cast<fftw_complex *>(in), 
                         out,
                         flags));
  }

  void FFTW::plan_c2c_forward(const IPosition &size, DComplex *in) {
    delete itsPlanC2CF;
    itsPlanC2CF = new FFTWPlan
      (fftw_plan_dft(size.nelements(),
                     size.asVector().data(),
                     reinterpret_cast<fftw_complex *>(in), 
                     reinterpret_cast<fftw_complex *>(in), 
                     FFTW_FORWARD, flags));

  }
    
  void FFTW::plan_c2c_forward(const IPosition &size, Complex *in) {
    delete itsPlanC2CFf;
    itsPlanC2CFf = new FFTWPlanf
      (fftwf_plan_dft(size.nelements(),
                      size.asVector().data(),
                      reinterpret_cast<fftwf_complex *>(in), 
                      reinterpret_cast<fftwf_complex *>(in), 
                      FFTW_FORWARD, flags));
  }

  void FFTW::plan_c2c_backward(const IPosition &size, DComplex *in) {
    delete itsPlanC2CB;
    itsPlanC2CB = new FFTWPlan
      (fftw_plan_dft(size.nelements(),
                     size.asVector().data(),
                     reinterpret_cast<fftw_complex *>(in), 
                     reinterpret_cast<fftw_complex *>(in), 
                     FFTW_BACKWARD, flags));
      
  }
    
  void FFTW::plan_c2c_backward(const IPosition &size, Complex *in) {
    delete itsPlanC2CBf;
    itsPlanC2CBf = new FFTWPlanf
      (fftwf_plan_dft(size.nelements(),
                      size.asVector().data(),
                      reinterpret_cast<fftwf_complex *>(in), 
                      reinterpret_cast<fftwf_complex *>(in), 
                      FFTW_BACKWARD, flags));
  }

  // the parameters are used only in order to overload this function
  void FFTW::r2c(const IPosition&, Float*, Complex*) 
  {
    fftwf_execute(itsPlanR2Cf->getPlan());
  }
    
  void FFTW::r2c(const IPosition&, Double*, DComplex*) 
  {
    fftw_execute(itsPlanR2C->getPlan());
  }

  void FFTW::c2r(const IPosition&, Complex*, Float*)
  {
    fftwf_execute(itsPlanC2Rf->getPlan());
  }
    
  void FFTW::c2r(const IPosition&, DComplex*, Double*)
  {
    fftw_execute(itsPlanC2R->getPlan());
  }
    
  void FFTW::c2c(const IPosition&, Complex*, Bool forward)
  {
    if (forward) {
      fftwf_execute(itsPlanC2CFf->getPlan());
    } else {
      fftwf_execute(itsPlanC2CBf->getPlan());
    }
  }
    
  void FFTW::c2c(const IPosition&, DComplex*, Bool forward)
  {
    if (forward) {
      fftw_execute(itsPlanC2CF->getPlan());
    } else {
      fftw_execute(itsPlanC2CB->getPlan());
    }
  }

#else

  FFTW::FFTW()
    : itsPlanR2Cf  (0),
      itsPlanR2C   (0),
      itsPlanC2Rf  (0),
      itsPlanC2R   (0),
      itsPlanC2CFf (0),
      itsPlanC2CF  (0),
      itsPlanC2CBf (0),
      itsPlanC2CB  (0)
  {}
  FFTW::~FFTW()
  {}
  void FFTW::plan_r2c(const IPosition&, Float*, Complex*) 
  {}
  void FFTW::plan_r2c(const IPosition&, Double*, DComplex*) 
  {}
  void FFTW::plan_c2r(const IPosition&, Complex*, Float*)
  {}
  void FFTW::plan_c2r(const IPosition&, DComplex*, Double*)
  {}
  void FFTW::plan_c2c_forward(const IPosition&, DComplex*)
  {}
  void FFTW::plan_c2c_forward(const IPosition&, Complex*)
  {}
  void FFTW::plan_c2c_backward(const IPosition&, DComplex*)
  {}
  void FFTW::plan_c2c_backward(const IPosition&, Complex*)
  {}
  void FFTW::r2c(const IPosition&, Float*, Complex*) 
  {}
  void FFTW::r2c(const IPosition&, Double*, DComplex*) 
  {}
  void FFTW::c2r(const IPosition&, Complex*, Float*)
  {}
  void FFTW::c2r(const IPosition&, DComplex*, Double*)
  {}
  void FFTW::c2c(const IPosition&, Complex*, Bool)
  {}
  void FFTW::c2c(const IPosition&, DComplex*, Bool)
  {}

#endif

} //# NAMESPACE CASACORE - END
