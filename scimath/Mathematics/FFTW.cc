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


#include <casacore/scimath/Mathematics/FFTW.h>
#include <casacore/casa/OS/HostInfo.h>

#ifdef HAVE_FFTW3
# include <fftw3.h>
#endif
#ifdef _OPENMP
# include <omp.h>
#endif

#include <iostream>


namespace casacore {

bool FFTW::is_initialized_fftw = false;
std::mutex FFTW::theirMutex;


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

    

  FFTW::FFTW() : flags(FFTW_ESTIMATE)
  { 
    initialize_fftw();
  }
  
  void FFTW::initialize_fftw()
  {
    std::lock_guard<std::mutex> lock(theirMutex);
    if (!is_initialized_fftw) {
      int numCPUs = HostInfo::numCPUs();
      int nthreads = 1;
      if (numCPUs > 1) {
        nthreads = numCPUs;
      }
      
#ifdef HAVE_FFTW3_THREADS
      fftwf_init_threads();
      fftw_init_threads();
      fftwf_plan_with_nthreads(nthreads);
      fftw_plan_with_nthreads(nthreads);
#endif
      is_initialized_fftw = true;
    }
  }

  FFTW::~FFTW()
  {
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
  

  void FFTW::plan_r2c(const IPosition &size, float *in, std::complex<float> *out) 
  {
    itsPlanR2Cf.reset( new FFTWPlanf
      (fftwf_plan_dft_r2c(size.nelements(),
                          size.asStdVector().data(),
                          in,
                          reinterpret_cast<fftwf_complex *>(out), 
                          flags)) );
  }

  void FFTW::plan_r2c(const IPosition &size, double *in, std::complex<double> *out) 
  {
    itsPlanR2C.reset( new FFTWPlan
      (fftw_plan_dft_r2c(size.nelements(),
                         size.asStdVector().data(),
                         in,
                         reinterpret_cast<fftw_complex *>(out), 
                         flags)) );
  }

  void FFTW::plan_c2r(const IPosition &size, std::complex<float> *in, float *out) {
    itsPlanC2Rf.reset( new FFTWPlanf
      (fftwf_plan_dft_c2r(size.nelements(),
                          size.asStdVector().data(),
                          reinterpret_cast<fftwf_complex *>(in),
                          out, 
                          flags)) );

  }

  void FFTW::plan_c2r(const IPosition &size, std::complex<double> *in, double *out) {
    itsPlanC2R.reset( new FFTWPlan
      (fftw_plan_dft_c2r(size.nelements(),
                         size.asStdVector().data(),
                         reinterpret_cast<fftw_complex *>(in), 
                         out,
                         flags)) );
  }

  void FFTW::plan_c2c_forward(const IPosition &size, std::complex<double> *in) {
    itsPlanC2CF.reset( new FFTWPlan
      (fftw_plan_dft(size.nelements(),
                     size.asStdVector().data(),
                     reinterpret_cast<fftw_complex *>(in), 
                     reinterpret_cast<fftw_complex *>(in), 
                     FFTW_FORWARD, flags)) );

  }
    
  void FFTW::plan_c2c_forward(const IPosition &size, std::complex<float> *in) {
    itsPlanC2CFf.reset( new FFTWPlanf
      (fftwf_plan_dft(size.nelements(),
                      size.asStdVector().data(),
                      reinterpret_cast<fftwf_complex *>(in), 
                      reinterpret_cast<fftwf_complex *>(in), 
                      FFTW_FORWARD, flags)) );
  }

  void FFTW::plan_c2c_backward(const IPosition &size, std::complex<double> *in) {
    itsPlanC2CB.reset( new FFTWPlan
      (fftw_plan_dft(size.nelements(),
                     size.asStdVector().data(),
                     reinterpret_cast<fftw_complex *>(in), 
                     reinterpret_cast<fftw_complex *>(in), 
                     FFTW_BACKWARD, flags)) );
      
  }
    
  void FFTW::plan_c2c_backward(const IPosition &size, std::complex<float> *in) {
    itsPlanC2CBf.reset( new FFTWPlanf
      (fftwf_plan_dft(size.nelements(),
                      size.asStdVector().data(),
                      reinterpret_cast<fftwf_complex *>(in), 
                      reinterpret_cast<fftwf_complex *>(in), 
                      FFTW_BACKWARD, flags)) );
  }

  // the parameters are used only in order to overload this function
  void FFTW::r2c(const IPosition&, float*, std::complex<float>*) 
  {
    fftwf_execute(itsPlanR2Cf->getPlan());
  }
    
  void FFTW::r2c(const IPosition&, double*, std::complex<double>*) 
  {
    fftw_execute(itsPlanR2C->getPlan());
  }

  void FFTW::c2r(const IPosition&, std::complex<float>*, float*)
  {
    fftwf_execute(itsPlanC2Rf->getPlan());
  }
    
  void FFTW::c2r(const IPosition&, std::complex<double>*, double*)
  {
    fftw_execute(itsPlanC2R->getPlan());
  }
    
  void FFTW::c2c(const IPosition&, std::complex<float>*, bool forward)
  {
    if (forward) {
      fftwf_execute(itsPlanC2CFf->getPlan());
    } else {
      fftwf_execute(itsPlanC2CBf->getPlan());
    }
  }
    
  void FFTW::c2c(const IPosition&, std::complex<double>*, bool forward)
  {
    if (forward) {
      fftw_execute(itsPlanC2CF->getPlan());
    } else {
      fftw_execute(itsPlanC2CB->getPlan());
    }
  }

  FFTW::Plan FFTW::plan_redft00(const IPosition &size, float *in, float *out)
  {
    initialize_fftw();
    
    std::vector<fftwf_r2r_kind> kinds(size.nelements(), FFTW_REDFT00);
    
    return Plan( new FFTWPlanf(
      fftwf_plan_r2r(size.nelements(), size.asStdVector().data(),
                     in, out, kinds.data(), FFTW_ESTIMATE)) );
  }
  
  FFTW::Plan FFTW::plan_redft00(const IPosition &size, double *in, double *out)
  {
    initialize_fftw();
    
    std::vector<fftw_r2r_kind> kinds(size.nelements(), FFTW_REDFT00);
    
    return Plan( new FFTWPlan(
      fftw_plan_r2r(size.nelements(), size.asStdVector().data(),
                    in, out, kinds.data(), FFTW_ESTIMATE)) );
  }
  
  void FFTW::Plan::Execute(float *in, float *out)
  {
    fftwf_execute_r2r(_planf->getPlan(), in, out);
  }
  
  void FFTW::Plan::Execute(double *in, double *out)
  {
    fftw_execute_r2r(_plan->getPlan(), in, out);
  }
  
#else

  class FFTWPlan { };
  class FFTWPlanf { };
  
  FFTW::FFTW()
  {}
  FFTW::~FFTW()
  {}
  void FFTW::plan_r2c(const IPosition&, float*, std::complex<float>*) 
  {}
  void FFTW::plan_r2c(const IPosition&, double*, std::complex<double>*) 
  {}
  void FFTW::plan_c2r(const IPosition&, std::complex<float>*, float*)
  {}
  void FFTW::plan_c2r(const IPosition&, std::complex<double>*, double*)
  {}
  void FFTW::plan_c2c_forward(const IPosition&, std::complex<double>*)
  {}
  void FFTW::plan_c2c_forward(const IPosition&, std::complex<float>*)
  {}
  void FFTW::plan_c2c_backward(const IPosition&, std::complex<double>*)
  {}
  void FFTW::plan_c2c_backward(const IPosition&, std::complex<float>*)
  {}
  void FFTW::r2c(const IPosition&, float*, std::complex<float>*) 
  {}
  void FFTW::r2c(const IPosition&, double*, std::complex<double>*) 
  {}
  void FFTW::c2r(const IPosition&, std::complex<float>*, float*)
  {}
  void FFTW::c2r(const IPosition&, std::complex<double>*, double*)
  {}
  void FFTW::c2c(const IPosition&, std::complex<float>*, bool)
  {}
  void FFTW::c2c(const IPosition&, std::complex<double>*, bool)
  {}

  FFTW::Plan FFTW::plan_redft00(const IPosition &, float *, float *)
  { throw std::runtime_error("FFTW not available"); }
  
  FFTW::Plan FFTW::plan_redft00(const IPosition &, double *, double *)
  { throw std::runtime_error("FFTW not available"); }
  
  void FFTW::Plan::Execute(float *, float *)
  { throw std::runtime_error("FFTW not available"); }
  
  void FFTW::Plan::Execute(double *, double *)
  { throw std::runtime_error("FFTW not available"); }
  
#endif

FFTW::Plan::Plan(Plan&&) = default;

FFTW::Plan::Plan(FFTWPlan* plan)
  : _plan(plan)
{ }

FFTW::Plan::Plan(FFTWPlanf* plan)
  : _planf(plan)
{ }

FFTW::Plan::~Plan() noexcept { }

FFTW::Plan& FFTW::Plan::operator=(Plan&&) = default;

} //# NAMESPACE CASACORE - END
