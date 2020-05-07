//# Copyright (C) 1993,1994,1995,1997,1999,2000,2001
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

#ifndef SCIMATH_FFTW_H
#define SCIMATH_FFTW_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>

#include <complex>
#include <memory>
#include <mutex>

namespace casacore {

//# Forward Declarations.
class FFTWPlan;
class FFTWPlanf;

// <summary> C++ interface to the FFTWw library </summary>
// <reviewed reviewer="NONE" date="" tests="" demos="">
// </reviewed>
// <synopsis>
// This is a wrapper of FFTW3.
// It is only active if FFTW3 was found during the build.
// If not found, all functions won't do anything at all.
// 
// The interface is such that the presence of FFTW3 is only visible
// in the implementation. The header file does not need to know.
// In this way external code using this class does not need to set HAVE_FFTW.
// </synopsis>

class FFTW
{
public:
  FFTW() ;
  
  ~FFTW() ;

  // overloaded interface to fftw[f]_plan...
  void plan_r2c(const IPosition &size, float *in, std::complex<float> *out) ;
  void plan_r2c(const IPosition &size, double *in, std::complex<double> *out) ;
  void plan_c2r(const IPosition &size, std::complex<float> *in, float *out) ;
  void plan_c2r(const IPosition &size, std::complex<double> *in, double *out) ;
  void plan_c2c_forward(const IPosition &size, std::complex<double> *in) ;
  void plan_c2c_forward(const IPosition &size, std::complex<float> *in) ;
  void plan_c2c_backward(const IPosition &size, std::complex<double> *in) ;
  void plan_c2c_backward(const IPosition &size, std::complex<float> *in) ;
  
  // TODO These overloads do not use their parameters at all. This should
  // be written to use an interface like plan_redft00().
  // overloaded interface to fftw[f]_execute...
  void r2c(const IPosition &size, float *in, std::complex<float> *out) ;
  void r2c(const IPosition &size, double *in, std::complex<double> *out) ;
  void c2r(const IPosition &size, std::complex<float> *in, float *out);
  void c2r(const IPosition &size, std::complex<double> *in, double *out);
  void c2c(const IPosition &size, std::complex<float> *in, bool forward);
  void c2c(const IPosition &size, std::complex<double> *in, bool forward);

  class Plan
  {
    public:
      Plan(FFTWPlan* plan);
      Plan(FFTWPlanf* plan);
      ~Plan() noexcept;
      Plan(const Plan&) = delete;
      Plan(Plan&&);
      Plan& operator=(const Plan&) = delete;
      Plan& operator=(Plan&&);
    
      void Execute(float* in, float* out);
      void Execute(double* in, double* out);
    private:
      friend FFTW;
      std::unique_ptr<FFTWPlan> _plan;
      std::unique_ptr<FFTWPlanf> _planf;
  };
  
  static Plan plan_redft00(const IPosition &size, float *in, float *out);
  static Plan plan_redft00(const IPosition &size, double *in, double *out);
  
private:
  static void initialize_fftw();
  
  std::unique_ptr<FFTWPlanf> itsPlanR2Cf;
  std::unique_ptr<FFTWPlan>  itsPlanR2C;
  
  std::unique_ptr<FFTWPlanf> itsPlanC2Rf;
  std::unique_ptr<FFTWPlan>  itsPlanC2R;
  
  std::unique_ptr<FFTWPlanf> itsPlanC2CFf;   // forward
  std::unique_ptr<FFTWPlan>  itsPlanC2CF;
  
  std::unique_ptr<FFTWPlanf> itsPlanC2CBf;   // backward
  std::unique_ptr<FFTWPlan>  itsPlanC2CB;
  
  std::unique_ptr<FFTWPlanf> itsPlanR2Rf;
  std::unique_ptr<FFTWPlan>  itsPlanR2R;
  
  unsigned flags;

  static bool is_initialized_fftw;  // FFTW needs initialization
                                             // only once per process,
                                             // not once per object
                                             
  // TODO this mutex does not make FFTW thread safe, because
  // planning an FFT with FFTW is not thread safe either.
  // So either the plan..() methods should take the mutex, or
  // FFTW should leave synchronization fully to the user of
  // this class: currently it's halfway in between.
  static std::mutex theirMutex;          // Initialization mutex
};    
    
} //# NAMESPACE CASACORE - END

#endif
