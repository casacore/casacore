//# SimButterworthBandpass.h: Declares a Butterworth function
//# Copyright (C) 2000,2001,2002,2003
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

#ifndef SCIMATH_SIMBUTTERWORTHBANDPASS_H
#define SCIMATH_SIMBUTTERWORTHBANDPASS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/scimath/Functionals/Function1D.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;

// <summary>
// a class for evaluating a Butterworth filter transfer function.
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="2001/11/14"
// tests="tSimButterworthBandpass" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class=Function1D>Function1D</linkto>
// </prerequisite>
//
// <etymology>
// "Butterworth" refers to the Butterworth function for describing
// filter transfer functions (Butterworth, S, "On the theory of filter 
// amplifiers," Wireless Engineer, vol. 7 pp. 536-541, October 1930).
// "Bandpass" reflects that the transfer function is has both low and high
// frequency cutoffs.  
// "Sim" indicates that this implementation is not necessarily appropriate 
// characterizing real bandpass filters; in the future, there may be a 
// more general class called simply "Butterworth".  
// </etymology>
//
// <synopsis>
// This function class simulates the (amplitude) transfer function for a 
// wideband bandpass filter constructed from the combination of a low-pass 
// and a high-pass Butterworth filter.  
//
// In analog electronic filter design, a Butterworth low-pass filter is 
// one in which the amplitude transfer function, |H(jw)| (where j = sqrt(-1) 
// and w is the angular frequency), is given by:
// <srcblock>
//   |H(jw)| = 1 / sqrt(1 + (w/w_c)^(2*n)) 
// </srcblock>
// where n refers to the filter "order" and w_c is the "cutoff frequency".  
// When w = w_c, the filter output is 1/sqrt(2) that of the input, and the 
// higher the order, the steeper the drop off towards zero and the closer 
// the approximation to a idealized step function.  
//
// Filter theory provides transformations for deriving transfer functions 
// of high-pass and band-pass filters which reflect how the electrical 
// circuits actually work.  However, to simplify this class's implementation 
// and to make the transfer function behavior more predictable by the naive 
// user, THIS CLASS DOES NOT ACTUALLY USE THE PROPER TRANSFORMATIONS (see 
// Etymology section above).  
// Instead, the Butterworth bandpass transfer function is approximated by 
// low pass component, given above, combined with a pseudo high-pass function
// that is of the same form but with w substituted with -w.  Both components 
// are shifted such that its peak transfer point is at a given "center" 
// position.  The cutoff value and order can be set independently for both
// ends of the passband.  
// </synopsis>
//
// <example>
// <srcblock>
//   // Create a bandpass function centered on x=0.8 and cutoffs at 0 and 2.5.
//   // The orders of the drop-offs will 4 at the low end and 5 at the high
//   // end.  The peak will by 1.0 by default.
//   SimButterworthBandpass<Double> butt(4, 5, 0, 2.5, 0.8);
//   
//   Double z = butt(1);    // z = 1.0
//   z = butt(0);           // z = 1/sqrt(2)
//   z = butt(2.5);         // z = 1/sqrt(2)
//   z = butt(-25);         // z ~ 9.24e-9 ~ 0
//
//   // change the low-end cutoff to -25.0
//   butt.setMinCutoff(-25);
//   z = butt(-25);         // z = 1/sqrt(2)
// </srcblock>
// </example>
//
// <motivation>
// This class was created to simulate systemtic Butterworth bandpasses 
// within the simulator tool.  It can used by the SimBJones class to vary the 
// bandpass in a predictable way.  However, it has limited value for real 
// filter analysis, and it is not expected to be a realistic representation
// of real bandpass filters in use with radio telescopes backends.  
// </motivation>
//
// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>
//
// <thrown>
//    <li> Assertion if indices out-of-range
// </thrown>
//
// <todo asof="2001/11/14">
//  <li> Nothing I know of
// </todo>

template<class T>
class SimButterworthBandpass : public Function1D<T>
{
public:
    //# Enumerations
    // Enumeration of the function parameters
    enum { CENTER, MINCUTOFF, MAXCUTOFF, PEAK };

  //# Constructors
  // create a zero-th order (all-pass) Butterworth bandpass function.
    SimButterworthBandpass();
  
    // create a Butterworth bandpass function.
    SimButterworthBandpass(const uInt minord, const uInt maxord, 
			   const T &mincut=T(-1), const T &maxcut=T(1), 
			   const T &center=T(0), const T &peak=T(1));
  
    // create a fully specified Butterworth bandpass in which the 
    // low and high pass orders are stored in a Record
    explicit SimButterworthBandpass(const RecordInterface& gr,
				    T mincut=T(-1), T maxcut=T(1), 
				    T center=T(0), T peak=T(1));

    // create a copy of another Butterworth bandpass function
    SimButterworthBandpass(const SimButterworthBandpass &other);
  
    // copy(deep) another Butterworth function
    SimButterworthBandpass<T> &
    operator=(const SimButterworthBandpass<T> &other);
  
  // Destructor
    virtual ~SimButterworthBandpass();


    //# Operators    
    // Evaluate the bandpass at "x".
    virtual T eval(const typename FunctionTraits<T>::ArgType *x) const;

  //# Member functions
  // set the center of the bandpass.  This is the x-ordinate value that 
  // evaluates to the peak of the function.  
    void setCenter(const T &x) { param_p[CENTER] = x; } 

    // return the center of the bandpass.  This is the x-ordinate value that 
    // evaluates to the peak of the function.  
    const T &getCenter() const { return param_p[CENTER]; }

    // set the characteristic minimum (high-pass) cutoff value.  At this 
    // x-ordinate value, the function has a value reduced 30 dB from its 
    // peak.  
    void setMinCutoff(const T &x) { param_p[MINCUTOFF] = x; } 

    // set the characteristic maximum (low-pass) cutoff value.  At this 
    // x-ordinate value, the function has a value reduced 30 dB from its 
    // peak.  
    void setMaxCutoff(const T &x) { param_p[MAXCUTOFF] = x; } 

    // set the order of the Butterworth function for the minimum (high-pass)
    // portion of the bandpass
    void setMinOrder(uInt order) { nl_p = order; }

    // set the order of the Butterworth function for the maximum (low-pass)
    // portion of the bandpass
    void setMaxOrder(uInt order) { nh_p = order; }

    // return the characteristic minimum (high-pass) cutoff value.  At this 
    // x-ordinate value, the function has a value reduced 30 dB from its 
    // peak.  
    const T &getMinCutoff() const { return param_p[MINCUTOFF]; }

    // return the characteristic maximum (low-pass) cutoff value.  At this 
    // x-ordinate value, the function has a value reduced 30 dB from its 
    // peak.  
    const T &getMaxCutoff() const { return param_p[MAXCUTOFF]; }

    // return the order of the Butterworth function for the minimum (high-pass)
    // portion of the bandpass
    uInt getMinOrder() const { return nl_p; }

    // return the order of the Butterworth function for the maximum (low-pass)
    // portion of the bandpass
    uInt getMaxOrder() const { return nh_p; }

    // set the scale of the function by setting its peak value.  By default,
    // the peak value is T(1);
    void setPeak(T val) { param_p[PEAK] = val; } 

    // return the scale of the function
    const T &getPeak() const { return param_p[PEAK]; }
  
    // get/set the function mode.  This is an alternate way to get/set the 
    // non-coefficient data for this function.  The supported record fields 
    // are as follows:
    // <pre>
    // Field Name     Type            Role
    // -------------------------------------------------------------------
    // minOrder   TpInt   the order of the Butterworth function for the 
    //                    minimum (high-pass) portion of the bandpass
    // maxOrder   TpInt   the order of the Butterworth function for the 
    //                    maximum (low-pass) portion of the bandpass
    // An exception is thrown if either value is less than zero
    // </pre>
    // <group>
    virtual void setMode(const RecordInterface& mode);
    virtual void getMode(RecordInterface& mode) const;
    // </group>

    // return True if the implementing function supports a mode.  This
    // implementation always returns True.
    virtual Bool hasMode() const;

    // clone this function
    virtual Function<T> *clone() const {
	return new SimButterworthBandpass<T>(*this); 
    }

private:
    //# Non-parameter Data
    // Minimum order
    uInt nl_p;
    // Maximum order
    uInt nh_p;

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/SimButterworthBandpass.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
