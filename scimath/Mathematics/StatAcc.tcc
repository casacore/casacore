//# StatAcc.cc: Statistics Accumulator
//# Copyright (C) 1996,1999,2001,2002
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

#ifndef SCIMATH_STATACC_TCC
#define SCIMATH_STATACC_TCC


#include <casacore/scimath/Mathematics/StatAcc.h>
#include <casacore/casa/Arrays.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Constructors:

template<class T> 
StatAcc<T>::StatAcc() {reset();} 

template<class T> 
StatAcc<T>::StatAcc(const StatAcc<T>& that) {
    copy(that);
} 


// Reset (initialise) the accumulator

template<class T>
void StatAcc<T>::reset ()
{
    itsWtot = 0;
    itsWsum = 0;
    itsWssum = 0;
    itsMax = 0;
    itsMin = 0;
    itsCount = 0;
}

// Copy the attributes of the given accumulator:

template<class T>
void StatAcc<T>::copy (const StatAcc<T>& that)
{
    itsWtot = that.itsWtot;
    itsWsum = that.itsWsum;
    itsWssum = that.itsWssum;
    itsMax = that.itsMax;
    itsMin = that.itsMin;
    itsCount = that.itsCount;
}

// Accumulate an Array of values (see ArrayMath.cc):

template<class T>
void StatAcc<T>::put(const Array<T>& v) {
    uint32_t ntotal = v.nelements();
    bool vDelete;
    const T* vStorage = v.getStorage(vDelete);
    const T* vs = vStorage;
    while (ntotal--) {put1(*vs++,1);}
    v.freeStorage(vStorage, vDelete); 
}

// Accumulate Array values with individual weights:

template<class T>
void StatAcc<T>::put(const Array<T>& v, const Array<float>& w) {
    uint32_t ntotal = v.nelements();
    if (ntotal != w.nelements()) {
	throw(AipsError("StatAcc<T>::put(Array& v, Array& w): v and w have different length"));
    }
    bool vDelete,wDelete;
    const T* vStorage = v.getStorage(vDelete);
    const T* vs = vStorage;
    const float* wStorage = w.getStorage(wDelete);
    const float* ws = wStorage;
    while (ntotal--) {put1(*vs++,*ws++);}
    v.freeStorage(vStorage, vDelete); 
    w.freeStorage(wStorage, wDelete); 
}


// Accumulate a Block (simple vector) of values:

template<class T>
void StatAcc<T>::put(const Block<T>& v) {
    for (uint32_t i=0; i<v.nelements(); i++) { 
	put1(v[i],1);
    }
}

// Accumulate a Block of values with indivudual weights:

template<class T>
void StatAcc<T>::put(const Block<T>& v, const Block<float>& w) {
    uint32_t ntotal = v.nelements();
    if (ntotal != w.nelements()) {
	throw(AipsError("StatAcc<T>::put(Block& v, Block& w): v and w have different length"));
    }
    for (uint32_t i=0; i<v.nelements(); i++) { 
	put1(v[i],w[i]);
    }
}

// Private helper routine: accumulate a single weighted value:

template<class T>
void StatAcc<T>::put1(const T v, const float w)
{
    if (w != 0) {
	if (itsWtot == 0) {                    // first time 
	    itsMin = v;                        // minimum value
	    itsMax = v;                        // maximum value
	} else {
	    if (v < itsMin) {itsMin = v;}      // minimum value
	    if (v > itsMax) {itsMax = v;}      // maximum value
	}
	
	if (w == 1) {                         
	    itsWtot++;                         // total weight (=count)
	    itsWsum += v;                      // sum
	    itsWssum += v*v;                   // sum of squares
	} else {
	    itsWtot += w;                      // total weight
	    itsWsum += w*v;                    // weighted sum
	    itsWssum += w*v*v;                 // weighetd sum of squares
	}
        itsCount++;
    }
}

// Get statistics results:

template<class T> 
double StatAcc<T>::getWtot() const              // get total weight
{
    return itsWtot;
}

template<class T>
uint32_t StatAcc<T>::getCount() const               // get number of samples
{     
    return itsCount;
}


template<class T>
Fallible<double> StatAcc<T>::getMax() const         // get minimum value  
{     
    if (itsWtot == 0) {
	return Fallible<double>();      
    }
    return Fallible<double>(itsMax);
}

template<class T> 
Fallible<double> StatAcc<T>::getMin() const       // get minimum value 
{
    if (itsWtot == 0) {
	return Fallible<double>();      
    }
    return Fallible<double>(itsMin);
}

template<class T> 
Fallible<double> StatAcc<T>::getMean() const     // get mean value 
{
    if (itsWtot == 0) {
	return Fallible<double>();
    }      
    return Fallible<double>(itsWsum/itsWtot);
}

template<class T> 
Fallible<double> StatAcc<T>::getRmsAbs() const  // get rmsAbs value 
{
    if (itsWtot == 0) {
	return Fallible<double>();
    }      
    return Fallible<double>(sqrt(itsWssum/itsWtot));
}

template<class T> 
Fallible<double> StatAcc<T>::getRms() const     // get rms w.r.t. the mean 
{
    if (getVariance().isValid()) {
	double ms = getVariance();
	if (ms >= 0) {
	    return Fallible<double>(sqrt(ms));  // valid
	} else {
	    return Fallible<double>(0);         // .....?
	}
    } else {
	return Fallible<double>();              // 
    }
}

template<class T> 
Fallible<double> StatAcc<T>::getVariance() const     // get variance
{ 
    if (getMean().isValid()) {
	double mean = getMean();
	return Fallible<double>(itsWssum/itsWtot - mean*mean); 
    } else {
	return Fallible<double>(); 
    }
}


// Operator=: this = that

template<class T>
StatAcc<T>& StatAcc<T>::operator= (const StatAcc<T>& that)
{    
    copy(that);
    return *this;
}

// Accumulators can be added. The result is the same as if the values
// accumulated in `that' were accumulated in `this'. 
// Operator+=: this = this + that

template<class T>
StatAcc<T>& StatAcc<T>::operator+= (const StatAcc<T>& that)
{    
    if (that.itsWtot != 0) {
	if (itsWtot != 0) {
	    if (that.itsMax > itsMax) {itsMax = that.itsMax;}
	    if (that.itsMin < itsMin) {itsMin = that.itsMin;}
	} else { 
	    itsMin = that.itsMin;
	    itsMax = that.itsMax;
	}
	itsWtot += that.itsWtot;
        itsCount += that.itsCount;
	itsWsum += that.itsWsum;
	itsWssum += that.itsWssum;
    }
    return *this;
}

// Operator+: new = this + that

template<class T>
StatAcc<T> StatAcc<T>::operator+ (const StatAcc<T>& that)
{    
    StatAcc<T> s(*this);   // construct a copy of this accumulator
    return s += that;      // add the given accumulator to the copy
}


// Print one-line summary of the accumulated statistics.

template<class T>
void StatAcc<T>::printSummaryLine (ostream& os, const String& caption) const
{ 
	ios::fmtflags flags = os.flags();          // save current setting
    uint32_t p = 4;                       // precision
    os.setf(ios::right,ios::adjustfield);

    if (itsWtot != 0) {
	os << setprecision(p) << setw(p+3) << getWtot(); 
	os << setprecision(p) << setw(p+3) << getCount(); 
	os << setprecision(p) << setw(p+3) << getMean(); 
	os << setprecision(p) << setw(p+3) << getRms(); 
	os << setprecision(p) << setw(p+3) << getMin(); 
	os << setprecision(p) << setw(p+3) << getMax(); 
    } else {
	os << setprecision(p) << setw(p+3) << getWtot(); 
	os << setw(5*(p+3)) << "  no values accumulated " ; 
    }
    os << "  : " << caption << endl;  // caption 
    os.flags(flags);                  // restore original setting
}

// Print header for SummaryLine:

template<class T>
void StatAcc<T>::printSummaryLineHeader (ostream& os, const String& caption) const
{ 
	ios::fmtflags flags = os.flags();          // save current setting
    uint32_t p = 4;                       // precision

    // print one-line header
    os.setf(ios::right,ios::adjustfield);
//    os << " " << endl;              // skip line (?)
    os << setw(p+3) << "wtot"; 
    os << setw(p+3) << "npts"; 
    os << setw(p+3) << "mean"; 
    os << setw(p+3) << "rms"; 
    os << setw(p+3) << "min"; 
    os << setw(p+3) << "max"; 
    os << "  : " << caption << endl; 

    os.flags(flags);                  // restore original setting
}


// Print multi-line summary (list) of the accumulated statistics.

template<class T>
void StatAcc<T>::printSummaryList (ostream& os, const String& caption) const
{ 
	ios::fmtflags flags = os.flags();         // save current setting

    os << " " << endl;               // skip line
    os << " StatAcc summary for: " << caption << endl; 
    uint32_t p = 12;                     // precision 
    os << setprecision(p); 

    os << " Wtot=    " << setw(p+3) << getWtot() << endl; 
    os << " Npts=    " << setw(p+3) << getCount() << endl;
    os << " Mean=    " << setw(p+3) << getMean() << endl; 
    os << " Min=     " << setw(p+3) << getMin() << endl; 
    os << " Max=     " << setw(p+3) << getMax() << endl; 
    os << " Rms=     " << setw(p+3) << getRms() << endl; 
    os << " Variance=" << setw(p+3) << getVariance() << endl; 
    os << " RmsAbs=  " << setw(p+3) << getRmsAbs() << endl; 

    os.flags(flags);                  // restore original setting
}


//**********************************************************************






} //# NAMESPACE CASACORE - END


#endif
