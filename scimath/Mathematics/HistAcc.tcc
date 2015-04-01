//# HistAcc.cc: Statistics Accumulator
//# Copyright (C) 1996,1998,1999,2001,2003
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
//#
//# $Id$

#ifndef SCIMATH_HISTACC_TCC
#define SCIMATH_HISTACC_TCC


#include <casacore/scimath/Mathematics/HistAcc.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays.h>
// #include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Constructor: Fully automatic bin definition

template<class T> 
HistAcc<T>::HistAcc(const uInt n) {
    init();                         // bring into a known state
    initBuffer(n);                  // initialise temporary buffer
} 

// Constructor: Semi-automatic (give bin width only).  

template<class T> 
HistAcc<T>::HistAcc(const uInt n, const T width) {
    init();                         // bring into a known state
    itsUserDefinedBinWidth = width; // user-defined bin width
    initBuffer(n);                 // initialise temporary buffer
} 

// Constructor: Define bins fully:

template<class T> 
HistAcc<T>::HistAcc(const T low, const T high, const T width) {
    init();                        // bring into a known state
    defineBins(low,high,width);
    itsAutoDefineMode = False;
} 

// Constructor: copy an existing histogram (that):

template<class T> 
HistAcc<T>::HistAcc(const HistAcc<T>& that) {
    copy(that);
} 

// Initialise the histogram definition, i.e. bring it into a
// known state:

template<class T>
void HistAcc<T>::init ()
{
    reset();
    itsBinContents.resize(0); 
    itsBinHighLimit.resize(0);
    itsUserDefinedBinWidth = 0;
    initBuffer(0);
}

// Reset, i.e. set the contents of the histogram bins to zero
// (but do NOT change their definition!)

template<class T>
void HistAcc<T>::reset ()
{
    itsStatAcc.reset();
    for (uInt i=0; i<itsBinContents.nelements(); i++) {
	itsBinContents[i] = 0; 
    }
}

// Copy the attributes of the given accumulator:

template<class T>
void HistAcc<T>::copy (const HistAcc<T>& that)
{
    itsStatAcc = that.itsStatAcc;

    itsUserDefinedBinWidth = that.itsUserDefinedBinWidth;
    uInt n = that.itsBinContents.nelements();
    itsBinContents.resize(n); 
    itsBinHighLimit.resize(n);
    uInt i;
    for (i=0; i<n; i++) {
	itsBinContents[i] = that.itsBinContents[i];
	itsBinHighLimit[i] = that.itsBinHighLimit[i];
    }

    n = that.itsBuffer.nelements();
    initBuffer(n);
    itsBufferContents = that.itsBufferContents;
    for (i=0; i<itsBufferContents; i++) {
	itsBuffer[i] = that.itsBuffer[i];
    }
}

// Operator=: this = that

template<class T>
HistAcc<T>& HistAcc<T>::operator= (const HistAcc<T>& that)
{    
    copy(that);
    return *this;
}

//************************************************************************
// Initialise a temporary buffer to store input values from which the
// bin parameters can be determined automatically:

template<class T>
void HistAcc<T>::initBuffer (const uInt bufferLength)
{
    if (bufferLength > 0) {
	itsAutoDefineMode = True; 
	itsBuffer.resize(bufferLength);
    } else {
	itsAutoDefineMode = False; 
	itsBuffer.resize(0);
    }
    itsBufferContents = 0;
}

// Put a value into the temporary buffer. 
// If the buffer is full, define the histogram bins.

template<class T>
void HistAcc<T>::putBuffer (const T v)
{
    if (itsBufferContents < itsBuffer.nelements()) {
	itsBuffer[itsBufferContents] = v;
	itsBufferContents++;
    }

    if (itsBufferContents >= itsBuffer.nelements()) {
	autoDefineBins();     // define bin parameters
    }
}

// Transfer the values from the buffer to the (defined!) bins,
// and clear up the `automatic-mode' machinery.

template<class T>
void HistAcc<T>::clearBuffer ()
{
    itsAutoDefineMode = False;            // BEFORE put1!

    // Transfer values from buffer to histogram
    itsStatAcc.reset();
    for (uInt i=0; i<itsBufferContents; i++) {
	put1(itsBuffer[i]);
    }
    // Clear up the temporary buffer.
    initBuffer(0);
}

// Define the bin parameters `automatically', i.e. using the values in
// itsBuffer:

template<class T>
void HistAcc<T>::autoDefineBins ()
{
    if (itsBufferContents == 0) {         // no values in itsBuffer
	// ............?

    } else {
	// Calculate statistics of the values in the buffer 
	itsStatAcc.reset();            // reset statistics
	itsStatAcc.put(itsBuffer);     // accumulate buffer values

	// Calculate bin range from statistics:
	Double hw = itsStatAcc.getRms();  // w.r.t. mean
	hw *= 3;                          // 3 sigma?
	Double low = itsStatAcc.getMean();// lowest bin
	Double high = low;                // highest bin
	low -= hw;                        // mean - 3 rms
	high += hw;                       // mean + 3 rms

	// Calculate bin width:
	T width = itsUserDefinedBinWidth; // use if defined
	Int k = 0;
	if (width <= 0) {                 // if not defined
	    width = T((high - low)/11);   // default: 11 bins?     
	    if (width <= 0) {             // if still not OK
		width = 1;                // ....?
	    } else {
		// Truncate the width to decimal units (pretty):
		Double q = 10000000;
		Double q1;
		for (uInt i=0; i<15; i++) {
		    q /= 10;
		    q1 = q;
		    if (width < q1) {q1 = q/2;}
		    if (width < q1) {q1 = q/5;}
		    if (width >= q1) {
			k = Int((width+q1/2)/q1);      // truncate
			width = T(k * q1);
			break;                    // escape
		    }
		}
	    }
	}


	// Make bin centres multiples of the bin width (pretty):
	k = Int(high / width);            // nr of multiples  
	high = k * width;                 // adjust highest bin
	k = Int(low / width);             // nr of multiples  
	low = (k-1) * width;              // adjust highest bin
	
	// Go ahead:
	defineBins(T(low),T(high),width); // define the bins
	clearBuffer();                    // transfer values to bins
    }
}

// Define the bin parameters. Low and high represent the centres of the
// lowest and highest bins. The first and last bins contain nr of  
// `spurious' values, i.e. values that are either too small or too large
// for the defined bins.

template<class T> 
void HistAcc<T>::defineBins(const T low, const T high, const T width) {

    T v;
    uInt n = 0;
    for (v=low; v<high+width; v+=width) {
	n++;                              // count the bins
    }

    itsBinContents.resize(n+2);           // n regular + 2 spurious
    itsBinHighLimit.resize(itsBinContents.nelements());

    v = low - width/2;                    // low limit of lowest bin
    for (uInt i=0; i<n+2; i++) {
	itsBinHighLimit[i] = v;           // high limit of bin 
	itsBinContents[i] = 0;            // contents of bin (=0)
	v += width;
    } 
} 

//*************************************************************************
// Accumulate an Array of values (see ArrayMath.cc):

template<class T>
void HistAcc<T>::put(const Array<T>& v) {
    uInt ntotal = v.nelements();
    Bool vDelete;
    const T* vStorage = v.getStorage(vDelete);
    const T* vs = vStorage;
    while (ntotal--) {put1(*vs++);}
    v.freeStorage(vStorage, vDelete); 
}

// Accumulate a Block (simple vector) of values:

template<class T>
void HistAcc<T>::put(const Block<T>& v) {
    for (uInt i=0; i < v.nelements(); i++){
	put1(v[i]);
    }
}

// Accumulate a single value into the histogram:

template<class T>
void HistAcc<T>::put1(const T v)
{
    if (itsAutoDefineMode) {
	putBuffer(v);                // put into temporary buffer

    } else {                         // put into histogram bins
	itsStatAcc.put(v);           // accumulate statistics (all values!)
	for (uInt i=0; i<itsBinContents.nelements()-1; i++) {
	    if (v < itsBinHighLimit[i]) {
		itsBinContents[i]++; 
		return;              // escape
	    }
	}

	// If get to here, put in bin for high spurious values
	itsBinContents[itsBinContents.nelements()-1]++; 
    }
}
    
//**********************************************************************
// Result: get the internal Statistics accumulator, with the aim 
// of getting the histogram statistics. 
// Example: uInt count = h.getStatistics().getCount();

template<class T> 
const StatAcc<T>& HistAcc<T>::getStatistics() 
{
    if (itsAutoDefineMode) {
	itsStatAcc.reset();
	itsStatAcc.put(itsBuffer);
    }
    return itsStatAcc;            
}

// Get the nr of spurious values:

template<class T> 
uInt HistAcc<T>::getSpurious(uInt& nlow, uInt& nhigh)
{
    if (itsAutoDefineMode) {
	autoDefineBins();      
    }
    nlow = itsBinContents[0];                              // tooSmall
    nhigh = itsBinContents[itsBinContents.nelements()-1];  // tooLarge
    return nlow + nhigh;                             // total spurious
}

// Result: get the actual bin-width. Use the difference in high limit
// between the first two bins. 

template<class T> 
Fallible<T> HistAcc<T>::getBinWidth() const
{
    T width = itsBinHighLimit[1] - itsBinHighLimit[0];
    if (width <= 0) {
	return Fallible<T>();
    } else {
	return Fallible<T>(width);
    }
}

// Result: get the Median (= the 50-percentile) 

template<class T> 
Fallible<T> HistAcc<T>::getMedian ()
{
    return getPercentile(50.0);
}

// Result: get the n-percentile, i.e. the value which has n% of the
// input values below it. (the Median is the 50-percentile).

template<class T> 
Fallible<T> HistAcc<T>::getPercentile (const Float p)
{
    if (itsAutoDefineMode) {
	autoDefineBins();
    }
    Double target = itsStatAcc.getWtot() * p/100;  // target value

    uInt n1 = itsBinContents[0];          // spurious low
    if (n1 > target) {
	return Fallible<T>();             // not defined
    }
    // Go through the regular bins, excuding spurious high
    for (uInt i=1; i<itsBinContents.nelements()-1; i++) {
	n1 += itsBinContents[i];
	if (n1 > target) {
	    return getBinValue(i);        // OK
	} 
    }
    return Fallible<T>();             // none of the regular bins
}
 
// Result: get the centre value for the bin with the given index.
// For the bins for low and high `spurious' values, return a value
// that is a full histogram-width away from the extreme bins.

template<class T> 
Fallible<T> HistAcc<T>::getBinValue (const uInt index) const
{
    if (getBinWidth().isValid()) {
	T width = getBinWidth();
	if (index == 0) {
	    return Fallible<T>(itsBinHighLimit[0] - 
			       itsBinContents.nelements() * width);
	    
	} else if (index < itsBinContents.nelements()-1) { 
	    // Regular bins. The strange construct of using -1+0.5 for -0.5 
	    // is to get the correct result for integers.
	    return Fallible<T>(itsBinHighLimit[index] - width + width/2);
	    
	} else if (index == itsBinContents.nelements()-1) {
	    return Fallible<T>(itsBinHighLimit[0] + 
			       itsBinContents.nelements() * width * 2);
	    
	} else {
	    return Fallible<T>();
	}

    } else {
	return Fallible<T>();
    }
}

// Result: get the Histogram itself in two Blocks (simple vectors)
   template<class T> 
   Fallible<uInt> HistAcc<T>::getHistogram (Block<uInt>& binContents, 
    					    Block<T>& binValues) 
{    
    if (itsAutoDefineMode) {
	autoDefineBins();
    }
    uInt n = itsBinContents.nelements();      // nr of bins
    if (n>0) {
	binContents.resize(n-2);
	binValues.resize(n-2);
	for (uInt i=1; i<n-1; i++) {
	    binContents[i-1] = itsBinContents[i];
	    binValues[i-1] = getBinValue(i);
	}
        return Fallible<uInt>(n);
    } else {
	return Fallible<uInt>();               // error
    }
}


//*********************************************************************
// Remove bins with contents smaller than nmin.
// Remove all the `spurious' values, assuming that they are so widely 
// spaced that their average density is less than one/bin.

template<class T> 
void HistAcc<T>::emptyBinsWithLessThan (const uInt nmin) 
{
    if (itsAutoDefineMode) {
	autoDefineBins();
    }

    if (nmin > 1) {
	// Remove the spurious values:
	itsBinContents[0]=0;                          // low
	itsBinContents[itsBinContents.nelements()-1]=0; // high

	// Deal with the regular bins:
	itsStatAcc.reset();                    // reset accumulator
	Float wgt;
	for (uInt i=1; i<itsBinContents.nelements()-1; i++) {
	    if (itsBinContents[i] < nmin) {    // low-contents bin
		itsBinContents[i]=0;            // set to zero
	    } else {                            // above the limit
		// Re-calculate statistics:
		wgt = itsBinContents[i];       // weight
		itsStatAcc.put(getBinValue(i),wgt);
	    }
	}
    }
}


//**********************************************************************
// `Print' histogram to the given o-stream:

template<class T>
void HistAcc<T>::printHistogram (ostream& os, const String& caption)
{ 
    if (itsAutoDefineMode) {autoDefineBins();}

    ios::fmtflags flags = os.flags();         // save current setting
    os << " " << endl;               // skip line
    os << " Histogram: " << caption << endl; 
    uInt pv = 3;                     // precision for bin values
    uInt pc = 3;                     // precision for bin contents

    for (uInt i=1; i<itsBinContents.nelements()-1; i++) {
	setprecision(pv);
	os << "  bin value=" << setw(pv+4) << getBinValue(i);
	setprecision(pc);
	os << " : n=" << setw(pc+1) << itsBinContents[i];
	os << endl;
    }

    os << "  nTotal=" << itsStatAcc.getWtot();
    if (getBinWidth().isValid()) {
	os << "    binWidth=" << getBinWidth();
    } else {
	os << "    binWidth not defined";
    }
    os << endl;

    uInt nlow;
    uInt nhigh;
    uInt nsp = getSpurious(nlow, nhigh);
    os << "  nSpurious=" << nsp;
    os << "   nLow=" << nlow;
    if (itsStatAcc.getMin().isValid()) {
	os << "(vMin=" << itsStatAcc.getMin() << ")";
    } else {
	os << "(vMin= ..)";
    }
    os << "   nHigh=" << nhigh;
    if (itsStatAcc.getMax().isValid()) {
	os << "(vMax=" << itsStatAcc.getMax() << ")";
    } else {
	os << "(vMax= ..)";
    }
    os << endl;

    os << " ";
    if (itsStatAcc.getMean().isValid()) {
	os << " mean=" << itsStatAcc.getMean();
    } else {
	os << " mean= ..";
    }
    if (getMedian().isValid()) {
	os << "   median=" << getMedian();
    } else {
	os << "   median= ..";
    }
    if (itsStatAcc.getRms().isValid()) {
	os  << "  rms=" << itsStatAcc.getRms();
    } else {
	os << "   rms= ..";
    }
    os << endl;

    os.flags(flags);                  // restore original setting
}


//**********************************************************************






} //# NAMESPACE CASACORE - END


#endif
