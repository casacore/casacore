//# HistAcc.h: Histogram Accumulator
//# Copyright (C) 1996,1999,2000,2001
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

#ifndef SCIMATH_HISTACC_H
#define SCIMATH_HISTACC_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/scimath/Mathematics/StatAcc.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// forward declarations:
template <class T> class Array;
class String;

// <reviewed reviewer="" date="" tests="tHistAcc" demos="">

// <prerequisite>
//   <li> module Arrays
//   <li> <linkto module="Arrays:description">Arrays </linkto> module
// </prerequisite>
//
// <summary> 
// Makes a histogram from input values.
// </summary>
//
// <etymology>
// HistAcc stands for `Histogram Accumulator'. 
// </etymology>
//
// <templating arg=T>
// <li> The accepted input types are real, i.e. Int, uInt, Float, Double, 
// but not Complex. 
// </templating>

// <synopsis>
// Makes a histogram from input values. The histogram bin parameters
// may be defined, or determined from the first n input values.
// The input values are fed to HistAcc via the member function `put'. 
// They can be fed individually, or in the form of an Array. 
//
// The histogram `bins' can be defined via the constructor in the
// form of loop variables: low bin, high bin, bin-width.
// It is also possible to let the bin parameters be determined 
// automatically from the first n (e.g. n=50) input values.
// If the actual nr of input values is less than n when the histogram
// is interrogated in some way, the bin parameters will be determined 
// from what is available.
// </synopsis> 
//
// <example>
// It is usually convenient to let the bins be defined automatically: 
// <srcblock>
//   Matrix<T> vv(30,100);        // an array of input values
//   vv = ...                     // fill the array
//   HistAcc<T> h(25);            // use the first 25 values to define bins 
//   h.put(vv);                   // accumulate values into histogram 
//   h.printHistogram(cout,"vv"); // print the histogram of vv
//   Fallible<Double> median = h1.getMedian();  // return the median
// </srcblock>
//  
// In some cases the bin parameters are pre-defined:
// <srcblock>
//   Vector<T> vv(100,0);        // a vector (array) of values
//   vv = ...                    // fill the vector
//   HistAcc<T> h(-10,20,3);     // bins with width 3, between -10 and 20
//   h.put(vv);                  // accumulate values into histogram   
//   uInt n = h.getSpurious(l,h);// get the number outside the bins
// </srcblock>
//
// The internal statistics accumulator can be interrogated explicitly
// or implicitly:
// <srcblock>
//   StatAcc<T> s = h.getStatistics();     // return the internal StatAcc
//   Fallible<Double> mean = s.getMean();  // get the mean of the input values
//   Fallible<Double> mean = h.getStatistics().getMean();  // alternative
// </srcblock>

// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="">
// </todo>


// *************************************************************************** 

template<class T> class HistAcc  {
public:
    // Constructors and destructor. If the bin-parameters low, high 
    // and width (for lowest and highest bin, and binwidth) are not
    // specified, they will be determined automatically from the
    // first nBuff input values (which are stored in a temporary buffer).
    // <group>
    HistAcc(const uInt nBuff);                 //# fully automatic  
    HistAcc(const uInt nBuff, const T width);  //# semi-automatic    
    HistAcc(const T low, const T high, const T width);  //# fully specified
    HistAcc(const HistAcc&);                   //# copy an existing one
    ~HistAcc(){;} 
    // </group>

    // Copy operations.
    // <group>
    void copy(const HistAcc&);           //# idem
    HistAcc& operator= (const HistAcc&); 
    // </group>

    // Accumulate (put) value(s) into the histogram.
    // <group>
    inline void put(const T v);           //# single value           
    void put(const Array<T>& vv);         //# array 
    void put(const Block<T>& vv);         //# block (simple array)
    // </group>

    // Reset the contents of the bins to zero, but retain the current 
    // bin definition. 
    void reset();                    

    // Empty all bins whose contents is < nmin (e.g. nmin=2). 
    // This is useful to remove `noise' values from the histogram.
    void emptyBinsWithLessThan(const uInt nmin);

    // The median is the 50-percentile (getPercentile(50)), i.e. the 
    // value which has 50 percent of the input values below it.
    // Calculation takes into account the spurious
    // input values, i.e. values that fell outside the bins.
    Fallible<T> getPercentile(const Float p); 
    Fallible<T> getMedian();                 

    // All bins have the same width.
    Fallible<T> getBinWidth() const;           

    // Get the internal Statistics accumulator (see StatAcc,h).
    // It can be used to obtain statistics of the input values.
    const StatAcc<T>& getStatistics();    

    // The return value is the nr of histogram bins, and is invalid
    // if the number is zero. The given blocks/vectors are resized,
    // and contain the contents and centre values of the bins. 
    Fallible<uInt> getHistogram(Block<uInt>& bins, Block<T>& values);

    // Get the nr of `spurious' values, i.e. the ones that fell
    // outside the defined bins. 
    uInt getSpurious(uInt& tooSmall, uInt& tooLarge);        

    // Print histogram.
    // <group>
    void printHistogram(ostream&, const String& caption); 
    // </group>
	
private:
    Block<uInt> itsBinContents;   //# Contents of histogram bins
    Block<T> itsBinHighLimit;     //# High limit of each bin
    T itsUserDefinedBinWidth;     //# if defined

    StatAcc<T> itsStatAcc;        //# private Statistics Accumulator

    Bool itsAutoDefineMode;       //# If true: automatic mode
    Block<T> itsBuffer;           //# temporary storage of input T-values
    uInt itsBufferContents;       //# nr of T-values in buffer 

    // Accumulate a single value into the histogram.
    void put1(const T);

    // Definition of histogram bins with given parameters.
    void defineBins(const T low, const T high, const T width);

    // Internal helper functions for the automatic definition of
    // histogram parameters, using the contents of itsBuffer.  
    // <group> 
    void initBuffer(const uInt size); 
    void putBuffer(const T v);     //# add input value to itsBuffer 
    void clearBuffer();            //# transfer from buffer to bins
    void autoDefineBins(); 
    // </group>

    // Other internal helper function(s).
    // <group>
    void init();   
    Fallible<T> getBinValue(const uInt index) const;  //# bin centre value
    // </group>

};



//*************************** inline functions, have to be in HistAcc.h ****


// Accumulate a single value:

template<class T> 
inline void HistAcc<T>::put(const T v) {
    put1(v);            
}


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/HistAcc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif












