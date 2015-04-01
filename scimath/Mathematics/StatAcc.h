//# StatAcc.h: Statistics Accumulator
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

#ifndef SCIMATH_STATACC_H
#define SCIMATH_STATACC_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// forward declarations:
template <class T> class Array;
template <class T> class Block;
class String;

// <reviewed reviewer="" date="" tests="tStatAcc" demos="">

// <prerequisite>
//   <li> module Arrays
//   <li> <linkto module="Arrays:description">Arrays </linkto> module
// </prerequisite>
//
// <summary> 
// A statistics accumulator 
// </summary>
//
// <etymology>
// StatAcc stands for `Statistics Accumulator'. 
// </etymology>
//
// <templating arg=T>
// <li> A statistics accumulator accepts (weighted) input values and 
// calculates simple statistice (min, max, weighted mean, rms etc).
// The accepted input types are real, i.e. Int, uInt, Float, Double, 
// but not Complex. The reason for this is that the < operator of
// Complex (needed for min/max) works on the norm in any case, and
// the sqrt function (needed for rms) yields an ambiguous result.
//   
// Restriction to real types also allows the internal arithmetic type 
// to be Double rather than the input type. The latter would give
// all kinds of complications with weighting, accuracy and overflow
// if the input type would be Int or uInt.
// </templating>

// <synopsis>
// The (weighted) values are fed to StatAcc via the member function
// `put'. They can be fed individually, or in the form of an
// Array. The weights are optional (default = 1) and always have 
// type Float.
//
// Asking for a result does not change the internal state. The 
// type of the returned results is always Fallible<Double>.
// A result is invalid if no input values with non-zero weight
// have been accumulated yet.
//
// The accumulator
// can be re-initialised with the function `reset'. Accumulators
// can be added to each other, which is as if their combined values had been
// accumulated in the same accumulator.
// 
// Some functions have been provided to display a summary of the
// statistics results. One may choose between a one-line format
// (with an optional associated header line), and a list.
// </synopsis> 
//
// <example>
// <srcblock>
//   StatAcc<T> s;               // T is Float, Double, Int etc
//   Matrix<T> vv(2,5);          // a matrix (array) of input values
//   Matrix<Float> wgt(2,5);     // an associated matrix of weights
//   .... fill vv and wgt with values and individual weights ... 
//   s.put(vv,wgt);              // accumulate the weighted values   
//   Fallible<Double> min = s.getMin();    // return the minimum value
//
//   s.reset();                  // re-initialise
//   s.put(vv);                  // if wgt omitted, default = 1.0
//   if (s.getRms().isValid() {  // check validity of rms
//         ... use it ...
//   }
// </srcblock>
// </example>
//
// <motivation>
// One often needs simple statistics of a series of values, which 
// may occur by themselves or in arrays at various points in a program.  
// Sincs it is a pain to have to assign accumulation variables, and to
// write statistics evaluation code (including exceptions),
// this helper class is provided.
// </motivation>
//
// <todo asof="">
// </todo>


// *************************************************************************** 

template<class T> class StatAcc  {
public:
    // constructors and destructor.
    // <group>
    StatAcc();  
    StatAcc(const StatAcc&);  
    ~StatAcc(){;} 
    // </group>

    // Reset or copy the accumulator attributes.
    // <group>
    void reset();
    void copy(const StatAcc&);
    // </group>

    // Operators for adding and copying accumulators.
    // <group>
    StatAcc& operator= (const StatAcc&);
    StatAcc operator+ (const StatAcc&);
    StatAcc& operator+= (const StatAcc&);        
    // </group>

    // Accumulate input value(s) v with weight w.
    // If weight is omitted, the default=1.
    // <group>
    inline void put(const T v);           
    inline void put(const T v, const Float w);
    void put(const Array<T>& v);      
    void put(const Array<T>& v, const Array<Float>& w);
    void put(const Block<T>& v);      
    void put(const Block<T>& v, const Block<Float>& w);
    // </group>

    // Get statistics results one at a time.
    // Count is the nr of values accumulated.
    // Wtot is the sum of the weights. 
    // Rms is defined w.r.t. the mean, and is the square of Variance. 
    // RmsAbs is the root-mean-square of the absolute input values.
    // <group>
    Double getWtot() const;           
    uInt             getCount() const;
    Fallible<Double> getMin() const;        
    Fallible<Double> getMax() const;      
    Fallible<Double> getMean() const;        
    Fallible<Double> getRms() const;      
    Fallible<Double> getVariance() const;      
    Fallible<Double> getRmsAbs() const; 
    // </group>

    // Print summary of accumulated statistics.
    // Line is a one-line summary, including the (short) caption.
    // LineHeader gives a one-line explanation of the numbers.
    // List uses a separate line for each result (mean, max etc).
    // <group>
    void printSummaryList(std::ostream&, const String& caption) const; 
    void printSummaryLine(std::ostream&, const String& caption) const;  
    void printSummaryLineHeader(std::ostream&, const String& caption) const; 
    // </group>
	
private:
    Double itsWtot;               //# Sum of weights
    Double itsWsum;               //# Sum of weighted values
    Double itsWssum;              //# Sum of weighted squares
    Double itsMin;                //# Minimum value
    Double itsMax;                //# Maximum value       
    uInt   itsCount;              //# Number of samples

    // Accumulate a single weighted value.
    void put1(const T, const Float);

};



//*************************** inline functions, have to be in StatAcc.h ****


// Accumulate a single value:

template<class T> 
inline void StatAcc<T>::put(const T v) {
    put1(v, 1);                                        // default weight = 1
}

template<class T> 
inline void StatAcc<T>::put(const T v, const Float w) {
    put1(v, w);
}    



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/StatAcc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif












