//# SparseDiff.h: An automatic differentiating class for functions
//# Copyright (C) 2007,2008
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
//# $Id: SparseDiff.h,v 1.3 2008/01/10 12:00:42 wbrouw Exp $

#ifndef SCIMATH_SPARSEDIFF_H
#define SCIMATH_SPARSEDIFF_H

//# Includes
#include <casa/aips.h>
#include <scimath/Mathematics/AutoDiff.h>
#include <scimath/Mathematics/SparseDiffRep.h>
#include <casa/vector.h>
#include <utility>

// Using
using std::pair;

namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declarations
  template <class T> class SparseDiff;

  // <summary>
  // Class that computes partial derivatives by automatic differentiation.
  // </summary>
  //
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="" tests="tSparseDiff.cc" demos="dSparseDiff.cc">
  // </reviewed>
  //
  // <prerequisite>
  // <li>  <linkto class=AutoDiff>AutoDiff</linkto> class
  // </prerequisite>
  //
  // <etymology>
  // Class that computes partial derivatives for some parameters by automatic
  // differentiation, thus SparseDiff.
  // </etymology>
  //
  // <synopsis>
  // Class that computes partial derivatives by automatic differentiation.
  // It does this by storing the value of a function and the values of its first 
  // derivatives with respect to some of its independent parameters. 
  // When a mathematical
  // operation is applied to a SparseDiff object, the derivative values of the 
  // resulting new object are computed according to the chain rules 
  // of differentiation. SparseDiff operates like the 
  // <linkto class=AutoDiff>AutoDiff</linkto> class, but only determines the
  // derivatives with respect to the actual dependent variables. 
  //
  // Suppose we have a function f(x0,x1,...,xn) and its differential is
  // <srcblock> 
  // df = (df/dx0)*dx0 + (df/dx1)*dx1 + ... + (df/dxn)*dxn
  // </srcblock> 
  // We can build a class that has the value of the function, 
  // f(x0,x1,...,xn), and the values of the derivatives, (df/dx0), (df/dx1), 
  // ..., (df/dxn) at (x0,x1,...,xn), as class members. 
  //
  // Now if we have another function, g(x0,x1,...,xn) and its differential is
  // dg = (dg/dx0)*dx0 + (dg/dx1)*dx1 + ... + (dg/dxn)*dxn,
  // since 
  // <srcblock> 
  // d(f+g) = df + dg, 
  // d(f*g) = g*df + f*dg, 
  // d(f/g) = df/g - fdg/g^2,
  // dsin(f) = cos(f)df, 
  // ...,
  // </srcblock> 
  // we can calculate
  // <srcblock>  
  // d(f+g), d(f*g), ..., 
  // </srcblock>  based on our information on
  // <srcblock>  
  // df/dx0, df/dx1, ..., dg/dx0, dg/dx1, ..., dg/dxn. 
  // </srcblock>  
  // All we need to do is to define the operators and derivatives of common 
  // mathematical functions.
  //
  // To be able to use the class as an automatic differentiator of a function
  // object, we need a templated function object, i.e. an object with:
  // <ul>
  // <li> a <src> template <class T> T operator()(const T)</src>
  // <li> or multiple variable input like: 
  //		<src> template <class T> T operator()(const Vector<T> &)</src>
  // <li> all dependent variables  used in the calculation of the function
  //	value should have been typed with T.
  // </ul>
  // A simple example of such a function object could be:
  // <srcblock>
  //	template <class T> f {
  //	public:
  //	  T operator()(const T &x, const T &a, const T &b) {
  //	  	return a*b*x; }
  // 	};
  //      // Instantiate the following versions:
  //	template class f<Double>;
  //	template class f<SparseDiff<Double> >;
  // </srcblock>
  // A call with values will produce the function value:
  // <srcblock>
  //	cout << f(7.0, 2.0, 3.0) << endl;
  //	// will produce the value at x=7 for a=2; b=3:
  //	42
  //	// But a call indicating that we want derivatives to a and b:
  //	cout << f(SparseDiff<Double>(7.0), SparseDiff<Double>(2.0, 0),
  //		  SparseDiff<Double>(3.0, 1)) << endl;
  //	// will produce the value at x=7 for a=2; b=3:
  //	// and the partial derivatives wrt a and b at x=7:
  //	(42, [21, 14])
  //	// The following will calculate the derivate wrt x:
  //	cout << f(SparseDiff<Double>(7.0, 0), SparseDiff<Double>(2.0),
  //		  SparseDiff<Double>(3.0)) << endl;
  //	(42,[6])
  // </srcblock>
  // Note that in practice constants may be given as Double constants.
  // In actual practice, there are a few rules to obey for the structure of
  // the function object if you want to use the function object and its
  // derivatives in least squares fitting procedures in the Fitting
  // module. The major one is to view the function object having 'fixed' and
  // 'variable' parameters. I.e., rather than viewing the function as
  // depending on parameters <em>a, b, x</em> (<src>f(a,b,x)</src>), the 
  // function is considered to be <src>f(x; a,b)</src>, where <em>a, b</em> 
  // are 'fixed' parameters, and <em>x</em> a variable parameter.
  // Fixed parameters should be contained in a 
  // <linkto class=FunctionParam>FunctionParam</linkto> container object;
  // while the variable parameter(s) are given in the function
  // <src>operator()</src>. See <linkto class=Function>Function</linkto> class
  // for details.
  //
  // A Gaussian spectral profile would in general have the center frequency,
  // the width and the amplitude as fixed parameters, and the frequency as
  // a variable. Given a spectrum, you would solve for the fixed parameters,
  // given spectrum values. However, in other cases the role of the
  // parameters could be reversed. An example could be a whole stack of
  // observed (in the laboratory) spectra at different temperatures at
  // one frequency. In that case the width would be the variable parameter,
  // and the frequency one of the fixed (and to be solved for)parameters. 
  //
  // Since the calculation of the derivatives is done with simple overloading, 
  // the calculation of second (and higher) derivatives is easy. It should be
  // noted that higher deivatives are inefficient in the current incarnation
  // (there is no knowledge e.g. about symmetry in the Jacobian). However,
  // it is a very good way to get the correct answers of the derivatives. In
  // practice actual production code will be better off with specialization
  // of the <src>f<SparseDiff<> ></src> implementation.
  //
  // The <src>SparseDiff</src> class is the class the user communicates with.
  // Alias classes (<linkto class=SparseDiffA>SparseDiffA</linkto> and
  // <linkto class=SparseDiffA>SparseDiffX</linkto>) exist
  // to make it possible to have different incarnations of a templated
  // method (e.g. a generic one and a specialized one). See the
  // <src>dSparseDiff</src> demo for an example of its use.
  //
  // All operators and functions are declared in <linkto file=SparseDiffMath.h> 
  // SparseDiffMath</linkto>. The output operator in 
  // <linkto file=SparseDiffIO.h>SparseDiffIO</linkto>.
  // The actual structure of the
  // data block used by <src>SparseDiff</src> is described in 
  // <linkto class=SparseDiffRep>SparseDiffRep</linkto>.
  //
  // A SparseDiff can be constructed from an AutoDiff.
  // <em>toAutoDiff(n)</em> can convert it to an AutoDiff.
  // </synopsis>
  //
  // <example>
  // <srcblock>
  // // First a simple example.
  // // We have a function of the form f(x,y,z); and want to know the
  // // value of the function for x=10; y=20; z=30; and for
  // // the derivatives at those points.
  // // Specify the values; and indicate the parameter dependence:
  // 	SparseDiff<Double> x(10.0, 0); 
  // 	SparseDiff<Double> y(20.0, 1); 
  // 	SparseDiff<Double> z(30.0, 2);
  // // The result will be:
  // 	SparseDiff<Double> result = x*y + sin(z);
  // 	cout << result.value() << endl;
  // // 199.012
  // 	cout << result.derivatives() << endl;
  // // [20, 10, 0.154251]
  // // Note: sin(30) = -0.988; cos(30) =  0.154251;
  // </srcblock>
  //
  // See for an extensive example the demo program dSparseDiff. It is
  // based on the example given above, and shows also the use of second
  // derivatives (which is just using <src>SparseDiff<SparseDiff<Double> ></src>
  // as template argument). 
  // <srcblock>
  // 	// The function, with fixed parameters a,b:
  //	template <class T> class f {
  //	public:
  //	  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
  //	  void set(const T& a, const T& b) { a_p = a; b_p = b; }
  //	private:
  //	  T a_p;
  //	  T b_p;
  //	};
  //	// Call it with different template arguments:
  //	  Double a0(2), b0(3), x0(7);
  //	  f<Double> f0; f0.set(a0, b0);
  //	  cout << "Value:     " << f0(x0) << endl;
  //	
  //	  SparseDiff<Double> a1(2,0), b1(3,1), x1(7);
  //	  f<SparseDiff<Double> > f1; f1.set(a1, b1);
  //	  cout << "Diff a,b:   " << f1(x1) << endl;
  //	
  //	  SparseDiff<Double> a2(2), b2(3), x2(7,0);
  //	  f<SparseDiff<Double> > f2; f2.set(a2, b2);
  //	  cout << "Diff x:     " << f2(x2) << endl;
  //	
  //	  SparseDiff<SparseDiff<Double> > a3(SparseDiff<Double>(2,0),0),
  //	    b3(SparseDiff<Double>(3,1),1), x3(SparseDiff<Double>(7));
  //	  f<SparseDiff<SparseDiff<Double> > > f3; f3.set(a3, b3);
  //	  cout << "Diff2 a,b:  " << f3(x3) << endl;
  //	
  //	  SparseDiff<SparseDiff<Double> > a4(SparseDiff<Double>(2)),
  //	    b4(SparseDiff<Double>(3)),
  //	    x4(SparseDiff<Double>(7,0),0);
  //	  f<SparseDiff<SparseDiff<Double> > > f4; f4.set(a4, b4);
  //	  cout << "Diff2 x:    " << f4(x4) << endl;
  //
  //    // Result will be:
  //    // Value:     504
  //    // Diff a,b:  (504, [756, 336])
  //    // Diff x:    (504, [72])
  //    // Diff2 a,b: ((504, [756, 336]), [(756, [756, 504]), (336, [504, 112])])
  //    // Diff2 x:   ((504, [72]), [(72, [0])])
  //
  //    // It needed the template instantiations definitions:
  //	template class f<Double>;
  //	template class f<SparseDiff<Double> >;
  //	template class f<SparseDiff<SparseDiff<Double> > >;
  // </srcblock>
  // </example>
  //
  // <motivation>
  // The creation of the class was motivated by least-squares non-linear fits
  // in cases where each individual condition equation depends only on a
  // fraction of the fixed parameters (e.g. self-calibration where only pairs
  // of antennas are present per equation), and hence only a few 
  // partial derivatives of a fitted function are needed. It would be tedious
  // to create functionals for all partial derivatives of a function.
  // </motivation>
  //
  // <templating arg=T>
  //  <li> any class that has the standard mathematical and comparison
  //	operators and functions defined.
  // </templating>
  //
  // <todo asof="2007/11/27">
  // <li> Nothing I know of.
  // </todo>

  template <class T> class SparseDiff {
  public:
    //# Typedefs
    typedef T 			value_type;
    typedef value_type&		reference;
    typedef const value_type&	const_reference;
    typedef value_type*		iterator;
    typedef const value_type*	const_iterator;

    //# Constructors
    // Construct a constant with a value of zero.  Zero derivatives.
    SparseDiff();
 
    // Construct a constant with a value of v.  Zero derivatives.
    SparseDiff(const T &v);

    // A function f(x0,x1,...,xn,...) with a value of v.  The 
    // nth derivative is one, and all other derivatives are zero. 
    SparseDiff(const T &v, const uInt n); 

    // A function f(x0,x1,...,xn,...) with a value of v.  The 
    // nth derivative is der, and all other derivatives are zero. 
    SparseDiff(const T &v, const uInt n, const T &der); 

    // Construct from an AutoDiff
    SparseDiff(const AutoDiff<T> &other);

    // Construct one from another (deep copy)
    SparseDiff(const SparseDiff<T> &other);

    // Destructor
    ~SparseDiff();

    // Assignment operator.  Assign a constant to variable.
    SparseDiff<T> &operator=(const T &v);

    // Assignment operator.  Add a gradient to variable.
    SparseDiff<T> &operator=(const pair<uInt, T> &der);

    // Assignment operator.  Assign gradients to variable.
    SparseDiff<T> &operator=(const vector<pair<uInt, T> > &der);

    // Assign from an Autodiff
    SparseDiff<T> &operator=(const AutoDiff<T> &other);

    // Assign one to another (deep copy)
    SparseDiff<T> &operator=(const SparseDiff<T> &other);

    // Assignment operators
    // <group>
    void operator*=(const SparseDiff<T> &other);
    void operator/=(const SparseDiff<T> &other);
    void operator+=(const SparseDiff<T> &other);
    void operator-=(const SparseDiff<T> &other);
    void operator*=(const T other) { rep_p->operator*=(other);
    value() *= other; }
    void operator/=(const T other) { rep_p->operator/=(other);
    value() /= other; }
    void operator+=(const T other) { value() += other; }
    void operator-=(const T other) { value() -= other; }
    // </group>

    // Convert to an AutoDiff of length <em>n</em>
    AutoDiff<T> toAutoDiff(uInt n) const;

    // Returns the pointer to the structure of value and derivatives.
    // <group>
    SparseDiffRep<T> *theRep() { return rep_p; }
    const SparseDiffRep<T> *theRep() const { return rep_p; }
    // </group>

    // Returns the value of the function
    // <group>
    T &value() { return rep_p->val_p; }
    const T &value() const { return rep_p->val_p; }
    // </group>
  
    // Returns a vector of the derivatives of a SparseDiff
    // <group>
    vector<pair<uInt, T> > &derivatives() const;
    void derivatives(vector<pair<uInt, T> > &res) const;
    const vector<pair<uInt, T> > &grad() const { return rep_p->grad_p; }
    vector<pair<uInt, T> > &grad() { return rep_p->grad_p; }
    // </group>

    // Returns a specific derivative. No check for a valid which.
    // <group>
    pair<uInt, T> &derivative(uInt which) { return rep_p->grad_p[which]; }
    const pair<uInt, T> &derivative(uInt which) const {
      return rep_p->grad_p[which]; }
    // </group>
  
    // Return total number of derivatives
    uInt nDerivatives() const { return rep_p->grad_p.size(); }
  
    // Is it a constant, i.e., with zero derivatives?
    Bool isConstant() const { return rep_p->grad_p.empty(); }

    // Sort criterium
    static Bool ltSort(pair<uInt, T> &lhs, pair<uInt, T> &rhs);

    // Sort derivative list; cater for doubles and zeroes
    void sort();
 
  private:
    //# Data
    // Value representation
    SparseDiffRep<T> *rep_p;

  };


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Mathematics/SparseDiff.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
