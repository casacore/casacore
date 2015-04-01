//# Random.h: Random number classes
//# Copyright (C) 1992,1993,1994,1995,1999,2000,2001
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

#ifndef CASA_RANDOM_H
#define CASA_RANDOM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
template<class T> class Vector;

// <summary>Base class for random number generators</summary>
//
// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> A knowledge of C++, in particular inheritance
//   <li> College level mathematics
// </prerequisite>
//
// <etymology>
// RNG stands for "Random Number Generator"
// </etymology>
//
// <synopsis>
// <h4>General Structure of the Classes</h4>
// 

// The two base classes <linkto class=RNG>RNG</linkto> and 
// <linkto class=Random>Random</linkto> are used together to generate a variety
// of random number distributions.  A distinction must be made between
// <em>random number generators</em>, implemented by class derived from
// <src>RNG</src>, and <em>random number distributions</em>.  A random number
// generator produces a series of randomly ordered bits.  These bits can be
// used directly, or cast to another representation, such as a floating point
// value.  A random number generator should produce a <em>uniform</em>
// distribution.  A random number distribution, on the other hand, uses the
// randomly generated bits of a generator to produce numbers from a
// distribution with specific properties.  Each instance of <src>Random</src>
// uses an instance of class <src>RNG</src> to provide the raw, uniform
// distribution used to produce the specific distribution.  Several instances
// of <src>Random</src> classes can share the same instance of <src>RNG</src>,
// or each instance can use its own copy.

// <h4> RNG </h4>
// 

// Random distributions are constructed from classes derived from
// <src>RNG</src>, the actual random number generators.  The <src>RNG</src>
// class contains no data; it only serves to define the interface to random
// number generators.  The <src>RNG::asuInt</src> member returns a 32-bit
// unsigned integer of random bits.  Applications that require a number of
// random bits can use this directly.  More often, these random bits are
// transformed to a uniformly distributed floating point number using either
// <src>asFloat</src> or <src>asDouble</src>.  These functions return differing
// precisions and the <src>asDouble</src> function will use two different
// random 32-bit integers to get a legal <src>double</src>, while
// <src>asFloat</src> will use a single integer.  These members are used by
// classes derived fro the <src>Random</src> base class to implement a variety
// of random number distributions.
//
// Currently, the following subclasses are provided:
// <ul>
// <li> <linkto class=MLCG>MLCG</linkto>: 
//      Multiplicative Linear Congruential Generator.
//      A reasonable generator for most purposes.
// <li> <linkto class=ACG>ACG</linkto>: Additive Number Generator. 
//      A high quality generator that uses more memory and computation time.
// </ul>
// 
// <note role=warning> This class assumes that IEEE floating point
// representation is used for the floating point numbers and that the integer
// and unsigned integer type is exactly 32 bits long.
// </note>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Random numbers are used everywhere, particularly in simulations.
// </motivation>
//
// <thrown>
// <li> AipsError: If a programming error or unexpected numeric size is
// detected. Should not occur in normal usage.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class RNG {
public:
  // A virtual destructor is needed to ensure that the destructor of derived
  // classes gets used.
  virtual ~RNG();

  // Resets the random number generator. After calling this function the random
  // numbers generated will be the same as if the object had just been
  // constructed.
  virtual void reset() = 0;

  // Return the 32-random bits as an unsigned integer
  virtual uInt asuInt() = 0;

  // Return random bits converted to either a Float or a Double. The returned
  // value x is in the range 1.0 > x >= 0.0 
  // <group>
  Float asFloat();
  Double asDouble();
  // </group>
};

// <summary>Additive number generator</summary>
//
// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> A knowledge of C++, in particular inheritance
//   <li> College level mathematics
// </prerequisite>
//
// <etymology>
// ACG stands for "Additive Congruential Generator"
// </etymology>
//
// <synopsis>
// This class implements the additive number generator as presented in Volume
// II of The Art of Computer Programming by Knuth. I have coded the algorithm
// and have added the extensions by Andres Nowatzyk of CMU to randomize the
// result of algorithm M a bit by using an LCG & a spatial permutation table.
//
// The version presented uses the same constants for the LCG that Andres uses
// (chosen by trial & error). The spatial permutation table is the same size
// (it is based on word size). This is for 32-bit words.
//
// The <src>auxillary table</src> used by the LCG table varies in size, and is
// chosen to be the the smallest power of two which is larger than twice the
// size of the state table.
//
// Class <src>ACG</src> is a variant of a Linear Congruential Generator
// (Algorithm M) described in Knuth, "Art of Computer Programming, Vol III".
// This result is permuted with a Fibonacci Additive Congruential Generator to
// get good independence between samples.  This is a very high quality random
// number generator, although it requires a fair amount of memory for each
// instance of the generator.
// 
// The constructor takes two parameters: the seed and the size.  The seed can
// be any number. The performance of the generator depends on having a
// distribution of bits through the seed.  If you choose a number in the range
// of 0 to 31, a seed with more bits is chosen. Other values are
// deterministically modified to give a better distribution of bits.  This
// provides a good random number generator while still allowing a sequence to
// be repeated given the same initial seed.
// 
// The <src>size</src> parameter determines the size of two tables used in the
// generator. The first table is used in the Additive Generator; see the
// algorithm in Knuth for more information. In general, this table contains
// <src>size</src> integers. The default value, used in the algorithm in Knuth,
// gives a table of 55 integers (220 bytes). The table size affects the period
// of the generators; smaller values give shorter periods and larger tables
// give longer periods. The smallest table size is 7 integers, and the longest
// is 98. The <src>size</src> parameter also determines the size of the table
// used for the Linear Congruential Generator. This value is chosen implicitly
// based on the size of the Additive Congruential Generator table. It is two
// powers of two larger than the power of two that is larger than
// <src>size</src>.  For example, if <src>size</src> is 7, the ACG table
// contains 7 integers and the LCG table contains 128 integers. Thus, the
// default size (55) requires 55 + 256 integers, or 1244 bytes. The largest
// table requires 2440 bytes and the smallest table requires 100 bytes.
// Applications that require a large number of generators or applications that
// are not so fussy about the quality of the generator may elect to use the
// <src>MLCG</src> generator.
//
// <note role=warning> This class assumes that the integer and unsigned integer
// type is exactly 32 bits long.
// </note> 
// </synopsis>
//
// <example>
// </example>
//
// <thrown>
// <li> AipsError: If a programming error or unexpected numeric size is
// detected. Should not occur in normal usage.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class ACG : public RNG {

public:
  // The constructor allows you to specify seeds. The seed should be a big
  // random number and size must be between 7 and 98. See the synopsis for more
  // details.
  explicit ACG(uInt seed = 0, Int size = 55);

  // The destructor cleans up memory allocated by this class
  virtual ~ACG();

  // Resets the random number generator. After calling this function the random
  // numbers generated will be the same as if the object had just been
  // constructed.
  virtual void reset();

  // Return the 32-random bits as an unsigned integer
  virtual uInt asuInt();

private:
  uInt itsInitSeed;     //# used to reset the generator
  Int itsInitTblEntry;
  
  uInt* itsStatePtr;
  uInt* itsAuxStatePtr;
  Short itsStateSize;
  Short itsAuxSize;
  uInt lcgRecurr;
  Short itsJ;
  Short itsK;
};

// <summary> Multiplicative linear congruential generator </summary>

// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> A knowledge of C++, in particular inheritance
//   <li> College level mathematics
// </prerequisite>
//
// <etymology>
// MLCG stands for "Multiplicative Linear Congruential Generator"
// </etymology>
//

// <synopsis>
// The <src>MLCG</src> class implements a <em>Multiplicative Linear
// Congruential Generator</em>. In particular, it is an implementation of the
// double MLCG described in <em>Efficient and Portable Combined Random Number
// Generators</em> by Pierre L'Ecuyer, appearing in <em>Communications of the
// ACM, Vol. 31. No. 6</em>. This generator has a fairly long period, and has
// been statistically analyzed to show that it gives good inter-sample
// independence.
// 

// The constructor has two parameters, both of which are seeds for the
// generator. As in the <src>ACG</src> generator, both seeds are modified to
// give a "better" distribution of seed digits. Thus, you can safely use values
// such as <src>0</src> or <src>1</src> for the seeds.  The <src>MLCG</src>
// generator used much less state than the <src>ACG</src> generator; only two
// integers (8 bytes) are needed for each generator.

// <note role=warning> This class assumes that the integer and unsigned integer
// type is exactly 32 bits long.
// </note> 
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: If a programming error or unexpected numeric size is
// detected. Should not occur in normal usage.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class MLCG : public RNG {
public:
  // The constructor allows you to specify seeds.
  explicit MLCG(Int seed1 = 0, Int seed2 = 1);
  
  // The destructor is trivial 
  virtual ~MLCG();

  // Return the 32-random bits as an unsigned integer
  virtual uInt asuInt();
  
  // Resets the random number generator. After calling this function the random
  // numbers generated will be the same as if the object had just been
  // constructed.
  virtual void reset();

  // Functions that allow the user to retrieve or change the seed integers. The
  // seeds returned are not the user supplied values but the values obtained
  // after some deterministic modification to produce a more uniform bit
  // distribution.
  // <group>
  Int seed1() const;
  void seed1(Int s);
  Int seed2() const;
  void seed2(Int s);
  void reseed(Int s1, Int s2);
  // </group>
  
private:
  Int itsInitSeedOne;
  Int itsInitSeedTwo;
  Int itsSeedOne;
  Int itsSeedTwo;
};

inline Int MLCG::seed1() const
{
  return itsSeedOne;
}

inline void MLCG::seed1(Int s)
{
  itsInitSeedOne = s;
  reset();
}

inline Int MLCG::seed2() const
{
  return itsSeedTwo;
}

inline void MLCG::seed2(Int s)
{
  itsInitSeedTwo = s;
  reset();
}

inline void MLCG::reseed(Int s1, Int s2)
{
  itsInitSeedOne = s1;
  itsInitSeedTwo = s2;
  reset();
}

// <summary>Base class for random number distributions</summary>

// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> A knowledge of C++, in particular inheritance
//   <li> College level mathematics
// </prerequisite>
//
// <synopsis>
// A random number generator may be declared by first constructing a
// <src>RNG</src> object and then a <src>Random</src>. For example,
// <srcblock>
//   ACG gen(10, 20); 
//   NegativeExpntl rnd (1.0, &gen);
// </srcblock>
// declares an additive congruential generator with seed 10 and table size 20,
// that is used to generate exponentially distributed values with mean of 1.0.
// 
// The virtual member <src>Random::operator()</src> is the common way of
// extracting a random number from a particular distribution.  The base class,
// <src>Random</src> does not implement <src>operator()</src>.  This is
// performed by each of the derived classes. Thus, given the above declaration
// of <src>rnd</src>, new random values may be obtained via, for example,
// <src>Double nextExpRand = rnd();</src>
//
// Currently, the following subclasses are provided:
//
// <ul>
//    <li> <linkto class=Binomial>Binomial</linkto>
//    <li> <linkto class=Erlang>Erlang</linkto>
//    <li> <linkto class=Geometric>Geometric</linkto>
//    <li> <linkto class=HyperGeometric>HyperGeometric</linkto>
//    <li> <linkto class=NegativeExpntl>NegativeExpntl</linkto>
//    <li> <linkto class=Normal>Normal</linkto>
//    <li> <linkto class=LogNormal>LogNormal</linkto>
//    <li> <linkto class=Poisson>Poisson</linkto>
//    <li> <linkto class=DiscreteUniform>DiscreteUniform</linkto>
//    <li> <linkto class=Uniform>Uniform</linkto>
//    <li> <linkto class=Weibull>Weibull</linkto>
// </ul>
// </synopsis>
//
// <example>
// </example>
//
// <thrown>
//   <li> No exceptions are thrown directly from this class.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Random {
public:
  
  // This enumerator lists all the predefined random number distributions.
  enum Types {
    // 2 parameters. The binomial distribution models successfully drawing
    // items from a pool.  Specify n and p. n is the number of items in the
    // pool, and p, is the probability of each item being successfully drawn.
    // It is required that n > 0 and 0 <= p <= 1
   BINOMIAL,

   // 2 parameters. Model a uniform random variable over the closed
   // interval. Specify the values low and high. The low parameter is the
   // lowest possible return value and the high parameter is the highest.  It
   // is required that low < high.
   DISCRETEUNIFORM,

   // 2 parameters, mean and variance.  It is required that the mean is
   // non-zero and the variance is positive.
   ERLANG, 

   // 1 parameters, the mean.  It is required that 0 <= probability < 1
   GEOMETRIC, 

   // 2 parameters, mean and variance.  It is required that the variance is
   // positive and that the mean is non-zero and not bigger than the
   // square-root of the variance.
   HYPERGEOMETRIC,

   // 2 parameters, the mean and variance.  It is required that the variance is
   // positive.
   NORMAL, 

   // 2 parameters, mean and variance.  It is required that the supplied
   // variance is positive and that the mean is non-zero
   LOGNORMAL,

   // 1 parameter, the mean.
   NEGATIVEEXPONENTIAL,

   // 1 parameter, the mean. It is required that the mean is non-negative
   POISSON, 

   // 2 parameters, low and high.  Model a uniform random variable over the
   // closed interval. The low parameter is the lowest possible return value
   // and the high parameter can never be returned.  It is required that low <
   // high.
   UNIFORM,

   // 2 parameters, alpha and beta.  It is required that the alpha parameter is
   // not zero.
   WEIBULL,

   // An non-predefined random number distribution
   UNKNOWN,
   
   // Number of distributions
   NUMBER_TYPES};

  // A virtual destructor is needed to ensure that the destructor of derived
  // classes gets used. Not that this destructor does NOT delete the pointer to
  // the RNG object
  virtual ~Random();

  // This function returns a random number from the appropriate distribution.
  virtual Double operator()() = 0;
  
  // Functions that allow you to access and change the class that generates the
  // random bits.
  // <group>
  RNG* generator();
  void generator(RNG* p);
  // </group>

  // Convert the enumerator to a lower-case string. 
  static String asString(Random::Types type);
  
  // Convert the string to enumerator. The parsing of the string is case
  // insensitive. Returns the Random::UNKNOWN value if the string does not
  // cotrtrespond to any of the enumerators.
  static Random::Types asType(const String& str);

  // Convert the Random::Type enumerator to a specific object (derived from
  // Random but upcast to a Random object). Returns a null pointer if the
  // object could not be constructed. This will occur is the enumerator is
  // UNKNOWN or NUMBER_TYPES or there is insufficient memory. The caller of
  // this function is responsible for deleting the pointer.
  static Random* construct(Random::Types type, RNG* gen);

  // These function allow you to manipulate the parameters (mean variance etc.)
  // of random number distribution. The parameters() function returns the
  // current value, the setParameters function allows you to change the
  // parameters and the checkParameters function will return False if the
  // supplied parameters are not appropriate for the distribution.
  // <group>
  virtual void setParameters(const Vector<Double>& parms) = 0;
  virtual Vector<Double> parameters() const = 0;
  virtual Bool checkParameters(const Vector<Double>& parms) const = 0;
  // </group>
  
  // returns the default parameters for the specified distribution. Returns an
  // empty Vector if a non-predifined distribution is used.
  static Vector<Double> defaultParameters (Random::Types type);
  
protected:
  //# This class contains pure virtual functions hence the constructor can only
  //# sensibly be used by derived classes.
  Random(RNG* generator);

  //# The RNG class provides the random bits.
  RNG* itsRNG;
};

inline Random::Random(RNG* gen)
{
  itsRNG = gen;
}

inline RNG* Random::generator()
{
  return itsRNG;
}

inline void Random::generator(RNG* p)
{
  itsRNG = p;
}


// <summary> Binomial distribution </summary>

// <synopsis>
// The binomial distribution models successfully drawing items from a pool.
// <src>n</src> is the number of items in the pool, and <src>p</src>, is the
// probability of each item being successfully drawn.  The
// <src>operator()</src> functions returns an integral value indicating the
// number of items actually drawn from the pool. It is possible to get this
// same value as an integer using the asInt function.

// It is assumed that <src>n > 0</src> and <src>0 <= p <= 1</src> an AipsError
// exception thrown if it is not true.  The remaining members allow you to read
// and set the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Binomial: public Random {
public:
  // Construct a random number generator for a binomial distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The second
  // and third arguments are the parameters are the Binomial distribution as
  // described in the synopsis.
  Binomial(RNG* gen, uInt n=1, Double p=0.5);

  // The destructor is trivial
  virtual ~Binomial();

  // Returns a value from the Binomial distribution. The returned value is a
  // non-negative integer and using the asInt function bypasses the conversion
  // to a floating point number.  
  // <group>
  virtual Double operator()();
  uInt asInt();
  // </group>
  
  // Functions that allow you to query and change the parameters of the
  // binomial distribution.
  // <group>
  uInt n() const;
  void n(uInt newN);
  void n(Double newN);
  Double p() const;
  void p(Double newP);
  // </group>
  
  // These function allow you to manipulate the parameters (n & p) described
  // above through the base class. The Vectors must always be of length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  uInt itsN;
  Double itsP;
};

inline uInt Binomial::n() const {
  return itsN;
}

inline Double Binomial::p() const {
  return itsP;
}

// <summary>Discrete uniform distribution</summary>

// <synopsis>

// The <src>DiscreteUniform</src> class implements a quantized uniform random
// variable over the closed interval ranging from <src>[low..high]</src>.  The
// <src>low</src> parameter is the lowest possible return value and the
// <src>high</src> parameter is the highest.  The <src>operator()</src>
// functions returns a value from this distribution. It is possible to get this
// same value as an integer using the asInt function.

// It is assumed that low limit is less than the high limit and an AipsError
// exception thrown if this is not true.  The remaining members allow you to
// read and set the parameters.

// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class DiscreteUniform: public Random {
public:
  // Construct a random number generator for a discrete uniform
  // distribution. The first argument is a class that produces random
  // bits. This pointer is NOT taken over by this class and the user is
  // responsible for deleting it. The second and third arguments define the
  // range of possible return values for this distribution as described in the
  // synopsis.
  DiscreteUniform(RNG* gen, Int low=-1, Int high=1);
  
  // The destructor is trivial
  virtual ~DiscreteUniform();

  // Returns a value from the discrete uniform distribution.  The returned
  // value is a integer and using the asInt function bypasses the conversion to
  // a floating point number.  
  // <group>
  virtual Double operator()();
  Int asInt();
  // </group>
  
  // Functions that allow you to query and change the parameters of the
  // discrete uniform distribution.  
  // <group>
  Int low() const;
  void low(Int x);
  Int high() const;
  void high(Int x);
  void range(Int low, Int high);
  // </group>
  
  // These function allow you to manipulate the parameters (low & high)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  static Double calcDelta(Int low, Int high);
  Int itsLow;
  Int itsHigh;
  Double itsDelta;
};

inline Int DiscreteUniform::low() const {
  return itsLow; 
}

inline Int DiscreteUniform::high() const {
  return itsHigh;
}

// <summary>Erlang distribution</summary>

// <synopsis>
// The <src>Erlang</src> class implements an Erlang distribution with mean
// <src>mean</src> and variance <src>variance</src>.

// It is assumed that the mean is non-zero and the variance is positive an
// AipsError exception thrown if this is not true.  The remaining members allow
// you to read and set the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Erlang: public Random {
public:
  // Construct a random number generator for an Erlang distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The second
  // and third arguments define the parameters for this distribution as
  // described in the synopsis.
  Erlang(RNG* gen, Double mean=1.0, Double variance=1.0);
  
  // The destructor is trivial
  virtual ~Erlang();

  // Returns a value from the Erlang distribution.
  virtual Double operator()();
  
  // Functions that allow you to query and change the parameters of the
  // discrete uniform distribution.
  // <group>
  Double mean() const;
  void mean(Double x);
  Double variance() const;
  void variance(Double x);
  // </group>

  // These function allow you to manipulate the parameters (mean & variance)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  void setState();
  Double itsMean;
  Double itsVariance;
  Int itsK;
  Double itsA;
};

inline Erlang::Erlang(RNG* gen, Double mean, Double variance) 
  :Random(gen),
   itsMean(mean),
   itsVariance(variance)
{
  setState();
}

inline Double Erlang::mean() const {
  return itsMean;
}

inline void Erlang::mean(Double x) {
  itsMean = x;
  setState(); 
}

inline Double Erlang::variance() const {
  return itsVariance;
}

inline void Erlang::variance(Double x) {
  itsVariance = x;
  setState();
}

// <summary> Discrete geometric distribution </summary>

// <synopsis>
// The <src>Geometric</src> class implements a discrete geometric distribution.
// The <src>probability</src> is the only parameter.  The <src>operator()</src>
// functions returns an non-negative integral value indicating the number of
// uniform random samples actually drawn before one is obtained that is larger
// than the given probability. To get this same value as an integer use the
// asInt function.
//
// It is assumed that the probability is between zero and one 
// <src>(0 <= probability < 1)</src> and and AipsError exception thrown if this
// is not true.  The remaining function allow you to read and set the
// parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Geometric: public Random {
public:
  // Construct a random number generator for a geometric uniform
  // distribution. The first argument is a class that produces random
  // bits. This pointer is NOT taken over by this class and the user is
  // responsible for deleting it. The second argument defines the range of
  // possible return values for this distribution as described in the synopsis.
  Geometric(RNG* gen, Double probability=0.5);
  
  // The destructor is trivial
  virtual ~Geometric();

  // Returns a value from the geometric uniform distribution.  The returned
  // value is a non-negative integer and using the asInt function bypasses the
  // conversion to a floating point number.  
  // <group>
  virtual Double operator()();
  uInt asInt();
  // </group>
  
  // Functions that allow you to query and change the parameters of the
  // geometric uniform distribution.  
  // <group>
  Double probability() const;
  void probability(Double x);
  // </group>
  
  // These function allow you to manipulate the parameter (probability)
  // described above through the base class. The Vectors must always be of
  // length one.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  Double itsProbability;
};

inline Double Geometric::probability() const {
  return itsProbability;
}

// <summary> Hypergeometric distribution </summary>

// <synopsis>
// The <src>HyperGeometric</src> class implements the hypergeometric
// distribution.  The <src>mean</src> and <src>variance</src> are the
// parameters of the distribution.  The <src>operator()</src> functions returns
// a value from this distribution

// It is assumed the variance is positive and that the mean is non-zero and not
// bigger than the square-root of the variance. An AipsError exception is
// thrown if this is not true.  The remaining members allow you to read and set
// the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class HyperGeometric: public Random {
public:
  // Construct a random number generator for an hypergeometric
  // distribution. The first argument is a class that produces random
  // bits. This pointer is NOT taken over by this class and the user is
  // responsible for deleting it. The second and third arguments define the
  // parameters for this distribution as described in the synopsis.
  HyperGeometric(RNG* gen, Double mean=0.5, Double variance=1.0);
  
  // The destructor is trivial
  virtual ~HyperGeometric();

  // Returns a value from the hypergeometric distribution.
  virtual Double operator()();
  
  // Functions that allow you to query and change the parameters of the
  // hypergeometric distribution.
  // <group>
  Double mean() const;
  void mean(Double x);
  Double variance() const;
  void variance(Double x);
  // </group>
  
  // These function allow you to manipulate the parameters (mean & variance)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  void setState();
  Double itsMean;
  Double itsVariance;
  Double itsP;
};


inline HyperGeometric::HyperGeometric(RNG* gen, Double mean, Double variance)
  :Random(gen),
   itsMean(mean),
   itsVariance(variance)
{
  setState();
}

inline Double HyperGeometric::mean() const {
  return itsMean; 
}

inline void HyperGeometric::mean(Double x) {
  itsMean = x;
  setState();
}

inline Double HyperGeometric::variance() const {
  return itsVariance; 
}

inline void HyperGeometric::variance(Double x) {
  itsVariance = x;
  setState(); 
}

// <summary>Normal or Gaussian distribution </summary>

// <synopsis>
// The <src>Normal</src> class implements the normal or Gaussian distribution.
// The <src>mean</src> and <src>variance</src> are the parameters of the
// distribution.  The <src>operator()</src> functions returns a value from this
// distribution

// It is assumed that the supplied variance is positive and an AipsError
// exception is thrown if this is not true.  The remaining members allow you to
// read and set the parameters. The <src>LogNormal</src> class is derived from
// this one.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Normal: public Random {
public:
  // Construct a random number generator for a normal distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The second
  // and third arguments define the parameters for this distribution as
  // described in the synopsis.
  Normal(RNG* gen, Double mean=0.0, Double variance=1.0);

  // The destructor is trivial
  virtual ~Normal();

  // Returns a value from the normal distribution.
  virtual Double operator()();
  
  // Functions that allow you to query and change the parameters of the
  // normal distribution.
  // <group>
  virtual Double mean() const;
  virtual void mean(Double x);
  virtual Double variance() const;
  virtual void variance(Double x);
  // </group>
  
  // These function allow you to manipulate the parameters (mean & variance)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  Double itsMean;
  Double itsVariance;
  Double itsStdDev;
  Bool itsCached;
  Double itsCachedValue;
};

inline Double Normal::mean() const {
  return itsMean;
}

inline Double Normal::variance() const {
  return itsVariance;
}

// <summary> Logarithmic normal distribution </summary>

// <synopsis>
// The <src>LogNormal</src> class implements the logaraithmic normal
// distribution.  The <src>mean</src> and <src>variance</src> are the
// parameters of the distribution. The <src>operator()</src> functions returns
// a value from this distribution

// It is assumed that the supplied variance is positive and an AipsError
// exception is thrown if this is not true.  The remaining members allow you to
// read and set the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class LogNormal: public Normal {
public:
  // Construct a random number generator for a log-normal distribution. The
  // first argument is a class that produces random bits. This pointer is NOT
  // taken over by this class and the user is responsible for deleting it. The
  // second and third arguments define the parameters for this distribution as
  // described in the synopsis.
  LogNormal(RNG* gen, Double mean=1.0, Double variance=1.0);

  // The destructor is trivial
  virtual ~LogNormal();

  // Returns a value from the log-normal distribution.
  virtual Double operator()();

  // Functions that allow you to query and change the parameters of the
  // log-normal distribution.
  // <group>
  virtual Double mean() const;
  virtual void mean(Double x);
  virtual Double variance() const;
  virtual void variance(Double x);
  // </group>

  // These function allow you to manipulate the parameters (mean & variance)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  void setState();
  Double itsLogMean;
  Double itsLogVar;
};

inline Double LogNormal::mean() const {
  return itsLogMean;
}

inline Double LogNormal::variance() const {
  return itsLogVar;
}

// <summary>Negative exponential distribution</summary>

// <synopsis>
// The <src>NegativeExpntl</src> class implements a negative exponential
// distribution.  The <src>mean</src> parameter, is the only parameter of this
// distribution.  The <src>operator()</src> functions returns a value from this
// distribution. The remaining members allow you to inspect and change the
// mean.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> No exceptions are thrown by this class.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class NegativeExpntl: public Random {
public:
  // Construct a random number generator for a negative exponential
  // distribution. The first argument is a class that produces random
  // bits. This pointer is NOT taken over by this class and the user is
  // responsible for deleting it. The second argument defines the parameters
  // for this distribution as described in the synopsis.
  NegativeExpntl(RNG* gen, Double mean=1.0);

  // The destructor is trivial
  virtual ~NegativeExpntl();

  // Returns a value from the negative exponential distribution.
  virtual Double operator()();

  // Functions that allow you to query and change the parameters of the
  // negative exponential distribution.
  // <group>
  Double mean() const;
  void mean(Double x);
  // </group>
  
  // These function allow you to manipulate the parameters (mean)
  // described above through the base class. The Vectors must always be of
  // length one.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  Double itsMean;
};

inline Double NegativeExpntl::mean() const {
  return itsMean; 
}

// <summary> Poisson distribution </summary>
// <synopsis>
// The <src>Poisson</src> class implements a Poisson distribution.  The
// <src>mean</src> parameter, is the only parameter of this distribution.  The
// <src>operator()</src> functions returns a value from this distribution. The
// remaining members allow you to inspect and change the mean.

// It is assumed that the supplied mean is non-negative and an AipsError
// exception is thrown if this is not true.  The remaining members allow you to
// read and set the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> No exceptions are thrown by this class.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Poisson: public Random {
public:
  // Construct a random number generator for a Poisson distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The second
  // argument defines the parameters for this distribution as described in the
  // synopsis.
  Poisson(RNG* gen, Double mean=0.0);
  
  // The destructor is trivial
  virtual ~Poisson();

  // Returns a value from the Poisson distribution. The returned value is a
  // non-negative integer and using the asInt function bypasses the conversion
  // to a floating point number.
  // <group>
  virtual Double operator()();
  uInt asInt();
  // </group>
  
  // Functions that allow you to query and change the parameters of the
  // Poisson distribution.
  // <group>
  Double mean() const;
  void mean(Double x);
  // </group>
  
  // These function allow you to manipulate the parameters (mean)
  // described above through the base class. The Vectors must always be of
  // length one.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  Double itsMean;
};

inline Double Poisson::mean() const { 
  return itsMean;
}

// <summary>Uniform distribution</summary>

// <synopsis>
// The <src>Uniform</src> class implements a uniform random variable over the
// copen interval ranging from <src>[low..high)</src>.  The <src>low</src>
// parameter is the lowest possible return value and the <src>high</src>
// parameter can never be returned.  The <src>operator()</src> functions
// returns a value from this distribution.

// It is assumed that low limit is less than the high limit and an AipsError
// exception is thrown if this is not true.  The remaining members allow you to
// read and set the parameters.

// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Uniform: public Random {
public:
  // Construct a random number generator for a uniform distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The
  // remaining arguments define the parameters for this distribution as
  // described in the synopsis.
  Uniform(RNG* gen, Double low=-1.0, Double high=1.0);

  // The destructor is trivial
  virtual ~Uniform();

  // Returns a value from the uniform distribution. 
  virtual Double operator()();
  
  // Functions that allow you to query and change the parameters of the
  // uniform distribution.  
  // <group>
  Double low() const;
  void low(Double x);
  Double high() const;
  void high(Double x);
  void range(Double low, Double high);
  // </group>

  // These function allow you to manipulate the parameters (low & high)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  static Double calcDelta(Double low, Double high);
  Double itsLow;
  Double itsHigh;
  Double itsDelta;
};

inline Double Uniform::low() const {
  return itsLow;
}

inline Double Uniform::high() const {
  return itsHigh;
}

// <summary>Weibull distribution</summary>

// <synopsis> 

// The <src>Weibull</src> class implements a weibull distribution with
// parameters <src>alpha</src> and <src>beta</src>.  The first parameter to the
// class constructor is <src>alpha</src>, and the second parameter is
// <src>beta</src>.  It is assumed that the alpha parameter is not zero and an
// AipsError exception is thrown if this is not true.  The remaining members
// allow you to read and set the parameters.
// </synopsis>

// <example>
// </example>
//
// <thrown>
// <li> AipsError: if bad values for the arguments are given, as specified
//      above.
// </thrown>
//
// <todo asof="2000/05/09">
//   <li> Nothing I hope!
// </todo>

class Weibull: public Random {
public:
  // Construct a random number generator for a uniform distribution. The first
  // argument is a class that produces random bits. This pointer is NOT taken
  // over by this class and the user is responsible for deleting it. The
  // remaining arguments define the parameters for this distribution as
  // described in the synopsis.
  Weibull(RNG* gen, Double alpha=1.0, Double beta=1.0);
  
  // The destructor is trivial
  virtual ~Weibull();

  // Returns a value from the Weiball distribution. 
  virtual Double operator()();
  
  // Functions that allow you to query and change the parameters of the
  // Weiball distribution.  
  // <group>
  Double alpha() const;
  void alpha(Double x);
  Double beta() const;
  void beta(Double x);
  // </group>

  // These function allow you to manipulate the parameters (alpha & beta)
  // described above through the base class. The Vectors must always be of
  // length two.
  // <group>
  virtual void setParameters(const Vector<Double>& parms);
  virtual Vector<Double> parameters() const;
  virtual Bool checkParameters(const Vector<Double>& parms) const;
  // </group>

private:
  void setState();
  Double itsAlpha;
  Double itsBeta;
  Double itsInvAlpha;
};

inline Double Weibull::alpha() const {
  return itsAlpha;
}

inline Double Weibull::beta() const {
  return itsBeta; 
}


} //# NAMESPACE CASACORE - END

#endif
