//# Random.h: Random number classes
//# Copyright (C) 1992,1993,1994,1995,1999
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

#if !defined(AIPS_RANDOM_H)
#define AIPS_RANDOM_H

//# This is essentially just a concatenation of:
//# ACG.h           Geom.h          NegExp.h        Random.h 
//# Binomial.h      HypGeom.h       Normal.h        RndInt.h
//# DiscUnif.h      LogNorm.h       Poisson.h       Uniform.h
//# Erlang.h        MLCG.h          RNG.h           Weibull.h
//#
//# from libg++-2.2. Random number classes.
//#     written by Dirk Grunwald (grunwald@cs.uiuc.edu)
//# Modifications for AIPS++ Brian Glendenning, 10/5/92.
//# Changed to use exceptions instead of assert 12/1/92.

#include <aips/aips.h>
#include <aips/Mathematics/Math.h>

//# on the alpha long is 64 bits, we redefine it here to make 
//# this nasty piece of code work
#if defined(__alpha__) || defined(SGI64)
#define long int
#endif

// used to access floats as unsigneds
union PrivateRNGSingleType {
    float s;
    unsigned long u;
};

// used to access doubles as unsigneds
union PrivateRNGDoubleType {
    double d;
    unsigned long u[2];
};


// <summary> Base class for Random Number Generators </summary>

// <synopsis>
// Base class for Random Number Generators.
// See <linkto class=ACG>ACG</linkto> and <linkto class=MLCG>MLCG</linkto>
// for instances.
// </synopsis>

// <h3> Random Number Generators and related classes </h3>
// 
// The two classes <linkto class=RNG><src>RNG</src></linkto> and
// <src>Random</src> are used together to
// generate a variety of random number distributions.  A distinction must be
// made between <em>random number generators</em>, implemented by class
// <src>RNG</src>, and <em>random number distributions</em>.
// A random number generator produces a
// series of randomly ordered bits.  These bits can be used directly, or
// cast to other representations, such as a floating point value.  A
// random number generator should produce a <em>uniform</em> distribution.  A
// random number distribution, on the other hand, uses the randomly
// generated bits of a generator to produce numbers from a distribution
// with specific properties.  Each instance of <src>Random</src> uses an instance
// of class <src>RNG</src> to provide the raw, uniform distribution used to
// produce the specific distribution.  Several instances of <src>Random</src>
// classes can share the same instance of <src>RNG</src>, or each instance can
// use its own copy.

// <h3> RNG </h3>
// 
// Random distributions are constructed from members of class <src>RNG</src>,
// the actual random number generators.  The <src>RNG</src> class contains no
// data; it only serves to define the interface to random number
// generators.  The <src>RNG::asLong</src> member returns an unsigned long
// (typically 32 bits) of random bits.  Applications that require a number
// of random bits can use this directly.  More often, these random bits
// are transformed to a uniform random number:
// 
// <srcblock>
//
//          //
//          // Return random bits converted to either a float or a double
//          //
//          float asFloat();
//          double asDouble();
//      };
//
// </srcblock>
// 
// using either <src>asFloat</src> or <src>asDouble</src>.  It is intended
// that <src>asFloat</src>
// and <src>asDouble</src> return differing precisions; typically,
// <src>asDouble</src> will
// draw two random longwords and transform them into a legal <src>double</src>,
// while <src>asFloat</src> will draw a single longword and transform it into a
// legal <src>float</src>.  These members are used by subclasses of the
// <src>Random</src> class to implement a variety of random number distributions.
//
// Currently, the following subclasses are provided:
//
// <ul>
//    <li> <linkto class=ACG>ACG</linkto>: Additive Number Generator
//    <li> <linkto class=MLCG>MLCG</linkto>:
//         Multiplicative Linear Congruential Generator
// </ul>
// 
// Note that on the DecAlpha we use Int instead of long to implement these
// classes. The function signatures below can thus be misleading.

class RNG {
    static PrivateRNGSingleType singleMantissa;	// mantissa bit vector
    static PrivateRNGDoubleType doubleMantissa;	// mantissa bit vector
public:
    RNG();
    //
    // Return a long-words word of random bits
    //
    virtual unsigned long asLong() = 0;
    virtual void reset() = 0;
    //
    // Return random bits converted to either a float or a double
    //
    float asFloat();
    double asDouble();
};


// <summary> Additive number generator </summary>

// <synopsis>
//	Additive number generator. This method is presented in Volume II
//	of The Art of Computer Programming by Knuth. I have coded the algorithm
//	and have added the extensions by Andres Nowatzyk of CMU to randomize
//	the result of algorithm M a bit	by using an LCG & a spatial
//	permutation table.
//
//	The version presented uses the same constants for the LCG that Andres
//	uses (chosen by trial & error). The spatial permutation table is
//	the same size (it is based on word size). This is for 32-bit words.
//
//	The <src>auxillary table</src> used by the LCG table varies in size, and
//	is chosen to be the the smallest power of two which is larger than
//	twice the size of the state table.
// </synopsis>

// <h3> ACG </h3>
// 
// Class <src>ACG</src> is a variant of a Linear Congruential Generator
// (Algorithm M) described in Knuth, *Art of Computer Programming, Vol
// III*.  This result is permuted with a Fibonacci Additive Congruential
// Generator to get good independence between samples.  This is a very
// high quality random number generator, although it requires a fair
// amount of memory for each instance of the generator.
// 
// The <src>ACG::ACG</src> constructor takes two parameters: the seed and the
// size.  The seed is any number to be used as an initial seed. The
// performance of the generator depends on having a distribution of bits
// through the seed.  If you choose a number in the range of 0 to 31, a
// seed with more bits is chosen. Other values are deterministically
// modified to give a better distribution of bits.  This provides a good
// random number generator while still allowing a sequence to be repeated
// given the same initial seed.
// 
// The <src>size</src> parameter determines the size of two tables used in the
// generator. The first table is used in the Additive Generator; see the
// algorithm in Knuth for more information. In general, this table is
// <src>size</src> longwords long. The default value, used in the algorithm in
// Knuth, gives a table of 220 bytes. The table size affects the period of
// the generators; smaller values give shorter periods and larger tables
// give longer periods. The smallest table size is 7 longwords, and the
// longest is 98 longwords. The <src>size</src> parameter also determines the
// size of the table used for the Linear Congruential Generator. This value is
// chosen implicitly based on the size of the Additive Congruential
// Generator table. It is two powers of two larger than the power of two
// that is larger than <src>size</src>.  For example, if <src>size</src> is 7,
// the ACG table is 7 longwords and the LCG table is 128 longwords. Thus, the
// default size (55) requires 55 + 256 longwords, or 1244 bytes. The
// largest table requires 2440 bytes and the smallest table requires 100
// bytes.  Applications that require a large number of generators or
// applications that are not so fussy about the quality of the generator
// may elect to use the <src>MLCG</src> generator.

class ACG : public RNG {

    unsigned long initialSeed;	// used to reset generator
    int initialTableEntry;

    unsigned long *state;
    unsigned long *auxState;
    short stateSize;
    short auxSize;
    unsigned long lcgRecurr;
    short j;
    short k;

protected:

public:
    ACG(unsigned long seed = 0, int size = 55);
    virtual ~ACG();
    //
    // Return a long-words word of random bits
    //
    virtual unsigned long asLong();
    virtual void reset();
};


// <summary> Multiplicative linear congruential generator </summary>

// <synopsis>
//	Multiplicative Linear Congruential Generator.
// </synopsis>

// <h3> MLCG </h3>
// 
// The <src>MLCG</src> class implements a <em>Multiplicative Linear Congruential
// Generator</em>. In particular, it is an implementation of the double MLCG
// described in <em>Efficient and Portable Combined Random Number
// Generators</em> by Pierre L'Ecuyer, appearing in <em>Communications of the
// ACM, Vol. 31. No. 6</em>. This generator has a fairly long period, and has
// been statistically analyzed to show that it gives good inter-sample
// independence.
// 
// The <src>MLCG::MLCG</src> constructor has two parameters, both of which are
// seeds for the generator. As in the <src>MLCG</src> generator, both seeds are
// modified to give a "better" distribution of seed digits. Thus, you can
// safely use values such as <src>0</src> or <src>1</src> for the seeds.
// The <src>MLCG</src>
// generator used much less state than the <src>ACG</src> generator; only two
// longwords (8 bytes) are needed for each generator.

class MLCG : public RNG {
    long initialSeedOne;
    long initialSeedTwo;
    long seedOne;
    long seedTwo;

protected:

public:
    MLCG(long seed1 = 0, long seed2 = 1);

    // Return a long-words word of random bits
    virtual unsigned long asLong();

    virtual void reset();
    long seed1();
    void seed1(long);
    long seed2();
    void seed2(long);
    void reseed(long, long);
};

inline long
MLCG::seed1()
{
    return(seedOne);
}

inline void
MLCG::seed1(long s)
{
    initialSeedOne = s;
    reset();
}

inline long
MLCG::seed2()
{
    return(seedTwo);
}

inline void
MLCG::seed2(long s)
{
    initialSeedTwo = s;
    reset();
}

inline void
MLCG::reseed(long s1, long s2)
{
    initialSeedOne = s1;
    initialSeedTwo = s2;
    reset();
}


// <summary> Base class for random number distributions </summary>

// <synopsis>
// Base class for random number distributions.
// </synopsis>

// <h3> Random Number Generators and related classes </h3>
// 
// The two classes <linkto class=RNG><src>RNG</src></linkto> and
// <linkto class=Random><src>Random</src></linkto> are used together to
// generate a variety of random number distributions.  A distinction must be
// made between <em>random number generators</em>, implemented by class
// <src>RNG</src>, and <em>random number distributions</em>.
// A random number generator produces a
// series of randomly ordered bits.  These bits can be used directly, or
// cast to other representations, such as a floating point value.  A
// random number generator should produce a <em>uniform</em> distribution.  A
// random number distribution, on the other hand, uses the randomly
// generated bits of a generator to produce numbers from a distribution
// with specific properties.  Each instance of <src>Random</src> uses an instance
// of class <src>RNG</src> to provide the raw, uniform distribution used to
// produce the specific distribution.  Several instances of <src>Random</src>
// classes can share the same instance of <src>RNG</src>, or each instance can
// use its own copy.

// <h3> Random </h3>
// 
// A random number generator may be declared by first declaring a
// <src>RNG</src> and then a <src>Random</src>. For example,
// <src>ACG gen(10, 20); NegativeExpntl rnd (1.0, &gen);</src>
// declares an additive congruential
// generator with seed 10 and table size 20, that is used to generate
// exponentially distributed values with mean of 1.0.
// 
// The virtual member <src>Random::operator()</src> is the common way of
// extracting a random number from a particular distribution.  The base
// class, <src>Random</src> does not implement <src>operator()</src>.
// This is performed by
// each of the subclasses. Thus, given the above declaration of <src>rnd</src>,
// new random values may be obtained via, for example,
// <src>double next_exp_rand = rnd();</src>
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

class Random {
protected:
    RNG *pGenerator;
public:
    Random(RNG *generator);
    virtual double operator()() = 0;

    RNG *generator();
    void generator(RNG *p);
};


inline Random::Random(RNG *gen)
{
    pGenerator = gen;
}

inline RNG *Random::generator()
{
    return(pGenerator);
}

inline void Random::generator(RNG *p)
{
    pGenerator = p;
}


// <summary> Binomial distribution </summary>

// <synopsis>
// Binomial distribution
// </synopsis>

//# // <h3> Binomial </h3>
//# // 
// The binomial distribution models successfully drawing items from a
// pool.  The first parameter to the constructor, <src>n</src>, is the number of
// items in the pool, and the second parameter, <src>u</src>, is the probability
// of each item being successfully drawn.  The member <src>asDouble</src> returns
// the number of samples drawn from the pool.  Although it is not
// checked, it is assumed that <src>n>0</src> and <src>0 <= u <= 1</src>.
// The remaining members allow you to read and set the parameters.

class Binomial: public Random {
protected:
    int pN;
    double pU;
public:
    Binomial(int n, double u, RNG *gen);

    int n();
    int n(int xn);

    double u();
    double u(int xu);

    virtual double operator()();

};


inline Binomial::Binomial(int n, double u, RNG *gen)
: Random(gen){
  pN = n; pU = u;
}

inline int Binomial::n() { return pN; }
inline int Binomial::n(int xn) { int tmp = pN; pN = xn; return tmp; }

inline double Binomial::u() { return pU; }
inline double Binomial::u(int xu) { double tmp = pU; pU = xu; return tmp; }


// <summary> Discrete uniform distribution </summary>

// <synopsis>
// Discrete uniform distribution.
// </synopsis>

//# // <h3> DiscreteUniform </h3>
//# // 
// The <src>DiscreteUniform</src> class implements a uniform random variable
// over the closed interval ranging from <src>[low..high]</src>.  The first
// parameter to the constructor is <src>low</src>, and the second is
// <src>high</src>,
// although the order of these may be reversed.  The remaining members
// allow you to inspect and change <src>low</src> and <src>high</src>.

class DiscreteUniform: public Random {
    long pLow;
    long pHigh;
    double delta;
public:
    DiscreteUniform(long low, long high, RNG *gen);

    long low();
    long low(long x);
    long high();
    long high(long x);

    virtual double operator()();
};


inline DiscreteUniform::DiscreteUniform(long low, long high, RNG *gen)
: Random(gen)
{
    pLow = (low < high) ? low : high;
    pHigh = (low < high) ? high : low;
    delta = (pHigh - pLow) + 1;
}

inline long DiscreteUniform::low() { return pLow; }

inline long DiscreteUniform::low(long x) {
  long tmp = pLow;
  pLow = x;
  delta = (pHigh - pLow) + 1;
  return tmp;
}

inline long DiscreteUniform::high() { return pHigh; }

inline long DiscreteUniform::high(long x) {
  long tmp = pHigh;
  pHigh = x;
  delta = (pHigh - pLow) + 1;
  return tmp;
}


// <summary> Erlang distribution </summary>

// <synopsis>
// Erlang distribution
// </synopsis>

//# // <h3> Erlang </h3>
//# // 
// The <src>Erlang</src> class implements an Erlang distribution with mean
// <src>mean</src> and variance <src>variance</src>.

class Erlang: public Random {
protected:
    double pMean;
    double pVariance;
    int k;
    double a;
    void setState();
public:
    Erlang(double mean, double variance, RNG *gen);

    double mean();
    double mean(double x);
    double variance();
    double variance(double x);

    virtual double operator()();

};


inline void Erlang::setState() {
  k = int( (pMean * pMean ) / pVariance + 0.5 );
  k = (k > 0) ? k : 1;
  a = k / pMean;
}

inline Erlang::Erlang(double mean, double variance, RNG *gen) : Random(gen)
{
  pMean = mean; pVariance = variance;
  setState();
}

inline double Erlang::mean() { return pMean; }
inline double Erlang::mean(double x) {
  double tmp = pMean; pMean = x; setState(); return tmp;
};

inline double Erlang::variance() { return pVariance; }
inline double Erlang::variance(double x) {
  double tmp = pVariance; pVariance = x; setState(); return tmp;
}


// <summary> Discrete geometric distribution </summary>

// <synopsis>
// Discrete geometric distribution
// </synopsis>

//# // <h3> Geometric </h3>
//# // 
// The <src>Geometric</src> class implements a discrete geometric distribution.
// The first parameter to the constructor, <src>mean</src>, is the mean of the
// distribution.  Although it is not checked, it is assumed that
// <src>0 <= mean <= 1</src>.
// <src>Geometric()</src> returns the number of uniform random samples
// that were drawn before the sample was larger than <src>mean</src>.  This
// quantity is always greater than zero.

class Geometric: public Random {
protected:
    double pMean;
public:
    Geometric(double mean, RNG *gen);

    double mean();
    double mean(double x);

    virtual double operator()();

};


inline Geometric::Geometric(double mean, RNG *gen) : Random(gen)
{
  pMean = mean;
}


inline double Geometric::mean() { return pMean; }
inline double Geometric::mean(double x) {
  double tmp = pMean; pMean = x; return tmp;
}


// <summary> Hypergeometric distribution </summary>

// <synopsis>
// Hypergeometric distribution
// </synopsis>

//# // <h3> HyperGeometric </h3>
//# // 
// The <src>HyperGeometric</src> class implements the hypergeometric
// distribution.  The first parameter to the constructor, <src>mean</src>, is the
// mean and the second, <src>variance</src>, is the variance.  The remaining
// members allow you to inspect and change the mean and variance.

class HyperGeometric: public Random {
protected:
    double pMean;
    double pVariance;
    double pP;
    void setState();

public:
    HyperGeometric(double mean, double variance, RNG *gen);

    double mean();
    double mean(double x);
    double variance();
    double variance(double x);

    virtual double operator()();
};


inline void HyperGeometric::setState() {
  double z = pVariance / (pMean * pMean);
  pP = 0.5 * (1.0 - sqrt((z - 1.0) / ( z + 1.0 )));
}

inline HyperGeometric::HyperGeometric(double mean, double variance, RNG *gen)
: Random(gen) {
  pMean = mean; pVariance = variance;
  setState();
}

inline double HyperGeometric::mean() { return pMean; };

inline double HyperGeometric::mean(double x) {
  double t = pMean; pMean = x;
  setState(); return t;
}

inline double HyperGeometric::variance() { return pVariance; }

inline double HyperGeometric::variance(double x) {
  double t = pVariance; pVariance = x;
  setState(); return t;
}


// <summary> Normal distribution </summary>

// <synopsis>
// Normal distribution.
// </synopsis>

//# // <h3> Normal </h3>
//# // 
// The <src>Normal</src>class implements the normal distribution.  The first
// parameter to the constructor, <src>mean</src>, is the mean and the second,
// <src>variance</src>, is the variance.  The remaining members allow you to
// inspect and change the mean and variance.  The <src>LogNormal</src> class is a
// subclass of <src>Normal</src>.

class Normal: public Random {
    char haveCachedNormal;
    double cachedNormal;

protected:
    double pMean;
    double pVariance;
    double pStdDev;
    
public:
    Normal(double xmean, double xvariance, RNG *gen);
    double mean();
    double mean(double x);
    double variance();
    double variance(double x);
    virtual double operator()();
};


inline Normal::Normal(double xmean, double xvariance, RNG *gen)
: Random(gen) {
  pMean = xmean;
  pVariance = xvariance;
  pStdDev = sqrt(pVariance);
  haveCachedNormal = 0;
}

inline double Normal::mean() { return pMean; };
inline double Normal::mean(double x) {
  double t=pMean; pMean = x;
  return t;
}

inline double Normal::variance() { return pVariance; }
inline double Normal::variance(double x) {
  double t=pVariance; pVariance = x;
  pStdDev = sqrt(pVariance);
  return t;
};


// <summary> Logarithmic normal distribution </summary>

// <synopsis>
// Logarithmic normal distribution.
// </synopsis>

//# // <h3> LogNormal </h3>
//# // 
// The <src>LogNormal</src>class implements the logarithmic normal
// distribution.  The first parameter to the constructor, <src>mean</src>, is the
// mean and the second, <src>variance</src>, is the variance.  The remaining
// members allow you to inspect and change the mean and variance.  The
// <src>LogNormal</src> class is a subclass of <src>Normal</src>.

class LogNormal: public Normal {
protected:
    double logMean;
    double logVariance;
    void setState();
public:
    LogNormal(double mean, double variance, RNG *gen);
    double mean();
    double mean(double x);
    double variance();
    double variance(double x);
    virtual double operator()();
};


inline void LogNormal::setState()
{
    double m2 = logMean * logMean;
    pMean = log(m2 / sqrt(logVariance + m2) );
//# from ch@heike.informatik.uni-dortmund.de:
//# (was   pVariance = log((sqrt(logVariance + m2)/m2 )); )
    pStdDev = sqrt(log((logVariance + m2)/m2 )); 
}

inline LogNormal::LogNormal(double mean, double variance, RNG *gen)
    : Normal(mean, variance, gen)
{
    logMean = mean;
    logVariance = variance;
    setState();
}

inline double LogNormal::mean() {
    return logMean;
}

inline double LogNormal::mean(double x)
{
    double t=logMean; logMean = x; setState();
    return t;
}

inline double LogNormal::variance() {
    return logVariance;
}

inline double LogNormal::variance(double x)
{
    double t=logVariance; logVariance = x; setState();
    return t;
}


// <summary> Negative exponential distribution </summary>

// <synopsis>
// Negative exponential distribution.
// </synopsis>

//# // <h3> NegativeExpntl </h3>
//# // 
// The <src>NegativeExpntl</src> class implements the negative exponential
// distribution.  The first parameter to the constructor is the mean. 
// The remaining members allow you to inspect and change the mean.

class NegativeExpntl: public Random {
protected:
    double pMean;
public:
    NegativeExpntl(double xmean, RNG *gen);
    double mean();
    double mean(double x);

    virtual double operator()();
};


inline NegativeExpntl::NegativeExpntl(double xmean, RNG *gen)
: Random(gen) {
  pMean = xmean;
}

inline double NegativeExpntl::mean() { return pMean; }
inline double NegativeExpntl::mean(double x) {
  double t = pMean; pMean = x;
  return t;
}


// <summary> Poisson distribution </summary>

// <synopsis>
// Poisson distribution.
// </synopsis>

//# // <h3> Poisson </h3>
//# // 
// The <src>Poisson</src> class implements the poisson distribution.  The first
// parameter to the constructor is the mean.  The remaining members allow
// you to inspect and change the mean.

class Poisson: public Random {
protected:
    double pMean;
public:
    Poisson(double mean, RNG *gen);

    double mean();
    double mean(double x);

    virtual double operator()();
};


inline Poisson::Poisson(double mean, RNG *gen)
: Random(gen) {
  pMean = mean;
}

inline double Poisson::mean() { return pMean; }
inline double Poisson::mean(double x) {
  double t = pMean;
  pMean = x;
  return t;
}


// <summary> Uniform integer distribution </summary>

// <synopsis>
// RandomInteger uses a random number generator to generate an integer
// in a specified range.  By default the range is 0..1.  Since in my
// experience random numbers are often needed for a wide variety of
// ranges in the same program, this generator accepts a new low or high value
// as an argument to the asLong and operator() methods to temporarily 
// override stored values
// </synopsis>

//# // <h3> RandomInteger </h3>
//# // 
// The <src>RandomInteger</src> class is *not* a subclass of Random, but a
// stand-alone integer-oriented class that is dependent on the RNG
// classes. RandomInteger returns random integers uniformly from the
// closed interval <src>[low..high]</src>.
// The first parameter to the constructor is <src>low</src>, and the second
// is <src>high</src>, although both are optional.  The
// last argument is always a generator.  Additional members allow you to
// inspect and change <src>low</src> and <src>high</src>.
// Random integers are generated
// using <src>asInt()</src> or <src>asLong()</src>.
// Operator syntax (<src>()</src>) is also
// available as a shorthand for <src>asLong()</src>.
// Because <src>RandomInteger</src> is
// often used in simulations for which uniform random integers are
// desired over a variety of ranges,
// <src>asLong()</src> and <src>asInt</src> have <src>high</src>
// as an optional argument.  Using this optional argument produces a
// single value from the new range, but does not change the default range.

class RandomInteger 
{
 protected:
  RNG *pGenerator;
  long pLow;
  long pHigh;

  long _asLong(long, long);

 public:

       RandomInteger(long low, long high, RNG *gen);
       RandomInteger(long high, RNG *gen);
       RandomInteger(RNG *gen);

// read params

  long low() const;
  long high() const;
  RNG* generator() const;

// change params

  long low(long x);
  long high(long x);
  RNG* generator(RNG *gen);

// get a random number

  long asLong();
  long operator()(); // synonym for asLong
  int  asInt();      // (possibly) truncate as int

// override params for one shot

  long asLong(long high);
  long asLong(long low, long high);

  long operator () (long high);  // synonyms
  long operator () (long low, long high);

};


inline RandomInteger::RandomInteger(long low, long high, RNG *gen) 
  :        pGenerator(gen), 
	   pLow((low < high) ? low : high),
	   pHigh((low < high) ? high : low)
{}

inline RandomInteger::RandomInteger(long high, RNG *gen) 
  :        pGenerator(gen),
	   pLow((0 < high) ? 0 : high),
	   pHigh((0 < high) ? high : 0)
{}
  

inline RandomInteger::RandomInteger(RNG *gen) 
  :        pGenerator(gen),
	   pLow(0),
	   pHigh(1)
{}

inline RNG* RandomInteger::generator() const { return pGenerator;}
inline long RandomInteger::low() const       { return pLow; }
inline long RandomInteger::high() const      { return pHigh; }

inline RNG* RandomInteger::generator(RNG *gen) 
{
  RNG *tmp = pGenerator; pGenerator = gen;  return tmp;
}

inline long RandomInteger::low(long x)  
{
  long tmp = pLow;  pLow = x;  return tmp;
}

inline long RandomInteger:: high(long x) 
{
  long tmp = pHigh; pHigh = x; return tmp;
}

inline long RandomInteger:: _asLong(long low, long high)
{	
  return (pGenerator->asLong() % (high-low+1)) + low;
}


inline long RandomInteger:: asLong() 
{
  return _asLong(pLow, pHigh);
}

inline long RandomInteger:: asLong(long high)
{
  return _asLong(pLow, high);
}

inline long RandomInteger:: asLong(long low, long high)
{
  return _asLong(low, high);
}

inline long RandomInteger:: operator () () 
{
  return _asLong(pLow, pHigh);
}

inline long RandomInteger:: operator () (long high)
{
  return _asLong(pLow, high);
}

inline long RandomInteger:: operator () (long low, long high)
{
  return _asLong(low, high);
}




inline int RandomInteger:: asInt() 
{
  return int(asLong());
}


// <summary> Uniform distribution </summary>

// <synopsis>
// Uniform distribution
// </synopsis>

//# // <h3> Uniform </h3>
//# // 
// The <src>Uniform</src> class implements a uniform random variable over the
// open interval ranging from <src>[low..high)</src>.  The first parameter to the
// constructor is <src>low</src>, and the second is <src>high</src>, although
// the order of
// these may be reversed.  The remaining members allow you to inspect and
// change <src>low</src> and <src>high</src>.

class Uniform: public Random {
    double pLow;
    double pHigh;
    double delta;
public:
    Uniform(double low, double high, RNG *gen);

    double low();
    double low(double x);
    double high();
    double high(double x);

    virtual double operator()();
};


inline Uniform::Uniform(double low, double high, RNG *gen) : Random(gen)
{
    pLow = (low < high) ? low : high;
    pHigh = (low < high) ? high : low;
    delta = pHigh - pLow;
}

inline double Uniform::low() { return pLow; }

inline double Uniform::low(double x) {
  double tmp = pLow;
  pLow = x;
  delta = pHigh - pLow;
  return tmp;
}

inline double Uniform::high() { return pHigh; }

inline double Uniform::high(double x) {
  double tmp = pHigh;
  pHigh = x;
  delta = pHigh - pLow;
  return tmp;
}


// <summary> Weibull distribution </summary>

// <synopsis>
// Weibull distribution.
// </synopsis>

//# // <h3> Weibull </h3>
//# // 
// The <src>Weibull</src> class implements a weibull distribution with
// parameters <src>alpha</src> and <src>beta</src>.  The first parameter to the
// class constructor is <src>alpha</src>, and the second parameter is
// <src>beta</src>.  The remaining members allow you to inspect and change
// <src>alpha</src> and <src>beta</src>.

class Weibull: public Random {
protected:
    double pAlpha;
    double pInvAlpha;
    double pBeta;

    void setState();
    
public:
    Weibull(double alpha, double beta, RNG *gen);

    double alpha();
    double alpha(double x);

    double beta();
    double beta(double x);

    virtual double operator()();
};


inline void Weibull::setState() {
  pInvAlpha = 1.0 / pAlpha;
}
    
inline Weibull::Weibull(double alpha, double beta,  RNG *gen) : Random(gen)
{
  pAlpha = alpha;
  pBeta = beta;
  setState();
}

inline double Weibull::alpha() { return pAlpha; }

inline double Weibull::alpha(double x) {
  double tmp = pAlpha;
  pAlpha = x;
  setState();
  return tmp;
}

inline double Weibull::beta() { return pBeta; };
inline double Weibull::beta(double x) {
  double tmp = pBeta;
  pBeta = x;
  return tmp;
};

#if defined(__alpha__) || defined(SGI64)
#undef long
#endif

#endif

