//# MeasTableMul.h: Nutation multiplication coefficient for MeasTable
//# Copyright (C) 1995-1999,2000-2004
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
//# $Id: MeasTable.h 21420 2014-03-19 09:18:51Z gervandiepen $

#ifndef MEASURES_MEASTABLEMUL_H
#define MEASURES_MEASTABLEMUL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/OS/Mutex.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class RotMatrix;
  class Euler;

  // <summary>
  // MeasTableMul provides thread-safe access to time-dependent multiplier matrices
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath" demos="">
  // </reviewed>

  // <synopsis>
  // MeasTableMul is a helper class for MeasTable to provide thread-safe
  // access to the various multiplier matrices for nutation, aberration, and
  // solar position. These matrices are dependent on the epoch.
  //
  // It is an abstract base class for specific derived classes dealing with
  // the various effects. This base class provides a cache to keep the matrices
  // for various epochs alive. The idea is that a program will process epochs
  // in order, where multiple threads can handle different epochs.
  // <br>When the cache is full, the least recently used entry is replaced by
  // the new matrix.
  //
  // The cache does not hold <src>Matrix</src> objects themselves, but a
  // <src>CountedPtr<Matrix></src> to avoid that in one thread a Matrix is
  // removed from the cache, while another thread is still using that Matrix.
  // This assumes that CountedPtr is compiled thread-safe.
  //
  // The class provides two virtual functions.
  // <ul>
  //  <li> <src>init</src> is called on the first access and makes it possible
  //       for the derived class to precompute some variables. In particular,
  //       <src>itsDefMatrix</src> should be filled with default values.
  //  <li> <src>calc</src> is called on each access and should return the
  //       matrix valid for the given epoch. Prior to calling this function,
  //       the class will copy <src>itsDefMatrix</src> to the result which
  //       also defines the shape of the result.
  //       Note that this function is only called if the matrix for the given
  //       epoch is not in the cache.
  // </ul>
  // </synopsis>
  //
  // <example>
  // Class MeasTable shows how it is used.
  // </example>

  class MeasTableMul
  {
  public:
    MeasTableMul();
    virtual ~MeasTableMul() {}
    void clear();
    CountedPtr<Matrix<Double> > getArray (Double time, Double epsilon);
    virtual void init() = 0;
    virtual void calc(Matrix<Double>&, Double time) = 0;
  protected:
    Mutex itsMutex;
    Int64 itsLastUsed;
    vector<Int64> itsUsed;
    vector<Double> itsTimes;
    vector<CountedPtr<Matrix<Double> > > itsArrays;
    Matrix<Double> itsDefArray;
  };


  // <summary>
  // Base class for standard and B1950 nutation multipliers.
  // </summary>
  class MeasTableMulSCBase: public MeasTableMul
  {
  public:
    MeasTableMulSCBase();
  protected:
    void doInit(Matrix<Double>& result,
                Polynomial<Double> poly[],
                Int nrowTD, const Long coeffTD[][5],
                Int nrowSC, const Short coeffSC[][2]);
    void doCalc(Matrix<Double>& result, Double time,
                const Polynomial<Double> poly[],
                Int nrowTD, const Long coeffTD[][5]);
  };

  // <summary>
  // Class calculating the standard nutation multipliers.
  // </summary>
  class MeasTableMulSC: public MeasTableMulSCBase
  {
  public:
    MeasTableMulSC();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[2*15];
    static const Long theirMULTD[15][5];
    static const Short theirMULSC[106][2];
  };

  // <summary>
  // Class calculating the B1950 nutation multipliers.
  // </summary>
  class MeasTableMulSC1950: public MeasTableMulSCBase
  {
  public:
    MeasTableMulSC1950();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[2*13];
    static const Long theirMULTD[13][5];
    static const Short theirMULSC[69][2];
  };


  // <summary>
  // Base class for J2000 nutation multipliers.
  // </summary>
  class MeasTableMulSC2000Base: public MeasTableMul
  {
  public:
    MeasTableMulSC2000Base();
  protected:
    void doInit(Matrix<Double>& result,
                Polynomial<Double> poly[],
                Int nrowSC, const Long coeffSC[][6]);
    void doCalc(Matrix<Double>& result, Double time,
                const Polynomial<Double> poly[],
                Int nrowSC);
  };

  // <summary>
  // Class calculating the J2000A nutation multipliers.
  // </summary>
  class MeasTableMulSC2000A: public MeasTableMulSC2000Base
  {
  public:
    MeasTableMulSC2000A();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[2*678];
    static const Long theirMULSC[678][6];
  };

  // <summary>
  // Class calculating the J2000B nutation multipliers.
  // </summary>
  class MeasTableMulSC2000B: public MeasTableMulSC2000Base
  {
  public:
    MeasTableMulSC2000B();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[2*77];
    static const Long theirMULSC[77][6];
  };


  // <summary>
  // Class calculating the standard aberration multipliers.
  // </summary>
  class MeasTableMulAber: public MeasTableMul
  {
  public:
    MeasTableMulAber();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[18];
    static const Long theirMABERTD[3][18];
    static const Short theirMABER[80][6];
  };


  // <summary>
  // Class calculating the B1950 aberration multipliers.
  // </summary>
  class MeasTableMulAber1950: public MeasTableMul
  {
  public:
    MeasTableMulAber1950();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    Polynomial<Double> itsPoly[18];
    double itsFactor;  //# AU/d
    static const Short theirMABER[130][6];
    static const Short theirABERT1T[10];
    static const Short theirABERT2T[2];
    static const Short theirABERT3T[1];
    static const Double theirABERSPEC[2][6];
  };


  // <summary>
  // Class calculating the XY solar position multipliers.
  // </summary>
  class MeasTableMulPosSunXY: public MeasTableMul
  {
  public:
    MeasTableMulPosSunXY();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    static const Double theirMPOSXY[98][4];
  };

  // <summary>
  // Class calculating the Z solar position multipliers.
  // </summary>
  class MeasTableMulPosSunZ: public MeasTableMul
  {
  public:
    MeasTableMulPosSunZ();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    static const Double theirMPOSZ[29][2];
  };


  // <summary>
  // Class calculating the XY earth position multipliers.
  // </summary>
  class MeasTableMulPosEarthXY: public MeasTableMul
  {
  public:
    MeasTableMulPosEarthXY();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    static const Double theirMPOSXY[189][4];
  };

  // <summary>
  // Class calculating the Z earth position multipliers.
  // </summary>
  class MeasTableMulPosEarthZ: public MeasTableMul
  {
  public:
    MeasTableMulPosEarthZ();
    virtual void init();
    virtual void calc(Matrix<Double>&, Double time);
  private:
    static const Double theirMPOSZ[32][2];
  };


} //# end namespace

#endif
