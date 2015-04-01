//# Fit2D.h: Class to fit 2-D objects to Lattices or Arrays
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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
//#   $Id$

#ifndef LATTICES_FIT2D_H
#define LATTICES_FIT2D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/Fitting/NonLinearFitLM.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Array;
template<class T> class Matrix;
template<class T> class Vector;
template<class T> class Lattice;
template<class T> class MaskedLattice;


// <summary>
// Fit 2-D objects to 2-D Lattices or Arrays
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Lattice>Lattice</linkto>
// </prerequisite>

// <synopsis> 
// This class allows you to fit different types of 2-D models
// to either Lattices or Arrays.  These must be 2 dimensional;
// for Lattices, the appropriate 2-D Lattice can be made with
// the SubLattice class.
//
// You may fit more than one model simultaneously to the data.
// Models are added with the addModel method.   With this method,
// you also specify the initial guesses of the parameters of
// the model.    Any parameters involving coordinates are
// expected in zero-relative absolute pixel coordinates (e.g. the centre of
// a model).  Additionally with the addModel method, 
// you may specify which parameters are to be held fixed
// during the fitting process.  This is done with the 
// parameterMask Vector which is in the same order as the
// parameter Vector.  A value of True indicates the parameter
// will be fitted for.  Presently, when you say fix the minor axis,
// you really end up fixing the axial ratio (internals).  I don't
// have a solution for this presently.
// 
// For Gaussians, the parameter Vector (input or output) consists, in order, of
// the peak, x location, y location, FWHM of major axis, FWHM of minor axis, 
// and position angle of the major axis (in radians). The 
// position angle is positive +x to +y 
// in the pixel coordinate system ([0,0] in center of image) and 
// in the range -2pi to 2pi.  When the solution is recovered, the
// position angle will be in the range 0 to pi.
//
// </synopsis> 
// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1998/12/11">
//  <li> template it 
//  <li> Speed up some Array calculations indexed with IPositions
//  <li> Don't handle Lattices simply by getting pixels into Arrays
//  <li> make an addModel interface taking functionals
// </todo>

class Fit2D 
{
public:

    // Enum describing the different models you can fit
    enum Types {
      GAUSSIAN = 0,
      DISK = 1,
      LEVEL=2,
      PLANE=3,
      nTypes
    };

    // Enum describing output error conditions
    enum ErrorTypes {
// ok
      OK = 0,
// Did not converge
      NOCONVERGE = 1,
// Solution failed
      FAILED = 2,
// There were no unmasked points
      NOGOOD = 3,
// No models set
      NOMODELS = 4,
// Number of conditions
      nErrorTypes
    };

    // Constructor
    explicit Fit2D(LogIO& logger);

    // Destructor
    ~Fit2D();

    // Copy constructor.  Uses copy semantics except for the logger
    // for which a reference copy is made
    Fit2D(const Fit2D& other);

    // Assignment operator. Uses copy semantics except for the logger
    // for which a reference copy is made
    Fit2D& operator=(const Fit2D& other);

    // Add a model to the list to be simultaneously fit and 
    // return its index.  Specify the initial guesses for
    // the model and a mask indicating whether the parameter
    // is fixed (False) during the fit or not.  Returns the
    // the model number added (0, 1, 2 etc)
    //<group>
    uInt addModel (Fit2D::Types type,
                   const Vector<Double>& parameters,
                   const Vector<Bool>& parameterMask);
    uInt addModel(Fit2D::Types type,
                   const Vector<Double>& parameters);
    //</group>

    // Convert mask from a string to a vector.  The string gives the parameters
    // to keep fixed in the fit (f (flux), x (x position), y (y position),
    // a (FWHM major axis), b (FWHM minor axis), p (position angle)
    static Vector<Bool> convertMask (const String fixedmask,
                                     Fit2D::Types type);


    // Set a pixel selection range.  When the fit is done, only
    // pixels in the specified range are included/excluded.
    // Only the last call of either of these will be active.
    //<group>
    void setIncludeRange (Double minVal, Double maxVal);
    void setExcludeRange (Double minVal, Double maxVal);
    void resetRange();
    //</group>

    // Return number of parameters for this type of model
    static uInt nParameters (Fit2D::Types type);

    // Recover number of models
    uInt nModels() const;

    // Determine an initial estimate for the solution of the specified
    // model type to the given data - no compound models are allowable
    // in this function.   If you have specified an include
    // or exclude pixel range to the fitter, that will be honoured.
    // This function does not interact with the addModel function.
    // Returns a zero length vector if it fails to make an estimate.
    //<group>
    Vector<Double> estimate(Fit2D::Types type,
			    const MaskedLattice<Float>& data);
    Vector<Double> estimate(Fit2D::Types type, const Lattice<Float>& data);
    Vector<Double> estimate(Fit2D::Types type, const Array<Float>& data);
    Vector<Double> estimate(Fit2D::Types type, const Array<Float>& data,
                            const Array<Bool>& mask);
    //</group>

    // Do the fit.  Returns an enum value to tell you what happened if the fit failed
    // for some reasons.  A message can also be found with function errorMessage if 
    // the fit was not successful.  For Array(i,j) i is x and j is y
    //<group>
    Fit2D::ErrorTypes fit(const MaskedLattice<Float>& data, 
                          const Lattice<Float>& sigma);
    Fit2D::ErrorTypes fit(const Lattice<Float>& data, 
                          const Lattice<Float>& sigma);
    Fit2D::ErrorTypes fit(const Array<Float>& data, 
                          const Array<Float>& sigma);
    Fit2D::ErrorTypes fit(const Array<Float>& data,
                          const Array<Bool>& mask, 
                          const Array<Float>& sigma);
    //</group>

    // Find the residuals to the fit. xOffset and yOffset allow one to provide a data
    // array that is offset in space from the grid that was fit. In this way, one
    // can fill out a larger image than the subimage that was fit, for example. A negative
    // value of xOffset means the supplied data array represents a grid that has a y axis left
    // of the grid of pixels that was fit. A negative yOffset value means the supplied data
    // array represents a grid that has an x axis that is below the x axis of the grid of pixels
    // that was fit.
    //<group>
    Fit2D::ErrorTypes residual(
    	Array<Float>& resid, Array<Float>& model,
    	const Array<Float>& data, Int xOffset=0, int yOffset=0
    ) const;
    Fit2D::ErrorTypes residual(Array<Float>& resid, Array<Float>& model,
                               const MaskedLattice<Float>& data);
    Fit2D::ErrorTypes residual(Array<Float>& resid, Array<Float>& model,
                               const Lattice<Float>& data);
    //</group>
    // If function fit failed, you will find a message here
    // saying why it failed
    String errorMessage () const;

    // Recover solution for either all model components or
    // a specific one.  These functions will return an empty vector
    // if there is no valid solution.    All available parameters (fixed and
    // adjustable) are included in the solution vectors.  
    //<group>
    Vector<Double> availableSolution () const;
    Vector<Double> availableSolution (uInt which) const;
    //</group>

    // The errors. All available parameters (fixed and adjustable) are 
    // included in the error vectors.  Unsolved for parameters will 
    // have error 0.
    //<group>
    Vector<Double> availableErrors() const;
    Vector<Double> availableErrors(uInt which) const;
    //</group>

    // The number of iterations that the fitter finished with
    uInt numberIterations() const;

    // The chi squared of the fit.  Returns 0 if fit has been done.
    Double chiSquared () const;

    // The number of points used for the last fit
    uInt numberPoints () const;

    // Return type as a string
    static String type(Fit2D::Types type);

    // Return string type as enum (min match)
    static Fit2D::Types type(const String& type);

    // Find type of specific model
    Fit2D::Types type(uInt which);

    // Convert p.a. (radians) from positive +x -> +y 
    // (Fit2D) to positive +y -> -x (Gaussian2D)
    static Double paToGauss2D (Double pa) {return pa - C::pi_2;};

    // Convert p.a. (radians) from positive +y -> -x
    // (Gaussian2D) to positive +x -> +y (Fit2D)
    static Double paFromGauss2D (Double pa) {return pa + C::pi_2;};

private:

   mutable LogIO itsLogger;
   Bool itsValid, itsValidSolution, itsHasSigma;
   Bool itsInclude;
   Vector<Float> itsPixelRange;
   CompoundFunction<AutoDiff<Double> > itsFunction;
   NonLinearFitLM<Double> itsFitter;
   Vector<Double> itsSolution;
   Vector<Double> itsErrors;
   Double itsChiSquared;
   String itsErrorMessage;
   uInt itsNumberPoints;
//
   Vector<uInt> itsTypeList;
//
   Fit2D::ErrorTypes fitData(const Vector<Double>& values,
                             const Matrix<Double>& pos,
                             const Vector<Double>& sigma);

// Returns available (adjustable + fixed) solution for model of
// interest and tells you where it began in the full solution vector
// Does no axial ratio nor position angle conversions from direct
// fit solution vector
// <group>
   Vector<Double> availableSolution (uInt& iStart, uInt which) const;
   Vector<Double> availableErrors (uInt& iStart, uInt which) const;
// </group>

   Vector<Double> getParams(uInt which) const;
   void setParams(const Vector<Double>& params, uInt which);

   Bool includeIt (Float value, const Vector<Float>& range, 
                   Int includeIt) const;

   Bool selectData (Matrix<Double>& pos, Vector<Double>& values,
                    Vector<Double>& weights,  const Array<Float>& pixels,
                    const Array<Bool>& mask, const Array<Float>& sigma);
   void piRange (Double& pa) const;

};

inline Bool Fit2D::includeIt (Float value, const Vector<Float>& range, 
                              Int includeIt) const
{
   if (includeIt==0) return True;
//
   if (includeIt==1) {
      if (value >= range(0) && value <= range(1)) return True;
   } else if (value < range(0) || value > range(1)) {
      return True;
   }
//
   return False;
}




} //# NAMESPACE CASACORE - END

#endif


