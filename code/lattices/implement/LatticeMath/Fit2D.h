//# Fit2D.h: Class to fit 2-D objects to Lattices or Arrays
//# Copyright (C) 1997,1998,1999
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

#if !defined(AIPS_FIT2D_H)
#define AIPS_FIT2D_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/SumFunction.h>
#include <aips/Functionals/Gaussian2D.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>
#include <trial/Fitting.h>
#include <aips/Logging.h>

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
//   <li> <linkto class=></linkto>
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
// will be fitted for.
// 
// For Gaussians, the parameter Vector consists, in order, of
// the peak, x location, y location, major axis, minor axis,
// and position angle of the major axis (in radians,
// positive counter clockwise from the y axis in the range
// -2pi to 2pi).  When the solution is recovered, the
// position angle will be in the range 0 to pi (this is consistent
// behaviour with the Gaussian2D functional).
//
// </synopsis> 
// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1998/12/11">
//  <li> Return error estimates
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
      LEVEL = 0,
      DISK = 1,
      GAUSSIAN = 2,
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
    // is fixed (False) during the fit or not.
    //<group>
    uInt addModel (Fit2D::Types type,
                   const Vector<Double>& parameters,
                   const Vector<Bool>& parameterMask);
    uInt addModel(Fit2D::Types type,
                   const Vector<Double>& parameters);
    //</group>

    // Convert mask from a string to a vector.  The string gives the parameters
    // to keep fixed in the fit (f (flux), x (x position), y (y position),
    // a (major axis), b (minor axis), p (position angle)
    static Vector<Bool> convertMask (const String fixedmask,
                                     Fit2D::Types type);

    // Return number of parameters for this type of model
    static uInt nParameters (Fit2D::Types type);

    // Recover number of models
    uInt nModels() const;

    // Do the fit.  The internal normalization, if invoked,
    // may help round off for many parameter fits. Returns
    // an enum value to tell you what happened if the fit failed
    // for some reasons.  A message can also be found with 
    // function errorMessage if the fit was not successful.
    //<group>
    Fit2D::ErrorTypes fit(const MaskedLattice<Float>& data, 
                          const Lattice<Float>& sigma ,
                          Bool norm=True);
    Fit2D::ErrorTypes fit(const Lattice<Float>& data, 
                          const Lattice<Float>& sigma, 
                          Bool norm=True);
    Fit2D::ErrorTypes fit(const Array<Float>& data, 
                          const Array<Float>& sigma, 
                          Bool norm=True);
    Fit2D::ErrorTypes fit(const Array<Float>& data,
                          const Array<Bool>& mask, 
                          const Array<Float>& sigma, 
                          Bool norm=True);
    //</group>

    // Find the residuals to the fit.   
    //<group>
    Fit2D::ErrorTypes residual(Array<Float>& resid,
                               const Array<Float>& data);
    Fit2D::ErrorTypes residual(Array<Float>& resid,
                               const MaskedLattice<Float>& data);
    Fit2D::ErrorTypes residual(Array<Float>& resid,
                               const Lattice<Float>& data);
    //</group>

    // Set a pixel selection range.  When the fit is done, only
    // pixels in the specified range are included/excluded.
    // Only the last call of either of these will be active.
    //<group>
    void setIncludeRange (Double minVal, Double maxVal);
    void setExcludeRange (Double minVal, Double maxVal);
    //</group>

    // If function fit failed, you will find a message here
    // saying why it failed
    String errorMessage () const;

    // Recover solution for either all model components or
    // a specific one.  These functions will return an empty vector
    // if there is no valid solution.    All available parameters (fixed or
    // adjustable) are included in the solution vectors.  
    //<group>
    Vector<Double> availableSolution ();
    Vector<Double> availableSolution (uInt which);
    //</group>

    // The number of iterations that the fitter finished with
    uInt numberIterations() const;

    // The chi squared of the fit.  Returns 0 if fit has been done.
    Double chiSquared () const;

    // The number of points used for the last fit
    uInt numberPoints () const;

    // Return type as a string
    static String type(Fit2D::Types type);

    // Find type of specific model
    Fit2D::Types type(uInt which);

private:

   LogIO itsLogger;

   Bool itsValid, itsValidSolution, itsIsNormalized, itsHasSigma;
   Bool itsInclude;
   Vector<Float> itsPixelRange;
   SumFunction<AutoDiff<Double>,AutoDiff<Double> > itsFunction;
   NonLinearFitLM<Double> itsFitter;
   Vector<Double> itsSolution;
   Double itsChiSquared;
   String itsErrorMessage;
   uInt itsNumberPoints;
//
   Vector<uInt> itsTypeList;
   Double itsNormVal;
   Double itsNormPos;
//
   

//
   Fit2D::ErrorTypes fit(const Vector<Double>& values, const Matrix<Double>& pos,
                         const Vector<Double>& sigma);

   Vector<Double> getAvailableSolution() const;
   Vector<Double> getSolution(uInt& iStart, uInt which);

   Vector<Double> getParams(uInt which) const;
   void setParams(const Vector<Double>& params, uInt which);

   Bool includeIt (Float value, const Vector<Float>& range, 
                   Int includeIt) const;

   Bool normalizeData (Matrix<Double>& pos, Vector<Double>& values,
                       Vector<Double>& weights,  const Array<Float>& pixels,
                       const Array<Bool>& mask, const Array<Float>& sigma);

   void normalizeModels (uInt direction);

   void normalizeSolution();

   void normalize (Double& value, Double& x, Double& y, 
                   Double& width, Double posNorm,
                   Double valNorm) const;

   void unNormalize (Double& value, Double& x, Double& y, 
                     Double& width, Double posNorm,
                     Double valNorm) const;
};

inline void Fit2D::normalize (Double& value, Double& x, Double& y, 
                              Double& width, Double posNorm, 
                              Double valNorm) const
{  
   x = x/posNorm;
   y = y/posNorm;
   width = width/posNorm;
   value = value/valNorm;
}

inline void Fit2D::unNormalize (Double& value, Double& x, Double& y, 
                                Double& width, Double posNorm, 
                                Double valNorm) const
{  
   x = x*posNorm;
   y = y*posNorm;
   width = width*posNorm;
   value = value*valNorm;
}

inline Bool Fit2D::includeIt (Float value, const Vector<Float>& range, 
                              Int includeIt) const
{
   if (includeIt==0) return True;
//
   if (includeIt==1) {
      if (value >= range(0) && value <= range(1)) return True;
      return False;
   } else {
      if (value < range(0) || value > range(1)) return True;
      return False;
   }
//
// Shut comiler up
//
   return False;
}



#endif


