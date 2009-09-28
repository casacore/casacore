//# ProfileFit1D.h: Class to fit profiles to vectors
//# Copyright (C) 2004
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

#ifndef COMPONENTS_PROFILEFIT1D_H
#define COMPONENTS_PROFILEFIT1D_H

//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>

#include <components/SpectralComponents/SpectralList.h>
#include <components/SpectralComponents/SpectralFit.h>

namespace casa {

class SpectralElement;

// <summary>
// Fit spectral components to a Vector of data
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tProfileFit1D.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SpectralElement">SpectralElement</linkto> 
//   <li> <linkto class="SpectralList">SpectralList</linkto> 
//   <li> <linkto class="SpectralFit">SpectralFit</linkto> 
// </prerequisite>

// <synopsis> 
// Fit lists (held in class SpectralList) of SpectralElements to a 
// Vector of data.  Each SpectralElement can  be one from a variety
// of types.  The values of the parameters for each SpectralElement 
// provide the initial starting guesses for the fitting process.  
// Also, a SpectralElement object holds a mask indicating whether 
// a parameter should be held fixed or solved for.   After the 
// fitting is done, a new SpectralList holding SpectralElements with 
// the fitted parameters is created.  
//
// For all the functions that return a status Bool, True is good. If
// False is returned, an error message can be recovered with function
// <src>errorMessage</src>,  You should not proceed if False is returned.
// </synopsis> 

// <example>
// <srcblock>
// const uInt n = 512;
// Vector<Double> x(n);
// Vector<Double> y(n);
// Vector<Bool> m(n);
// 
// // Code to fill data vectors x,y,m
//
// ProfileFit1D<Float> fitter; 
// Bool ok = fitter.setData (x, y, m);
// ok = fitter.setGaussianElements (2);              
// ok = fitter.fit();       
// const SpectralList& fitList = fitter.getList(True);
// </srcblock>
// </example>

// <todo asof="2004/07/10">
//   <li> Add constraints
// </todo>
//
//
//  <note role=caution>
//  At the moment, because of templating limitations of the underlying
//  fitting classes, you must use template type Double.  This restriction
//  will be lifted in the future.
//  </note>
//

template <class T> class ProfileFit1D 
{
public:
    // Constructor
    ProfileFit1D();

    // Destructor
    ~ProfileFit1D();

    // Copy constructor.  Uses copy semantics.
    ProfileFit1D(const ProfileFit1D& other);

    // Assignment operator. Uses copy semantics.
    ProfileFit1D& operator=(const ProfileFit1D& other);

    // Set abcissa, ordinate, mask and weights. A True mask value means the 
    // data are good. If you don't specify the weights vector, all weights 
    // are assumed to be unity.  If you don't specify a mask it will be
    // created as all good.
    // Status is returned, if False, error message can be recovered with <src>errorMessage</src>
    // <group>
    Bool setData (const Vector<Double>& x, const Vector<T>& y, const Vector<Bool>& mask,
                  const Vector<Double>& weight);
    Bool setData (const Vector<Double>& x, const Vector<T>& y, const Vector<Bool>& mask);
    Bool setData (const Vector<Double>& x, const Vector<T>& y);
    // </group>

    // Set a SpectralList of SpectralElements to fit for.
    // The SpectralElements in the list hold the
    // initial estimates and must reflect the abcissa and ordinate units.  
    // They also contain the information about whether
    // specific parameters are to be held fixed or allowed to vary in
    // the fitting process.
    // You can recover the list of elements with function getList.
    void setElements (const SpectralList& list);

    // Set a SpectralList of Gaussian SpectralElements to fit for.  
    // The initial estimates for the Gaussians will be automatically determined.
    // All of the parameters created by this function will be solved for
    // by default. You can recover the list of elements with function getList.
    // Status is returned, if False, error message can be 
    // recovered with <src>errorMessage</src>
    Bool setGaussianElements (uInt nGauss);

    // Add new SpectralElement(s) to the SpectralList (can be empty)
    // of SpectralElements to be fit for.  
    //<group>
    void addElement (const SpectralElement& el);
    void addElements (const SpectralList& list);
    // </group>

    // Clear the SpectralList of elements to be fit for
    void clearList ();

    // Set range mask.  You can specify a number of ranges
    // via a vector of start indices (or X values) and a vector of end
    // indices (or X values).   When argument insideIsGood is True,
    // a mask will be created which
    // will be True (good) inside the ranges and bad (False)
    // outside of those ranges.  When argument insideIsGood is False,
    // the mask will be False (bad) inside the ranges and 
    // True (good) outside of those ranges. When the data are fit, a total
    // mask is formed combining (via a logical AND) the 
    // data mask (setData) and this range mask.
    // Status is returned, if False, error message can be recovered with <src>errorMessage</src>
    // <group>
    Bool setRangeMask (const Vector<uInt>& startIndex, 
                       const Vector<uInt>& endIndex,
                       Bool insideIsGood=True);
    Bool setRangeMask (const Vector<T>& startIndex, 
                       const Vector<T>& endIndex,
                       Bool insideIsGood=True);
    // </group>

    // Recover masks.  These are the data mask (setData) the range
    // mask (setRangeMask may be length zero) and the total 
    // mask combining the two.
    // <group>
    Vector<Bool> getDataMask() const {return itsDataMask;};
    Vector<Bool> getRangeMask() const {return itsRangeMask;}
    Vector<Bool> getTotalMask() const {return makeTotalMask();};
    // </group>

    // Do the fit and return status.  Returns convergence status.
    // Error conditions in the solution process will generate
    // an AipsError exception and you should catch these yourself.
    Bool fit ();

    // Get Chi Squared of fit
    Double getChiSquared () const {return itsFitter.chiSq();}

    // Get number of iterations for last fit
    Double getNumberIterations () const {return itsFitter.nIterations();}

    // Recover the list of elements.  You can get the elements
    // as initially estimated (fit=False), or after fitting 
    // (fit=True).  In the latter case, the SpectralElements
    // hold the parameters and errors of the fit.
    const SpectralList& getList (Bool fit=True) const;

    // Recover vectors for the estimate, fit and residual.
    // If you don't specify which element, all elements are included
    // If the Vectors are returned with zero length, it means an error
    // condition exists (e.g. asking for fit before you do one). In this
    // case an error message can be recovered with function <src>errorMessage</src>.
    //<group>
    Vector<T> getEstimate (Int which=-1) const;
    Vector<T> getFit (Int which=-1) const;
    Vector<T> getResidual (Int which=-1, Bool fit=True)  const;
    //</group>

    // Recover the error message
    String errorMessage () const {return itsError;};

private:
   Vector<Double> itsX;                    // Abcissa (really should not be type T)
   Vector<T> itsY;                         // Ordinate 
   Vector<Double> itsWeight;               // Weights 
   Vector<Bool> itsDataMask;               // Data mask
   Vector<Bool> itsRangeMask;              // Mask associated with ranges
//
   SpectralList itsList;                   // List of elements to fit for
//
   SpectralFit itsFitter;                  // Fitter
   mutable String itsError;                // Error message

// Functions
   Vector<Bool> makeTotalMask() const;
   SpectralList getSubsetList (const SpectralList& list, Int which)  const;
   void checkType() const;
   void copy(const ProfileFit1D<T>& other);
};

} //#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <components/SpectralComponents/ProfileFit1D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
