//# GaussianConvert.h: Class to convert units of Gaussians from pixel to world 
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_GAUSSIANCONVERT_H)
#define AIPS_GAUSSIANCONVERT_H

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <trial/Coordinates/CoordinateSystem.h>

template<class T> class Quantum;


// <summary>
// Converts Gaussian parameters between pixel and world
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=></linkto>
// </prerequisite>

// <synopsis> 
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1998/12/11">
// <li> There is some trouble with my mathematics when
//      the increments are unequal.  Get this right !
// </todo>

class GaussianConvert
{
public:

    GaussianConvert ();

    GaussianConvert (const CoordinateSystem& cSys, 
                     const Vector<uInt>& worldAxes);

   ~GaussianConvert ();

    // Copy constructor.  Uses copy semantics.
    GaussianConvert(const GaussianConvert& other);
 
    // Assignment operator. Uses copy semantics.
    GaussianConvert& operator=(const GaussianConvert& other);

    void setCoordinateSystem (const CoordinateSystem& cSys);
 
    void setWorldAxes (const Vector<uInt>& worldAxes);

    Bool toWorld(Quantum<Double>& majorAxisOut, Quantum<Double>& minorAxisOut,
                 Quantum<Double>& positionAngleOut, Double majorAxisIn,
                 Double minorAxisIn, const Quantum<Double>& positionAngleIn);

    Bool toPixel(Double& majorAxisOut, Double& minorAxisOut,
                 Quantum<Double>& positionAngleOut, const Quantum<Double>& majorAxisIn,
                 const Quantum<Double>& minorAxisIn, const Quantum<Double>& positionAngleIn);
    
    String errorMessage() const {return itsErrorMessage;}

private:

   CoordinateSystem itsCSys;
   Vector<uInt> itsWorldAxes;
   String itsErrorMessage;
   Bool itsValid;

   void convertPositionAngle(Quantum<Double>& paOut,
                             const Quantum<Double>& paIn,
                             const Vector<Double>& deltas);
};

#endif
