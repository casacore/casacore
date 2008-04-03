//# ImageSourceFinder.h: find sources in image
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGESOURCEFINDER_H
#define IMAGES_IMAGESOURCEFINDER_H


//# Includes
#include <casa/aips.h>
#include <measures/Measures/Stokes.h>      
#include <components/ComponentModels/ComponentType.h>      

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Vector;
class ComponentList;
class SkyComponent;
class LogIO;


// <summary>
// Provides functionality to find sources in an image
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto module=Coordinates>Coordinates</linkto> 
// </prerequisite>

// <etymology>
// It finds sources.
// </etymology>


// <synopsis>
// This class provides methods to find sources in an image.
// 
// The finding procedes in two stages.  First, strong point sources
// are found via an efficient algorithm producing POINT components. 
// If you wish, you can further request a Gaussian fit to these 
// found point sources and then return the parameters of the 
// fit (as a GAUSIAN component).
// </synopsis>

// <example>
// <srcBlock>
// </srcBlock>
// </example>

// <motivation>
// This complements source fitting by providing an initial estimate
// </motivation>

// <todo asof="2000/11/08">
// </todo>
 

template <class T> class ImageSourceFinder
{
public:
// Constructor
   ImageSourceFinder (const ImageInterface<T>&);

// Copy constructor
   ImageSourceFinder (const ImageSourceFinder<T> &other);

// Destructor
  ~ImageSourceFinder();

// Assignment operator
   ImageSourceFinder<T> &operator=(const ImageSourceFinder<T> &other);

// Find strong sources.  nMax specifies the maximum number of sources to find.
// cutoff is the fractional cutoff (of peak) and soiurces below this limit
// will not be found. If absFind is True, only positive sources are found, else
// positive and negative are found. If doPoint=True, the returned components
// are of type POINT.  If doPoint=False, the position and shape information is 
// returned via a Gaussian fit (and components will be of
// type GAUSSIAN) to the point sources initially found.    The parameter width
// specifies the half-width of a square grid of pixels centered on the initial
// point source location to be used in the fit.  If you set doPoint=False and width=0,
// a default width of 3 and position angle 0 is returned in the GAUSSIAN component.
// Because  the flux of the component is integrated, this rough shape influences the 
// flux values as well.  
   ComponentList findSources (LogIO& os, Int nMax, Double cutoff=0.1, 
                              Bool absFind=True, Bool doPoint=True,
                              Int width=4);

// Find one source in sky plane.  Exception if no sky
   SkyComponent findSourceInSky (LogIO& os, Vector<Double>& absPixel,
                                 Double cutoff=0.1, Bool absFind=True, 
                                 Bool doPoint=True, Int width=4);

// Set a new image
   Bool setNewImage (const ImageInterface<T>& image);

private:
   const ImageInterface<T>* pImage_p;

// Find strong (point) sources
   ComponentList findSources (LogIO& os, const ImageInterface<T>& image,
                              Int nMax, 
                              Double cutoff, Bool absFind, Bool doPoint,
                              Int width);
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageSourceFinder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
