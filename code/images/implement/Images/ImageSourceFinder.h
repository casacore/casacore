//# ImageSourceFinder.h: find sources in image
//# Copyright (C) 1996,1997,1998,1999,2000
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

#if !defined(AIPS_IMAGESOURCEFINDER_H)
#define AIPS_IMAGESOURCEFINDER_H


//# Includes
#include <aips/aips.h>
#include <aips/Measures/Stokes.h>      
#include <trial/ComponentModels/ComponentType.h>      

//# Forward Declarations
template <class T> class ImageInterface;
template <class T> class Vector;
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
// Currently just a simple strong point source finder is available.
// </synopsis>

// <example>
// <srcBlock>
// </srcBlock>
// </example>

// <motivation>
// This complements source fitting by providing an initial estimate
// </motivation>

// <todo asof="2000/11/08">
//  Lots
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

// Find strong point sources
   Vector<SkyComponent> findPointSources (LogIO& os, Int nMax, 
                                          Double cutoff, Bool absFind);

// Set a new image
   Bool setNewImage (const ImageInterface<T>& image);

private:
   const ImageInterface<T>* pImage_p;
};

#endif
