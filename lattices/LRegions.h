//# LRegions.h: Regions in a lattice.
//# Copyright (C) 1996,1997,1998,1999,2003
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
//# $Id: Lattices.h 21521 2014-12-10 08:06:42Z gervandiepen $

#ifndef LATTICES_LREGIONS_H
#define LATTICES_LREGIONS_H


//#include <casacore/lattices/LRegions/LatticeRegion.h>
//#include <casacore/lattices/LRegions/LCSlicer.h>
//#include <casacore/lattices/LRegions/LCBox.h>
//#include <casacore/lattices/LRegions/LCEllipsoid.h>
//#include <casacore/lattices/LRegions/LCPolygon.h>
//#include <casacore/lattices/LRegions/LCUnion.h>
//#include <casacore/lattices/LRegions/LCIntersection.h>
//#include <casacore/lattices/LRegions/LCDifference.h>
//#include <casacore/lattices/LRegions/LCConcatenation.h>
//#include <casacore/lattices/LRegions/LCComplement.h>
//#include <casacore/lattices/LRegions/LCExtension.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// Regions in a lattice.
// </summary>

// <prerequisite>
//   <li> module <linkto module=Lattices>Lattices</linkto>
// </prerequisite>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" demos="">
// </reviewed>

// <synopsis>
// There is a rich variety of <linkto class=LCRegion>region</linkto>
// classes which can be used to define a LatticeRegion in pixel coordinates.
// The elementary ones are:
// <ul>
//  <li> <linkto class=LCBox>box</linkto>
//  <li> <linkto class=LCEllipsoid>ellipsoid</linkto>
//  <li> <linkto class=LCPolygon>polygon</linkto>
//  <li> <linkto class=LCPixelSet>pixelset</linkto>
//  <li> <linkto class=LCPagedMask>good/bad mask</linkto>
// </ul>
// Compound region classes can be used to make a combination of one or more
// regions.
// <ul>
//  <li> <linkto class=LCUnion>union</linkto>
//  <li> <linkto class=LCIntersection>intersection</linkto>
//  <li> <linkto class=LCDifference>difference</linkto>
//  <li> <linkto class=LCConcatenation>concatenation</linkto>
//  <li> <linkto class=LCComplement>complement</linkto>
//  <li> <linkto class=LCExtension>extension</linkto>
// </ul>
// Apart from these region classes, class
// <linkto class=LCSlicer>LCSlicer</linkto> can be used to define
// a box with optional strides. It also offers the opportunity to
// define the box in fractions or to define it relative to the
// center of the lattice or relative to a reference pixel.
// <br>The final, and most general way, to define regions is by
// means of the world coordinates region classes in the
// <linkto module=Images>Images</linkto> module, in particular
// the <linkto class=WCRegion>WCRegion</linkto> class.
// However, world coordinate regions can only be used with images.
// </synopsis>

// </module>


} //# NAMESPACE CASACORE - END

#endif
