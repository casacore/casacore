//# WCPolygon.h: Class to define a polygonal world coordinate region
//# Copyright (C) 1998,1999,2001
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


#ifndef IMAGES_WCELLIPSOID_H
#define IMAGES_WCELLIPSOID_H

#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/images/Regions/WCRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Class to define an n-dimensional ellipsoid in world coordinates.
//
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=WCRegion>WCRegion</linkto>
//   <li> <linkto class=WCRegion>WCPolygon</linkto>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// </prerequisite>
//
// <synopsis> 
//
// </synopsis> 
//

// <motivation>
// Users must be able to specify ellipsoids in world as well as lattice
// coordinates. 
// </motivation>
//


class WCEllipsoid : public WCRegion
{
public:

	// ellipsoid with axes parallel to coordinate axes
	WCEllipsoid(
		const Vector<Quantity>& center,
		const Vector<Quantity>& radii,
        const IPosition& pixelAxes,
        const CoordinateSystem& cSys,
        const RegionType::AbsRelType absRel=RegionType::Abs
	);

	// sphere. <src>pixelAxes</src> must have the same base units
	// and those pixels musb be square or an exception is thrown.
	WCEllipsoid(
		const Vector<Quantity>& center,
		const Quantity& radius,
        const IPosition& pixelAxes,
        const CoordinateSystem& cSys,
        const RegionType::AbsRelType absRel=RegionType::Abs
	);

	// 2-D ellipse . The axes must have the same base units
	// and those pixels must be square or an exception is thrown.
	// <src>theta</src> is the angle between the <src>pixelAxis0</src> and
	// the major axis of the ellipse.
	WCEllipsoid(
		const Quantity& xcenter, const Quantity& ycenter,
		const Quantity& majorAxis, const Quantity& minorAxis,
		const Quantity& theta,
		const uInt pixelAxis0, const uInt pixelAxis1,
        const CoordinateSystem& cSys,
        const RegionType::AbsRelType absRel=RegionType::Abs
	);

	WCEllipsoid(const WCEllipsoid& that);

	WCEllipsoid& operator= (const WCEllipsoid& that);

	Bool operator== (const WCRegion& other) const;

	WCRegion* cloneRegion() const;

	Bool canExtend() const;

	String type() const;

	static String className();

	static WCEllipsoid* fromRecord(
		const TableRecord& rec,
		const String&
	);

	TableRecord toRecord(const String& tableName) const;

	LCRegion* doToLCRegion (
		const CoordinateSystem& csys,
	    const IPosition& latticeShape,
	    const IPosition& pixelAxesMap,
	    const IPosition& outOrder
	) const;

private:

	// WARN do not change the order of the members of this enum
	// or you will break backward compatibility with records previously
	// saved persistently. Add new types to the end of the enum.
	enum SpecialType {
		NOT_SPECIAL,
		SPHERE,
		ELLIPSE_2D
	};

	WCEllipsoid();

	Vector<Quantity> _center;
	Vector<Quantity> _radii;

	IPosition _pixelAxes;
	CoordinateSystem _csys;
	RegionType::AbsRelType _absRel;
	Quantity _theta;
	SpecialType _specType;

	void _checkPixelAxes() const;
	void _checkUnits() const;

	void _init();

};

} //# NAMESPACE CASACORE - END

#endif
