//# LCSlicer.h: Class to define a rectangular box of interest with strides
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

#ifndef LATTICES_LCSLICER_H
#define LATTICES_LCSLICER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class Slicer;
class TableRecord;


// <summary>
// Class to define a rectangular box of interest with strides.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Slicer>Slicer</linkto>
// </prerequisite>

// <synopsis> 
// The LCSlicer makes it possible to define a rectangular box
// with strides. Note that this class is not derived from
// <linkto class=LCRegion>LCRegion</linkto>, so it cannot be used in
// a compound region object like <linkto class=LCUnion>LCUnion</linkto>.
// The reason is that strides make it impossible to use a region
// in a compound.
// <br>
// The slicer region can be defined from an
// <linkto class=Slicer>Slicer</linkto> object defining the blc/trc
// and a vector (of the same length) containing the strides.
// The LCSlicer can be of any type (thus relative, fractional, unspecified),
// while the strides can be defined as a number or a fraction.
// <br>
// It is also possible to construct it directly from a
// <linkto class=Slicer>Slicer</linkto> object, 
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCSlicer
{
public:
    LCSlicer();

    // Construct a slicer from the blc, trc, and stride (default 1).
    // The vectors can be different in lengths. The longest determines
    // the dimensionality of the region. The shorter ones get padded
    // with default values.
    // <br> For each axis (or all axes) it can be defined if the blc/trc are
    // given as pixel coordinates or as fractional values between 0 and 1. In the
    // latter case the true pixel coordinate is derived from the image shape.
    // <br> Also the region type can be given, if needed per axis.
    // <ul>
    // <li> RegionType::Abs is absolute
    // <li> RegionType::RelRef is relative to reference pixel given in toSlice().
    // <li> RegionType::RelCen is relative to image center.
    // </ul>
    // <group>
    LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
	      Bool fractionalBlcTrc = False,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
	      const Vector<Float>& inc, Bool fractionalBlcTrc = False,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
	      const Vector<Float>& inc,
	      const Vector<Bool>& fractionalBlc,
	      const Vector<Bool>& fractionalTrc,
	      const Vector<Bool>& fractionalInc,
	      const Vector<Int>& absRelBlc,
	      const Vector<Int>& absRelTrc);
    LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
	      Bool fractionalBlcTrc = False,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
	      const Vector<Double>& inc, Bool fractionalBlcTrc = False,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
	      const Vector<Double>& inc,
	      const Vector<Bool>& fractionalBlc,
	      const Vector<Bool>& fractionalTrc,
	      const Vector<Bool>& fractionalInc,
	      const Vector<Int>& absRelBlc,
	      const Vector<Int>& absRelTrc);
    LCSlicer (const Slicer& slicer);
    LCSlicer (const IPosition& blc, const IPosition& trc,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const IPosition& blc, const IPosition& trc,
	      const IPosition& inc,
	      RegionType::AbsRelType = RegionType::Abs);
    LCSlicer (const IPosition& blc, const IPosition& trc,
	      const IPosition& inc,
	      const Vector<Int>& absRelBlc,
	      const Vector<Int>& absRelTrc);
    // </group>

    // Copy constructor (reference semantics).
    LCSlicer (const LCSlicer& other);

    ~LCSlicer();

    // Assignment (copy semantics).
    LCSlicer& operator= (const LCSlicer& other);

    // Test for equality.
    // True is returned when the given region is a slicer with exactly
    // the same specification as this slicer.
    // It does not compare the comment.
    // <group>
    Bool operator== (const LCSlicer& other) const;
    Bool operator!= (const LCSlicer& other) const;
    // </group>

    // The region is completely specified if it is absolute, not fractional,
    // and has no unspecified values.
    Bool isComplete() const;

    // Get the dimensionality of the region.
    uInt ndim() const;

    // Simple accessor functions.
    // <group>
    const Vector<Float>& blc() const;
    const Vector<Float>& trc() const;
    const Vector<Float>& inc() const;
    Bool isFractional() const;
    Bool isAbsolute() const;
    Bool isUnspecified() const;
    Bool isStrided() const;
    // </group>

    // Get the class name (to store in the record).
    static String className();

    // Get the region type. Returns className().
    String type() const;

    // Get or set the comment.
    // <group>
    const String& comment() const;
    void setComment (const String& comment);
    // </group>

    // Make the region complete using the given reference pixel
    // and shape. It returns a new region where the relative regions
    // are made absolute by translating them with respect to the
    // reference pixel. Furthermore unspecified values are filled
    // in and fractional values are turned into absolute ones.
    // <group>
    Slicer toSlicer (const IPosition& referencePixel,
		     const IPosition& latticeShape) const;
    Slicer toSlicer (const Vector<Double>& referencePixel,
		     const IPosition& latticeShape) const;
    Slicer toSlicer (const Vector<Float>& referencePixel,
		     const IPosition& newLatticeShape) const;
    // </group>

    // Convert the object to a record.
    TableRecord toRecord (const String& tableName) const;

    // Convert to correct object from a record.
    static LCSlicer* fromRecord (const TableRecord&,
				 const String& tablename);

private:
    // Fill the pixel based flags from the general ones.
    void fillFlags (Bool fractional, Int absRel,
		    uInt nrblc, uInt nrtrc, uInt nrinc);

    // Fill the vectors from the values given as doubles.
    void fillFromDouble (const Vector<Double>& blc,
			 const Vector<Double>& trc,
			 const Vector<Double>& inc);

    // Fill the vectors from the values given as IPositions.
    void fillFromIPosition (const IPosition& blc,
			    const IPosition& trc,
			    const IPosition& inc);

    // Fill the remaining variables.
    // It also adjust the lengths of the vectors if they are different.
    // Check if everything is given correctly.
    void fill();

    //# Variables
    Vector<Float> itsBlc;
    Vector<Float> itsTrc;
    Vector<Float> itsInc;
    Vector<Bool>  itsFracBlc;
    Vector<Bool>  itsFracTrc;
    Vector<Bool>  itsFracInc;
    Vector<Int>   itsAbsRelBlc;
    Vector<Int>   itsAbsRelTrc;
    Bool itsIsFractional;
    Bool itsIsAbsolute;
    Bool itsIsUnspecified;
    Bool itsIsStrided;
    String itsComment;
};


inline Bool LCSlicer::operator!= (const LCSlicer& other) const
{
    return  (! operator==(other));
}
inline uInt LCSlicer::ndim() const
{
    return itsBlc.nelements();
}
inline const Vector<Float>& LCSlicer::blc() const
{
    return itsBlc;
}
inline const Vector<Float>& LCSlicer::trc() const
{
    return itsTrc;
}
inline const Vector<Float>& LCSlicer::inc() const
{
    return itsInc;
}
inline Bool LCSlicer::isFractional() const
{
    return itsIsFractional;
}
inline Bool LCSlicer::isAbsolute() const
{
    return itsIsAbsolute;
}
inline Bool LCSlicer::isUnspecified() const
{
    return itsIsUnspecified;
}
inline Bool LCSlicer::isStrided() const
{
    return itsIsStrided;
}
inline const String& LCSlicer::comment() const
{
    return itsComment;
}
inline void LCSlicer::setComment (const String& comment)
{
    itsComment = comment;
}



} //# NAMESPACE CASACORE - END

#endif
