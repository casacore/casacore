//# ImageInfo.h: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2000
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

#if !defined(AIPS_IMAGE_INFO_H)
#define AIPS_IMAGE_INFO_H

#include <aips/aips.h>
#include <aips/Utilities/RecordTransformable.h>

#include <aips/Arrays/Vector.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/String.h>

#if defined(AIPS_STDLIB)
#include <iosfwd>
#else
class ostream;
#endif

// <summary>
// Miscellaneous information related to an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=RecordTransformable>RecordTransformable</linkto>
// </prerequisite>
//
// <synopsis>
// This class is used to record information about an image.
// At present it contains the following:
// <ol>
//    <li> The restoring beam
// </ol>
// This list can easily be extended if necessary.
//
// </synopsis>
//
// <example>
// The interface is a simple get/set interface. Note that the "set" methods
// can be chained together since each set method returns a reference to its
// object (rather like cout).
// <srcblock>
//    ImageInfo ii;
//    ii.setRestoringBeam(Quantity(30,"arcsec"), Quantity(10,"arcsec"),
//                        Quantity(-18,"deg"));
//    ...
//    cout << "The restoring beam is : " << oi.restoringBeam() << endl;
// </srcblock>
// </example>
//
// <motivation>
// This sort of information needed a standard place to go with a
// standard interface so it could be moved out of MiscInfo.
// </motivation>
//
// <todo asof="1999/03/28">
//   <li> Probably add the image min and max
// </todo>

class ImageInfo : public RecordTransformable
{
public:
    ImageInfo();
    ~ImageInfo();

    // Copy constructor (copy semantics)
    ImageInfo(const ImageInfo &other);

    // Assignment (copy semantics)
    ImageInfo &operator=(const ImageInfo &other);

    // Set and get the restoring beam.  Vector beam in order
    // major axis, minor axis, position angle.
    // <group>
    Vector<Quantum<Double> > restoringBeam() const;
    ImageInfo& setRestoringBeam(const Vector<Quantum<Double> >& beam);
    ImageInfo& setRestoringBeam(const Quantum<Double>& major,
                                const Quantum<Double>& minor,
                                const Quantum<Double>& pa);
    ImageInfo& removeRestoringBeam();
    // </group>

    // Functions to interconvert between an ImageInfo and a record. These 
    // functions are inherited from class
    // <linkto class=RecordTransformable>RecordTransformable</linkto>. As new
    // fields get added to ImageInfo these functions should be augmented. Missing
    // fields should not generate an error to in fromRecord to allow for 
    // backwards compatibility - null values should be supplied instead.
    // The record field names are: "restoringbeam".
    // <group>
    virtual Bool toRecord(String & error, RecordInterface & outRecord) const;
    virtual Bool fromRecord(String & error, const RecordInterface & inRecord);
    // </group>

    // In some circumstances it might be useful to know what the defaults for
    // the various values are so you can check if they have been set.
    // The default restoring beam is a null vector.
    // <group>
    static Vector<Quantum<Double> > defaultRestoringBeam();
    // </group>

private:
    Vector<Quantum<Double> > itsRestoringBeam;

    // Common copy ctor/assignment operator code.
    void copy_other(const ImageInfo &other);
};

// <summary> Global functions </summary>
// <group name=Output>
// Output declaration - useful for debugging.
ostream &operator<<(ostream &os, const ImageInfo &info);
// </group>

#endif
