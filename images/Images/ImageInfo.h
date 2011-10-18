//# ImageInfo.h: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGEINFO_H
#define IMAGES_IMAGEINFO_H

#include <casa/aips.h>
#include <casa/Utilities/RecordTransformable.h>

#include <casa/Arrays/Vector.h>
#include <casa/Quanta/Quantum.h>
#include <casa/BasicSL/String.h>

//# Forward declarations
#include <casa/iosfwd.h>
namespace casa { //# NAMESPACE CASA - BEGIN

class LoggerHolder;

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
//    <li> A parameter describing what quantity the image holds.
//    <li> The image object name.
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
// <todo asof="2002/02/05">
//   <li> Probably add the image min and max
// </todo>

class ImageInfo : public RecordTransformable
{
public:

    // This enum defines the actual quantity being held in an image
    // It's really only used for descriptive information.
    enum ImageTypes {
       Undefined=0,
       Intensity,
       Beam,
       ColumnDensity,
       DepolarizationRatio,
       KineticTemperature,
       MagneticField,
       OpticalDepth,
       RotationMeasure,
       RotationalTemperature,
       SpectralIndex,
       Velocity,
       VelocityDispersion,
       nTypes
    };

// Default constructor

    ImageInfo();

// Destructor
    ~ImageInfo();

// Copy constructor (copy semantics)
    ImageInfo(const ImageInfo &other);

// Assignment (copy semantics)
    ImageInfo &operator=(const ImageInfo &other);

// Set and get the restoring beam.  Vector beam in order
// major axis, minor axis, position angle.
// <group>
    Vector<Quantum<Double> > restoringBeam() const;

    ImageInfo& setRestoringBeam(const Record& inRecord);
    ImageInfo& setRestoringBeam(const Vector<Quantum<Double> >& beam);
    ImageInfo& setRestoringBeam(const Quantum<Double>& major,
                                const Quantum<Double>& minor,
                                const Quantum<Double>& pa);
    ImageInfo& removeRestoringBeam();
// </group>

// Get the restoring beam from a LoggerHolder (where the history is stored)
// as AIPS writes the beam in the FITS history rather than the header keywords.  
// If there is no beam,  False is returned, and the internal state of the object
// is unchanged.
   Bool getRestoringBeam (LoggerHolder& logger);

// Set and get the Image Type.  
// <group>
    ImageInfo::ImageTypes imageType () const;
    ImageInfo& setImageType(ImageTypes type);
    static String imageType(ImageInfo::ImageTypes type);
    static ImageInfo::ImageTypes imageType(String type);
// </group>

// Set and get the Image object name
// <group>
    String objectName () const;
    ImageInfo& setObjectName (const String& object);
// </group>

// Functions to interconvert between an ImageInfo and a record. These 
// functions are inherited from class
// <linkto class=RecordTransformable>RecordTransformable</linkto>. As new
// fields get added to ImageInfo these functions should be augmented. Missing
// fields should not generate an error to in fromRecord to allow for 
// backwards compatibility - null values should be supplied instead.
// The record field names are: "restoringbeam, imagetype, objectname".
// <group>
    virtual Bool toRecord(String& error, RecordInterface& outRecord) const;
    virtual Bool fromRecord(String& error, const RecordInterface& inRecord);
// </group>

// In some circumstances it might be useful to know what the defaults for
// the various values are so you can check if they have been set.
// The default restoring beam is a null vector.
// <group>
    static Vector<Quantum<Double> > defaultRestoringBeam();
    static ImageTypes defaultImageType();
    static String defaultObjectName();
// </group>

// Functions to interconvert between an ImageInfo and FITS keywords
// (converted to a Record).    Failure of <src>fromFITS</src>
// should probably not be regarded as fatal as the default ImageInfo
// values are viable.  For each item contained
// in the ImageInfo, an attempt to decode it from FITS is made.
// If any of them fail, False is returned, but it attempts to decode
// them all.  For those that fail an error message is held in <src>error</src> 
// in the order restoring beam, and image type.
// <src>error</src> will be returned of length 0 if the return 
// value is True, else it will be length 2.
// <group>
    Bool toFITS(String & error, RecordInterface & outRecord) const;
    Bool fromFITS(Vector<String>& error, const RecordInterface & inRecord);
// </group>

// Old version
//    Bool fromFITSOld(Vector<String>& error, const RecordInterface & inRecord);



// This function takes an unofficial fitsValue found on the Stokes axis
// and returns the appropriate ImageType.  The idea is that you 
// detect the unofficial value, drop the Stokes axis, and store
// the value as an ImageType in ImageInfo.  Only values pertaining
// to beam, optical depth and spectral index are handled here.  All others
// give back Undefined.  See usage in Image FITS conversion classes.
    static ImageInfo::ImageTypes imageTypeFromFITS(Int fitsValue);

// It might be useful to know what FITS keyword names are used in to/from
// FITS so we can remove them so they won't be used more than once. The
// names are in lower case.
    static Vector<String> keywordNamesFITS();

// Convert the Miriad 'btype' strings to the ImageType.  Some 
// Miriad 'btype's are dealt with in aips++ via the Stokes
// axis (fractional_polarization, polarized_intensity, position_angle)
// and so these will return Undefined.
   static ImageInfo::ImageTypes MiriadImageType (const String& type);

private:
    Vector<Quantum<Double> > itsRestoringBeam;
    ImageInfo::ImageTypes itsImageType;
    String itsObjectName;

// Common copy ctor/assignment operator code.
    void copy_other(const ImageInfo &other);
};

// <summary> Global functions </summary>
// <group name=Output>
// Output declaration - useful for debugging.
ostream &operator<<(ostream &os, const ImageInfo &info);
// </group>



} //# NAMESPACE CASA - END

#endif




