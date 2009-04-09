//# RegionFileReaderWriter.h: Interfaces for classes that read/write image regions.
//# Copyright (C) 2009
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
#ifndef RFREADERWRITER_H_
#define RFREADERWRITER_H_

//# Includes
#include <casa/Logging/LogIO.h>
#include <casa/Containers/Record.h>
#include <coordinates/Coordinates/CoordinateSystem.h>

namespace casa {//# NAMESPACE CASA - BEGIN 

class RFReader;
class RFWriter;
 
// <summary>
// Convenience class for a String/bool pair.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <author>
// <li> Laura Glendening, original author
// <li> Shannon Jaeger, Relocated from display tool to image regions.
// </author>
// 
// <prerequisite>
// <li>
// </prerequesite>
//
// <synopsis>
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
//
//# <todo asof="2009/03/10">
//# <li>
//# </todo> 
class RFError {
public:
    // Constructor, blank error.
    RFError();
    
    // Constructor, error with the given text and isFatal flag.
    RFError(const String& error, bool isFatal = false);
    
    // Destructor.
    ~RFError();
    
    
    // Returns whether this error was fatal or not.
    bool isFatal() const;
    
    // Returns this error's text.
    const String& error() const;

    // Sets the error.
    void set(const String& error, bool isFatal = false);
    
private:
    String   error_p;
    bool     fatal_p;
};


// <summary>
// Superclass for readers and writers containing common definitions and
// operations.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <author>
// <li> Laura Glendening, original author
// <li> Shannon Jaeger, Relocated from display tool to image regions.
// </author>
// 
// <prerequisite>
// <li>
// </prerequesite>
//
// <synopsis>
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
//
//# <todo asof="2009/03/10">
//# <li>
//# </todo> 
class RFReaderWriter {
public:
    // Public Static Methods //
    
    // An enum of all known subclasses/formats supported.
    enum SupportedType {
        AIPS_BOX, DS9, CASA_XML, AIPS_IO
    };
    
    // Converts between enum and String for SupportedType.
    // <group>
    static SupportedType supportedTypes(String type);
    static String supportedTypes(SupportedType type);
    // </group>

    // Returns the file extension for the given SupportedType.
    static String extensionForType(SupportedType type);

    // Returns all known SupportedTypes.
    // <group>
    static Vector<SupportedType> supportedTypes();
    static Vector<String> supportedTypeStrings();
    // </group>

    // Returns an appropriate child RFReader class for the given
    // SupportedType, or NULL for an error (shouldn't happen).
    static RFReader* readerForType(SupportedType type);
    
    // Returns an new appropriate child RfWriter class for the given
    // SupportedType, or NULL for an error (shouldn't happen).
    static RFWriter* writerForType(SupportedType type);
    
    // Returns an new appropriate options widget for the given SupportedType,
    // or NULL for an error (shouldn't happen).
    static Record* optionsWidgetForType(SupportedType type);
    
    
    // Non-Static Members //
    
    // Constructor.
    RFReaderWriter() { }
    
    // Destructor.
    virtual ~RFReaderWriter() { }
    
    // Sets the file to be read/written to the given.
    virtual void setFile(const String& filename);


    // Sets the region name associated withe the file to be read or written.
    virtual void setName(const String& regionName);
    
    
    // Returns the last error set during read/write.
    virtual const RFError& lastError() const;
    
protected:
    // Filename to be read/written.
    String *pFilename_p;

    // Name to be assigned to the region
    String *pRegionName_p;
    
    // Last error seen during read/write.
    RFError lastError_p;

    // Record containg plotting options for the regions
    Record   options_p;

    // Convenience method for setting last error during read/write.
    virtual void setError(const String& error, bool fatal = false) const;
};


// <summary>
// Abstract superclass for any class that reads a format that produces
// Regions from a file.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <author>
// <li> Laura Glendening, original author
// <li> Shannon Jaeger, Relocated from display tool to image regions.
// </author>
// 
// <prerequisite>
// <li>
// </prerequesite>
//
// <synopsis>
// Provide a well defined set of operations for reading
// region files, regardless of the data format.
//
// Note that some file formats allow for plotting options
// to be defined as well as the regions. These options are
// read and stored in a record of ... , the contents
// of this record is ill-definted (ie. there is no standard).
// 
// There may come a time where a standard is necessary.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
//
//# <todo asof="2009/03/10">
//# <li>
//# </todo> 
class RFReader : public virtual RFReaderWriter {
public:
    // Constructor.
    RFReader() { }

    // Destructor.
    virtual ~RFReader() { }

    // Provides access to the plotting options that
    // were found in the region file.
    virtual Record* options() {
	return &options_p;
    };
    
    // reported, false otherwise.  If false is returned, the details can be
    // found using lastError().  Any valid Regions that were read from the
    // file are placed in the given vector (which is cleared first).
    virtual bool read(Record& region) = 0;
    

    // Calls setFile() then read().
    virtual bool readFile(const String& file, Record& region) {
        setFile(file);
        return read(region);
    }
};


// <summary>
// Abstract superclass for any class that writes Regions to a region
// file format.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <author>
// <li> Laura Glendening, original author
// <li> Shannon Jaeger, Relocated from display tool to image regions.
// </author>
//
// <prerequisite>
// <li>
// </prerequesite>
//
// <synopsis>
// Provide a well defined set of operations that all
// region file writers must contain regardless of the
// file format of the file being saved. .
//
// Note that some file formats allow for plotting options
// to be stored with the region information. The setOptions
// method allows the user to supply this information. 
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
//
//# <todo asof="2009/03/10">
//# <li>
//# </todo> 
class RFWriter : public virtual RFReaderWriter {
public:
    // Constructor.
    RFWriter() { }
    
    // Destructor.
    virtual ~RFWriter() { }    
    
    // Sets the optional to the values. These values are related to
    // the drawing of regions and not defining the regions themselves.
    // For example, the colour to draw the region as.
    virtual void setOptions(const Record* options) {
	options_p.defineRecord( "regionoptions", *options );
    };
    

    // Write the given regions to the filename set with setFile and returns
    // true if no errors were reported, false otherwise.  If false is returned,
    // the details can be found using lastError().
    virtual bool write(const Record& region) const = 0;    
    
    // Calls setFile then write.
    virtual bool writeFile(const String& filename,
                           const Record& regions) {
        setFile(filename);
        return write(regions);
    }

   };
}

#endif /*RFREADERWRITER_H_*/
