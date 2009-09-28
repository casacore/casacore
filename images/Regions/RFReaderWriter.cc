//# RFReaderWriter.cc: Interfaces for classes that read/write image regions.
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
#include <images/Regions/RFReaderWriter.h>
#include <images/Regions/AipsIOReaderWriter.h>
    //#include <images/Regions/DS9ReaderWriter.h>

namespace casa {

// RFERROR DEFINITIONS //

RFError::RFError() : fatal_p(false) { }

RFError::RFError(const String& error, bool isFatal) : error_p(error),
     fatal_p(isFatal) { }

RFError::~RFError() { }

bool RFError::isFatal() const { return fatal_p; }

const String& RFError::error() const { return error_p; }

void RFError::set(const String& error, bool isFatal) {
    error_p = error;
    fatal_p = isFatal;
}


// RFREADERWRITER DEFINITIONS //

// Non-Static Methods //

void RFReaderWriter::setFile(const String& filename) {
    pFilename_p = new String( filename.c_str() );
}

void RFReaderWriter::setName(const String& regionName) {
    pRegionName_p = new String( regionName );
};


const RFError& RFReaderWriter::lastError() const{ return lastError_p; }

void RFReaderWriter::setError(const String& error, bool isFatal) const {
    const_cast<RFError&>(lastError_p).set(error, isFatal);
}


// Static Methods //

RFReaderWriter::SupportedType RFReaderWriter::supportedTypes(String t) {
    t.downcase();
    if(t == "aips-box" )     return AIPS_BOX;
    else if(t == "ds9")      return DS9;
    else if(t == "casa-xml") return CASA_XML;
    else if(t == "aips-io")  return AIPS_IO;
    
    else return DS9; // default
}

String RFReaderWriter::supportedTypes(SupportedType type) {
    switch(type) {
	case AIPS_BOX: return "AIPS-BOX";
	case DS9:      return "DS9";
	case CASA_XML: return "CASA-XML";
	case AIPS_IO:  return "AIPS-IO";
    
	default: return ""; // unknown
    }
}

String RFReaderWriter::extensionForType(SupportedType type) {
    switch(type) {
	case AIPS_BOX:	return "";
	case DS9:       return "reg";
	case CASA_XML:  return "xml";
	case AIPS_IO:   return "rgn";
        
	default: return ""; // unknown    
    }
}

Vector<RFReaderWriter::SupportedType> RFReaderWriter::supportedTypes(){
    vector<SupportedType> v(2);
    v[0] = AIPS_BOX; v[1] = DS9; v[2] = CASA_XML; v[3] = AIPS_IO;

    return v;
}

Vector<String> RFReaderWriter::supportedTypeStrings() {
    Vector<SupportedType> types = supportedTypes();
    Vector<String> v(types.size());
    for(unsigned int i = 0; i < v.size(); i++) v[i] = supportedTypes(types[i]);
    return v;
}

RFReader* RFReaderWriter::readerForType(SupportedType type) {
    switch(type) {
	//case AIPS_BOX  return new AipsBoxReaderWriter();
	case AIPS_IO:  return new AipsIOReaderWriter();
	//case DS9:      return new DS9ReaderWriter();
	//case CASA_XML: return new XMLReaderWriter();
    
	default: return NULL; // unknown
    }
}

RFWriter* RFReaderWriter::writerForType(SupportedType type) {
    switch(type) {
	//case AIPS_BOX: return new AipsBoxReaderWriter();
	case AIPS_IO:  return new AipsIOReaderWriter();
        //case DS9:      return new DS9ReaderWriter();
	//case CASA_XML: return new XMLFileReaderWriter();
    
	default: return NULL; // unknown
    }
}

}
