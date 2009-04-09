//# AipsIOReaderWriter.cc: Implementation for reading/writing CASA region AIPSIO files.
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

#include <images/Regions/ImageRegion.h>
#include <tables/Tables/TableRecord.h>
#include <images/Regions/AipsIOReaderWriter.h>

namespace casa {
// AIPSIOREADERWRITER DEFINITIONS //

//  Static Definitions //
//static const pair<String, String> AipsIOReaderWriter::VERSION("version", "1.0");


// Non-Static Definitions //

AipsIOReaderWriter::AipsIOReaderWriter() { }

AipsIOReaderWriter::~AipsIOReaderWriter() { }


// currently only supports a single region contained within
// the file.  It would be nice to be able to have multiple
// regions in a single file.
bool AipsIOReaderWriter::read(Record& region){

    ImageRegion *leImgReg;
    read( leImgReg );
    
    try{
	// Convert the ImageRegion to a Record
	//Record * leRecord = new Record();
	region.assign( leImgReg->toRecord(String("")) );
	String comment = "Created from file: "; // pFilename_p->c_str() );
	region.define( "comment", comment );
	delete leImgReg;
    } catch(...) {
	setError( String( "An error has occurred while reading file " )
		  + *pFilename_p, True );
	return False;
    }    

    return True;
}


// currently only supports a single region contained within
// the file.  It would be nice to be able to have multiple
// regions in a single file.
bool AipsIOReaderWriter::read(const ImageRegion *region){

    try {
	// open the file
	AipsIO ios(pFilename_p->c_str(), ByteIO::Old );

	// The commented out lines really should be used, but when
	// uncommented we get exceptions thrown.  For some reason
	// AipsIO finds a type of TableRecord, then a type of RecordDesc
	// this causes Exceptions to be thrown because `the type we give
	// and the type found don't match.  This could be due to the way
	// the file is saved or some other quirk in AipsIO.
	TableRecord leTblRec;
	//ios.getstart( "TableRecord" );
	ios >> leTblRec;
	//ios.getend();
      
	//if ( regionname.length() > 0 )
	//	  region = ImageRegion::fromRecord( leTblRec, regionname );
	//else
	// TODO strip path part off and use just the tail of
	// the filename.
	region = ImageRegion::fromRecord( leTblRec, pFilename_p->c_str() );
	//delete leTblRec;
    } catch(...) {
	setError( String( "An error has occurred while reading file " )
		  + *pFilename_p, True );
	return False;
    }    

    return True;
}


Bool AipsIOReaderWriter::write(const Record& region ) const {

    // open the file
    try {
	AipsIO os( pFilename_p->c_str(), ByteIO::NewNoReplace );
	os << region;
    } catch(...) {
	setError( String( "An error has occurred while writing file " )
		  + *pFilename_p, True );
	return False;
    }
    
    return True;
}


Bool AipsIOReaderWriter::write(const ImageRegion& region ) const {

    // Convert the ImageRegion to a record and call tour
    // other write method that uses records.   
    try {
	Record * leRecord = new Record();
	leRecord->assign( region.toRecord( *pRegionName_p ) );
	write( *leRecord );
    } catch(...) {
	setError( String( "An error has occurred while writing file " )
		  + *pFilename_p, True );
	return False;
    }
    
    return True;
}


void AipsIOReaderWriter::setOptions(const Record* options ) 
{
    setError( String( "AipsIO region files do not contain any display options, no options to set." ), False );
    
    return;
}
	
 
} //# NAMESPACE CASA - END
