//# AipsIOReaderWriter.h: Implementation for reading/writing CASA AipsIO region files produced by the viewer.
//# Copyright (C) 2008
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

#ifndef IMAGES_AIPSIOREADERWRITER_H
#define IMAGES_AIPSIOREADERWRITER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/RFReaderWriter.h>
#include <casacore/images/Regions/ImageRegion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Implementation of CASA region AipsIO file reader and writer
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=RFReaderWriter>RFReaderWriter</linkto>
// </prerequisite>
//
// <synopsis> 
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// To provide a class for reading and writing ImageRegion class records that
// have been stored into an AipsIO file.
// </motivation>
//
//# <todo asof="1998/05/20">
//# </todo>

class AipsIOReaderWriter : public RFReader, public RFWriter
{
public:
    // Constructor.
    AipsIOReaderWriter();
    
    // Destructor.
    ~AipsIOReaderWriter();
    
    // RSFileReader methods //

    //<group>
    // Implements RSFileReader::read.
    bool read(Record& region);
    bool read(ImageRegion*& region);
    //</group>


    // RSFileWriter methods //
    
    // Implements RSFileWriter::setOptions.
    void setOptions(const Record* options);

    //<group>
    // Implements RSFileWriter::write
    bool write(const Record& region) const;
    bool write(const ImageRegion& region) const;
    //</group>
};

} //# end namespace

#endif
