//# ImageAttrHandler.cc: Abstract base class for an image attributes handler
//# Copyright (C) 2012
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

//# Includes
#include <casacore/images/Images/ImageAttrHandler.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore {

  ImageAttrHandler::~ImageAttrHandler()
  {
    flush();
  }

  void ImageAttrHandler::flush()
  {}

  Bool ImageAttrHandler::hasGroup (const String&)
  {
    return False;
  }

  Vector<String> ImageAttrHandler::groupNames() const
  {
    return Vector<String>();
  }

  ImageAttrGroup& ImageAttrHandler::openGroup (const String& groupName)
  {
    throw AipsError("ImageAttrHandler: openGroup " + groupName +
                    " does not exist");
  }

  ImageAttrGroup& ImageAttrHandler::createGroup (const String& groupName)
  {
    throw AipsError("ImageAttrHandler: creation of group " + groupName +
                    " cannot be done");
  }

  void ImageAttrHandler::closeGroup (const String&)
  {}

} //# NAMESPACE CASACORE - END
