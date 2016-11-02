//# JsonError.h: Error class for Json
//# Copyright (C) 2016
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

#ifndef CASA_JSONERROR_H
#define CASA_JSONERROR_H

//# Includes
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore
{
  // <summary>
  // </summary>
  // <synopsis>
  // JsonError is used in case of parser errors.
  // An exception with this class is thrown. The object contains
  // the actual error message.
  // 
  // One can put a try/catch block around JsonParser::parse to
  // catch this error object and, for example, to output a message.
  // </synopsis>
  class JsonError: public AipsError
  {
  public:
    // Construct the error object with the given message.
    JsonError (const String& message);

    ~JsonError() throw();
  };

} // end namespace

#endif
