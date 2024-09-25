//# MSSpwErrorHandler.h: Error handler class for SPW parser
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MS_MSSPWERRORHANDLER_H
#define MS_MSSPWERRORHANDLER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# This header file defines the error handler class for the
  //# SPW parser in MSSelection module


  // <summary>
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>

  // <synopsis> 
  //
  // Specialization of the MSSelectionErrorHandler for SPW parser.
  // The constructMessage() and handleError() methods are specialized
  // here.
  //
  //</synopsis>
  
  class MSSSpwErrorHandler: public MSSelectionErrorHandler 
  {
  public:
    // The default constructor generates the message "Table error".
    MSSSpwErrorHandler():MSSelectionErrorHandler() {};
    virtual ~MSSSpwErrorHandler () {};
    
    String constructMessage();
    void handleError(MSSelectionError& mssErrorType) ;
  };
} //# NAMESPACE CASACORE - END


#endif
