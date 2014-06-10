//# MSSelectionErrorHandler.cc: Error handler for the MSSelection classes
//# Copyright (C) 1994,1995,1996,1997,2000
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

#include <ms/MeasurementSets/MSSelectionErrorHandler.h>
#include <ms/MeasurementSets/MSSelectionError.h>
#include <casa/Arrays/Vector.h>
#include <vector>

namespace casa { //# NAMESPACE CASA - BEGIN

  MSSelectionErrorHandler::MSSelectionErrorHandler()
    :tokenList(), messageList()
  {
  }

  MSSelectionErrorHandler::~MSSelectionErrorHandler () {}
  
  void MSSelectionErrorHandler::reportError(const char *token,const String message)
  {
    if (token!=NULL) tokenList.push_back(token);
    messageList.push_back(message);
  }

  String MSSelectionErrorHandler::constructMessage()
  {
    ostringstream Mesg;
    if (messageList.size() > 0)
      {
	Mesg << messageList[0];
	if (tokenList.size() > 0)
	  for (uInt i=0;i<tokenList.size();i++) Mesg << tokenList[i] << " ";
	else
	  for (uInt i=1;i<messageList.size(); i++) Mesg << endl << messageList[i];
      }
    String casaMesg(Mesg.str());
    return casaMesg;
  }

  void MSSelectionErrorHandler::handleError(MSSelectionError& mssErrorType) 
  {
    if (messageList.size() > 0)
      {
	String mesg(constructMessage());
	mssErrorType.addMessage(mesg);
	throw(mssErrorType);
	// LogIO logIO;
	// logIO << mssErrorType.getMesg() << LogIO::WARN;
      }
  }

  void MSSelectionErrorHandler::reset()
  {
    tokenList.resize(0);
    messageList.resize(0);
  }

} //# NAMESPACE CASA - END

