//# MSSelectionError.h: MSSelection error classes
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef MS_MSSELECTIONERROR_H
#define MS_MSSELECTIONERROR_H

//# Includes
#include <casa/aips.h>
#include <casa/Exceptions/Error.h>


namespace casa { //# NAMESPACE CASA - BEGIN

  //# This header file defines the error classes thrown by the
  //# MSSelection and related classes.


  // <summary>
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>

  // <synopsis> 
  // The top-level generic MSSelection exception class.  All
  // exceptions thrown by the MSSelection and related classes are
  // derived from this.  Catching this class will catch only MSSelection
  // exceptions (but all exceptions from the MSSelection line of
  // classes).  To catch more specific MSSelection exceptions, catch
  // the derived classes.  Note that you have to catch AipsError to
  // catch all possible exceptions thrown by all of AIPS++ modules! 
  //</synopsis>
  
  class MSSelectionError : public AipsError {
  public:
    // The default constructor generates the message "Table error".
    MSSelectionError (Category c=GENERAL);
    // Construct with given message.
    void changeMessage(String& message);
    void addMessage(String& message);
    void reset() {message="";};
    MSSelectionError (const String& message,Category c=GENERAL);
    ~MSSelectionError () throw();
    Bool hasMessage;
  };
  //
  //-------------------------------------------------------------------
  //  
  // <summary>
  // Error thrown by MSTimeXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  class MSSelectionTimeError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionTimeError (const String& message,Category c=GENERAL);
    ~MSSelectionTimeError () throw();
  };
  
  class MSSelectionTimeParseError: public MSSelectionTimeError {
  public:
    MSSelectionTimeParseError (const String& message,Category c=GENERAL);
    ~MSSelectionTimeParseError () throw();
  };
  //
  //-------------------------------------------------------------------
  //  

  // <summary>
  // Error thrown by MSAntennaXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  class MSSelectionAntennaError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionAntennaError (const String& message,Category c=GENERAL);
    ~MSSelectionAntennaError () throw();
  };
  //
  //-------------------------------------------------------------------
  //  
  
  class MSSelectionAntennaParseError: public MSSelectionAntennaError {
  public:
    MSSelectionAntennaParseError (const String& message,Category c=GENERAL);
    ~MSSelectionAntennaParseError () throw();
  };

  // <summary>
  // Error thrown by MSFieldXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFieldError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionFieldError (const String& message,Category c=GENERAL);
    ~MSSelectionFieldError () throw();
  };
  
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFieldParseError: public MSSelectionFieldError {
  public:
    MSSelectionFieldParseError (const String& message,Category c=GENERAL);
    ~MSSelectionFieldParseError () throw();
  };

  // <summary>
  // Error thrown by MSUvDistXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionUvDistError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionUvDistError (const String& message,Category c=GENERAL);
    ~MSSelectionUvDistError () throw();
  };
  
  class MSSelectionUvDistParseError: public MSSelectionUvDistError {
  public:
    MSSelectionUvDistParseError (const String& message,Category c=GENERAL);
    ~MSSelectionUvDistParseError () throw();
  };

  //
  //-------------------------------------------------------------------
  //  
  // <summary>
  // Error thrown by MSSpwXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  class MSSelectionSpwError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionSpwError (const String& message,Category c=GENERAL);
    ~MSSelectionSpwError () throw();
  };
  
  class MSSelectionSpwParseError: public MSSelectionSpwError {
  public:
    MSSelectionSpwParseError (const String& message,Category c=GENERAL);
    ~MSSelectionSpwParseError () throw();
  };

  class MSSelectionSpwWarning: public MSSelectionSpwError {
  public:
    MSSelectionSpwWarning (const String& message,Category c=GENERAL);
    ~MSSelectionSpwWarning () throw();
  };
  //
  //-------------------------------------------------------------------
  //  
  // <summary>
  // Error thrown by MSScanXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  class MSSelectionScanError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionScanError (const String& message,Category c=GENERAL);
    ~MSSelectionScanError () throw();
  };
  
  class MSSelectionScanParseError: public MSSelectionScanError {
  public:
    MSSelectionScanParseError (const String& message,Category c=GENERAL);
    ~MSSelectionScanParseError () throw();
  };

  class MSSelectionScanWarning: public MSSelectionScanError {
  public:
    MSSelectionScanWarning (const String& message,Category c=GENERAL);
    ~MSSelectionScanWarning () throw();
  };


  //  String constructMessage(const Int pos, const String& command);
  //
  //-------------------------------------------------------------------
  //  
  // <summary>
  // Error thrown by MSArrayXXXX classes.
  // </summary>
  // <use visibility=export>
  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>
  
  // <synopsis> 
  // </synopsis> 
  
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionArrayError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionArrayError (const String& message,Category c=GENERAL);
    ~MSSelectionArrayError () throw();
  };
  
  class MSSelectionArrayParseError: public MSSelectionArrayError {
  public:
    MSSelectionArrayParseError (const String& message,Category c=GENERAL);
    ~MSSelectionArrayParseError () throw();
  };

  class MSSelectionArrayWarning: public MSSelectionArrayError {
  public:
    MSSelectionArrayWarning (const String& message,Category c=GENERAL);
    ~MSSelectionArrayWarning () throw();
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionPolnError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionPolnError (const String& message,Category c=GENERAL);
    ~MSSelectionPolnError () throw();
  };
  
  class MSSelectionPolnParseError: public MSSelectionPolnError {
  public:
    MSSelectionPolnParseError (const String& message,Category c=GENERAL);
    ~MSSelectionPolnParseError () throw();
  };

  class MSSelectionPolnWarning: public MSSelectionPolnError {
  public:
    MSSelectionPolnWarning (const String& message,Category c=GENERAL);
    ~MSSelectionPolnWarning () throw();
  };

  //
  //-------------------------------------------------------------------
  //  

  String constructMessage(const Int pos, const String& command);
} //# NAMESPACE CASA - END

#endif
