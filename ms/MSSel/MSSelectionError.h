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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MS_MSSELECTIONERROR_H
#define MS_MSSELECTIONERROR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  // catch all possible exceptions thrown by all of Casacore modules! 
  //</synopsis>
  
  class MSSelectionError : public AipsError {
  public:
    // The default constructor generates the message "Table error".
    MSSelectionError (Category c=GENERAL);
    // Construct with given message.
    void changeMessage(String& message);
    void addMessage(String& message);
    void reset() {message="";}
    MSSelectionError (const String& message,Category c=GENERAL);
    ~MSSelectionError () noexcept;
    Bool hasMessage;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionNullSelection : public MSSelectionError {
  public:
    MSSelectionNullSelection (const String& message, Category c=GENERAL);
    ~MSSelectionNullSelection () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionNullExpr : public MSSelectionError {
  public:
    MSSelectionNullExpr (const String& message, Category c=GENERAL);
    ~MSSelectionNullExpr () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionNullTEN : public MSSelectionError {
  public:
    MSSelectionNullTEN (const String& message, Category c=GENERAL);
    ~MSSelectionNullTEN () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionTimeError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionTimeError (const String& message,Category c=GENERAL);
    ~MSSelectionTimeError () noexcept;
  };
  
  class MSSelectionTimeParseError: public MSSelectionTimeError {
  public:
    MSSelectionTimeParseError (const String& message,Category c=GENERAL);
    ~MSSelectionTimeParseError () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionAntennaError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionAntennaError (const String& message,Category c=GENERAL);
    ~MSSelectionAntennaError () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  
  class MSSelectionAntennaParseError: public MSSelectionAntennaError {
  public:
    MSSelectionAntennaParseError (const String& message,Category c=GENERAL);
    ~MSSelectionAntennaParseError () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFieldError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionFieldError (const String& message,Category c=GENERAL);
    ~MSSelectionFieldError () noexcept;
  };
  
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFieldParseError: public MSSelectionFieldError {
  public:
    MSSelectionFieldParseError (const String& message,Category c=GENERAL);
    ~MSSelectionFieldParseError () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFieldWarning: public MSSelectionFieldError {
  public:
    MSSelectionFieldWarning (const String& message,Category c=GENERAL);
    ~MSSelectionFieldWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionUvDistError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionUvDistError (const String& message,Category c=GENERAL);
    ~MSSelectionUvDistError () noexcept;
  };
  
  class MSSelectionUvDistParseError: public MSSelectionUvDistError {
  public:
    MSSelectionUvDistParseError (const String& message,Category c=GENERAL);
    ~MSSelectionUvDistParseError () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionSpwError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionSpwError (const String& message,Category c=GENERAL);
    ~MSSelectionSpwError () noexcept;
  };
  
  class MSSelectionSpwParseError: public MSSelectionSpwError {
  public:
    MSSelectionSpwParseError (const String& message,Category c=GENERAL);
    ~MSSelectionSpwParseError () noexcept;
  };

  class MSSelectionSpwWarning: public MSSelectionSpwError {
  public:
    MSSelectionSpwWarning (const String& message,Category c=GENERAL);
    ~MSSelectionSpwWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //
  class MSSelectionScanError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionScanError (const String& message,Category c=GENERAL);
    ~MSSelectionScanError () noexcept;
  };
  
  class MSSelectionScanParseError: public MSSelectionScanError {
  public:
    MSSelectionScanParseError (const String& message,Category c=GENERAL);
    ~MSSelectionScanParseError () noexcept;
  };

  class MSSelectionScanWarning: public MSSelectionScanError {
  public:
    MSSelectionScanWarning (const String& message,Category c=GENERAL);
    ~MSSelectionScanWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionArrayError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionArrayError (const String& message,Category c=GENERAL);
    ~MSSelectionArrayError () noexcept;
  };
  
  class MSSelectionArrayParseError: public MSSelectionArrayError {
  public:
    MSSelectionArrayParseError (const String& message,Category c=GENERAL);
    ~MSSelectionArrayParseError () noexcept;
  };

  class MSSelectionArrayWarning: public MSSelectionArrayError {
  public:
    MSSelectionArrayWarning (const String& message,Category c=GENERAL);
    ~MSSelectionArrayWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionPolnError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionPolnError (const String& message,Category c=GENERAL);
    ~MSSelectionPolnError () noexcept;
  };
  
  class MSSelectionPolnParseError: public MSSelectionPolnError {
  public:
    MSSelectionPolnParseError (const String& message,Category c=GENERAL);
    ~MSSelectionPolnParseError () noexcept;
  };

  class MSSelectionPolnWarning: public MSSelectionPolnError {
  public:
    MSSelectionPolnWarning (const String& message,Category c=GENERAL);
    ~MSSelectionPolnWarning () noexcept;
  };

  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionStateError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionStateError (const String& message,Category c=GENERAL);
    ~MSSelectionStateError () noexcept;
  };
  
  class MSSelectionStateParseError: public MSSelectionStateError {
  public:
    MSSelectionStateParseError (const String& message,Category c=GENERAL);
    ~MSSelectionStateParseError () noexcept;
  };

  class MSSelectionStateWarning: public MSSelectionStateError {
  public:
    MSSelectionStateWarning (const String& message,Category c=GENERAL);
    ~MSSelectionStateWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionObservationError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionObservationError (const String& message,Category c=GENERAL);
    ~MSSelectionObservationError () noexcept;
  };
  
  class MSSelectionObservationParseError: public MSSelectionObservationError {
  public:
    MSSelectionObservationParseError (const String& message,Category c=GENERAL);
    ~MSSelectionObservationParseError () noexcept;
  };

  class MSSelectionObservationWarning: public MSSelectionObservationError {
  public:
    MSSelectionObservationWarning (const String& message,Category c=GENERAL);
    ~MSSelectionObservationWarning () noexcept;
  };
  //
  //-------------------------------------------------------------------
  //  
  class MSSelectionFeedError : public MSSelectionError {
  public:
    // Add given message to string "MSSelection time error: ".
    MSSelectionFeedError (const String& message,Category c=GENERAL);
    ~MSSelectionFeedError () noexcept;
  };
  
  class MSSelectionFeedParseError: public MSSelectionFeedError {
  public:
    MSSelectionFeedParseError (const String& message,Category c=GENERAL);
    ~MSSelectionFeedParseError () noexcept;
  };

  //
  //-------------------------------------------------------------------
  //  
  String constructMessage(const Int pos, const String& command);
} //# NAMESPACE CASACORE - END

#endif
