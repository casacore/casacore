//# MSSelection.cc: Implementation of MSSelection.h
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//# $Id: 
//----------------------------------------------------------------------------

#include <ms/MeasurementSets/MSAntennaGram.h>
#include <ms/MeasurementSets/MSCorrGram.h>
#include <ms/MeasurementSets/MSFieldGram.h>
#include <ms/MeasurementSets/MSSPWGram.h>
#include <ms/MeasurementSets/MSTimeGram.h>
#include <ms/MeasurementSets/MSUvDistGram.h>

#include <ms/MeasurementSets/MSMainColumns.h>
#include <measures/Measures/MeasureHolder.h>
#include <measures/Measures/MEpoch.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/DataType.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode *MSSelection::msTableExprNode = 0x0;

//----------------------------------------------------------------------------

MSSelection::MSSelection() : 
  antennaExpr_p(""), corrExpr_p(""), fieldExpr_p(""),
  spwExpr_p(""), timeExpr_p(""), uvDistExpr_p("")
{
// Default null constructor 
// Output to private data:
//    antennaExpr_p             String   Antenna STaQL expression
//    corrExpr_p                String   Correlator STaQL expression
//    fieldExpr_p               String   Field STaQL expression
//    spwExpr_p                 String   SPW STaQL expression
//    timeExpr_p                String   Time STaQL expression
//    uvDistExpr_p              String   UV Distribution STaQL expression
//
  priority_p.resize(MAX_EXPR);
}

//----------------------------------------------------------------------------

MSSelection::~MSSelection()
{
// Default desctructor
//
}

//----------------------------------------------------------------------------

MSSelection::MSSelection(const Record& selectionItem) : 
  antennaExpr_p(""), corrExpr_p(""), fieldExpr_p(""),
  spwExpr_p(""), timeExpr_p(""), uvDistExpr_p("")
{
// Construct from a record representing a selection item
// Output to private data:
//    antennaExpr_p             String   Antenna STaQL expression
//    corrExpr_p                String   Correlator STaQL expression
//    fieldExpr_p               String   Field STaQL expression
//    spwExpr_p                 String   SPW STaQL expression
//    timeExpr_p                String   Time STaQL expression
//    uvDistExpr_p              String   UV Distribution STaQL expression
//
  // Extract fields from the selection item record
  fromSelectionItem(selectionItem);
}

//----------------------------------------------------------------------------

MSSelection::MSSelection (const MSSelection& other)
{
// Copy constructor
// Input:
//    other            const MSSelection&    Existing MSSelection object
// Output to private data:
//
  if(this != &other) {
    this->antennaExpr_p = other.antennaExpr_p;
    this->corrExpr_p    = other.corrExpr_p;
    this->fieldExpr_p   = other.fieldExpr_p;
    this->spwExpr_p     = other.spwExpr_p;
    this->timeExpr_p    = other.timeExpr_p;
    this->uvDistExpr_p  = other.uvDistExpr_p;
    this->priority_p    = other.priority_p;
  }
}

//----------------------------------------------------------------------------

MSSelection& MSSelection::operator= (const MSSelection& other)
{
// Assignment operator
// Input:
//    other            const MSSelection&    RHS MSSelection object
// Output to private data:
//
  if(this != &other) {
    this->antennaExpr_p = other.antennaExpr_p;
    this->corrExpr_p    = other.corrExpr_p;
    this->fieldExpr_p   = other.fieldExpr_p;
    this->spwExpr_p     = other.spwExpr_p;
    this->timeExpr_p    = other.timeExpr_p;
    this->uvDistExpr_p  = other.uvDistExpr_p;
    this->priority_p    = other.priority_p;
  }

  return *this;
}

//----------------------------------------------------------------------------

TableExprNode MSSelection::toTableExprNode(const MeasurementSet& ms)
{
// Convert the MS selection to a TableExprNode object, 
// representing a TaQL selection in C++.
// Input:
//    ms               const MeasurementSet&     MeasurementSet to bind TaQL
// Output:
//    toTableExprNode  TableExprNode             Table expression node
//
  // Interpret all expressions in the MS selection
  //
  TableExprNode condition;

  for(unsigned int i=0; i<priority_p.ndim(); i++)
  {
    switch(priority_p[i])
    {
      case ANTENNA_EXPR:
        if(antennaExpr_p != "")
          condition = msAntennaGramParseCommand(ms, antennaExpr_p);
        break;
      case CORR_EXPR:
        if(corrExpr_p != "")
          condition = condition && msCorrGramParseCommand(ms, corrExpr_p);
        break;
      case FIELD_EXPR:
        if(fieldExpr_p != "")
          condition = condition && msFieldGramParseCommand(ms, fieldExpr_p);
        break;
      case SPW_EXPR:
        if(spwExpr_p != "")
          condition = condition && msSPWGramParseCommand(ms, spwExpr_p);
        break;
      case TIME_EXPR:
        if(timeExpr_p != "")
          condition = condition && msTimeGramParseCommand(ms, timeExpr_p);
        break;
      case UVDIST_EXPR:
        if(uvDistExpr_p != "")
          condition = condition && msUvDistGramParseCommand(ms, uvDistExpr_p);
        break;
      case NO_EXPR:
      default:
        break;
    }
  }

  return condition;
}

//----------------------------------------------------------------------------

void MSSelection::clear(void)
{
  antennaExpr_p = "";
  corrExpr_p    = "";
  fieldExpr_p   = "";
  spwExpr_p     = "";
  timeExpr_p    = "";
  uvDistExpr_p  = "";

  priority_p = Vector<Int>();
  priority_p.resize(MAX_EXPR);
}

//----------------------------------------------------------------------------

void MSSelection::setAntennaExpr(const String& antennaExpr)
{
// Set the antenna
// Input:
//    antennaExpr       const String&  Supplementary antenna expression
// Output to private data:
//    antennaExpr_p     String         Supplementary antenna expression
//
  antennaExpr_p = antennaExpr;
  priority_p = ANTENNA_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::setCorrExpr(const String& corrExpr)
{
// Set the correlator
// Input:
//    corrExpr          const String&  Supplementary correlator expression
// Output to private data:
//    corrExpr_p        String         Supplementary correlator expression
//
  corrExpr_p = corrExpr;
  priority_p = CORR_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::setFieldExpr(const String& fieldExpr)
{
// Set the field
// Input:
//    fieldExpr         const String&  Supplementary field expression
// Output to private data:
//    fieldExpr_p       String         Supplementary field expression
//
  fieldExpr_p = fieldExpr;
  priority_p = FIELD_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::setSPWExpr(const String& spwExpr)
{
// Set the SPW
// Input:
//    spwExpr           const String&  Supplementary SPW expression
// Output to private data:
//    spwExpr_p         String         Supplementary SPW expression
//
  spwExpr_p = spwExpr;
  priority_p = SPW_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::setTimeExpr(const String& timeExpr)
{
// Set the time
// Input:
//    timeExpr          const String&  Supplementary time expression
// Output to private data:
//    timeExpr_p        String         Supplementary time expression
//
  timeExpr_p = timeExpr;
  priority_p = TIME_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::setUVDistExpr(const String& uvDistExpr)
{
// Set the UV distribution
// Input:
//    uvdistExpr        const String&  Supplementary uvdist expression
// Output to private data:
//    uvdistExpr_p      String         Supplementary uvdist expression
//
  uvDistExpr_p = uvDistExpr;
  priority_p = UVDIST_EXPR;
}

//----------------------------------------------------------------------------

void MSSelection::fromSelectionItem(const Record& selectionItem)
{
// Convert from an input selection data item
// Input:
//    selectionItem     const Record&     Selection item
// Output to private data:
// 
  // Debug print statements
  cout << "MSSel::fromSI, at start, selectionItem.nfields=" << selectionItem.nfields()
       << endl;
  for (uInt fld=0; fld<selectionItem.nfields(); fld++) {
    cout << "MSSel::fromGR, fld= " << fld << ", name= "
	 << selectionItem.name(fld) << ", type= "
	 << selectionItem.type(fld) << endl;
  }
  cout << "------------------------------------------------------" << endl;

  priority_p = Vector<Int>();

  // Extract and set all expressions
  //
  // Antenna expression
  if (definedAndSet(selectionItem,"antenna")) {
    setTimeExpr(selectionItem.asString("antenna"));
    priority_p = ANTENNA_EXPR;
    cout << timeExpr_p << ", antenna" << endl;
  }

  // Correlator expression
  if (definedAndSet(selectionItem,"corr")) {
    setCorrExpr(selectionItem.asString("corr"));
    priority_p = CORR_EXPR;
    cout << corrExpr_p << ", corr" << endl;
  }

  // Field expression
  if (definedAndSet(selectionItem,"field")) {
    setFieldExpr(selectionItem.asString("field"));
    priority_p = FIELD_EXPR;
    cout << fieldExpr_p << ", field" << endl;
  }

  // SPW expression
  if (definedAndSet(selectionItem,"spw")) {
    setSPWExpr(selectionItem.asString("spw"));
    priority_p = SPW_EXPR;
    cout << spwExpr_p << ", spw" << endl;
  }

  // Time expression
  if (definedAndSet(selectionItem,"time")) {
    setTimeExpr(selectionItem.asString("time"));
    priority_p = TIME_EXPR;
    cout << timeExpr_p << ", time" << endl;
  }

  // UV Distribution expression
  if (definedAndSet(selectionItem,"uvdist")) {
    setUVDistExpr(selectionItem.asString("uvdist"));
    priority_p = UVDIST_EXPR;
    cout << uvDistExpr_p << ", uvdist" << endl;
  }
}

Bool MSSelection::definedAndSet(const Record& inpRec, const String& fieldName)
{
// Check if a record field is defined and not AIPS++ unset
// Input:
//    inpRec          const Record&     Input Record
//    fieldName       const String&     Field name
// Ouput:
//    definedAndSet   Bool              True if field defined and
//                                      not AIPS++ unset
//
  Bool retval = False;
  // Check if record field is defined
  if (inpRec.isDefined(fieldName)) {
    retval = True;
    // Now check if AIPS++ unset
//    if (inpRec.dataType(fieldName) == TpRecord) {
//      retval = !Unset::isUnset(inpRec.subRecord(fieldName));
//    };
  };
  return retval;
}
//----------------------------------------------------------------------------


} //# NAMESPACE CASA - END

