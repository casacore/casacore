//# MSSelectionTools.h: Classes to hold results from antenna grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <ms/MeasurementSets/MSSelection.h>
#include <string.h>
namespace casa { //# NAMESPACE CASA - BEGIN
  //
  //----------------------------------------------------------------------------
  //
  Vector<Int> set_intersection(const Vector<Int>& v1, const Vector<Int>& v2)
  {
    Vector<Int> loc;
    Bool found=False;
    Int n1=v1.nelements(), n2=v2.nelements();

    for(Int i=0;i<n1;i++)
      {
	found=False;
	for(Int j=0;j<n2;j++) if (v2(j) == v1(i)) {found=True;break;}
	if (found)
	  {
	    loc.resize(loc.nelements()+1,True);
	    loc(loc.nelements()-1) = v1(i);
	  }
      }

    return loc;
     
  }
  //
  //----------------------------------------------------------------------------
  //
  Vector<Int> set_union(const Vector<Int>& v1, const Vector<Int>& v2)
  {
    Vector<Int> loc;
    Bool found=False;
    loc = v2;
    Int n1=v1.nelements(),n2;

    for(Int i=0;i<n1;i++)
      {
	n2=loc.nelements();
	found=False;
	for(Int j=0;j<n2;j++) if (loc(j) == v1(i)) {found=True;break;}
	if (!found)
	  {
	    loc.resize(loc.nelements()+1,True);
	    loc(loc.nelements()-1) = v1(i);
	  }
      }

    return loc;
  }
  //
  //----------------------------------------------------------------------------
  //
  Bool mssSetData(const MeasurementSet& ms, 
		  MeasurementSet& selectedMS,
		  const String& outMSName,
		  const String& timeExpr,
		  const String& antennaExpr,
		  const String& fieldExpr,
		  const String& spwExpr,
		  const String& uvDistExpr,
		  const String& taQLExpr,
		  const String& polnExpr,
		  const String& scanExpr,
		  const String& arrayExpr
		  )
  {
    //
    // Parse the various expressions and produce the accmuluated TEN
    // internally.
    //
	
    MSSelection *mss = new MSSelection(ms,MSSelection::PARSE_NOW,
				       timeExpr,antennaExpr,fieldExpr,spwExpr,
				       uvDistExpr,taQLExpr,polnExpr,scanExpr,arrayExpr);
    //
    // Apply the internal accumulated TEN to the MS and produce the
    // selected MS.  
    //
    // If the accumulated TEN is NULL, this returns False.  Else
    // return True.
    //
    Bool rstat = mss->getSelectedMS(selectedMS,outMSName);
    delete mss;
    return rstat;
  }
  //
  //----------------------------------------------------------------------------
  //
  Bool mssSetData(const MeasurementSet& ms, 
		  MeasurementSet& selectedMS,
		  Vector<Vector<Slice> >& chanSlices,
		  Vector<Vector<Slice> >& corrSlices,
		  const String& outMSName,
		  const String& timeExpr,
		  const String& antennaExpr,
		  const String& fieldExpr,
		  const String& spwExpr,
		  const String& uvDistExpr,
		  const String& taQLExpr,
		  const String& polnExpr,
		  const String& scanExpr,
		  const String& arrayExpr,
		  const Int defaultChanStep
		  )
  {
    //
    // Parse the various expressions and produce the accmuluated TEN
    // internally.
    //
	
    MSSelection *mss = new MSSelection(ms,MSSelection::PARSE_NOW,
				       timeExpr,antennaExpr,fieldExpr,spwExpr,
				       uvDistExpr,taQLExpr,polnExpr,scanExpr,arrayExpr);
    //
    // Apply the internal accumulated TEN to the MS and produce the
    // selected MS.  
    //
    // If the accumulated TEN is NULL, this returns False.  Else
    // return True.
    //
    Bool rstat = mss->getSelectedMS(selectedMS,outMSName);

    // Get in-row selection info
    mss->getChanSlices(chanSlices,&ms,defaultChanStep);
    mss->getCorrSlices(corrSlices,&ms);

    delete mss;
    return rstat;
  }
  //
  //----------------------------------------------------------------------------
  //
  String stripWhite(const String& str, Bool onlyends)
  {
    Int j0,j1;
    j0=0;j1=str.length()-1;
    if (onlyends)
      {
	while(str[j0] == ' ') j0++;
	while(str[j1] == ' ') j1--;
      }
    return str.substr(j0,j1-j0+1);
  }
  //
  //----------------------------------------------------------------------------
  //
  int tokenize(const String& str, const String& sep, Vector<String>& tokens,
	       Bool upcase)
  {
    String tmpStr(str);
    /* String::size_type tokpos,startpos=0; */
    if (upcase) tmpStr.upcase();
    char *sep_p=(char *)sep.c_str();

    char *tok=strtok((char *)tmpStr.c_str(), sep_p);
    if (tok)
      {
	tokens.resize(1);
	tokens(0)=tok;
	while((tok=strtok((char*)NULL,sep_p)))
	  {
	    tokens.resize(tokens.nelements()+1,True);
	    tokens(tokens.nelements()-1)=stripWhite(String(tok),True).c_str();
	  }
      }
    else
      {tokens.resize(1); tokens(0)=tmpStr;}
    return tokens.nelements();
    /*    
    while ((tokpos=tmpStr.index(sep,startpos)))
      {
	tokens.resize(tokens.nelements()+1,True);
	if (tokpos==String::npos)
	  tokens(tokens.nelements()-1)=tmpStr.after(startpos-1);
	else
	  tokens(tokens.nelements()-1)=tmpStr.before(sep,startpos);
	
	if (tokpos==String::npos) break;
	
	startpos=tokpos+1;
      }

    return (int)(tokens.nelements());
    */
  }
}
