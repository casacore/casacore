//# FluxStdSrcs.cc: Implementation of FluxStdSrcs.h
//# Copyright (C) 2013
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
#include <components/ComponentModels/FluxStdSrcs.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <casa/System/Aipsrc.h>
#include <measures/Measures/MDirection.h>
#include <tables/Tables/TableParse.h>
#include <tables/Tables/ScalarColumn.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>
#include <map>


namespace casa { //# NAMESPACE CASA - BEGIN

FluxStdSrcs::FluxStdSrcs()
{
  names_p[FSS::THREEC286] = RVS6("3C286", "3C 286", "3C_286", "1328+307", "1331+305", "J1331+3030").vector();
  directions_p[FSS::THREEC286] = MDirection(MVDirection(3.539257626070549, 0.5324850225220917),
					     MDirection::J2000);
  names_p[FSS::THREEC48]  = RVS6("3C48", "3C 48", "3C_48", "0134+329",
                                  "0137+331",              // Together these match 33d09m35s
                                  "J0137+3309").vector();  // for dd.d or ddmm with a margin.
  directions_p[FSS::THREEC48] = MDirection(MVDirection(0.4262457643630985, 0.5787463318245085),
					    MDirection::J2000);
  names_p[FSS::THREEC147] = RVS6("3C147", "3C 147", "3C_147", "0538+498", "0542+498", 
                                  "J0542+4951").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FSS::THREEC147] = MDirection(MVDirection(1.4948817765383597, 0.8700805690768509),
					     MDirection::J2000);
  names_p[FSS::THREEC138] = RVS6("3C138", "3C 138", "3C_138", "0518+165", "0521+166",
                                  "J0521+1638").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FSS::THREEC138] = MDirection(MVDirection(1.401346673041897, 0.2904130912582342),
					     MDirection::J2000);
  names_p[FSS::NINETEEN34M638] = RigidVector<String, 2>("1934-638","J1939-6342").vector();
  directions_p[FSS::NINETEEN34M638] = MDirection(MVDirection(5.146176021557448, -1.1119977478136984),
						  MDirection::J2000);
  names_p[FSS::THREEC295] = RVS6("3C295", "3C 295", "3C_295", "1409+524", "1411+522",
                                  "J1411+5212").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FSS::THREEC295] = MDirection(MVDirection(3.7146787856873478, 0.9111103509091509),
					     MDirection::J2000);
  names_p[FSS::THREEC196] = RVS6("3C196", "3C 196", "3C_196", "0809+483", "0813+482",
                                  "J0813+4813").vector();          // Jhhmm+ddmm, CAS-2020
  directions_p[FSS::THREEC196] = MDirection(MVDirection(2.1537362969610023, 0.8415541320803659),
					     MDirection::J2000);
  // added for Perley-Butler 2013 4h 37m 4.375301s 29d 40' 13.819008" CAS-4489 (other alias:B0433+2934)
  names_p[FSS::THREEC123] = RVS6("3C123", "3C 123", "3C_123", "0433+295", "0437+296",
                                  "J0437+2940").vector();
  directions_p[FSS::THREEC123] = MDirection(MVDirection(1.2089586878736391, 0.51784800786636209),
					     MDirection::J2000);
  names_p[FSS::THREEC380] = RVS6("3C380", "3C 380", "3C_380", "1828+487", "1829+487",
                                  "J1829+4845").vector();          // Jhhmm+ddm, CAS-2020
  directions_p[FSS::THREEC380] = MDirection(MVDirection(4.84123669, 0.85078092),
                                             MDirection::J2000);
  directions_p[FSS::UNKNOWN_SOURCE] = MDirection();	// Default.
}

FluxStdSrcs::~FluxStdSrcs(){ }

FluxStdSrcs::Source FluxStdSrcs::srcNameToEnum(const String& srcName, const MDirection& dir) const
{
  FSS::Source srcEnum = FSS::UNKNOWN_SOURCE;  
  String tmpSrcName(srcName);
  tmpSrcName.upcase();
  for(std::map<FSS::Source, Vector<String> >::const_iterator it = names_p.begin();
      it != names_p.end(); ++it){
    for(Int i = it->second.nelements(); i--;){
      if(tmpSrcName.contains(it->second[i])){
        srcEnum = it->first;
        break;
      }
    }
    if(srcEnum != FSS::UNKNOWN_SOURCE)
      break;
  }
  // now try cone search usng the flux calibrator table
  if(srcEnum == FSS::UNKNOWN_SOURCE) {
      LogIO os(LogOrigin("FluxStdSrcs", "srcNameToEnum"));
      String fcaldatapath;
      String tabName("fluxcalibrator.data");
      if(!Aipsrc::findDir(fcaldatapath,"./"+tabName)) {
          if(!Aipsrc::findDir(fcaldatapath, Aipsrc::aipsRoot()+"/data/nrao/VLA/standards/"+tabName)) {
              //LogIO os(LogOrigin("FluxStdSrcs", "srcNameToEnum", WHERE));
               os << LogIO::NORMAL
                  << "No flux calibrator table: " <<  tabName
                  << " ./ or in ~/data/nrao/VLA/standards/. Skip a cone search "
                  << LogIO::POST;
               return srcEnum;
          }
      }
      // input source coordinates (input dir must be in J2000)
      Quantum<Vector<Double> > radec = dir.getAngle();
      String srcRA = String::toString(radec.getValue("rad")[0])+"rad";
      String srcDec = String::toString(radec.getValue("rad")[1])+"rad";
      //String radius("0.0001454441rad"); //search radius is set to 30arcsec
      String radius("4.8481367e-06rad"); //search radius is set to 1arcsec
      String taql="select Name, RA_J2000, Dec_J2000 from "+fcaldatapath+
		 " where anycone([RA_J2000,Dec_J2000]"+",["+srcRA+","+srcDec+"],"+radius+")";
      //cerr<<"taql command="<<taql<<endl;
      os << LogIO::NORMAL
	 << "Trying to determine source match by position..."
	 << LogIO::POST;
      Table tmpsubtab = tableCommand(taql);
      if(tmpsubtab.nrow()) {
	  ROScalarColumn<String> srcNameCol(tmpsubtab,"Name");
	  String srcNameInTable = srcNameCol.getColumnCells(RefRows(0,0))[0];
	  //cerr<<"srcNameInTable="<<srcNameInTable<<endl;
	  for (std::map<FSS::Source, Vector<String> >::const_iterator it2 = names_p.begin(); 
	       it2 != names_p.end(); ++it2) {
	      if (srcNameInTable.contains(it2->second[0])) {
		  srcEnum = it2->first;
		  os << LogIO::NORMAL
		     << "Found "<<srcNameInTable
		     <<" as a match for the input field, "
		     <<srcName<<" in the cone search within radius of "<<radius
		     << LogIO::POST;
		  break;
	      }
	  }
     }
     else {
     os << LogIO::NORMAL
	<< " no match was found"
	<< LogIO::POST;
     }
  }
  return srcEnum;
}

String FluxStdSrcs::EnumToSrcName(const FSS::Source srcEnum) const
{
   return names_p.find(srcEnum)->second[0];
}

MDirection FluxStdSrcs::getDirection(const FSS::Source srcEnum) const
{
  return directions_p.find(srcEnum)->second;
}

} //# NAMESPACE CASA - END
