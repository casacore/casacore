//# MeasuresProxy.h: This class gives a high-level interface to Measures
//# Copyright (C) 2006
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef MEASURES_MEASURESPROXY_H
#define MEASURES_MEASURESPROXY_H

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/BasicSL/String.h>

//# Forward declarations
namespace casacore { //# NAMESPACE CASACORE - BEGIN
class String;
class MeasureHolder;
class MeasComet;

class MeasuresProxy
{
public:
  MeasuresProxy();
  virtual ~MeasuresProxy();
  Record measure(const Record& rec, const String& str, const Record& form);
  Bool doframe(const Record& rec);
  String dirshow(const Record& rec);
  Record doptorv(const Record& rec, const String& str);
  Record doptofreq(const Record& rec, const String& str,
		   const Quantity& form);
  Record todop(const Record& rec, const Quantity& form);
  Record torest(const Record& rec, const Record& form);
  Vector<String> obslist();
  Vector<String> srclist();
  Vector<String> linelist();
  Record observatory(const String& str);
  Record source(const String& str);
  Record line(const String& str);
  Record alltyp(const Record& rec);
  Quantum<Vector<Double> > posangle(const Record& lrec, const Record& rrec);
  Quantum<Vector<Double> > separation(const Record& lrec, const Record& rrec);
  Record uvw(const Record& mhrec);
  Record expand(const Record& mhrec);

private:
  String vec2str(const Vector<String>& lst);
  Bool doFrame(const MeasureHolder &in);
  Bool doFrame(const String &in);
  Bool makeMeasure(String &error, MeasureHolder &out,
		   const MeasureHolder &in, const String &outref,
		   const Record &off);
  Bool toUvw(String &error, MeasureHolder &out,
		       Vector<Double> &xyz, Vector<Double> &dot,
		       const MeasureHolder &in);
  Bool expandIt(String &error, MeasureHolder &out,
			Vector<Double> &xyz,
			const MeasureHolder &in);
  MeasureHolder rec2mh(const Record& rec);
  Record mh2rec(const MeasureHolder& mh);

  static String getMeasureType(const Record &in);

  //# Data
  // The globally used MeasFrame for this DO
  MeasFrame frame_p;
  // The current comet class
  MeasComet *pcomet_p;
  
};

} //# NAMESPACE CASACORE - END

#endif
