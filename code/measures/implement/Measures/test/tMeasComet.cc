//# tMeasComet.cc: MeasComet test
//# Copyright (C) 2000
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

//# Includes
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures.h>
#include <aips/Measures/MeasComet.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/String.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/MVRadialVelocity.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Arrays/Vector.h>
#include <strstream.h>
#include <iomanip.h>

main() {
  try {
    cout << "Test MeasComet..." << endl;
    cout << "--------------------------------------" << endl;
    
    {
      MeasComet comet("VGEO");
      cout << "Opened VGEO" << endl;
      cout << "--------------------------------------" << endl;
      cout << "Name:           " << comet.getName() << endl;
      cout << "Type:           " << 
	MDirection::showType(comet.getType()) << endl;
      cout << "Topography:     " << comet.getTopo() << endl;
      cout << "Start:          " <<
	MVTime(comet.getStart()).string(MVTime::YMD) << endl;
      cout << "End:            " <<
	MVTime(comet.getEnd()).string(MVTime::YMD) << endl;
      cout << "Entries:        " << comet.nelements() << endl;
      cout << "--------------------------------------" << endl;
      cout << "Radial velocity:" << endl;
      for (Double x=50802.75; x<50803.0625001; x += 10.0/60./24.) {
	MVRadialVelocity y;
	cout << MVTime(x).string(MVTime::YMD) << " " <<
	  comet.getRadVel(y, x) << ": " << y << endl;
      };
      cout << "--------------------------------------" << endl;
      cout << "Position:" << endl;
      for (Double x=50802.75; x<50803.0625001; x += 10.0/60./24.) {
	MVPosition y;
	cout << MVTime(x).string(MVTime::YMD) << " " <<
	  comet.get(y, x) << ": " << y << endl;
      };
      cout << "--------------------------------------" << endl;
      cout << "Disk longitude and latitude:" << endl;
      for (Double x=50802.75; x<50803.0625001; x += 10.0/60./24.) {
	MVDirection y;
	cout << MVTime(x).string(MVTime::YMD) << " " <<
	  comet.getDisk(y, x) << ": " << y << endl;
      };
      cout << "Frame and conversion:" << endl;
      MeasFrame frame;
      MEpoch epo(MVEpoch(50802.729167), MEpoch::ET);
      frame.set(epo);
      frame.set(comet);
      MPosition pos;
      MeasTable::Observatory(pos, "vla");
      frame.set(pos);
      cout << "Frame: " << frame << endl;
      MDirection::Convert cvt(MDirection::Ref(MDirection::COMET, frame),
			      MDirection::APP);
      cout << "Apparent: ";
      cout << cvt() << endl;
      cvt = MDirection::Convert(MDirection::Ref(MDirection::COMET, frame),
				MDirection::TOPO);
      cout << "Topo: ";
      cout << cvt() << endl;
      cvt = MDirection::Convert(MDirection::Ref(MDirection::COMET, frame),
				MDirection::HADEC);
      cout << "HADEC: ";
      cout << cvt() << endl;
    }

    MeasComet *cl;
    {
      cout << "--------------------------------------" << endl;
      MeasComet comet("VTOP");
      cout << "Opened VTOP" << endl;
      cout << "--------------------------------------" << endl;
      cout << "Name:           " << comet.getName() << endl;
      cout << "Type:           " << 
	MDirection::showType(comet.getType()) << endl;
      cout << "Topography:     " << comet.getTopo() << endl;
      cout << "Start:          " <<
	MVTime(comet.getStart()).string(MVTime::YMD) << endl;
      cout << "End:            " <<
	MVTime(comet.getEnd()).string(MVTime::YMD) << endl;
      cout << "Entries:        " << comet.nelements() << endl;
      cout << "--------------------------------------" << endl;
      cout << "Radial velocity:" << endl;
      for (Double x=50802.75; x<50803.0625001; x += 10.0/60./24.) {
	MVRadialVelocity y;
	cout << MVTime(x).string(MVTime::YMD) << " " <<
	  comet.getRadVel(y, x) << ": " << y << endl;
      };
      cout << "--------------------------------------" << endl;
      cl = comet.clone();
      cout << "--------------------------------------" << endl;
      cout << "Frame and conversion:" << endl;
      MeasFrame frame;
      MEpoch epo(MVEpoch(50802.729167), MEpoch::ET);
      frame.set(epo);
      frame.set(comet);
      MPosition pos;
      MeasTable::Observatory(pos, "vla");
      frame.set(pos);
      cout << "Frame: " << frame << endl;
      MDirection::Convert cvt(MDirection::Ref(MDirection::COMET, frame),
			      MDirection::APP);
      cout << "Apparent: ";
      cout << cvt() << endl;
      cvt = MDirection::Convert(MDirection::Ref(MDirection::COMET, frame),
				MDirection::TOPO);
      cout << "Topo: ";
      cout << cvt() << endl;
      cvt = MDirection::Convert(MDirection::Ref(MDirection::COMET, frame),
				MDirection::HADEC);
      cout << "HADEC: ";
      cout << cvt() << endl;
      cout << "--------------------------------------" << endl;
    }

    {
      cout << "--------------------------------------" << endl;
      cout << "Cloned VTOP" << endl;
      cout << "--------------------------------------" << endl;
      cout << "OK:             " << cl->ok() << endl;
      cout << "Name:           " << cl->getName() << endl;
      cout << "Type:           " << 
	MDirection::showType(cl->getType()) << endl;
      cout << "Topography:     " << cl->getTopo() << endl;
      cout << "Start:          " <<
	MVTime(cl->getStart()).string(MVTime::YMD) << endl;
      cout << "End:            " <<
	MVTime(cl->getEnd()).string(MVTime::YMD) << endl;
      cout << "Entries:        " << cl->nelements() << endl;
      cout << "--------------------------------------" << endl;
      cout << "Radial velocity:" << endl;
      for (Double x=50802.75; x<50802.8; x += 10.0/60./24.) {
	MVRadialVelocity y;
	cout << MVTime(x).string(MVTime::YMD) << " " <<
	  cl->getRadVel(y, x) << ": " << y << endl;
      };
      cout << "--------------------------------------" << endl;
      delete cl;
    }

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 

  try {
    cout << "--------------------------------------" << endl;
    cout << "Default VTOP" << endl;
    cout << "--------------------------------------" << endl;
    MeasComet comet;
    cout << "OK:             " << comet.ok() << endl;
    cout << "Name:           " << comet.getName() << endl;
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  exit(0);
}
