//# tBaseline.cc: This program test Baseline and UVW functions
//# Copyright (C) 1998
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
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <trial/Measures/MBaseline.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

main()
{
    try {
      	cout << "Test measure class MBaseline" << endl;
	cout << "--------------------------------------" << endl;


	MEpoch tbm(Quantity(50927.92931, "d"));
	MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
				 -3200483.75028), 
		      MPosition::ITRF);
	MeasFrame mf(tbm, pos);
	MVBaseline mvb0(100 ,10, 0);
	cout << "Baseline: " << mvb0 << endl;

	MBaseline::Ref mbref0(MBaseline::ITRF, mf);
	MBaseline mb0(mvb0, mbref0);

	cout << "Baseline: " << mb0 << endl;
	cout << "Baseline reference: " << mbref0 << endl;

	MBaseline::Ref mbref1(MBaseline::J2000);

	cout << "Baseline reference: " << mbref1 << endl;

	cout << "Test Baseline conversion ..." << endl;
	cout << "--------------------------------------" << endl;
	MBaseline::Convert bconv(mb0, mbref1);
	cout << "Converted " << mb0 << endl <<
	  " to " << mbref1 << endl <<
	  " as " << bconv() << endl;

	MBaseline::Convert bconvb(mbref1, mbref0);
	cout << "Back      " << bconv() << endl <<
	  " as " << bconvb(bconv()) << endl;

	Vector<Double> tvec(3);
	tvec = 0.0;
	for (uInt i=MBaseline::ITRF; i<MBaseline::N_Types; i++) {
	  for (uInt j=MBaseline::ITRF; j<MBaseline::N_Types; j++) {
	    MBaseline::Ref rin(i, mf);
	    MBaseline::Ref rout(j, mf);
	    MBaseline mb0(mvb0, rin);
	    MBaseline::Convert forw(rin, rout);
	    MBaseline::Convert backw(rout, rin);
	    if (!allNearAbs(mb0.getValue().getValue().ac() -
			 backw(forw(mb0)).getValue().getValue().ac(), 
			 tvec.ac(), 1e-5)) {
	      cout << MBaseline::showType(i) << " to " <<
		MBaseline::showType(j) << ": " <<
		mb0.getValue().getValue().ac() -
		backw(forw(mb0)).getValue().getValue().ac() << endl;
	    };
	  };
	};
 	{
	  MVDirection mvd0(0.5, 0.5, 0.5);
	  for (uInt i=MDirection::J2000; i<MDirection::N_Types; i++) {
	    for (uInt j=MDirection::J2000; j<MDirection::N_Types; j++) {
	      MDirection::Ref rin(i, mf);
	      MDirection::Ref rout(j, mf);
	      MDirection mb0(mvd0, rin);
	      MDirection::Convert forw(rin, rout);
	      MDirection::Convert backw(rout, rin);
	      if (!allNearAbs(mb0.getValue().getValue().ac() -
			      backw(forw(mb0)).getValue().getValue().ac(), 
			      tvec.ac(), 1e-5)) {
		cout << MDirection::showType(i) << " to " <<
		  MDirection::showType(j) << ": " <<
		  mb0.getValue().getValue().ac() -
		  backw(forw(mb0)).getValue().getValue().ac() << endl;
	      };
	    };
	  };
	}
       
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

    exit(0);
}
