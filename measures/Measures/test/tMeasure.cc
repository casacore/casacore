//# tMeasure.cc: This program test Measure functions
//# Copyright (C) 1995-2000,2002,2007
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
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/measures/Measures.h>
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
      	cout << "Test measure class MVAngle" << endl;
	cout << "--------------------------------------" << endl;

	MVAngle mva1 = Quantity(190,"deg") + Quantity(59,"'") +
	    Quantity(59.95,"\"");
	cout << "Degrees:  " << mva1.degree() << endl;
	cout << "Radians:  " << mva1.radian() << endl;
	cout << "Fraction: " << mva1.circle() << endl;
	cout << "Degrees:  " << mva1().degree() << endl;
	cout << "Radians:  " << mva1().radian() << endl;
	cout << "Fraction: " << mva1().circle() << endl;
	cout << "Degrees:  " << mva1(+2).degree() << endl;
	cout << "Degrees:  " << mva1(-2).degree() << endl;
	cout << "Fraction: " << mva1(0).circle() << endl;
	cout << "Prec =2=: " << mva1.string(2) << endl;
	cout << "Prec =4=: " << mva1.string(4) << endl;
	cout << "Prec =6=: " << mva1.string(6) << endl;
	cout << "Prec =8=: " << mva1.string(8) << endl;
	cout << "Clean=2=: " << mva1.string(MVAngle::ANGLE_CLEAN, 2) << endl;
	cout << "Time = =: " << mva1.string(MVAngle::TIME) << endl;
	cout << "CNO_H= =: " << mva1.string(MVAngle::TIME_CLEAN_NO_H) << endl;

	MVTime mtim = 44362.6;
	cout << "Days:       " << mtim.day() << endl;    
	cout << "Hours:      " << mtim.hour() << endl;    
	cout << "Seconds:    " << mtim.second() << endl;    
	cout << "Minutes:    " << mtim.minute() << endl;    
	cout << "Prec =6=:   " << mtim.string(6) << endl;
	cout << "Day:        " << mtim.string((MVTime::formatTypes)
					      (MVTime::TIME | MVTime::DAY),
					      6) << endl;
	cout << "YMD:        " << mtim.string(MVTime::YMD, 6) << endl;
	cout << "DMY:        " << mtim.string(MVTime::DMY, 6) << endl;
	cout << "Day+DMY:    " << mtim.string((MVTime::formatTypes)
					      (MVTime::DMY | MVTime::DAY),
					      6) << endl;
	cout << "Day+YMD:    " << mtim.string((MVTime::formatTypes)
					      (MVTime::YMD | MVTime::DAY),
					      6) << endl;
	cout << "Weekday:    " << mtim.weekday() << endl;
	cout << "yyyy mm dd: " << mtim.year() << " " <<
	  mtim.month() << " " <<
	  mtim.monthday() << endl;
	cout << "yyyymmdd:   " << mtim.ymd() << endl;
	cout << "Yearday:    " << mtim.yearday() << endl;
	cout << "Yearweek:   " << mtim.yearweek() << endl;
	cout << "Week 960101:" << MVTime(1996,1,1).yearweek() << endl;
	cout << "Read it back:" << endl;
	mtim = MVTime(1980,5,3.6);
	cout << "Prec =6=:   " << mtim.string(6) << endl;
	cout << "Day:        " << mtim.string((MVTime::formatTypes)
					      (MVTime::TIME | MVTime::DAY),
					      6) << endl;
	cout << "YMD:        " << mtim.string(MVTime::YMD, 6) << endl;
	cout << "DMY:        " << mtim.string(MVTime::DMY, 6) << endl;
	cout << "Day+DMY:    " << mtim.string((MVTime::formatTypes)
					      (MVTime::DMY | MVTime::DAY),
					      6) << endl;
	cout << "Day+YMD:    " << mtim.string((MVTime::formatTypes)
					      (MVTime::YMD | MVTime::DAY),
					      6) << endl;
	cout << "Day (NO_T): " << mtim.string((MVTime::formatTypes)
					      (MVTime::TIME | MVTime::DAY |
					       MVTime::NO_TIME),
					      6) << endl;
	cout << "Day+YMD:    " << mtim.string((MVTime::formatTypes)
					      (MVTime::YMD | MVTime::DAY |
					       MVTime::NO_TIME),
					      6) << endl;
	cout << "Day+YMD:    " << mtim.string((MVTime::YMD | MVTime::DAY |
					       MVTime::NO_TIME),
					      6) << endl;

	cout << "\nTest measure class ..." << endl;
	cout << "--------------------------------------" << endl;

	cout << endl << "MDirection state transition matrix:\n" << endl;
	cout << MCDirection::showState() << endl;

	cout << endl << "MPosition state transition matrix:\n" << endl;
	cout << MCPosition::showState() << endl;

	cout << endl << "MEpoch state transition matrix:\n" << endl;
	cout << MCEpoch::showState() << endl;

	cout << endl << "MFrequency state transition matrix:\n" << endl;
	cout << MCFrequency::showState() << endl;

	cout << endl << "MRadialVelocity state transition matrix:\n" << endl;
	cout << MCRadialVelocity::showState() << endl;

	cout << endl << "MDoppler state transition matrix:\n" << endl;
	cout << MCDoppler::showState() << endl;

	MEpoch tbm(Quantity(MeasData::MJDB1950,"d"),MEpoch::Ref());
	cout << "Epoch B1950: " << tbm << endl;

	MEpoch::Ref tmref(MEpoch::TAI);
	MEpoch tm(Quantity(MeasData::MJD2000,"d"), tmref);

	cout << "Epoch 2000: " << tm << endl;
	cout << "Epoch reference: " << tmref << endl;

	MEpoch::Ref tbmref(MEpoch::TAI,tbm);
	MEpoch tm2(Quantity(MeasData::MJD2000-MeasData::MJDB1950,"d"),
		   tbmref);

	cout << "Epoch 2000 ref B1950: " << tm2 << endl;
	cout << "Epoch reference: " << tbmref << endl;

	cout << "Test measure conversion ..." << endl;
	cout << "--------------------------------------" << endl;
	MEpoch::Convert tconv(tm2,tmref);
	cout << "Converted " << tm2 << endl <<
	    " to " << tmref << endl <<
		" as " << tconv() << endl;
	MEpoch::Ref turef(MEpoch::UTC);
	MEpoch::Convert tconv2(tm2,turef);
	cout << "Converted " << tm2 << endl <<
	    " to " << turef << endl <<
		" as " << tconv2() << endl;

	MEpoch::Ref tgsref(MEpoch::UT1);
	MEpoch tm3(Quantity(50082.0,"d"),
		   tgsref);
	MEpoch tm4(Quantity(50082.72315521759259259259,"d"),
		   tgsref);
	MEpoch tm5(Quantity(44238.0,"d"),
		   tgsref);
	MEpoch::Ref tlsref(MEpoch::GMST1);
	MEpoch::Convert tconv3(tm3,tlsref);
	cout << "Converted " << tm3 << endl <<
	    " to " << tlsref << endl <<
		" as " << tconv3() << endl;
	cout << "Converted " << tm4 << endl <<
	    " to " << tlsref << endl <<
		" as " << tconv3(50082.72315521759259259259) << endl;
	cout << "Converted " << tm5 << endl <<
	    " to " << tlsref << endl <<
		" as " << tconv3(44238.0) << endl;
	MEpoch tm6(tconv3(44238.0));
	MEpoch::Convert tconv4(tm6,tgsref);
	cout << "Converted back " << tm6 << endl <<
	    " to " << tgsref << endl <<
		" as " << tconv4() << endl;

	MEpoch::Ref tasref(MEpoch::GAST);
	MEpoch::Convert tconv5(tm3,tasref);
	cout << "Converted " << tm3 << endl <<
	    " to " << tasref << endl <<
		" as " << tconv5() << endl;
	MEpoch tm7(tconv5());
	MEpoch::Convert tconv6(tm7,tgsref);
	cout << "Converted back " << tm7 << endl <<
	    " to " << tgsref << endl <<
		" as " << tconv6() << endl;

    {	
	MeasFrame b1900((MEpoch(Quantity(MeasData::MJDB1900,"d"))));
	MDirection lsr1900(Quantity(270,"deg"),
			   Quantity(30,"deg"),
			   MDirection::Ref(MDirection::BMEAN,
					   b1900));
	cout << "LSR (B1900): " << 
	    lsr1900.getValue().getAngle("deg") << endl;
	cout << "LSR (B1950): " << 
	    MDirection::Convert(lsr1900, MDirection::B1950)()
		.getValue().getAngle("deg") << endl;
	cout << "LSR (J2000): " << 
	    MDirection::Convert(lsr1900, MDirection::J2000)()
		.getValue().getAngle("deg") << endl;
	Vector<Double> vlsr1900(lsr1900.getValue().getValue());
	if (nearAbs(vlsr1900(0), 0.0)) vlsr1900(0) = 0;
	cout << "LSR (B1900): " << 
	    vlsr1900 << endl;
	cout << "LSR (B1950): " << 
	    MDirection::Convert(lsr1900, MDirection::B1950)()
		.getValue() << endl;
	cout << "LSR (J2000): " << 
	    MDirection::Convert(lsr1900, MDirection::J2000)()
		.getValue() << endl;
	MeasFrame flsr1900(lsr1900);
	// Next one precision problems with cos(90 deg) in Linux
	//	cout << "LSR frame: " << flsr1900 << endl;
    }

    {
	MDirection dirJ2000(Quantity(0,"deg"),
			    Quantity(30,"deg"),
			    MDirection::J2000);
	MEpoch ep(Quantity(50083.,"d"));
	MeasFrame frame(dirJ2000, ep);
	MRadialVelocity rvBary(Quantity(1000.,"km/s"),
			       MRadialVelocity::Ref
			       (MRadialVelocity::BARY,
				frame));
	cout << "Radial velocity (BARY): " << rvBary << endl <<
	    "                 (GEO): " <<
		MRadialVelocity::Convert(rvBary,
					 MRadialVelocity::GEO)()
					     .getValue() << endl;
	MRadialVelocity rvGeo = MRadialVelocity::Convert
	    (rvBary,
	     MRadialVelocity::GEO)();
	rvGeo.set(MRadialVelocity::Ref(MRadialVelocity::GEO, frame));
	cout << "and back: " <<
	    MRadialVelocity::Convert(rvGeo,
				     MRadialVelocity::BARY)()
					 .getValue() << endl;
	MPosition obs(Quantity(0,"m"),
		      Quantity(-289375.79, Unit('"')),
		      Quantity(50,"deg"));
	frame.set(obs);
	rvGeo.set(MVRadialVelocity(0.0));
	cout << "and 0 (GEO) to TOPO: " <<
	    MRadialVelocity::Convert(rvGeo,
				     MRadialVelocity::Ref
				     (MRadialVelocity::TOPO,
				      frame))()
					 .getValue() << endl;
	MRadialVelocity rvTopo = MRadialVelocity::Convert(rvGeo,
							  MRadialVelocity::Ref
							  (MRadialVelocity::TOPO,
							   frame))();
	// The following necessary for errors in Intel chip
	Double mrvback(MRadialVelocity::Convert(rvTopo,
						MRadialVelocity::GEO)()
		       .getValue());
	if (nearAbs(mrvback, 0.0)) mrvback = 0;
	cout << "and back: " << mrvback << endl;
	rvBary.set(MVRadialVelocity(0.0));
	cout << "and 0 (BARY) to LSR: " <<
	    MRadialVelocity::Convert(rvBary,
				     MRadialVelocity::Ref
				     (MRadialVelocity::LSRK,
				      frame))()
					 .getValue() << endl;
	MRadialVelocity rvLSR = MRadialVelocity::Convert
	    (rvBary,
	     MRadialVelocity::Ref
	     (MRadialVelocity::LSRK,
	      frame))();
	// The following necessary for errors in Intel chip
	mrvback = MRadialVelocity::Convert(rvLSR,
					   MRadialVelocity::BARY)()
	  .getValue();
	if (nearAbs(mrvback, 0.0, 3e-13)) mrvback = 0;
	cout << "and back: " << mrvback << endl;
    }
    
    {
	cout << "Test real radial velocity" << endl <<
	    "-----------------------------------------" <<endl;
	MDirection coord(Quantity(185.425833,"deg"),
			 Quantity(31.799167,"deg"),
			 MDirection::B1950);
	MEpoch epo(Quantity(50217.625,"d"));
	MPosition pos(Quantity(10,"m"),
		      Quantity(6.60417,"deg"),
		      Quantity(52.8,"deg"),
		      MPosition::WGS84);
	MeasFrame frame(coord,epo,pos);
	cout << frame << endl;
	cout << MDirection::Convert(coord,
				    MDirection::Ref(MDirection::APP,
						    frame))()
	  .getValue()
	  .getAngle("deg") <<
	  endl;
	cout << MDirection::Convert(coord,
				    MDirection::Ref(MDirection::APP,
						    frame))()
	  .getValue() <<
	  endl;
	cout << MEpoch::Convert(epo,
				MEpoch::Ref(MEpoch::LAST,
					    frame))().getValue() 
					      << endl;
	MRadialVelocity rv(Quantity(1253,"km/s"),
			   MRadialVelocity::Ref(MRadialVelocity::LSRK,
						frame));

	 cout << MRadialVelocity::Convert(rv,
					 MRadialVelocity::Ref
					 (MRadialVelocity::TOPO,
					  frame))()
					      .getValue() -
						  rv.getValue() <<
						      endl;
 	epo = MEpoch(Quantity(50217.625,"d"));
	frame.set(epo);
	cout << MDirection::Convert(coord,
				    MDirection::Ref(MDirection::APP,
						    frame))()
							.getValue() <<
								endl;
	rv.set(MRadialVelocity::Ref(MRadialVelocity::BARY,
				    frame));
	Aberration ab2(Aberration::B1950);
	cout << "Aberration: " <<
	    ab2(epo.getValue().get()) << endl;
	cout << MRadialVelocity::Convert(rv,
					 MRadialVelocity::Ref
					 (MRadialVelocity::TOPO,
					  frame))()
					      .getValue() -
						  rv.getValue() <<
						      endl;
    }

    {
	MPosition obs(Quantity(0,"m"),
		      Quantity(-289375.79, Unit('"')),
		      Quantity(50,"deg"));
	MeasFrame frame(obs);
	MEpoch::Ref tasref(MEpoch::LAST, frame);
	MEpoch tm3(MVEpoch(50272.40590277778), MEpoch::UT1);
	MEpoch::Convert tconv5(tm3,tasref);
	MEpoch::Convert tconv51(tm3,MEpoch::GMST);
	MEpoch::Convert tconv52(tm3,MEpoch::GAST);
	cout << "Converted " << tm3 << endl <<
	    " to " << MEpoch::Ref(MEpoch::GMST) << endl <<
		" as " << tconv51() << endl;
	cout << "Converted " << tm3 << endl <<
	    " to " << MEpoch::Ref(MEpoch::GAST) << endl <<
		" as " << tconv52() << endl;
	cout << "Converted " << tm3 << endl <<
	    " to " << tasref << endl <<
		" as " << tconv5() << endl;
	MEpoch tm7(tconv5());
	MEpoch::Convert tconv6(tm7, MEpoch::UT1);
	cout << "Converted back " << tm7 << endl <<
	    " to " << MEpoch::Ref(MEpoch::UT1) << endl <<
		" as " << tconv6() << endl;

	MEpoch tm31(MVEpoch(50272.40590277778),
		    MEpoch::Types(MEpoch::UT1+MEpoch::RAZE));
	MEpoch::Ref tofref(MEpoch::LAST, frame, tm31);
	MEpoch::Convert tconv53(tm3,tofref);
	cout << "Converted " << tm3 << endl <<
	    " to " << tofref << endl <<
		" as " << tconv53() << endl;
	
    };	

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	cout << "Test MeasFrame ..." << endl;
	cout << "--------------------------------------" << endl;

	MEpoch tbm(Quantity(MeasData::MJDB1950,"d"));
	cout << "Epoch B1950: " << tbm << endl;

        MeasFrame mftbm(tbm);
	cout << mftbm << endl;

    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	cout << "Test coordinate conversion ..." << endl;
	cout << "--------------------------------------" << endl;
	
	MDirection::Ref eqref(MDirection::B1950);
	MDirection::Ref galref(MDirection::GALACTIC);
	MDirection gpole(Quantity(192.25,"deg"),Quantity(27.4,"deg"),eqref); 
	MDirection eqpole(Quantity(192.25,"deg"),Quantity(27.4,"deg"),galref); 
	MDirection::Convert eqgal(gpole,galref);
	MDirection::Convert galeq(eqpole,eqref);
    
	Vector<Double> veqgal(eqgal().getValue().getValue());
	if (nearAbs(veqgal(2), 1.0, 1e-10)) {
	  veqgal(0) = 0;
	  veqgal(1) = 0;
	};
	cout << "Converted B1950 galactic pole " << gpole << endl << 
	  " to " << galref << endl <<
	  " as " << veqgal << endl;
	cout << "Converted B1950 galactic pole " << eqpole << endl << 
	    " to " << eqref << endl <<
		" as " << galeq().getAngle("deg") << endl;

	MEpoch app(Quantity(50083.0,"d"));
	MeasFrame appframe;
	MDirection::Ref appref(MDirection::APP, appframe);
	MDirection::Ref j2000ref(MDirection::J2000);
	MVDirection j2000vec(
			     -0.373798658,
			     -0.312643465,
			     -0.873228852);
	MDirection j2000(j2000vec);
	MDirection b1950(j2000vec, MDirection::B1950);
	MDirection::Convert j2000app(j2000,appref);
	appframe.set(app);
	MDirection appc(j2000app());
	MDirection::Convert appj2000(appc,j2000ref);
	MDirection::Convert b1950j2000(b1950, MDirection::J2000);
	MDirection appj(b1950j2000());
	MDirection::Convert j2000b1950(appj, MDirection::B1950);
	
	cout << "Converted J2000 coordinates " << j2000.getValue() <<
	    " to: " << j2000app().getValue() << endl;
	
	cout << "Converted back to J2000 coordinates " << appc.getValue() <<
	    " to: " << appj2000().getValue() << endl;
	
	cout << "Converted B1950 to J2000 coordinates " << b1950.getValue() <<
	    " to: " << b1950j2000().getValue() << endl;
	
	cout << "Converted J2000 to B1950 coordinates " << 
	    appj.getValue() <<
		" to: " << j2000b1950(appj).getValue() << endl;
	MPosition wsrt(Quantity(16,"m"), Quantity(6.60417,"deg"),
		       Quantity(52.91692,"deg"), MPosition::WGS84);
	MPosition::Convert wsrtitrf(wsrt, MPosition::ITRF);
	MPosition geod(wsrtitrf());

	cout << "Converted geodetic position: " << endl <<
	    wsrt.getValue().getLength() << ", " <<
		wsrt.getAngle("deg") << endl << "to: " <<
		    geod.getValue().getLength() << ", " <<
			geod.getAngle("deg") << endl;

	MPosition::Convert wsrtwgs(geod, MPosition::WGS84);
	wsrt = wsrtwgs();

	cout << "Converted geocentric position: " << endl <<
	    geod.getValue().getLength() << ", " <<
		geod.getAngle("deg") << endl << "to: " <<
		    wsrt.getValue().getLength() << ", " <<
			wsrt.getAngle("deg") << endl;

    {
	MEpoch tim(MVEpoch(50082), MEpoch::UT1);
	MeasFrame frame(wsrt, tim);
	MDirection appvec(MDirection::Convert(j2000vec, 
					      MDirection::Ref(MDirection::APP,
							      frame))());

	cout << "J2000 coordinates: " <<
	    j2000vec.getAngle("deg") << endl;
	cout << "Apparent coordinate" <<
	    appvec.getAngle("deg") << endl;
	Double d1, d2 , d3;
	frame.getLong(d1);
	frame.getLat(d2);
	frame.getLAST(d3);

	cout << "Longitude: " << MVAngle(d1).get("deg") << endl;
	cout << "Latitude:  " << MVAngle(d2).get("deg") << endl;
	cout << "LAST:      " << MVAngle(d3*C::circle).
	    string(MVAngle::TIME, 8) << endl;
	cout << "LAST:      " << MVAngle(d3*C::circle).
	    string(MVAngle::ANGLE, 8) << endl;
	cout << "LAST:      " << 
	    MEpoch::Convert(tim,
			    MEpoch::Ref(MEpoch::LAST, frame))() << endl;
	cout << "HA/DEC: " <<
	    MDirection::Convert(appvec, 
				MDirection::Ref(MDirection::HADEC,
						frame))()
						    .getAngle("deg") <<
							endl;
	MDirection had(MDirection::Convert(appvec, 
					   MDirection::Ref(MDirection::HADEC,
							   frame))());
	cout << "Back:   " <<
	    MDirection::Convert(had, 
				MDirection::Ref(MDirection::APP,
						frame))()
						    .getAngle("deg") <<
							endl;
	cout << "Az/El:  " <<
	    MDirection::Convert(had, 
				MDirection::Ref(MDirection::AZEL,
						frame))()
						    .getAngle("deg") <<
							endl;
	MDirection azel(MDirection::Convert(appvec, 
					   MDirection::Ref(MDirection::AZEL,
							   frame))());
	cout << "Back:   " <<
	    MDirection::Convert(azel, 
				MDirection::Ref(MDirection::HADEC,
						frame))()
						    .getAngle("deg") <<
							endl;
    };
    {
      MEpoch app(Quantity(47165.8,"d"));
      MeasFrame appframe(app);
      MDirection::Ref appref(MDirection::APP, appframe);
      MDirection::Ref b1950ref(MDirection::B1950);
      MDirection bappc(Quantity(85.4267,"deg"),
		       Quantity(49.8502,"deg"), appref);
      MDirection::Convert appb1950(bappc, b1950ref);
      MDirection b1950(appb1950());
      MDirection::Convert b1950app(b1950, appref);
      MVDirection mvpole;
      MDirection bappc1(Quantity(85.4267,"deg"),
			Quantity(50.8502,"deg"), appref);
      
      cout << "Converted to B1950 coordinates " << bappc.getAngle("deg") <<
	" to: " << appb1950().getAngle("deg") << endl;
      cout << "and back to: " <<
	b1950app().getAngle("deg") << endl;
      MDirection apole(b1950app(mvpole));
      cout << "Rotation angle: " << 
	bappc.getValue().positionAngle(apole.getValue(), "deg") << endl;
    }

    MEpoch tbm(Quantity(50927.92931, "d"));
    MPosition pos(MVPosition(-4750915.84032, 2792906.17778, 
			     -3200483.75028), 
		  MPosition::ITRF);
    MDirection fmb0(MVDirection(0.5, 0.5, 0.5), MDirection::J2000);
    MRadialVelocity fmfrq0(MVRadialVelocity(100.), MRadialVelocity::LSRK);
    MeasFrame mf(tbm, pos, fmb0);
    mf.set(fmfrq0);
    Vector<Double> tvec(3);
    tvec = 0.0;
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MDirection conversions forward/backward" << endl;
      Bool isok = True;

      MVDirection mvd0(0.5, 0.5, 0.5);
      Double tp;
      for (uInt i=MDirection::J2000; i<MDirection::N_Types; i++) {
	for (uInt j=MDirection::J2000; j<MDirection::N_Types; j++) {
	  if (i == MDirection::B1950 || i == MDirection::BMEAN ||
	      i == MDirection::BTRUE || i == MDirection::B1950_VLA ||
	      j == MDirection::B1950 || j == MDirection::BMEAN ||
	      j == MDirection::BTRUE || j == MDirection::B1950_VLA) tp = 5e-9;
	  else tp = 1e-9;
	  MDirection::Ref rin(i, mf);
	  MDirection::Ref rout(j, mf);
	  MDirection mb0(mvd0, rin);
	  MDirection::Convert forw(rin, rout);
	  MDirection::Convert backw(rout, rin);
	  if (!allNearAbs(mb0.getValue().getValue() -
			  backw(forw(mb0)).getValue().getValue(), 
			  tvec, tp)) {
	    cout << MDirection::showType(i) << " to " <<
	      MDirection::showType(j) << ": " <<
	      mb0.getValue().getValue() -
	      backw(forw(mb0)).getValue().getValue() << endl;
	    isok = False;
	  };
	};
      };
      if (isok) {
	cout << "All forward/backward Direction conversions: ok" << endl;
      } else {
	cout << "Some forward/backward Direction conversions wrong" << endl;
      };
    }
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MPosition conversions forward/backward" << endl;
      Bool isok = True;

      MVPosition mvd0(-100, 100, -100);
      Double tp;
      for (uInt i=0; i<MPosition::N_Types; i++) {
	if (i == MPosition::WGS84) mvd0 = MVPosition(-100, 100, -100);
	else mvd0 = MVPosition(-3.68663e+06, 3.68663e+06, -3.66196e+06);
	for (uInt j=0; j<MPosition::N_Types; j++) {
	  tp = 5e-8;
	  MPosition::Ref rin(i, mf);
	  MPosition::Ref rout(j, mf);
	  MPosition mb0(mvd0, rin);
	  MPosition::Convert forw(rin, rout);
	  MPosition::Convert backw(rout, rin);
	  if (!allNearAbs(mb0.getValue().getValue() -
			  backw(forw(mb0)).getValue().getValue(), 
			  tvec, tp)) {
	    cout << MPosition::showType(i) << " to " <<
	      MPosition::showType(j) << ": " <<
	      mb0.getValue().getValue() -
	      backw(forw(mb0)).getValue().getValue() << endl;
	    isok = False;
	  };
	};
      };
      if (isok) {
	cout << "All forward/backward Position conversions: ok" << endl;
      } else {
	cout << "Some forward/backward Position conversions wrong" << endl;
      };
    }
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MEpoch conversions forward/backward" << endl;
      Bool isok = True;

      MVEpoch mvd0(50930);
      Double tp;
      for (uInt i=0; i<MEpoch::N_Types; i++) {
	for (uInt j=0; j<MEpoch::N_Types; j++) {
	  tp = 1e-9;
	  MEpoch::Ref rin(i, mf);
	  MEpoch::Ref rout(j, mf);
	  MEpoch mb0(mvd0, rin);
	  MEpoch::Convert forw(rin, rout);
	  MEpoch::Convert backw(rout, rin);
	  if (!nearAbs(mb0.getValue().get() -
		       backw(forw(mb0)).getValue().get(), 
		       tvec(0), tp)) {
	    cout << MEpoch::showType(i) << " to " <<
	      MEpoch::showType(j) << ": " <<
	      mb0.getValue().get() -
	      backw(forw(mb0)).getValue().get() << endl;
	    isok = False;
	  };
	};
      };
      if (isok) {
	cout << "All forward/backward Epoch conversions: ok" << endl;
      } else {
	cout << "Some forward/backward Epoch conversions wrong" << endl;
      };
    }
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MFrequency conversions forward/backward" << endl;
      cout << "  including the \"impossible\" conversion to/from " 
	   << MFrequency::showType(MFrequency::Undefined) << endl;
      Bool isok = True;

      MVFrequency mvd0(1e9);
      Double tp;
      for (uInt i=0; i<MFrequency::N_Types; i++) {
	for (uInt j=0; j<MFrequency::N_Types; j++) {
	  tp = 1e-6;
	  MFrequency::Ref rin(i, mf);
	  MFrequency::Ref rout(j, mf);
	  MFrequency mb0(mvd0, rin);
	  MFrequency::Convert forw(rin, rout);
	  MFrequency::Convert backw(rout, rin);
	  if (!near(mb0.getValue().getValue(),
		       backw(forw(mb0)).getValue().getValue(), 
		       tp)) {
	    cout << MFrequency::showType(i) << " to " <<
	      MFrequency::showType(j) << ": " <<
	      mb0.getValue().getValue() -
	      backw(forw(mb0)).getValue().getValue() << endl;
	    isok = False;
	  };
	};
	// try also the "impossible" conversion to/from Undefined
	tp = 1e-6;
	MFrequency::Ref r(i, mf);
	MFrequency::Ref rundef(MFrequency::Undefined, mf);
	try{
	  MFrequency::Convert forw;
          MFrequency mfreq(MFrequency::MVType(), r);
          forw.setModel(mfreq);
          forw.setOut (rundef);
	  cout << MFrequency::showType(i) << " to " <<
	    MFrequency::showType(MFrequency::Undefined) << " should not be possible." << endl;
	  isok = False;
	}
	catch(const AipsError& x){
	  // expected error
	  cout << x.getMesg() << endl;
	}
        try {
	  MFrequency::Convert backw;
          MFrequency mfreq(MFrequency::MVType(), rundef);
          backw.setModel(mfreq);
          backw.setOut (r);
	  cout << MFrequency::showType(MFrequency::Undefined) << " to " <<
	    MFrequency::showType(i) << " should not be possible." << endl;
	}
	catch(const AipsError& x){
	  // expected error
	  cout << x.getMesg() << endl;
	}
	  
      };
      if (isok) {
	cout << "All forward/backward Frequency conversions: ok" << endl;
      } else {
	cout << "Some forward/backward Frequency conversions wrong" << endl;
      };
      

    }
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MDoppler conversions forward/backward" << endl;
      Bool isok = True;

      MVDoppler mvd0(0.01);
      Double tp;
      for (uInt i=0; i<MDoppler::N_Types; i++) {
	if (i == MDoppler::GAMMA) mvd0 = MVDoppler(2);
	else mvd0 = MVDoppler(0.01);
	for (uInt j=0; j<MDoppler::N_Types; j++) {
	  tp = 1e-9;
	  MDoppler::Ref rin(i, mf);
	  MDoppler::Ref rout(j, mf);
	  MDoppler mb0(mvd0, rin);
	  MDoppler::Convert forw(rin, rout);
	  MDoppler::Convert backw(rout, rin);
	  if (!nearAbs(mb0.getValue().getValue() -
		       backw(forw(mb0)).getValue().getValue(), 
		       tvec(0), tp)) {
	    cout << MDoppler::showType(i) << " to " <<
	      MDoppler::showType(j) << ": " <<
	      mb0.getValue().getValue() -
	      backw(forw(mb0)).getValue().getValue() << endl;
	    isok = False;
	  };
	};
      };
      if (isok) {
	cout << "All forward/backward Doppler conversions: ok" << endl;
      } else {
	cout << "Some forward/backward Doppler conversions wrong" << endl;
      };
    }
    
    {
      cout << "------------------------------------" << endl;
      cout << "Testing all MRadialVelocity conversions forward/backward" << endl;
      Bool isok = True;

      MVRadialVelocity mvd0(100);
      Double tp;
      for (uInt i=0; i<MRadialVelocity::N_Types; i++) {
	for (uInt j=0; j<MRadialVelocity::N_Types; j++) {
	  tp = 1e-9;
	  MRadialVelocity::Ref rin(i, mf);
	  MRadialVelocity::Ref rout(j, mf);
	  MRadialVelocity mb0(mvd0, rin);
	  MRadialVelocity::Convert forw(rin, rout);
	  MRadialVelocity::Convert backw(rout, rin);
	  if (!nearAbs(mb0.getValue().getValue() -
		       backw(forw(mb0)).getValue().getValue(), 
		       tvec(0), tp)) {
	    cout << MRadialVelocity::showType(i) << " to " <<
	      MRadialVelocity::showType(j) << ": " <<
	      mb0.getValue().getValue() -
	      backw(forw(mb0)).getValue().getValue() << endl;
	    isok = False;
	  };
	};
      };
      if (isok) {
	cout << "All forward/backward RadialVelocity conversions: ok" << endl;
      } else {
	cout << "Some forward/backward RadialVelocity conversions wrong" << endl;
      };
    }

    } catch (AipsError x) {
      cout << x.getMesg() << endl;
    } 

    return 0;
}
