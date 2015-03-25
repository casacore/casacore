//# tMSDerivedValues: Tests the MSDerivedValues class
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/ms/MSOper/MSDerivedValues.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Arrays/ArrayIO.h>

#include <casacore/casa/namespace.h>
int main()
{
  try {
    Quantity longitude; Quantity::read(longitude,"149.33.00.5");
    cout << "longitude: "<<MVAngle(longitude)<<endl;
    Quantity latitude;  Quantity::read(latitude,"-30.18.46.385");
    cout << "latitude : "<<MVAngle(latitude)<<endl;
    Vector<MPosition> pos(1);
    pos(0)=MPosition(Quantity(236.9,"m"),longitude,latitude,
		     MPosition::Ref(MPosition::WGS84));

    MSDerivedValues msd;
    msd.setAntennaPositions(pos);

    Quantity time; MVTime::read(time,"today");
    MEpoch today(time);

    cout <<" Current time: "<<MVTime(today.getValue())<<endl;

    msd.setEpoch(today);

    MEpoch last = msd.last();
    
    cout <<" Current last: "<< MVTime(last.getValue())<<endl;

    Quantity ra; Quantity::read(ra,"12:00:00.0");
    Quantity dec; Quantity::read(dec,"-30.00.00.0");
    MDirection mySource(ra,dec);
   
    cout <<" FieldCenter: "<< mySource.getAngle("deg") <<endl;

    msd.setFieldCenter(mySource);

    cout <<" Hour angle : "<< MVAngle(msd.hourAngle()) <<endl;

    cout <<" Az & El    : "<< msd.azel().getAngle("deg")<<endl;

    Vector<String> mount(1); mount(0)="alt-az";

    msd.setAntennaMount(mount);

    cout << " Par. angle: "<< MVAngle(msd.parAngle()) << endl;

    // test the obsVel conversion
    
    cout << " observatory velocity in LSR  frame: "<< msd.obsVel().get("km/s").
      getValue() << "km/s"<<endl;

    msd.setVelocityFrame(MRadialVelocity::BARY);
    cout << " observatory velocity in BARY frame: "<< msd.obsVel().get("km/s").
      getValue() << "km/s"<<endl;

    
    msd.setVelocityFrame(MRadialVelocity::GEO);
    cout << " observatory velocity in GEO frame: "<< msd.obsVel().get("km/s").
      getValue() << "km/s"<<endl;

    Quantity restFreq(1.42041,"GHz");
    Quantity radioVel(2196.25,"km/s");
    msd.setVelocityReference(MDoppler::RADIO);
    msd.setFrequencyReference(MFrequency::TOPO);
    cout << "radio velocity " << radioVel << " is frequency " << 
      msd.toFrequency(radioVel, restFreq) << endl;
    //====
    /*
    MSDerivedValues msd1;
    MeasurementSet ms("/home/rohir/kgolap/Regression/ALMATST1/NGC5921/ngc5921.ms");
    msd1.setMeasurementSet(ms);
    msd1.setVelocityReference(MDoppler::RADIO);
    msd1.setFrequencyReference(MFrequency::LSRK);
    //locate restFrequency for field 0 and spwid 0
    Bool hasRestFreq=msd1.setRestFrequency(0,0);
    if(!hasRestFreq){
      cout << "Ou la la .....has no restfrequency in this ms" << endl;
    }
    else{
     cout << "radio velocity " << radioVel << " is frequency " << 
       msd1.toFrequency(radioVel) << endl; 
    }
    */

    } catch (AipsError x) {
	cout << "Caught exception " << endl;
	cout << x.getMesg() << endl;
	return 1;
    } 
    cout << "Done." << endl;
    return 0;
}
