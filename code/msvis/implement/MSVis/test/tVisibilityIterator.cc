//# tVisibilityIterator.cc: Tests the Synthesis MeasurementSet Iterator
//# Copyright (C) 1995,1999,2000
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

#include <trial/MeasurementEquations/VisibilityIterator.h>
#include <aips/Tables.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <iomanip.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Constants.h>

#include <aips/Tables/ForwardCol.h>

Int main(int argc, char **argv)
{
// register forward col engine
  ForwardColumnEngine::registerClass();

  if (argc<2) {
      cout <<"Usage: tVisibilityIterator ms-table-name"<<endl;
      exit(1);
  }
  // try to iterate over a pre-existing MS table 

  try {
    // read the MS from file
    cout << "Reading MS from file and checking type"<<endl;
    MeasurementSet synms(argv[1],Table::Update);

    cout << "Number of rows in ms: "<< synms.nrow()<<endl;

    cout << "Constructing Iterator"<<endl;
    Block<Int> bi(0); // create empty block with sortColumns
    VisibilityIterator syniter(synms,bi);
    // set iterator to start of data
    syniter.origin();

    cout << "Now testing data access functions"<<endl;
    cout << "Showing first 10 iterations:"<<endl;
    Double time0=syniter.time();
    for (Int i=0; i<10; i++) {
	cout << "Iteration "<<i<<" contains:"<<endl;
	cout << "Antenna1="<<syniter.antenna1()
	    <<", Antenna2="<<syniter.antenna2()
		<< setprecision(9)
		    <<", freq="<<syniter.frequency()
			<< setprecision(9)
			    <<", time="<<syniter.time()-time0
				<<", uvw="<<syniter.uvw()
				    <<", vis="<<syniter.visibility()
					<<", sigma="<<syniter.sigma()
					    <<", flag="<<syniter.flag()<<endl;
	syniter++;
    }

    Double time=syniter.time();
    while( time == syniter.time()) syniter++;
    cout << "Next timeslot contains:"<<endl;
    cout << "Antenna1="<<syniter.antenna1()
      <<",Antenna2="<<syniter.antenna2()
      << setprecision(9)
       <<", freq="<<syniter.frequency()
        << setprecision(9)
	<<", time="<<syniter.time()
	 <<", uvw="<<syniter.uvw()
	  <<", vis="<<syniter.visibility()
	      <<", sigma="<<syniter.sigma()
	       <<", flag="<<syniter.flag()
		      <<", feed_pa="<<syniter.feed_pa()<<endl;

    cout << " Reading through visibility data till end of file "<< endl;
    CStokesVector sum;
    for (; syniter.more(); syniter++) {
	cout <<"."; sum+=syniter.visibility(); syniter.uvw();
    }
    cout << " done : sum="<<sum<<endl;

    cout << " Now try to iterate in time-intervals of 10s"<<endl;
    VisibilityIterator syniter2(synms,bi,10.);
    
    for (syniter2.originChunks();syniter2.moreChunks(); syniter2.nextChunk()) {
	syniter2.origin();
	cout <<" Interval start: "<< setprecision(10)<< syniter2.time();
	Int n=0;
	Double time;
	for (;syniter2.more(); syniter2++) {
	    n++;
	    time=syniter2.time();
	}
	cout << ". end: "<< setprecision(10)<< time;
	cout << ", # visibilities in interval: "<<n<<endl;
    }
    cout << " Try iterator with 1000s interval."<<endl;
    VisibilityIterator syniter3; syniter3=syniter2;
    syniter3.setInterval(1000.);

    for (syniter3.originChunks();syniter3.moreChunks(); syniter3.nextChunk()) {
	syniter3.origin();
	cout <<" Interval start: "<< setprecision(10)<< syniter3.time();
	Int n=0;
	syniter.feed_pa();
	for (;syniter3.more(); syniter3++) {
	    n++;
	}
	cout << ", end: "<< setprecision(10)<< syniter3.time();
	cout << ", # visibilities in interval: "<<n<<endl;
    }

    cout << " Two iterators running simultaneously"<<endl;
    for (syniter2.originChunks(), syniter3.originChunks();
	 syniter2.moreChunks() || syniter3.moreChunks(); 
	 syniter2.nextChunk(), syniter3.nextChunk()) {
	syniter2.origin();
	cout <<" Interval 1 start: "<< setprecision(10)<< syniter2.time();
	Int n=0;
	for (;syniter2.more(); syniter2++) {
	    n++;
	}
	cout << ", end: "<< setprecision(10)<< syniter2.time();
	cout << ", # visibilities in interval: "<<n<<endl;
	syniter3.origin();
	cout <<" Interval 2 start: "<< setprecision(10)<< syniter3.time();
	n=0;
	for (;syniter3.more(); syniter3++) {
	    n++;
	}
	cout << ", end: "<< setprecision(10)<< syniter3.time();
	cout << ", # visibilities in interval: "<<n<<endl;
    }
        
    // try the polarization stuff
    syniter.originChunks(); syniter.origin();
    cout << " Polarization frame "<< syniter.polFrame()<<endl;
    cout << " Circular="<< ROVisibilityIterator::Circular<<
	", Linear="<< ROVisibilityIterator::Linear<<endl;
    cout << " CJones(antenna==0)="<< syniter.CJones(0).matrix()<<endl;

    // try assigment
    VisibilityIterator myiter;
    myiter=syniter;

    cout << "Exiting scope of tVisibilityIterator" << endl;
    } catch (AipsError x) {
	cout << "Caught exception " << endl;
	cout << x.getMesg() << endl;
	return 1;
    } 
    cout << "Done." << endl;
    return 0;
}
