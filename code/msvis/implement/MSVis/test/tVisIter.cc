//# tVisibilityIterator.cc: Tests the Synthesis MeasurementSet Iterator
//# Copyright (C) 1995,1999,2000,2001
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

#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>
#include <msvis/MSVis/VisSet.h>
#include <msvis/MSVis/VisBuffer.h>
#include <msvis/MSVis/VisibilityIterator.h>
#include <casa/OS/Timer.h>
#include <casa/iomanip.h>
#include <casa/namespace.h>
using namespace std;

int main(int argc, char **argv) {
  Timer timer;
  for(Int i=0;i<20;i++) cout<<endl;  cout<<"Start."<<endl;
  try {
    if (argc < 2) throw(AipsError("no table given on the command line"));


    MS ms(argv[1], Table::Update);

    Block<int> sort(4);
    sort[2] = MS::FIELD_ID;
    sort[3] = MS::ARRAY_ID;
    sort[1] = MS::DATA_DESC_ID;
    sort[0] = MS::TIME;
//    sort[4] = MS::ANTENNA1;	//these _really_ slow things down. (fctr 100)
//    sort[5] = MS::ANTENNA2;   //cause 1 row per chunk or iter
    Matrix<Int> allChannels;
    Double intrvl=2;
    VisSet vs(ms,sort,allChannels,intrvl);
    VisIter& vi(vs.iter());
//    VisIter vi(ms,sort);
    VisBuffer vb(vi);

    Int iRow=0, iTime=-1, iIter=0, iChunk=0;
    Double oldTime=-1., time0; Vector<Double> curTime;
    Complex xxx;
    Int row, chn, pol;
    Bool newTime, newAnt;
    Vector<Int> ant1, ant2;   Int a1, a2, oldAnt=-1;

    timer.show(" setup: "); timer.mark();

    for (vi.originChunks();vi.moreChunks();vi.nextChunk()) {
      for (vi.origin();vi.more();vi++) {
	if(oldTime==-1) time0=vi.time(curTime)(0);
        newTime=(oldTime!=vi.time(curTime)(0));
	if(newTime) {oldTime=curTime(0); iTime++; }
        a1=vi.antenna1(ant1)(0); a2=vi.antenna2(ant2)(0);
	if(newAnt=(oldAnt!=a2)) oldAnt=a2;

//	Cube<Complex>& vc= vb.visCube();   //forces actual data retreival

	Int nRow=vb.nRow();
	Int nChan=vb.nChannel();
        Int nPol=vi.visibilityShape()(0);
	for (row=0; row<nRow; row++) {
//	  if (iRow % 1000 == 0) {
//	  if (newTime && row==0) {
	  if (newTime && (iTime%100)==0 && row==0) {
	    cout<<endl<<"Chunk "<<iChunk<<"  Time chg. "<<iTime<<
		   " ("<<Int(oldTime-time0+.5)<<
		   ")  Iter "<<iIter<<"  Row: "<<iRow<<"  nRows: "<<nRow;
	    timer.show("  ");
            cout<< "Bsln "<<a1<<"-"<<a2<<
		   "  Field "<<vi.fieldId()<<"("<<vi.fieldName()<< 
		   ")  SpWwin: "<< vi.spectralWindow()<<endl;

//	    cout<< "  vis ("<<nPol<<" polzn x "<<nChan<<" chan):";
// 	    for (chn=0; chn<nChan; chn++) {
//	      for (pol=0; pol<nPol; pol++) {
//	        cout<< "  "<< vc(pol,chn,row);  }  }
//	    cout<<endl;

          }
/*	  for (chn=0; chn<nChan; chn++) {
	    for (pol=0; pol<nPol; pol++) {

	      // retrieve all obs. data individually, to check timing
	      xxx=vc(pol,chn,row);  }  }
*/
          iRow++;  }
        iIter++;  }
      iChunk++;  }
    iTime++;
    cout<<endl<<"TOTALS: Chunks: "<<iChunk<<"  Time chgs: "<<iTime<<"  Iters: "<<iIter<<
	  "  Rows: "<<iRow<<endl;
  } 
  catch (const AipsError &x) {
    cerr << "***Exception:" << endl;
    cerr << x.getMesg() << endl;
  }
  
  timer.show("Done.  "); cout<<endl<<endl<<endl;
  return 0;  }

