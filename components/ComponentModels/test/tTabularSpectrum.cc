//# tTabularSpectrum.cc: tests the TabularSpectrum class
//# Copyright (C) 2010
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
//# $Id:$

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/TabularSpectrum.h>
#include <components/ComponentModels/Flux.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasureHolder.h>
#include <measures/Measures/MeasRef.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
  try {
    SpectralModel* siPtr = 0;;
    MVFrequency f1(Quantity(1.0, "GHz"));
    MVFrequency f2(Quantity(2.0, "GHz"));
    MVFrequency f4(Quantity(4.0, "GHz"));
    MFrequency::Ref frame(MFrequency::LSRK);
    Flux<Double> j1(1.0, 0.0, 0.0, 0.0);
    Flux<Double> j2(2.0, 0.0, 0.0, 0.0);
    Flux<Double> j4(4.0, 0.0, 0.0, 0.0);
    Vector<Flux<Double> >js(3);
    js[0]=j1; js[1]=j2; js[2]=j4;
    Vector<MVFrequency> fs(3);
    fs[0]=f1; fs[1]=f2; fs[2]=f4;
    MFrequency refFreq(Quantity(1.0, "GHz"), MFrequency::LSRK);
    
    TabularSpectrum siModel;
    AlwaysAssert(siModel.ok(), AipsError);
    AlwaysAssert(siModel.type() == ComponentType::TABULAR_SPECTRUM, 
		 AipsError);
    cout << "Passed the default constructor test" << endl;
    
    siModel.setRefFrequency(MFrequency(Quantity(1.5, "GHz"), MFrequency::LSRK));
    siModel.setValues(fs, js, frame);
    cout << siModel.sample(MFrequency(f1, MFrequency::LSRK)) << "  " << siModel.sample(MFrequency(f2, MFrequency::LSRK)) << endl;
      //AlwaysAssert(near(siModel.sample(MFrequency(f1, MFrequency::LSRK)), 0.666667), AipsError);
      //AlwaysAssert(near(siModel.sample(MFrequency(f2, MFrequency::LSRK)), 1.33333), AipsError);
       
    Vector<Double> scale;
    siModel.sample(scale, fs, frame);
    cout << "Scale " << scale << endl;
    
    siModel.setRefFrequency(f2);
    siPtr = siModel.clone();
    cout << "Passed the index test" << endl;
    
    
   
    
    
    Record rec;
    String errMsg;
    AlwaysAssert(siPtr->toRecord(errMsg, rec), AipsError); 
    AlwaysAssert(errMsg == "", AipsError); 
    AlwaysAssert(rec.isDefined("type"), AipsError);
    AlwaysAssert(rec.isDefined("frequency"), AipsError);
    AlwaysAssert(rec.isDefined("freqRef"), AipsError);
    AlwaysAssert(rec.isDefined("tabFreqVal"), AipsError);
    String type;
    rec.get(RecordFieldId("type"), type);
    AlwaysAssert(type == "Tabular Spectrum", AipsError);
    Record rc=rec.asRecord("freqRef");
    cout <<" rc " << rc << endl; 
    Record freqRec = rec.asRecord(RecordFieldId("frequency"));
    MeasureHolder mh;
    mh.fromRecord(errMsg, freqRec);
    AlwaysAssert(errMsg.length() == 0, AipsError);
    AlwaysAssert(mh.isMFrequency(), AipsError);
    
    TabularSpectrum newSp;
    newSp.fromRecord(errMsg, rec);
    Vector<Double> scale1;
    newSp.sample(scale, fs, frame);
    cout << "Scale " << scale << endl;

      /*
      mh = f1;
      Record newRec;
      newRec.define(RecordFieldId("type"), "spEctrAl IndEx");
      newRec.define(RecordFieldId("index"), 0.0);
      Record newFreq;
      AlwaysAssert(mh.toRecord(errMsg, newFreq), AipsError);
      AlwaysAssert(errMsg.length() == 0, AipsError);
      newRec.defineRecord(RecordFieldId("frequency"), newFreq);
      AlwaysAssert(siModel.fromRecord(errMsg, newRec), AipsError);
      AlwaysAssert(near(siModel.index(), 0.0), AipsError);
      AlwaysAssert(near(siModel.refFrequency().get("GHz").getValue(), 1.0), 
		   AipsError);
      Record emptyRec;
      AlwaysAssert(siModel.convertUnit(errMsg, emptyRec), AipsError);
      emptyRec.define(RecordFieldId("index"), "");
      AlwaysAssert(siModel.convertUnit(errMsg, emptyRec), AipsError);
      emptyRec.define(RecordFieldId("index"), "deg");
      AlwaysAssert(siModel.convertUnit(errMsg, emptyRec) == False, AipsError);
      cout << "Passed the record handling test" << endl;
    }
    */
    delete siPtr;
    }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSpectralIndex"
// End: 
