//# tSpectralIndex.cc: tests the SpectralIndex class
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/SpectralIndex.h>
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
    const MFrequency f1(Quantity(1.0, "GHz"), MFrequency::LSRK);
    const MFrequency f2(Quantity(2.0, "GHz"), MFrequency::LSRK);
    const MFrequency f4(Quantity(4.0, "GHz"), MFrequency::LSRK);
    {
      SpectralIndex siModel;
      AlwaysAssert(siModel.ok(), AipsError);
      AlwaysAssert(siModel.type() == ComponentType::SPECTRAL_INDEX, 
		   AipsError);
      AlwaysAssert(near(siModel.index(), 0.0), AipsError);
      cout << "Passed the default constructor test" << endl;
       
      AlwaysAssert(near(siModel.sample(f1), 1.0), AipsError);
      AlwaysAssert(near(siModel.sample(f2), 1.0), AipsError);
       
      siModel.setIndex(1.0);  
      AlwaysAssert(near(siModel.sample(f1), 1.0), AipsError);
      AlwaysAssert(near(siModel.sample(f2), 2.0), AipsError);
      siModel.setRefFrequency(f2);
      siPtr = siModel.clone();
      cout << "Passed the index test" << endl;
    }
    {
      Vector<Double> p = siPtr->parameters().copy();
      AlwaysAssert(near(p(0), 1.0), AipsError);
      AlwaysAssert(near(siPtr->refFrequency().get("GHz").getValue(), 2.0), 
		   AipsError);
      AlwaysAssert(siPtr->nParameters() == 1, AipsError);
      p(0) = -1.0;
      siPtr->setParameters(p);
      cout << "Passed the parameters test" << endl;
      SpectralIndex copy(*((SpectralIndex*)siPtr));
      SpectralIndex assigned;
      assigned = copy;
      p(0) = 1.0;
      siPtr->setParameters(p);
      siPtr->setRefFrequency(f4);
      copy.setIndex(0.0);
      copy.setRefFrequency(f1);
      p(0) = -10.0;
      p = siPtr->parameters();
      AlwaysAssert(near(p(0), 1.0), AipsError);
      AlwaysAssert(near(siPtr->refFrequency().get("GHz").getValue(), 4.0), 
		   AipsError);
      AlwaysAssert(near(copy.index(), 0.0), AipsError);
      AlwaysAssert(near(copy.refFrequency().get("GHz").getValue(), 1.0), 
		   AipsError);
      AlwaysAssert(near(assigned.index(), -1.0), AipsError);
      AlwaysAssert(near(assigned.refFrequency().get("GHz").getValue(), 2.0), 
		   AipsError);
      cout << "Passed the copy semantics test" << endl;
    }
    {
      SpectralIndex siModel(f2, 1.0);
      AlwaysAssert(near(siModel.index(), 1.0), AipsError);
      AlwaysAssert(near(siModel.refFrequency().get("GHz").getValue(), 2.0), 
		   AipsError);
      Vector<MVFrequency> freqs(3);
      freqs(0) = f1.getValue(); 
      freqs(1) = f2.getValue();
      freqs(2) = f4.getValue();
      MFrequency::Ref ref(MFrequency::LSRK);
      Vector<Double> results(3);
      siModel.sample(results, freqs, ref);
      AlwaysAssert(near(results(0), 0.5), AipsError);
      AlwaysAssert(near(results(1), 1.0), AipsError);
      AlwaysAssert(near(results(2), 2.0), AipsError);
      cout << "Passed the multi-sample test" << endl;
      Record rec;
      String errMsg;
      AlwaysAssert(siModel.toRecord(errMsg, rec), AipsError); 
      AlwaysAssert(errMsg == "", AipsError); 
      AlwaysAssert(rec.isDefined("type"), AipsError);
      AlwaysAssert(rec.isDefined("frequency"), AipsError);
      AlwaysAssert(rec.isDefined("index"), AipsError);
      AlwaysAssert(rec.isDefined("error"), AipsError);
      String type;
      rec.get(RecordFieldId("type"), type);
      AlwaysAssert(type == "Spectral Index", AipsError);
      Double index;
      rec.get(RecordFieldId("index"), index);
      AlwaysAssert(near(index, 1.0), AipsError); 
      Record freqRec = rec.asRecord(RecordFieldId("frequency"));
      MeasureHolder mh;
      mh.fromRecord(errMsg, freqRec);
      AlwaysAssert(errMsg.length() == 0, AipsError);
      AlwaysAssert(mh.isMFrequency(), AipsError);
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
