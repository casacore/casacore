//# tMeasureHolder.cc: This program tests MeasureHolder
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
#include <aips/Exceptions.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Measures/QuantumHolder.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Record.h>
#include <aips/Glish/GlishRecord.h>

Int main() {

  try {

    cout << "----------------------------------------------------" << endl;
    cout << "Test MeasureHolder  " << endl;
    cout << "----------------------------------------------------" << endl;

    String error;
    MeasureHolder q00;
    MDirection x00(Quantity(30, "deg"), Quantity(-40, "deg"));
    Record y00;
    GlishRecord z00;

    cout << "Input value:              " << x00 << " (" <<
	  x00.getRefString() << ")" << endl;
    if (MeasureHolder(x00).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	  q00.asMeasure().getRefString() << ")" << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
    if (MeasureHolder(x00).toRecord(error, z00)) {
      if (q00.fromRecord(error, z00)) {
	cout <<"Glish output value:       " << q00.asMeasure() << " (" <<
	  q00.asMeasure().getRefString() << ")" << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };

    MeasureHolder q01 = q00;
    MeasureHolder q02(q00);
    if (q00.asMDirection().getValue().getValue()(0) !=
	q01.asMDirection().getValue().getValue()(0) ||
	q00.asMDirection().getValue().getValue()(0) !=
        q02.asMDirection().getValue().getValue()(0)) {
      cout << "Error in copy constructor or assignment" << endl;
    };

    cout << "Is measure:        " << q00.isMeasure() << endl;
    cout << "Is direction:      " << q00.isMDirection() << endl;
    cout << "Is empty:          " << q00.isEmpty() << endl;
    cout << "Is epoch:          " << q00.isMEpoch() << endl;
    cout << "Is doppler:        " << q00.isMDoppler() << endl;
    cout << "Is position:       " << q00.isMPosition() << endl;
    cout << "Is frequency:      " << q00.isMFrequency() << endl;
    cout << "Is radialvelocity: " << q00.isMRadialVelocity() << endl;
    cout << "As measure:        " << q00.asMeasure() << endl;
    cout << "As direction:      " << q00.asMDirection() << endl;

    cout << "Error expected:" << endl;
    cout << "Input value:              " << x00 << " (" <<
	  x00.getRefString() << ")" << endl;
    if (MeasureHolder(x00).toRecord(error, y00)) {
      y00.renameField("units", RecordFieldId("refer"));
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	  q00.asMeasure().getRefString() << ")" << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  try {

    String error;
    MeasureHolder q00;
    MDirection x00(Quantity(30, "deg"), Quantity(-40, "deg"));
    Record y00;
    GlishRecord z00;

    cout << "Input value:              " << x00 << " (" <<
	  x00.getRefString() << ")" << endl;
    if (MeasureHolder(x00).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	  q00.asMeasure().getRefString() << ")" << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
    cout << "As epoch:      " << q00.asMEpoch() << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  try {
    {
      String error;
      MeasureHolder q00;
      MEpoch x00(Quantity(30456, "d"));
      Record y00;
      GlishRecord z00;
      
      cout << "Input value:              " << x00 << " (" <<
	x00.getRefString() << ")" << endl;
      if (MeasureHolder(x00).toRecord(error, y00)) {
	if (q00.fromRecord(error, y00)) {
	  cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	    q00.asMeasure().getRefString() << ")" << endl;
	} else {
	  cout << "From error: " << error << endl;
	};
      } else {
	cout << "To error: " << error << endl;
      };
      cout << "As epoch:      " << q00.asMEpoch() << endl;
    }      
    {
      String error;
      MeasureHolder q00;
      MDoppler x00(Quantity(30456, "m/s"));
      Record y00;
      GlishRecord z00;
      
      cout << "Input value:              " << x00 << " (" <<
	x00.getRefString() << ")" << endl;
      if (MeasureHolder(x00).toRecord(error, y00)) {
	if (q00.fromRecord(error, y00)) {
	  cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	    q00.asMeasure().getRefString() << ")" << endl;
	} else {
	  cout << "From error: " << error << endl;
	};
      } else {
	cout << "To error: " << error << endl;
      };
      cout << "As Doppler:      " << q00.asMDoppler() << endl;
    }      
    {
      String error;
      MeasureHolder q00;
      MFrequency x00(Quantity(30456, "MHz"));
      Record y00;
      GlishRecord z00;
      
      cout << "Input value:              " << x00 << " (" <<
	x00.getRefString() << ")" << endl;
      if (MeasureHolder(x00).toRecord(error, y00)) {
	if (q00.fromRecord(error, y00)) {
	  cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	    q00.asMeasure().getRefString() << ")" << endl;
	} else {
	  cout << "From error: " << error << endl;
	};
      } else {
	cout << "To error: " << error << endl;
      };
      cout << "As Frequency:      " << q00.asMFrequency() << endl;
    }      
    {
      String error;
      MeasureHolder q00;
      MPosition x00(Quantity(6, "Mm"), Quantity(20, "deg"),
		    Quantity(30, "deg"));
      Record y00;
      GlishRecord z00;
      
      cout << "Input value:              " << x00 << " (" <<
	x00.getRefString() << ")" << endl;
      if (MeasureHolder(x00).toRecord(error, y00)) {
	if (q00.fromRecord(error, y00)) {
	  cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	    q00.asMeasure().getRefString() << ")" << endl;
	} else {
	  cout << "From error: " << error << endl;
	};
      } else {
	cout << "To error: " << error << endl;
      };
      cout << "As Position:      " << q00.asMPosition() << endl;
    }      
    {
      String error;
      MeasureHolder q00;
      MRadialVelocity x00(Quantity(30456, "m/s"));
      Record y00;
      GlishRecord z00;
      
      cout << "Input value:              " << x00 << " (" <<
	x00.getRefString() << ")" << endl;
      if (MeasureHolder(x00).toRecord(error, y00)) {
	if (q00.fromRecord(error, y00)) {
	  cout <<"Record output value:      " << q00.asMeasure() << " (" <<
	    q00.asMeasure().getRefString() << ")" << endl;
	} else {
	  cout << "From error: " << error << endl;
	};
      } else {
	cout << "To error: " << error << endl;
      };
      cout << "As RadialVelocity:      " << q00.asMRadialVelocity() << endl;
    }      

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  exit(0);
}
