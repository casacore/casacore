//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <casa/aips.h>
#include <casa/Quanta/QLogical.h>
#include <components/ComponentModels/GaussianBeam.h>
#include <casa/Containers/Record.h>

#include <casa/namespace.h>


int main() {
	try {
		// null beam
		GaussianBeam null;
		AlwaysAssert(null.isNull(), AipsError);
		Quantity qzero(0, "rad");
		AlwaysAssert(null.getMajor() == qzero, AipsError);
		AlwaysAssert(null.getMinor() == qzero, AipsError);
		AlwaysAssert(null.getPA() == qzero, AipsError);
		// non null beam constructor
		Quantity majAx(4, "arcsec");
		Quantity minAx(3, "arcsec");
		Quantity pa(20, "deg");
		GaussianBeam beam(majAx, minAx, pa);
		AlwaysAssert(beam.getMajor() == majAx, AipsError);
		AlwaysAssert(beam.getMinor() == minAx, AipsError);
		AlwaysAssert(beam.getPA() == pa, AipsError);

		// copy constructor
		GaussianBeam beam2(beam);
		AlwaysAssert(beam2 == beam, AipsError);
		AlwaysAssert(beam2 != null, AipsError);

		// = operator
		beam2 = beam;
		AlwaysAssert(beam2 == beam, AipsError);
		AlwaysAssert(beam2 != null, AipsError);



		Bool except = False;
		try {
			// bogus units
			majAx = Quantity(4, "m");
			GaussianBeam beam3(majAx, minAx, pa);

		}
		catch (AipsError x) {
			cout << "Exception thrown as expected: " << x.getMesg() << endl;
			except = True;
		}
		AlwaysAssert(except, AipsError);
		except = False;
		try {
			// major smaller than minor
			majAx = Quantity(2, "arcsec");
			GaussianBeam beam3(majAx, minAx, pa);

		}
		catch (AipsError x) {
			cout << "Exception thrown as expected: " << x.getMesg() << endl;
			except = True;
		}
		AlwaysAssert(except, AipsError);

		// getArea
		majAx = Quantity(1, "arcsec");
		minAx = majAx;
		beam = GaussianBeam(majAx, minAx, pa);
		AlwaysAssert(beam.getArea("arcsec2") == Quantity(C::pi/4/C::ln2), AipsError);
		except = False;
		try {
			// bogus units
			beam.getArea("arcsec");
		}
		catch (AipsError x) {
			cout << "Exception thrown as expected: " << x.getMesg() << endl;
			except = True;
		}
		AlwaysAssert(except, AipsError);

		// to/from Record
		Record rec = beam.toRecord();
		beam2 = GaussianBeam::fromRecord(rec);
		AlwaysAssert(beam == beam2, AipsError);
		except = False;
		try {
			// bogus record
			rec.define("bogus", 5);
			beam2 = GaussianBeam::fromRecord(rec);

		}
		catch (AipsError x) {
			cout << "Exception thrown as expected: " << x.getMesg() << endl;
			except = True;
		}
		AlwaysAssert(except, AipsError);
		Vector<Quantity> v(3);
		v[0] = majAx;
		v[1] = minAx;
		v[2] = pa;
		GaussianBeam beam3(v);
		AlwaysAssert(beam3 == beam, AipsError);

		v = beam.toVector();
		GaussianBeam beam4(v);
		AlwaysAssert(beam4 == beam3, AipsError);
		const Unit rad("rad");
		{
			// Easy test 1 - P.A. = 0
			Angular2DGaussian source(
				Quantity(20, "arcsec"),
				Quantity(10, "arcsec"),
				Quantity(0, "deg")
			);
		    GaussianBeam beam(
		    	Quantity(15, "arcsec"),
		    	Quantity(5, "arcsec"),
		    	Quantity(0, "deg")
		    );
		    Angular2DGaussian model;
		    Double maj = sqrt(
		    	square(source.getMajor(rad))
		    	- square(beam.getMajor(rad))
		    );
		    Quantity majQ(maj, rad);
		    majQ.convert(source.getMajor().getFullUnit());

		    Double min = sqrt(square(source.getMinor(rad)) -
		                        square(beam.getMinor(rad)));
		    Quantity minQ(min, rad);
		    minQ.convert(source.getMinor().getFullUnit());

		    Quantity paQ(0.0,source.getPA().getFullUnit());
		    Angular2DGaussian expected(majQ, minQ, paQ);

		    Bool isPoint = beam.deconvolve(model, source);
		    cout << "Source   = " << source << endl;
		    cout << "Beam     = " << beam << endl;
		    cout << "Model    = " << model << endl;
		    cout << "Expected = " << expected << endl;
		    cout << "isPoint  = " << isPoint << endl << endl;
		    AlwaysAssert(!isPoint, AipsError);
			AlwaysAssert(near(expected, model,1e-6, Quantity(0.001, "arcsec")), AipsError);
		}
		{
			// Easy test 2 - P.A. aligned
			Angular2DGaussian source(
				Quantity(10, "arcsec"),
				Quantity(5, "arcsec"),
				Quantity(20, "deg")
		    );
			GaussianBeam beam(
				Quantity(8, "arcsec"),
				Quantity(4, "arcsec"),
				Quantity(20, "deg")
		    );

			Double maj = sqrt(
				square(
					source.getMajor(rad))
					- square(beam.getMajor(rad)
		    	)
		    );
			Quantity majQ(maj, rad);
			majQ.convert(source.getMajor().getFullUnit());

			Double min = sqrt(
				square(source.getMinor(rad))
				- square(beam.getMinor(rad))
		    );
			Quantity minQ(min, rad);
			minQ.convert(source.getMinor().getFullUnit());
			Quantity paQ(20, source.getPA().getFullUnit());

			Angular2DGaussian expected(majQ, minQ, paQ);
			Angular2DGaussian model;
			model.convert("arcsec", "arcsec", "deg");
			Bool isPoint = beam.deconvolve(model, source);
			cout << "Source   = " << source << endl;
			cout << "Beam     = " << beam << endl;
			cout << "Model    = " << model << endl;
			cout << "Expected = " << expected << endl;
			cout << "isPoint  = " << isPoint << endl << endl;
			AlwaysAssert(!isPoint, AipsError);
			AlwaysAssert(near(expected, model,1e-6, Quantity(1, "mas")), AipsError);
		}
		{
			// Easy test 3 - beam and source the same
			Angular2DGaussian source(
				Quantity(20, "arcsec"),
				Quantity(10, "arcsec"),
				Quantity(45, "deg")
			);
			GaussianBeam beam(
				Quantity(20.00001, "arcsec"),
				Quantity(10.00001, "arcsec"),
				Quantity(45, "deg")
			);
			Angular2DGaussian model;
			Bool isPoint = beam.deconvolve(model, source);
			cout << "Source   = " << source << endl;
			cout << "Beam     = " << beam << endl;
			cout << "Model    = " << model << endl;
			cout << "Expected = " << beam << endl;
			cout << "isPoint  = " << isPoint << endl << endl;
			AlwaysAssert(isPoint, AipsError);
			AlwaysAssert(near(beam, model,1e-6, Quantity(1, "mas")), AipsError);
		}
		{
			// Not so easy test 4
			Angular2DGaussian source(
				Quantity(4, "arcsec"),
				Quantity(3, "arcsec"),
				Quantity(20, "deg")
			);
			GaussianBeam beam(
				Quantity(3, "arcsec"),
				Quantity(2, "arcsec"),
				Quantity(50, "deg")
			);
			Angular2DGaussian expected(
				Quantity(3.0203474964313628, "arcsec"),
				Quantity(1.6963198403605391, "arcsec"),
				Quantity(-1.9489431240069859, "deg")
			);
			Angular2DGaussian model;
			model.convert("arcsec", "arcsec", "deg");
			Bool isPoint = beam.deconvolve(model, source);
			cout << "Source   = " << source << endl;
			cout << "Beam     = " << beam << endl;
			cout << "Model    = " << model << endl;
			cout << "Expected = " << expected << endl;
			cout << "isPoint  = " << isPoint << endl << endl;
			AlwaysAssert(!isPoint, AipsError);
			AlwaysAssert(near(expected, model,1e-6, Quantity(1, "mas")), AipsError);
		}
	}
	catch (AipsError x) {
		cout << x.getMesg() << endl;
		cout << "FAIL" << endl;
		return 1;
	}
	cout << "OK" << endl;
	return 0;
}
