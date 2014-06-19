//# tProfileFit1D.cc: test the ProfileFit1D class
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2004
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

#include <casa/aips.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/PtrHolder.h>
#include <components/SpectralComponents/PolynomialSpectralElement.h>

#include <components/SpectralComponents/GaussianSpectralElement.h>
#include <components/SpectralComponents/GaussianMultipletSpectralElement.h>
#include <components/SpectralComponents/SpectralElementFactory.h>

#include <casa/Utilities/Assert.h>
#include <casa/Arrays/ArrayIO.h>

#include <casa/Arrays/Vector.h>

#include <casa/iostream.h>

#include <casa/namespace.h>

#include <vector>

int main() {

	{
		cout << "Test that exception is thrown if there is "
			<< "a mismatch between number of Gaussians and "
			<< "number of relationships between them" << endl;
		GaussianSpectralElement g1(4,4,4);
		GaussianSpectralElement g2(5,5,5);
		vector<GaussianSpectralElement> doublet(2);
		doublet[0] = g1;
		doublet[1] = g2;
		Matrix<Double> relations(2, 3, 0);
		relations(0,0) = 4;
		Bool result = True;
		try {
			GaussianMultipletSpectralElement(doublet, relations);
			result = False;
		}
		catch (AipsError) {}
		AlwaysAssert(result, AipsError);
	}
	{
		cout << "Test that exception is thrown if relations "
			<< "does not have three columns" << endl;
		GaussianSpectralElement g1(4, 4, 4);
		GaussianSpectralElement g2(5, 5, 5);
		vector<GaussianSpectralElement> doublet(2);
		doublet[0] = g1;
		doublet[1] = g2;
		Matrix<Double> relations(1, 4, 0);
		relations(0,0) = 4;
		Bool result = True;
		try {
			GaussianMultipletSpectralElement(doublet, relations);
			result = False;
		}
		catch (AipsError) {}
		AlwaysAssert(result, AipsError);
	}
	{
		cout << "Test that exception is thrown if there is "
			<< "an amplitude fixing incompatibility" << endl;
		GaussianSpectralElement g1(4, 4, 4);
		GaussianSpectralElement g2(5, 5, 5);
		g2.fixAmpl();
		cout << "g1 " << g1 << endl;
		cout << "g2 " << g2 << endl;
		std::vector<GaussianSpectralElement> doublet(2);
		doublet[0] = g1;
		cout << "doub 0 " << doublet[0] << " " << &doublet[0]  << endl;
		cout << "doub 1 " << doublet[1] << " " << &doublet[1] << endl;

		doublet[1] = g2;
		cout << "doub 0 " << doublet[0] << " " << &doublet[0]  << endl;
		cout << "doub 1 " << doublet[1] << " " << &doublet[1] << endl;
		cout << "g1 " << g1 << endl;
				cout << "g2 " << g2 << endl;
		Matrix<Double> relations(1, 3, 0);
		relations(0,0) = 4;
		Bool result = True;
		try {
			GaussianMultipletSpectralElement(doublet, relations);
			result = False;
		}
		catch (AipsError x) {}
		AlwaysAssert(result, AipsError);
	}
	{
		cout << "Test that exception is thrown if there is "
			<< "a center fixing incompatibility" << endl;
		GaussianSpectralElement g1(4, 4, 4);
		GaussianSpectralElement g2(5, 5, 5);
		g2.fixCenter();
		vector<GaussianSpectralElement> doublet(2);
		doublet[0] = g1;
		doublet[1] = g2;
		Matrix<Double> relations(1, 3, 0);
		relations(0,1) = 4;
		Bool result = True;
		try {
			GaussianMultipletSpectralElement(doublet, relations);
			result = False;
		}
		catch (AipsError x) {}
		AlwaysAssert(result, AipsError);
	}
	{
		cout << "Test that exception is thrown if there is "
			<< "a width fixing incompatibility" << endl;
		GaussianSpectralElement g1(4, 4, 4);
		GaussianSpectralElement g2(5, 5, 5);
		g2.fixFWHM();
		vector<GaussianSpectralElement> doublet(2);
		doublet[0] = g1;
		doublet[1] = g2;
		Matrix<Double> relations(1, 3, 0);
		relations(0, 2) = 4;
		Bool result = True;
		try {
			GaussianMultipletSpectralElement(doublet, relations);
			result = False;
		}
		catch (AipsError x) {}
		AlwaysAssert(result, AipsError);
	}
	{
		cout << "Test gaussians were correctly constructed " << endl;
		GaussianSpectralElement g1(4, 4.4, 4.6);
		GaussianSpectralElement g2(5, 5.2, 5.8);
		vector<GaussianSpectralElement> gaussians(2);
		gaussians[0] = g1;
		gaussians[1] = g2;
		Matrix<Double> relations(1, 3, 0);
		Double ampRatio = 4.5;
		relations(0, 0) = ampRatio;
		GaussianMultipletSpectralElement doublet(gaussians, relations);
		for (uInt i=0; i<gaussians.size(); i++) {
			Vector<Double> expected = gaussians[i].get();
			if (i==1) {
				expected[0] = ampRatio*gaussians[0].getAmpl();
			}
			Vector<Double> got = doublet.getGaussians()[i].get();
			cout << "*** got " << got << endl;
			cout << "*** exp " << expected << endl;
			AlwaysAssert(allTrue(got == expected), AipsError);
		}

		relations = 0;
		Double centerOff = -20;
		relations(0, 1) = centerOff;
		doublet = GaussianMultipletSpectralElement(gaussians, relations);
		for (uInt i=0; i<gaussians.size(); i++) {
			Vector<Double> expected = gaussians[i].get();
			if (i==1) {
				expected[1] = centerOff + gaussians[0].getCenter();
			}
			Vector<Double> got = doublet.getGaussians()[i].get();
			AlwaysAssert(allTrue(got == expected), AipsError);
		}
		relations = 0;
		Vector<Double> x(5, 2);
		Double sigmaRatio = 0.5;
		relations(0, 2) = sigmaRatio;
		doublet = GaussianMultipletSpectralElement(gaussians, relations);
		for (uInt i=0; i<gaussians.size(); i++) {
			Vector<Double> expected = gaussians[i].get();
			if (i==1) {
				expected[2] = sigmaRatio * gaussians[0].getSigma();
			}
			Vector<Double> got = doublet.getGaussians()[i].get();
			AlwaysAssert(allTrue(got == expected), AipsError);
		}
	}
	{
		cout << "Test toRecord()/fromRecord()" << endl;
		GaussianSpectralElement g1(4.6, 4.5, 4.4);
		GaussianSpectralElement g2(5.6, 5.2, 5.5);
		vector<GaussianSpectralElement> gaussians(2);
		gaussians[0] = g1;
		gaussians[1] = g2;
		Matrix<Double> relations(1, 3, 0);
		relations(0, 2) = 4;
		GaussianMultipletSpectralElement doublet(gaussians, relations);
		Record myRec;
		cout << "doublet real " << doublet << endl;
		doublet.toRecord(myRec);
		cout << "myrec " << myRec << endl;
        cout << __FILE__ << " " << __LINE__ << endl;
		PtrHolder<SpectralElement> ptr(SpectralElementFactory::fromRecord(myRec));
        cout << __FILE__ << " " << __LINE__ << endl;
		GaussianMultipletSpectralElement out = *dynamic_cast<GaussianMultipletSpectralElement*>(
			ptr.ptr()
		);
        cout << __FILE__ << " " << __LINE__ << endl;
		cout << "out " << out << endl;
		cout << "doublet " << doublet << endl;
		AlwaysAssert(out == doublet, AipsError);
	}
	{
		cout << "Test setting/getting" << endl;
		GaussianSpectralElement g1(4.6, 4.5, 4.4);
		GaussianSpectralElement g2(5.6, 5.2, 5.5);
		GaussianSpectralElement g3(6.6, 6.2, 6.5);

		Vector<Double> errs(3, 0);
		errs[0] = 0.1;
		errs[1] = 0.2;
		errs[2] = 0.3;
		g1.setError(errs);
		Vector<Bool> fixed(3, False);
		fixed[1] = True;
		g1.fix(fixed);


		vector<GaussianSpectralElement> gaussians(3);
		gaussians[0] = g1;
		gaussians[1] = g2;
		gaussians[2] = g3;

		Matrix<Double> relations(2, 3, 0);
		relations(0, 2) = 4;
		relations(1, 1) = 10;
		GaussianMultipletSpectralElement triplet(gaussians, relations);

		Vector<Double> g(7, 0);
		uInt j = 0;
		for (uInt i=0; i<9; i++) {
			if (i != 5 && i != 7) {
				g[j] = gaussians[i/3].get()[i%3];
				j++;
			}
		}
		AlwaysAssert(allNear(triplet.get(), g, 1e-8), AipsError);
		AlwaysAssert(
			allNear(
				gaussians[0].get(), triplet.getGaussians()[0].get(), 1e-8
			), AipsError
		);
		AlwaysAssert(
			allNear(
				gaussians[0].getError(), triplet.getGaussians()[0].getError(), 1e-8
			), AipsError
		);
		AlwaysAssert(
			allTrue(
				gaussians[0].fixed() == triplet.getGaussians()[0].fixed()
			), AipsError
		);
		Vector<Double> z = gaussians[1].get();
		z[2] = relations(0, 2) * gaussians[0].getSigma();
		Vector<Double> err = gaussians[1].getError();
		err[2] = relations(0, 2) * gaussians[0].getSigmaErr();
		AlwaysAssert(
			allNear(
				z, triplet.getGaussians()[1].get(), 1e-8
			), AipsError
		);
		AlwaysAssert(
			allNear(
				err, triplet.getGaussians()[1].getError(), 1e-8
			), AipsError
		);
		AlwaysAssert(
			allTrue(
				gaussians[1].fixed() == triplet.getGaussians()[1].fixed()
			), AipsError
		);
		z = gaussians[2].get();
		z[1] = relations(1, 1) + gaussians[0].getCenter();
		err = gaussians[2].getError();
		err[1] = gaussians[0].getCenterErr();
		AlwaysAssert(
			allNear(
				z, triplet.getGaussians()[2].get(), 1e-8
			), AipsError
		);
		AlwaysAssert(
			allNear(
				err, triplet.getGaussians()[2].getError(), 1e-8
			), AipsError
		);
		cout << gaussians[2].fixed() << endl;
		cout << triplet.getGaussians()[2].fixed() << endl;
		cout << fixed << endl;
		AlwaysAssert(
			allTrue(
				triplet.getGaussians()[2].fixed() == fixed
			), AipsError
		);
		cout << triplet.getFunction() << endl;
		Vector<Double> parms(7);
		for (uInt i=0; i < parms.size(); i++) {
			parms[i] = 10+i;
		}
		triplet.set(parms);

		AlwaysAssert(
			allNear(
				triplet.get(), parms, 1e-5
			), AipsError
		);
		Vector<Double> exp(3);
		exp[0] = parms[0];
		exp[1] = parms[1];
		exp[2] = parms[2];
		AlwaysAssert(
			allNear(
				triplet.getGaussians()[0].get(), exp, 1e-5
			), AipsError
		);
		exp[0] = parms[3];
		exp[1] = parms[4];
		exp[2] = relations(0, 2) * parms[2];
		AlwaysAssert(
			allNear(
				triplet.getGaussians()[1].get(), exp, 1e-5
			), AipsError
		);
		exp[0] = parms[5];
		exp[1] = relations(1, 1) + parms[1];
		exp[2] = parms[6];
		AlwaysAssert(
			allNear(
				triplet.getGaussians()[2].get(), exp, 1e-5
			), AipsError
		);





	}
	cout << "ok" << endl;
	return 0;


}
