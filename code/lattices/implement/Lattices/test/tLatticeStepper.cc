//# tLatticeStepper.cc:  mechanical test of LatticeLayout class
//# Copyright (C) 1995,1996,1997,1998
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

#include <aips/aips.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

int main()
{
  try {
    // typical lattice and stepper specifications
    IPosition latticeShape(4,10,12,4,7);
    IPosition stepperShape(1,4);
    IPosition stepperOrientation(4,0,1,2,3);
    // Check the exception handling
    LogIO logger(LogOrigin("tLatticeStepper"));
    logger << "Expect to see a number of SEVERE errors logged shortly"
 	   << " (if in Debug mode)"
 	   << LogIO::POST;
    {
      IPosition badCursor(4,1); 
      IPosition smallLatticeShape(3, latticeShape(0), latticeShape(1),
				  latticeShape(2));
      try { // test the check for an bad cursor dimension
 	LatticeStepper demented(smallLatticeShape, badCursor);
      } catch (AipsError x) {
 	if (!x.getMesg().contains("more axes than lattice")) {
 	  cout << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      if (aips_debug) { // these checks are only done in debug mode.
	IPosition bigCursor(4,12,1,1,1);
	try { // test the check for an bad cursor size (upper bound exceeded)
	  LatticeStepper demented(latticeShape, bigCursor, stepperOrientation);
	} catch (AipsError x) {
	  if (!x.getMesg().contains("> latticeShape")) {
	    cout << x.getMesg() << endl << "FAIL" << endl;
	    return 1;
	  }
	} end_try;
	IPosition zeroCursor(4,0);
	try { // test the check for an bad cursor size (lower bound exceeded)
	  LatticeStepper demented(latticeShape, zeroCursor,stepperOrientation);
	} catch (AipsError x) {
 	  if (!x.getMesg().contains("cursorShape <=0")) {
	    cout << x.getMesg() << endl << "FAIL" << endl;
	    return 1;
	  }
	} end_try;
 	IPosition badOrientation(3,1,2,3);
 	try { // test the check for an bad orientation dimension
 	  LatticeStepper demented(latticeShape, stepperShape, badOrientation);
 	} catch (AipsError x) {
 	  if (!x.getMesg().contains("ok() == True")) {
 	    cout << x.getMesg() << endl << "FAIL" << endl;
 	    return 1;
 	  }
 	} end_try;
	IPosition badOrientation1(4,1,2,3,4);
	try { // test the check for an bad orientation bounds
	  LatticeStepper demented(latticeShape, stepperShape, badOrientation1);
	} catch (AipsError x) {
  	  if (!x.getMesg().contains("makeAxisPath")){
	    cout << x.getMesg() << endl << "FAIL" << endl;
	    return 1;
	  }
	} end_try;
	IPosition badOrientation2(4,0,2,2,3);
	try { // test the check for an bad orientation contents
	  LatticeStepper demented(latticeShape, stepperShape, badOrientation2);
	} catch (AipsError x) {
 	  if (!x.getMesg().contains("makeAxisPath")){
	    cout << x.getMesg() << endl << "FAIL" << endl;
	    return 1;
	  }
	} end_try;
      }
    }
    logger << "End of section which checks error detection" 
 	   << LogIO::POST;
    uInt count = 0;
    // Try the simplest thing moving forward with a one-dimensional congruent
    // cursor
    LatticeStepper huh(latticeShape, IPosition(1,10));
    for (huh.reset(); !huh.atEnd(); huh++)
      count++;
    // Check the cursor is at the end
    AlwaysAssert(huh.endPosition().isEqual(IPosition(4,9,11,3,6)),
 		 AipsError);
    AlwaysAssert(count == 12*4*7, AipsError);
    AlwaysAssert(huh.nsteps() == 12*4*7, AipsError);
    
    // Now move back again with the same cursor
    for ( ; !huh.atStart(); huh--)
      count--;
    // Should end up where we started
    AlwaysAssert(huh.position().isEqual(IPosition(4,0)), AipsError);
    AlwaysAssert(count == 0, AipsError);
    AlwaysAssert(huh.nsteps() == 2*12*4*7, AipsError);
    
    // Test moving forward with a one-dimensional NON-congruent cursor
    // Check the first few steps manually;
    LatticeStepper s0(latticeShape, stepperShape, stepperOrientation);
    AlwaysAssert(s0.position().isEqual(IPosition(4,0,0,0,0)), AipsError);
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,3,0,0,0)), 
		 AipsError);
    s0++; 
    AlwaysAssert(s0.position().isEqual(IPosition(4,4,0,0,0)), AipsError);
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,7,0,0,0)), 
		 AipsError);
    s0++; 
    AlwaysAssert(s0.position().isEqual(IPosition(4,8,0,0,0)), AipsError);
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,9,0,0,0)), 
		 AipsError);
    s0++;
    AlwaysAssert(s0.position().isEqual(IPosition(4,0,1,0,0)), AipsError);
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,3,1,0,0)), 
		 AipsError);
    s0++;
    AlwaysAssert(s0.position().isEqual(IPosition(4,4,1,0,0)), AipsError);
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,7,1,0,0)), 
		 AipsError);

    count = 0;
    for (s0.reset(); !s0.atEnd(); s0++)
      count++;
    // Check the cursor is at the end (note the overhang)
    AlwaysAssert(s0.endPosition().isEqual(IPosition(4,9,11,3,6)),
 		 AipsError);
    AlwaysAssert(count == 12*4*7*3, AipsError);
 
    for (; !s0.atStart(); s0--)
      count--;
    // Should end up where we started
    AlwaysAssert(s0.position().isEqual(IPosition(4,0)), AipsError);
    AlwaysAssert(count == 0, AipsError);
    AlwaysAssert(s0.nsteps() == 12*4*7*3*2, AipsError);

    // Test the copy constructor and assignment operator use copy semantics
    LatticeStepper s1(s0);
    s1++;
    LatticeStepper s2(s0); // Cannot use the default constructor;
    s2 = s1;
    s2++;
    
    AlwaysAssert(s0.position().isEqual(IPosition(4,0)), AipsError);
    AlwaysAssert(s1.position().isEqual(IPosition(4,4,0,0,0)), AipsError);
    AlwaysAssert(s2.position().isEqual(IPosition(4,8,0,0,0)), AipsError);
    // Check the hangover function
    AlwaysAssert(s0.hangOver() == False, AipsError);
    AlwaysAssert(s1.hangOver() == False, AipsError);
    AlwaysAssert(s2.hangOver() == True, AipsError);
    // Check that things work with the RESIZE cursor
    LatticeStepper s3(latticeShape, stepperShape, LatticeStepper::RESIZE);
    AlwaysAssert(s3.hangOver() == False, AipsError);
    AlwaysAssert(s3.position() == IPosition(4,0), AipsError);
    AlwaysAssert(s3.endPosition() == IPosition(4,3,0,0,0), AipsError);
    AlwaysAssert(s3.cursorShape() == IPosition(4,4,1,1,1), AipsError);
    s3++; 
    s3++;
    AlwaysAssert(s3.hangOver() == True, AipsError);
    AlwaysAssert(s3.position() == IPosition(4,8,0,0,0), AipsError);
    AlwaysAssert(s3.endPosition() == IPosition(4,9,0,0,0), AipsError);
    AlwaysAssert(s3.cursorShape() == IPosition(4,2,1,1,1), AipsError);

    LatticeStepper s4(latticeShape, stepperShape, stepperOrientation, 
		      LatticeStepper::RESIZE);
    s4.subSection(IPosition(4,1,0,0,0), IPosition(4,9,0,0,0), 
		  IPosition(4,2,1,1,1));
    AlwaysAssert(s4.hangOver() == False, AipsError);
    AlwaysAssert(s4.relativePosition() == IPosition(4,0), AipsError);
    AlwaysAssert(s4.position() == IPosition(4,1,0,0,0), AipsError);
    AlwaysAssert(s4.relativeEndPosition() == IPosition(4,3,0,0,0), AipsError);
    AlwaysAssert(s4.endPosition() == IPosition(4,7,0,0,0), AipsError);
    AlwaysAssert(s4.cursorShape() == IPosition(4,4,1,1,1), AipsError);
    s4++; 
    AlwaysAssert(s4.hangOver() == True, AipsError);
    AlwaysAssert(s4.relativePosition() == IPosition(4,4,0,0,0), AipsError);
    AlwaysAssert(s4.position() == IPosition(4,9,0,0,0), AipsError);
    AlwaysAssert(s4.relativeEndPosition() == IPosition(4,4,0,0,0), AipsError);
    AlwaysAssert(s4.endPosition() == IPosition(4,9,0,0,0), AipsError);
    AlwaysAssert(s4.cursorShape() == IPosition(4,1,1,1,1), AipsError);
    
    // Check the latticeshape, cursorShape & orientation functions
    AlwaysAssert(s1.latticeShape() == latticeShape, AipsError);
    AlwaysAssert(s1.cursorShape().nonDegenerate() == stepperShape, AipsError);
    AlwaysAssert(s2.axisPath() == stepperOrientation, AipsError);

    LatticeStepper method(IPosition(3,4,5,6),IPosition(2,4,5));
    LatticeNavigator* clonePtr = method.clone();
    AlwaysAssert(clonePtr != 0, AipsError);

    AlwaysAssert(clonePtr->ok() == True, AipsError);
    AlwaysAssert(clonePtr->latticeShape() == method.latticeShape(), AipsError);
    AlwaysAssert(clonePtr->cursorShape() == method.cursorShape(),
		 AipsError);
    AlwaysAssert(clonePtr->position() == method.position(), AipsError);
    AlwaysAssert(clonePtr->nsteps() == method.nsteps(), AipsError);
    AlwaysAssert(clonePtr->atStart() == method.atStart(), AipsError);
    AlwaysAssert(clonePtr->atEnd() == method.atEnd(), AipsError);
    AlwaysAssert(clonePtr->hangOver() == method.hangOver(), AipsError);
    //  LatticeStepper method(IPosition(3,4,5,6),IPosition(2,4,5));
    method.setCursorShape(IPosition(1,4));
    method.subSection(IPosition(3,0,1,0), IPosition(3,3,4,3),
 			IPosition(3,1,2,3));
    LatticeNavigator* stepPtr = method.clone();
    AlwaysAssert(stepPtr->blc() == IPosition(3,0,1,0), AipsError);
    AlwaysAssert(stepPtr->trc() == IPosition(3,3,3,3), AipsError);
    AlwaysAssert(stepPtr->increment() == IPosition(3,1,2,3), AipsError);
    AlwaysAssert(stepPtr->subLatticeShape() == IPosition(3,4,2,2), AipsError);
    AlwaysAssert(stepPtr->relativePosition() == IPosition(3,0), AipsError);
    AlwaysAssert(stepPtr->position() == IPosition(3,0,1,0), AipsError);
    AlwaysAssert(stepPtr->relativeEndPosition() == IPosition(3,3,0,0), 
		 AipsError);
    AlwaysAssert(stepPtr->endPosition() == IPosition(3,3,1,0), AipsError);
    delete clonePtr;
    delete stepPtr;
  } catch  (AipsError x) {
    cout << x.getMesg() << endl << "FAIL" << endl;
    return 1;
  } end_try;

  cout << "OK" << endl;
  return 0;
}
// Local Variables:
// compile-command: "gmake OPTLIB=1 tLatticeStepper"
// End:
