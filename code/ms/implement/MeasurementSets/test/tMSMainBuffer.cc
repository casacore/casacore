#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <trial/MeasurementSet/MSMainBuffer.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Array.h>
#include <aips/Mathematics/Constants.h>

int main() {
  try {
    MSMainBuffer mainBuffer;
    { // test the correlation length functions
      AlwaysAssert(mainBuffer.numCorrelations() == 0, AipsError);
      mainBuffer.setCorrelations(4);
      AlwaysAssert(mainBuffer.numCorrelations() == 4, AipsError);
    }
    { // test the channel size functions
      AlwaysAssert(mainBuffer.numChannels() == 0, AipsError);
      mainBuffer.setChannels(32);
      AlwaysAssert(mainBuffer.numChannels() == 32, AipsError);
    }
    { // test the rows functions
      AlwaysAssert(mainBuffer.rows() == 0, AipsError);
      mainBuffer.addRows(5);
      AlwaysAssert(mainBuffer.rows() == 5, AipsError);
    }
    { // test the antenna1 functions
      for (uInt i = 0; i < mainBuffer.rows(); i++) {
	mainBuffer.antenna1(i) = Int(i) + 1;
      }
      Vector<Int> expectedResult(5);
      for (uInt i = 0; i < mainBuffer.rows(); i++) {
	AlwaysAssert(mainBuffer.antenna1(i) == Int(i+1), AipsError);
	expectedResult(i) = Int(i+1);
      }
      AlwaysAssert(allEQ(mainBuffer.antenna1().ac(), expectedResult.ac()), 
		   AipsError);
      expectedResult.ac() += 2;
      Vector<Int> test(mainBuffer.antenna1());
      test = expectedResult;
      AlwaysAssert(allEQ(mainBuffer.antenna1().ac(), expectedResult.ac()), 
		   AipsError);
    }
    { // test the complex data access functions
      AlwaysAssert(mainBuffer.data().shape().isEqual(IPosition(3,4,32,5)),
		   AipsError);
      Cube<Complex> data(mainBuffer.data());
      data = Complex(0.5, 1.1);
      AlwaysAssert(allNear(mainBuffer.data(),Complex(0.5,1.1), C::flt_epsilon),
		  AipsError);
      mainBuffer.data(0,0,0) = Complex(1.2,1.3);
      AlwaysAssert(allNear(mainBuffer.data(0,0,0),Complex(1.2,1.3), 
 			   C::flt_epsilon), AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tMSMainBuffer"
// End: 
