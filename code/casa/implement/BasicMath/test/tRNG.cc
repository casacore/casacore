#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <iostream.h>
#include <iomanip.h>

#include <aips/Mathematics/Random.h>

int main() {
  try {
    uInt i;
    Float f;
    Double d;
    cout << "testing the MLCG generator" << endl;
    {
      cout << "random integers, floats & doubles" << endl; 
      MLCG g;
      for (uInt k = 0; k < 4; k++) {
	// Note the values are calculated here rather than in the print
	// statemement because of the problem discussed in
	// http://aips2.nrao.edu/mail/aips2-lib/1391
 	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i
 	     << ": " << setprecision(6) << f
 	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
 	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
 	cout << k << ": " << setbase(16) << i
 	     << ": " << setprecision(6) << f
 	     << ": " << setprecision(12) << d << endl;
      }
    }
    {
      cout << "Using a different seed" << endl; 
      MLCG g(1, 0);
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
    cout << "testing the ACG generator" << endl;
    {
      cout << "random integers, floats & doubles" << endl; 
      ACG g;
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
    {
      cout << "Using a different seed" << endl; 
      ACG g(7326458, 98);
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake tRNG"
// End: 
