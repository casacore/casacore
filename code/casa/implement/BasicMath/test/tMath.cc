#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <aips/Mathematics/Math.h>

int main() {
  try {
    {
      Float x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Double x = floatNaN();
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Float x = doubleNaN();
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Double x;
      setNaN(x);
      AlwaysAssert(isNaN(x), AipsError);
    }
    {
      Float x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Double x = floatInf();
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Float x = doubleInf();
      AlwaysAssert(isInf(x), AipsError);
    }
    {
      Double x;
      setInf(x);
      AlwaysAssert(isInf(x), AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tMath"
// End: 
