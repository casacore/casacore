
// Test program for class StatAcc

#include <iostream.h>
#include <aips/Arrays.h>
#include <trial/Mathematics/StatAcc.h>
// #include <aips/Utilities/Assert.h>
// #include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>


main()
{
    try {

      Int v1 = -10;
      Int v2 = 9;
      Int v3 = 1;
      uInt nv = 0;                     // nr ov test-values
      Int v;
      for (v=v1; v<=v2; v+=v3) {nv++;}  
      cout << "value-vector vv:  nv=" << nv;
      cout << "  v1=" << v1;
      cout << "  v2=" << v2;
      cout << "  v3=" << v3;
      cout << endl;
      cout << "weight-vector ww: 0,nv,1" << endl;

// For testing other types, change them here:

      String caption(" tStatAcc test for <Int>");
      StatAcc<Int> s;               // accumulator
      StatAcc<Int> s1;                   
      Vector<Int> vv(nv,0);         // values of required type
      Block<Int> bv(nv);            // values of required type

      Vector<Float> ww(nv,0);          // weights are always Float
      Block<Float> bw(nv);             // weights are always Float
      
      Int i=0;                         // index
      for (v=v1; v<=v2; v+=v3) {
	  vv(i) = v;                   // Array values
	  bv[i] = v;                   // Block values
	  ww(i) = i;                   // Array weights 
	  bw[i] = i;                   // Block weights 
	  i++;
      }

      s.printSummaryLineHeader(cout,caption);
      s.printSummaryLine(cout,"After initialisation");

      s.reset();
      s.printSummaryLine(cout,"After reset");
      for (i=0; i<20; i++) {
	  s.put(vv(i));
      }    
      s.printSummaryLine(cout,"vv(i)");

      s.reset();
      for (i=0; i<20; i++) {
	  s.put(vv(i),ww(i));
      }
      s.printSummaryLine(cout,"vv(i), weight=ww(i)");

      s.reset();
      s.put(vv);           
      s.printSummaryLine(cout,"vv");

      s.reset();
      s.put(bv);           
      s.printSummaryLine(cout,"bv");

      s.reset();
      s.put(vv,ww);           
      s.printSummaryLine(cout,"vv, weight=ww");
      s.put(vv,ww);           
      s.printSummaryLine(cout,"again");

      s.reset();
      s.put(bv,bw);           
      s.printSummaryLine(cout,"bv, weight=bw");

      s.reset();
      s.put(vv);
      s.put(-50);
      s.printSummaryLine(cout,"s=vv, plus extra -50");
      s1 = s;
      s1.put(100);
      s1.printSummaryLine(cout,"s1=s, plus extra 100");
      s1 += s;           
      s1.printSummaryLine(cout,"s1 += s");
      s1 = s1 + s;           
      s1.printSummaryLine(cout,"s1 = s1 + s");
      s1 = s + s1;           
      s1.printSummaryLine(cout,"s1 = s + s1");

      s.printSummaryLine(cout,"s");

      cout << "Test of s.get-functions: " << endl;
      cout << "  s.getWtot:    " << s.getWtot() << endl;
      cout << "  s.getCount:   " << s.getCount() << endl;
      cout << "  s.getMean:    " << s.getMean() << endl;
      cout << "  s.getRms:     " << s.getRms() << endl;
      cout << "  s.getVariance:" << s.getVariance() << endl;
      cout << "  s.getRmsAbs:  " << s.getRmsAbs() << endl;
      cout << "  s.getMin:     " << s.getMin() << endl;
      cout << "  s.getMax:     " << s.getMax() << endl;

      s.printSummaryList(cout,"Test of s.printSummaryList");


    } catch (AipsError x) {
        cout << x.getMesg() << endl;
        return 1;                       // unexpected error
    } end_try;
    return 0;                           // exit with success status
}






