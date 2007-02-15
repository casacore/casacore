#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/Assert.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/OS/Timer.h>
#include <casa/BasicSL/Constants.h>

#include <scimath/Mathematics/VanVleck.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
Matrix<Double> qfn(Int nlevels, Double thresh, Double dcoff)
{
  // works for odd numbers of levels
  Matrix<Double> result(2,nlevels);
  for (Int i=0;i<(nlevels-1);i++) {
    result(0,i) = -(nlevels-2)*thresh+2*i*thresh-dcoff;
  }
  for (Int i=0;i<nlevels;i++) {
    result(1,i) = -(nlevels-1)/2 + i;
  }
  return result;
}

void showTable(const Vector<Double> rs, const Vector<Double> rhos,
	       VanVleck &vv)
{
  cout.precision(9);
  for (uInt i=0;i<rs.nelements();i++) {
    cout << i << " " << rs[i] << " " << rhos[i] 
	 << " : " << vv.r(rs[i]) << endl;
  }
}

void showThresh(VanVleck &vv, Int n, Double zerolag)
{
  zerolag *= 16.0;
  cout << n << " : " << zerolag << " -> " << vv.thresh(n, zerolag) << endl;
}

int main() {
  try {
    VanVleck vv;
    {
      // Optimal 3-level by 3-level quantization function:
      //  (I.e., the 3-level by 3-level quantization function for 
      //     zero-mean input signals with voltage thresholds set 
      //     at the optimal values of +/- ~0.612003 sigma.)

      Matrix<Double> qx, qy;
      //      qx = qfn(3,0.61200318096,0.0);
      //      qy = qx;
      //      vv.setQuantization(qx,qy);
      vv.setEquiSpaced(0.61200318096, 0.61200318096, 0.0, 0.0, 3);
      Vector<Double> rs, rhos;
      vv.getTable(rs,rhos);
      showTable(rs,rhos,vv);
      cout << "Prediction : "
	   << vv.predict(3,0.61200318096) << endl;
    }

    {

      //  Like the above, but if the threshold for the x-quantizer
      //     were set non-optimally at 0.6 sigma and there were a d.c.
      //     offset of 0.01 sigma in the x-inputs, and if the y-quantizer
      //     were set non-optimally at 0.7 sigma and there were a d.c.
      //     offset of -.02 sigma in the y-inputs

      Matrix<Double> qx, qy;
      //qx = qfn(3,.6,.01);
      //qy = qfn(3,.7,-.02);
      //vv.setQuantization(qx,qy);
      vv.setEquiSpaced(.6, .7, .01, -.02, 3);
      Vector<Double> rs, rhos;
      vv.getTable(rs,rhos);
      showTable(rs,rhos,vv);
    }

    {

      //  Optimal 3-level by 9-level quantization function:
      //  (I.e., no d.c. offsets, and thresholds set optimally for both
      //     the 3-level x-quantizer's input signal and the 9-level
      //     y-quantizer's input signal.)

      Matrix<Double> qx, qy;
      qx = qfn(3,0.61200318096,0.0);
      qy = qfn(9,0.26691110435,0.0);
      //vv.setQuantization(qx,qy);
      //Vector<Double> rs, rhos;
      //vv.getTable(rs,rhos);
      //showTable(rs,rhos,vv);
    }

    {

      //  Optimal 9-level by 9-level quantization function:
      //  (I.e., the 9-level by 9-level quantization function for 
      //     zero-mean input signals with voltage thresholds set 
      //     at the optimal values of +/- (2k-1)*0.266911 sigma, k=1,2,3,4.)

      Matrix<Double> qx, qy;
      //qx = qfn(9,0.26691110435,0.);
      //qy = qx;
      //vv.setQuantization(qx,qy);
      vv.setEquiSpaced(0.26691110435, 0.26691110435, 0.0, 0.0, 9);
      Vector<Double> rs, rhos;
      vv.getTable(rs,rhos);
      showTable(rs,rhos,vv);
      cout << "Prediction : "
	   << vv.predict(9,0.26691110435) << endl;
    }

    {
      // how long does it take to set up the interpolation fn
      // do it 100 times for the 9x9 optimized case
      Timer timer;
      Matrix<Double> qx, qy;
      // qx = qfn(9,0.26691110435,0.);
      // qy = qx;
      for (uInt i=0;i<100;i++) {
	  // vv.setQuantization(qx,qy);
	  vv.setEquiSpaced(0.26691110435, 0.26691110435, 0.0, 0.0, 9);
      }
      timer.show("Set up 9x9 100 times");
      timer.mark();
      // divide up -1 to 1 by 80000 segments and get corresponding one
      Double rho = -1.0;
      Double incr = 2.0/80001.0;
      Double r;
      for (uInt i=0;i<80001;i++) {
	r = vv.r(rho);
	rho += incr;
      }
      timer.show("After 80000 calls to ()");
      // the chebyshev polynomials for size=65 and
      // corresponding rs from vv interpolator
      Double twoN = 2.0*65.0;
      Double denom = cos(C::pi/twoN);
      for (uInt i=0;i<65;i++) {
	Double rho = -cos(Double(2*i+1)*C::pi/twoN)/denom;
	Double r = vv.r(rho);
	cout << i << " " << r << " " << rho << endl;
      }
    }

    {
      // how long does it take to set up the interpolation fn
      // do it 100 times for the 3x3 optimized case
      Timer timer;
      Matrix<Double> qx, qy;
      // qx = qfn(3,0.61200318096,0.0);
      // qy = qx;
      for (uInt i=0;i<100;i++) {
	  // vv.setQuantization(qx,qy);
	  vv.setEquiSpaced(0.61200318096, 0.61200318096, 0.0, 0.0, 3);
      }
      timer.show("Set up 3x3 100 times");
      timer.mark();
      // divide up -1 to 1 by 80000 segments and get corresponding one
      Double rho = -1.0;
      Double incr = 2.0/80001.0;
      Double r;
      for (uInt i=0;i<80001;i++) {
	r = vv.r(rho);
	rho += incr;
      }
      timer.show("After 80000 calls to ()");
    }

    {
      // some zero lags to check out
      showThresh(vv,9,0.570756);
      showThresh(vv,9,0.5700021);
      showThresh(vv,9,0.5781991);
      showThresh(vv,9,0.5776786);
      showThresh(vv,9,0.5750569);
      showThresh(vv,9,0.5743146);
      showThresh(vv,9,0.5788578);
      showThresh(vv,9,0.5784147);
      showThresh(vv,9,0.5792729);
      showThresh(vv,9,0.5786775);
      showThresh(vv,9,0.5791781);
      showThresh(vv,9,0.5788941);
      showThresh(vv,9,0.5839967);
      showThresh(vv,9,0.5834859);
      showThresh(vv,9,0.5823558);
      showThresh(vv,9,0.5821444);
      showThresh(vv,9,0.3521442);
      showThresh(vv,9,0.3550488);
      showThresh(vv,9,0.3824283);
      showThresh(vv,9,0.3856382);
      showThresh(vv,9,0.3602951);
      showThresh(vv,9,0.3632926);
      showThresh(vv,9,0.3799098);
      showThresh(vv,9,0.3831574);
      showThresh(vv,9,0.5692192);
      showThresh(vv,9,0.5680515);
      showThresh(vv,9,0.5817156);
      showThresh(vv,9,0.5808259);
      showThresh(vv,9,0.5764849);
      showThresh(vv,9,0.5750596);
      showThresh(vv,9,0.5776771);
      showThresh(vv,9,0.5765055);
      showThresh(vv,9,0.4078194);
      showThresh(vv,9,0.3736775);
      showThresh(vv,9,0.4172075);
      showThresh(vv,9,0.3840109);
      showThresh(vv,9,0.3968355);
      showThresh(vv,9,0.3628625);
      showThresh(vv,9,0.4120503);
      showThresh(vv,9,0.3788624);
      showThresh(vv,9,0.369375);
      showThresh(vv,9,0.3359462);
      showThresh(vv,9,0.4058436);
      showThresh(vv,9,0.3726806);
      showThresh(vv,9,0.4196527);
      showThresh(vv,9,0.3851627);
      showThresh(vv,9,0.4170341);
      showThresh(vv,9,0.3837453);
      showThresh(vv,9,0.334571);
      showThresh(vv,9,0.3447004);
      showThresh(vv,9,0.3494235);
      showThresh(vv,9,0.3602981);
      showThresh(vv,9,0.3617256);
      showThresh(vv,9,0.371273);
      showThresh(vv,9,0.3754921);
      showThresh(vv,9,0.3855272);
      showThresh(vv,9,0.3697195);
      showThresh(vv,9,0.3791553);
      showThresh(vv,9,0.3827082);
      showThresh(vv,9,0.3925593);
      showThresh(vv,9,0.3797117);
      showThresh(vv,9,0.3890676);
      showThresh(vv,9,0.3922659);
      showThresh(vv,9,0.4019398);
      showThresh(vv,9,0.3185588);
      showThresh(vv,9,0.3293913);
      showThresh(vv,9,0.3343834);
      showThresh(vv,9,0.3460897);
      showThresh(vv,9,0.3112122);
      showThresh(vv,9,0.321894);
      showThresh(vv,9,0.3273709);
      showThresh(vv,9,0.3390449);
      showThresh(vv,9,0.3029225);
      showThresh(vv,9,0.3131599);
      showThresh(vv,9,0.319591);
      showThresh(vv,9,0.3309426);
      showThresh(vv,9,0.2922837);
      showThresh(vv,9,0.3022471);
      showThresh(vv,9,0.3095268);
      showThresh(vv,9,0.3207578);
      showThresh(vv,9,0.921033);
      showThresh(vv,9,0.9143527);
      showThresh(vv,9,0.9207418);
      showThresh(vv,9,0.9137397);
      showThresh(vv,9,0.9209988);
      showThresh(vv,9,0.9143579);
      showThresh(vv,9,0.9206958);
      showThresh(vv,9,0.9137287);
      showThresh(vv,9,0.9190011);
      showThresh(vv,9,0.9704426);
      showThresh(vv,9,0.9174222);
      showThresh(vv,9,0.9698221);
      showThresh(vv,9,0.7692035);
      showThresh(vv,9,0.8275466);
      showThresh(vv,9,0.7629184);
      showThresh(vv,9,0.8213883);
      showThresh(vv,9,0.5394604);
      showThresh(vv,9,0.5656639);
      showThresh(vv,9,0.544381);
      showThresh(vv,9,0.5701309);
      showThresh(vv,9,0.5379332);
      showThresh(vv,9,0.5654352);
      showThresh(vv,9,0.5429364);
      showThresh(vv,9,0.5700172);
      showThresh(vv,9,0.5435538);
      showThresh(vv,9,0.5697622);
      showThresh(vv,9,0.5486179);
      showThresh(vv,9,0.5743307);
      showThresh(vv,9,0.5498347);
      showThresh(vv,9,0.5750396);
      showThresh(vv,9,0.5545064);
      showThresh(vv,9,0.5792525);
      showThresh(vv,9,0.3203699);
      showThresh(vv,9,0.359142);
      showThresh(vv,9,0.3347408);
      showThresh(vv,9,0.3744646);
      showThresh(vv,9,0.2933097);
      showThresh(vv,9,0.3306891);
      showThresh(vv,9,0.3089331);
      showThresh(vv,9,0.3478108);
      showThresh(vv,9,0.2961192);
      showThresh(vv,9,0.3336673);
      showThresh(vv,9,0.312097);
      showThresh(vv,9,0.3510777);
      showThresh(vv,9,0.3096372);
      showThresh(vv,9,0.3479083);
      showThresh(vv,9,0.3247232);
      showThresh(vv,9,0.3641273);
      showThresh(vv,9,0.5459048);
      showThresh(vv,9,0.5712026);
      showThresh(vv,9,0.5507359);
      showThresh(vv,9,0.5755962);
      showThresh(vv,9,0.3489026);
      showThresh(vv,9,0.341387);
      showThresh(vv,9,0.3715567);
      showThresh(vv,9,0.3716146);
      showThresh(vv,9,0.3431585);
      showThresh(vv,9,0.3354017);
      showThresh(vv,9,0.366536);
      showThresh(vv,9,0.3665553);
      showThresh(vv,9,0.3436162);
      showThresh(vv,9,0.3362611);
      showThresh(vv,9,0.3669193);
      showThresh(vv,9,0.3673983);
      showThresh(vv,9,0.3447839);
      showThresh(vv,9,0.3374888);
      showThresh(vv,9,0.3680037);
      showThresh(vv,9,0.3685214);
      showThresh(vv,9,0.359266);
      showThresh(vv,9,0.353186);
      showThresh(vv,9,0.3811512);
      showThresh(vv,9,0.3823101);
      showThresh(vv,9,0.3599611);
      showThresh(vv,9,0.3536995);
      showThresh(vv,9,0.3817651);
      showThresh(vv,9,0.3827734);
      showThresh(vv,9,0.3604349);
      showThresh(vv,9,0.3541578);
      showThresh(vv,9,0.3822528);
      showThresh(vv,9,0.3831535);
      showThresh(vv,9,0.361209);
      showThresh(vv,9,0.3549063);
      showThresh(vv,9,0.3829554);
      showThresh(vv,9,0.3838758);
      showThresh(vv,9,0.3530525);
      showThresh(vv,9,0.3403655);
      showThresh(vv,9,0.3767908);
      showThresh(vv,9,0.3668419);
      showThresh(vv,9,0.3532979);
      showThresh(vv,9,0.3409442);
      showThresh(vv,9,0.3770015);
      showThresh(vv,9,0.3674191);
      showThresh(vv,9,0.3543381);
      showThresh(vv,9,0.3419594);
      showThresh(vv,9,0.3779769);
      showThresh(vv,9,0.3683796);
      showThresh(vv,9,0.3544584);
      showThresh(vv,9,0.3417986);
      showThresh(vv,9,0.378097);
      showThresh(vv,9,0.368221);
      showThresh(vv,9,0.3394508);
      showThresh(vv,9,0.3260299);
      showThresh(vv,9,0.3647039);
      showThresh(vv,9,0.3541952);
      showThresh(vv,9,0.3392384);
      showThresh(vv,9,0.3259756);
      showThresh(vv,9,0.3644707);
      showThresh(vv,9,0.3541369);
      showThresh(vv,9,0.3420936);
      showThresh(vv,9,0.3293294);
      showThresh(vv,9,0.367372);
      showThresh(vv,9,0.3575394);
      showThresh(vv,9,0.3458747);
      showThresh(vv,9,0.3332081);
      showThresh(vv,9,0.3710668);
      showThresh(vv,9,0.3613658);
      showThresh(vv,9,0.2775754);
      showThresh(vv,9,0.274166);
      showThresh(vv,9,0.3000241);
      showThresh(vv,9,0.3013617);
      showThresh(vv,9,0.2779866);
      showThresh(vv,9,0.2747351);
      showThresh(vv,9,0.3003704);
      showThresh(vv,9,0.3018582);
      showThresh(vv,9,0.2779422);
      showThresh(vv,9,0.2748132);
      showThresh(vv,9,0.3003416);
      showThresh(vv,9,0.3019316);
      showThresh(vv,9,0.2782591);
      showThresh(vv,9,0.2750526);
      showThresh(vv,9,0.3006591);
      showThresh(vv,9,0.3021792);
      showThresh(vv,9,0.2641738);
      showThresh(vv,9,0.2611372);
      showThresh(vv,9,0.2877783);
      showThresh(vv,9,0.289629);
      showThresh(vv,9,0.2641329);
      showThresh(vv,9,0.2610094);
      showThresh(vv,9,0.2877155);
      showThresh(vv,9,0.289522);
      showThresh(vv,9,0.2648556);
      showThresh(vv,9,0.2616547);
      showThresh(vv,9,0.2884237);
      showThresh(vv,9,0.2901475);
      showThresh(vv,9,0.2652715);
      showThresh(vv,9,0.2619487);
      showThresh(vv,9,0.2888355);
      showThresh(vv,9,0.2904285);
      showThresh(vv,9,0.2822566);
      showThresh(vv,9,0.2664075);
      showThresh(vv,9,0.3056722);
      showThresh(vv,9,0.2893118);
      showThresh(vv,9,0.2820567);
      showThresh(vv,9,0.266308);
      showThresh(vv,9,0.3053422);
      showThresh(vv,9,0.2891054);
      showThresh(vv,9,0.2820061);
      showThresh(vv,9,0.2661623);
      showThresh(vv,9,0.3052764);
      showThresh(vv,9,0.2889432);
      showThresh(vv,9,0.2671696);
      showThresh(vv,9,0.2508954);
      showThresh(vv,9,0.2917009);
      showThresh(vv,9,0.2749783);
      showThresh(vv,9,0.2669829);
      showThresh(vv,9,0.2505649);
      showThresh(vv,9,0.2915318);
      showThresh(vv,9,0.2745961);
      showThresh(vv,9,0.2667687);
      showThresh(vv,9,0.2501054);
      showThresh(vv,9,0.2913115);
      showThresh(vv,9,0.2741979);
      showThresh(vv,9,0.1216316);
      showThresh(vv,9,0.1131639);
      showThresh(vv,9,0.1322801);
      showThresh(vv,9,0.1241305);
      showThresh(vv,9,0.1217624);
      showThresh(vv,9,0.1133628);
      showThresh(vv,9,0.1324191);
      showThresh(vv,9,0.1243321);
      showThresh(vv,9,0.1220046);
      showThresh(vv,9,0.1134972);
      showThresh(vv,9,0.1326797);
      showThresh(vv,9,0.1244635);
      showThresh(vv,9,0.1222211);
      showThresh(vv,9,0.1136796);
      showThresh(vv,9,0.1329199);
      showThresh(vv,9,0.1246549);
      showThresh(vv,9,0.122408);
      showThresh(vv,9,0.1138413);
      showThresh(vv,9,0.1331067);
      showThresh(vv,9,0.1248384);
      showThresh(vv,9,0.1225752);
      showThresh(vv,9,0.1139635);
      showThresh(vv,9,0.1332604);
      showThresh(vv,9,0.1249742);
      showThresh(vv,9,0.1227315);
      showThresh(vv,9,0.1140941);
      showThresh(vv,9,0.1334585);
      showThresh(vv,9,0.1250851);
      showThresh(vv,9,0.1229062);
      showThresh(vv,9,0.114181);
      showThresh(vv,9,0.1335968);
      showThresh(vv,9,0.125194);
      showThresh(vv,9,0.1230138);
      showThresh(vv,9,0.1142716);
      showThresh(vv,9,0.1337085);
      showThresh(vv,9,0.125279);
      showThresh(vv,9,0.1230808);
      showThresh(vv,9,0.114302);
      showThresh(vv,9,0.1337986);
      showThresh(vv,9,0.1253237);
      showThresh(vv,9,0.1232614);
      showThresh(vv,9,0.1143428);
      showThresh(vv,9,0.1339932);
      showThresh(vv,9,0.1253591);
      showThresh(vv,9,0.1234251);
      showThresh(vv,9,0.1144401);
      showThresh(vv,9,0.1341681);
      showThresh(vv,9,0.1254652);

      Double thresh = 0.1254652*16.0;
      Double result = vv.thresh(9,thresh);
      cout << thresh << " -> "
	   << result << " -> "
	   << vv.predict(9,result) << endl;

      thresh = 9.25119;
      result = vv.thresh(9,thresh);
      cout << thresh << " -> "
	   << result << " -> "
	   << vv.predict(9,result) << endl;
      Matrix<Double> qx, qy;
      // qx = qfn(9,result,0.);
      // qy = qx;
      // vv.setQuantization(qx,qy);
      vv.setEquiSpaced(result, result, 0.0, 0.0, 9);
      Vector<Double> rs, rhos;
      vv.getTable(rs,rhos);
      showTable(rs,rhos,vv);
      cout << "vv.r(zerolag) : " << vv.r(thresh) << endl;

      // test of dcoff
      Double zerolag = 0.4925;
      Double bias = 6.7e-4;
      cout << "vv.dcoff for n==3 and zerolag==" << zerolag
	   << " and bias == " << bias << endl;
      Double dcoffset, threshold;
      cout << "return value : " <<
	  vv.dcoff(dcoffset, threshold, 3, zerolag, bias) << endl;
      cout << "dcoffset : " << dcoffset << endl;
      cout << "threshold: " << threshold << endl;
      
      // verify that for n==9 case, thresh == thresh(9,zerolag)
      // and dcoffset == 0.0
      cout << "Test that n==9 returns default value" <<endl;
      cout << "return value : " <<
	  vv.dcoff(dcoffset, threshold, 9, zerolag, bias) << endl;
      cout << "dcoffset : " << dcoffset << endl;
      cout << "threshold: " << threshold << endl;
      cout << "thresh(9,zerolag) : " << vv.thresh(9,zerolag) << endl;
    }
    
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
