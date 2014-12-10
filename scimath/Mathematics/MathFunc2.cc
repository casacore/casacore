//# MathFunc2.cc: non templated static data for MathFunc
//# Copyright (C) 1993,1994,1995,1996,1999
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

#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/scimath/Mathematics/MathFunc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// Define static members of MathFunc<T> for g++
//
#if defined(__GNUC__) && (__GNUC__<3 || (__GNUC__==3 && __GNUC_MINOR__<4))
Float MathFunc<Float>::defcutoff_p = (2.0);
Float MathFunc<Float>::defwidth_p = (1.3);
Float MathFunc<Float>::defKBwidth_p = (2.0);
Float MathFunc<Float>::defKBparm_p = (2.5);
Float MathFunc<Float>::defmodKBparm_p = (3.0);
Float MathFunc<Float>::defSphcutoff_p = (3.0);
Float MathFunc<Float>::defSphparm_p = (1.0);
Float MathFunc<Float>::defSincparm_p = (1.14);
Float MathFunc<Float>::defExpPower_p = (2.0);
Float MathFunc<Float>::defExpScale_p = (1.3/sqrt(4.0*C::ln2));
Double MathFunc<Double>::defcutoff_p = (2.0);
Double MathFunc<Double>::defwidth_p = (1.3);
Double MathFunc<Double>::defKBwidth_p = (2.0);
Double MathFunc<Double>::defKBparm_p = (2.5);
Double MathFunc<Double>::defmodKBparm_p = (3.0);
Double MathFunc<Double>::defSphcutoff_p = (3.0);
Double MathFunc<Double>::defSphparm_p = (1.0);
Double MathFunc<Double>::defSincparm_p = (1.14);
Double MathFunc<Double>::defExpPower_p = (2.0);
Double MathFunc<Double>::defExpScale_p = (1.3/sqrt(4.0*C::ln2));
#endif


/* 
   Fred Schwab's SPHFN.f -- 
   translated by f2c (version of 22 July 1992  22:54:52).
*/

#if defined(__cplusplus)
extern "C" {
#endif

/* Table of constant values 
static Int c__1 = 1;
static Int c__8 = 8;
*/

/* Subroutine */ Int sphfn(Int *ialf, Int *im, Int *iflag, float *
	eta, float *psi, Int *ierr)
{
    /* Initialized data */

    static float alpha[5] = { (float)0.,(float).5,(float)1.,(float)1.5,(float)
	    2. };
    static float p7l[25]	/* was [5][5] */ = { (float).02460495,(float)
	    -.1640964,(float).434011,(float)-.5705516,(float).4418614,(float)
	    .03070261,(float)-.1879546,(float).4565902,(float)-.5544891,(
	    float).389279,(float).03770526,(float)-.2121608,(float).4746423,(
	    float)-.5338058,(float).3417026,(float).04559398,(float)-.236267,(
	    float).4881998,(float)-.5098448,(float).2991635,(float).054325,(
	    float)-.2598752,(float).4974791,(float)-.4837861,(float).2614838 }
	    ;
    static float q7l[10] /* was [2][5] */ = { (float)1.124957,(float).3784976,(
	    float)1.07542,(float).3466086,(float)1.029374,(float).3181219,(
	    float).9865496,(float).2926441,(float).9466891,(float).2698218 };
    static float p7u[25]	/* was [5][5] */ = { (float)1.924318e-4,(float)
	    -.005044864,(float).02979803,(float)-.06660688,(float).06792268,(
	    float)5.030909e-4,(float)-.008639332,(float).04018472,(float)
	    -.07595456,(float).06696215,(float).001059406,(float)-.01343605,(
	    float).0513536,(float)-.08386588,(float).06484517,(float)
	    .001941904,(float)-.01943727,(float).06288221,(float)-.09021607,(
	    float).06193,(float).003224785,(float)-.02657664,(float).07438627,
	    (float)-.09500554,(float).05850884 };
    static float q7u[10] /* was [2][5] */ = { (float)1.45073,(float).6578685,(
	    float)1.353872,(float).5724332,(float)1.269924,(float).5032139,(
	    float)1.196177,(float).4460948,(float)1.130719,(float).3982785 };
    static float p8l[30] /* was [6][5] */ = { (float).0137803,(float)-.1097846,
	    (float).3625283,(float)-.6522477,(float).6684458,(float)-.4703556,
	    (float).01721632,(float)-.1274981,(float).3917226,(float)
	    -.6562264,(float).6305859,(float)-.4067119,(float).02121871,(
	    float)-.1461891,(float).4185427,(float)-.6543539,(float).590466,(
	    float)-.3507098,(float).02580565,(float)-.1656048,(float).4426283,
	    (float)-.6473472,(float).5494752,(float)-.3018936,(float)
	    .03098251,(float)-.1854823,(float).4637398,(float)-.6359482,(
	    float).5086794,(float)-.2595588 };
    static float q8l[10] /* was [2][5] */ = { (float)1.076975,(float).3394154,(
	    float)1.036132,(float).3145673,(float).9978025,(float).2920529,(
	    float).9617584,(float).2715949,(float).9278774,(float).2530051 };
    static float p8u[30] /* was [6][5] */ = { (float)4.29046e-5,(float)
	    -.001508077,(float).01233763,(float)-.0409127,(float).06547454,(
	    float)-.05664203,(float)1.201008e-4,(float)-.002778372,(float)
	    .01797999,(float)-.05055048,(float).07125083,(float)-.05469912,(
	    float)2.698511e-4,(float)-.004628815,(float).0247089,(float)
	    -.06017759,(float).07566434,(float)-.05202678,(float)5.259595e-4,(
	    float)-.007144198,(float).03238633,(float)-.06946769,(float)
	    .07873067,(float)-.0488949,(float)9.255826e-4,(float)-.01038126,(
	    float).04083176,(float)-.07815954,(float).08054087,(float)
	    -.04552077 };
    static float q8u[10] /* was [2][5] */ = { (float)1.379457,(float).5786953,(
	    float)1.300303,(float).5135748,(float)1.230436,(float).4593779,(
	    float)1.168075,(float).4135871,(float)1.111893,(float).3744076 };
    static float p4[25]	/* was [5][5] */ = { (float).01584774,(float)
	    -.1269612,(float).2333851,(float)-.1636744,(float).05014648,(
	    float).03101855,(float)-.1641253,(float).23855,(float)-.1417069,(
	    float).03773226,(float).050079,(float)-.1971357,(float).2363775,(
	    float)-.1215569,(float).02853104,(float).0720126,(float)-.225158,(
	    float).2293715,(float)-.1038359,(float).02174211,(float).09585932,
	    (float)-.2481381,(float).2194469,(float)-.08862132,(float)
	    .01672243 };
    static float q4[10]	/* was [2][5] */ = { (float).4845581,(float).07457381,
	    (float).4514531,(float).0645864,(float).4228767,(float).05655715,(
	    float).3978515,(float).04997164,(float).3756999,(float).044488 };
    static float p5[35]	/* was [7][5] */ = { (float).003722238,(float)
	    -.04991683,(float).1658905,(float)-.238724,(float).1877469,(float)
	    -.08159855,(float).03051959,(float).008182649,(float)-.07325459,(
	    float).1945697,(float)-.2396387,(float).1667832,(float)-.06620786,
	    (float).02224041,(float).01466325,(float)-.09858686,(float)
	    .2180684,(float)-.2347118,(float).1464354,(float)-.05350728,(
	    float).01624782,(float).02314317,(float)-.1246383,(float).2362036,
	    (float)-.2257366,(float).1275895,(float)-.04317874,(float)
	    .01193168,(float).03346886,(float)-.1503778,(float).2492826,(
	    float)-.2142055,(float).1106482,(float)-.03486024,(float)
	    .008821107 };
    static float q5[5] = { (float).241882,(float).2291233,(float).2177793,(
	    float).2075784,(float).1983358 };
    static float p6l[25]	/* was [5][5] */ = { (float).05613913,(float)
	    -.3019847,(float).6256387,(float)-.6324887,(float).3303194,(float)
	    .06843713,(float)-.3342119,(float).6302307,(float)-.5829747,(
	    float).27657,(float).08203343,(float)-.3644705,(float).627866,(
	    float)-.5335581,(float).2312756,(float).09675562,(float)-.3922489,
	    (float).6197133,(float)-.485747,(float).1934013,(float).1124069,(
	    float)-.4172349,(float).6069622,(float)-.4405326,(float).1618978 }
	    ;
    static float q6l[10] /* was [2][5] */ = { (float).9077644,(float).2535284,(
	    float).8626056,(float).22914,(float).8212018,(float).2078043,(
	    float).7831755,(float).1890848,(float).7481828,(float).1726085 };
    static float p6u[25]	/* was [5][5] */ = { (float)8.531865e-4,(float)
	    -.01616105,(float).06888533,(float)-.1109391,(float).07747182,(
	    float).00206076,(float)-.02558954,(float).08595213,(float)
	    -.1170228,(float).07094106,(float).004028559,(float)-.03697768,(
	    float).1021332,(float)-.1201436,(float).06412774,(float)
	    .006887946,(float)-.04994202,(float).1168451,(float)-.1207733,(
	    float).0574421,(float).01071895,(float)-.06404749,(float).1297386,
	    (float)-.1194208,(float).05112822 };
    static float q6u[10] /* was [2][5] */ = { (float)1.10127,(float).3858544,(
	    float)1.025431,(float).3337648,(float).9599102,(float).2918724,(
	    float).9025276,(float).2575336,(float).851747,(float).2289667 };

    /* Format strings */

    /* System generated locals */
    float r__1;
    double d__1, d__2;

    /* 
    Builtin functions 
    Int s_wsfe(cilist *), do_fio(Int *, char *, ftnlen), e_wsfe();
    */

    /* Local variables */
    static Int j, k;
    static float x;
    extern /* Subroutine */ int msgwrt_(Int *);
    static float eta2;

    /* Fortran I/O blocks 
    static cilist io___23 = { 0, 0, 0, fmt_1900, 0 };
    */


/* ----------------------------------------------------------------------- */
/* ! Evaluate rational approx. to selected spheriodial functions. */
/*                                                                        */
/*   This software is the subject of a User agreement and is confidential */
/*   in nature. It shall not be sold or otherwise made available or */
/*   disclosed to third parties. */
/* ----------------------------------------------------------------------- */
/*   SPHFN is a subroutine to evaluate rational approximations to */
/*   selected zero-order spheroidal functions, psi(c,eta), which are, in */
/*   a sense defined in VLA Scientific Memorandum No. 132, optimal for */
/*   gridding interferometer data.  The approximations are taken from */
/*   VLA Computer Memorandum No. 156.  The parameter c is related to the */
/*   support width, m, of the convoluting function according to */
/*   c=pi*m/2.  The parameter alpha determines a weight function in the */
/*   definition of the criterion by which the function is optimal. */
/*   SPHFN incorporates approximations to 25 of the spheroidal func- */
/*   tions, corresponding to 5 choices of m (4, 5, 6, 7, or 8 cells) */
/*   and 5 choices of the weighting exponent (0, 1/2, 1, 3/2, or 2). */
/*   Inputs: */
/*      IALF     I           Selects the weighting exponent, alpha */
/*                           (IALF = 1, 2, 3, 4, and 5 correspond to */
/*                           alpha = 0, 1/2, 1, 3/2, and 2, resp.). */
/*      IM       I           Selects the support width m, (=IM) and, */
/*                           correspondingly, the parameter c of the */
/*                           spheroidal function (only the choices 4, */
/*                           5, 6, 7, and 8 are allowed). */
/*      IFLAG    I           Chooses whether the spheroidal function */
/*                           itself, or its Fourier transform, is to be */
/*                           approximated.  The latter is appropriate */
/*                           for gridding, and the former for the u-v */
/*                           plane convolution.  The two differ by a */
/*                           factor (1-eta**2)**alpha.  IFLAG less than */
/*                           or equal to zero chooses the function */
/*                           appropriate for gridding, and IFLAG positive */
/*                           chooses its Fourier transform. */
/*      ETA      R           Eta, as the argument of the spheroidal */
/*                           function, is a variable which ranges from 0 */
/*                           at the center of the convoluting function to */
/*                           1 at its edge (also from 0 at the center of */
/*                           the gridding correction function to unity at */
/*                           the edge of the map). */
/*   Output: */
/*      PSI      R           function value which, on entry to the */
/*                           subroutine, was to have been computed. */
/*      IERR     I           Error return code: */
/*                              0  =>  No error */
/*                              1  =>  IALF out of range */
/*                              2  =>  IM out of range */
/*                              3  =>  ABS(ETA).GT.1 */
/*                             12  =>  IALF and IM both out of range */
/*                             13  =>  IALF and ETA both illegal */
/*                             23  =>  IM and ETA both illegal */
/*                            123  =>  IALF, IM, and ETA all illegal */
/* ----------------------------------------------------------------------- */
/*     INCLUDE 'INCS:DMSG.INC' */
/* ----------------------------------------------------------------------- */
    *ierr = 0;
/*                                       Check inputs. */
    if (*ialf < 1 || *ialf > 5) {
	*ierr = 1;
    }
    if (*im < 4 || *im > 8) {
	*ierr = *ierr * 10 + 2;
    }
    if (fabs(*eta) > (float)1.) {
	*ierr = *ierr * 10 + 3;
    }
    if (*ierr != 0) {
	goto L900;
    }
/*                                       So far, so good. */
/* Computing 2nd power */
    r__1 = *eta;
    eta2 = r__1 * r__1;
    j = *ialf;
    k = *im - 3;
/*                                       Branch on support width. */
    switch (k) {
	case 1:  goto L100;
	case 2:  goto L200;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L500;
    }
/*                                       Support width = 4 cells. */
L100:
    x = eta2 - (float)1.;
    *psi = (p4[j * 5 - 5] + x * (p4[j * 5 - 4] + x * (p4[j * 5 - 3] + x * (p4[
	    j * 5 - 2] + x * p4[j * 5 - 1])))) / (x * (q4[(j << 1) - 2] + x * 
	    q4[(j << 1) - 1]) + (float)1.);
    goto L800;
/*                                       Support width = 5 cells. */
L200:
    x = eta2 - (float)1.;
    *psi = (p5[j * 7 - 7] + x * (p5[j * 7 - 6] + x * (p5[j * 7 - 5] + x * (p5[
	    j * 7 - 4] + x * (p5[j * 7 - 3] + x * (p5[j * 7 - 2] + x * p5[j * 
	    7 - 1])))))) / (x * q5[j - 1] + (float)1.);
    goto L800;
/*                                       Support width = 6 cells. */
L300:
    if (fabs(*eta) > (float).75) {
	goto L350;
    }
    x = eta2 - (float).5625;
    *psi = (p6l[j * 5 - 5] + x * (p6l[j * 5 - 4] + x * (p6l[j * 5 - 3] + x * (
	    p6l[j * 5 - 2] + x * p6l[j * 5 - 1])))) / (x * (q6l[(j << 1) - 2] 
	    + x * q6l[(j << 1) - 1]) + (float)1.);
    goto L800;
L350:
    x = eta2 - (float)1.;
    *psi = (p6u[j * 5 - 5] + x * (p6u[j * 5 - 4] + x * (p6u[j * 5 - 3] + x * (
	    p6u[j * 5 - 2] + x * p6u[j * 5 - 1])))) / (x * (q6u[(j << 1) - 2] 
	    + x * q6u[(j << 1) - 1]) + (float)1.);
    goto L800;
/*                                       Support width = 7 cells. */
L400:
    if (fabs(*eta) > (float).775) {
	goto L450;
    }
    x = eta2 - (float).600625;
    *psi = (p7l[j * 5 - 5] + x * (p7l[j * 5 - 4] + x * (p7l[j * 5 - 3] + x * (
	    p7l[j * 5 - 2] + x * p7l[j * 5 - 1])))) / (x * (q7l[(j << 1) - 2] 
	    + x * q7l[(j << 1) - 1]) + (float)1.);
    goto L800;
L450:
    x = eta2 - (float)1.;
    *psi = (p7u[j * 5 - 5] + x * (p7u[j * 5 - 4] + x * (p7u[j * 5 - 3] + x * (
	    p7u[j * 5 - 2] + x * p7u[j * 5 - 1])))) / (x * (q7u[(j << 1) - 2] 
	    + x * q7u[(j << 1) - 1]) + (float)1.);
    goto L800;
/*                                       Support width = 8 cells. */
L500:
    if (fabs(*eta) > (float).775) {
	goto L550;
    }
    x = eta2 - (float).600625;
    *psi = (p8l[j * 6 - 6] + x * (p8l[j * 6 - 5] + x * (p8l[j * 6 - 4] + x * (
	    p8l[j * 6 - 3] + x * (p8l[j * 6 - 2] + x * p8l[j * 6 - 1]))))) / (
	    x * (q8l[(j << 1) - 2] + x * q8l[(j << 1) - 1]) + (float)1.);
    goto L800;
L550:
    x = eta2 - (float)1.;
    *psi = (p8u[j * 6 - 6] + x * (p8u[j * 6 - 5] + x * (p8u[j * 6 - 4] + x * (
	    p8u[j * 6 - 3] + x * (p8u[j * 6 - 2] + x * p8u[j * 6 - 1]))))) / (
	    x * (q8u[(j << 1) - 2] + x * q8u[(j << 1) - 1]) + (float)1.);
/*                                       Normal return. */
L800:
    if (*iflag > 0 || *ialf == 1 || *eta == (float)0.) {
	goto L999;
    }
    if (fabs(*eta) == (float)1.) {
	goto L850;
    }
    d__1 = (double) ((float)1. - eta2);
    d__2 = (double) alpha[*ialf - 1];
    *psi = pow(d__1, d__2) * *psi;
    goto L999;
L850:
    *psi = (float)0.;
    goto L999;
/*                                       Error exit. */
L900:
/*   
    io___23.ciunit = msgtxt;
     s_wsfe(&io___23);
    do_fio(&c__1, (char *)&(*ierr), (ftnlen)sizeof(Int));
    e_wsfe();
    msgwrt_(&c__8);
*/

L999:
    return 0;
/* -----------------------------------------------------------------------
 */
} /* sphfn */

#if defined(__cplusplus)
	}
#endif

float sphfn(Int ialf, Int im, float eta)
{
  Int ialphahold, imhold, iflaghold, ierrhold;
  ialphahold = ialf;
  imhold = im; 
  iflaghold = 0;
  ierrhold = 0;

  float psihold, etahold;
  psihold = 0; 
  etahold = eta;

  sphfn(&ialphahold, &imhold, &iflaghold, &etahold, &psihold, &ierrhold);
  return psihold;
}

} //# NAMESPACE CASACORE - END

