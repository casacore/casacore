/*============================================================================
*
*   WCSLIB 3.5 - an implementation of the FITS WCS convention.
*   Copyright (C) 1995-2004, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta
*                      Australia Telescope National Facility, CSIRO
*                      PO Box 76
*                      Epping NSW 1710
*                      AUSTRALIA
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   http://www.atnf.csiro.au/~mcalabre/index.html
*   $Id$
*=============================================================================
*
*   WCSLIB 3.5 - C routines that implement the spherical map projections
*   recognized by the FITS World Coordinate System (WCS) convention.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (paper II)
*
*
*   Summary of routines
*   -------------------
*   Each map projection is implemented via separate functions for the
*   spherical projection, *s2x(), and deprojection, *x2s().
*
*   Initialization routines, *set(), compute intermediate values from the
*   projection parameters but need not be called explicitly - see the
*   explanation of the flag member of the prjprm struct below.
*
*      prjini                 Initialization routine for the prjprm struct.
*      prjprt                 Service routine to print the prjprm struct.
*
*      prjset prjx2s prjs2x   Generic driver routines (see below).
*
*      azpset azpx2s azps2x   AZP: zenithal/azimuthal perspective
*      szpset szpx2s szps2x   SZP: slant zenithal perspective
*      tanset tanx2s tans2x   TAN: gnomonic
*      stgset stgx2s stgs2x   STG: stereographic
*      sinset sinx2s sins2x   SIN: orthographic/synthesis
*      arcset arcx2s arcs2x   ARC: zenithal/azimuthal equidistant
*      zpnset zpnx2s zpns2x   ZPN: zenithal/azimuthal polynomial
*      zeaset zeax2s zeas2x   ZEA: zenithal/azimuthal equal area
*      airset airx2s airs2x   AIR: Airy
*      cypset cypx2s cyps2x   CYP: cylindrical perspective
*      ceaset ceax2s ceas2x   CEA: cylindrical equal area
*      carset carx2s cars2x   CAR: Plate carree
*      merset merx2s mers2x   MER: Mercator
*      sflset sflx2s sfls2x   SFL: Sanson-Flamsteed
*      parset parx2s pars2x   PAR: parabolic
*      molset molx2s mols2x   MOL: Mollweide
*      aitset aitx2s aits2x   AIT: Hammer-Aitoff
*      copset copx2s cops2x   COP: conic perspective
*      coeset coex2s coes2x   COE: conic equal area
*      codset codx2s cods2x   COD: conic equidistant
*      cooset coox2s coos2x   COO: conic orthomorphic
*      bonset bonx2s bons2x   BON: Bonne
*      pcoset pcox2s pcos2x   PCO: polyconic
*      tscset tscx2s tscs2x   TSC: tangential spherical cube
*      cscset cscx2s cscs2x   CSC: COBE quadrilateralized spherical cube
*      qscset qscx2s qscs2x   QSC: quadrilateralized spherical cube
*
*
*   Initialization routine for the prjprm struct; prjini()
*   ------------------------------------------------------
*   This service routine may be used to set the members of a prjprm struct to
*   default values.
*
*   Returned:
*      prj      struct prjprm*
*                        Projection parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null prjprm pointer passed.
*
*
*   Print routine for the prjprm struct; prjprt()
*   ---------------------------------------------
*   This service routine may be used to print the members of a prjprm struct.
*
*   Given:
*      prj      const struct prjprm*
*                        Projection parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null prjprm pointer passed.
*
*
*   Generic driver routines; prjset(), prjx2s() & prjs2x()
*   ------------------------------------------------------
*   Driver routines are available for use as a generic interface to the
*   specific projection routines.  The interfaces to these routines are the
*   same as those of the corresponding routines for the specific projections
*   (see below).
*
*   The one important distinction between these and the specific projection
*   routines is that the projection code must be defined in the prjprm struct
*   as this is needed by prjset() to identify the required projection.  Once
*   prjset() has initialized the prjprm struct, prjx2s() and prjs2x() use the
*   pointers to the specific projection and deprojection contained therein.
*
*
*   Initialization routine; *set()
*   ------------------------------
*   Initializes a prjprm data structure according to information supplied
*   within it (see "Projection parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   prjx2s() and prjs2x() if the "flag" structure member is anything other
*   than a predefined magic value.
*
*   Given and/or returned:
*      prj      struct prjprm*
*                        Projection parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null prjprm pointer passed.
*                           2: Invalid projection parameters.
*
*
*   Cartesian-to-spherical deprojection; *x2s()
*   -------------------------------------------
*   Compute native spherical coordinates (phi,theta) from (x,y) coordinates in
*   the plane of projection.
*
*   Given and returned:
*      prj      struct prjprm*
*                        Projection parameters (see below).
*
*   Given:
*      nx,ny    int      Vector lengths, see below.
*      sxy,spt  int      Vector strides, see below.
*      x,y      const double[]
*                        Projected coordinates.
*
*   Returned:
*      phi,     double[] Longitude and latitude of the projected point in
*      theta             native spherical coordinates, in degrees.
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of (x,y).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null prjprm pointer passed.
*                           2: Invalid projection parameters.
*                           3: One or more of the (x,y) coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Spherical-to-Cartesian projection; *s2x()
*   -----------------------------------------
*   Compute (x,y) coordinates in the plane of projection from native spherical
*   coordinates (phi,theta).
*
*   Given and returned:
*      prj      struct prjprm*
*                        Projection parameters (see below).
*
*   Given:
*      nphi,    int
*      ntheta            Vector lengths, see below.
*      spt,sxy  int      Vector strides, see below.
*      phi,     const double[]
*      theta             Longitude and latitude of the projected point in
*                        native spherical coordinates, in degrees.
*
*   Returned:
*      x,y      double[] Projected coordinates.
*      stat     int[]    Error status for each vector element:
*                           0: Success.
*                           1: Invalid value of (phi,theta).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null prjprm pointer passed.
*                           2: Invalid projection parameters.
*                           4: One or more of the (phi,theta) coordinates
*                              were, invalid, as indicated by the stat vector.
*
*   Projection parameters
*   ---------------------
*   The prjprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following prjprm
*         structure members are set or changed: code, r0, pv[], phi0, and
*         theta0.  This signals the initialization routine to recompute
*         intermediaries.  It need not be reset when bounds is changed.
*
*      char code[4]
*         Three-letter projection code.
*
*      double r0
*         r0; The radius of the generating sphere for the projection, a linear
*         scaling parameter.  If this is zero, it will be reset to the default
*         value of 180/pi (the value for FITS WCS).
*
*      double pv[30]
*         Projection parameters.  These correspond to the PVi_ma keywords in
*         FITS, so pv[0] is PVi_0a, pv[1] is PVi_1a, etc., where i denotes the
*         latitude-like axis.  Many projections use pv[1] (PVi_1a), some also
*         use pv[2] (PVi_2a) and SZP uses pv[3] (PVi_3a).  ZPN is the only
*         projection that uses any of the others.
*
*      double phi0, theta0
*         Native longitude and latitude of the reference point, i.e. the point
*         (x,y) = (0,0).  If undefined (set to a magic value by prjini()) the
*         initialization routine will set this to a projection-specific
*         default.
*
*      int bounds
*         Controls strict bounds checking for the AZP, SZP, TAN, SIN, ZPN, and
*         COP projections; set to zero to disable checking.
*
*   The remaining members of the prjprm struct are maintained by *set() and
*   must not be modified elsewhere:
*
*      char name[40]
*      int  category, pvrange, simplezen, equiareal, conformal, global,
*           divergent
*         These items are provided for the caller's information only and are
*         not used by the projection routines:
*            - Long name of the projection.
*            - category matches the value of the relevant global variable:
*              ZENITHAL, CYLINDRICAL, PSEUDOCYLINDRICAL, CONVENTIONAL, CONIC,
*              POLYCONIC, and QUADCUBE.  The category name may also be
*              identified via the prj_categories character array.
*            - Range of projection parameter indices: 100 times the first
*              allowed index plus the number of parameters, e.g. TAN is 0
*              (no parameters), SZP is 103 (1 to 3), and ZPN is 30 (0 to 29).
*            - simplezen is true if the projection is a radially-symmetric
*              zenithal projection,
*            - equiareal is true if the projection is equal area,
*            - conformal is true if the projection is conformal,
*            - global    is true if the projection can represent the whole
*                        sphere in a finite, non-overlapped mapping, and
*            - divergent is true if the projection diverges in latitude.
*
*      double x0, y0
*         Offsets used to force (x,y) = (0,0) at (phi0,theta0).
*
*      double w[10]
*      int n
*         Intermediate values derived from the projection parameters.
*
*      int (*prjx2s)()
*      int (*prjs2x)()
*         Pointers to the projection and deprojection routines.
*
*   Usage of the pv[] and w[] arrays as it applies to each projection is
*   described in the prologue to each trio of projection routines in prj.c.
*
*
*   Vector lengths
*   --------------
*   Many of the projection equations are partially or fully separable in the
*   mathematical sense, e.g. (x,y) = f(phi) * g(theta).  In vector
*   computations this property provides an opportunity for caching
*   intermediate calculations and this may result in a much greater speedup
*   than simple amortisation of the function call overhead.  Accordingly, the
*   transformation routines are matrix-oriented with input coordinate pairs
*   specified via pairs of vectors (one-dimensional C arrays).
*
*   The following diagram describes the situation for the projection routines
*   with nphi = 6, and ntheta = 3:
*
*                   phi[0]   phi[1]   phi[2]   phi[3]   phi[4]   phi[5]
*                   ------   ------   ------   ------   ------   ------
*      theta[0]  |  x,y[0]   x,y[1]   x,y[2]   x,y[3]   x,y[4]   x,y[5]
*      theta[1]  |  x,y[6]   x,y[7]   x,y[8]   x,y[9]   x,y[10]  x,y[11]
*      theta[2]  |  x,y[12]  x,y[13]  x,y[14]  x,y[15]  x,y[16]  x,y[17]
*
*   In this case the projection routine would calculate nphi * ntheta = 18
*   (x,y) coordinate pairs.  It is the responsibility of the caller to ensure
*   that sufficient space has been allocated in the x[], y[] and stat[]
*   arrays.
*
*   Vector computation will often be required where neither phi nor theta is
*   constant.  This is accomplished by setting ntheta = 0 which is interpreted
*   to mean ntheta = nphi but only the matrix diagonal is to be computed.
*   Thus, for nphi = 3 and ntheta = 0 only three (x,y) coordinate pairs are
*   computed:
*
*                   phi[0]   phi[1]   phi[2]
*                   ------   ------   ------
*      theta[0]  |  x,y[0]
*      theta[1]  |           x,y[1]
*      theta[2]  |                    x,y[2]
*
*   Note how this differs from nphi = 3, ntheta = 1:
*
*                   phi[0]   phi[1]   phi[2]
*                   ------   ------   ------
*      theta[0]  |  x,y[0]   x,y[1]   x,y[2]
*
*   The situation for the deprojection routines is similar; the x-coordinate
*   (like phi) varies fastest.
*
*
*   Vector strides
*   --------------
*   The vector stride arguments allow the caller to specify that successive
*   elements of a vector are not contiguous in memory.  This applies equally
*   to vectors given to, or returned from a function.  As a simple example
*   consider the following two arrangements in memory of the elements of four
*   (x,y) coordinate pairs together with an unrelated z coordinate element:
*
*      1) x1 x2 x3 x4 y1 y2 y3 y4 z1 z2 z3 z4
*
*      2) x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4
*
*   In (1), the address of x[] is x1, its stride is 1, and length 4.
*           the address of y[] is y1, its stride is 1, and length 4.
*
*   In (2), the address of x[] is x1, its stride is 3, and length 4.
*           the address of y[] is y1, its stride is 3, and length 4.
*
*   The phi[] and theta[] arrays are assumed to have the same vector stride,
*   spt, as are the x[] and y[] arrays, sxy.
*
*   If the vector length is 1 then the stride is ignored and may be set to 0.
*
*
*   Argument checking
*   -----------------
*   Projection routines:
*
*      The values of phi and theta (the native longitude and latitude)
*      normally lie in the range [-180,180] for phi, and [-90,90] for theta.
*      However, all projection routines will accept any value of phi and will
*      not normalize it.
*
*      The projection routines do not explicitly check that theta lies within
*      the range [-90,90].  They do check for any value of theta that produces
*      an invalid argument to the projection equations (e.g. leading to
*      division by zero).  The projection routines for AZP, SZP, TAN, SIN,
*      ZPN, and COP also return error 2 if (phi,theta) corresponds to the
*      overlapped (far) side of the projection but also return the
*      corresponding value of (x,y).  This strict bounds checking may be
*      relaxed at any time by setting prj.bounds to 0 (rather than 1); the
*      projections need not be reinitialized.
*
*   Deprojection routines:
*
*      Error checking on the projected coordinates (x,y) is limited to that
*      required to ascertain whether a solution exists.  Where a solution does
*      exist no check is made that the value of phi and theta obtained lie
*      within the ranges [-180,180] for phi, and [-90,90] for theta.
*
*
*   Error codes
*   -----------
*   Error messages to match the error codes returned from each function are
*   encoded in the prj_errmsg character array.
*
*
*   Accuracy
*   --------
*   No warranty is given for the accuracy of these routines (refer to the
*   copyright notice above); intending users must satisfy for themselves their
*   adequacy for the intended purpose.  However, closure to a precision of at
*   least 1E-10 degree of longitude and latitude has been verified for typical
*   projection parameters on the 1 degree graticule of native longitude and
*   latitude (to within 5 degrees of any latitude where the projection may
*   diverge).  Refer to the tproj1.c and tproj2.c test routines which
*   accompany this software.
*
*===========================================================================*/

#ifndef WCSLIB_PROJ
#define WCSLIB_PROJ

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif

/* Total number of projection parameters; 0 to PVN-1. */
#define PVN 30


extern const char *prj_errmsg[];
#define prjini_errmsg prj_errmsg
#define prjprt_errmsg prj_errmsg
#define prjset_errmsg prj_errmsg
#define prjx2s_errmsg prj_errmsg
#define prjs2x_errmsg prj_errmsg

extern const int CONIC, CONVENTIONAL, CYLINDRICAL, POLYCONIC,
                 PSEUDOCYLINDRICAL, QUADCUBE, ZENITHAL;
extern const char prj_categories[8][32];

extern const int  prj_ncode;
extern const char prj_codes[26][4];


/* Use the preprocessor to define function interfaces. */
#ifdef PRJINI
#undef PRJINI
#endif

#ifdef PRJPRT
#undef PRJPRT
#endif

#ifdef PRJSET
#undef PRJSET
#endif

#ifdef PRJX2S
#undef PRJX2S
#endif

#ifdef PRJS2X
#undef PRJS2X
#endif

#if __STDC__ || defined(__cplusplus)
#define PRJINI struct prjprm *
#define PRJPRT const struct prjprm *
#define PRJSET struct prjprm *
#define PRJX2S struct prjprm *, int, int, int, int, \
               const double[], const double[],      \
               double[], double[], int[]
#define PRJS2X struct prjprm *, int, int, int, int, \
               const double[], const double[],      \
               double[], double[], int[]
#else
#define PRJINI
#define PRJPRT
#define PRJSET
#define PRJX2S
#define PRJS2X
#endif


struct prjprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* Parameters to be provided (see the prologue above).                   */
   /*-----------------------------------------------------------------------*/
   char   code[4];		/* Three-letter projection code.            */
   double r0;			/* Radius of the generating sphere.         */
   double pv[PVN];		/* Projection parameters.                   */
   double phi0, theta0;		/* Fiducial native coordinates.             */
   int    bounds;		/* Enable strict bounds checking.           */

   /* Information derived from the parameters supplied.                     */
   /*-----------------------------------------------------------------------*/
   char   name[40];		/* Projection name.                         */
   int    category;		/* Projection category.                     */
   int    pvrange;		/* Range of projection parameter indices.   */
   int    simplezen;		/* Is it a simple zenithal projection?      */
   int    equiareal;		/* Is it an equal area projection?          */
   int    conformal;		/* Is it a conformal projection?            */
   int    global;		/* Can it map the whole sphere?             */
   int    divergent;		/* Does the projection diverge in latitude? */
   double x0, y0;		/* Fiducial offsets.                        */

   double w[10];		/* Intermediate values.                     */
   int    n;			/* Intermediate value.                      */

   int (*prjx2s)(PRJX2S);	/* Pointers to the spherical projection and */
   int (*prjs2x)(PRJS2X);	/* deprojection functions.                  */

   double *p;			/* Aliased to pv[] by prjini() for backward */
                                /* compatibility with WCSLIB 2.x.           */
};

#define PRJLEN (sizeof(struct prjprm)/sizeof(int))


/* Use the preprocessor to declare function prototypes. */
int prjini(PRJINI);
int prjprt(PRJPRT);

#define PROTO(CODE)    \
   int CODE##set(PRJSET); \
   int CODE##x2s(PRJX2S); \
   int CODE##s2x(PRJS2X);

PROTO(prj)
PROTO(azp)
PROTO(szp)
PROTO(tan)
PROTO(stg)
PROTO(sin)
PROTO(arc)
PROTO(zpn)
PROTO(zea)
PROTO(air)
PROTO(cyp)
PROTO(cea)
PROTO(car)
PROTO(mer)
PROTO(sfl)
PROTO(par)
PROTO(mol)
PROTO(ait)
PROTO(cop)
PROTO(coe)
PROTO(cod)
PROTO(coo)
PROTO(bon)
PROTO(pco)
PROTO(tsc)
PROTO(csc)
PROTO(qsc)


/* Define macros for scalar invokation for compatibility with WCSLIB 2.x. */
#define prjfwd_errmsg prj_errmsg
#define prjrev_errmsg prj_errmsg

extern int prj_stat;
#define prjrev(x, y, prj, phi, theta) \
        prjx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define prjfwd(phi, theta, prj, x, y) \
        prjs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define azprev(x, y, prj, phi, theta) \
        azpx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define azpfwd(phi, theta, prj, x, y) \
        azps2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define szprev(x, y, prj, phi, theta) \
        szpx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define szpfwd(phi, theta, prj, x, y) \
        szps2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define tanrev(x, y, prj, phi, theta) \
        tanx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define tanfwd(phi, theta, prj, x, y) \
        tans2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define stgrev(x, y, prj, phi, theta) \
        stgx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define stgfwd(phi, theta, prj, x, y) \
        stgs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define sinrev(x, y, prj, phi, theta) \
        sinx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define sinfwd(phi, theta, prj, x, y) \
        sins2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define arcrev(x, y, prj, phi, theta) \
        arcx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define arcfwd(phi, theta, prj, x, y) \
        arcs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define zpnrev(x, y, prj, phi, theta) \
        zpnx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define zpnfwd(phi, theta, prj, x, y) \
        zpns2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define zearev(x, y, prj, phi, theta) \
        zeax2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define zeafwd(phi, theta, prj, x, y) \
        zeas2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define airrev(x, y, prj, phi, theta) \
        airx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define airfwd(phi, theta, prj, x, y) \
        airs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define cyprev(x, y, prj, phi, theta) \
        cypx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define cypfwd(phi, theta, prj, x, y) \
        cyps2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define cearev(x, y, prj, phi, theta) \
        ceax2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define ceafwd(phi, theta, prj, x, y) \
        ceas2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define carrev(x, y, prj, phi, theta) \
        carx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define carfwd(phi, theta, prj, x, y) \
        cars2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define merrev(x, y, prj, phi, theta) \
        merx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define merfwd(phi, theta, prj, x, y) \
        mers2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define sflrev(x, y, prj, phi, theta) \
        sflx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define sflfwd(phi, theta, prj, x, y) \
        sfls2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define parrev(x, y, prj, phi, theta) \
        parx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define parfwd(phi, theta, prj, x, y) \
        pars2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define molrev(x, y, prj, phi, theta) \
        molx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define molfwd(phi, theta, prj, x, y) \
        mols2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define aitrev(x, y, prj, phi, theta) \
        aitx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define aitfwd(phi, theta, prj, x, y) \
        aits2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define coprev(x, y, prj, phi, theta) \
        copx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define copfwd(phi, theta, prj, x, y) \
        cops2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define coerev(x, y, prj, phi, theta) \
        coex2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define coefwd(phi, theta, prj, x, y) \
        coes2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define codrev(x, y, prj, phi, theta) \
        codx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define codfwd(phi, theta, prj, x, y) \
        cods2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define coorev(x, y, prj, phi, theta) \
        coox2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define coofwd(phi, theta, prj, x, y) \
        coos2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define bonrev(x, y, prj, phi, theta) \
        bonx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define bonfwd(phi, theta, prj, x, y) \
        bons2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define pcorev(x, y, prj, phi, theta) \
        pcox2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define pcofwd(phi, theta, prj, x, y) \
        pcos2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define tscrev(x, y, prj, phi, theta) \
        tscx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define tscfwd(phi, theta, prj, x, y) \
        tscs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define cscrev(x, y, prj, phi, theta) \
        cscx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define cscfwd(phi, theta, prj, x, y) \
        cscs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#define qscrev(x, y, prj, phi, theta) \
        qscx2s(prj, 1, 1, 1, 1, &(x), &(y), phi, theta, &prj_stat)
#define qscfwd(phi, theta, prj, x, y) \
        qscs2x(prj, 1, 1, 1, 1, &(phi), &(theta), x, y, &prj_stat)

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_PROJ */
