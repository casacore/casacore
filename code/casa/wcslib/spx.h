/*============================================================================
*
*   WCSLIB 4.0 - an implementation of the FITS WCS standard.
*   Copyright (C) 1995-2005, Mark Calabretta
*
*   WCSLIB is free software; you can redistribute it and/or modify it under
*   the terms of the GNU General Public License as published by the Free
*   Software Foundation; either version 2 of the License, or (at your option)
*   any later version.
*
*   WCSLIB is distributed in the hope that it will be useful, but WITHOUT ANY
*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
*   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
*   details.
*
*   You should have received a copy of the GNU General Public License along
*   with WCSLIB; if not, write to the Free Software Foundation, Inc.,
*   59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
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
*   WCSLIB 4.0 - C routines that implement the spectral coordinate systems
*   recognized by the FITS World Coordinate System (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2005, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*      specx() is a scalar routine that, given one spectral variable (e.g.
*      frequency), computes all the others (e.g. wavelength, velocity, etc.)
*      plus the required derivatives of each with respect to the others.  The
*      results are returned in the spxprm struct, described in detail below.
*
*      The remaining routines are all vector conversions from one spectral
*      variable to another.  Conversion may be done "in place" by calling
*      the routine with the output vector set to the input.
*
*      Logarithmic conversions
*      ------------------------
*      speclog()    Converts spectral values to their natural logarithm
*      logspec()    Restores spectral values from their natural logarithm
*
*      Non-linear   From                    To
*      ----------   ---------------------   ---------------------
*      freqwave()   frequency               vacuum wavelength
*      wavefreq()   vacuum wavelength       frequency
*
*      freqawav()   frequency               air wavelength
*      awavfreq()   air wavelength          frequency
*
*      freqvelo()   frequency               relativistic velocity
*      velofreq()   relativistic velocity   frequency
*
*      waveawav()   vacuum wavelength       air wavelength
*      awavwave()   air wavelength          vacuum wavelength
*
*      wavevelo()   vacuum wavelength       relativistic velocity
*      velowave()   relativistic velocity   vacuum wavelength
*
*      awavvelo()   air wavelength          relativistic velocity
*      veloawav()   relativistic velocity   air wavelength
*
*      Linear       From                    To
*      ----------   ---------------------   ---------------------
*      freqafrq()   frequency               angular frequency
*      afrqfreq()   angular frequency       frequency
*
*      freqener()   frequency               energy
*      enerfreq()   energy                  frequency
*
*      freqwavn()   frequency               wave number
*      wavnfreq()   wave number             frequency
*
*      freqvrad()   frequency               radio velocity
*      vradfreq()   radio velocity          frequency
*
*      wavevopt()   vacuum wavelength       optical velocity
*      voptwave()   optical velocity        vacuum wavelength
*
*      wavezopt()   vacuum wavelength       redshift
*      zoptwave()   redshift                vacuum wavelength
*
*      velobeta()   relativistic velocity   beta (= v/c)
*      betavelo()   beta (= v/c)            relativistic velocity
*
*
*   Spectral cross conversions (scalar); specx()
*   --------------------------------------------
*   Given one spectral variable specx() computes all the others, plus the
*   required derivatives of each with respect to the others.
*
*   Given:
*      type     const char*
*                        The type of spectral variable given by spec, FREQ,
*                        AFRQ, ENER, WAVN, VRAD, WAVE, VOPT, ZOPT, AWAV, VELO,
*                        or BETA (case sensitive).
*      spec     double   The spectral variable given, in SI units.
*      restfrq  double   Rest frequency (Hz) or rest wavelength in vacuum (m),
*      restwav  double   only one of which need be given.  The other should be
*                        set to zero.  If both are zero, only a subset of the
*                        spectral variables can be computed, the remainder
*                        are set to zero.  Specifically, given one of FREQ,
*                        AFRQ, ENER, WAVN, WAVE, or AWAV the others can be
*                        computed without knowledge of the rest frequency.
*                        Likewise, VRAD, VOPT, ZOPT, VELO, and BETA.
*
*   Given and returned:
*      specs    struct spxprm*
*                        Data structure defined below containing all spectral
*                        variables and their derivatives, in SI units.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null spxprm pointer passed.
*                           2: Invalid spectral parameters.
*                           3: Invalid spectral variable.
*
*
*   Logarithmic conversions (vector); speclog(), logspec()
*   ---------------------------------------------------------------
*   Vector logarithmic conversions.
*
*   Given:
*      param    double   Ignored.
*      nspec    int      Vector length, see below.
*      instep   int
*      outstep  int      Vector strides, see below.
*      inspec   const double[]
*                        Input spectral variables, in SI units.
*
*   Returned:
*      outspec  double[] Output spectral variables, in SI units.
*      stat     int[]    Status return value for each vector element:
*                           0: Success.
*                           1: Invalid value of inspec.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*                           4: One or more of the inspec coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Spectral conversions (vector); freqwave(), wavefreq(), etc.
*   -----------------------------------------------------------
*   Vector conversions from one spectral variable to another.  These are the
*   workhorse routines to be used for fast transformations.
*
*   Given:
*      param    double   1) Rest frequency (Hz) when the conversion is between
*                           frequency and any type of velocity, i.e.
*                              freqvelo(), velofreq(),
*                              freqvrad(), vradfreq().
*
*                        2) Rest wavelength in vacuum (m) when the conversion
*                           is between vacuum or air wavelength and any type
*                           of velocity, i.e.
*                              wavevelo(), velowave(),
*                              awavvelo(), veloawav(),
*                              wavevopt(), voptwave(),
*                              wavezopt(), zoptwave().
*
*                        3) Ignored otherwise, i.e.
*                              freqwave(), wavefreq(),
*                              freqawav(), awavfreq(),
*                              waveawav(), awavwave(),
*                              freqafrq(), afrqfreq(),
*                              freqener(), enerfreq(),
*                              freqwavn(), wavnfreq(),
*                              velobeta(), betavelo().
*
*      nspec    int      Vector length, see below.
*      instep   int
*      outstep  int      Vector strides, see below.
*      inspec   const double[]
*                        Input spectral variables, in SI units.
*
*   Returned:
*      outspec  double[] Output spectral variables, in SI units.
*      stat     int[]    Status return value for each vector element:
*                           0: Success.
*                           1: Invalid value of inspec.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           2: Invalid spectral parameters.
*                           4: One or more of the inspec coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Vector lengths and strides
*   --------------------------
*   Vector computation in the spectral code mainly speeds it by amortising the
*   function call overhead.  The vector length function argument refers to the
*   number of spectral variables in both the input and output spectral
*   vectors.
*
*   The vector stride arguments allow the caller to specify that successive
*   elements of a vector are not contiguous in memory.  This applies equally
*   to vectors given to, or returned from a function.  As a simple example
*   consider the following two arrangements in memory of a spectral vector
*   s[], with four elements, s1, s2, s3, and s4, together with x[] and y[]
*   vectors:
*
*      1) s1 s2 s3 s4 x1 x2 x3 x4 y1 y2 y3 y4
*
*      2) x1 y1 s1 x2 y2 s2 x3 y3 s3 x4 y4 s4
*
*   In (1), the address of s[] is s1, its stride is 1, and length 4.
*   In (2), the address of s[] is s1, its stride is 3, and length 4.
*
*   If the vector length is 1 then the stride is ignored and may be set to 0.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   are encoded in the spx_errmsg character array.
*
*
*   Argument checking
*   -----------------
*   The input spectral values are only checked for values that would result
*   in floating point exceptions.  In particular, negative frequencies and
*   wavelengths are allowed, as are velocities greater than the speed of
*   light.  The same is true for the spectral parameters - rest frequency and
*   wavelength.
*
*
*   Accuracy
*   --------
*   No warranty is given for the accuracy of these routines (refer to the
*   copyright notice above); intending users must satisfy for themselves their
*   adequacy for the intended purpose.  However, closure effectively to within
*   double precision rounding error was demonstrated by test routine tspec.c
*   which accompanies this software.
*
*===========================================================================*/

#ifndef WCSLIB_SPEC
#define WCSLIB_SPEC

#ifdef __cplusplus
extern "C" {
#endif


extern const char *spx_errmsg[];


struct spxprm {
   /* The spxprm data structure is used solely by specx().                  */

   double restfrq, restwav;	/* Rest frequency (Hz) and wavelength (m).  */

   /* If one or other of restfrq and restwav is given (non-zero) then all   */
   /* spectral variables may be computed.  If both are given, restfrq is    */
   /* used.  If restfrq and restwav are both zero, only wave characteristic */
   /* xor velocity type spectral variables may be computed depending on the */
   /* variable given.   These flags indicate what is available.             */
   /*-----------------------------------------------------------------------*/
   int wavetype, velotype;	/* True if wave/velocity types have been    */
				/* computed; types are defined below.       */

   /* Spectral variables computed by specx().                               */
   /*-----------------------------------------------------------------------*/
   double freq,			/* wavetype: Frequency (Hz).                */
          afrq,			/* wavetype: Angular frequency (rad/s).     */
          ener,			/* wavetype: Photon energy (J).             */
          wavn,			/* wavetype: Wave number (/m).              */
          vrad,			/* velotype: Radio velocity (m/s).          */
          wave,			/* wavetype: Vacuum wavelength (m).         */
          vopt,			/* velotype: Optical velocity (m/s).        */
          zopt,			/* velotype: Redshift.                      */
          awav,			/* wavetype: Air wavelength (m).            */
          velo,			/* velotype: Relativistic velocity (m/s).   */
          beta;			/* velotype: Relativistic beta.             */

   /* Derivatives of spectral variables computed by specx().                */
   /*-----------------------------------------------------------------------*/
   double dfreqafrq, dafrqfreq, /* Constant, always available.              */
          dfreqener, denerfreq, /* Constant, always available.              */
          dfreqwavn, dwavnfreq, /* Constant, always available.              */
          dfreqvrad, dvradfreq, /* wavetype && velotype.                    */
          dfreqwave, dwavefreq, /* wavetype.                                */
          dfreqawav, dawavfreq, /* wavetype.                                */
          dfreqvelo, dvelofreq, /* wavetype && velotype.                    */
          dwavevopt, dvoptwave, /* wavetype && velotype.                    */
          dwavezopt, dzoptwave, /* wavetype && velotype.                    */
          dwaveawav, dawavwave, /* wavetype.                                */
          dwavevelo, dvelowave, /* wavetype && velotype.                    */
          dawavvelo, dveloawav, /* wavetype && velotype.                    */
          dvelobeta, dbetavelo; /* Constant, always available.              */
};

#define SPXLEN (sizeof(struct spxprm)/sizeof(int))


int specx(const char *, double, double, double, struct spxprm *);

/* Use the preprocessor to declare the remaining function prototypes. */
#define SPX_ARGS double, int, int, int, const double[], double[], int[]

#define SPX_PROTO(CODE) int CODE(SPX_ARGS);

SPX_PROTO(speclog)
SPX_PROTO(logspec)

SPX_PROTO(freqafrq)
SPX_PROTO(afrqfreq)

SPX_PROTO(freqener)
SPX_PROTO(enerfreq)

SPX_PROTO(freqwavn)
SPX_PROTO(wavnfreq)

SPX_PROTO(freqvrad)
SPX_PROTO(vradfreq)

SPX_PROTO(freqwave)
SPX_PROTO(wavefreq)

SPX_PROTO(freqawav)
SPX_PROTO(awavfreq)

SPX_PROTO(freqvelo)
SPX_PROTO(velofreq)

SPX_PROTO(wavevopt)
SPX_PROTO(voptwave)

SPX_PROTO(wavezopt)
SPX_PROTO(zoptwave)

SPX_PROTO(waveawav)
SPX_PROTO(awavwave)

SPX_PROTO(wavevelo)
SPX_PROTO(velowave)

SPX_PROTO(awavvelo)
SPX_PROTO(veloawav)

SPX_PROTO(velobeta)
SPX_PROTO(betavelo)


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_SPEC */
