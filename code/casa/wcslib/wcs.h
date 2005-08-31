/*============================================================================
*
*   WCSLIB 4.1 - an implementation of the FITS WCS standard.
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
*   WCSLIB 4.1 - C routines that implement the FITS World Coordinate System
*   (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (Paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (Paper II)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2005, A&A,
*      (Paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   These routines implement the FITS World Coordinate System (WCS) standard
*   which defines methods to be used for computing world coordinates from
*   image pixel coordinates, and vice versa.  They are based on the wcsprm
*   struct, described in detail below, which contains all information needed
*   for the computations.  The struct contains some members that must be set
*   by the caller, and others that are maintained by these routines, somewhat
*   like a C++ class but with no encapsulation.
*
*   Three routines, wcsini(), wcssub(), and wcsfree() are provided to manage
*   the wcsprm struct and another, wcsprt(), to prints its contents.  See
*   "Coordinate transformation parameters" below for an explanation of the
*   anticipated usage of these routines.
*
*   A setup routine, wcsset(), computes intermediate values in the wcsprm
*   struct from parameters in it that were supplied by the caller.  The
*   struct always needs to be set up by wcsset() but this need not be called
*   explicitly - see the explanation of wcs.flag below.
*
*   wcsp2s() and wcss2p() implement the WCS world coordinate transformations.
*   In fact, they are high level driver routines for the WCS linear,
*   logarithmic, celestial, spectral and tabular transformation routines
*   described in lin.h, log.h, cel.h, spc.h and tab.h.
*
*   Given either the celestial longitude or latitude plus an element of the
*   pixel coordinate a hybrid routine, wcsmix(), iteratively solves for the
*   unknown elements.
*
*   wcssptr() translates the spectral axis in a wcsprm struct.  For example, a
*   'FREQ' axis may be translated into 'ZOPT-F2W' and vice versa.
*
*
*   Default constructor for the wcsprm struct; wcsini()
*   ---------------------------------------------------
*   wcsini() optionally allocates memory for arrays in a wcsprm struct and
*   sets all members of the struct to default values.  Memory is allocated for
*   up to NPVMAX PVi_ma cards or NPSMAX PSi_ma cards per WCS representation.
*   These *   may be changed via wcsnpv() and wcsnps() before wcsini() is
*   called.
*
*   N.B. every wcsprm struct should be initialized by wcsini(), possibly
*   repeatedly.  On the first invokation, and only the first invokation, the
*   flag member of the wcsprm struct must be set to -1 to initialize memory
*   management, regardless of whether wcsini() will actually be used to
*   allocate memory.
*
*   Given:
*      alloc    int      If true, allocate memory unconditionally for the
*                        crpix, etc. arrays (see "Memory allocation and
*                        deallocation below").
*
*                        If false, it is assumed that pointers to these arrays
*                        have been set by the caller except if they are null
*                        pointers in which case memory will be allocated for
*                        them regardless.  (In other words, setting alloc true
*                        saves having to initalize these pointers to zero.)
*
*      naxis    int      The number of world coordinate axes.  This is used to
*                        determine the length of the various wcsprm vectors
*                        and matrices and therefore the amount of memory to
*                        allocate for them.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*                        Note that, in order to initialize memory management
*                        wcs->flag should be set to -1 when wcs is initialized
*                        for the first time (memory leaks may result if it had
*                        already been initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*
*
*   Memory allocation for PVi_ma and PSi_ma; wcsnpv() and wcsnps()
*   --------------------------------------------------------------
*   wcsnpv() and wcsnps() change the values of NPVMAX (default 64) and NPSMAX
*   (default 8).  These global variables control the number of PVi_ma and
*   PSi_ma cards that wcsini() should allocate space for.
*
*   Given:
*      n        int      Value of NPVMAX or NPSMAX; ignored if < 0.
*
*   Function return value:
*               int      Current value of NPVMAX or NPSMAX.
*
*
*   Subimage extraction routine for the wcsprm struct; wcssub()
*   -----------------------------------------------------------
*   wcssub() extracts the coordinate description for a subimage from a wcsprm
*   struct.  It does a deep copy, using wcsini() to allocate memory for its
*   arrays if required.  Only the "information to be provided" part of the
*   struct is extracted; a call to wcsset() is required to set up the
*   remainder.
*
*   The world coordinate system of the subimage must be separable in
*   the sense that the world coordinates at any point in the subimage must
*   depend only on the pixel coordinates of the axes extracted.  In practice,
*   this means that the PCi_ja matrix of the original image must not contain
*   non-zero off-diagonal terms that associate any of the subimage axes with
*   any of the non-subimage axes.
*
*   Note that while the required elements of the tabprm array are extracted,
*   the wtbarr array is not.  (Thus it is not appropriate to call wcssub()
*   after wcstab() but before filling the tabprm structs - refer to wcshdr.h.)
*
*   Given:
*      alloc    int      If true, allocate memory for the crpix, etc. arrays
*                        in the destination (see "Memory allocation and
*                        deallocation below").  Otherwise, it is assumed that
*                        pointers to these arrays have been set by the caller
*                        except if they are null pointers in which case memory
*                        will be allocated for them regardless.
*
*      wcssrc   const struct wcsprm*
*                        Struct to extract from.
*
*   Given and returned:
*      nsub     int*
*      axes     int[]    Vector of length *nsub containing the image axis
*                        numbers (1-relative) to extract.  Order is
*                        significant; axes[0] is the axis number of the input
*                        image that corresponds to the first axis in the
*                        subimage, etc.
*
*                        nsub (the pointer) may be set to zero, and so also
*                        may *nsub, to indicate the number of axes in the
*                        input image; the number of axes will be returned if
*                        nsub != 0.  axes itself (the pointer) may be set to
*                        zero to indicate the first *nsub axes in their
*                        original order.
*
*                        Set both nsub and axes to zero to do a deep copy of
*                        one wcsprm struct to another.
*
*                        Subimage extraction by coordinate axis type may be
*                        done by setting the elements of axes[] to the
*                        following special preprocessor macro values:
*
*                           WCSSUB_LONGITUDE: Celestial longitude.
*                           WCSSUB_LATITUDE:  Celestial latitude.
*                           WCSSUB_CUBEFACE:  Quadcube CUBEFACE axis.
*                           WCSSUB_SPECTRAL:  Spectral axis.
*                           WCSSUB_STOKES:    Stokes axis.
*
*                        See note 2 below for further usage notes.
*
*                        On return, *nsub will contain the number of axes in
*                        the subimage; this may be zero if there were no axes
*                        of the required type(s) (in which case no memory will
*                        be allocated).  axes[] will contain the axis numbers
*                        that were extracted.  The vector length must be
*                        sufficient to contain all axis numbers.  No checks
*                        are performed to verify that the coordinate axes are
*                        consistent, this is done by wcsset().
*
*      wcsdst   struct wcsprm*
*                        Struct describing the subimage.  wcsdst->flag should
*                        be set to -1 if wcsdst was not previously initialized
*                        (memory leaks may result if it was previously
*                        initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                          12: Invalid subimage specification.
*                          13: Non-separable subimage coordinate system.
*
*
*   Copy routine for the wcsprm struct; wcscopy()
*   ---------------------------------------------
*   wcscopy() does a deep copy of one wcsprm struct to another.  As of
*   WCSLIB 3.6, it is implemented as a preprocessor macro that invokes
*   wcssub() with the nsub and axes pointers both set to zero.
*
*
*   Destructor for the wcsprm struct; wcsfree()
*   -------------------------------------------
*   wcsfree() frees memory allocated for the wcsprm arrays by wcsini() and/or
*   wcsset().  wcsini() records the memory it allocates and wcsfree() will
*   only attempt to free this.
*
*   N.B. wcsfree() must not be invoked on a wcsprm struct that was not
*   initialized by wcsini().
*
*   Returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Print routine for the wcsprm struct; wcsprt()
*   ---------------------------------------------
*   wcsprt() prints the contents of a wcsprm struct.
*
*   Given:
*      wcs      const struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Setup routine; wcsset()
*   -----------------------
*   wcsset() sets up a wcsprm struct according to information supplied within
*   it (see "Coordinate transformation parameters" below).
*
*   wcsset() recognizes the NCP projection and converts it to the equivalent
*   SIN projection and it also recognizes GLS as a synonym for SFL.  It does
*   alias translation for the AIPS spectral types ('FREQ-LSR', 'FELO-HEL',
*   etc.) but without changing the input header cards.
*
*   Note that this routine need not be called directly; it will be invoked by
*   wcsp2s() and wcss2p() if the "flag" struct member is anything other than a
*   predefined magic value.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*
*
*   Pixel-to-world transformation; wcsp2s()
*   ---------------------------------------
*   wcsp2s() transforms pixel coordinates to world coordinates.
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing wcs.naxis coordinate elements.
*                        Thus nelem must equal or exceed the value of the
*                        NAXIS keyword unless ncoord == 1, in which case nelem
*                        is not used.
*      pixcrd   const double[ncoord][nelem]
*                        Array of pixel coordinates.
*
*   Returned:
*      imgcrd   double[ncoord][nelem]
*                        Array of intermediate world coordinates.  For
*                        celestial axes, imgcrd[][wcs.lng] and
*                        imgcrd[][wcs.lat] are the projected x-, and
*                        y-coordinates, in "degrees".  For spectral axes,
*                        imgcrd[][wcs.spec] is the intermediate spectral
*                        coordinate, in SI units.
*
*      phi,     double[ncoord]
*      theta             Longitude and latitude in the native coordinate
*                        system of the projection, in degrees.
*
*      world    double[ncoord][nelem]
*                        Array of world coordinates.  For celestial axes,
*                        world[][wcs.lng] and world[][wcs.lat] are the
*                        celestial longitude and latitude, in degrees.  For
*                        spectral axes, imgcrd[][wcs.spec] is the intermediate
*                        spectral coordinate, in SI units.
*
*      stat     int[ncoord]
*                        Status return value for each coordinate:
*                           0: Success.
*                           1: Invalid pixel coordinate.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*                           8: One or more of the pixel coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   World-to-pixel transformation; wcss2p()
*   ---------------------------------------
*   wcss2p() transforms world coordinates to pixel coordinates.
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing wcs.naxis coordinate elements.
*                        Thus nelem must equal or exceed the value of the
*                        NAXIS keyword unless ncoord == 1, in which case nelem
*                        is not used.
*      world    const double[ncoord][nelem]
*                        Array of world coordinates.  For celestial axes,
*                        world[][wcs.lng] and world[][wcs.lat] are the
*                        celestial longitude and latitude, in degrees. For
*                        spectral axes, world[][wcs.spec] is the spectral
*                        coordinate, in SI units.
*
*   Returned:
*      phi,     double[ncoord]
*      theta             Longitude and latitude in the native coordinate
*                        system of the projection, in degrees.
*
*      imgcrd   double[ncoord][nelem]
*                        Array of intermediate world coordinates.  For
*                        celestial axes, imgcrd[][wcs.lng] and
*                        imgcrd[][wcs.lat] are the projected x-, and
*                        y-coordinates, in "degrees".  For quadcube
*                        projections with a CUBEFACE axis the face number is
*                        also returned in imgcrd[][wcs.cubeface].  For
*                        spectral axes, imgcrd[][wcs.spec] is the intermediate
*                        spectral coordinate, in SI units.
*
*      pixcrd   double[ncoord][nelem]
*                        Array of pixel coordinates.
*
*      stat     int[ncoord]
*                        Status return value for each coordinate:
*                           0: Success.
*                           1: Invalid world coordinate.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*                           9: One or more of the world coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Hybrid transformation; wcsmix()
*   -------------------------------
*   wcsmix(), given either the celestial longitude or latitude plus an element
*   of the pixel coordinate, solves for the remaining elements by iterating on
*   the unknown celestial coordinate element using wcss2p().
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Given:
*      mixpix   int      Which element of the pixel coordinate is given.
*      mixcel   int      Which element of the celestial coordinate is given:
*                           1: Celestial longitude is given in
*                              world[wcs.lng], latitude returned in
*                              world[wcs.lat].
*                           2: Celestial latitude is given in
*                              world[wcs.lat], longitude returned in
*                              world[wcs.lng].
*
*      vspan    const double[2]
*                        Solution interval for the celestial coordinate, in
*                        degrees.  The ordering of the two limits is
*                        irrelevant.  Longitude ranges may be specified with
*                        any convenient normalization, for example [-120,+120]
*                        is the same as [240,480], except that the solution
*                        will be returned with the same normalization, i.e.
*                        lie within the interval specified.
*
*      vstep    const double
*                        Step size for solution search, in degrees.  If zero,
*                        a sensible, although perhaps non-optimal default will
*                        be used.
*
*      viter    int      If a solution is not found then the step size will be
*                        halved and the search recommenced.  viter controls
*                        how many times the step size is halved.  The allowed
*                        range is 5 - 10.
*
*   Given and returned:
*      world    double[naxis]
*                        World coordinate elements.  world[wcs.lng] and
*                        world[wcs.lat] are the celestial longitude and
*                        latitude, in degrees.  Which is given and which
*                        returned depends on the value of mixcel.  All other
*                        elements are given.
*
*   Returned:
*      phi,     double[naxis]
*      theta             Longitude and latitude in the native coordinate
*                        system of the projection, in degrees.
*
*      imgcrd   double[naxis]
*                        Image coordinate elements.  imgcrd[wcs.lng] and
*                        imgcrd[wcs.lat] are the projected x-, and
*                        y-coordinates, in "degrees".
*
*   Given and returned:
*      pixcrd   double[naxis]
*                        Pixel coordinate.  The element indicated by mixpix is
*                        given and the remaining elements are returned.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*                          10: Invalid world coordinate.
*                          11: No solution found in the specified interval.
*
*
*   Spectral axis translation; wcssptr()
*   ------------------------------------
*   wcssptr() translates the spectral axis in a wcsprm struct.  For example, a
*   'FREQ' axis may be translated into 'ZOPT-F2W' and vice versa.
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*      i        int*     Index of the spectral axis (0-rel).  If given < 0 it
*                        will be set to the first spectral axis identified
*                        from the ctype[] cards in the wcsprm struct.
*      ctype    char[9]  Required spectral CTYPEia.  Wildcarding may be used
*                        as for the ctypeS2 argument to spctrn() as described
*                        in the prologue of spc.h, i.e. if the final three
*                        characters are specified as "???", or if just the
*                        eighth character is specified as '?', the correct
*                        algorithm code will be substituted and returned.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Linear transformation matrix is singular.
*                           4: Inconsistent or unrecognized coordinate axis
*                              types.
*                           5: Invalid parameter value.
*                           6: Invalid coordinate transformation parameters.
*                           7: Ill-conditioned coordinate transformation
*                              parameters.
*                          12: Invalid subimage specification (no spectral
*                              axis).
*
*
*   Notes
*   -----
*    1) The quadcube projections (TSC, CSC, QSC) may be represented in FITS in
*       either of two ways:
*
*          a) The six faces may be laid out in one plane and numbered as
*             follows:
*
*                                       0
*
*                              4  3  2  1  4  3  2
*
*                                       5
*
*             Faces 2, 3 and 4 may appear on one side or the other (or both).
*             The world-to-pixel routines map faces 2, 3 and 4 to the left but
*             the pixel-to-world routines accept them on either side.
*
*          b) The "COBE" convention in which the six faces are stored in a
*             three-dimensional structure using a "CUBEFACE" axis indexed from
*             0 to 5 as above.
*
*       These routines support both methods; wcsset() determines which is
*       being used by the presence or absence of a CUBEFACE axis in ctype[].
*       wcsp2s() and wcss2p() translate the CUBEFACE axis representation to
*       the single plane representation understood by the lower-level WCSLIB
*       projection routines.
*
*    2) In wcssub(), combinations of subimage axes of particular types may be
*       extracted in the same order as they occur in the input image by
*       combining preprocessor codes, for example
*
*          *nsub = 1;
*          axes[0] = WCSSUB_LONGITUDE | WCSSUB_LATITUDE | WCSSUB_SPECTRAL;
*
*       would extract the longitude, latitude, and spectral axes in the same
*       order as the input image.  If one of each were present, *nsub = 3
*       would be returned.
*
*       For convenience, WCSSUB_CELESTIAL is defined as the combination
*       WCSSUB_LONGITUDE | WCSSUB_LATITUDE | WCSSUB_CUBEFACE.
*
*       The codes may also be negated to extract all but the types specified,
*       for example
*
*          *nsub = 4;
*          axes[0] = WCSSUB_LONGITUDE;
*          axes[1] = WCSSUB_LATITUDE;
*          axes[2] = WCSSUB_CUBEFACE;
*          axes[3] = -(WCSSUB_SPECTRAL | WCSSUB_STOKES);
*
*       The last of these specifies all axis types other than spectral or
*       Stokes.  Extraction is done in the order specified by axes[] a
*       longitude axis (if present) would be extracted first (via axes[0]) and
*       not subsequently (via axes[3]).  Likewise for the latitude and
*       cubeface axes in this example.
*
*       From the foregoing, it is apparent that the value of *nsub returned
*       may be less than or greater than that given.  However, it will never
*       exceed the number of axes in the input image.
*
*
*   Coordinate transformation parameters
*   ------------------------------------
*   The wcsprm struct consists of a number of data members that must be
*   supplied and others that are computed from them.
*
*   In practice, it is expected that a FITS header parser would scan the FITS
*   header until it encountered the NAXIS (or WCSAXESa) card which must occur
*   near the start of the header before any of the WCS keywords.  It would
*   then use wcsini() to allocate memory for arrays in the wcsprm struct and
*   set default values.  Then as it read and identified each WCS header card
*   it would load the value into the relevant wcsprm array element.  This is
*   essentially what wcspih() does - refer to the prologue of wcshdr.h.
*
*   As the final step, wcsset() is invoked, either directly or indirectly, to
*   set the derived members of the wcsprm struct.  wcsset() strips off
*   trailing blanks in all string members and null-fills the character array.
*
*      int flag
*         This flag must be set to zero whenever any of the following members
*         of the wcsprm struct are set or modified.  This signals the setup
*         routine, wcsset(), to recompute intermediaries.
*
*         flag should be set to -1 when wcsini() is called for the first time
*         for a wcsprm struct in order to initialize memory management.  It
*         must ONLY be used on the first initialization otherwise memory leaks
*         may result.
*
*      int naxis
*         Number of pixel and world coordinate elements, given by the NAXIS or
*         WCSAXESa keywords.
*
*      double *crpix
*         Address of the first element of an array of double containing the
*         coordinate reference pixel, CRPIXja.
*
*      double *pc
*         Address of the first element of the PCi_ja (pixel coordinate)
*         transformation matrix.  The expected order is
*
*            pc = {PC1_1, PC1_2, PC2_1, PC2_2};
*
*         This may be constructed conveniently from a two-dimensional array
*         via
*
*            double m[2][2] = {{PC1_1, PC1_2},
*                              {PC2_1, PC2_2}};
*
*         which is equivalent to,
*
*            double m[2][2];
*            m[0][0] = PC1_1;
*            m[0][1] = PC1_2;
*            m[1][0] = PC2_1;
*            m[1][1] = PC2_2;
*
*         The storage order in each case is
*
*            PC1_1, PC1_2, PC2_1, PC2_2
*
*         so it would be legitimate to set pc = *m.
*
*      double *cdelt
*         Address of the first element of an array of double containing the
*         coordinate increments, CDELTia.
*
*      double *crval
*         Address of the first element of an array of double containing the
*         coordinate reference values, CRVALia.
*
*      char (*cunit)[72]
*         Address of the first element of an array of char[72] containing the
*         CUNITia cards which define the units of measurement of the CRVALia,
*         CDELTia, and CDi_ja cards.
*
*         As CUNITia is an optional header card, cunit[][72] may be left blank
*         but otherwise is expected to contain a standard units specification
*         as defined by WCS Paper I.  Utility function wcsutrn(), described in
*         wcsunits.h, is available to translate commonly used non-standard
*         units specifications but this must be done as a separate step before
*         invoking wcsset().
*
*         For celestial axes, if cunit[][72] is not blank, wcsset() uses
*         wcsunits() to parse it and scale cdelt[], crval[], and cd[][*] to
*         degrees.  It then resets cunit[][72] to "deg".
*
*         For spectral axes, if cunit[][72] is not blank, wcsset() uses
*         wcsunits() to parse it and scale cdelt[], crval[], and cd[][*] to SI
*         units.  It then resets cunit[][72] accordingly.
*
*         wcsset() ignores cunit[][72] for other coordinate types; cunit[][72]
*         may be used to label coordinate values.
*
*         These variables accomodate the longest allowed string-valued FITS
*         keyword, being limited to 68 characters, plus the null-terminating
*         character.
*
*      char (*ctype)[72]
*         Address of the first element of an array of char[72] containing the
*         coordinate axis types, CTYPEia.
*
*         The ctype[][72] keyword values must be in upper case and there must
*         be zero or one pair of matched celestial axis types, and zero or one
*         spectral axis.  The ctype[][72] strings should be padded with blanks
*         on the right and null-terminated so that they are at least eight
*         characters in length.
*
*         These variables accomodate the longest allowed string-valued FITS
*         keyword, being limited to 68 characters, plus the null-terminating
*         character.
*
*      double lonpole, latpole
*         The native longitude and latitude of the celestial pole, LONPOLEa
*         and LATPOLEa (deg).  These may be left to default to values set by
*         wcsini() (as described in the prologue of cel.h), but in any case
*         they will be reset by wcsset() to the values actually used.  Note
*         therefore that if the wcsprm struct is reused without resetting
*         them, wether directly or via wcsini(), they will no longer have
*         their default values.
*
*      double restfrq, restwav
*         Rest frequency (Hz) and/or wavelength (m).
*
*      int npv, npvmax
*      struct pvcard *pv
*         Address of the first element of an array of length npvmax of pvcard
*         structs:
*
*            struct pvcard {
*               int i;			...Axis number, as in PVi_ma,
*                                          (i.e. 1-relative).
*               int m;			...Parameter number, as in PVi_ma,
*                                          (i.e. 0-relative).
*               double value;		...Parameter value.
*            };
*
*         As a FITS header parser encounters each PVi_ma card it should load
*         it into a pvcard struct in the array and increment npv.  wcsset()
*         interprets these as required.
*
*         npvmax will be set by wcsini() if it allocates memory for pv[],
*         otherwise it must be set by the caller.  See also wcsnpv() above.
*
*      int nps, npsmax
*      struct pscard *ps
*         Address of the first element of an array of length npsmax of pscard
*         structs:
*
*            struct pscard {
*               int i;			...Axis number, as in PSi_ma,
*                                          (i.e. 1-relative).
*               int m;			...Parameter number, as in PSi_ma,
*                                          (i.e. 0-relative).
*               char value[72];		...Parameter value.
*            };
*
*         As a FITS header parser encounters each PSi_ma card it should load
*         it into a pscard struct in the array and increment nps.  wcsset()
*         interprets these as required (currently no PSi_ma cards are
*         recognized).
*
*         npsmax will be set by wcsini() if it allocates memory for ps[],
*         otherwise it must be set by the caller.  See also wcsnps() above.
*
*      int altlin
*      double *cd
*      double *crota
*         For historical compatibility, the wcsprm struct supports two
*         alternate specifications of the linear transformation matrix, those
*         associated with the CDi_ja and CROTAia cards.  Although these may
*         not formally co-exist with PCi_ja, the approach here is simply to
*         ignore them if given in conjunction with PCi_ja.
*
*         altlin is a bit flag that denotes which of these cards are present
*         in the header:
*
*         Bit 0: PCi_ja is present.
*
*         Bit 1: CDi_ja is present.  Matrix elements in the IRAF convention
*                are equivalent to the product CDi_ja = CDELTia * PCi_ja, but
*                the defaults differ from that of the PCi_ja matrix.  If one
*                or more CDi_ja cards are present then all unspecified CDi_ja
*                default to zero.  If no CDi_ja (or CROTAia) cards are
*                present, then the header is assumed to be in PCi_ja form
*                whether or not any PCi_ja cards are present since this
*                results in an interpretation of CDELTia consistent with the
*                original FITS specification.
*
*                While CDi_ja may not formally co-exist with PCi_ja, it may
*                co-exist with CDELTia and CROTAia which are to be ignored.
*
*         Bit 2: CROTAia is present.  In the AIPS convention, CROTAia may only
*                be associated with the latitude axis of a celestial axis
*                pair.  It specifies a rotation in the image plane that is
*                applied AFTER the CDELTia; any other CROTAia cards are
*                ignored.
*
*                CROTAia may not formally co-exist with PCi_ja.  CROTAia and
*                CDELTia may formally co-exist with CDi_ja but if so are to be
*                ignored.
*
*         CDi_ja and CROTAia cards, if found, are to be stored in the cd and
*         crota arrays which are dimensioned similarly to pc and cdelt.  FITS
*         header parsers should use the following precedure:
*            Whenever a PCi_ja  card is encountered: altlin |= 1;
*            Whenever a CDi_ja  card is encountered: altlin |= 2;
*            Whenever a CROTAia card is encountered: altlin |= 4;
*         If none of these bits are set the PCi_ja representation results,
*         i.e. pc and cdelt will be used as given.
*
*         These alternate specifications of the linear transformation matrix
*         are translated immediately to PCi_ja by wcsset() and are nowhere
*         visible to the lower-level routines.  In particular, wcsset() resets
*         cdelt to unity if CDi_ja is present (and no PCi_ja).  If no CROTAia
*         is associated with the latitude axis, wcsset() reverts to a unity
*         PCi_ja matrix.
*
*   The following members of the wcsprm struct are provided solely so that it
*   may contain a complete copy of all WCS cards associated with a particular
*   coordinate representation.  They are not used by any WCSLIB routines.
*
*      char alt[4]
*         Character code for alternate coordinate descriptions (i.e. the "a"
*         in keyword names such as CTYPEia).  This is blank for the primary
*         coordinate description, or one of the 26 upper-case letters, A-Z.
*
*         An array of four characters is provided for alignment purposes,
*         only the first is used.
*
*      int colnum
*         Where the coordinate representation is associated with an image-
*         array column in a FITS binary table, this variable may be used to
*         record the relevant column number.
*
*         It should be set to zero for an image header, or a binary table for
*         which there is no association with a specific column.
*
*      char wcsname[72]
*      char (*cname)[72]
*         The name given to the coordinate representation WCSNAMEa, and the
*         address of the first element of an array of char[72] containing the
*         coordinate axis names, CNAMEia.
*
*         These variables accomodate the longest allowed string-valued FITS
*         keyword, being limited to 68 characters, plus the null-terminating
*         character.
*
*      double *crder, *csyer
*         Addresses of the first elements of arrays of double recording the
*         random and systematic error in the coordinate value, CRDERia and
*         CSYERia.
*
*      char radesys[72]
*      double equinox
*         The equatorial or ecliptic coordinate system type, RADESYSa, and for
*         the dynamical systems, the associated equinox, EQUINOXa (or EPOCH in
*         older headers).
*
*      char specsys[72], ssysobs[72]
*      double velosys
*         Spectral reference frame (standard of rest), SPECSYSa, and the
*         actual frame in which there is no differential variation in the
*         spectral coordinate across the field-of-view, SSYSOBSa.  The
*         relative radial velocity (m/s) between the observer and the selected
*         standard of rest in the direction of the celestial reference
*         coordinate, VELOSYSa.
*
*      char ssyssrc[72]
*      double zsource
*         The redshift, ZSOURCEa, of the source and the spectral reference
*         frame (standard of rest) in which this was measured, SSYSSRCa.
*
*      double obsgeo[3]
*         Location of the observer in a standard terrestrial reference frame,
*         OBSGEO-X, OBSGEO-Y, OBSGEO-Z (in metres).
*
*      char dateobs[72], dateavg[72]
*         The date of observation in ISO format, yyyy-mm-ddThh:mm:ss.
*         DATE-OBS refers to the start of the observation unless otherwise
*         explained in the comment field of the DATE-OBS card, and DATE-AVG
*         refers to a representative mid-point of the observation.
*
*      double mjdobs, mjdavg
*         Modified Julian Date (MJD = JD - 2400000.5), MJD-OBS and MJD-AVG,
*         corresponding to DATE-OBS and DATE-AVG.
*
*
*   The tabprm structs contain some members that must be supplied and others
*   that are derived.  The information to be supplied comes primarily from
*   arrays stored in one or more FITS binary table extensions.  These arrays,
*   referred to here as "wcstab arrays", are themselves located by parameters
*   stored in the FITS image header.
*
*   Function wcstab(), which is invoked automatically by wcspih(), sets up an
*   array of wtbarr structs to assist in extracting this information.  Refer
*   to the usage notes for wcspih() and wcstab() in wcshdr.h, and also the
*   prologue to tab.h.
*
*      int ntab
*      struct tabprm *tab
*         Address of the first element of an array of ntab tabprm structs used
*         to store tabular transformation parameters.
*
*      int nwtb
*      struct wtbarr *wtb
*         Address of the first element of an array of nwtb wtbarr structs used
*         in extracting wcstab arrays from a FITS binary table.  The wtbarr
*         structs contain the following members:
*
*         int i
*            Image axis number.
*
*         int m
*            wcstab array axis number for index vectors.
*
*         int kind
*            Character identifying the wcstab array type:
*               'c': coordinate array,
*               'i': index vector.
*
*         char extnam[72]
*            EXTNAME identifying the binary table extension.
*
*         int extver
*            EXTVER identifying the binary table extension.
*
*         int extlev
*            EXTLEV identifying the binary table extension.
*
*         char ttype[72]
*            TTYPEn identifying the column of the binary table that contains
*            the wcstab array.
*
*         long row
*            Table row number.
*
*         int ndim
*            Expected dimensionality of the wcstab array.
*
*         int *dimlen
*            Address of the first element of an array of int of length ndim
*            into which the wcstab array axis lengths are to be written.
*
*         double **arrayp
*            Pointer to an array of double which is to be allocated by the
*            user and into which the wcstab array is to be written.
*
*
*   The remaining members of the wcsprm struct are maintained by wcsset() and
*   must not be modified elsewhere:
*
*      double *types
*         Address of the first element of an array of int containing a four-
*         digit type code for each axis.
*
*            First digit (i.e. 1000s):
*               0: Non-specific coordinate type.
*               1: Stokes coordinate.
*               2: Celestial coordinate (including CUBEFACE).
*               3: Spectral coordinate.
*
*            Second digit (i.e. 100s):
*               0: Linear axis.
*               1: Quantized axis (STOKES, CUBEFACE).
*               2: Non-linear celestial axis.
*               3: Non-linear spectral axis.
*               4: Logarithmic axis.
*               5: Tabular axis.
*
*            Third digit (i.e. 10s):
*               0: Group number, e.g. lookup table number, being an index into
*                  the tabprm array (see below).
*
*         The fourth digit is used as a qualifier depending on the axis type.
*         For celestial axes:
*               0: Longitude coordinate.
*               1: Latitude coordinate.
*               2: CUBEFACE number.
*         For lookup tables: the axis number in a multidimensional table.
*
*         CTYPEia in "4-3" form with unrecognized algorithm code will have its
*         type set to -1 and generate an error.
*
*      char lngtyp[8], lattyp[8]
*         Four-character WCS celestial axis types. e.g. RA, DEC, GLON, GLAT,
*         etc.  (Declared as char[8] for alignment reasons.)
*
*      int lng, lat, spec
*         Indices into the imgcrd[][], and world[][] arrays as described
*         above. These may also serve as indices into the pixcrd[][] array
*         provided that the PCi_ja matrix does not transpose axes.
*
*      int cubeface
*         Index into the pixcrd[][] array for the CUBEFACE axis.  This is used
*         for quadcube projections where the cube faces are stored on a
*         separate axis (see note 1 above).
*
*      struct linprm lin
*         Linear transformation parameters (usage is described in the
*         prologue to lin.h).
*
*      struct celprm cel
*         Celestial transformation parameters (usage is described in the
*         prologue to cel.h).
*
*      struct spcprm spc
*         Spectral transformation parameters (usage is described in the
*         prologue to spc.h).
*
*      The remaining members of the wcsprm struct are used for memory
*      management by wcsini() and wcsfree().
*
*
*   Vector arguments
*   ----------------
*   Arrays of pixel and world coordinates are two dimensional, i.e.
*   pixcrd[ncoord][nelem], where nelem must equal or exceed the number of
*   coordinate elements, naxis (i.e. the value of the NAXIS keyword), stored
*   in the wcsprm struct.  Exception: when ncoord == 1, nelem is not used.
*
*   Note that the function prototypes must declare two-dimensional arrays as
*   one-dimensional to avoid warnings about declaration of "incomplete types".
*   This was considered preferable to declaring them as simple pointers-to-
*   double which gives no indication that storage is associated with them.
*
*
*   Memory allocation and deallocation
*   ----------------------------------
*   wcsini() optionally allocates memory for the crpix, pc, cdelt, crval,
*   cunit, ctype, pv, ps, cd, crota, cname, crder, and csyer arrays in the
*   wcsprm struct as described in the usage notes above.
*
*   Note that wcsini() does not allocate memory for the tab array - refer to
*   the usage notes for wcstab() in wcshdr.h.
*
*   If the pc matrix is not unity, wcsset() also allocates memory for the
*   piximg and imgpix arrays.  The caller must not modify these.
*
*   wcsini() maintains a record of memory it has allocated and this is used
*   by wcsfree().  wcsini() uses wcsfree() to free any memory that it may have
*   allocated on a previous invokation.  Thus it is not necessary for the
*   caller to invoke wcsfree() separately if wcsini() is invoked repeatedly on
*   the same wcsprm struct.  Likewise, wcsset() deallocates memory that it may
*   have allocated in the same wcsprm struct on a previous invokation.
*
*   A memory leak will result if a wcsprm struct goes out of scope before the
*   memory has been free'd, either by wcsfree() or otherwise.  Likewise, if
*   the wcsprm struct itself has been malloc'd and the allocated memory is not
*   free'd when the memory for the struct is free'd.  A leak may also arise if
*   the caller interferes with the array pointers in the "private" part of the
*   wcsprm struct.
*
*   Beware of making a shallow copy of a wcsprm struct by assignment; any
*   changes made to allocated memory in one would be reflected in the other,
*   and if the memory allocated for one was free'd the other would reference
*   unallocated memory.  Use wcssub() instead to make a deep copy.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   encoded in the wcs_errmsg character array.
*
*
*   wcsmix() algorithm
*   ------------------
*   Initially the specified solution interval is checked to see if it's a
*   "crossing" interval.  If it isn't, a search is made for a crossing
*   solution by iterating on the unknown celestial coordinate starting at the
*   upper limit of the solution interval and decrementing by the specified
*   step size.  A crossing is indicated if the trial value of the pixel
*   coordinate steps through the value specified.  If a crossing interval is
*   found then the solution is determined by a modified form of "regula falsi"
*   division of the crossing interval.  If no crossing interval was found
*   within the specified solution interval then a search is made for a "non-
*   crossing" solution as may arise from a point of tangency.  The process is
*   complicated by having to make allowance for the discontinuities that occur
*   in all map projections.
*
*   Once one solution has been determined others may be found by subsequent
*   invokations of wcsmix() with suitably restricted solution intervals.
*
*   Note the circumstance that arises when the solution point lies at a
*   native pole of a projection in which the pole is represented as a finite
*   curve, for example the zenithals and conics.  In such cases two or more
*   valid solutions may exist but WCSMIX only ever returns one.
*
*   Because of its generality wcsmix() is very compute-intensive.  For
*   compute-limited applications more efficient special-case solvers could be
*   written for simple projections, for example non-oblique cylindrical
*   projections.
*
*===========================================================================*/

#ifndef WCSLIB_WCS
#define WCSLIB_WCS

#include <lin.h>
#include <cel.h>
#include <spc.h>
#include <tab.h>

#ifdef __cplusplus
extern "C" {
#endif

#define WCSSUB_LONGITUDE 0x1001
#define WCSSUB_LATITUDE  0x1002
#define WCSSUB_CUBEFACE  0x1004
#define WCSSUB_CELESTIAL 0x1007
#define WCSSUB_SPECTRAL  0x1008
#define WCSSUB_STOKES    0x1010


extern const char *wcs_errmsg[];
#define wcsini_errmsg wcs_errmsg
#define wcssub_errmsg wcs_errmsg
#define wcscopy_errmsg wcs_errmsg
#define wcsfree_errmsg wcs_errmsg
#define wcsprt_errmsg wcs_errmsg
#define wcsset_errmsg wcs_errmsg
#define wcsp2s_errmsg wcs_errmsg
#define wcss2p_errmsg wcs_errmsg
#define wcsmix_errmsg wcs_errmsg

				/* Struct used for storing PVi_ma cards.    */
struct pvcard {
   int i;			/* Axis number, as in PVi_ma (1-relative).  */
   int m;			/* Parameter number, ditto  (0-relative).   */
   double value;		/* Parameter value.                         */
};

				/* Struct used for storing PSi_ma cards.    */
struct pscard {
   int i;			/* Axis number, as in PSi_ma (1-relative).  */
   int m;			/* Parameter number, ditto  (0-relative).   */
   char value[72];		/* Parameter value.                         */
};

				/* For extracting wcstab arrays.            */
struct wtbarr {
   int  i;			/* Image axis number.                       */
   int  m;			/* Array axis number for index vectors.     */
   int  kind;			/* wcstab array type.                       */
   char extnam[72];		/* EXTNAME of binary table extension.       */
   int  extver;			/* EXTVER  of binary table extension.       */
   int  extlev;			/* EXTLEV  of binary table extension.       */
   char ttype[72];		/* TTYPEn of column containing the array.   */
   long row;			/* Table row number.                        */
   int  ndim;			/* Expected wcstab array dimensionality.    */
   int  *dimlen;		/* Where to write the array axis lengths.   */
   double **arrayp;		/* Where to write the address of the array  */
                                /* allocated to store the wcstab array.     */
};


struct wcsprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* FITS header cards to be provided (see the prologue above).            */
   /*-----------------------------------------------------------------------*/
   int    naxis;		/* Number of axes (pixel and coordinate).   */
   double *crpix;		/* CRPIXja cards for each pixel axis.       */
   double *pc;			/* PCi_ja  linear transformation matrix.    */
   double *cdelt;		/* CDELTia cards for each coordinate axis.  */
   double *crval;		/* CRVALia cards for each coordinate axis.  */

   char   (*cunit)[72];		/* CUNITia cards for each coordinate axis.  */
   char   (*ctype)[72];		/* CTYPEia cards for each coordinate axis.  */

   double lonpole;		/* LONPOLEa card.                           */
   double latpole;		/* LATPOLEa card.                           */

   double restfrq;		/* RESTFRQa card.                           */
   double restwav;		/* RESTWAVa card.                           */

   int    npv;			/* Number of PVi_ma cards, and the          */
   int    npvmax;		/* number for which space was allocated.    */
   struct pvcard *pv;		/* PVi_ma cards for each i and m.           */

   int    nps;			/* Number of PSi_ma cards, and the          */
   int    npsmax;		/* number for which space was allocated.    */
   struct pscard *ps;		/* PSi_ma cards for each i and m.           */

   /* Alternative header cards (see the prologue above).                    */
   /*-----------------------------------------------------------------------*/
   int    altlin;		/* Alternative representations              */
				/*   Bit 0: PCi_ja  is present,             */
				/*   Bit 1: CDi_ja  is present,             */
				/*   Bit 2: CROTAia is present.             */
   double *cd;			/* CDi_ja linear transformation matrix.     */
   double *crota;		/* CROTAia cards for each coordinate axis.  */

   /* Auxiliary coordinate system information, not used by WCSLIB.          */
   char   alt[4];
   int    colnum;
   char   wcsname[72];
   char   (*cname)[72];
   double *crder;
   double *csyer;
   char   radesys[72];
   double equinox;
   char   specsys[72];
   char   ssysobs[72];
   double velosys;
   char   ssyssrc[72];
   double zsource;
   double obsgeo[3];
   char   dateobs[72];
   char   dateavg[72];
   double mjdobs;
   double mjdavg;

   /* Coordinate lookup tables (see the prologue above).                    */
   /*-----------------------------------------------------------------------*/
   int    ntab;			/* Number of separate tables.               */
   struct tabprm *tab;		/* Tabular transformation parameters.       */

   int    nwtb;			/* Number of wtbarr structs.                */
   struct wtbarr *wtb;		/* Array of wtbarr structs.                 */

   /* Information derived from the FITS header cards by wcsset().           */
   /*-----------------------------------------------------------------------*/
   int    *types;		/* Coordinate type codes for each axis.     */
   char   lngtyp[8], lattyp[8];	/* Celestial axis types, e.g. RA, DEC.      */
   int    lng, lat, spec;	/* Longitude, latitude and spectral axis    */
                                /* indices (0-relative).                    */
   int    cubeface;		/* True if there is a CUBEFACE axis.        */
   int    padding1;		/* (Dummy inserted for alignment purposes.) */

   struct linprm lin;		/* Linear    transformation parameters.     */
   struct celprm cel;		/* Celestial transformation parameters.     */
   struct spcprm spc;		/* Spectral  transformation parameters.     */

   int    m_flag, m_naxis;	/* The remainder are for memory management. */
   double *m_crpix, *m_pc, *m_cdelt, *m_crval;
   char  (*m_cunit)[72], (*m_ctype)[72];
   struct pvcard *m_pv;
   struct pscard *m_ps;
   double *m_cd, *m_crota;
   char  (*m_cname)[72];
   double *m_crder, *m_csyer;
   struct tabprm *m_tab;
   struct wtbarr *m_wtb;
   int    padding2;		/* (Dummy inserted for alignment purposes.) */
};

#define WCSLEN (sizeof(struct wcsprm)/sizeof(int))


int wcsnpv(int);
int wcsnps(int);

int wcsini(int, int, struct wcsprm *);
int wcssub(int, const struct wcsprm *, int *, int[], struct wcsprm *);
int wcsfree(struct wcsprm *);
int wcsprt(const struct wcsprm *);
int wcsset(struct wcsprm *);
int wcsp2s(struct wcsprm *, int, int, const double[],
           double[], double[], double[], double[], int[]);
int wcss2p(struct wcsprm *, int, int, const double[],
           double[], double[], double[], double[], int[]);
int wcsmix(struct wcsprm *, int, int, const double[], double, int,
           double[], double[], double[], double[], double[]);
int wcssptr(struct wcsprm *, int *, char [9]);

#define wcscopy(alloc, wcssrc, wcsdst) wcssub(alloc, wcssrc, 0, 0, wcsdst)


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCS */
