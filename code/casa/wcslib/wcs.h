/*============================================================================
*
*   WCSLIB 3.4 - an implementation of the FITS WCS convention.
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
*=============================================================================
*
*   WCSLIB 3.4 - C routines that implement the FITS World Coordinate System
*   (WCS) convention.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*      "Representations of celestial coordinates in FITS",
*      Calabretta, M.R., & Greisen, E.W. 2002, A&A, 395, 1077 (paper II)
*
*      "Representations of spectral coordinates in FITS",
*      Greisen, E.W., Valdes, F.G., Calabretta, M.R., & Allen, S.L. 2004, A&A,
*      (paper III, in preparation)
*
*
*   Summary of routines
*   -------------------
*   wcsp2s() and wcss2p() are high level driver routines for the WCS linear,
*   celestial, and spectral transformation routines.
*
*   Given either the celestial longitude or latitude plus an element of the
*   pixel coordinate a hybrid routine, wcsmix(), iteratively solves for the
*   unknown elements.
*
*   An initialization routine, wcsset(), computes indices from the ctype
*   array but need not be called explicitly - see the explanation of
*   wcs.flag below.
*
*   Three service routines, wcsini(), wcscopy(), and wcsfree() are provided to
*   manage the wcsprm struct.  A third, wcsprt(), prints its contents.  See
*   "Coordinate transformation parameters" below for an explanation of the
*   anticipated usage of these routines.
*
*
*   Service routines for the wcsprm struct; wcsini(), wcscopy(), & wcsfree()
*   -----------------------------------------------------------------------
*   These service routines are provided to manage the wcsprm struct (see also
*   "Memory allocation and deallocation" below).
*
*   wcsini() allocates memory for the crpix, pc, cdelt, ctype, cunit, crval,
*   and pv arrays and sets the members of the wcsprm struct to default values.
*   Memory is allocated for up to NPVMAX (default 64) PVi_m cards per WCS
*   representation.
*
*   Given:
*      alloc    int      If true, allocate memory for the crpix, pc, cdelt,
*                        ctype, cunit, crval, and pv arrays.  Otherwise, it is
*                        assumed that pointers to these arrays have been set
*                        by the caller except if they are null pointers in
*                        which case memory will be allocated for them
*                        regardless.  (In other words, setting alloc true
*                        saves having to initalize these pointers to zero.) 
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
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
*
*
*   wcscopy() does a deep copy of one wcsprm struct to another, using wcsini()
*   to allocate memory for its arrays if required.  Only the "information to
*   be provided" part of the struct is copied; a call to wcsset() is required
*   to initialize the remainder.
*
*   Given:
*      alloc    int      If true, allocate memory for the crpix, pc, cdelt,
*                        ctype, cunit, crval, and pv arrays in the
*                        destination.  Otherwise, it is assumed that pointers
*                        to these arrays have been set by the caller except if
*                        they are null pointers in which case memory will be
*                        allocated for them regardless.
*      wcsfrom  const struct wcsprm*
*                        Struct to copy from.
*
*   Given and returned:
*      wcsto    struct wcsprm*
*                        Struct to copy to.  wscto->flag should be set to -1
*                        if wcsto was not previously initialized (memory
*                        leaks may result if it was previously initialized).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
*
*
*   wcsfree() frees memory allocated for the wcsprm arrays by wcsini() and/or
*   wcsset().  wcsini() records the memory it allocates and wcsfree() will
*   only attempt to free this.
*
*   Returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Print routine for the wcsprm struct; wcsprt()
*   ---------------------------------------------
*   This service routine may be used to print the members of a wcsprm struct.
*
*   Given:
*      wcs      const struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*
*
*   Initialization routine; wcsset()
*   --------------------------------
*   Initializes a wcsprm data structure according to information supplied
*   within it (see "Coordinate transformation parameters" below).
*
*   wcsset() recognizes the NCP projection and converts it to the equivalent
*   SIN projection.  It also recognizes GLS as a synonym for SFL.
*
*   Note that this routine need not be called directly; it will be invoked by
*   wcsp2s() and wcss2p() if the "flag" structure member is anything other
*   than a predefined magic value.
*
*   Given and returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
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
*   Transform an array of pixel coordinates to world coordinates.
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing wcs.naxis coordinate elements.
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
*                        Error status for each coordinate:
*                           0: Success.
*                           1: Invalid pixel coordinate.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
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
*   Transform an array of world coordinates to pixel coordinates.
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Coordinate transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing wcs.naxis coordinate elements.
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
*      stat     int[ncoord]
*                        Error status for each coordinate:
*                           0: Success.
*                           1: Invalid world coordinate.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
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
*   Given either the celestial longitude or latitude plus an element of the
*   pixel coordinate solve for the remaining elements by iterating on the
*   unknown celestial coordinate element using wcss2p().
*
*   Given or returned:
*      wcs      struct wcsprm*
*                        Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Given:
*      mixpix   int      Which element of the pixel coordinate is given.
*      mixcel   int      Which element of the celestial coordinate is
*                        given:
*                           1: Celestial longitude is given in
*                              world[wcs.lng], latitude returned in
*                              world[wcs.lat].
*                           2: Celestial latitude is given in
*                              world[wcs.lat], longitude returned in
*                              world[wcs.lng].
*      vspan    const double[2]
*                        Solution interval for the celestial coordinate, in
*                        degrees.  The ordering of the two limits is
*                        irrelevant.  Longitude ranges may be specified with
*                        any convenient normalization, for example [-120,+120]
*                        is the same as [240,480], except that the solution
*                        will be returned with the same normalization, i.e.
*                        lie within the interval specified.
*      vstep    const double
*                        Step size for solution search, in degrees.  If zero,
*                        a sensible, although perhaps non-optimal default will
*                        be used.
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
*               int      Error status
*                           0: Success.
*                           1: Null wcsprm pointer passed.
*                           2: Memory allocation error.
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
*
*   Coordinate transformation parameters
*   ------------------------------------
*   The wcsprm struct consists of a number of data members that must be
*   supplied and others that are computed from them.  In practice, it is
*   expected that a FITS header parser would scan the FITS header until it
*   encountered the NAXIS (or WCSAXES) card which must occur near the start of
*   the header before any of the WCS keywords.  It would then use wcsini() to
*   allocate memory for arrays in the wcsprm struct and set default values.
*   Then as it read and identified each WCS header card it would load the
*   value into the relevant wcsprm array element.  Finally it would invoke
*   wcsset(), either directly or indirectly, to initialize the derived
*   members of the wcsprm struct.
*
*      int flag
*         This flag must be set to zero whenever any of the following members
*         of the wcsprm struct are set or modified.  This signals the
*         initialization routine, wcsset(), to recompute intermediaries.
*
*         flag should be set to -1 when wcsini() is called for the first time
*         for a wcsprm struct in order to initialize memory management.  It
*         must ONLY be used on the first initialization otherwise memory leaks
*         may result.
*
*      int naxis
*         Number of pixel and world coordinate elements, given by NAXIS or
*         WCSAXES.
*
*      double *crpix
*         Pointer to the first element of an array of double containing the
*         coordinate reference pixel, CRPIXj.
*
*      double *pc
*         Pointer to the first element of the PCi_j (pixel coordinate)
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
*         for which the storage order is
*
*            PC1_1, PC1_2, PC2_1, PC2_2
*
*         so it would be legitimate to set pc = *m.
*
*      double *cdelt
*         Pointer to the first element of an array of double containing the
*         coordinate increments, CDELTi.
*
*      char (*ctype)[72]
*         Pointer to the first element of an array of char[72] containing the
*         coordinate axis types, CTYPEi.
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
*      char (*cunit)[72]
*         Pointer to the first element of an array of char[72] containing the
*         CUNITi cards which define the units of measurement of the CRVALi,
*         CDELTi, and CDi_j cards.
*
*         For celestial coordinates, CRVALi, CDELTi, and CDi_j are measured in
*         degrees and CUNITi is not applicable.
*
*         For spectral coordinates, CUNITi is currently ignored and default
*         (SI) units are assumed.
*
*         For simple linear coordinate types CUNITi may be used to label
*         coordinate values.
*
*         These variables accomodate the longest allowed string-valued FITS
*         keyword, being limited to 68 characters, plus the null-terminating
*         character.
*
*      double *crval
*         Pointer to the first element of an array of double containing the
*         coordinate reference values, CRVALi.
*
*      double lonpole, latpole
*         The native longitude and latitude of the celestial pole, LONPOLE and
*         LATPOLE (degrees).
*
*      double restfrq, restwav
*         Rest frequency (Hz) and/or wavelength (m).
*
*      int npv
*      struct pvcard *pv
*         Pointer to the first element of an array of length npv of pvcard
*         structs.  As a FITS header parser encounters each PVi_m card it may
*         load it into a pvcard struct in the array:
*
*            struct pvcard {
*               int    i;		...Axis number, as in PVi_m,
*                                          (i.e. 1-relative).
*               int    m;		...Parameter number, as in PVi_m,
*                                          (i.e. 0-relative).
*               double value;		...Parameter value.
*            };
*
*         wcsset() interprets these as required.
*
*      int altlin
*      double *cd
*      double *crota
*         For historical compatibility, the wcsprm struct supports two
*         alternate specifications of the linear transformation matrix, those
*         associated with the CDi_j and CROTAi cards.  Although these may not
*         formally co-exist with PCi_j, the approach here is simply to ignore
*         them if given in conjunction with PCi_j.
*
*         altlin is a bit flag which denotes which of these cards are present
*         in the header:
*
*         Bit 0: PCi_j is present.
*
*         Bit 1: CDi_j is present.  Matrix elements in the IRAF convention are
*                equivalent to the product CDi_j = CDELTi * PCi_j, but the
*                defaults differ from that of the PCi_j matrix.  If one or
*                more CDi_j cards are present then all unspecified CDi_j
*                default to zero.  If no CDi_j (or CROTAi) cards are present,
*                then the header is assumed to be in PCi_j form whether or not
*                any PCi_j cards are present since this results in an
*                interpretation of CDELTi consistent with the original FITS
*                specification.
*
*                While CDi_j may not formally co-exist with PCi_j, it may
*                co-exist with CDELTi and CROTAi which are to be ignored.
*
*         Bit 2: CROTAi is present.  In the AIPS convention, CROTAi may only
*                be associated with the latitude axis of a celestial axis
*                pair.  It specifies a rotation in the image plane that is
*                applied AFTER the CDELTi; any other CROTAi cards are ignored.
*
*                CROTAi may not formally co-exist with PCi_j.  CROTAi and
*                CDELTi may formally co-exist with CDi_j but if so are to be
*                ignored.
*
*         CDi_j and CROTAi cards, if found, are to be stored in the cd and
*         crota arrays which are dimensioned similarly to pc and cdelt.  FITS
*         header parsers should use the following precedure:
*            Whenever a PCi_j  card is encountered: altlin &= 1;
*            Whenever a CDi_j  card is encountered: altlin &= 2;
*            Whenever a CROTAi card is encountered: altlin &= 4;
*         If none of these bits are set the PCi_j representation results, i.e.
*         pc and cdelt will be used as given.
*
*         These alternate specifications of the linear transformation matrix
*         are translated immediately to PCi_j by wcsset() and are nowhere
*         visible to the lower-level routines.  In particular, wcsset() resets
*         cdelt to unity if CDi_j is present (and no PCi_j).  If no CROTAi is
*         associated with the latitude axis, wcsset() reverts to a unity PCi_j
*         matrix.
*
*   The remaining members of the wcsprm struct are maintained by wcsset() and
*   must not be modified elsewhere:
*
*      char lngtyp[8], lattyp[8]
*         Four-character WCS celestial axis types. e.g. RA, DEC, GLON, GLAT,
*         etc.  (Declared as char[8] for alignment reasons.)
*
*      int lng, lat, spc
*         Indices into the imgcrd[][], and world[][] arrays as described
*         above. These may also serve as indices into the pixcrd[][] array
*         provided that the PCi_j matrix does not transpose axes.
*
*      int cubeface
*         Index into the pixcrd[][] array for the CUBEFACE axis.  This is used
*         for quadcube projections where the cube faces are stored on a
*         separate axis (see Note 1 above).
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
*      int m_flag, m_naxis
*      char (*m_ctype)[72], (*m_cunit)[72]
*      double *m_crpix, *m_pc, *m_cdelt, *m_crval
*      struct pvcard *m_pv
*      double *m_cd, *m_crota
*         These are used for memory management by wcsini() and wcsfree().
*
*
*   Vector arguments
*   ----------------
*   Arrays of pixel and world coordinates are two dimensional, i.e.
*   pixcrd[ncoord][nelem], where nelem must equal or exceed the number of
*   coordinate elements, naxis, stored in the wcsprm struct.  Exception: when
*   ncoord == 1, nelem is not used.
*
*   Note that the function prototypes must declare two-dimensional arrays as
*   one-dimensional to avoid warnings about declaration of "incomplete types".
*   This was considered preferable to declaring them as simple
*   pointers-to-double which gives no indication that storage is associated
*   with them.
*
*
*   Memory allocation and deallocation
*   ----------------------------------
*   wcsini() allocates memory for the crpix, pc, and cdelt arrays in the
*   wcsprm struct.  It is provided as a service routine; usage is optional,
*   and the caller is at liberty to set these pointers independently.
*
*   If the pc matrix is not unity, wcsset() also allocates memory for the
*   piximg and imgpix arrays.  The caller must not modify these.
*
*   wcsini() maintains a record of memory it has allocated and this is used
*   by wcsfree() which wcsini() uses to free any memory that it may have
*   allocated on a previous invokation.  Thus it is not necessary for the
*   caller to invoke wcsfree() separately if wcsini() is invoked repeatedly on
*   the same wcsprm struct.  Likewise, wcsset() deallocates memory that it
*   may have allocated in the same wcsprm struct on a previous invokation.
*
*   However, a memory leak will result if a wcsprm struct goes out of scope
*   before the memory has been free'd, either by wcsfree() or otherwise.
*   Likewise, if the wcsprm struct itself has been malloc'd and the allocated
*   memory is not free'd when the memory for the struct is free'd.  A leak may
*   also arise if the caller interferes with the array pointers in the
*   "private" part of the wcsprm struct.
*
*   Beware of making a shallow copy of a wcsprm struct by assignment; any
*   changes made to allocated memory in one would be reflected in the other,
*   and if the memory allocated for one was free'd the other would reference
*   unallocated memory.  Use wcscopy() instead to make a deep copy.
*
*
*   Error codes
*   -----------
*   Error messages to match the error codes returned from each function are
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
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*===========================================================================*/

#ifndef WCSLIB_WCS
#define WCSLIB_WCS

#include "lin.h"
#include "cel.h"
#include "spc.h"

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif


extern const char *wcs_errmsg[];
#define wcsini_errmsg wcs_errmsg
#define wcscopy_errmsg wcs_errmsg
#define wcsfree_errmsg wcs_errmsg
#define wcsprt_errmsg wcs_errmsg
#define wcsset_errmsg wcs_errmsg
#define wcsp2s_errmsg wcs_errmsg
#define wcss2p_errmsg wcs_errmsg
#define wcsmix_errmsg wcs_errmsg

#define NPVMAX 64		/* Maximum number of PVi_m cards.           */

                                /* Struct used for storing PVi_m cards.     */
struct pvcard {
   int    i;			/* Axis number, as in PVi_m (1-relative).   */
   int    m;			/* Parameter number, ditto  (0-relative).   */
   double value;		/* Parameter value.                         */
};


struct wcsprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* FITS header cards to be provided (see the prologue above).            */
   /*-----------------------------------------------------------------------*/
   int    naxis;		/* Number of axes (pixel and coordinate).   */
   double *crpix;		/* CRPIXj cards for each pixel axis.        */
   double *pc;			/* PCi_j  linear transformation matrix.     */
   double *cdelt;		/* CDELTi cards for each coordinate axis.   */

   char   (*ctype)[72];		/* CTYPEi cards for each coordinate axis.   */
   char   (*cunit)[72];		/* CUNITi cards for each coordinate axis.   */

   double *crval;		/* CRVALi cards for each coordinate axis.   */
   double lonpole;		/* LONPOLE card.                            */
   double latpole;		/* LATPOLE card.                            */

   double restfrq;		/* RESTFRQ card.                            */
   double restwav;		/* RESTWAV card.                            */

   int    npv;			/* Total number of PVi_m cards.             */
   struct pvcard *pv;		/* PVi_m cards for each i and m.            */

   /* Alternative header cards (see the prologue above).                    */
   /*-----------------------------------------------------------------------*/
   int    altlin;		/* Alternative representations              */
				/*   Bit 0: PCi_j  is present,              */
				/*   Bit 1: CDi_j  is present,              */
				/*   Bit 2: CROTAi is present.              */
   double *cd;			/* CDi_j linear transformation matrix.      */
   double *crota;		/* CROTAi cards for each coordinate axis.   */

   /* Information derived from the FITS header cards by wcsset().           */
   /*-----------------------------------------------------------------------*/
   char   lngtyp[8], lattyp[8];	/* Celestial axis types, e.g. RA, DEC.      */
   int    lng, lat, spec;	/* Longitude, latitude, and spectral axis   */
                                /* numbers.                                 */
   int    cubeface;		/* True if there is a CUBEFACE axis.        */
   int    padding1;		/* (Dummy inserted for alignment purposes.) */

   struct linprm lin;		/* Linear    transformation parameters.     */
   struct celprm cel;		/* Celestial transformation parameters.     */
   struct spcprm spc;		/* Spectral  transformation parameters.     */

   int m_flag, m_naxis;		/* The remainder are for memory management. */
   char  (*m_ctype)[72], (*m_cunit)[72];
   double *m_crpix, *m_pc, *m_cdelt, *m_crval;
   struct pvcard *m_pv;
   double *m_cd, *m_crota;
   int    padding2;		/* (Dummy inserted for alignment purposes.) */
};

#if __STDC__ || defined(__cplusplus)
   int wcsini(int, int, struct wcsprm *);

   int wcscopy(int, const struct wcsprm *, struct wcsprm *);

   int wcsfree(struct wcsprm *);

   int wcsprt(const struct wcsprm *);

   int wcsset(struct wcsprm *);

   int wcsp2s(struct wcsprm *, int, int, const double[],
              double[], double[], double[], double[], int[]);

   int wcss2p(struct wcsprm *, int, int, const double[],
              double[], double[], double[], double[], int[]);

   int wcsmix(struct wcsprm *, int, int, const double[], double, int,
              double[], double[], double[], double[], double[]);

   int wcs_allEq(int, int, const double *);
   void wcs_setAll(int, int, double *);
   void wcs_setAli(int, int, int *);
#else
   int wcsini(), wcscopy(), wcsfree(), wcsprt(), wcsset(), wcsp2s(), wcss2p(),
       wcsmix();
   int wcs_allEq();
   void wcs_setAll(), wcs_setAli();
#endif

#define WCSLEN (sizeof(struct wcsprm)/sizeof(int))

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_WCS */
