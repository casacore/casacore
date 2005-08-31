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
*   WCSLIB 4.1 - C routines that implement tabular coordinate systems as
*   defined by the FITS World Coordinate System (WCS) standard.  Refer to
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
*   These routines implement the part of the FITS WCS standard that deals with
*   tabular coordinates, i.e. coordinates that are defined via a lookup table.
*   They define methods to be used for computing tabular world coordinates
*   from intermediate world coordinates (a linear transformation of image
*   pixel coordinates), and vice versa.  They are based on the tabprm struct,
*   described in detail below, which contains all information needed for the
*   computations.  The struct contains some members that must be set by the
*   caller, and others that are maintained by these routines, somewhat like a
*   C++ class but with no encapsulation.
*
*   tabini(), tabmem(), tabcpy(), and tabfree() are provided to manage the
*   tabprm struct, and another, tabprt(), to print its contents.
*
*   A setup routine, tabset(), computes intermediate values in the tabprm
*   struct from parameters in it that were supplied by the caller.  The
*   struct always needs to be set up by tabset() but it need not be called
*   explicitly - see the explanation of tab.flag below.
*
*   tabx2s() and tabs2x() implement the WCS tabular coordinate
*   transformations.
*
*
*   Default constructor for the tabprm struct; tabini()
*   ---------------------------------------------------
*   tabini() allocates memory for arrays in a tabprm struct and sets all
*   members of the struct to default values.
*
*   N.B. every tabprm struct should be initialized by tabini(), possibly
*   repeatedly.  On the first invokation, and only the first invokation, the
*   flag member of the tabprm struct must be set to -1 to initialize memory
*   management, regardless of whether tabini() will actually be used to
*   allocate memory.
*
*   Given:
*      alloc    int      If true, allocate memory unconditionally for arrays
*                        in the tabprm struct (see "Memory allocation and
*                        deallocation below").
*
*                        If false, it is assumed that pointers to these arrays
*                        have been set by the caller except if they are null
*                        pointers in which case memory will be allocated for
*                        them regardless.  (In other words, setting alloc true
*                        saves having to initalize these pointers to zero.)
*
*      M        int      The number of tabular coordinate axes.
*      K        const int[]
*                        Vector of length M whose elements (K_1, K_2,... K_M)
*                        record the lengths of the axes of the coordinate
*                        array and of each indexing vector.  M and K[] are
*                        used to determine the length of the various tabprm
*                        arrays and therefore the amount of memory to allocate
*                        for them.  Their values are copied into the tabprm
*                        struct.
*
*                        It is permissible to set K (i.e. the address of the
*                        array) to zero which has the same effect as setting
*                        each element of K[] to zero.  In this case no memory
*                        will be allocated for the index vectors or coordinate
*                        array in the tabprm struct.  These together with the
*                        K vector must be set separately before calling
*                        tabset().
*
*   Given and returned:
*      tab      struct tabprm*
*                        Tabular transformation parameters (see below).
*                        Note that, in order to initialize memory management
*                        tab->flag should be set to -1 when tab is initialized
*                        for the first time (memory leaks may result if it had
*                        already been initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*                           2: Memory allocation failed.
*                           3: Invalid tabular parameters.
*
*
*   Acquire tabular memory; tabmem()
*   --------------------------------
*   tabmem() takes control of memory allocated by the user for arrays in the
*   tabprm struct.
*
*   Given and returned:
*      tab      struct tabprm*
*                        Tabular transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*
*
*   Copy routine for the tabprm struct; tabcpy()
*   --------------------------------------------
*   tabcpy() does a deep copy of one tabprm struct to another, using tabini()
*   to allocate memory for its arrays if required.  Only the "information to
*   be provided" part of the struct is copied; a call to tabset() is required
*   to set up the remainder.
*
*   Given:
*      alloc    int      If true, allocate memory unconditionally for arrays
*                        in the tabprm struct (see "Memory allocation and
*                        deallocation below").
*
*                        If false, it is assumed that pointers to these arrays
*                        have been set by the caller except if they are null
*                        pointers in which case memory will be allocated for
*                        them regardless.  (In other words, setting alloc true
*                        saves having to initalize these pointers to zero.)
*
*      tabsrc   const struct tabprm*
*                        Struct to copy from.
*
*   Given and returned:
*      tabdst   struct tabprm*
*                        Struct to copy to.  tabdst->flag should be set to -1
*                        if tabdst was not previously initialized (memory
*                        leaks may result if it was previously initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*                           2: Memory allocation failed.
*
*
*   Destructor for the tabprm struct; tabfree()
*   -------------------------------------------
*   tabfree() frees memory allocated for the tabprm arrays by tabini().
*   tabini() records the memory it allocates and tabfree() will only attempt
*   to free this.
*
*   N.B. tabfree() must not be invoked on a tabprm struct that was not
*   initialized by tabini().
*
*   Returned:
*      tab      struct tabprm*
*                        Coordinate transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*
*
*   Print routine for the tabprm struct; tabprt()
*   ---------------------------------------------
*   tabprt() prints the contents of a tabprm struct.
*
*   Given:
*      tab      const struct tabprm*
*                        Tabular transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*
*
*   Set up routine; tabset()
*   ------------------------
*   tabset() allocates memory for work arrays in the tabprm struct and sets up
*   the struct according to information supplied within it (see "Tabular
*   transformation parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   tabx2s() and tabs2x() if the "flag" struct member is anything other than a
*   predefined magic value.
*
*   Given and returned:
*      tab      struct tabprm*
*                        Tabular transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*                           3: Invalid tabular parameters.
*
*
*   Pixel-to-world transformation; tabx2s()
*   ---------------------------------------
*   tabx2s() transforms intermediate world coordinates to world coordinates
*   using coordinate lookup.
*
*   Given and returned:
*      tab      struct tabprm*
*                        Tabular transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem.
*      x        const double[ncoord][nelem]
*                        Array of intermediate world coordinates, SI units.
*
*   Returned:
*      world    double[ncoord][nelem]
*                        Array of world coordinates, in SI units.
*      stat     int[ncoord]
*                        Status return value status for each coordinate:
*                           0: Success.
*                           1: Invalid intermediate world coordinate.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*                           3: Invalid tabular parameters.
*                           4: One or more of the x coordinates were invalid,
*                              as indicated by the stat vector.
*
*
*   World-to-pixel transformation; tabs2x()
*   ---------------------------------------
*   tabs2x() transforms world coordinates to intermediate world coordinates.
*
*   Given and returned:
*      tab      struct tabprm*
*                        Tabular transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem.
*      world    const double[ncoord][nelem]
*                        Array of world coordinates, in SI units.
*
*   Returned:
*      x        double[ncoord][nelem]
*                        Array of intermediate world coordinates, SI units.
*      stat     int[ncoord]
*                        Status return value status for each vector element:
*                           0: Success.
*                           1: Invalid world coordinate.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null tabprm pointer passed.
*                           3: Invalid tabular parameters.
*                           5: One or more of the world coordinates were
*                              invalid, as indicated by the stat vector.
*
*
*   Tabular transformation parameters
*   ----------------------------------
*   The tabprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following tabprm
*         structure members are set or changed.  This signals the
*         initialization routine, tabset(), to recompute intermediaries.
*
*         flag should be set to -1 when tabini() is called for the first time
*         for a tabprm struct in order to initialize memory management.  It
*         must ONLY be used on the first initialization otherwise memory leaks
*         may result.
*
*      int M
*         Number of tabular coordinate axes.
*
*      int *K
*         Pointer to the first element of a vector of length M whose elements
*         (K_1, K_2,... K_M) record the lengths of the axes of the coordinate
*         array and of each indexing vector.
*
*      int *map
*         Pointer to the first element of a vector of length M that defines
*         the association between axis m in the M-dimensional coordinate array
*         (1 <= m <= M) and the indices of the intermediate world coordinate
*         and world coordinate arrays, x[] and world[], in the argument lists
*         for tabx2s() and tabs2x().
*
*         When x[] and world[] contain the full complement of coordinate
*         elements in image-order, as will usually be the case, then
*         map[m-1] == i-1 for axis i in the N-dimensional image (1 <= i <= N).
*         In terms of the FITS keywords
*
*            map[PVi_3a - 1] == i - 1.
*
*         However, a different association may result if x[], for example,
*         only contains a (relevant) subset of intermediate world coordinate
*         elements.  For example, if M == 1 for an image with N > 1, it is
*         possible to fill x[] with the relevant coordinate element with nelem
*         set to 1.  In this case map[0] = 0 regardless of the value of i.
*
*      double *crval
*         Pointer to the first element of a vector of length M whose elements
*         contain the index value for the reference pixel for each of the
*         tabular coordinate axes.
*
*      double **index
*         Pointer to the first element of a vector of length M of pointers to
*         vectors of lengths (K_1, K_2,... K_M) of 0-relative indexes.
*
*         The address of any or all of these index vectors may be set to zero,
*         i.e. index[m] == 0; this is interpreted as default indexing -
*         index[m][k] = k.
*
*      double *coord
*         Pointer to the first element of the tabular coordinate array,
*         treated as though it were defined as
*
*            double coord[K_M]...[K_2][K_1][M]
*
*         i.e. with the M dimension varying fastest so that the M elements of
*         a coordinate vector are stored contiguously in memory.
*
*   The remaining members of the tabprm struct are maintained by tabset() and
*   must not be modified elsewhere:
*
*      int nc
*         Total number of coordinate vectors in the coordinate array being the
*         product of K_1 * K_2 * ... * K_M.
*
*      int *sense
*         Pointer to the first element of a vector of length M whose elements
*         indicate whether the corresponding indexing vector is monotonic
*         increasing (+1), or decreasing (-1).
*
*      double *p0, *delta
*         Pointer to the first element of a vector of length M of interpolated
*         indices into the coordinate array such that Upsilon[m], as defined
*         in Paper III, is equal to p0[m] + delta[m].
*
*      double *extrema
*         Pointer to the first element of an array that records the minimum
*         and maximum value of each element of the coordinate vector in each
*         row of the coordinate array, treated as though it were defined as
*
*            double extrema[K_M]...[K_2][2][M]
*
*         The minimum is recorded in the first element of the compressed K_1
*         dimension, then the maximum.  This array is used by the inverse
*         table lookup function, tabs2x(), to speed up table searches.
*
*      The remaining elements of the tabprm struct are used for memory
*      management by tabini(), tabmem(), and tabfree().
*
*
*   Vector arguments
*   ----------------
*   Arrays of intermediate world coordinates and world coordinates are two
*   dimensional, i.e. x[ncoord][nelem].
*
*   Note that the function prototypes must declare two-dimensional arrays as
*   one-dimensional to avoid compiler warnings about declaration of
*   "incomplete types".  This was considered preferable to declaring them as
*   simple pointers-to-double which gives no indication that storage is
*   associated with them.
*
*
*   Memory allocation and deallocation
*   ----------------------------------
*   tabini() optionally allocates memory for the K, map, crval, index, and
*   coord arrays (including the arrays referenced by index[]) in the tabprm
*   struct as described in the usage notes above.  tabmem() takes control of
*   any of these arrays that may have been allocated by the user, specifically
*   in that tabfree() will free it.
*
*   tabset() also allocates memory for the sense, p0, delta and extrema
*   arrays.  The caller must not modify these.
*
*   tabini() maintains a record of memory it has allocated and this is used
*   by tabfree() which tabini() uses to free any memory that it may have
*   allocated on a previous invokation.  Thus it is not necessary for the
*   caller to invoke tabfree() separately if tabini() is invoked repeatedly on
*   the same tabprm struct.  Likewise, tabset() deallocates memory that it
*   may have allocated in the same tabprm struct on a previous invokation.
*
*   A memory leak will result if a tabprm struct goes out of scope before the
*   memory has been free'd, either by tabfree() or otherwise.  Likewise, if
*   the tabprm struct itself has been malloc'd and the allocated memory is not
*   free'd when the memory for the struct is free'd.  A leak may also arise if
*   the caller interferes with the array pointers in the "private" part of the
*   tabprm struct.
*
*   Beware of making a shallow copy of a tabprm struct by assignment; any
*   changes made to allocated memory in one would be reflected in the other,
*   and if the memory allocated for one was free'd the other would reference
*   unallocated memory.  Use tabcpy() instead to make a deep copy.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   are encoded in the tab_errmsg character array.
*
*
*   Accuracy
*   --------
*   No warranty is given for the accuracy of these routines (refer to the
*   copyright notice above); intending users must satisfy for themselves their
*   adequacy for the intended purpose.  However, closure effectively to within
*   double precision rounding error was demonstrated by test routine ttab.c
*   which accompanies this software.
*
*===========================================================================*/

#ifndef WCSLIB_TAB
#define WCSLIB_TAB

#ifdef __cplusplus
extern "C" {
#endif


extern const char *tab_errmsg[];
#define tabini_errmsg tab_errmsg
#define tabcpy_errmsg tab_errmsg
#define tabfree_errmsg tab_errmsg
#define tabprt_errmsg tab_errmsg
#define tabset_errmsg tab_errmsg
#define tabx2s_errmsg tab_errmsg
#define tabs2x_errmsg tab_errmsg

struct tabprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int    flag;			/* Set to zero to force initialization.     */

   /* Parameters to be provided (see the prologue above).                   */
   /*-----------------------------------------------------------------------*/
   int    M;			/* Number of tabular coordinate axes.       */
   int    *K;			/* Vector of length M whose elements        */
				/* (K_1, K_2,... K_M) record the lengths of */
				/* the axes of the coordinate array and of  */
				/* each indexing vector.                    */
   int    *map;			/* Vector of length M usually such that     */
				/* map[m-1] == i-1 for coordinate array     */
				/* axis m and image axis i (see above).     */
   double *crval;		/* Vector of length M containing the index  */
				/* value for the reference pixel for each   */
				/* of the tabular coordinate axes.          */
   double **index;		/* Vector of pointers to M indexing vectors */
				/* of lengths (K_1, K_2,... K_M).           */
   double *coord;		/* (1+M)-dimensional tabular coordinate     */
				/* array (see above).                       */

   /* Information derived from the parameters supplied.                     */
   /*-----------------------------------------------------------------------*/
   int    nc;			/* Number of coordinate vectors (of length  */
				/* M) in the coordinate array.              */
   int    *sense;		/* Vector of M flags that indicate whether  */
				/* the Mth indexing vector is monotonic     */
				/* increasing, or else decreasing.          */
   int    *p0;			/* Vector of M indices.                     */
   double *delta;		/* Vector of M increments.                  */
   double *extrema;		/* (1+M)-dimensional array of coordinate    */
				/* extrema.                                 */

   int    m_flag, m_M, m_N;	/* The remainder are for memory management. */
   int    *m_K, *m_map;
   double *m_crval, **m_index, **m_indxs, *m_coord;
   int    set_M;
};

#define TABLEN (sizeof(struct tabprm)/sizeof(int))


int tabini(int, int, const int[], struct tabprm *);
int tabmem(struct tabprm *);
int tabcpy(int, const struct tabprm *, struct tabprm *);
int tabfree(struct tabprm *);
int tabprt(const struct tabprm *);
int tabset(struct tabprm *);
int tabx2s(struct tabprm *, int, int, const double[], double[], int[]);
int tabs2x(struct tabprm *, int, int, const double[], double[], int[]);


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_TAB */
