*=======================================================================
*
*   PGCRVL draws and labels a curvilinear coordinate grid.
*
*   Given:
*      AXEN     I(2)     Number of pixels on each axis of the image.
*                        Pixel coordinate (1,1) corresponds to the
*                        centre of the pixel in the bottom left-hand
*                        corner (BLC).  Thus pixel coordinates of the
*                        BLC of the frame are (0.5,0.5) and the TRC
*                        frame pixel coordinates are
*                        (AXEN(1)+0.5,AXEN(2)+0.5).
*
*      IDENTS   C(3)*(*) Identification strings:
*                           1: Name of the first world coordinate
*                              element used for axis labelling.
*                           2: Name of the second world coordinate.
*                           3: Title, written at top.
*
*      OPT      C(2)*(*) Formatting control for the world coordinates,
*                        used for axis labelling (see notes 1 and 2):
*                           ' ': plain numeric
*                           'A': angle in degrees expressed in
*                                decimal notation normalized in the
*                                range [0,360).
*                           'B': as 'A' but normalized in the range
*                                (-180,180].
*                           'C': as 'A' but unnormalized.
*                           'D': angle in degrees expressed in
*                                sexagesimal notation, DD^MM'SS".S,
*                                normalized in the range [0,360).
*                           'E': as 'D' but normalized in the range
*                                (-180,180].
*                           'F': as 'D' but unnormalized.
*                           'G': angle in degrees expressed in
*                                hms notation, HHhMMmSSs.S, normalized
*                                in the range [0,24) hours.
*                           'H': as 'G' but normalized in the range
*                                (-12,12] hours.
*                           'I': as 'G' but unnormalized.
*                           'L': logarithmic (see note 2)
*                           'T': time in hours expressed as HH:MM:SS.S
*
*                        For the angular types, NLFUNC is assumed to
*                        return the angle in degrees whereupon it will
*                        be formatted for display in the specified way.
*                        For example, an angle of -417.2958 degrees
*                        (returned by NLFUNC):
*
*                            'A':  302^.7042
*                            'B':  -57^.2958
*                            'C': -417^.2958
*                            'D':  302^42'15"
*                            'E':  -57^17'45"
*                            'F': -417^17'45"
*                            'H':   20h10m49s
*                            'I':   -3h49m11s
*                            'J':  -27h49m11s
*
*                        These are properly superscripted.
*
*      LABCTL   I        Decimal encoded grid labelling control:
*                             -1: Accumulate information on grid labels
*                                 (see note 3) but do not write them.
*                              0: Let PGCRVL decide what edges to label.
*                              1: Label bottom of frame with the first
*                                 world coordinate.
*                              2: Label bottom of frame with the second
*                                 world coordinate.
*                             10: ... left side of frame.
*                             20: ... left side of frame.
*                            100: ... top of frame.
*                            200: ... top of frame.
*                           1000: ... right side of frame.
*                           2000: ... right side of frame.
*                          10000: Write labels from information
*                                 accumulated in previous calls without
*                                 drawing grid lines or tick marks.
*
*                        LABCTL = 0 usually gets what you want.
*                        LABCTL = 3333 labels all sides with both world
*                        coordinates.
*
*      LABDEN   I        Decimal encoded labelling density control for
*                        use where PGCRVL is called upon to determine a
*                        suitable grid spacing (e.g. via NG1 = 0,
*                        GRID1(0) = 0).  LABDEN = 100*D2 + D1 where
*                        D1, and D2 are the approximate number of grid
*                        lines for the first and second world
*                        coordinate.  LABDEN = 0 is effectively the same
*                        as LABDEN = 808.
*
*      CI       I(7)     Table of predefined colours established by
*                        calls to PGSCR.  This is used to control the
*                        colour used for different parts of the plot.
*                        CI table entries are used as follows:
*
*                                                 world
*                                               coordinate
*                        Index      usage        element
*                        -----  --------------  ----------
*                          1      grid lines        1
*                          2      grid lines        2
*                          3    numeric labels      1
*                          4    numeric labels      2
*                          5    axis annotation     1
*                          6    axis annotation     2
*                          7        title           -
*
*                        For example, CI(3) is used for numeric labels
*                        for the first world coordinate.
*
*                        Colour selection is disabled for component J
*                        if CI(J) < 0.
*
*      GCODE(2) I        Code for the type of grid to draw for each
*                        world coordinate:
*                           0: No grid or tick marks.
*                           1: Tick marks (on all edges).
*                           2: Full coordinate grid.
*
*                        Tick marks can be restricted to particular
*                        edges of the frame; a negative GCODE is
*                        interpreted as a decimal encoded control
*                        variable:
*                             -1: bottom
*                            -10: left
*                           -100: top
*                          -1000: right
*
*                        The digit scales the basic tick length.  For
*                        example, GCODE(1) = -102 restricts tick marks
*                        for the first world coordinate to the bottom
*                        and top edges of the frame.  Those on the
*                        bottom will be twice the length specified by
*                        TIKLEN.
*
*      TIKLEN   D        Tick length, in mm.  Negative values produce
*                        outside tick marks.
*
*      NG1      I        Upper array index for GRID1.
*
*      GRID1    D(0:NG1) Grid values in WORLD(1) in the same units as
*                        returned by NLFUNC.
*
*                        If NG1 is zero, then
*                           a) if GRID1(0) is non-zero it defines a
*                              uniform grid spacing.
*                           b) if GRID1(0) is zero a suitable spacing
*                              will be determined (see LABDEN).
*
*                        If NG1 is greater than zero, then GRID1(0) is
*                        ignored.
*
*      NG2      I        Upper array index for GRID2.
*
*      GRID2    D(0:NG2) Grid values in WORLD(2) in the same units as
*                        returned by NLFUNC.
*
*      DOEQ     L        If NG1 = NG2 = 0, and GRID1(0) = 0D0 and/or
*                        GRID2(0) = 0D0, then chose the same grid
*                        spacing for each world coordinate.
*
*      NLFUNC   Ext      Non-linear coordinate function, see below.
*
*      NLC      I        Number of elements in NLCPRM (must be >0).
*
*      NLI      I        Number of elements in NLIPRM (must be >0).
*
*      NLD      I        Number of elements in NLDPRM (must be >0).
*
*   Given and/or returned:
*      NLCPRM   C(NLC)*1 Character coefficients for NLFUNC.
*
*      NLIPRM   I(NLI)   Integer coefficients for NLFUNC.
*
*      NLDPRM   D(NLD)   Double precision coefficients for NLFUNC.
*
*      NC       I        Upper array index for CACHE.
*
*      IC       I        Current number of entries in the CACHE table.
*                        Should be set to -1 on the first call to
*                        PGCRVL (see note 3).
*
*      CACHE    D(4,0:NC)
*                        Table of points where the tick marks or grid
*                        lines cross the frame (see note 3).
*                           1: Frame segment
*                              1: bottom
*                              2: left
*                              3: top
*                              4: right
*                           2: X or Y-pixel coordinate.
*                           3: World coordinate element (1 or 2).
*                           4: Value.
*
*                        CACHE(,0) is used to cache the extrema of the
*                        coordinate values between calls.
*
*   Returned:
*      IERR     I        Error status
*                           0: Success
*                           1: Initialization error
*
*   Notes on PGCRVL
*   ---------------
*
*    1) Where a logarithmic world coordinate type is indicated PGCRVL
*       chooses grid lines and labels on the basis that the value
*       returned by NLFUNC is a base 10 logarithm.  PGCRVL does not
*       itself take logarithms or antilogarithms.  For example, if the
*       range of values returned by NLFUNC were 0.9 - 2.5, then PGCRVL
*       would draw a subset of the following set of grid lines and
*       labels:
*
*                   value          label
*            ------------------    -----
*            0.9031 = log10(8)      8
*            0.9542 = log10(9)      9
*            1.0000 = log10(10)    10**1
*            1.3010 = log10(20)     2
*            1.4771 = log10(30)     3
*            1.6021 = log10(40)     4
*            1.6990 = log10(50)     5
*            1.7782 = log10(60)     6
*            1.8451 = log10(70)     7
*            1.9031 = log10(80)     8
*            1.9542 = log10(90)     9
*            2.0000 = log10(100)   10**2
*            2.3010 = log10(200)    2
*            2.4771 = log10(300)    3
*
*       The subset chosen depends on the coordinate increment as
*       specified by the caller (e.g. via NG1 = 0, GRID1(0) != 0) or as
*       deduced by PGCRVL from the required density of grid lines
*       specified in the LABDEN argument.  The selection is made
*       according to the following table:
*
*            increment          grid lines
*           (0.00,0.12]   1, 2, 3, 4, 5, 6, 7, 8, 9
*           (0.12,0.18]   1, 2, 3, 4, 5,    7
*           (0.18,0.23]   1, 2, 3,    5,    7
*           (0.23,0.28]   1, 2, 3,    5
*           (0.28,0.40]   1, 2,       5
*           (0.40,0.70]   1, 3
*           (0.70,1.00]   1
*
*       For increments greater than 1 the nearest integer is used.
*
*    2) PGCRVL will attempt to handle discontinuities in angle, such as
*       may occur when cycling through 360 degrees, wherever the
*       discontinuity may occur, for example
*
*          359 -> 0 -> 1
*
*       or
*
*          179 -> 180 -> -179
*
*       Only single cycles are detected, so the sequence
*
*          -360 -> 0 -> 359 -> 0 -> 359
*
*       would not be handled properly.  In such cases NLFUNC should be
*       changed to return a normalized angle, or else a continuous
*       sequence.
*
*    3) PGCRVL maintains a table of axis crossings, CACHE, in which it
*       stores information used for axis labelling.  The caller need not
*       normally be concerned about the use of this table other than to
*       provide sufficient space.
*
*       However, a coordinate grid may be produced via multiple calls to
*       PGCRVL with deferment of axis labelling.  This might be done to
*       change the pen colour and/or thickness for different sets of
*       grid lines.  The table accumulates information from each
*       successive call until the labels are produced.  The table index,
*       IC, may be reset to zero by the caller to discard the
*       information collected.
*
*       The extrema of the world coordinate elements are stored in
*       CACHE(,0).  When a coordinate grid is plotted with multiple calls
*       to PGCRVL the initial call should always have IC set to -1 to
*       signal that PGCRVL needs to determine the extrema.  On
*       subsequent calls with IC non-negative PGCRVL uses the extrema
*       cached from the first call.  This can speed up execution
*       considerably.
*
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
*   The curvilinear coordinate grid is defined by external function
*   NLFUNC whose interface is defined as follows:
*
*   Given:
*      OPCODE   I        Transformation code:
*                           +2: Compute a set of pixel coordinates which
*                               describe a path between this and the
*                               previous pair of world coordinates
*                               remembered from the last call with
*                               OPCODE = +1 or +2.  Usually only takes
*                               a single step unless traversing a
*                               discontinuity or some other
*                               irregularity (see explanation below).
*                           +1: Compute pixel coordinates from world
*                               coordinates.
*                            0: Initialize.
*                           -1: Compute world coordinates from pixel
*                               coordinates.
*
*      NLC      I        Number of elements in NLCPRM (must be >0).
*
*      NLI      I        Number of elements in NLIPRM (must be >0).
*
*      NLD      I        Number of elements in NLDPRM (must be >0).
*
*   Given and/or returned:
*      NLCPRM   C(NLC)*1 Character array.
*
*      NLIPRM   I(NLI)   Integer coefficients.
*
*      NLDPRM   D(NLD)   Double precision coefficients.
*
*      WORLD    D(2)     World coordinates.  Given if OPCODE > 0,
*                        returned if OPCODE < 0.
*
*      PIXEL    D(2)     Pixel coordinates.  Given if OPCODE < 0,
*                        returned if OPCODE > 0.
*
*      CONTRL   I        Control flag for OPCODE = +2:
*                           0: Normal state.
*                           1: Force PGCRVL to flush its plotting buffer
*                              and call NLFUNC again with the same world
*                              coordinates.
*                           2: Force PGCRVL to call NLFUNC again with
*                              the same world coordinates (without
*                              flushing its plotting buffer).
*
*      CONTXT   D(20)    Context elements for OPCODE = +2.
*
*   Returned:
*      IERR     I        Error status
*                           0: Success.
*                           1: Invalid parameters.
*                           2: Invalid world coordinate.
*                           3: Invalid pixel coordinate.
*
*   PGCRVL passes its NLCPRM, NLIPRM, and NLDPRM adjustable size array
*   arguments of length NLC, NLI, and NLD to NLFUNC without
*   modification.  Comments within NLFUNC should specify the parameters
*   it wants passed to it via these arrays.
*
*   PGCRVL first calls NLFUNC with OPCODE = 0 to cause it to initialize
*   its work arrays (if necessary).  It then uses OPCODE = -1 to
*   determine the range of world coordinate values.  It anchors the
*   start of each coordinate grid line with a call with OPCODE = +1, and
*   then tracks it with OPCODE = +2.
*
*   The CONTXT array is also passed to NLFUNC without modification to
*   allow it to preserve state information between calls for OPCODE = 2.
*   In particular, NLFUNC can use this to detect discontinuities in the
*   grid lines.
*
*   The CONTRL argument is provided so that NLFUNC can force PGCRVL to
*   call it again with or without flushing its plotting buffer.  This
*   may be needed when plotting a grid line through a discontinuity.
*   PGCRVL does not modify CONTRL.
*
*   Notes on NLFUNC
*   ---------------
*    1) NLFUNC must define a single-valued function, that is, each
*       pixel coordinate (i,j) must map to a unique world coordinate
*       pair (x,y).
*
*    2) Notwithstanding the fact that PGCRVL declares NLCPRM, NLIPRM,
*       and NLDPRM as single dimension arrays of length NLC, NLI, and
*       NLD, NLFUNC may treat these as higher-dimensional arrays, for
*       example, NLDPRM(2,NLD).  (The FORTRAN standard requires that
*       only the last dimension is adjustable.)
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=======================================================================
      SUBROUTINE PGCRVL (AXEN, IDENTS, OPT, LABCTL, LABDEN, CI, GCODE,
     *   TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI, NLD,
     *   NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)
*-----------------------------------------------------------------------
      INTEGER   BUFSIZ
      PARAMETER (BUFSIZ = 2048)

      LOGICAL   DOEQ, GETEND, INSIDE, ISANGL(2), MAJOR, PRVIN
      INTEGER   AXEN(2), CI(7), CI0, CJ(7), CONTRL, DENS(2), FSEG,
     *          GCODE(2), IC, IERR, IPIX, ISTEP, IWJ, IWK, J, JPIX, K,
     *          L, LABCTL, LABDEN, LDIV(2), LTABL(6,2:6), N1, N2, NC,
     *          NG(2), NG1, NG2, NLC, NLD, NLI, NLIPRM(NLI), NP, NW(2),
     *          TCODE(2,4)
      REAL      S, WPIX(4), X1, X2, XPOINT, XR(BUFSIZ), XSCL, XVP1,
     *          XVP2, Y1, Y2, YR(BUFSIZ), YSCL, YVP1, YVP2
      DOUBLE PRECISION CONTXT(20), CACHE(4,0:NC), D1, D2, DW(2), FACT,
     *          G0(2), GSTEP(2), GRID1(0:NG1), GRID2(0:NG2),
     *          NLDPRM(NLD), PIXEL(2), STEP, TIKLEN, TMP, VMAX(2,2),
     *          VMIN(2,2), WMAX(2), WMIN(2), WORLD(2)
      CHARACTER IDENTS(3)*(*), NLCPRM(NLC)*1, OPT(2)*(*), TYPE(2)

      EXTERNAL NLFUNC

*     Approximate number of grid lines for each coordinate.
      INTEGER DENS0
      PARAMETER (DENS0 = 8)

*     Number of steps per grid line.
      INTEGER NSTEP
      PARAMETER (NSTEP = 80)

*     Double precision round-off tolerance.
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 1D-8)

*     Table of logarithmic grid values.
      DATA LTABL /3, 10,  0,  0,  0,  0,
     *            2,  5, 10,  0,  0,  0,
     *            2,  3,  5, 10,  0,  0,
     *            2,  3,  5,  7, 10,  0,
     *            2,  3,  4,  5,  7, 10/
*-----------------------------------------------------------------------
*  Initialize.
      CALL NLFUNC (0, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM, WORLD,
     *   PIXEL, CONTRL, CONTXT, IERR)
*     Quick return for now.
      IF (IERR.NE.0) THEN
         IERR = 1
         RETURN
      END IF

      NG(1) = NG1
      NG(2) = NG2

      TYPE(1) = OPT(1)(1:1)
      TYPE(2) = OPT(2)(1:1)

*     Extend the PGPLOT window and rescale it to pixels.
      CALL PGQVP (0, XVP1, XVP2, YVP1, YVP2)
      CALL PGQWIN (WPIX(1), WPIX(2), WPIX(3), WPIX(4))
      CALL PGSVP (0.0, 1.0, 0.0, 1.0)
      XSCL = AXEN(1)/(XVP2-XVP1)
      YSCL = AXEN(2)/(YVP2-YVP1)
      CALL PGSWIN (0.5-XSCL*XVP1, AXEN(1)+0.5+XSCL*(1.0-XVP2),
     *             0.5-YSCL*YVP1, AXEN(2)+0.5+YSCL*(1.0-YVP2))

*     Labels only?
      IF (LABCTL.GE.10000) GO TO 120


*  Find world coordinate ranges.
      IF (IC.GE.0) THEN
*        Extrema cached from a previous call.
         WMIN(1) = CACHE(1,0)
         WMAX(1) = CACHE(2,0)
         WMIN(2) = CACHE(3,0)
         WMAX(2) = CACHE(4,0)
      ELSE
*        Coarse search to find approximate ranges.
         WMIN(1) =  1D99
         WMAX(1) = -1D99
         WMIN(2) =  1D99
         WMAX(2) = -1D99

*        Need to consider cycles in angle through 360 degrees.
         ISANGL(1) = INDEX('ABCDEFGHI',TYPE(1)).NE.0
         ISANGL(2) = INDEX('ABCDEFGHI',TYPE(2)).NE.0
         VMIN(1,1) =  1D99
         VMIN(1,2) =  1D99
         VMAX(1,1) = -1D99
         VMAX(1,2) = -1D99
         VMIN(2,1) =  1D99
         VMIN(2,2) =  1D99
         VMAX(2,1) = -1D99
         VMAX(2,2) = -1D99

*        Decimate the image.
         N1 = AXEN(1)/10 + 1
         N2 = AXEN(2)/10 + 1
         D1 = DBLE(AXEN(1))/N1
         D2 = DBLE(AXEN(2))/N2

         PIXEL(1) = 0.5D0
         DO 20 IPIX = 0, N1

            PIXEL(2) = 0.5D0
            DO 10 JPIX = 0, N2
               CALL NLFUNC (-1, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM,
     *            WORLD, PIXEL, CONTRL, CONTXT, IERR)
               IF (IERR.NE.0) GO TO 10

               IF (WORLD(1).LT.WMIN(1)) WMIN(1) = WORLD(1)
               IF (WORLD(1).GT.WMAX(1)) WMAX(1) = WORLD(1)
               IF (WORLD(2).LT.WMIN(2)) WMIN(2) = WORLD(2)
               IF (WORLD(2).GT.WMAX(2)) WMAX(2) = WORLD(2)

               IF (ISANGL(1)) THEN
*                 Normalize to the range [0,360).
                  WORLD(1) = MOD(WORLD(1), 360D0)
                  IF (WORLD(1).LT.0D0) WORLD(1) = WORLD(1) + 360D0
                  IF (WORLD(1).LT.VMIN(1,1)) VMIN(1,1) = WORLD(1)
                  IF (WORLD(1).GT.VMAX(1,1)) VMAX(1,1) = WORLD(1)

*                 Normalize to the range (-180,180].
                  IF (WORLD(1).GT.180D0) WORLD(1) = WORLD(1) - 360D0
                  IF (WORLD(1).LT.VMIN(1,2)) VMIN(1,2) = WORLD(1)
                  IF (WORLD(1).GT.VMAX(1,2)) VMAX(1,2) = WORLD(1)
               END IF

               IF (ISANGL(2)) THEN
*                 Normalize to the range [0,360).
                  WORLD(2) = MOD(WORLD(2), 360D0)
                  IF (WORLD(2).LT.0D0) WORLD(2) = WORLD(2) + 360D0
                  IF (WORLD(2).LT.VMIN(2,1)) VMIN(2,1) = WORLD(2)
                  IF (WORLD(2).GT.VMAX(2,1)) VMAX(2,1) = WORLD(2)

*                 Normalize to the range (-180,180].
                  IF (WORLD(2).GT.180D0) WORLD(2) = WORLD(2) - 360D0
                  IF (WORLD(2).LT.VMIN(2,2)) VMIN(2,2) = WORLD(2)
                  IF (WORLD(2).GT.VMAX(2,2)) VMAX(2,2) = WORLD(2)
               END IF

               PIXEL(2) = PIXEL(2) + D2
 10         CONTINUE

            PIXEL(1) = PIXEL(1) + D1
 20      CONTINUE

*        Check for cycles in angle.
         DO 30 J = 1, 2
            IF (ISANGL(J)) THEN
               IF (WMAX(J)-WMIN(J).LT.360D0 .AND.
     *             WMAX(J)-WMIN(J).GT.VMAX(J,1)-VMIN(J,1)+TOL) THEN
*                 Must have a cycle, preserve the sign.
                  IF (WMAX(J).GE.0D0) THEN
                     WMIN(J) = VMIN(J,1)
                     WMAX(J) = VMAX(J,1)
                  ELSE
                     WMIN(J) = VMIN(J,1) - 360D0
                     WMAX(J) = VMAX(J,1) - 360D0
                  END IF
               END IF

               IF (WMAX(J)-WMIN(J).LT.360D0 .AND.
     *             WMAX(J)-WMIN(J).GT.VMAX(J,2)-VMIN(J,2)+TOL) THEN
*                 Must have a cycle, preserve the sign.
                  IF (WMAX(J).GE.0D0) THEN
                     IF (VMAX(J,2).GE.0D0) THEN
                        WMIN(J) = VMIN(J,2)
                        WMAX(J) = VMAX(J,2)
                     ELSE
                        WMIN(J) = VMIN(J,2) + 360D0
                        WMAX(J) = VMAX(J,2) + 360D0
                     END IF
                  ELSE
                     IF (VMAX(J,2).LT.0D0) THEN
                        WMIN(J) = VMIN(J,2)
                        WMAX(J) = VMAX(J,2)
                     ELSE
                        WMIN(J) = VMIN(J,2) - 360D0
                        WMAX(J) = VMAX(J,2) - 360D0
                     END IF
                  END IF
               END IF
            END IF
 30      CONTINUE

*        Cache extrema for subsequent calls.
         CACHE(1,0) = WMIN(1)
         CACHE(2,0) = WMAX(1)
         CACHE(3,0) = WMIN(2)
         CACHE(4,0) = WMAX(2)

         IC = 0
      END IF

*     Choose an appropriate grid spacing.
      IF (LABDEN.GT.0) THEN
*        User specified grid density.
         DENS(1) = MOD(LABDEN,100)
         DENS(2) = LABDEN/100
         IF (DENS(1).EQ.0) DENS(1) = DENS0
         IF (DENS(2).EQ.0) DENS(2) = DENS0
      ELSE
*        Default grid density.
         DENS(1) = DENS0
         DENS(2) = DENS0
      END IF

      G0(1) = GRID1(0)
      G0(2) = GRID2(0)
      DO 40 J = 1, 2
         IF (J.EQ.1) THEN
            K = 2
         ELSE
            K = 1
         END IF

         IF (NG(J).EQ.0 .AND. G0(J).NE.0D0) THEN
            GSTEP(J) = G0(J)
         ELSE IF (DOEQ .AND. NG(K).EQ.0 .AND. G0(K).NE.0D0) THEN
            GSTEP(J) = G0(K)
         ELSE
            DW(J) = WMAX(J) - WMIN(J)
            STEP = DW(J)/DENS(J)
            IF (INDEX('ABCDEF',TYPE(J)).NE.0 .AND. STEP.GE.1D0) THEN
*              Angle with multi-degree increment.
               IF (STEP.LT.1.5D0) THEN
                  STEP = 1D0
               ELSE IF (STEP.LT.3D0) THEN
                  STEP = 2D0
               ELSE IF (STEP.LT.7D0) THEN
                  STEP = 5D0
               ELSE IF (STEP.LT.12D0) THEN
                  STEP = 10D0
               ELSE IF (STEP.LT.20D0) THEN
                  STEP = 15D0
               ELSE IF (STEP.LT.40D0) THEN
                  STEP = 30D0
               ELSE IF (STEP.LT.70D0) THEN
                  STEP = 45D0
               ELSE IF (STEP.LT.120D0) THEN
                  STEP = 90D0
               ELSE IF (STEP.LT.270D0) THEN
                  STEP = 180D0
               ELSE IF (STEP.LT.520D0) THEN
                  STEP = 360D0
               ELSE
                  STEP = 360D0*INT(STEP/360D0 + 0.5)
               END IF

            ELSE IF (INDEX('GHI',TYPE(J)).NE.0 .AND. STEP.GE.15D0 .OR.
     *         TYPE(J).EQ.'T' .AND. STEP.GE.1D0) THEN
*              Angle or time in hms format with multi-hour increment.

               FACT = 1D0
               IF (INDEX('GHI',TYPE(J)).NE.0) THEN
*                 Rescale degrees to hours.
                  FACT = FACT/15D0
                  STEP = STEP/15D0
               END IF

               IF (STEP.LT.1.5D0) THEN
                  STEP = 1D0
               ELSE IF (STEP.LT.2.5D0) THEN
                  STEP = 2D0
               ELSE IF (STEP.LT.3.5D0) THEN
                  STEP = 3D0
               ELSE IF (STEP.LT.5D0) THEN
                  STEP = 4D0
               ELSE IF (STEP.LT.7D0) THEN
                  STEP = 6D0
               ELSE IF (STEP.LT.10D0) THEN
                  STEP = 8D0
               ELSE IF (STEP.LT.15D0) THEN
                  STEP = 12D0
               ELSE IF (STEP.LT.21D0) THEN
                  STEP = 18D0
               ELSE IF (STEP.LT.36D0) THEN
                  STEP = 24D0
               ELSE
                  STEP = 24D0*INT(STEP/24D0 + 0.5)
               END IF

               STEP = STEP/FACT

            ELSE IF (INDEX('GHI',TYPE(J)).NE.0 .AND. STEP.LT.15D0 .OR.
     *               INDEX('DEFT',TYPE(J)).NE.0 .AND. STEP.LT.1D0) THEN
*              Angle or time in sexagesimal format with sub-degree/hour
*              increment.

               FACT = 1D0
               IF (INDEX('GHI',TYPE(J)).NE.0) THEN
*                 Rescale degrees to hours.
                  FACT = FACT/15D0
                  STEP = STEP/15D0
               END IF

               FACT = FACT*60D0
               STEP = STEP*60D0
               IF (STEP.LT.1D0) THEN
*                 Sub-minute increment.
                  FACT = FACT*60D0
                  STEP = STEP*60D0
               END IF

               IF (STEP.LT.1D0) THEN
*                 Sub-second increment.
                  TMP = 10D0**(INT(LOG10(STEP))-1)
                  IF (1.5*TMP.GE.STEP) THEN
                     STEP = TMP
                  ELSE
                     IF (3D0*TMP.GE.STEP) THEN
                        STEP = 2D0*TMP
                     ELSE
                        IF (7D0*TMP.GE.STEP) THEN
                           STEP = 5D0*TMP
                        ELSE
                           STEP = 10D0*TMP
                        END IF
                     END IF
                  END IF

               ELSE
                  IF (STEP.LT.1.5D0) THEN
                     STEP = 1D0
                  ELSE IF (STEP.LT.2.5D0) THEN
                     STEP = 2D0
                  ELSE IF (STEP.LT.3.5D0) THEN
                     STEP = 3D0
                  ELSE IF (STEP.LT.4.5D0) THEN
                     STEP = 4D0
                  ELSE IF (STEP.LT.5.5D0) THEN
                     STEP = 5D0
                  ELSE IF (STEP.LT.8D0) THEN
                     STEP = 6D0
                  ELSE IF (STEP.LT.11D0) THEN
                     STEP = 10D0
                  ELSE IF (STEP.LT.14D0) THEN
                     STEP = 12D0
                  ELSE IF (STEP.LT.18D0) THEN
                     STEP = 15D0
                  ELSE IF (STEP.LT.25D0) THEN
                     STEP = 20D0
                  ELSE IF (STEP.LT.45D0) THEN
                     STEP = 30D0
                  ELSE
                     STEP = 60D0
                  END IF
               END IF

               STEP = STEP/FACT

            ELSE
*              Just numbers.
               TMP = 10D0**INT(LOG10(STEP))
               IF (STEP.LT.1D0) TMP = TMP/10D0

               IF (1.5D0*TMP.GE.STEP) THEN
                  STEP = TMP
               ELSE
                  IF (3D0*TMP.GE.STEP) THEN
                     STEP = 2D0*TMP
                  ELSE
                     IF (7D0*TMP.GE.STEP) THEN
                        STEP = 5D0*TMP
                     ELSE
                        STEP = 10D0*TMP
                     END IF
                  END IF
               END IF

*              Adjust the step size for logarithmic values.
               IF (TYPE(J).EQ.'L') THEN
                  IF (STEP.GT.0.7D0) THEN
                     LDIV(J) = 1
                     STEP = NINT(STEP)
                  ELSE
                     IF (STEP.GT.0.4D0) THEN
                        LDIV(J) = 2
                     ELSE IF (STEP.GT.0.28D0) THEN
                        LDIV(J) = 3
                     ELSE IF (STEP.GT.0.23D0) THEN
                        LDIV(J) = 4
                     ELSE IF (STEP.GT.0.18D0) THEN
                        LDIV(J) = 5
                     ELSE IF (STEP.GT.0.12D0) THEN
                        LDIV(J) = 6
                     ELSE
                        LDIV(J) = 9
                     END IF

                     STEP = 1D0/LDIV(J)
                  END IF
               END IF
            END IF

            GSTEP(J) = STEP
         END IF
 40   CONTINUE

*     Equal grid spacing?
      IF (DOEQ .AND. NG(1).EQ.0 .AND. NG(2).EQ.0) THEN
         IF (GRID1(0).EQ.0D0 .AND. GRID2(0).EQ.0D0) THEN
            GSTEP(1) = MIN(GSTEP(1), GSTEP(2))
            GSTEP(2) = GSTEP(1)
         ELSE IF (GRID1(0).EQ.0D0) THEN
            GSTEP(1) = GSTEP(2)
         ELSE IF (GRID2(0).EQ.0D0) THEN
            GSTEP(2) = GSTEP(1)
         END IF
      END IF

*     Fine tune the end points.
      DO 50 J = 1, 2
         IF (TYPE(J).EQ.'L') THEN
            WMIN(J) = AINT(WMIN(J)-1D0)
            WMAX(J) = AINT(WMAX(J)+1D0)
         ELSE
            TMP = AINT(WMIN(J)/GSTEP(J))*GSTEP(J)
            IF (TMP.GE.WMIN(J)) TMP = TMP - GSTEP(J)
            WMIN(J) = TMP
            TMP = AINT(WMAX(J)/GSTEP(J))*GSTEP(J)
            IF (TMP.LE.WMAX(J)) TMP = TMP + GSTEP(J)
            WMAX(J) = TMP
         END IF
         DW(J) = WMAX(J) - WMIN(J)
 50   CONTINUE


*  Draw the grid.
*     Get absolute scale for tick marks.
      CALL PGQVP (2, X1, X2, Y1, Y2)
      XSCL = (X2-X1)/AXEN(1)
      YSCL = (Y2-Y1)/AXEN(2)

*     Decode tick mark control.
      DO 60 J = 1, 2
         IF (GCODE(J).EQ.2) THEN
            TCODE(J,1) = -1
            TCODE(J,2) = -1
            TCODE(J,3) = -1
            TCODE(J,4) = -1
         ELSE IF (GCODE(J).EQ.1) THEN
            TCODE(J,1) = 1
            TCODE(J,2) = 1
            TCODE(J,3) = 1
            TCODE(J,4) = 1
         ELSE IF (GCODE(J).LT.0) THEN
            K = ABS(GCODE(J))
            TCODE(J,1) = MOD(K,10)
            TCODE(J,2) = MOD(K/10,10)
            TCODE(J,3) = MOD(K/100,10)
            TCODE(J,4) = MOD(K/1000,10)
         ELSE
            TCODE(J,1) = 0
            TCODE(J,2) = 0
            TCODE(J,3) = 0
            TCODE(J,4) = 0
         END IF
 60   CONTINUE

*     Determine initial colour.
      CALL PGQCI (CI0)
      DO 70 J = 1, 7
         IF (CI(J).GE.0) THEN
            CJ(J) = CI(J)
         ELSE
            CJ(J) = CI0
         END IF
 70   CONTINUE

*     Draw each set of grid lines.
      DO 110 J = 1, 2
         IF (GCODE(J).EQ.0) GO TO 110

         IF (J.EQ.1) THEN
            CALL PGSCI (CJ(1))
            K = 2
         ELSE
            CALL PGSCI (CJ(2))
            K = 1
         END IF

         IF (NG(J).GT.0) THEN
            NW(J) = NG(J)
         ELSE
            NW(J) = NINT(DW(J)/GSTEP(J))
         END IF

         DO 100 IWJ = 0, NW(J)
            MAJOR = .FALSE.

*           Determine the world coordinate of the grid line.
            IF (NG(J).GT.0) THEN
*              User-specified.
               IF (IWJ.EQ.0) GO TO 100
               WORLD(1) = GRID1(IWJ)
               WORLD(2) = GRID2(IWJ)
            ELSE
*              Internally computed.
               WORLD(J) = WMIN(J) + IWJ*GSTEP(J)

*              Logarithmic?
               IF (TYPE(J).EQ.'L') THEN
                  TMP = MOD(WORLD(J),1D0)
                  IF (TMP.LT.0D0) TMP = TMP + 1D0
                  L = NINT(TMP*LDIV(J))

                  IF (L.EQ.0) THEN
*                    Major tick mark.
                     MAJOR = .TRUE.
                  ELSE
*                    Adjust logarithmic scales.
                     IF (LDIV(J).LE.6) THEN
                        L = LTABL(L,LDIV(J))
                     ELSE
                        L = L + 1
                     END IF

                     WORLD(J) = WORLD(J) - TMP + LOG10(DBLE(L))
                  END IF
               END IF
            END IF

            NP = 0
            GETEND = .TRUE.
            DO 90 IWK = 0, NSTEP
               WORLD(K) = WMIN(K) + IWK*(DW(K)/NSTEP)

               IF (GETEND) THEN
*                 Get end-point context.
                  CALL NLFUNC (1, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM,
     *               WORLD, PIXEL, CONTRL, CONTXT, IERR)
                  IF (IERR.EQ.0) THEN
                     INSIDE = PIXEL(1).GT.0.5 .AND.
     *                        PIXEL(1).LT.AXEN(1)+0.5 .AND.
     *                        PIXEL(2).GT.0.5 .AND.
     *                        PIXEL(2).LT.AXEN(2)+0.5

                     NP = 1
                     XR(1) = PIXEL(1)
                     YR(1) = PIXEL(2)

                     PRVIN  = INSIDE
                     GETEND = .FALSE.
                  END IF
                  GO TO 90
               END IF

               DO 80 ISTEP = 1, 1000
                  CALL NLFUNC (2, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM,
     *               WORLD, PIXEL, CONTRL, CONTXT, IERR)
                  IF (IERR.NE.0) THEN
*                    Flush buffer.
                     IF (NP.GT.0) CALL PGLINE(NP, XR, YR)
                     GO TO 100
                  END IF

                  IF (NP.EQ.BUFSIZ) THEN
*                    Recycle buffer.
                     CALL PGLINE(NP, XR, YR)
                     XR(1) = XR(NP)
                     YR(1) = YR(NP)
                     NP = 1
                  END IF

                  INSIDE = PIXEL(1).GT.0.5 .AND.
     *                     PIXEL(1).LT.AXEN(1)+0.5 .AND.
     *                     PIXEL(2).GT.0.5 .AND.
     *                     PIXEL(2).LT.AXEN(2)+0.5

                  IF (NP.EQ.0) THEN
                     NP = 1
                     XR(1) = PIXEL(1)
                     YR(1) = PIXEL(2)
                  ELSE
                     IF (INSIDE) THEN
*                       This point is inside the frame...
                        IF (.NOT.PRVIN) THEN
*                          ...but the previous one was outside.
                           X1 = XR(NP)
                           Y1 = YR(NP)
                           X2 = PIXEL(1)
                           Y2 = PIXEL(2)

                           IF (X2.NE.X1) THEN
                              S = (Y2-Y1)/(X2-X1)
                              IF (XR(NP).LE.0.5) THEN
                                 FSEG = 2
                                 XR(NP) = 0.5
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              ELSE IF (XR(NP).GE.AXEN(1)+0.5) THEN
                                 FSEG = 4
                                 XR(NP) = AXEN(1) + 0.5
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              END IF
                           END IF

                           IF (Y2.NE.Y1) THEN
                              S = (X2-X1)/(Y2-Y1)
                              IF (YR(NP).LE.0.5) THEN
                                 FSEG = 1
                                 YR(NP) = 0.5
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              ELSE IF (YR(NP).GE.AXEN(2)+0.5) THEN
                                 FSEG = 3
                                 YR(NP) = AXEN(2) + 0.5
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              END IF
                           END IF

*                          Record this crossing point.
                           IF (TCODE(J,FSEG).NE.0) THEN
                              IF (IC.LT.NC) THEN
                                 IC = IC + 1
                                 CACHE(1,IC) = FSEG
                                 CACHE(2,IC) = XPOINT
                                 CACHE(3,IC) = J
                                 CACHE(4,IC) = WORLD(J)
                              END IF
                           END IF

                           IF (TCODE(J,FSEG).GT.0) THEN
*                             Just want tick marks.
                              X1 = XR(NP)
                              Y1 = YR(NP)
                              S = SQRT((XSCL*(X2-X1))**2 +
     *                                 (YSCL*(Y2-Y1))**2)/TCODE(J,FSEG)
                              IF (MAJOR) S = S/1.5
                              NP = NP + 1
                              XR(NP) = X1 + (X2-X1)*TIKLEN/S
                              YR(NP) = Y1 + (Y2-Y1)*TIKLEN/S

                              CALL PGLINE(NP, XR, YR)
                              NP = 1
                           END IF
                        END IF

                        IF (GCODE(J).EQ.2) THEN
*                          Full grid.
                           NP = NP + 1
                        END IF
                        XR(NP) = PIXEL(1)
                        YR(NP) = PIXEL(2)
                     ELSE
*                       This point is outside the frame...
                        IF (PRVIN) THEN
*                          ...but the previous one was inside.
                           X1 = XR(NP)
                           Y1 = YR(NP)
                           X2 = PIXEL(1)
                           Y2 = PIXEL(2)

                           NP = NP + 1
                           XR(NP) = PIXEL(1)
                           YR(NP) = PIXEL(2)
                           IF (X2.NE.X1) THEN
                              S = (Y2-Y1)/(X2-X1)
                              IF (XR(NP).LE.0.5) THEN
                                 FSEG = 2
                                 XR(NP) = 0.5
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              ELSE IF (XR(NP).GE.AXEN(1)+0.5) THEN
                                 FSEG = 4
                                 XR(NP) = AXEN(1)+0.5
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              END IF
                           END IF

                           IF (Y2.NE.Y1) THEN
                              S = (X2-X1)/(Y2-Y1)
                              IF (YR(NP).LE.0.5) THEN
                                 FSEG = 1
                                 YR(NP) = 0.5
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              ELSE IF (YR(NP).GE.AXEN(2)+0.5) THEN
                                 FSEG = 3
                                 YR(NP) = AXEN(2)+0.5
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              END IF
                           END IF

*                          Record this crossing point.
                           IF (TCODE(J,FSEG).NE.0) THEN
                              IF (IC.LT.NC) THEN
                                 IC = IC + 1
                                 CACHE(1,IC) = FSEG
                                 CACHE(2,IC) = XPOINT
                                 CACHE(3,IC) = J
                                 CACHE(4,IC) = WORLD(J)
                              END IF
                           END IF

                           IF (TCODE(J,FSEG).GT.0) THEN
*                             Just want tick marks.
                              X1 = XR(NP)
                              Y1 = YR(NP)
                              X2 = XR(NP-1)
                              Y2 = YR(NP-1)
                              S = SQRT((XSCL*(X2-X1))**2 +
     *                                 (YSCL*(Y2-Y1))**2)/TCODE(J,FSEG)
                              IF (MAJOR) S = S/1.5
                              XR(NP-1) = X1 + (X2-X1)*TIKLEN/S
                              YR(NP-1) = Y1 + (Y2-Y1)*TIKLEN/S
                           END IF

*                          Flush buffer.
                           IF (TCODE(J,FSEG).NE.0) THEN
                              CALL PGLINE(NP, XR, YR)
                           END IF
                           NP = 0
                        ELSE
*                          The previous point was also outside.
                           XR(NP) = PIXEL(1)
                           YR(NP) = PIXEL(2)
                        END IF
                     END IF
                  END IF

                  PRVIN = INSIDE

                  IF (CONTRL.EQ.0) THEN
                     GO TO 90
                  ELSE IF (CONTRL.EQ.1) THEN
*                    Flush buffer.
                     IF (NP.GT.1) CALL PGLINE(NP, XR, YR)
                     NP = 0
                  END IF
 80            CONTINUE
 90         CONTINUE

            IF (NP.GT.1) CALL PGLINE(NP, XR, YR)
 100     CONTINUE
 110  CONTINUE


*  Produce axis labels.
 120  IF (LABCTL.NE.-1) CALL PGCRLB (AXEN, IDENTS, TYPE, LABCTL, CJ, NC,
     *   IC, CACHE)


*  Clean up.
*     Restore the original viewport, window and pen colour.
      CALL PGSVP (XVP1, XVP2, YVP1, YVP2)
      CALL PGSWIN (WPIX(1), WPIX(2), WPIX(3), WPIX(4))
      CALL PGSCI (CI0)

      RETURN
      END



*=======================================================================
*
*   PGCRLB is a helper routine for PGCRVL, not meant to be called
*   directly since it expects the viewport and window to be scaled to
*   the full extent; it labels a curvilinear coordinate grid.
*
*   Given:
*      AXEN     I(2)     Number of pixels on each axis of the image.
*
*      IDENTS   C(3)**   Identification strings (see PGCRVL).
*
*      TYPE     C(2)*1   Axis types, used for axis labelling (see
*                        PGCRVL).
*
*      LABCTL   I        Decimal encoded grid labelling control (see
*                        PGCRVL).
*
*      CI       I(7)     Colour table (see PGCRVL).
*
*   Given and/or returned:
*      NC       I        Upper array index for CACHE.
*
*      IC       I        Current number of entries in the CACHE table.
*
*      CACHE    D(4,0:NC)
*                        Table of points where the tick marks or grid
*                        lines cross the frame (see PGCRVL).
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*=======================================================================
      SUBROUTINE PGCRLB (AXEN, IDENTS, TYPE, LABCTL, CI, NC, IC, CACHE)
*-----------------------------------------------------------------------
      LOGICAL   ANGLE, LFORCE, TICKIT
      INTEGER   AXEN(2), CI(7), EDGE, IC, IMAG(2), ITER, IWRLD, J, JC,
     *          K, K1, K2, L, LABCTL, LD, LM, LMAG(2), LS, LV, M, M1,
     *          M2, MM, NC, NCH, NI(2,0:4), NSWAP, PP, PRVD(2), PRVH(2),
     *          PRVM(2), PRVEDG, SKIP(4), SKOP(4)
      REAL      ANGL, FJUST, BNDRY(4), OMAG(2), SI(2), X, XBOX(4),
     *          XCH, XL, XW1, XW2, Y, YCH, YBOX(4), YL, YW1, YW2, Z
      DOUBLE PRECISION CACHE(4,0:NC), TMP
      CHARACTER EXPONT*20, FMT*8, IDENTS(3)*(*), TEXT*80, TYPE(2)*1,
     *          TXT(2)*40
*-----------------------------------------------------------------------
*  Normalize angular table entries.
      IF (INDEX('ABDEGH',TYPE(1)).NE.0 .OR.
     *    INDEX('ABDEGH',TYPE(2)).NE.0) THEN
         DO 10 J = 1, IC
            IF (CACHE(3,J).EQ.1D0) THEN
               IWRLD = 1
            ELSE
               IWRLD = 2
            END IF

            IF (INDEX('ADG', TYPE(IWRLD)).NE.0) THEN
               CACHE(4,J) = MOD(CACHE(4,J), 360D0)
               IF (CACHE(4,J).LT.0D0) CACHE(4,J) = CACHE(4,J) + 360D0
            ELSE IF (INDEX('BEH', TYPE(IWRLD)).NE.0) THEN
               CACHE(4,J) = MOD(CACHE(4,J), 360D0)
               IF (CACHE(4,J).LE.-180D0) THEN
                  CACHE(4,J) = CACHE(4,J) + 360D0
               ELSE IF (CACHE(4,J).GT.180D0) THEN
                  CACHE(4,J) = CACHE(4,J) - 360D0
               END IF
            END IF

            IF (INDEX('GHI', TYPE(IWRLD)).NE.0) THEN
*              Angle expressed as time.
               CACHE(4,J) = CACHE(4,J)/15D0
            END IF
 10      CONTINUE
      END IF


*  Reorganize the table entries.
*     Sort crossings for each of the four frame segments.
      DO 40 ITER = 1, IC
         NSWAP = 0
         DO 30 J = 1, IC-1
            IF (CACHE(1,J).LT.CACHE(1,J+1)) GO TO 30
            IF (CACHE(1,J).EQ.CACHE(1,J+1) .AND.
     *          CACHE(2,J).LE.CACHE(2,J+1)) GO TO 30

            NSWAP = NSWAP + 1
            DO 20 M = 1, 4
               TMP = CACHE(M,J)
               CACHE(M,J) = CACHE(M,J+1)
               CACHE(M,J+1) = TMP
 20         CONTINUE
 30      CONTINUE
         IF (NSWAP.EQ.0) GO TO 50
 40   CONTINUE

*     Squeeze out duplicates.
 50   JC = IC
      DO 90 J = 2, IC
         IF (J.GT.JC) GO TO 100

         DO 60 M = 1, 4
            IF (CACHE(M,J).NE.CACHE(M,J-1)) GO TO 90
 60      CONTINUE

*        This entry is the same as the previous one.
         JC = JC - 1
         DO 80 K = J, JC
            DO 70 M = 1, 4
               CACHE(M,K) = CACHE(M,K+1)
 70         CONTINUE
 80      CONTINUE
 90   CONTINUE

 100  IC = JC


*  How do we label the edges of the frame?
*     Determine separability indices.
      NI(1,0) = 0
      NI(1,1) = 0
      NI(1,2) = 0
      NI(1,3) = 0
      NI(1,4) = 0
      NI(2,0) = 0
      NI(2,1) = 0
      NI(2,2) = 0
      NI(2,3) = 0
      NI(2,4) = 0
      DO 110 J = 1, IC
         IF (CACHE(3,J).EQ.1D0) THEN
            IWRLD = 1
         ELSE
            IWRLD = 2
         END IF

         EDGE = NINT(CACHE(1,J))

         NI(IWRLD,0) = NI(IWRLD,0) + 1
         NI(IWRLD,EDGE) = NI(IWRLD,EDGE) + 1
 110  CONTINUE

      SI(1) = 0.0
      SI(2) = 0.0
      IF (NI(1,0).GT.0) SI(1) = 2.0*REAL(NI(1,1)+NI(1,3))/NI(1,0) - 1.0
      IF (NI(2,0).GT.0) SI(2) = 2.0*REAL(NI(2,1)+NI(2,3))/NI(2,0) - 1.0

*     Which coordinates go on which edges?
      IF (LABCTL.GT.0) THEN
*        User-defined.
         L = LABCTL
      ELSE
*        Work it out ourselves.
         L = 0
         IF (ABS(SI(1)-SI(2)).GT.1.0) THEN
*           Approximately horizontal/vertical grid lines.
            IF (SI(1).GT.SI(2)) THEN
*              First world coordinate with vertical grid lines.
               IF (NI(1,1).GT.3 .OR. NI(1,1).GE.NI(1,3)) THEN
*                 Label bottom of frame.
                  L = L + 1
               ELSE
*                 Label top of frame.
                  L = L + 100
               END IF

*              Second world coordinate with horizontal grid lines.
               IF (NI(2,2).GT.3 .OR. NI(2,2).GE.NI(2,4)) THEN
*                 Label left side of frame.
                  L = L + 20
               ELSE
*                 Label right side of frame.
                  L = L + 2000
               END IF
            ELSE
*              First world coordinate with horizontal grid lines.
               IF (NI(1,2).GT.3 .OR. NI(1,2).GE.NI(1,4)) THEN
*                 Label left side of frame.
                  L = L + 10
               ELSE
*                 Label right side of frame.
                  L = L + 1000
               END IF

*              Second world coordinate with vertical grid lines.
               IF (NI(2,1).GT.3 .OR. NI(2,1).GE.NI(2,3)) THEN
*                 Label bottom of frame.
                  L = L + 2
               ELSE
*                 Label top of frame.
                  L = L + 200
               END IF
            END IF
         ELSE
*           Skew grid lines or worse.
            IF (SI(1).GT.0.5D0) THEN
*              First world coordinate with vertical grid lines.
               IF (NI(1,1).GT.1 .OR. NI(1,1).GT.NI(1,3)) THEN
                  L = 1
               ELSE
                  L = 100
               END IF

               IF (SI(2).GT.0.5D0) THEN
*                 Second world coordinate also with vertical grid lines.
                  IF (L.EQ.1) THEN
                     L = 201
                  ELSE
                     IF (NI(2,1).GT.1 .OR. NI(2,1).GT.NI(2,3)) THEN
                        L = 102
                     ELSE
                        L = 300
                     END IF
                  END IF
               ELSE
*                 Second world coordinate with diagonal grid lines.
                  L = L + 2020
               END IF
            ELSE IF (SI(1).LT.-0.5D0) THEN
*              First world coordinate with horizontal grid lines.
               IF (NI(1,2).GT.1 .OR. NI(1,2).GT.NI(1,4)) THEN
                  L = 10
               ELSE
                  L = 1000
               END IF

               IF (SI(2).LT.-0.5D0) THEN
*                 Second world coordinate also with horizontal grid.
                  IF (L.EQ.10) THEN
                     L = 2010
                  ELSE
                     IF (NI(2,2).GT.1 .OR. NI(2,2).GT.NI(2,4)) THEN
                        L = 1020
                     ELSE
                        L = 3000
                     END IF
                  END IF
               ELSE
*                 Second world coordinate with diagonal grid lines.
                  L = L + 202
               END IF
            ELSE IF (SI(2).GT.0.5D0) THEN
*              Second world coordinate with vertical grid lines.
               IF (NI(2,1).GT.1 .OR. NI(2,1).GT.NI(2,3)) THEN
                  L = 1012
               ELSE
                  L = 1210
               END IF
            ELSE IF (SI(2).LT.-0.5D0) THEN
*              Second world coordinate with horizontal grid lines.
               IF (NI(2,2).GT.1 .OR. NI(2,2).GT.NI(2,4)) THEN
                  L = 121
               ELSE
                  L = 2101
               END IF
            ELSE
*              Desperation stakes!  Label all four axes.
               K1 = NI(1,3) + NI(1,1)
               K2 = NI(2,4) + NI(2,2)
               L = 2121

               M1 = NI(1,4) + NI(1,1)
               M2 = NI(2,3) + NI(2,2)
               IF (M1.GE.K1 .AND. M2.GE.K2) THEN
                  L  = 1221
                  K1 = M1
                  K2 = M2
               END IF

               M1 = NI(1,2) + NI(1,1)
               M2 = NI(2,4) + NI(2,3)
               IF (M1.GE.K1 .AND. M2.GE.K2) THEN
                  L  = 2211
                  K1 = M1
                  K2 = M2
               END IF

               M1 = NI(1,4) + NI(1,2)
               M2 = NI(2,3) + NI(2,1)
               IF (M1.GE.K1 .AND. M2.GE.K2) THEN
                  L  = 1212
                  K1 = M1
                  K2 = M2
               END IF

               M1 = NI(1,3) + NI(1,2)
               M2 = NI(2,4) + NI(2,1)
               IF (M1.GE.K1 .AND. M2.GE.K2) THEN
                  L  = 2112
                  K1 = M1
                  K2 = M2
               END IF

               M1 = NI(1,4) + NI(1,3)
               M2 = NI(2,2) + NI(2,1)
               IF (M1.GE.K1 .AND. M2.GE.K2) THEN
                  L  = 1122
                  K1 = M1
                  K2 = M2
               END IF
            END IF
         END IF
      END IF

*     Extract the information in a more usable form.
      SKIP(1) = MOD(L,10)
      SKIP(2) = MOD(L/10,10)
      SKIP(3) = MOD(L/100,10)
      SKIP(4) = MOD(L/1000,10)


*  Determine labelling precision.
*     Order of magnitude for plain numeric world coordinates.
      IMAG(1) = 0
      IMAG(2) = 0

      IF (TYPE(1).EQ.' ' .OR. TYPE(2).EQ.' ') THEN
         OMAG(1) = 0.0
         OMAG(2) = 0.0
         DO 120 J = 1, IC
            IF (CACHE(3,J).EQ.1D0) THEN
               IWRLD = 1
            ELSE
               IWRLD = 2
            END IF

            IF (TYPE(IWRLD).EQ.' ') THEN
*              Plain numeric.
               IF (CACHE(4,J).EQ.0D0) GO TO 120
               OMAG(IWRLD) = OMAG(IWRLD) + LOG10(ABS(CACHE(4,J)))
               IMAG(IWRLD) = IMAG(IWRLD) + 1
            END IF
 120     CONTINUE

         IF (IMAG(1).GT.0) IMAG(1) = INT(OMAG(1)/IMAG(1))
         IF (IMAG(1).GE.-2 .AND. IMAG(1).LE.4) IMAG(1) = 0

         IF (IMAG(2).GT.0) IMAG(2) = INT(OMAG(2)/IMAG(2))
         IF (IMAG(2).GE.-2 .AND. IMAG(2).LE.4) IMAG(2) = 0

*        Renormalize grid values.
         IF (IMAG(1).NE.0 .OR. IMAG(2).NE.0) THEN
            DO 130 J = 1, IC
               IF (CACHE(3,J).EQ.1D0) THEN
                  IWRLD = 1
               ELSE
                  IWRLD = 2
               END IF

               CACHE(4,J) = CACHE(4,J)/10D0**IMAG(IWRLD)
 130        CONTINUE
         END IF
      END IF

*     Sexagesimal labelling.
      IF (INDEX('DEFGHIT', TYPE(1)).NE.0 .OR.
     *    INDEX('DEFGHIT', TYPE(2)).NE.0) THEN
         LMAG(1) = -2
         LMAG(2) = -2

         DO 150 J = 1, IC
            IF (CACHE(3,J).EQ.1D0) THEN
               IWRLD = 1
            ELSE
               IWRLD = 2
            END IF

*           Skip non-sexagesimal coordinates.
            IF (INDEX('DEFGHIT', TYPE(IWRLD)).EQ.0) GO TO 150

*           Defeat rounding errors.
            LS = INT(ABS(CACHE(4,J)*3600D0) + 0.5D-6)
            LV = NINT(MOD(ABS(CACHE(4,J)*3600D0), 1D0)*1D6)
            IF (LV.NE.0) THEN
*              Sub-arcsec/second resolution.
               M = 1
               DO 140 K = 1, 6
                  M = M*10
                  IF (MOD(LV,M).NE.0) THEN
                     LMAG(IWRLD) = MAX(LMAG(IWRLD), 7-K)
                     GO TO 150
                  END IF
 140           CONTINUE
            ELSE IF (LMAG(IWRLD).LT.0) THEN
               IF (MOD(LS,60).NE.0) THEN
                  LMAG(IWRLD) = MAX(LMAG(IWRLD), 0)
               ELSE IF (MOD(LS,3600).NE.0) THEN
                  LMAG(IWRLD) = MAX(LMAG(IWRLD), -1)
               END IF
            END IF
 150     CONTINUE
      END IF


*  Produce labels.
      XW1 = 0.5
      XW2 = AXEN(1) + 0.5
      YW1 = 0.5
      YW2 = AXEN(2) + 0.5

*     These will define a box which just contains the labels.
      BNDRY(1) = YW1
      BNDRY(2) = XW1
      BNDRY(3) = YW2
      BNDRY(4) = XW2

*     These will record the edges on which labels were actually written.
      SKOP(1) = 0
      SKOP(2) = 0
      SKOP(3) = 0
      SKOP(4) = 0

*     Character height, in pixels.
      CALL PGQCS (4, XCH, YCH)
      PRVEDG = 0

*     Loop through the axis crossing table.
      DO 180 J = 1, IC
*        Determine the position.
         IF (CACHE(3,J).EQ.1D0) THEN
            IWRLD = 1
            CALL PGSCI (CI(3))
         ELSE
            CALL PGSCI (CI(4))
            IWRLD = 2
         END IF

         EDGE = NINT(CACHE(1,J))
         IF (EDGE.NE.PRVEDG) THEN
            PRVD(1) = -1
            PRVD(2) = -1
            PRVH(1) = -1
            PRVH(2) = -1
            PRVM(1) = -1
            PRVM(2) = -1
            XL = -999.0
            YL = -999.0
         END IF
         PRVEDG = EDGE
         LFORCE = .FALSE.

         IF (EDGE.EQ.1) THEN
*           Bottom.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(1),2).NE.1) GO TO 180
            ELSE
               IF (MOD(SKIP(1)/2,2).NE.1) GO TO 180
            END IF

            FJUST = 0.5
            X = CACHE(2,J)
            Y = YW1 - 1.5*YCH
         ELSE IF (EDGE.EQ.2) THEN
*           Left.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(2),2).NE.1) GO TO 180
            ELSE
               IF (MOD(SKIP(2)/2,2).NE.1) GO TO 180
            END IF

            FJUST = 1.0
            X = XW1 - 0.5*XCH
            Y = CACHE(2,J) - YCH/2.0
         ELSE IF (EDGE.EQ.3) THEN
*           Top.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(3),2).NE.1) GO TO 180
            ELSE
               IF (MOD(SKIP(3)/2,2).NE.1) GO TO 180
            END IF

            FJUST = 0.5
            X = CACHE(2,J)
            Y = YW2 + 0.5*YCH
         ELSE IF (EDGE.EQ.4) THEN
*           Right.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(4),2).NE.1) GO TO 180
            ELSE
               IF (MOD(SKIP(4)/2,2).NE.1) GO TO 180
            END IF

            FJUST = 0.0
            X = XW2 + 0.5*XCH
            Y = CACHE(2,J) - YCH/2.0
         END IF

*        Format the numeric label.
         IF (INDEX('ABC', TYPE(IWRLD)).NE.0) THEN
*           Decimal angle; allow up to 6 decimal digits.
            TMP = ABS(CACHE(4,J)) + 0.5D-6
            LD  = INT(TMP)

            K = 1
            IF (CACHE(4,J).LT.0D0) THEN
*              Insert a minus sign.
               TEXT(1:1) = '-'
               K = 2
            END IF

            CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
            K = K + NCH
            TEXT(K:) = '\\uo\\d'
            K = K + 5

            LV = INT(MOD(TMP,1D0)*1D6)
            IF (LV.NE.0) CALL PGNUMB (LV, -6, 1, TEXT(K:), NCH)

         ELSE IF (INDEX('DEFGHIT', TYPE(IWRLD)).NE.0) THEN
*           Sexagesimal format; angle or time?
            ANGLE = INDEX('DEF', TYPE(IWRLD)).NE.0
            L = LMAG(IWRLD)

*           Use integer arithmetic to avoid rounding problems.
            TMP = ABS(CACHE(4,J))*3600D0 + 0.5D-6
            LV = INT(TMP)

*           Degree/hour field.
            LD = LV/3600

            K = 1
            IF (L.EQ.-2 .OR. LD.NE.PRVD(IWRLD)) THEN
*              Write the degree/hour field.
               IF (CACHE(4,J).LT.0D0) THEN
*                 Insert a minus sign.
                  TEXT(1:1) = '-'
                  K = 2
               END IF

               IF (ANGLE) THEN
*                 Angle.
                  CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
                  K = K + NCH
                  TEXT(K:) = '\\uo\\d'
                  K = K + 5
               ELSE
*                 Time.
                  IF (LD.LE.9 .AND. TYPE(IWRLD).NE.'T') THEN
*                    Write leading zeroes in the hour field.
                     WRITE (TEXT(K:), '(I2.2)') LD
                     K = K + 2
                  ELSE
                     CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
                     K = K + NCH
                  END IF
                  TEXT(K:) = '\\uh\\d'
                  K = K + 5
               END IF

            END IF

            IF (L.GE.-1) THEN
*              Arcminute/minute field.
               LV = LV - LD*3600
               LM = LV/60

               IF (L.EQ.-1 .OR. K.GT.1 .OR. LM.NE.PRVM(IWRLD)) THEN
                  IF (ANGLE) THEN
                     WRITE (TEXT(K:), '(I2.2,A)') LM, ''''
                     K = K + 3
                  ELSE
                     WRITE (TEXT(K:), '(I2.2,A)') LM, '\\um\\d'
                     K = K + 7
                  END IF
               END IF

               IF (L.GE.0) THEN
*                 Arcsec/second field.
                  LS = LV - LM*60

                  IF (ANGLE) THEN
                     WRITE (TEXT(K:), '(I2.2,A)') LS, '"'
                     K = K + 3
                  ELSE
                     WRITE (TEXT(K:), '(I2.2,A)') LS,'\\us\\d'
                     K = K + 7
                  END IF

                  IF (L.GT.0) THEN
*                    Sub-arcsec/second field.
                     WRITE (FMT, '(A,I1,A,I1,A)') '(A,I', L, '.', L, ')'
                     LV = INT(MOD(TMP,1D0)*10**L)
                     WRITE (TEXT(K:), FMT) '.', LV
                  END IF
               END IF
            END IF

         ELSE IF (TYPE(IWRLD).EQ.'L') THEN
*           Logarithmic.
            TMP = MOD(CACHE(4,J),1D0)
            IF (TMP.LT.0D0) TMP = TMP + 1D0
            MM = NINT(10D0**TMP)
            IF (MM.EQ.10) THEN
               TMP = TMP - 1D0
               MM = 1
            END IF

            PP = NINT(CACHE(4,J) - TMP)

            IF (MM.NE.1) THEN
               WRITE (TEXT, '(I1)') MM
            ELSE
*              FORTRAN is really abysmal sometimes.
               WRITE (TEXT, '(I8)') PP
               DO 160 K = 1, 8
                  IF (TEXT(K:K).NE.' ') GO TO 170
 160           CONTINUE
 170           TEXT = '10\\u' // TEXT(K:8)

               LFORCE = .TRUE.
            END IF

         ELSE
*           Plain number; allow up to six significant digits.
            IF (CACHE(4,J).NE.0D0) THEN
               PP = INT(LOG10(ABS(CACHE(4,J)))) - 6
               MM = NINT(CACHE(4,J)/10D0**PP)
            ELSE
               PP = 0
               MM = 0
            END IF

            CALL PGNUMB (MM, PP, 0, TEXT, NCH)
         END IF

*        Write the label if it doesn't overlap the previous one.
         CALL PGQTXT (X, Y, 0.0, FJUST, TEXT, XBOX, YBOX)
         IF (LFORCE .OR. XBOX(1).GT.XL .OR. YBOX(1).GT.YL) THEN
            IF (IWRLD.EQ.1) THEN
               CALL PGSCI (CI(3))
            ELSE
               CALL PGSCI (CI(4))
            END IF

            CALL PGPTXT (X, Y, 0.0, FJUST, TEXT)
            XL = XBOX(4) + XCH
            YL = YBOX(2) + 0.5*YCH

*           Sexagesimal formatting.
            PRVD(IWRLD) = LD
            PRVM(IWRLD) = LM

*           Record the fact.
            IF (IWRLD.EQ.1) THEN
               SKOP(EDGE) = 2*(SKOP(EDGE)/2) + 1
            ELSE
               SKOP(EDGE) = 2 + MOD(SKOP(EDGE),2)
            END IF

*           Boundary within which the numeric labels lie.
            IF (EDGE.EQ.1) THEN
               IF (YBOX(1).LT.BNDRY(1)) BNDRY(1) = YBOX(1)
            ELSE IF (EDGE.EQ.2) THEN
               IF (XBOX(1).LT.BNDRY(2)) BNDRY(2) = XBOX(1)
            ELSE IF (EDGE.EQ.3) THEN
               IF (YBOX(2).GT.BNDRY(3)) BNDRY(3) = YBOX(2)
            ELSE IF (EDGE.EQ.4) THEN
               IF (XBOX(4).GT.BNDRY(4)) BNDRY(4) = XBOX(4)
            END IF

*           Check the distance to the previous grid line.
            IF (J.GT.1) THEN
               IF (CACHE(1,J).EQ.CACHE(1,J-1)) THEN
                  IF (EDGE.EQ.1 .OR. EDGE.EQ.3) THEN
                     TICKIT = XBOX(1).LT.CACHE(2,J-1)
                  ELSE
                     TICKIT = YBOX(1).LT.CACHE(2,J-1)
                  END IF
               END IF
            END IF

*           Check the distance to the next grid line.
            IF (J.LT.IC) THEN
               IF (CACHE(1,J).EQ.CACHE(1,J+1)) THEN
                  IF (EDGE.EQ.1 .OR. EDGE.EQ.3) THEN
                     TICKIT = XBOX(2).GT.CACHE(2,J+1)
                  ELSE
                     TICKIT = YBOX(2).GT.CACHE(2,J+1)
                  END IF
               END IF
            END IF

*           Density of grid lines is high.
            IF (TICKIT) THEN
*              Draw outside pip mark.
               Z = REAL(CACHE(2,J))
               IF (EDGE.EQ.1) THEN
                  CALL PGMOVE (Z, YW1-0.3*YCH)
                  CALL PGDRAW (Z, YW1)
               ELSE IF (EDGE.EQ.2) THEN
                  CALL PGMOVE (XW1-0.3*XCH, Z)
                  CALL PGDRAW (XW1, Z)
               ELSE IF (EDGE.EQ.3) THEN
                  CALL PGMOVE (Z, YW2+0.3*YCH)
                  CALL PGDRAW (Z, YW2)
               ELSE IF (EDGE.EQ.4) THEN
                  CALL PGMOVE (XW2+0.3*XCH, Z)
                  CALL PGDRAW (XW2, Z)
               END IF
            END IF
         END IF
 180  CONTINUE


*  Write the identification strings.
*     World coordinates.
      DO 260 EDGE = 1, 4
         TEXT = ' '

         DO 230 IWRLD = 1, 2
            TXT(IWRLD) = ' '

            IF (MOD(SKOP(EDGE)/IWRLD,2).EQ.0) GO TO 230

            L = LEN(IDENTS(IWRLD))
            DO 190 K = 1, L
               IF (IDENTS(IWRLD)(K:K).NE.' ') THEN
                  TXT(IWRLD) = IDENTS(IWRLD)(K:L)
                  GO TO 200
               END IF
 190        CONTINUE

 200        IF (IMAG(IWRLD).NE.0) THEN
               DO 210 K = 40, 1, -1
                  IF (TXT(IWRLD)(K:K).NE.' ') GO TO 220
 210           CONTINUE

*              Add scaling information.
 220           CALL PGNUMB (IMAG(IWRLD), 0, 1, EXPONT, NCH)
               TXT(IWRLD)(K+1:) = '  x10\\u' // EXPONT
            END IF

 230     CONTINUE

         K = 0
         IF (TXT(1).NE.' ') THEN
*           Identify first world coordinate...
            TEXT = TXT(1)
            CALL PGSCI (CI(5))

            IF (TXT(2).NE.' ') THEN
*              ...and also second world coordinate.
               DO 240 K = 40, 1, -1
                  IF (TEXT(K:K).NE.' ') GO TO 250
 240           CONTINUE

 250           K = K + 1
               TEXT(K:) = ',  ' // TXT(2)
            END IF
         ELSE IF (TXT(2).NE.' ') THEN
*           Identify second world coordinate only.
            TEXT = TXT(2)
            CALL PGSCI (CI(6))
         ELSE
*           No text to write.
            GO TO 260
         END IF

         IF (EDGE.EQ.1) THEN
            X = (XW1 + XW2)/2.0
            Y = BNDRY(1) - 1.5*YCH
            ANGL = 0.0
         ELSE IF (EDGE.EQ.2) THEN
            IF (TEXT(2:).EQ.' ') THEN
*              One character, write upright.
               X = BNDRY(2) - 1.0*XCH
               Y = (YW1 + YW2)/2.0
               ANGL = 0.0
            ELSE
               X = BNDRY(2) - 0.5*XCH
               Y = (YW1 + YW2)/2.0
               ANGL = 90.0
            END IF
         ELSE IF (EDGE.EQ.3) THEN
            X = (XW1 + XW2)/2.0
            Y = BNDRY(3) + 0.5*YCH
            ANGL = 0.0
            BNDRY(3) = BNDRY(3) + 1.5*YCH
         ELSE IF (EDGE.EQ.4) THEN
            IF (TEXT(2:).EQ.' ') THEN
*              One character, write upright.
               X = BNDRY(4) + 1.0*XCH
               Y = (YW1 + YW2)/2.0
               ANGL = 0.0
            ELSE
               X = BNDRY(4) + 0.5*XCH
               Y = (YW1 + YW2)/2.0
               ANGL = -90.0
            END IF
         END IF

         IF (K.EQ.0) THEN
            CALL PGPTXT (X, Y, ANGL, 0.5, TEXT)
         ELSE
*           Two-colour annotation.
            CALL PGQTXT (X, Y, ANGL, 0.5, TEXT, XBOX, YBOX)
            CALL PGPTXT (XBOX(1), YBOX(1), ANGL, 0.0, TEXT(:K))
            CALL PGSCI (CI(6))
            CALL PGPTXT (XBOX(4), YBOX(4), ANGL, 1.0, TEXT(K+1:))
         END IF

 260  CONTINUE

*     Title.
      IF (IDENTS(3).NE.' ') THEN
         CALL PGSCI (CI(7))
         X = (XW1 + XW2)/2.0
         Y = BNDRY(3) + 0.5*YCH
         CALL PGPTXT (X, Y, 0.0, 0.5, IDENTS(3))
      END IF


      RETURN
      END
