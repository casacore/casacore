*=======================================================================
*
*   PGSBOX 3.3 - a non-linear coordinate axis plotter for PGPLOT.
*   Copyright (C) 1997-2003, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify
*   it under the terms of the GNU Library General Public License as
*   published by the Free Software Foundation; either version 2 of the
*   License, or (at your option) any later version.
*
*   This library is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*   Library General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public
*   License along with this library; if not, write to the Free
*   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning PGSBOX may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta
*                      Australia Telescope National Facility, CSIRO
*                      PO Box 76
*                      Epping NSW 1710
*                      AUSTRALIA
*
*=======================================================================
*
*   PGSBOX draws and labels a curvilinear coordinate grid.  The caller
*   must provide a separate external function, NLFUNC, to define the
*   non-linear coordinate transformation.
*
*   PGLBOX, a simplified ENTRY point to PGSBOX, has been provided for
*   drawing simple linear axes without the need to specify NLFUNC.
*   PGLBOX allows simplified access to formatting control for labelling
*   world coordinate axes which is not provided by the standard PGPLOT
*   routines, PGBOX or PGTBOX.  PGLBOX uses the world coordinate range
*   set by a prior call to PGSWIN and omits the following arguments:
*
*      BLC, TRC, NLFUNC, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM
*
*   The remaining arguments are specified in the same order as PGSBOX.
*
*   Given:
*      BLC      R(2)     Cartesian coordinates of the bottom left-hand
*      TRC      R(2)     corner and top right-hand corner of the frame.
*                        Any convenient Cartesian system may be used in
*                        conjunction with the non-linear transformation
*                        function, NLFUNC, described below.
*
*                        For example, FITS images have pixel coordinate
*                        (1,1) at the centre of the pixel in the bottom
*                        left-hand corner.  Thus it would be
*                        appropriate to set BLC to (0.5,0.5) and TRC to
*                        (NAXIS1+0.5, NAXIS2+0.5).
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
*                           'Y': Modified Julian Date to be expressed as
*                                a Gregorian calendar date, YYYY/MM/DD.
*                                (MJD = JD - 2400000.5.)
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
*                              0: Let PGSBOX decide what edges to label.
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
*                        use where PGSBOX is called upon to determine a
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
*                           a) if GRID1(0) is greater than zero it
*                              defines a uniform grid spacing.
*                           b) if GRID1(0) is zero a suitable spacing
*                              will be determined (see LABDEN).
*                           c) if GRID1(0) is less than zero then no
*                              grid lines will be drawn.
*
*                        If NG1 is greater than zero, then GRID1(0) is
*                        ignored.
*
*      NG2      I        Upper array index for GRID2.
*
*      GRID2    D(0:NG2) Grid values in WORLD(2) in the same units as
*                        returned by NLFUNC, interpreted the same way
*                        as GRID1.
*
*      DOEQ     L        If NG1 = NG2 = 0, and GRID1(0) = 0D0 and/or
*                        GRID2(0) = 0D0, then choose the same grid
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
*      NC       I        Upper array index for CACHE (see note 3).
*
*      IC       I        Current number of entries in the CACHE table.
*                        Should be set to -1 on the first call to
*                        PGSBOX (see note 3).
*
*      CACHE    D(4,0:NC)
*                        Table of points where the tick marks or grid
*                        lines cross the frame (see note 3).
*                           1: Frame segment
*                              1: bottom
*                              2: left
*                              3: top
*                              4: right
*                           2: X or Y-Cartesian coordinate.
*                           3: World coordinate element (1 or 2).
*                           4: Value.
*
*                        CACHE(,0) is used to cache the extrema of the
*                        coordinate values between calls.  CACHE(1,NC-1)
*                        is also used to store related information.
*
*                        CACHE(,NC) will contain the margin widths in
*                        Cartesian coordinates when the labels are
*                        produced (i.e. the same Cartesian system used
*                        for BLC and TRC).
*
*   Returned:
*      IERR     I        Error status
*                           0: Success
*                           1: Initialization error
*                           2: Invalid coordinate system
*                           3: Cache overflow (see note 3).
*
*                        The following status returns are recognized for
*                        opcodes +2 and +1
*                          -1: Accept the returned (x,y) coordinates but
*                              do not consider this as one end of a
*                              crossing segment for labelling world
*                              coordinate 1.
*                          -2: Ditto for world coordinate 2.
*                          -3: Ditto for world coordinates 1 and 2.
*
*   Notes on PGSBOX
*   ---------------
*
*    1) Where a logarithmic world coordinate type is indicated PGSBOX
*       chooses grid lines and labels on the basis that the value
*       returned by NLFUNC is a base 10 logarithm.  PGSBOX does not
*       itself take logarithms or antilogarithms.  For example, if the
*       range of values returned by NLFUNC were 0.9 - 2.5, then PGSBOX
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
*       deduced by PGSBOX from the required density of grid lines
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
*    2) PGSBOX will attempt to handle discontinuities in angle, such as
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
*          -360 -> 0 -> ... -> 359 -> 0 -> ... -> 359
*
*       would not be handled properly.  In such cases NLFUNC should be
*       changed to return a normalized angle, or else a continuous
*       sequence.
*
*    3) PGSBOX maintains a table of axis crossings, CACHE, in which it
*       stores information used for axis labelling.  The caller need not
*       normally be concerned about the use of this table other than to
*       provide sufficient space.  Typically, NC = 256 should be enough;
*       if not, IERR = 3 will be returned.
*
*       However, a coordinate grid may be produced via multiple calls to
*       PGSBOX with deferment of axis labelling.  This might be done to
*       change the pen colour and/or thickness for different sets of
*       grid lines.  The table accumulates information from each
*       successive call until the labels are produced.  The table index,
*       IC, may be reset to zero by the caller to discard the
*       information collected.
*
*       The extrema of the world coordinate elements are stored in
*       CACHE(,0).  When a coordinate grid is plotted with multiple
*       calls to PGSBOX the initial call should always have IC set to -1
*       to signal that PGSBOX needs to determine the extrema.  On
*       subsequent calls with IC non-negative PGSBOX uses the extrema
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
*                           +2: Compute a set of Cartesian coordinates
*                               which describe a path between this and
*                               the previous pair of world coordinates
*                               remembered from the last call with
*                               OPCODE = +1 or +2.  Usually only takes
*                               a single step unless traversing a
*                               discontinuity or some other
*                               irregularity (see explanation below).
*                           +1: Compute Cartesian coordinates from world
*                               coordinates.
*                            0: Initialize.
*                           -1: Compute world coordinates from Cartesian
*                               coordinates.
*
*                        N.B. NLFUNC must not change the input
*                        coordinates; that is the world coordinates for
*                        OPCODEs = +2 and +1, or the Cartesian
*                        coordinates for OPCODE = -1.
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
*      XY       D(2)     Cartesian coordinates.  Given if OPCODE < 0,
*                        returned if OPCODE > 0.
*
*      CONTRL   I        Control flag for OPCODE = +2:
*                           0: Normal state.
*                           1: Force PGSBOX to flush its plotting buffer
*                              and call NLFUNC again with the same world
*                              coordinates.
*                           2: Force PGSBOX to call NLFUNC again with
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
*                           3: Invalid Cartesian coordinate.
*
*   PGSBOX passes its NLCPRM, NLIPRM, and NLDPRM adjustable size array
*   arguments of length NLC, NLI, and NLD to NLFUNC without
*   modification.  Comments within NLFUNC should specify the parameters
*   it wants passed to it via these arrays.
*
*   PGSBOX first calls NLFUNC with OPCODE = 0 to cause it to initialize
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
*   The CONTRL argument is provided so that NLFUNC can force PGSBOX to
*   call it again with or without flushing its plotting buffer.  This
*   may be needed when plotting a grid line through a discontinuity.
*   PGSBOX does not modify CONTRL.
*
*   Notes on NLFUNC
*   ---------------
*    1) NLFUNC must not change the input coordinates; that is the world
*       coordinates for OPCODEs = +1 and +2, or the Cartesian coordinates
*       for OPCODE = -1.
*
*    2) NLFUNC must define a single-valued function, that is, each
*       Cartesian coordinate (x,y) must map to a unique world coordinate
*       pair (xi,eta).
*
*    3) Notwithstanding the fact that PGSBOX declares NLCPRM, NLIPRM,
*       and NLDPRM as single dimension arrays of length NLC, NLI, and
*       NLD, NLFUNC may treat these as higher-dimensional arrays, for
*       example, NLDPRM(2,NLD).  (The FORTRAN standard requires that
*       only the last dimension is adjustable.)
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=======================================================================
      SUBROUTINE PGSBOX (BLC_, TRC_, IDENTS, OPT, LABCTL, LABDEN, CI,
     :   GCODE, TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI,
     :   NLD, NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)
*-----------------------------------------------------------------------
      INTEGER   BUFSIZ
      PARAMETER (BUFSIZ = 2048)

      LOGICAL   DOEDGE, DOEQ, DOLBOX, FULLSM, GETEND, INSIDE, ISANGL(2),
     :          LABLOK, MAJOR, OVERFL, PREVIN
      INTEGER   CI(7), CI0, CJ(7), CONTRL, DENS(2), FSEG, GCODE(2), IC,
     :          ID, IERR, IM, ISTEP, IW0, IWJ, IWK, IX, IY, IYPREV, J,
     :          K, KX, L, LABCTL, LABDEN, LDIV(2), LTABL(6,2:6), NC,
     :          NG(2), NG1, NG2, NLC, NLD, NLI, NLIPRM(NLI), NP,
     :          NSTEP(2), NWJ, NX, NY, TCODE(2,4)
      REAL      BLC(2), BLC_(2), S, TRC(2), TRC_(2), WXY(4), X1, X2,
     :          XPOINT, XR(BUFSIZ), XSCL, XSPAN, XTOL, XVP1, XVP2,
     :          Y1, Y2, YR(BUFSIZ), YSCL, YSPAN, YTOL, YVP1, YVP2
      DOUBLE PRECISION CONTXT(20), CACHE(4,0:NC), DW(2), DX, DY, FACT,
     :          G0(2), GSTEP(2), GRID1(0:NG1), GRID2(0:NG2),
     :          NLDPRM(NLD), STEP, SW(2), TIKLEN, TMP, VMAX(2,2),
     :          VMIN(2,2), W1PREV, W1X0, W2PREV, W2X0, WJUMP, WMAX(2),
     :          WMIN(2), WORLD(2), XY(2)
      CHARACTER FTYPE(2), IDENTS(3)*(*), NLCPRM(NLC)*1, OPT(2)*(*)

      EXTERNAL NLFUNC

*     Approximate number of grid lines for each coordinate.
      INTEGER DENS0
      PARAMETER (DENS0 = 8)

*     Double precision round-off tolerance.
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 1D-8)

*     Number of steps per grid line.
      DATA NSTEP /80, 80/

*     Table of logarithmic grid values.
      DATA LTABL /3, 10,  0,  0,  0,  0,
     :            2,  5, 10,  0,  0,  0,
     :            2,  3,  5, 10,  0,  0,
     :            2,  3,  5,  7, 10,  0,
     :            2,  3,  4,  5,  7, 10/
*-----------------------------------------------------------------------
*  Initialize.
      DOLBOX = .FALSE.

      CALL NLFUNC (0, NLC, NLI, NLD, NLCPRM, NLIPRM, NLDPRM, WORLD,
     :   XY, CONTRL, CONTXT, IERR)
*     Quick return for now.
      IF (IERR.NE.0) THEN
         IERR = 1
         RETURN
      END IF

      BLC(1) = BLC_(1)
      BLC(2) = BLC_(2)
      TRC(1) = TRC_(1)
      TRC(2) = TRC_(2)

      DOEDGE = GCODE(1).NE.2 .AND. GCODE(2).NE.2

*-----------------------------------------------------------------------
      GO TO 10

      ENTRY PGLBOX (IDENTS, OPT, LABCTL, LABDEN, CI, GCODE, TIKLEN, NG1,
     :   GRID1, NG2, GRID2, DOEQ, NC, IC, CACHE, IERR)

      DOLBOX = .TRUE.

      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1.0
      TRC(2) = 1.0

      DOEDGE = .TRUE.

      CONTRL = 0
      IERR = 0

 10   CONTINUE

*-----------------------------------------------------------------------
      IF (NC.LT.1) THEN
         IERR = 3
         RETURN
      END IF

      NG(1) = NG1
      NG(2) = NG2

      FTYPE(1) = OPT(1)(1:1)
      FTYPE(2) = OPT(2)(1:1)

*     Extend the PGPLOT window and rescale it.
      CALL PGQVP (0, XVP1, XVP2, YVP1, YVP2)
      CALL PGQWIN (WXY(1), WXY(2), WXY(3), WXY(4))
      CALL PGSVP (0.0, 1.0, 0.0, 1.0)
      XSCL = (TRC(1)-BLC(1))/(XVP2-XVP1)
      YSCL = (TRC(2)-BLC(2))/(YVP2-YVP1)
      CALL PGSWIN (BLC(1)-XSCL*XVP1, TRC(1)+XSCL*(1.0-XVP2),
     :             BLC(2)-YSCL*YVP1, TRC(2)+YSCL*(1.0-YVP2))

*     Labels only?
      IF (LABCTL.GE.10000) GO TO 130


      XSPAN = WXY(2) - WXY(1)
      YSPAN = WXY(4) - WXY(3)
      XTOL  = XSPAN*TOL
      YTOL  = YSPAN*TOL

*  Find world coordinate ranges.
      FULLSM = .FALSE.
      IF (IC.GE.0 .AND. IC.LT.NC-1) FULLSM = CACHE(1,NC-1).EQ.1D0

      IF (IC.GE.0 .AND. (FULLSM .OR. DOEDGE)) THEN
*        Use extrema cached from a previous call.
         WMIN(1) = CACHE(1,0)
         WMAX(1) = CACHE(2,0)
         WMIN(2) = CACHE(3,0)
         WMAX(2) = CACHE(4,0)

      ELSE
*        Do a coarse search to find approximate ranges.
         WMIN(1) =  1D99
         WMAX(1) = -1D99
         WMIN(2) =  1D99
         WMAX(2) = -1D99

*        Need to consider cycles in angle through 360 degrees.
         ISANGL(1) = INDEX('ABCDEFGHI',FTYPE(1)).NE.0
         ISANGL(2) = INDEX('ABCDEFGHI',FTYPE(2)).NE.0
         VMIN(1,1) =  1D99
         VMIN(1,2) =  1D99
         VMAX(1,1) = -1D99
         VMAX(1,2) = -1D99
         VMIN(2,1) =  1D99
         VMIN(2,2) =  1D99
         VMAX(2,1) = -1D99
         VMAX(2,2) = -1D99

*        Sample coordinates on a 50 x 50 grid.
         NX = 50
         NY = 50
         DX = DBLE(TRC(1)-BLC(1))/NX
         DY = DBLE(TRC(2)-BLC(2))/NY

         K = 0
         IYPREV = -1
         DO 30 IY = 0, NY
            XY(2) = BLC(2) + IY*DY

*           Sample the edges only?
            KX = 1
            IF (DOEDGE) THEN
               IF (IY.NE.0 .AND. IY.NE.NY) KX = NX
            END IF

            DO 20 IX = 0, NX, KX
               XY(1) = BLC(1) + IX*DX

               IF (DOLBOX) THEN
                  WORLD(1) = WXY(1) + XY(1)*XSPAN
                  WORLD(2) = WXY(3) + XY(2)*YSPAN
               ELSE
                  CALL NLFUNC (-1, NLC, NLI, NLD, NLCPRM, NLIPRM,
     :               NLDPRM, WORLD, XY, CONTRL, CONTXT, IERR)
               END IF

               IF (IERR.EQ.0) THEN
                  K = K + 1

                  IF (ISANGL(1)) THEN
                     IF (K.EQ.1) W1X0 = WORLD(1)

                     IF (IY.NE.IYPREV) THEN
                        W1PREV = W1X0
                        W1X0 = WORLD(1)
                     END IF

*                    Iron out jumps.
                     WJUMP = WORLD(1) - W1PREV
                     IF (ABS(WJUMP).GT.180D0) THEN
                        WJUMP = WJUMP + SIGN(WJUMP,180D0)
                        WJUMP = 360D0*INT(WJUMP/360D0)
                        WORLD(1) = WORLD(1) - WJUMP
                     END IF

                     W1PREV = WORLD(1)
                     IYPREV = IY
                  END IF

                  IF (ISANGL(2)) THEN
                     IF (K.EQ.1) W2X0 = WORLD(2)

                     IF (IY.NE.IYPREV) THEN
                        W2PREV = W2X0
                        W2X0 = WORLD(2)
                     END IF

*                    Iron out jumps.
                     WJUMP = WORLD(2) - W2PREV
                     IF (ABS(WJUMP).GT.180D0) THEN
                        WJUMP = WJUMP + SIGN(WJUMP,180D0)
                        WJUMP = 360D0*INT(WJUMP/360D0)
                        WORLD(2) = WORLD(2) - WJUMP
                     END IF

                     W2PREV = WORLD(2)
                     IYPREV = IY
                  END IF

                  IF (WORLD(1).LT.WMIN(1)) WMIN(1) = WORLD(1)
                  IF (WORLD(1).GT.WMAX(1)) WMAX(1) = WORLD(1)
                  IF (WORLD(2).LT.WMIN(2)) WMIN(2) = WORLD(2)
                  IF (WORLD(2).GT.WMAX(2)) WMAX(2) = WORLD(2)

                  IF (ISANGL(1)) THEN
*                    Normalize to the range [0,360).
                     WORLD(1) = MOD(WORLD(1), 360D0)
                     IF (WORLD(1).LT.0D0) WORLD(1) = WORLD(1) + 360D0
                     IF (WORLD(1).LT.VMIN(1,1)) VMIN(1,1) = WORLD(1)
                     IF (WORLD(1).GT.VMAX(1,1)) VMAX(1,1) = WORLD(1)

*                    Normalize to the range (-180,180].
                     IF (WORLD(1).GT.180D0) WORLD(1) = WORLD(1) - 360D0
                     IF (WORLD(1).LT.VMIN(1,2)) VMIN(1,2) = WORLD(1)
                     IF (WORLD(1).GT.VMAX(1,2)) VMAX(1,2) = WORLD(1)
                  END IF

                  IF (ISANGL(2)) THEN
*                    Normalize to the range [0,360).
                     WORLD(2) = MOD(WORLD(2), 360D0)
                     IF (WORLD(2).LT.0D0) WORLD(2) = WORLD(2) + 360D0
                     IF (WORLD(2).LT.VMIN(2,1)) VMIN(2,1) = WORLD(2)
                     IF (WORLD(2).GT.VMAX(2,1)) VMAX(2,1) = WORLD(2)

*                    Normalize to the range (-180,180].
                     IF (WORLD(2).GT.180D0) WORLD(2) = WORLD(2) - 360D0
                     IF (WORLD(2).LT.VMIN(2,2)) VMIN(2,2) = WORLD(2)
                     IF (WORLD(2).GT.VMAX(2,2)) VMAX(2,2) = WORLD(2)
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE

         IF (K.EQ.0) THEN
*           No valid coordinates found within the frame.
            IERR = 2
            GO TO 999
         END IF

*        Check for cycles in angle.
         DO 40 J = 1, 2
            IF (ISANGL(J)) THEN
               IF (WMAX(J)-WMIN(J).LT.360D0 .AND.
     :             WMAX(J)-WMIN(J).GT.VMAX(J,1)-VMIN(J,1)+TOL) THEN
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
     :             WMAX(J)-WMIN(J).GT.VMAX(J,2)-VMIN(J,2)+TOL) THEN
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
 40      CONTINUE

*        Cache extrema for subsequent calls.
         CACHE(1,0) = WMIN(1)
         CACHE(2,0) = WMAX(1)
         CACHE(3,0) = WMIN(2)
         CACHE(4,0) = WMAX(2)

*        Was full sampling done?
         IF (DOEDGE) THEN
            CACHE(1,NC-1) = 0D0
         ELSE
            CACHE(1,NC-1) = 1D0
         END IF

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
      DO 50 J = 1, 2
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

            FACT = 1D0
            IF (INDEX('GHI',FTYPE(J)).NE.0) THEN
*              Rescale degrees to hours.
               FACT = 1D0/15D0
               STEP = STEP*FACT
            ELSE IF (FTYPE(J).EQ.'Y' .AND. STEP.LT.0.5D0) THEN
*              Calendar increment of less than 12h; use time format.
               FTYPE(J) = 'y'

*              Rescale days to hours.
               FACT = 24D0
               STEP = STEP*FACT
            END IF

            IF (INDEX('ABCDEF',FTYPE(J)).NE.0 .AND. STEP.GE.1D0) THEN
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

            ELSE IF (INDEX('GHITy',FTYPE(J)).NE.0 .AND.
     :         STEP.GE.1D0) THEN
*              Angle or time in hms format with multi-hour increment.
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

            ELSE IF (INDEX('DEFGHITy',FTYPE(J)).NE.0 .AND.
     :         STEP.LT.1D0) THEN
*              Angle or time in sexagesimal format with sub-degree/hour
*              increment.
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

            ELSE IF (FTYPE(J).EQ.'Y') THEN
*              Calendar axis: use coded steps.
               IF (STEP.LT.15D0) THEN
*                 Timespan of a few months; use multi-day increments.
                  STEP = ANINT(STEP)
                  IF (STEP.LT.1D0) THEN
                     STEP = 1D0
                  ELSE IF (STEP.GT.9D0) THEN
*                    Fortnightly.
                     STEP = 14D0
                  ELSE IF (STEP.GT.4D0) THEN
*                    Weekly.
                     STEP = 7D0
                  END IF

               ELSE IF (STEP.LT.270D0) THEN
*                 Timespan of a few years; use multi-month increments.
                  STEP = ANINT(STEP/30.44D0)
                  IF (STEP.LT.1.5D0) THEN
                     STEP = 1D0
                  ELSE IF (STEP.LT.2.5D0) THEN
                     STEP = 2D0
                  ELSE IF (STEP.LT.3.5D0) THEN
                     STEP = 3D0
                  ELSE IF (STEP.LT.4.5D0) THEN
                     STEP = 4D0
                  ELSE
                     STEP = 6D0
                  END IF

*                 Coding for multi-month increments.
                  STEP = 100D0*STEP

               ELSE
*                 Multi-year increments.
                  STEP = ANINT(DW(J)/DENS(J)/365.25D0)
                  IF (STEP.LT.1D0) THEN
                     STEP = 1D0
                  ELSE
                     TMP = 10D0**INT(LOG10(STEP))

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
                  END IF

*                 Coding for multi-year increments.
                  STEP = 10000D0*STEP
               END IF

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
               IF (FTYPE(J).EQ.'L') THEN
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
 50   CONTINUE

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
      DO 60 J = 1, 2
         IF (FTYPE(J).EQ.'L') THEN
            WMIN(J) = AINT(WMIN(J)-1D0)
            WMAX(J) = AINT(WMAX(J)+1D0)
         ELSE IF (FTYPE(J).EQ.'Y') THEN
*           Calendar axis.
            IF (GSTEP(J).LT.100D0) THEN
*              Daily increments.
               WMIN(J) = AINT(WMIN(J))
               WMAX(J) = AINT(WMAX(J)+1D0)
            ELSE
*              Start on Jan/01.
               CALL PGMJD (0, WMIN(J), IY, IM, ID)
               CALL PGMJD (1, WMIN(J), IY, 1, 1)

               CALL PGMJD (0, WMAX(J), IY, IM, ID)
               IF (GSTEP(J).LT.10000D0) THEN
*                 Monthly increments.
                  CALL PGMJD (1, WMAX(J), IY, 12, 1)
               ELSE
*                 Yearly increments.
                  CALL PGMJD (1, WMAX(J), IY+1, 1, 1)
               END IF
            END IF

         ELSE
            TMP = AINT(WMIN(J)/GSTEP(J))*GSTEP(J)
            IF (TMP.GE.WMIN(J)) TMP = TMP - GSTEP(J)
            WMIN(J) = TMP
            TMP = AINT(WMAX(J)/GSTEP(J))*GSTEP(J)
            IF (TMP.LE.WMAX(J)) TMP = TMP + GSTEP(J)
            WMAX(J) = TMP
         END IF

         DW(J) = WMAX(J) - WMIN(J)
         SW(J) = DW(J)/NSTEP(J)

*        Adjust NSTEP so that SW divides GSTEP.
         IF (SW(J).LT.GSTEP(J)) THEN
            SW(J) = GSTEP(J)/ANINT(GSTEP(J)/SW(J))
         ELSE
            SW(J) = GSTEP(J)
         END IF
         NSTEP(J) = ANINT(DW(J)/SW(J))
 60   CONTINUE


*  Draw the grid.
*     Get absolute scale for tick marks.
      CALL PGQVP (2, X1, X2, Y1, Y2)
      XSCL = (X2-X1)/(TRC(1)-BLC(1))
      YSCL = (Y2-Y1)/(TRC(2)-BLC(2))

*     Decode tick mark control.
      DO 70 J = 1, 2
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
 70   CONTINUE

*     Determine initial colour.
      CALL PGQCI (CI0)
      DO 80 J = 1, 7
         IF (CI(J).GE.0) THEN
            CJ(J) = CI(J)
         ELSE
            CJ(J) = CI0
         END IF
 80   CONTINUE

*     Draw each set of grid lines.
      OVERFL = .FALSE.
      DO 120 J = 1, 2
         IF (GCODE(J).EQ.0) GO TO 120

         IF (J.EQ.1) THEN
            CALL PGSCI (CJ(1))
            K = 2
         ELSE
            CALL PGSCI (CJ(2))
            K = 1
         END IF

         IF (NG(J).GT.0) THEN
            NWJ = NG(J)

         ELSE IF (FTYPE(J).EQ.'Y' .AND. GSTEP(J).GE.100D0) THEN
*           Calendar axis.
            CALL PGMJD (0, WMAX(J), IY, IM, ID)
            IF (GSTEP(J).LT.10000D0) THEN
               NWJ = 12*IY + IM
               CALL PGMJD (0, WMIN(J), IY, IM, ID)
               NWJ = (NWJ - (12*IY + IM))/INT(GSTEP(J)/100D0)
            ELSE
               NWJ = IY
               CALL PGMJD (0, WMIN(J), IY, IM, ID)
               NWJ = (NWJ - IY)/INT(GSTEP(J)/10000D0)
            END IF

         ELSE
            NWJ = NINT(DW(J)/GSTEP(J))
            IW0 = NINT(WMIN(J)/GSTEP(J))
         END IF

         DO 110 IWJ = 0, NWJ
            MAJOR = .FALSE.

*           Determine the world coordinate of the grid line.
            IF (NG(J).GT.0) THEN
*              User-specified.
               IF (IWJ.EQ.0) GO TO 110
               WORLD(1) = GRID1(IWJ)
               WORLD(2) = GRID2(IWJ)
            ELSE
*              Internally computed.
               IF (FTYPE(J).EQ.'Y' .AND. GSTEP(J).GE.100D0) THEN
*                 Calendar axis.
                  CALL PGMJD (0, WMIN(J), IY, IM, ID)
                  IF (GSTEP(J).LT.10000D0) THEN
                     IM = IM + IWJ*INT(GSTEP(J)/100D0)
                     CALL PGMJD (1, WORLD(J), IY, IM, ID)
                  ELSE
                     IY = IY + IWJ*INT(GSTEP(J)/10000D0)
                     CALL PGMJD (1, WORLD(J), IY, IM, ID)
                  END IF

               ELSE
                  WORLD(J) = (IW0 + IWJ)*GSTEP(J)

*                 Logarithmic?
                  IF (FTYPE(J).EQ.'L') THEN
                     TMP = MOD(WORLD(J),1D0)
                     IF (TMP.LT.0D0) TMP = TMP + 1D0
                     L = NINT(TMP*LDIV(J))

                     IF (L.EQ.0) THEN
*                       Major tick mark.
                        MAJOR = .TRUE.
                     ELSE
*                       Adjust logarithmic scales.
                        IF (LDIV(J).LE.6) THEN
                           L = LTABL(L,LDIV(J))
                        ELSE
                           L = L + 1
                        END IF

                        WORLD(J) = WORLD(J) - TMP + LOG10(DBLE(L))
                     END IF
                  END IF
               END IF
            END IF

            NP = 0
            GETEND = .TRUE.
            DO 100 IWK = 0, NSTEP(K)
               WORLD(K) = WMIN(K) + IWK*SW(K)

               IF (GETEND) THEN
*                 Get end-point context.
                  IF (DOLBOX) THEN
                     XY(1) = (WORLD(1) - WXY(1))/XSPAN
                     XY(2) = (WORLD(2) - WXY(3))/YSPAN
                  ELSE
                     CALL NLFUNC (1, NLC, NLI, NLD, NLCPRM, NLIPRM,
     :                  NLDPRM, WORLD, XY, CONTRL, CONTXT, IERR)
                  END IF

                  IF (IERR.LE.0) THEN
                     X1 = REAL(XY(1))
                     Y1 = REAL(XY(2))
                     INSIDE = X1.GT.BLC(1) .AND. X1.LT.TRC(1) .AND.
     :                        Y1.GT.BLC(2) .AND. Y1.LT.TRC(2)

                     NP = 1
                     XR(1) = X1
                     YR(1) = Y1

                     PREVIN = INSIDE
                     GETEND = .FALSE.

                     LABLOK = IERR.NE.-J .AND. IERR.NE.-3
                  END IF
                  GO TO 100
               END IF

               DO 90 ISTEP = 1, 1000
                  IF (DOLBOX) THEN
                     XY(1) = (WORLD(1) - WXY(1))/XSPAN
                     XY(2) = (WORLD(2) - WXY(3))/YSPAN
                  ELSE
                     CALL NLFUNC (2, NLC, NLI, NLD, NLCPRM, NLIPRM,
     :                  NLDPRM, WORLD, XY, CONTRL, CONTXT, IERR)
                  END IF

                  IF (IERR.GT.0) THEN
*                    Flush buffer.
                     IF (NP.GT.0) CALL PGLINE(NP, XR, YR)
                     GO TO 110
                  END IF

                  IF (NP.EQ.BUFSIZ) THEN
*                    Recycle buffer.
                     CALL PGLINE(NP, XR, YR)
                     XR(1) = XR(NP)
                     YR(1) = YR(NP)
                     NP = 1
                  END IF

                  X2 = REAL(XY(1))
                  Y2 = REAL(XY(2))
                  INSIDE = X2.GT.BLC(1) .AND. X2.LT.TRC(1) .AND.
     :                     Y2.GT.BLC(2) .AND. Y2.LT.TRC(2)

                  IF (.NOT.INSIDE) THEN
*                    For tick marks at the left or right edge.
                     IF ((X2.EQ.BLC(1) .OR.  X2.EQ.TRC(1)) .AND.
     :                    Y2.GT.BLC(2) .AND. Y2.LT.TRC(2)) THEN
                        INSIDE = X2.EQ.XR(NP)
                     END IF
                  END IF

                  IF (.NOT.INSIDE) THEN
*                    For tick marks at the bottom or top edge.
                     IF ((Y2.EQ.BLC(2) .OR.  Y2.EQ.TRC(2)) .AND.
     :                    X2.GT.BLC(1) .AND. X2.LT.TRC(1)) THEN
                        INSIDE = Y2.EQ.YR(NP)
                     END IF
                  END IF

                  IF (NP.EQ.0) THEN
                     NP = 1
                     XR(1) = X2
                     YR(1) = Y2
                  ELSE
                     IF (INSIDE) THEN
*                       This point is inside the frame...
                        IF (.NOT.PREVIN) THEN
*                          ...but the previous one was outside.
                           X1 = XR(NP)
                           Y1 = YR(NP)

                           FSEG = 0
                           IF (ABS(X2-X1).GT.XTOL) THEN
                              S = (Y2-Y1)/(X2-X1)
                              IF (XR(NP).LE.BLC(1)) THEN
                                 FSEG = 2
                                 XR(NP) = BLC(1)
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              ELSE IF (XR(NP).GE.TRC(1)) THEN
                                 FSEG = 4
                                 XR(NP) = TRC(1)
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              END IF
                           END IF

                           IF (ABS(Y2-Y1).GT.YTOL) THEN
                              S = (X2-X1)/(Y2-Y1)
                              IF (YR(NP).LE.BLC(2)) THEN
                                 FSEG = 1
                                 YR(NP) = BLC(2)
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              ELSE IF (YR(NP).GE.TRC(2)) THEN
                                 FSEG = 3
                                 YR(NP) = TRC(2)
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              END IF
                           END IF


                           IF (FSEG.EQ.0) THEN
*                             The crossing is too oblique.
                              INSIDE = .FALSE.

                           ELSE
*                             Record this crossing point.
                              IF (TCODE(J,FSEG).NE.0 .AND. LABLOK) THEN
                                 IF (IC.LT.NC-1) THEN
                                    IC = IC + 1
                                    CACHE(1,IC) = FSEG
                                    CACHE(2,IC) = XPOINT
                                    CACHE(3,IC) = J
                                    CACHE(4,IC) = WORLD(J)
                                 ELSE
*                                   Cache overflow.
                                    OVERFL = .TRUE.
                                 END IF
                              END IF

                              IF (TCODE(J,FSEG).GT.0) THEN
*                                Just want tick marks.
*                                 X1 = XR(NP)
*                                 Y1 = YR(NP)
                                 S = (XSCL*(X2-X1))**2 +
     :                               (YSCL*(Y2-Y1))**2
                                 S = SQRT(S)/TCODE(J,FSEG)
                                 IF (MAJOR) S = S/1.5
                                 NP = NP + 1
                                 XR(NP) = XR(NP-1) + (X2-X1)*TIKLEN/S
                                 YR(NP) = YR(NP-1) + (Y2-Y1)*TIKLEN/S

                                 CALL PGLINE(NP, XR, YR)
                                 NP = 1
                              END IF
                           END IF
                        END IF

                        IF (INSIDE .AND. GCODE(J).EQ.2) THEN
*                          Full grid.
                           NP = NP + 1
                        END IF
                        XR(NP) = X2
                        YR(NP) = Y2
                     ELSE
*                       This point is outside the frame...
                        IF (PREVIN) THEN
*                          ...but the previous one was inside.
                           X1 = XR(NP)
                           Y1 = YR(NP)

                           NP = NP + 1
                           XR(NP) = X2
                           YR(NP) = Y2

                           FSEG = 0
                           IF (ABS(X2-X1).GT.XTOL) THEN
                              S = (Y2-Y1)/(X2-X1)
                              IF (XR(NP).LE.BLC(1)) THEN
                                 FSEG = 2
                                 XR(NP) = BLC(1)
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              ELSE IF (XR(NP).GE.TRC(1)) THEN
                                 FSEG = 4
                                 XR(NP) = TRC(1)
                                 XPOINT = Y1 + (XR(NP) - X1)*S
                                 YR(NP) = XPOINT
                              END IF
                           END IF

                           IF (ABS(Y2-Y1).GT.YTOL) THEN
                              S = (X2-X1)/(Y2-Y1)
                              IF (YR(NP).LE.BLC(2)) THEN
                                 FSEG = 1
                                 YR(NP) = BLC(2)
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              ELSE IF (YR(NP).GE.TRC(2)) THEN
                                 FSEG = 3
                                 YR(NP) = TRC(2)
                                 XPOINT = X1 + (YR(NP) - Y1)*S
                                 XR(NP) = XPOINT
                              END IF
                           END IF


                           IF (FSEG.EQ.0) THEN
*                             The crossing is too oblique.
                              INSIDE = .TRUE.

                              IF (GCODE(J).EQ.2) THEN
*                                Full grid.
                                 NP = NP + 1
                              END IF
                              XR(NP) = X2
                              YR(NP) = Y2

                           ELSE
*                             Record this crossing point.
                              IF (TCODE(J,FSEG).NE.0 .AND. LABLOK) THEN
                                 IF (IC.LT.NC-1) THEN
                                    IC = IC + 1
                                    CACHE(1,IC) = FSEG
                                    CACHE(2,IC) = XPOINT
                                    CACHE(3,IC) = J
                                    CACHE(4,IC) = WORLD(J)
                                 ELSE
*                                   Cache overflow.
                                    OVERFL = .TRUE.
                                 END IF
                              END IF

                              IF (TCODE(J,FSEG).GT.0) THEN
*                                Just want tick marks.
                                 X1 = XR(NP)
                                 Y1 = YR(NP)
                                 X2 = XR(NP-1)
                                 Y2 = YR(NP-1)
                                 S = (XSCL*(X2-X1))**2 +
     :                               (YSCL*(Y2-Y1))**2
                                 S = SQRT(S)/TCODE(J,FSEG)
                                 IF (MAJOR) S = S/1.5
                                 XR(NP-1) = X1 + (X2-X1)*TIKLEN/S
                                 YR(NP-1) = Y1 + (Y2-Y1)*TIKLEN/S
                              END IF

*                             Flush buffer.
                              IF (TCODE(J,FSEG).NE.0) THEN
                                 CALL PGLINE(NP, XR, YR)
                              END IF
                              NP = 0
                           END IF
                        ELSE
*                          The previous point was also outside.
                           XR(NP) = X2
                           YR(NP) = Y2
                        END IF
                     END IF
                  END IF

                  PREVIN = INSIDE
                  LABLOK = IERR.NE.-J .AND. IERR.NE.-3

                  IF (CONTRL.EQ.0) THEN
                     GO TO 100
                  ELSE IF (CONTRL.EQ.1) THEN
*                    Flush buffer.
                     IF (NP.GT.1) CALL PGLINE(NP, XR, YR)
                     NP = 0
                  END IF
 90            CONTINUE
 100        CONTINUE

            IF (NP.GT.1) CALL PGLINE(NP, XR, YR)
 110     CONTINUE
 120  CONTINUE


*  Produce axis labels.
 130  IF (LABCTL.NE.-1) CALL PGCRLB (BLC, TRC, IDENTS, FTYPE, LABCTL,
     :   CJ, NC, IC, CACHE)


*  Clean up.
      IF (OVERFL) IERR = 3

*     Restore the original viewport, window and pen colour.
 999  CALL PGSVP (XVP1, XVP2, YVP1, YVP2)
      CALL PGSWIN (WXY(1), WXY(2), WXY(3), WXY(4))
      CALL PGSCI (CI0)

      RETURN
      END



*=======================================================================
*
*   PGCRLB is a helper routine for PGSBOX, not meant to be called
*   directly since it expects the viewport and window to be scaled to
*   the full extent; it labels a curvilinear coordinate grid.
*
*   Given:
*      BLC      R(2)     Cartesian coordinates of the bottom left-hand
*                        corner.
*      TRC      R(2)     Cartesian coordinates of the top right-hand
*                        corner.
*
*      IDENTS   C(3)*(*) Identification strings (see PGSBOX).
*
*      FTYPE    C(2)*1   Axis types, used for axis labelling (see
*                        PGSBOX).
*
*      LABCTL   I        Decimal encoded grid labelling control (see
*                        PGSBOX).
*
*      CI       I(7)     Colour table (see PGSBOX).
*
*   Given and/or returned:
*      NC       I        Upper array index for CACHE.
*
*      IC       I        Current number of entries in the CACHE table.
*
*      CACHE    D(4,0:NC)
*                        Table of points where the tick marks or grid
*                        lines cross the frame (see PGSBOX).
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*=======================================================================
      SUBROUTINE PGCRLB (BLC, TRC, IDENTS, FTYPE, LABCTL, CI, NC, IC,
     :                   CACHE)
*-----------------------------------------------------------------------
      LOGICAL   ANGLE, DODEG, DOMIN, DOYEAR, LFORCE, SEXA(2), TICKIT
      INTEGER   CI(7), EDGE, IC, ID, ID2, IM, IM2, IMAG(2), ITER, IWRLD,
     :          IY, IY2, J, JC, K, K1, K2, KWRLD, L, LABCTL, LD, LM,
     :          LMAG(2), LS, LV, M, M1, M2, MM, NC, NCH, NI(2,0:4),
     :          NSWAP, PP, PRVDEG(2), PRVMIN(2), PRVEDG, PRVYR(2),
     :          SEXSUP(2), SKIP(4), SKOP(4)
      REAL      ANGL, BLC(2), FJUST, BNDRY(4), OMAG(2), SI(2), TRC(2),
     :          X, XBOX(4), XCH, XL, XW1, XW2, Y, YCH, YBOX(4), YL, YW1,
     :          YW2, Z
      DOUBLE PRECISION CACHE(4,0:NC), MJD1(2), MJD2(2), TMP, VS
      CHARACTER ESCAPE*1, EXPONT*20, FMT*8, IDENTS(3)*(*), TEXT*80,
     :          FTYPE(2)*1, TXT(2)*80

      DATA ESCAPE /'\\'/
*-----------------------------------------------------------------------
*  Normalize angular table entries.
      IF (INDEX('ABDEGH',FTYPE(1)).NE.0 .OR.
     :    INDEX('ABDEGH',FTYPE(2)).NE.0) THEN
         DO 10 J = 1, IC
            IWRLD = NINT(CACHE(3,J))

            IF (INDEX('ADG', FTYPE(IWRLD)).NE.0) THEN
               CACHE(4,J) = MOD(CACHE(4,J), 360D0)
               IF (CACHE(4,J).LT.0D0) CACHE(4,J) = CACHE(4,J) + 360D0
            ELSE IF (INDEX('BEH', FTYPE(IWRLD)).NE.0) THEN
               CACHE(4,J) = MOD(CACHE(4,J), 360D0)
               IF (CACHE(4,J).LE.-180D0) THEN
                  CACHE(4,J) = CACHE(4,J) + 360D0
               ELSE IF (CACHE(4,J).GT.180D0) THEN
                  CACHE(4,J) = CACHE(4,J) - 360D0
               END IF
            END IF

            IF (INDEX('GHI', FTYPE(IWRLD)).NE.0) THEN
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
     :          CACHE(2,J).LE.CACHE(2,J+1)) GO TO 30

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
         IWRLD = NINT(CACHE(3,J))
         EDGE  = NINT(CACHE(1,J))

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

      IF (FTYPE(1).EQ.' ' .OR. FTYPE(2).EQ.' ') THEN
         OMAG(1) = 0.0
         OMAG(2) = 0.0
         DO 120 J = 1, IC
            IWRLD = NINT(CACHE(3,J))
            IF (FTYPE(IWRLD).EQ.' ') THEN
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
               IWRLD = NINT(CACHE(3,J))
               CACHE(4,J) = CACHE(4,J)/10D0**IMAG(IWRLD)
 130        CONTINUE
         END IF
      END IF

*     Sexagesimal labelling.
      SEXA(1) = INDEX('DEFGHITy', FTYPE(1)).NE.0
      SEXA(2) = INDEX('DEFGHITy', FTYPE(2)).NE.0

      IF (SEXA(1) .OR. SEXA(2)) THEN
         LMAG(1) = -2
         LMAG(2) = -2

         DO 150 J = 1, IC
*           Skip non-sexagesimal coordinates.
            IWRLD = NINT(CACHE(3,J))
            IF (.NOT.SEXA(IWRLD)) GO TO 150

*           Defeat rounding errors.
            IF (FTYPE(IWRLD).EQ.'y') THEN
               TMP = ABS(MOD(CACHE(4,J),1D0)*86400D0) + 5D-7
            ELSE
               TMP = ABS(CACHE(4,J)*3600D0) + 5D-7
            END IF

            LS = INT(TMP)
            LV = INT(MOD(TMP, 1D0)*1D6)
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
      XW1 = BLC(1)
      XW2 = TRC(1)
      YW1 = BLC(2)
      YW2 = TRC(2)

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

*     Calendar date range.
      MJD1(1) =  1D99
      MJD1(2) =  1D99
      MJD2(1) = -1D99
      MJD2(2) = -1D99

*     Character height.
      CALL PGQCS (4, XCH, YCH)
      PRVEDG = 0

*     Loop through the axis crossing table.
      DO 230 J = 1, IC
*        Determine the position.
         IWRLD = NINT(CACHE(3,J))
         CALL PGSCI (CI(IWRLD+2))

         EDGE = NINT(CACHE(1,J))
         IF (EDGE.NE.PRVEDG) THEN
*           Start of new edge.

            IF (SEXA(1) .OR. SEXA(2)) THEN
*              Sexagesimal field suppression policy.
               PRVDEG(1) = -1
               PRVMIN(1) = -1
               PRVDEG(2) = -1
               PRVMIN(2) = -1

               SEXSUP(1) = 0
               SEXSUP(2) = 0

*              Vertical sides.
               IF (MOD(NINT(CACHE(1,J)),2).EQ.0) THEN
                  DO 160 K = J, IC
                     IF (NINT(CACHE(1,K)).NE.EDGE) GO TO 170

                     KWRLD = NINT(CACHE(3,K))
                     IF (SEXSUP(KWRLD).EQ.1) GO TO 160

                     IF (FTYPE(KWRLD).EQ.'y') THEN
                        TMP = ABS(MOD(CACHE(4,K),1D0)*24D0)
                     ELSE
                        TMP = ABS(CACHE(4,K))
                     END IF

                     LV = INT(TMP*3600D0 + 5D-7)
                     LD =  LV/3600
                     LM = (LV - LD*3600)/60
                     LS =  LV - LD*3600 - LM*60
                     TMP = TMP - LD

                     IF (TMP.LT.5D-7) THEN
*                       Write deg/hr field only when min/sec are zero.
                        SEXSUP(KWRLD) = 1
                     ELSE IF (TMP*60-LM.LT.3D-5) THEN
*                       Write min field only when sec is zero; only
*                       write the deg/hr field when min is written.
                        SEXSUP(KWRLD) = 2
                     END IF
 160              CONTINUE
               END IF
            END IF
 170        CONTINUE

            PRVYR(1) = -1
            PRVYR(2) = -1

            XL = -999.0
            YL = -999.0
         END IF
         PRVEDG = EDGE
         LFORCE = .FALSE.

         IF (EDGE.EQ.1) THEN
*           Bottom.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(1),2).NE.1) GO TO 230
            ELSE
               IF (MOD(SKIP(1)/2,2).NE.1) GO TO 230
            END IF

            FJUST = 0.5
            X = CACHE(2,J)
            Y = YW1 - 1.5*YCH
         ELSE IF (EDGE.EQ.2) THEN
*           Left.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(2),2).NE.1) GO TO 230
            ELSE
               IF (MOD(SKIP(2)/2,2).NE.1) GO TO 230
            END IF

            FJUST = 1.0
            X = XW1 - 0.5*XCH
            Y = CACHE(2,J) - YCH/2.0
         ELSE IF (EDGE.EQ.3) THEN
*           Top.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(3),2).NE.1) GO TO 230
            ELSE
               IF (MOD(SKIP(3)/2,2).NE.1) GO TO 230
            END IF

            FJUST = 0.5
            X = CACHE(2,J)
            Y = YW2 + 0.5*YCH
         ELSE IF (EDGE.EQ.4) THEN
*           Right.
            IF (IWRLD.EQ.1) THEN
               IF (MOD(SKIP(4),2).NE.1) GO TO 230
            ELSE
               IF (MOD(SKIP(4)/2,2).NE.1) GO TO 230
            END IF

            FJUST = 0.0
            X = XW2 + 0.5*XCH
            Y = CACHE(2,J) - YCH/2.0
         END IF

*        Format the numeric label.
         IF (INDEX('ABC', FTYPE(IWRLD)).NE.0) THEN
*           Decimal angle; allow up to 6 decimal digits.
            TMP = ABS(CACHE(4,J)) + 5D-7
            LD  = INT(TMP)

            K = 1
            IF (CACHE(4,J).LT.0D0) THEN
*              Insert a minus sign.
               TEXT(1:1) = '-'
               K = 2
            END IF

            CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
            K = K + NCH
            TEXT(K:) = ESCAPE // 'uo' // ESCAPE // 'd'
            K = K + 4

            LV = INT(MOD(TMP,1D0)*1D6)
            IF (LV.NE.0) CALL PGNUMB (LV, -6, 1, TEXT(K:), NCH)
            TEXT(K:K) = 'd'

         ELSE IF (SEXA(IWRLD)) THEN
*           Sexagesimal format; angle or time?
            ANGLE = INDEX('DEF', FTYPE(IWRLD)).NE.0
            L = LMAG(IWRLD)

*           Use integer arithmetic to avoid rounding problems.
            IF (FTYPE(IWRLD).EQ.'y') THEN
               TMP = ABS(MOD(CACHE(4,J),1D0)*24D0)

*              Determine date range.
               IF (CACHE(4,J).LT.MJD1(IWRLD)) MJD1(IWRLD) = CACHE(4,J)
               IF (CACHE(4,J).GT.MJD2(IWRLD)) MJD2(IWRLD) = CACHE(4,J)
            ELSE
               TMP = ABS(CACHE(4,J))
            END IF
            VS = TMP*3600D0 + 5D-7
            LV = INT(VS)

*           Sexagesimal fields.
            LD =  LV/3600
            LM = (LV - LD*3600)/60
            LS =  LV - LD*3600 - LM*60

*           Field suppression policy.
            IF (SEXSUP(IWRLD).GT.0) THEN
               TMP = TMP - LD

               IF (TMP.LT.5D-7) THEN
                 DODEG = .TRUE.
                 DOMIN = .TRUE.
               ELSE IF (SEXSUP(IWRLD).EQ.2) THEN
                 DOMIN = TMP*60-LM.LT.3D-5
                 DODEG = DOMIN .AND. LD.NE.PRVDEG(IWRLD)
               ELSE
                 DODEG = .FALSE.
                 DOMIN = .FALSE.
               END IF
            ELSE
               DODEG = LD.NE.PRVDEG(IWRLD)
               DOMIN = LM.NE.PRVMIN(IWRLD)
            END IF

            K = 1
            IF (L.EQ.-2 .OR. DODEG) THEN
*              Write the degree/hour field.
               DODEG = .TRUE.

               IF (CACHE(4,J).LT.0D0) THEN
*                 Insert a minus sign.
                  TEXT(1:1) = '-'
                  K = 2
               END IF

               IF (ANGLE) THEN
*                 Angle.
                  CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
                  K = K + NCH
                  TEXT(K:) = ESCAPE // 'uo' // ESCAPE // 'd'
                  K = K + 5
               ELSE
*                 Time.
                  IF (LD.LE.9 .AND. INDEX('Ty',FTYPE(IWRLD)).EQ.0) THEN
*                    Write leading zeroes in the hour field.
                     WRITE (TEXT(K:), '(I2.2)') LD
                     K = K + 2
                  ELSE
                     CALL PGNUMB (LD, 0, 1, TEXT(K:), NCH)
                     K = K + NCH
                  END IF
                  TEXT(K:) = ESCAPE // 'uh' // ESCAPE // 'd'
                  K = K + 5
               END IF
            END IF

            IF (L.GE.-1) THEN
*              Write arcminute/minute field.

               IF (L.EQ.-1 .OR. K.GT.1 .OR. DOMIN) THEN
                  DOMIN = .TRUE.
                  IF (ANGLE) THEN
                     WRITE (TEXT(K:), '(I2.2,A)') LM, ''''
                     K = K + 3
                  ELSE
                     WRITE (TEXT(K:), '(I2.2,A)') LM,
     :                  ESCAPE // 'um' // ESCAPE // 'd'
                     K = K + 7
                  END IF
               END IF

               IF (L.GE.0) THEN
*                 Arcsec/second field.
                  IF (ANGLE) THEN
                     WRITE (TEXT(K:), '(I2.2,A)') LS, '"'
                     K = K + 3
                  ELSE
                     WRITE (TEXT(K:), '(I2.2,A)') LS,
     :                  ESCAPE // 'us' // ESCAPE // 'd'
                     K = K + 7
                  END IF

                  IF (L.GT.0) THEN
*                    Sub-arcsec/second field.
                     WRITE (FMT, '(A,I1,A,I1,A)') '(A,I', L, '.', L, ')'
                     LV = INT(MOD(VS,1D0)*10**L)
                     WRITE (TEXT(K:), FMT) '.', LV
                  END IF
               END IF
            END IF

         ELSE IF (FTYPE(IWRLD).EQ.'L') THEN
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
               DO 180 K = 1, 8
                  IF (TEXT(K:K).NE.' ') GO TO 190
 180           CONTINUE
 190           TEXT = '10' // ESCAPE // 'u' // TEXT(K:8)

               LFORCE = .TRUE.
            END IF

         ELSE IF (FTYPE(IWRLD).EQ.'Y') THEN
*           Convert MJD to Gregorian calendar date, YYYY/MM/DD.
            CALL PGMJD(0, CACHE(4,J), IY, IM, ID)
            DOYEAR = IY.NE.PRVYR(IWRLD)

            IF (DOYEAR) THEN
               WRITE (TEXT, 200) IY, IM, ID
 200           FORMAT (I12,'/',I2.2,'/',I2.2)

               DO 210 K = 1, 12
                  IF (TEXT(K:K).NE.' ') GO TO 220
 210           CONTINUE
 220           TEXT = TEXT(K:18)

            ELSE
               WRITE (TEXT, 225) IM, ID
 225           FORMAT (I2.2,'/',I2.2)
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
            XL = XBOX(4) + 0.5*XCH
            YL = YBOX(2) + 0.5*YCH

*           Sexagesimal formatting.
            IF (SEXA(IWRLD)) THEN
               IF (DODEG) PRVDEG(IWRLD) = LD
               IF (DOMIN) PRVMIN(IWRLD) = LM
            END IF

*           Calendar formatting.
            IF (FTYPE(IWRLD).EQ.'Y') THEN
               IF (DOYEAR) PRVYR(IWRLD) = IY
            END IF

*           Record the fact.
            IF (IWRLD.EQ.1) THEN
               SKOP(EDGE) = 2*(SKOP(EDGE)/2) + 1
            ELSE
               SKOP(EDGE) = 2 + MOD(SKOP(EDGE),2)
            END IF

*           Boundary within which the numeric labels lie.
            IF (YBOX(1).LT.BNDRY(1)) BNDRY(1) = YBOX(1)
            IF (XBOX(1).LT.BNDRY(2)) BNDRY(2) = XBOX(1)
            IF (YBOX(3).GT.BNDRY(3)) BNDRY(3) = YBOX(3)
            IF (XBOX(3).GT.BNDRY(4)) BNDRY(4) = XBOX(3)

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
 230  CONTINUE


*  Write the identification strings.
*     World coordinates.
      DO 360 EDGE = 1, 4
         TEXT = ' '

         DO 330 IWRLD = 1, 2
            TXT(IWRLD) = ' '

            IF (MOD(SKOP(EDGE)/IWRLD,2).EQ.0) GO TO 330

*           Strip off leading blanks.
            L = LEN(IDENTS(IWRLD))
            DO 240 K = 1, L
               IF (IDENTS(IWRLD)(K:K).NE.' ') THEN
                  TXT(IWRLD) = IDENTS(IWRLD)(K:L)
                  GO TO 250
               END IF
 240        CONTINUE

 250        IF (IMAG(IWRLD).NE.0 .OR. FTYPE(IWRLD).EQ.'y') THEN
*              Find the last non-blank.
               DO 260 K = 40, 1, -1
                  IF (TXT(IWRLD)(K:K).NE.' ') GO TO 270
 260           CONTINUE
 270           K = K + 1

               IF (IMAG(IWRLD).NE.0) THEN
*                 Add scaling information.
                  CALL PGNUMB (IMAG(IWRLD), 0, 1, EXPONT, NCH)
                  TXT(IWRLD)(K:) = '  x10' // ESCAPE // 'u' // EXPONT
               ELSE
*                 Add calendar date range.
                  CALL PGMJD(0, MJD1(IWRLD), IY, IM, ID)

                  WRITE (TEXT, 280) IY, IM, ID
 280              FORMAT (I12,'/',I2.2,'/',I2.2)
                  DO 290 L = 1, 12
                     IF (TEXT(L:L).NE.' ') GO TO 300
 290              CONTINUE
 300              TXT(IWRLD)(K:) = ' (' // TEXT(L:18)
                  K = K + 21 - L

                  CALL PGMJD(0, MJD2(IWRLD), IY2, IM2, ID2)
                  WRITE (TEXT, 280) IY2, IM2, ID2
                  IF (IY2.EQ.IY) THEN
                     IF (IM2.EQ.IM) THEN
                        IF (ID2.EQ.ID) THEN
                           TXT(IWRLD)(K:) = ')'
                        ELSE
                           TXT(IWRLD)(K:) = ' - ' // TEXT(17:18) // ')'
                        END IF
                     ELSE
                        TXT(IWRLD)(K:) = ' - ' // TEXT(14:18) // ')'
                     END IF
                  ELSE
                     DO 310 L = 1, 12
                        IF (TEXT(L:L).NE.' ') GO TO 320
 310                 CONTINUE
 320                 TXT(IWRLD)(K:) = ' - ' // TEXT(L:18) // ')'
                  END IF
               END IF
            END IF

 330     CONTINUE

         K = 0
         IF (TXT(1).NE.' ') THEN
*           Identify first world coordinate...
            TEXT = TXT(1)
            CALL PGSCI (CI(5))

            IF (TXT(2).NE.' ') THEN
*              ...and also second world coordinate.
               DO 340 K = 40, 1, -1
                  IF (TEXT(K:K).NE.' ') GO TO 350
 340           CONTINUE
 350           K = K + 1

               TEXT(K:) = ',  ' // TXT(2)
            END IF
         ELSE IF (TXT(2).NE.' ') THEN
*           Identify second world coordinate only.
            TEXT = TXT(2)
            CALL PGSCI (CI(6))
         ELSE
*           No text to write.
            GO TO 360
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

         CALL PGQTXT (X, Y, ANGL, 0.5, TEXT, XBOX, YBOX)
         IF (K.EQ.0) THEN
            CALL PGPTXT (X, Y, ANGL, 0.5, TEXT)
         ELSE
*           Two-colour annotation.
            CALL PGPTXT (XBOX(1), YBOX(1), ANGL, 0.0, TEXT(:K))
            CALL PGSCI (CI(6))
            CALL PGPTXT (XBOX(4), YBOX(4), ANGL, 1.0, TEXT(K+1:))
         END IF

*        Update the boundary.
         IF (YBOX(1).LT.BNDRY(1)) BNDRY(1) = YBOX(1)
         IF (YBOX(3).LT.BNDRY(1)) BNDRY(1) = YBOX(3)
         IF (XBOX(1).LT.BNDRY(2)) BNDRY(2) = XBOX(1)
         IF (XBOX(3).LT.BNDRY(2)) BNDRY(2) = XBOX(3)
         IF (YBOX(1).GT.BNDRY(3)) BNDRY(3) = YBOX(1)
         IF (YBOX(3).GT.BNDRY(3)) BNDRY(3) = YBOX(3)
         IF (XBOX(1).GT.BNDRY(4)) BNDRY(4) = XBOX(1)
         IF (XBOX(3).GT.BNDRY(4)) BNDRY(4) = XBOX(3)
 360  CONTINUE

*     Title.
      IF (IDENTS(3).NE.' ') THEN
         CALL PGSCI (CI(7))
         X = (XW1 + XW2)/2.0
         Y = BNDRY(3) + 0.5*YCH
         CALL PGQTXT (X, Y, 0.0, 0.5, IDENTS(3), XBOX, YBOX)
         CALL PGPTXT (X, Y, 0.0, 0.5, IDENTS(3))

*        Update the boundary.
         IF (XBOX(1).LT.BNDRY(2)) BNDRY(2) = XBOX(1)
         IF (YBOX(3).GT.BNDRY(3)) BNDRY(3) = YBOX(3)
         IF (XBOX(3).GT.BNDRY(4)) BNDRY(4) = XBOX(3)
      END IF

*     Return margin widths in Cartesian coordinates.
      CACHE(1,NC) = YW1 - BNDRY(1)
      CACHE(2,NC) = XW1 - BNDRY(2)
      CACHE(3,NC) = BNDRY(3) - YW2
      CACHE(4,NC) = BNDRY(4) - XW2


      RETURN
      END


*=======================================================================
*   MJD to/from Gregorian calendar date.
*
*   Given:
*      CODE     I        If 1, compute MJD from IY,IM,ID, else vice
*                        versa.
*
*   Given and/or returned:
*      MJD      D        Modified Julian date, (MJD = JD - 2400000.5).
*      IY       I        Year.
*      IM       I        Month (for CODE.EQ.1 may exceed 12, or be zero,
*                        or negative).
*      ID       I        Day of month (for CODE.EQ.1, may exceed the
*                        legitimate number of days in the month, or be
*                        zero, or negative).
*
*   Notes:
*    1) These algorithms are from D.A. Hatcher, QJRAS 25, 53-55, as
*       modified by P.T. Wallace for use in SLALIB (subroutines CLDJ
*       and DJCL).
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*-----------------------------------------------------------------------
      SUBROUTINE PGMJD (CODE, MJD, IY, IM, ID)
*-----------------------------------------------------------------------
      INTEGER   CODE, DD, ID, IM, IY, JD, JM, JY, N4
      DOUBLE PRECISION MJD
*-----------------------------------------------------------------------
      IF (CODE.EQ.1) THEN
*        Calendar date to MJD.
         IF (IM.LT.1) THEN
            JY = IY - 1 + IM/12
            JM = 12 + MOD(IM,12)
         ELSE
            JY = IY + (IM-1)/12
            JM = MOD(IM-1,12) + 1
         END IF

         MJD =DBLE((1461*(JY - (12-JM)/10 + 4712))/4
     :            +(306*MOD(JM+9, 12) + 5)/10
     :            -(3*((JY - (12-JM)/10 + 4900)/100))/4
     :            + ID - 2399904)

      ELSE
*        MJD to calendar date.
         JD = 2400001 + INT(MJD)

         N4 =  4*(JD + ((2*((4*JD - 17918)/146097)*3)/4 + 1)/2 - 37)
         DD = 10*(MOD(N4-237, 1461)/4) + 5

         IY = N4/1461 - 4712
         IM = MOD(2 + DD/306, 12) + 1
         ID = MOD(DD, 306)/10 + 1
      END IF

      RETURN
      END



*=======================================================================
*   This FORTRAN wrapper on PGSBOX exists solely to define fixed-length
*   CHARACTER arguments for cpgsbox().
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*-----------------------------------------------------------------------
      SUBROUTINE PGSBOK (BLC, TRC, IDENTS, OPT, LABCTL, LABDEN, CI,
     :   GCODE, TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI,
     :   NLD, NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)
*-----------------------------------------------------------------------
      LOGICAL   DOEQ
      INTEGER   CI(7), GCODE(2), IC, IERR, LABCTL, LABDEN, NC, NG1, NG2,
     :          NLC, NLD, NLI, NLIPRM(NLI)
      REAL      BLC(2), TRC(2)
      DOUBLE PRECISION CACHE(4,0:NC), GRID1(0:NG1), GRID2(0:NG2),
     :          NLDPRM(NLD), TIKLEN
      CHARACTER IDENTS(3)*80, NLCPRM(NLC)*1, OPT(2)*1

      EXTERNAL NLFUNC
*-----------------------------------------------------------------------
      CALL PGSBOX (BLC, TRC, IDENTS, OPT, LABCTL, LABDEN, CI, GCODE,
     :   TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI, NLD,
     :   NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)

      RETURN
      END



*=======================================================================
*   This FORTRAN wrapper on PGLBOX exists solely to define fixed-length
*   CHARACTER arguments for cpglbox().
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*-----------------------------------------------------------------------
      SUBROUTINE PGLBOK (IDENTS, OPT, LABCTL, LABDEN, CI, GCODE, TIKLEN,
     :   NG1, GRID1, NG2, GRID2, DOEQ, NC, IC, CACHE, IERR)
*-----------------------------------------------------------------------
      LOGICAL   DOEQ
      INTEGER   CI(7), GCODE(2), IC, IERR, LABCTL, LABDEN, NC, NG1, NG2
      DOUBLE PRECISION CACHE(4,0:NC), GRID1(0:NG1), GRID2(0:NG2), TIKLEN
      CHARACTER IDENTS(3)*80, OPT(2)*1
*-----------------------------------------------------------------------
      CALL PGLBOX (IDENTS, OPT, LABCTL, LABDEN, CI, GCODE, TIKLEN, NG1,
     :   GRID1, NG2, GRID2, DOEQ, NC, IC, CACHE, IERR)

      RETURN
      END
