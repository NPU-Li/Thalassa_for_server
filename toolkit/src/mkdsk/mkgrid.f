C$Procedure MKGRID ( MKDSK: create plate set from height grid )

      SUBROUTINE MKGRID ( INFILE, PLTTYP, AUNITS, DUNITS,
     .                    CORSYS, CORPAR, MAXNV,  MAXNP,
     .                    NV,     VERTS,  NP,     PLATES )

C$ Abstract
C
C     Create a DSK type 2 plate set from a height grid provided
C     in a file.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     DSK
C
C$ Keywords
C
C     MKDSK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dsk02.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'
      INCLUDE 'mkdsk.inc'


      CHARACTER*(*)         INFILE
      INTEGER               PLTTYP
      CHARACTER*(*)         AUNITS
      CHARACTER*(*)         DUNITS
      INTEGER               CORSYS
      DOUBLE PRECISION      CORPAR ( * )
      INTEGER               MAXNV
      INTEGER               MAXNP
      INTEGER               NV
      DOUBLE PRECISION      VERTS  ( 3, * )
      INTEGER               NP
      INTEGER               PLATES ( 3, * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INFILE     I   Name of input file.
C     PLTTYP     I   MKDSK input file format code.
C     AUNITS     I   Angular units.
C     DUNITS     I   Distance units.
C     CORSYS     I   Coordinate system.
C     CORPAR     I   Coordinate parameters.
C     MAXNV      I   Maximum number of vertices.
C     MAXNP      I   Maximum number of plates.
C     NV         O   Number of vertices.
C     VERTS      O   Vertex array.
C     NP         O   Number of plates.
C     PLATES     O   Plate array.
C
C$ Detailed_Input
C
C     INFILE     is the name of an input data file containing height
C                grid data.
C
C     PLTTYP     is the MKDSK code indicating the format of the input
C                data file.
C
C     AUNITS     is the name of the angular unit associated with the
C                grid coordinates, if the grid coordinate system is
C                latitudinal or planetodetic. AUNITS must be supported
C                by the SPICELIB routine CONVRT.
C
C     DUNITS     is the name of the distance unit associated with the
C                grid coordinates. DUNITS must be supported by the
C                SPICELIB routine CONVRT.
C
C     CORSYS     is a DSK subsystem code designating the coordinate
C                system of the input coordinates.
C
C     CORPAR     is an array containing parameters associated with
C                the input coordinate system. The contents of the
C                array are as described in the DSK include file
C                dskdsc.inc.
C
C     COORDS     is a pair of domain coordinates: these may be,
C
C                   - planetocentric longitude and latitude
C
C                   - planetodetic longitude and latitude
C
C                   - X and Y
C
C                For a given coordinate system, the order of the
C                elements of COORDS is that of the coordinate names in
C                the list above.
C
C     MAXNV      I   Maximum number of vertices to return.
C
C     MAXNP      I   Maximum number of plates to return.
C
C
C$ Detailed_Output
C
C     NV         is the number of vertices in VERTS.
C
C     VERTS      is an array of 3-dimensional vertices corresponding
C                to the height grid.
C
C                Units are km.
C
C     NP         is the number of plates in PLATES.
C
C     PLATES     is an array of plates representing a tessellation of
C                the height grid.
C
C$ Parameters
C
C     See the MKDSK include files
C
C        mkdsk.inc
C        mkdsk02.inc
C
C     and the DSK include file
C
C        dskdsc.inc
C
C
C$ Exceptions
C
C     1)  If a polar cap is created, either due to a setup file
C         command or due to a vertex row having polar latitude, there
C         must be at least one non-polar vertex row. If no polar cap is
C         created, there must be at least two non-polar vertex rows.
C
C         If either the row or column count is insufficient to define a
C         surface, the error SPICE(INVALIDCOUNT) is signaled.
C
C     2)  If longitude wrap is specified for a rectangular coordinate
C         system, the error SPICE(SPURIOUSKEYWORD) is signaled.
C
C     3)  If either the north or south polar cap flag is .TRUE., and
C         the coordinate system is rectangular, the error
C         SPICE(SPURIOUSKEYWORD) is signaled.
C
C     4)  If either the row or column step is not strictly positive,
C         the error SPICE(INVALIDSTEP) is signaled.
C
C     5)  If the height scale is not is not strictly positive,
C         the error SPICE(INVALIDSCALE) is signaled.
C
C     6)  If the coordinate system is latitudinal and the height
C         reference is negative, the error SPICE(INVALIDREFVAL) is
C         signaled.
C
C     7)  If the number of vertices that must be created exceeds
C         MAXNV, the error SPICE(TOOMANYVERTICES) is signaled.
C
C     8)  If the number of plates that must be created exceeds
C         MAXNP, the error SPICE(TOOMANYPLATES) is signaled.
C
C     9)  If the input file format code is not recognized, the
C         error SPICE(NOTSUPPORTED) is signaled.
C
C    10)  If an error occurs while reading the input file, the
C         error will be diagnosed by routines in the call tree
C         of this routine.
C
C    11)  If an error occurs while processing the setup file, the error
C         will be diagnosed by routines in the call tree of this
C         routine.
C
C    12)  If an error occurs while converting the input data to a
C         vertex array, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C$ Files
C
C     The file specified by INFILE can have any of the attributes (one
C     choice from each row below):
C
C        row-major  or column-major
C        top-down   or bottom-up
C        left-right or right-left
C
C     The number of tokens per line may vary. The number need have no
C     particular relationship to the row or column dimensions of the
C     output grid.
C
C     The file must contain only tokens that can be read as double
C     precision values. No non-printing characters can be present in
C     the file.
C
C     Tokens can be delimited by blanks or commas. Tokens must not be
C     split across lines.
C
C     Blank lines are allowed; however, their use is discouraged
C     because they'll cause line numbers in diagnostic messages to
C     be out of sync with actual line numbers in the file.
C
C     The file must end with a line terminator.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     See usage in the MKDSK routine ZZWSEG02.
C
C$ Restrictions
C
C     1) For use only within program MKDSK.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    MKDSK Version 2.0.0, 26-DEC-2021 (NJB)
C
C        Now replaces rows at the poles, if present, with polar
C        vertices, and creates polar caps using these vertices and the
C        adjacent vertex rows. Polar vertices have heights equal to the
C        average height of vertices in the corresponding row. "Make
C        cap" commands in the MKDSK setup file are not used to invoke
C        this behavior.

C        Error checking was added for row latitudes outside the range
C        [-pi/2, pi/2], when the coordinate system is latitudinal or
C        geodetic. Error checking was added for the case where both
C        north and south vertex rows are at the poles, when the
C        coordinate system is latitudinal or geodetic.
C
C        Bug fix: corrected computation of required number of plates
C        used for overflow detection.
C
C        Corrected description of bad height reference error.
C
C-    MKDSK Version 1.0.0, 25-FEB-2017 (NJB)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 15 )

      INTEGER               NCOSYS
      PARAMETER           ( NCOSYS = 4 )
C
C     Local variables
C
      CHARACTER*(NAMLEN)    SYSNMS ( NCOSYS )

      DOUBLE PRECISION      BOTLAT
      DOUBLE PRECISION      COLSTP
      DOUBLE PRECISION      LFTCOR
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ROWSTP
      DOUBLE PRECISION      HSCALE
      DOUBLE PRECISION      SUM
      DOUBLE PRECISION      TOPCOR
      DOUBLE PRECISION      TOPLAT

      INTEGER               B
      INTEGER               I
      INTEGER               J
      INTEGER               PLTBAS
      INTEGER               NCOLS
      INTEGER               NMID
      INTEGER               NNORTH
      INTEGER               NPOLAR
      INTEGER               NROWS
      INTEGER               NSOUTH
      INTEGER               POLIDX
      INTEGER               REQNP
      INTEGER               REQNV
      INTEGER               REQROW

      LOGICAL               LEFTRT
      LOGICAL               MKNCAP
      LOGICAL               MKSCAP
      LOGICAL               NPOLE
      LOGICAL               ROWMAJ
      LOGICAL               SPOLE
      LOGICAL               TOPDWN
      LOGICAL               WRAP

C
C     Saved variables
C
      SAVE                 SYSNMS


C
C     Initial values
C
      DATA                 SYSNMS / 'Latitudinal',
     .                              'Cylindrical',
     .                              'Rectangular',
     .                              'Planetodetic' /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'MKGRID' )

C
C     Initial values of the polar row flags. These flags indicate
C     whether a vertex row is present at the indicated pole.
C
      NPOLE = .FALSE.
      SPOLE = .FALSE.

C
C     If we support the requested grid type, process the data.
C     The type is contained in the PLTTYP argument.
C
      IF ( PLTTYP .EQ. GRID5 ) THEN
C
C        Fetch grid parameters from the kernel pool.
C
         CALL GETG05 ( CORSYS, WRAP,   MKNCAP, MKSCAP, ROWMAJ, TOPDWN,
     .                 LEFTRT, REFVAL, HSCALE, NCOLS,  NROWS,
     .                 LFTCOR, TOPCOR, COLSTP, ROWSTP         )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'MKGRID' )
            RETURN
         END IF

C
C        Perform checks common to all coordinate systems.
C
C        First make sure we're working with a recognized system.
C
         IF (      ( CORSYS .NE. LATSYS )
     .       .AND. ( CORSYS .NE. PDTSYS )
     .       .AND. ( CORSYS .NE. RECSYS ) ) THEN

            CALL SETMSG ( 'Coordinate system code # is not '
     .      //            'recognized.'                     )
            CALL ERRINT ( '#', CORSYS                       )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
            CALL CHKOUT ( 'MKGRID'                          )
            RETURN

         END IF

         IF ( NCOLS .LT. 2 ) THEN

            CALL SETMSG ( 'Number of columns was #; must have at '
     .      //            'least two columns to create a grid.'   )
            CALL ERRINT ( '#', NCOLS                              )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                   )
            CALL CHKOUT ( 'MKGRID'                                )
            RETURN

         END IF

         IF ( COLSTP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Column step must be strictly positive but '
     .      //            'was #.'                                    )
            CALL ERRDP  ( '#', COLSTP                                 )
            CALL SIGERR ( 'SPICE(INVALIDSTEP)'                        )
            CALL CHKOUT ( 'MKGRID'                                    )
            RETURN

         END IF

         IF ( ROWSTP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Row step must be strictly positive but '
     .      //            'was #.'                                    )
            CALL ERRDP  ( '#', ROWSTP                                 )
            CALL SIGERR ( 'SPICE(INVALIDSTEP)'                        )
            CALL CHKOUT ( 'MKGRID'                                    )
            RETURN

         END IF

         IF ( HSCALE .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Height scale must be strictly positive but '
     .      //            'was #.'                                    )
            CALL ERRDP  ( '#', HSCALE                                 )
            CALL SIGERR ( 'SPICE(INVALIDSCALE)'                       )
            CALL CHKOUT ( 'MKGRID'                                    )
            RETURN

         END IF

C
C        Let NPOLAR indicate the number of polar vertex rows. We'll use
C        this variable for all coordinate systems; it will remain at
C        zero when it does not apply.
C
         NPOLAR = 0

C
C        For safety, check the parameters that could get us into real
C        trouble.
C
         IF ( CORSYS .EQ. RECSYS ) THEN

            IF ( NROWS .LT. 2 ) THEN

               CALL SETMSG ( 'Number of rows was #; must have at least '
     .         //            'two rows to create a grid using the '
     .         //            'rectangular coordinate system.'         )
               CALL ERRINT ( '#', NROWS                               )
               CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
               CALL CHKOUT ( 'MKGRID'                                 )
               RETURN

            END IF

C
C           The following error should be caught by GETG05, but we
C           check here for safety.
C
            IF ( WRAP ) THEN

               CALL SETMSG ( 'Longitude wrap is not applicable to the '
     .         //            'rectangular coordinate system.'          )
               CALL SIGERR ( 'SPICE(SPURIOUSKEYWORD)'                  )
               CALL CHKOUT ( 'MKGRID'                                  )
               RETURN

            END IF

            IF ( MKNCAP .OR. MKSCAP ) THEN

               CALL SETMSG ( 'Polar cap creation is not applicable '
     .         //            'to the rectangular coordinate system.' )
               CALL SIGERR ( 'SPICE(SPURIOUSKEYWORD)'                )
               CALL CHKOUT ( 'MKGRID'                                )
               RETURN

            END IF

         ELSE IF (     ( CORSYS .EQ. LATSYS )
     .            .OR. ( CORSYS .EQ. PDTSYS ) ) THEN
C
C           We have latitudinal or planetodetic coordinates.
C
C           Make sure we have enough rows of vertices. We'll need to
C           know whether any rows are at the poles.
C
C           We have a special case for grids having rows at the poles.
C           We'll replace rows at the poles with single points. We'll
C           use only the non-polar rows to form the main grid.
C
C           If the top row is at the north pole, just use one vertex
C           from that row and make a polar cap using that vertex and
C           the next row to the south.
C
            CALL CONVRT( TOPCOR, AUNITS, 'RADIANS', TOPLAT )

            IF ( TOPLAT .GT. ( HALFPI()+ANGMRG ) ) THEN

               CALL SETMSG ( 'Northernmost vertex row latitude '
     .         //            'is # degrees (# radians).'        )
               CALL ERRDP  ( '#',  TOPLAT*DPR()                 )
               CALL ERRDP  ( '#',  TOPLAT                       )
               CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
               CALL CHKOUT ( 'MKGRID'                           )
               RETURN

            ELSE
               TOPLAT = MIN( HALFPI(), TOPLAT )
            END IF

            IF ( SIN(TOPLAT) .EQ. 1.D0 ) THEN
C
C              We consider the top row to be at the pole. Note that
C              we use a single-precision criterion for latitude
C              because use inputs may have round-off errors of at
C              the single- precision level.
C
               NPOLE  = .TRUE.
C
C              Indicate we're making a north polar cap.
C
               MKNCAP = .TRUE.

            END IF

C
C           Convert the latitude of the bottom row to radians.
C
            CALL CONVRT( TOPCOR - (NROWS-1)*ROWSTP,
     .                   AUNITS,
     .                   'RADIANS',
     .                   BOTLAT    )

            IF ( BOTLAT .LT. ( -HALFPI()-ANGMRG ) ) THEN

              CALL SETMSG ( 'Southernmost vertex row latitude '
     .        //            'is # degrees (# radians).'        )
              CALL ERRDP  ( '#',  BOTLAT*DPR()                 )
              CALL ERRDP  ( '#',  BOTLAT                       )
              CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
              CALL CHKOUT ( 'MKGRID'                           )
              RETURN

            ELSE
              BOTLAT = MAX( -HALFPI(), BOTLAT )
            END IF

            IF ( SIN(BOTLAT) .EQ. -1.D0 ) THEN
C
C              We consider the bottom row to be at the pole. Note
C              that we use a single-precision criterion for latitude
C              because use inputs may have round-off errors of at
C              the single- precision level.
C
               SPOLE  = .TRUE.
C
C              Indicate we're making a south polar cap.
C
               MKSCAP = .TRUE.

            END IF

C
C           Check the number of available rows. If we're creating
C           at least one polar cap, there must be at least one row
C           that's at a latitude between the poles.
C
            IF ( MKNCAP .OR. MKSCAP ) THEN
C
C              The default number of required rows is 1, when we create
C              a polar cap. Rows at the poles don't count.
C
               REQROW = 1

               IF ( NPOLE .AND. SPOLE ) THEN

                  REQROW = 3
                  NPOLAR = 2

               ELSE IF ( NPOLE .OR. SPOLE ) THEN

                  REQROW = 2
                  NPOLAR = 1

               END IF

               IF ( NROWS .LT. REQROW ) THEN

                  CALL SETMSG ( 'Number of vertex grid rows was #; '
     .            //            'required number of rows was #. '
     .            //            'Number of polar rows was #. '
     .            //            'There must be at least 1 non-polar '
     .            //            'row to create a grid having at '
     .            //            'least one polar cap. '
     .            //            'Coordinate system was #.'          )
                  CALL ERRINT ( '#', NROWS                          )
                  CALL ERRINT ( '#', REQROW                         )
                  CALL ERRINT ( '#', NPOLAR                         )
                  CALL ERRCH  ( '#', SYSNMS(CORSYS)                 )
                  CALL SIGERR ( 'SPICE(INVALIDCOUNT)'               )
                  CALL CHKOUT ( 'MKGRID'                            )
                  RETURN

               END IF

            ELSE
C
C              No polar caps are being created.
C
               IF ( NROWS .LT. 2 ) THEN

                  CALL SETMSG ( 'Number of rows was #; must have at '
     .            //            'least two rows to create a grid using '
     .            //            'the # coordinate system when no '
     .            //            'polar caps are created.'             )
                  CALL ERRINT ( '#', NROWS                            )
                  CALL ERRCH  ( '#', SYSNMS(CORSYS)                   )
                  CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                 )
                  CALL CHKOUT ( 'MKGRID'                              )
                  RETURN

               END IF

            END IF

         END IF

C
C        For latitudinal coordinates, the height reference value
C        must be non-negative. This constraint does not apply to the
C        height reference value for rectangular coordinates. The height
C        reference does not apply to planetodetic coordinates; for that
C        system, the reference spheroid serves as the height reference.
C
         IF ( CORSYS .EQ. LATSYS ) THEN

            IF ( REFVAL .LT. 0.D0 ) THEN

               CALL SETMSG ( 'For latitudinal coordinates, the '
     .         //            'height reference value must be '
     .         //            'non-negative. It was #.'        )
               CALL ERRDP  ( '#', REFVAL                      )
               CALL SIGERR ( 'SPICE(INVALIDREFVAL)'           )
               CALL CHKOUT ( 'MKGRID'                         )
               RETURN

            END IF

         END IF

C
C        Now proceed with vertex and plate construction. All coordinate
C        systems will be handled together.
C
C        Let REQNV and REQNP be, respectively, the numbers of vertices
C        and plates we need to create. Make sure we can handle these
C        numbers.
C
         NV    = NROWS * NCOLS

         REQNV = NV

         REQNP = 2 * ( NROWS - 1 ) * ( NCOLS - 1 )

         IF ( WRAP ) THEN
            REQNP = REQNP + ( 2 * ( NROWS - 1 ) )
         END IF


         IF ( MKNCAP ) THEN

            REQNV = REQNV + 1

            REQNP = REQNP + NCOLS - 1

            IF ( WRAP ) THEN
               REQNP = REQNP + 1
            END IF

         END IF

         IF ( MKSCAP ) THEN

            REQNV = REQNV + 1

            REQNP = REQNP + NCOLS - 1

            IF ( WRAP ) THEN
               REQNP = REQNP + 1
            END IF

         END IF

         IF ( REQNV .GT. MAXNV ) THEN

            CALL SETMSG ( 'The number of vertices that must be '
     .      //            'created is #. The maximum allowed '
     .      //            'number is #.'                        )
            CALL ERRINT ( '#', REQNV                            )
            CALL ERRINT ( '#', MAXNV                            )
            CALL SIGERR ( 'SPICE(TOOMANYVERTICES)'              )
            CALL CHKOUT ( 'MKGRID'                              )
            RETURN

         END IF

C
C        Due to Euler's formula for polyhedra (V+F-E = 2), we don't
C        expect the condition of the next test to be met. The test
C        could be relevant if MAXNP is less than the value derived
C        from MAXNV by applying Euler's formula.
C
         IF ( REQNP .GT. MAXNP ) THEN

            CALL SETMSG ( 'The number of plates that must be '
     .      //            'created is #. The maximum allowed '
     .      //            'number is #.'                        )
            CALL ERRINT ( '#', REQNP                            )
            CALL ERRINT ( '#', MAXNP                            )
            CALL SIGERR ( 'SPICE(TOOMANYPLATES)'                )
            CALL CHKOUT ( 'MKGRID'                              )
            RETURN

         END IF

C
C        Create vertices. If we're making a north polar cap, leave
C        room for it at the start of the vertex array. We'll create
C        that vertex later.
C
C        At this point, we don't yet account for deletion of the
C        vertices of the top row, if the top row is at the pole.
C
         IF ( MKNCAP ) THEN
C
C           B is the index of the first vertex to be computed by
C           MKVARR.
C
            B  = 2
C
C           Update NV to indicate the number of occupied vertices
C           of the VERTS array after the MKVARR call below. The first
C           vertex is not set yet.
C
            NV = NV + 1
         ELSE
            B  = 1
         END IF

         CALL MKVARR ( INFILE, AUNITS, DUNITS, ROWMAJ, TOPDWN,
     .                 LEFTRT, CORSYS, CORPAR, REFVAL, HSCALE,
     .                 NCOLS,  NROWS,  LFTCOR, TOPCOR,
     .                 COLSTP, ROWSTP, MAXNV,  VERTS(1,B)     )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'MKGRID' )
            RETURN
         END IF

C
C        The output vertices have units of km.
C
C        Make plates. Fill in the polar vertices, if they're needed.
C
C        We create the plates in top-down order, so the polar caps
C        will be adjacent to the nearby non-polar plates.
C
         PLTBAS = 1
         NNORTH = 0
         NSOUTH = 0
         NP     = 0

         IF ( MKNCAP ) THEN
C
C           Create the plates of the north polar cap. Note that the
C           polar vertex has not been computed yet.
C
            POLIDX = 1

            CALL ZZCAPPLT ( NCOLS, .TRUE.,  WRAP,
     .                      PLTBAS, POLIDX, NNORTH, PLATES )

            NP = NNORTH
C
C           The north vertex magnitude is the average of the magnitudes
C           of the vertices in the top row.
C
C           Note that we still use the vertices in the top row
C           to compute this average if the top row is at the pole.
C
            SUM = 0.D0

            DO I = 1, NCOLS

               J   = B - 1 + I

               SUM = SUM + VNORM( VERTS(1,J) )

            END DO

            CALL VPACK ( 0.D0, 0.D0, SUM/NCOLS, VERTS(1,1) )

            IF ( NPOLE ) THEN
C
C              The top row of the grid is at +90 degrees latitude,
C              within single precision round-off error.
C
C              Now that we've computed a radius value for the vertex at
C              the north pole, we have no need of the vertices in the
C              top row. We'll compress them out of the VERTS array.
C              Note that the top row starts at the second vertex of
C              VERTS.
C
               NROWS = NROWS - 1
               NV    = NV    - NCOLS

               DO I = 2, NV

                  CALL VEQU( VERTS(1, I+NCOLS ),  VERTS(1,I) )

               END DO

            END IF

         END IF

C
C        Create the non-polar grid, if we have enough rows for it.
C        First adjust the row count, if necessary, to account for
C        deletion of a row at latitude -90 degrees.
C
         IF ( SPOLE ) THEN
C
C           The bottom row of the grid is at -90 degrees latitude,
C           within single precision round-off error.
C
C           Don't use the bottom row as part of the non-polar grid.
C           This row will be used only to determine the south polar
C           vertex.
C
            NROWS = NROWS - 1
            NV    = NV    - NCOLS

         END IF

C
C        Make the non-polar plates, if any. There must be at least two
C        non-polar vertex rows in order for there to be any bands of
C        plates that don't contact a pole. Note that the number of
C        rows has already been reduced by NPOLAR.
C
         IF ( NROWS .GT. 1 ) THEN

            CALL ZZGRDPLT ( NROWS, NCOLS, WRAP, NMID, PLATES(1,NP+1) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'MKGRID' )
               RETURN
            END IF

            NP = NP + NMID

            IF ( MKNCAP ) THEN
C
C              Adjust the vertex indices in the plate set.
C
               DO I = NNORTH+1, NP

                  DO J = 1, 3

                     PLATES(J,I) = PLATES(J,I) + 1

                  END DO

               END DO

            END IF

         END IF


         IF ( MKSCAP ) THEN
C
C           Create the plates of the north polar cap. Note that the
C           polar vertex has not been computed yet.
C
            POLIDX = NV + 1
            PLTBAS = B  - 1 + ( (NROWS-1)*NCOLS )

            CALL ZZCAPPLT ( NCOLS, .FALSE., WRAP,
     .                      PLTBAS, POLIDX, NSOUTH, PLATES(1,NP+1) )


            NP = NP + NSOUTH

C
C           The south vertex magnitude is the average of the
C           magnitudes of the vertices in the bottom row.
C
C           Note that we still use the vertices in the bottom row
C           to compute this average if the bottom row is at the pole.
C
            SUM = 0.D0

            DO I = 1, NCOLS
C
C              Pick vertices from the row just above the pole.
C
               J = PLTBAS + I

               IF ( SPOLE ) THEN
C
C                 Pick values from the row at the south pole.
C
                  J = J + NCOLS

               END IF

               SUM = SUM + VNORM( VERTS(1,J) )

            END DO

            CALL VPACK ( 0.D0, 0.D0, -SUM/NCOLS, VERTS(1,POLIDX) )

            NV = NV + 1

         END IF

      ELSE
C
C        This error should be caught in RDFFPL. Check here for safety.
C
         CALL SETMSG ( 'Input data format type is #; only type # '
     .   //            'is supported.'                            )
         CALL ERRINT ( '#', PLTTYP                                )
         CALL ERRINT ( '#', GRID5                                 )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'MKGRID'                                   )
         RETURN

      END IF

      CALL CHKOUT ( 'MKGRID' )
      RETURN
      END
