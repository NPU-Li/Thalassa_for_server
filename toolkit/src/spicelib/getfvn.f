C$Procedure GETFVN (Get instrument FOV parameters, by instrument name)

      SUBROUTINE GETFVN ( INST,
     .                    ROOM,
     .                    SHAPE,
     .                    FRAME,
     .                    BSIGHT,
     .                    N,
     .                    BOUNDS  )

C$ Abstract
C
C     Return the field-of-view (FOV) parameters for a specified
C     instrument. The instrument is specified by name.
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
C     NAIF_IDS
C
C$ Keywords
C
C     FOV
C     INSTRUMENT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      CHARACTER*(*)         INST
      INTEGER               ROOM
      CHARACTER*(*)         SHAPE
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      BSIGHT ( 3    )
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INST       I   Name of an instrument.
C     ROOM       I   Maximum number of vectors that can be returned.
C     SHAPE      O   Instrument FOV shape.
C     FRAME      O   Name of the frame in which FOV vectors are defined.
C     BSIGHT     O   Boresight vector.
C     N          O   Number of boundary vectors returned.
C     BOUNDS     O   FOV boundary vectors.
C
C$ Detailed_Input
C
C     INST     is the name of an instrument, such as a
C              spacecraft-mounted framing camera, for which the field of
C              view parameters are to be retrieved from the kernel pool.
C              INST is case-insensitive, and leading and trailing blanks
C              in INST are not significant. Optionally, you may supply
C              the integer ID for the instrument as an integer string.
C              For example, both 'CASSINI_ISS_NAC' and '-82360' are
C              legitimate strings that indicate the CASSINI ISS NAC
C              camera is the instrument of interest.
C
C     ROOM     is the maximum number of 3-dimensional vectors that can
C              be returned in BOUNDS.
C
C$ Detailed_Output
C
C     SHAPE    is a character string that describes the "shape" of
C              the field of view. Possible values returned are:
C
C                 'POLYGON'
C                 'RECTANGLE'
C                 'CIRCLE'
C                 'ELLIPSE'
C
C              If the value of SHAPE is 'POLYGON' the field of view
C              of the instrument is a pyramidal polyhedron. The
C              vertex of the pyramid is at the instrument focal
C              point. The rays along the edges of the pyramid are
C              parallel to the vectors returned in BOUNDS.
C
C              If the value of SHAPE is 'RECTANGLE' the field of view
C              of the instrument is a rectangular pyramid. The vertex
C              of the pyramid is at the instrument focal point. The
C              rays along the edges of the pyramid are parallel to
C              the vectors returned in BOUNDS. Moreover, in this
C              case, the boresight points along the axis of symmetry
C              of the rectangular pyramid.
C
C              If the value of SHAPE is 'CIRCLE' the field of view of
C              the instrument is a circular cone centered on the
C              boresight vector. The vertex of the cone is at the
C              instrument focal point. A single vector will be
C              returned in BOUNDS. This vector will be parallel to a
C              ray that lies in the cone that makes up the boundary
C              of the field of view.
C
C              If the value of SHAPE is 'ELLIPSE' the field of view
C              of the instrument is an elliptical cone with the
C              boresight vector as the axis of the cone. In this
C              case two vectors are returned in BOUNDS. One of the
C              vectors returned in BOUNDS points to the end of the
C              semi-major axis of a perpendicular cross section of
C              the elliptic cone. The other vector points to the end
C              of the semi-minor axis of a perpendicular cross
C              section of the cone.
C
C     FRAME    is the name of the reference frame in which the field
C              of view boundary vectors are defined.
C
C     BSIGHT   is a vector representing the principal instrument view
C              direction that can be
C
C                 -  the central pixel view direction,
C                 -  the optical axis direction,
C                 -  the FOV geometric center view direction,
C                 -  an axis of the FOV frame,
C
C              or any other vector specified for this purpose
C              in the IK FOV definition. The length of BSIGHT
C              is not specified other than being non-zero.
C
C     N        is the number of boundary vectors returned.
C
C     BOUNDS   is an array of vectors that point to the "corners" of
C              the instrument field of view. (See the discussion
C              accompanying SHAPE for an expansion of the term
C              "corner of the field of view.") Note that the vectors
C              returned in BOUNDS are not necessarily unit vectors.
C              Their magnitudes will be as set in the IK (for
C              'CORNERS'-style FOV specifications) or the same as the
C              magnitude of the boresight (for 'ANGLES'-style FOV
C              specifications.)
C
C$ Parameters
C
C     MINCOS   is the lower limit on the value of the cosine of the
C              cross or reference angles in the 'ANGLES' specification
C              cases. See the header of the routine GETFOV for its
C              current value.
C
C$ Exceptions
C
C     1)  If the name of the instrument cannot be translated to its
C         NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the frame associated with the instrument can not be found,
C         an error is signaled by a routine in the call tree of this
C         routine.
C
C     3)  If the shape of the instrument field of view can not be found
C         in the kernel pool, an error is signaled by a routine in the
C         call tree of this routine.
C
C     4)  If the FOV_SHAPE specified by the instrument kernel is not
C         one of the four values: 'CIRCLE', 'POLYGON', 'ELLIPSE', or
C         'RECTANGLE', an error is signaled by a routine in the call
C         tree of this routine. If the 'ANGLES' specification is used,
C         FOV_SHAPE must be one of the three values: 'CIRCLE',
C         'ELLIPSE', or 'RECTANGLE'.
C
C     5)  If the direction of the boresight cannot be located in the
C         kernel pool, an error is signaled by a routine in the call
C         tree of this routine.
C
C     6)  If the number of components for the boresight vector in the
C         kernel pool is not 3, or they are not numeric, an error is
C         signaled by a routine in the call tree of this routine.
C
C     7)  If the boresight vector is the zero vector, an error is
C         signaled by a routine in the call tree of this routine.
C
C     8)  If the 'ANGLES' specification is not present in the kernel
C         pool and the boundary vectors for the edge of the field of
C         view cannot be found in the kernel pool, an error is signaled
C         by a routine in the call tree of this routine.
C
C     9)  If there is insufficient room (as specified by the argument
C         ROOM) to return all of the vectors associated with the
C         boundary of the field of view, an error is signaled by a
C         routine in the call tree of this routine.
C
C     10) If the number of components of vectors making up the field of
C         view is not a multiple of 3, an error is signaled by a routine
C         in the call tree of this routine.
C
C     11) If the number of components of vectors making up the field of
C         view is not compatible with the shape specified for the field
C         of view, an error is signaled by a routine in the call tree of
C         this routine.
C
C     12) If the reference vector for the 'ANGLES' specification can not
C         be found in the kernel pool, an error is signaled by a routine
C         in the call tree of this routine.
C
C     13) If the reference vector stored in the kernel pool to support
C         the 'ANGLES' specification contains an incorrect number of
C         components, contains 3 character components, or is parallel to
C         the boresight, an error is signaled by a routine in the call
C         tree of this routine.
C
C     14) If the 'ANGLES' specification is present in the kernel pool
C         and the reference angle stored in the kernel pool to support
C         the 'ANGLES' specification is absent from the kernel pool, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     15) If the keyword that stores the angular units for the angles
C         used in the 'ANGLES' specification is absent from the kernel
C         pool, an error is signaled by a routine in the call tree of
C         this routine.
C
C     16) If the value used for the units in the 'ANGLES' specification
C         is not one of the supported angular units of CONVRT, an error
C         is signaled by a routine in the call tree of this routine.
C
C     17) If the keyword that stores the cross angle for the 'ANGLES'
C         specification is needed and is absent from the kernel pool, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     18) If the angles for the 'RECTANGLE'/'ANGLES' specification case
C         have cosines that are less than those stored in the parameter
C         MINCOS defined in the GETFOV routine, an error is signaled by
C         a routine in the call tree of this routine.
C
C     19) If the class specification contains something other than
C         'ANGLES' or 'CORNERS', an error is signaled by a routine in
C         the call tree of this routine.
C
C     20) In the event that the CLASS_SPEC keyword is absent from the
C         kernel pool for the instrument whose FOV is sought, this
C         module assumes the 'CORNERS' specification is to be utilized.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     This routine relies upon having successfully loaded an instrument
C     kernel (IK file) via the routine FURNSH prior to calling this
C     routine.
C
C     The name INST must be associated with an NAIF ID code, normally
C     through a frames kernel (FK file) or instrument kernel (IK file).
C
C     Kernel data are normally loaded via FURNSH once per program run,
C     NOT every time this routine is called.
C
C$ Particulars
C
C     This routine provides a common interface for retrieving from the
C     kernel pool the geometric characteristics of an instrument field
C     of view for a wide variety of remote sensing instruments
C     across many different space missions.
C
C     This routine is identical in function to the routine GETFOV except
C     that it allows you to refer to an instrument by name (via a
C     character string) instead of by its NAIF instrument ID.
C
C     Given the NAIF instrument name, (and having "loaded" the
C     instrument field of view description and instrument name to NAIF
C     ID mapping) this routine returns the boresight of the instrument,
C     the "shape" of the field of view, a collection of vectors
C     that point along the edges of the field of view, and the
C     name of the reference frame in which these vectors are defined.
C
C     Currently this routine supports two classes of specifications
C     for FOV definitions: "corners" and "angles".
C
C     The "corners" specification requires that the following keywords
C     defining the shape, boresight, boundary vectors, and reference
C     frame of the FOV be provided in one of the text kernel files
C     (normally an IK file) loaded into the kernel pool (in the
C     keywords below <INSTID> is replaced with the instrument ID which
C     corresponds to the INST name passed into the module):
C
C        INS<INSTID>_FOV_CLASS_SPEC         must be set to 'CORNERS' or
C                                           omitted to indicate the
C                                           "corners"-class
C                                           specification.
C
C        INS<INSTID>_FOV_SHAPE              must be set to one of these
C                                           values:
C
C                                              'CIRCLE'
C                                              'ELLIPSE'
C                                              'RECTANGLE'
C                                              'POLYGON'
C
C        INS<INSTID>_FOV_FRAME              must contain the name of
C                                           the frame in which the
C                                           boresight and boundary
C                                           corner vectors are defined.
C
C        INS<INSTID>_BORESIGHT              must be set to a 3D vector
C                                           defining the boresight in
C                                           the FOV frame specified in
C                                           the FOV_FRAME keyword.
C
C        INS<INSTID>_FOV_BOUNDARY   or
C        INS<INSTID>_FOV_BOUNDARY_CORNERS   must be set to one (for
C                                           FOV_SHAPE = 'CIRCLE'), two
C                                           (for FOV_SHAPE =
C                                           'ELLIPSE'), four (for
C                                           FOV_SHAPE = 'RECTANGLE'),
C                                           or three or more (for
C                                           'POLYGON') 3D vectors
C                                           defining the corners of the
C                                           FOV in the FOV frame
C                                           specified in the FOV_FRAME
C                                           keyword. The vectors should
C                                           be listed in either
C                                           clockwise or
C                                           counterclockwise order.
C                                           This is required by some
C                                           SPICE routines that make
C                                           use of FOV specifications.
C
C     The "angles" specification requires the following keywords
C     defining the shape, boresight, reference vector, reference and
C     cross angular extents of the FOV be provided in one of the text
C     kernel files (normally an IK file) loaded into the kernel
C     pool (in the keywords below <INSTID> is replaced with the
C     instrument ID which corresponds to the INST name passed into the
C     module):
C
C        INS<INSTID>_FOV_CLASS_SPEC         must be set to 'ANGLES' to
C                                           indicate the "angles"-class
C                                           specification.
C
C        INS<INSTID>_FOV_SHAPE              must be set to one of these
C                                           values:
C
C                                              'CIRCLE'
C                                              'ELLIPSE'
C                                              'RECTANGLE'
C
C        INS<INSTID>_FOV_FRAME              must contain the name of
C                                           the frame in which the
C                                           boresight and the computed
C                                           boundary corner vectors are
C                                           defined.
C
C        INS<INSTID>_BORESIGHT              must be set to a 3D vector
C                                           defining the boresight in
C                                           the FOV frame specified in
C                                           the FOV_FRAME keyword.
C
C        INS<INSTID>_FOV_REF_VECTOR         must be set to a 3D vector
C                                           that together with the
C                                           boresight vector defines
C                                           the plane in which the
C                                           first angular extent of the
C                                           FOV specified in the
C                                           FOV_REF_ANGLE keyword is
C                                           measured.
C
C        INS<INSTID>_FOV_REF_ANGLE          must be set to the angle
C                                           that is 1/2 of the total
C                                           FOV angular extent in the
C                                           plane defined by the
C                                           boresight and the vector
C                                           specified in the
C                                           FOV_REF_VECTOR keyword. The
C                                           the FOV angular half-extents
C                                           are measured from the
C                                           boresight vector.
C
C        INS<INSTID>_FOV_CROSS_ANGLE        must be set to the angle
C                                           that is 1/2 of the total
C                                           FOV angular extent in the
C                                           plane containing the
C                                           boresight and perpendicular
C                                           to the plane defined by the
C                                           boresight and the vector
C                                           specified in the
C                                           FOV_REF_VECTOR keyword. The
C                                           the FOV angular half-extents
C                                           are measured from the
C                                           boresight vector. This
C                                           keyword is not required for
C                                           FOV_SHAPE = 'CIRCLE'.
C
C        INS<INSTID>_FOV_ANGLE_UNITS        must specify units for the
C                                           angles given in the
C                                           FOV_REF_ANGLE and
C                                           FOV_CROSS_ANGLE keywords.
C                                           Any angular units
C                                           recognized by CONVRT are
C                                           acceptable.
C
C     The INS<INSTID>_FOV_REF_ANGLE and INS<INSTID>_FOV_CROSS_ANGLE
C     keywords can have any values for the 'CIRCLE' and 'ELLIPSE'
C     FOV shapes but must satisfy the condition COS( ANGLE ) > 0 for
C     the 'RECTANGLE' shape.
C
C     This routine is intended to be an intermediate level routine.
C     It is expected that users of this routine will be familiar
C     with the SPICE frames subsystem and will be comfortable writing
C     software to further manipulate the vectors retrieved by this
C     routine.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Load an IK, fetch the parameters for each of the FOVs defined
C        within and print these parameters to the screen.
C
C        Use the kernel shown below, an IK defining four FOVs of
C        various shapes and sizes, to load the FOV definitions, and the
C        mapping between their NAMES and NAIF IDs.
C
C
C           KPL/IK
C
C           File name: getfvn_ex1.ti
C
C           The keywords below define a circular, 10-degree wide FOV
C           with the boresight along the +Z axis of the 'SC999_INST001'
C           frame for an instrument with ID -999001 using the
C           "angles"-class specification.
C
C           \begindata
C              INS-999001_FOV_CLASS_SPEC       = 'ANGLES'
C              INS-999001_FOV_SHAPE            = 'CIRCLE'
C              INS-999001_FOV_FRAME            = 'SC999_INST001'
C              INS-999001_BORESIGHT            = ( 0.0, 0.0, 1.0 )
C              INS-999001_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
C              INS-999001_FOV_REF_ANGLE        = ( 5.0 )
C              INS-999001_FOV_ANGLE_UNITS      = ( 'DEGREES' )
C           \begintext
C
C           The keywords below define an elliptical FOV with 2- and
C           4-degree angular extents in the XZ and XY planes and the
C           boresight along the +X axis of the 'SC999_INST002' frame for
C           an instrument with ID -999002 using the "corners"-class
C           specification.
C
C           \begindata
C              INS-999002_FOV_SHAPE            = 'ELLIPSE'
C              INS-999002_FOV_FRAME            = 'SC999_INST002'
C              INS-999002_BORESIGHT            = ( 1.0, 0.0, 0.0 )
C              INS-999002_FOV_BOUNDARY_CORNERS = (
C                                      1.0,  0.0,        0.01745506,
C                                      1.0,  0.03492077, 0.0        )
C           \begintext
C
C           The keywords below define a rectangular FOV with 1.2- and
C           0.2-degree angular extents in the ZX and ZY planes and the
C           boresight along the +Z axis of the 'SC999_INST003' frame for
C           an instrument with ID -999003 using the "angles"-class
C           specification.
C
C           \begindata
C              INS-999003_FOV_CLASS_SPEC       = 'ANGLES'
C              INS-999003_FOV_SHAPE            = 'RECTANGLE'
C              INS-999003_FOV_FRAME            = 'SC999_INST003'
C              INS-999003_BORESIGHT            = ( 0.0, 0.0, 1.0 )
C              INS-999003_FOV_REF_VECTOR       = ( 1.0, 0.0, 0.0 )
C              INS-999003_FOV_REF_ANGLE        = ( 0.6 )
C              INS-999003_FOV_CROSS_ANGLE      = ( 0.1 )
C              INS-999003_FOV_ANGLE_UNITS      = ( 'DEGREES' )
C           \begintext
C
C           The keywords below define a triangular FOV with the
C           boresight along the +Y axis of the 'SC999_INST004' frame
C           for an instrument with ID -999004 using the "corners"-class
C           specification.
C
C           \begindata
C              INS-999004_FOV_SHAPE            = 'POLYGON'
C              INS-999004_FOV_FRAME            = 'SC999_INST004'
C              INS-999004_BORESIGHT            = (  0.0,  1.0,  0.0 )
C              INS-999004_FOV_BOUNDARY_CORNERS = (  0.0,  0.8,  0.5,
C                                                   0.4,  0.8, -0.2,
C                                                  -0.4,  0.8, -0.2 )
C           \begintext
C
C           The keywords below define the INSTRUMENT name to NAIF ID
C           mappings for this example. For convenience we will keep them
C           in the example's IK although they could be placed elsewhere
C           (normally on the mission's frames kernel)
C
C           \begindata
C              NAIF_BODY_NAME += ( 'SC999_INST001' )
C              NAIF_BODY_CODE += ( -999001 )
C
C              NAIF_BODY_NAME += ( 'SC999_INST002' )
C              NAIF_BODY_CODE += ( -999002 )
C
C              NAIF_BODY_NAME += ( 'SC999_INST003' )
C              NAIF_BODY_CODE += ( -999003 )
C
C              NAIF_BODY_NAME += ( 'SC999_INST004' )
C              NAIF_BODY_CODE += ( -999004 )
C           \begintext
C
C           End of IK
C
C
C        Example code begins here.
C
C
C              PROGRAM GETFVN_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters
C        C
C              INTEGER               MAXBND
C              PARAMETER           ( MAXBND = 4 )
C
C              INTEGER               NUMINS
C              PARAMETER           ( NUMINS = 4 )
C
C              INTEGER               WDSIZE
C              PARAMETER           ( WDSIZE = 32 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(WDSIZE)    INSTNM ( NUMINS )
C              CHARACTER*(WDSIZE)    FRAME
C              CHARACTER*(WDSIZE)    SHAPE
C
C              DOUBLE PRECISION      BOUNDS ( 3, MAXBND )
C              DOUBLE PRECISION      BSIGHT ( 3 )
C
C              INTEGER               I
C              INTEGER               J
C              INTEGER               N
C
C        C
C        C     Define the instrument IDs.
C        C
C              DATA                  INSTNM  /  'SC999_INST001',
C             .                                 'SC999_INST002',
C             .                                 'SC999_INST003',
C             .                                 'SC999_INST004'  /
C
C        C
C        C     Load the IK file.
C        C
C              CALL FURNSH( 'getfvn_ex1.ti' )
C
C        C
C        C     For each instrument ...
C        C
C              WRITE (*,'(A)') '--------------------------------------'
C              DO I = 1, NUMINS
C
C        C
C        C        ... fetch FOV parameters and ...
C        C
C                 CALL GETFVN ( INSTNM(I), MAXBND,
C             .                 SHAPE, FRAME, BSIGHT, N, BOUNDS )
C
C        C
C        C        ... print them to the screen.
C        C
C                 WRITE (*,'(2A)')       'Instrument NAME: ', INSTNM(I)
C                 WRITE (*,'(2A)')       '    FOV shape: ', SHAPE
C                 WRITE (*,'(2A)')       '    FOV frame: ', frame
C                 WRITE (*,'(A,3F12.8)') 'FOV boresight: ', BSIGHT
C
C                 WRITE (*,'(A)') '  FOV corners: '
C                 DO J = 1, N
C                    WRITE (*,'(A,3F12.8)') '               ',
C             .                  BOUNDS(1,J), BOUNDS(2,J), BOUNDS(3,J)
C                 END DO
C                 WRITE (*,'(A)')
C             .            '--------------------------------------'
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        --------------------------------------
C        Instrument NAME: SC999_INST001
C            FOV shape: CIRCLE
C            FOV frame: SC999_INST001
C        FOV boresight:   0.00000000  0.00000000  1.00000000
C          FOV corners:
C                         0.08715574  0.00000000  0.99619470
C        --------------------------------------
C        Instrument NAME: SC999_INST002
C            FOV shape: ELLIPSE
C            FOV frame: SC999_INST002
C        FOV boresight:   1.00000000  0.00000000  0.00000000
C          FOV corners:
C                         1.00000000  0.00000000  0.01745506
C                         1.00000000  0.03492077  0.00000000
C        --------------------------------------
C        Instrument NAME: SC999_INST003
C            FOV shape: RECTANGLE
C            FOV frame: SC999_INST003
C        FOV boresight:   0.00000000  0.00000000  1.00000000
C          FOV corners:
C                         0.01047177  0.00174523  0.99994365
C                        -0.01047177  0.00174523  0.99994365
C                        -0.01047177 -0.00174523  0.99994365
C                         0.01047177 -0.00174523  0.99994365
C        --------------------------------------
C        Instrument NAME: SC999_INST004
C            FOV shape: POLYGON
C            FOV frame: SC999_INST004
C        FOV boresight:   0.00000000  1.00000000  0.00000000
C          FOV corners:
C                         0.00000000  0.80000000  0.50000000
C                         0.40000000  0.80000000 -0.20000000
C                        -0.40000000  0.80000000 -0.20000000
C        --------------------------------------
C
C
C$ Restrictions
C
C     1)  This routine will not operate unless proper mapping between
C         the instrument name INST and a NAIF ID exists, an I-kernel for
C         that instrument ID has been loaded via a call to FURNSH prior
C         to calling this routine and this IK contains the specification
C         for the instrument field of view consistent with the
C         expectations of this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     M. Liukis          (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 17-DEC-2021 (JDR) (BVS) (ML)
C
C-&


C$ Index_Entries
C
C     return instrument's FOV parameters, using instrument name
C
C-&


C
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Saved instrument name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

C
C     Local variables
C
      DOUBLE PRECISION      INSTID

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVINST
      INTEGER               SVINID
      LOGICAL               SVFND1

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVINST
      SAVE                  SVINID
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GETFVN' )

C
C     Initialization.
C
      IF ( FIRST ) THEN
C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

C
C     Translate the instrument's name to its ID code
C
      CALL ZZBODS2C ( SVCTR1, SVINST, SVINID, SVFND1,
     .                INST,   INSTID, FOUND           )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( '''#'' is not a recognized name for an '
     .   //            'instrument. The cause of this '
     .   //            'problem may be that you have not loaded '
     .   //            'a required frame kernel or instrument '
     .   //            'kernel.'                                  )
         CALL ERRCH  ( '#', INST                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'GETFVN'                                   )
         RETURN

      END IF

      CALL GETFOV ( INSTID, ROOM, SHAPE, FRAME, BSIGHT, N, BOUNDS )

      CALL CHKOUT ( 'GETFVN' )
      RETURN

      END
