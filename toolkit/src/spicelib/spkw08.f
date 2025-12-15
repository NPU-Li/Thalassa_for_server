C$Procedure SPKW08 ( Write SPK segment, type 8 )

      SUBROUTINE SPKW08 (  HANDLE,  BODY,    CENTER,  FRAME,
     .                     FIRST,   LAST,    SEGID,   DEGREE,
     .                     N,       STATES,  BEGTIM,  STEP     )

C$ Abstract
C
C     Write a type 8 segment to an SPK file.
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
C     SPC
C     SPK
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'spk08.inc'

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               DEGREE
      INTEGER               N
      DOUBLE PRECISION      STATES ( 6, * )
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      STEP

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MAXDEG     P   Maximum degree of interpolating polynomials.
C     TOLSCL     P   Scale factor used to compute time bound tolerance.
C     HANDLE     I   Handle of an SPK file open for writing.
C     BODY       I   NAIF code for an ephemeris object.
C     CENTER     I   NAIF code for center of motion of BODY.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     DEGREE     I   Degree of interpolating polynomials.
C     N          I   Number of states.
C     STATES     I   Array of states.
C     BEGTIM     I   Epoch of first state in STATES array.
C     STEP       I   Time step separating epochs of states.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing.
C
C     BODY     is the NAIF integer code for an ephemeris object
C              whose state relative to another body is described
C              by the segment to be created.
C
C     CENTER   is the NAIF integer code for the center of motion
C              of the object identified by BODY.
C
C     FRAME    is the NAIF name for a reference frame
C              relative to which the state information for BODY
C              is specified.
C
C     FIRST,
C     LAST     are, respectively, the start and stop times of
C              the time interval over which the segment defines
C              the state of BODY.
C
C     SEGID    is the segment identifier. An SPK segment
C              identifier may contain up to 40 characters.
C
C     DEGREE   is the degree of the Lagrange polynomials used to
C              interpolate the states. All components of the
C              state vectors are interpolated by polynomials of
C              fixed degree.
C
C     N        is the number of states in the input state vector
C              array.
C
C     STATES   contains a time-ordered array of geometric states
C              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
C              kilometers per second ) of BODY relative to CENTER,
C              specified relative to FRAME.
C
C     BEGTIM   is the epoch corresponding to the first state in
C              the state array. Because extra states are needed
C              at the beginning and end of the segment in order
C              for the interpolation method to work, BEGTIM will
C              normally precede FIRST.
C
C     STEP     is the time step separating the epochs of adjacent
C              states in the input state array. STEP is specified
C              in seconds.
C
C$ Detailed_Output
C
C     None. See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     See the include file spk08.inc for declarations of the
C     parameters described below.
C
C     MAXDEG   is the maximum degree of Lagrange polynomials that
C              can be used to interpolate states from the segment
C              written by this routine.
C
C     TOLSCL   is a tolerance scale factor (also called a
C              "relative tolerance") used for time coverage
C              bound checking. TOLSCL is unitless. TOLSCL
C              produces a tolerance value via the formula
C
C                 TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
C
C              where FIRST and LAST are the coverage time bounds
C              of a type 8 segment, expressed as seconds past
C              J2000 TDB.
C
C              The resulting parameter TOL is used as a tolerance
C              for comparing the input segment descriptor time
C              bounds to the first and last epoch covered by the
C              sequence of time intervals defined by the inputs
C              to SPKW08:
C
C                 BEGTIM
C                 STEP
C                 N
C
C$ Exceptions
C
C     If any of the following exceptions occur, this routine will return
C     without creating a new segment.
C
C     1)  If FRAME is not a recognized name, the error
C         SPICE(INVALIDREFFRAME) is signaled.
C
C     2)  If the last non-blank character of SEGID occurs past index 40,
C         the error SPICE(SEGIDTOOLONG) is signaled.
C
C     3)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     4)  If DEGREE is not at least 1 or is greater than MAXDEG, the
C         error SPICE(INVALIDDEGREE) is signaled.
C
C     5)  If the number of states N is not at least DEGREE+1, the error
C         SPICE(TOOFEWSTATES) is signaled.
C
C     6)  If FIRST is greater than LAST, the error
C         SPICE(BADDESCRTIMES) is signaled.
C
C     7)  If STEP is non-positive, the error SPICE(INVALIDSTEPSIZE) is
C         signaled.
C
C     8)  If the start time of the first record exceeds the descriptor
C         begin time by more than a computed tolerance, or if the end
C         time of the last record precedes the descriptor end time by
C         more than a computed tolerance, the error SPICE(COVERAGEGAP)
C         is signaled. See the $Parameters section above for a
C         description of the tolerance.
C
C$ Files
C
C     A new type 8 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 08 data segment to the open SPK
C     file according to the format described in the type 08 section of
C     the SPK Required Reading. The SPK file must have been opened with
C     write access.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to create an SPK type 8 kernel
C        containing only one segment, given a time-ordered set of
C        discrete states and epochs.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKW08_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK8
C              PARAMETER           ( SPK8   = 'spkw08_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
C
C              INTEGER               DEGREE
C              PARAMETER           ( DEGREE = 3  )
C
C              INTEGER               NSTATS
C              PARAMETER           ( NSTATS = 9  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      BEGTIM
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      STATES ( 6, NSTATS )
C              DOUBLE PRECISION      STEP
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Set the array of discrete states to write to the SPK
C        C     segment.
C        C
C              DATA                  STATES /
C             .        101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
C             .        102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
C             .        103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
C             .        104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
C             .        105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
C             .        106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
C             .        107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
C             .        108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
C             .        109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment, and the time step separating epochs of states.
C        C
C              FIRST = 100.D0
C              LAST  = 900.D0
C              STEP  = 100.D0
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, so set to zero.
C        C
C              NCOMCH = 0
C
C        C
C        C     Internal file name and segment ID.
C        C
C              IFNAME = 'Type 8 SPK internal file name.'
C              SEGID  = 'SPK type 8 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK8, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Set the epoch of first state in STATES array to be
C        C     the start time of the interval covered by the segment.
C        C
C              BEGTIM = FIRST
C
C        C
C        C     Create a type 8 segment.
C        C
C              CALL SPKW08 (  HANDLE,  BODY,    CENTER,  REF,
C             .               FIRST,   LAST,    SEGID,   DEGREE,
C             .               NSTATS,  STATES,  BEGTIM,  STEP     )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 8 exists in
C        the output directory.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     J.M. Lynch         (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.1.0, 03-JUN-2021 (JDR)
C
C        Changed the input argument name EPOCH1 to BEGTIM for
C        consistency with other routines.
C
C        Edited the header to comply with NAIF standard. Added
C        complete example code from existing fragment.
C
C        Removed unnecessary $Revisions sections.
C
C-    SPICELIB Version 3.0.0, 11-JAN-2013 (NJB)
C
C        Relaxed test on relationship between the time bounds of the
C        input record set (determined by BEGTIM, STEP, and N) and the
C        descriptor bounds FIRST and LAST. Now the descriptor bounds
C        may extend beyond the time bounds of the record set by a ratio
C        computed using the parameter TOLSCL (see $Parameters above for
C        details). MAXDEG was increased to 27.
C
C-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
C
C        The routine was upgraded to support non-inertial reference
C        frames.
C
C-    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG)
C
C        Removed all references to a specific method of opening the SPK
C        file in the $Brief_I/O, $Detailed_Input, $Particulars and
C        $Examples sections of the header. It is assumed that a person
C        using this routine has some knowledge of the DAF system and the
C        methods for obtaining file handles.
C
C-    SPICELIB Version 1.0.0, 08-AUG-1993 (NJB) (JML) (WLT)
C
C-&


C$ Index_Entries
C
C     write SPK type_8 ephemeris data segment
C
C-&


C
C     SPICELIB functions
C
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local parameters
C

C
C     SIDLEN is the maximum number of characters allowed in an
C     SPK segment identifier.
C
C     NS is the size of a packed SPK segment descriptor.
C
C     ND is the number of double precision components in an SPK
C     segment descriptor.
C
C     NI is the number of integer components in an SPK segment
C     descriptor.
C
C     DTYPE is the data type.
C
C     FPRINT is the integer value of the first printable ASCII
C     character.
C
C     LPRINT is the integer value of the last printable ASCII character.
C
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN =  40 )

      INTEGER               NS
      PARAMETER           ( NS     =   5 )

      INTEGER               ND
      PARAMETER           ( ND     =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =   6 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE  =   8 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )

      INTEGER               STATSZ
      PARAMETER           ( STATSZ =   6 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN =  40 )

C
C     Local variables
C
      CHARACTER*(TIMLEN)    ETSTR

      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      DESCR  ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      TOL

      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               ICD    ( NI )
      INTEGER               REFCOD


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKW08' )

C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW08'                                    )
         RETURN

      END IF

C
C     Check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' //
     .                 '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW08'                                  )
         RETURN

      END IF

C
C     Now check that all the characters in the segment identifier
C     can be printed.
C
      DO I = 1, LASTNB(SEGID)

         CHRCOD = ICHAR( SEGID(I:I) )

         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '
     .               //   'nonprintable characters: ICHAR(SEGID(#:#)) '
     .               //   ' = #'                                  )
            CALL ERRINT ( '#',   I                                )
            CALL ERRINT ( '#',   I                                )
            CALL ERRINT ( '#',   CHRCOD                           )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW08'                                )
            RETURN

         END IF

      END DO

C
C     Make sure that the degree of the interpolating polynomials is
C     in range.
C
      IF (  ( DEGREE .LT. 1 ) .OR. ( DEGREE .GT. MAXDEG )  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '//
     .                 'the valid degree range is [1, #].'             )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'SPKW08'                                        )
         RETURN

      END IF

C
C     Make sure that the number of states is sufficient to define a
C     polynomial whose degree is DEGREE.
C
      IF ( N .LE. DEGREE ) THEN

         CALL SETMSG ( 'At least # states are required to define a '  //
     .                 'polynomial of degree #.  Number of states '   //
     .                 'supplied:  #.'                                 )
         CALL ERRINT ( '#', DEGREE + 1                                 )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWSTATES)'                           )
         CALL CHKOUT ( 'SPKW08'                                        )
         RETURN

      END IF

C
C     The segment stop time should be greater than the begin time.
C
      IF ( FIRST .GE. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater than ' //
     .                 'or equal to the segment end time: #'       )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW08'                                    )
         RETURN

      END IF

C
C     The step size must be positive.
C
      IF ( STEP .LE. 0.D0 ) THEN

         CALL SETMSG ( 'The step size must be > 0 but was #. ' )
         CALL ERRDP  ( '#', STEP                               )
         CALL SIGERR ( 'SPICE(INVALIDSTEPSIZE)'                )
         CALL CHKOUT ( 'SPKW08'                                )
         RETURN

      END IF

C
C     Compute the tolerance to use for descriptor time bound checks.
C
      TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )

      IF ( FIRST .LT. BEGTIM - TOL ) THEN

         CALL SETMSG ( 'The segment descriptor start time # is too '
     .   //            'much less than the beginning time of the  '
     .   //            'segment data # (in seconds past J2000: #). '
     .   //            'The difference is # seconds; the  '
     .   //            'tolerance is # seconds.'                     )
         CALL ETCAL  ( FIRST,  ETSTR                                 )
         CALL ERRCH  ( '#',    ETSTR                                 )
         CALL ETCAL  ( BEGTIM, ETSTR                                 )
         CALL ERRCH  ( '#',    ETSTR                                 )
         CALL ERRDP  ( '#',    FIRST                                 )
         CALL ERRDP  ( '#',    BEGTIM-FIRST                          )
         CALL ERRDP  ( '#',    TOL                                   )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW08'                                      )
         RETURN

      END IF

C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      LTIME = BEGTIM  +  ( (N-1) * STEP )

      IF ( LAST .GT. LTIME + TOL ) THEN

         CALL SETMSG ( 'The segment descriptor end time # is too '
     .   //            'much greater than the end time of the segment '
     .   //            'data # (in seconds past J2000: #). The '
     .   //            'difference is # seconds; the tolerance is # '
     .   //            'seconds.'                                    )
         CALL ETCAL  ( LAST,  ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ETCAL  ( LTIME, ETSTR                                  )
         CALL ERRCH  ( '#',   ETSTR                                  )
         CALL ERRDP  ( '#',   LAST                                   )
         CALL ERRDP  ( '#',   LAST-LTIME                             )
         CALL ERRDP  ( '#',   TOL                                    )
         CALL SIGERR ( 'SPICE(COVERAGEGAP)'                          )
         CALL CHKOUT ( 'SPKW08'                                      )
         RETURN

      END IF

C
C     If we made it this far, we're ready to start writing the segment.
C
C     Store the start and end times to be associated
C     with this segment.
C
      DCD(1) = FIRST
      DCD(2) = LAST

C
C     Create the integer portion of the descriptor.
C
      ICD(1) = BODY
      ICD(2) = CENTER
      ICD(3) = REFCOD
      ICD(4) = DTYPE

C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW08' )
         RETURN
      END IF

C
C     The type 8 segment structure is eloquently described by this
C     diagram from the SPK Required Reading:
C
C        +-----------------------+
C        | State 1               |
C        +-----------------------+
C        | State 2               |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | State N               |
C        +-----------------------+
C        | Epoch of state 1 (ET) |
C        +-----------------------+
C        | Step size             |
C        +-----------------------+
C        | Polynomial degree     |
C        +-----------------------+
C        | Number of states      |
C        +-----------------------+
C
C
      CALL DAFADA ( STATES,          N*STATSZ )
      CALL DAFADA ( BEGTIM,          1        )
      CALL DAFADA ( STEP,            1        )
      CALL DAFADA ( DBLE(DEGREE),    1        )
      CALL DAFADA ( DBLE(N),         1        )

C
C     As long as nothing went wrong, end the segment.
C
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF

      CALL CHKOUT ( 'SPKW08' )
      RETURN
      END
