C$Procedure SPKW05 ( Write SPK segment, type 5 )

      SUBROUTINE SPKW05 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
     .                    SEGID,  GM,    N,      STATES, EPOCHS      )

C$ Abstract
C
C     Write an SPK segment of type 5 given a time-ordered set of
C     discrete states and epochs, and the gravitational parameter
C     of a central body.
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
C     SPK
C     SPC
C     NAIF_IDS
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      GM
      INTEGER               N
      DOUBLE PRECISION      STATES   ( 6, * )
      DOUBLE PRECISION      EPOCHS   (    * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an SPK file open for writing.
C     BODY       I   Body code for ephemeris object.
C     CENTER     I   Body code for the center of motion of the body.
C     FRAME      I   The reference frame of the states.
C     FIRST      I   First valid time for which states can be computed.
C     LAST       I   Last valid time for which states can be computed.
C     SEGID      I   Segment identifier.
C     GM         I   Gravitational parameter of central body.
C     N          I   Number of states and epochs.
C     STATES     I   States.
C     EPOCHS     I   Epochs.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file
C              opened for writing.
C
C     BODY     is the NAIF ID for the body whose states are
C              to be recorded in an SPK file.
C
C     CENTER   is the NAIF ID for the center of motion associated
C              with BODY.
C
C     FRAME    is the reference frame that states are referenced to,
C              for example 'J2000'.
C
C     FIRST,
C     LAST     are the bounds on the ephemeris times, expressed as
C              seconds past J2000, for which the states can be used
C              to interpolate a state for BODY.
C
C     SEGID    is the segment identifier. An SPK segment identifier
C              may contain up to 40 characters.
C
C     GM       is the gravitational parameter of the central body
C              ( in units of kilometers **3 / seconds **2 ).
C
C     N        is the number of states and epochs to be stored
C              in the segment.
C
C     STATES   contains a time-ordered array of geometric states
C              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
C              kilometers per second ) of the target body with
C              respect to the central body specified in the segment
C              descriptor.
C
C     EPOCHS   contains the epochs (ephemeris seconds past J2000)
C              corresponding to the states in STATES. Epochs must
C              form a strictly increasing sequence.
C
C$ Detailed_Output
C
C     None. A type 5 segment is written to the file attached to HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If GM is not positive, the error SPICE(NONPOSITIVEMASS)
C         is signaled.
C
C     2)  If the input epochs do not form an increasing sequence, the
C         error SPICE(UNORDEREDTIMES) is signaled.
C
C     3)  If the number of states and epochs is not positive, the
C         error SPICE(NUMSTATESNOTPOS) is signaled.
C
C     4)  If FIRST is greater than LAST, the error
C         SPICE(BADDESCRTIMES) is signaled.
C
C     5)  If SEGID is more than 40 characters long, the error
C         SPICE(SEGIDTOOLONG) is signaled.
C
C     6)  If SEGID contains any nonprintable characters, the error
C         SPICE(NONPRINTABLECHARS) is signaled.
C
C     7)  If a file I/O problem occurs, an error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     A new type 05 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 05 data segment to the open SPK
C     file according to the format described in the type 05 section of
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
C     1) This example demonstrates how to create an SPK type 5 kernel
C        containing only one segment, given a time-ordered set of
C        discrete states and epochs, and the gravitational parameter
C        of a central body.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPKW05_EX1
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
C              CHARACTER*(*)         SPK5
C              PARAMETER           ( SPK5  = 'spkw05_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              DOUBLE PRECISION      GMSUN
C              PARAMETER           ( GMSUN  = 132712440023.310D0 )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
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
C              DOUBLE PRECISION      EPOCHS (    NSTATS )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      STATES ( 6, NSTATS )
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Define the states and epochs.
C        C
C              DATA                  STATES /
C             .       101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
C             .       102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
C             .       103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
C             .       104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
C             .       105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
C             .       106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
C             .       107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
C             .       108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
C             .       109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0 /
C
C              DATA                  EPOCHS / 100.D0, 200.D0, 300.D0,
C             .                               400.D0, 500.D0, 600.D0,
C             .                               700.D0, 800.D0, 900.D0 /
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment.
C        C
C              FIRST  = EPOCHS(1)
C              LAST   = EPOCHS(NSTATS)
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
C              IFNAME = 'Type 5 SPK internal file name.'
C              SEGID  = 'SPK type 5 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK5, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Write the segment.
C        C
C              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF,
C             .              FIRST,  LAST,   SEGID,  GMSUN,
C             .              NSTATS, STATES, EPOCHS        )
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
C        screen. After run completion, a new SPK type 5 exists in
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
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     J.M. Lynch         (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        example code from existing fragment. Removed unnecessary
C        $Revisions section.
C
C-    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS)
C
C        Removed restriction that the input reference frame should be
C        inertial by changing the routine that determines the frame ID
C        from the name from IRFNUM to NAMFRM.
C
C-    SPICELIB Version 1.0.2, 27-JAN-2003 (EDW)
C
C        Added error check to catch non-positive gravitational
C        parameter GM.
C
C-    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG)
C
C        Removed all references to a specific method of opening the SPK
C        file in the $Brief_I/O, $Detailed_Input, $Particulars and
C        $Examples sections of the header. It is assumed that a person
C        using this routine has some knowledge of the DAF system and the
C        methods for obtaining file handles.
C
C-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     write SPK type_5 ephemeris data segment
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
      INTEGER               LASTNB

C
C     Local parameters
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
      PARAMETER           ( SIDLEN  =  40 )

      INTEGER               NS
      PARAMETER           ( NS      =   5 )

      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI      =   6 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   5 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )


C
C     Local variables
C

      DOUBLE PRECISION      DCD      ( ND     )
      DOUBLE PRECISION      DESCR    ( NS     )

      INTEGER               I
      INTEGER               REFCOD
      INTEGER               VALUE
      INTEGER               ICD      ( NI )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKW05' )
      END IF


      IF ( GM .LE. 0.0D0 ) THEN

         CALL SETMSG ( 'GM = #; Non-positive gravitational parameter' )
         CALL ERRDP  ( '#',  GM                   )
         CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'   )
         CALL CHKOUT ( 'SPKW05'                   )
         RETURN

      END IF


C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW05'                                    )
         RETURN

      END IF

C
C     Make sure that the number of states and epochs is positive.
C
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of states and epochs is not '  //
     .                 'positive. N = #'                           )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(NUMSTATESNOTPOS)'                    )
         CALL CHKOUT ( 'SPKW05'                                    )
         RETURN

      END IF
C
C     Check the input epochs to make sure that they form a
C     strictly increasing sequence.
C
      DO I = 2, N

         IF ( EPOCHS(I) .LE. EPOCHS(I-1) ) THEN

            CALL SETMSG ( 'Epoch # is out of order. ' )
            CALL ERRDP  ( '#',  EPOCHS(I)             )
            CALL SIGERR ( 'SPICE(UNORDEREDTIMES)'     )
            CALL CHKOUT ( 'SPKW05'                    )
            RETURN

         END IF

      END DO

C
C     The segment stop time should be greater then the begin time.
C
      IF ( FIRST .GT. LAST ) THEN

         CALL SETMSG ( 'The segment start time: # is greater then ' //
     .                 'the segment end time: #'                   )
         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRDP  ( '#', LAST                                   )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW05'                                    )
         RETURN

      END IF

C
C     Now check that all the characters in the segid can be printed.
C
      DO I = 1, LASTNB(SEGID)

         VALUE = ICHAR( SEGID(I:I) )

         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '  //
     .                    'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'SPKW05'                                )
            RETURN

         END IF

      END DO

C
C     Also check to see if the segment identifier is too long.
C
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' //
     .                 '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'SPKW05'                                  )
         RETURN

      END IF

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
         CALL CHKOUT ( 'SPKW05' )
         RETURN
      END IF

C
C     This could hardly be simpler. Stuff the states into the segment,
C     followed by the epochs.
C
      CALL DAFADA ( STATES, 6 * N )
      CALL DAFADA ( EPOCHS,     N )

C
C     If there are at least 100 state/epoch pairs, write a directory
C     containing every 100'th epoch.
C
      I = 100

      DO WHILE ( I .LE. N )
         CALL DAFADA ( EPOCHS(I), 1 )
         I = I + 100
      END DO

C
C     Store the GM of the central body, and the number of states.
C
      CALL DAFADA ( GM,         1 )
      CALL DAFADA ( DBLE ( N ), 1 )

C
C     If anything went wrong, don't end the segment.
C
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF

      CALL CHKOUT ( 'SPKW05' )
      RETURN
      END
