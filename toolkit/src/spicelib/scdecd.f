C$Procedure SCDECD ( Decode spacecraft clock )

      SUBROUTINE SCDECD ( SC, SCLKDP, SCLKCH )

C$ Abstract
C
C     Convert a double precision encoding of spacecraft clock time into
C     a character representation.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'sclk.inc'

      INTEGER               SC
      DOUBLE PRECISION      SCLKDP
      CHARACTER*(*)         SCLKCH

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     SCLKDP     I   Encoded representation of a spacecraft clock count.
C     SCLKCH     O   Character representation of a clock count.
C     MXPART     P   Maximum number of spacecraft clock partitions.
C
C$ Detailed_Input
C
C     SC       is the NAIF integer code of the spacecraft whose
C              clock's time is being decoded.
C
C     SCLKDP   is the double precision encoding of a clock time in
C              units of ticks since the spacecraft clock start time.
C              This value does reflect partition information.
C
C              An analogy may be drawn between a spacecraft clock
C              and a standard wall clock. The number of ticks
C              corresponding to the wall clock string
C
C                 hh:mm:ss
C
C              would be the number of seconds represented by that
C              time.
C
C              For example:
C
C                 Clock string      Number of ticks
C                 ------------      ---------------
C                   00:00:10              10
C                   00:01:00              60
C                   00:10:00             600
C                   01:00:00            3600
C
C              If SCLKDP contains a fractional part the result
C              is the same as if SCLKDP had been rounded to the
C              nearest whole number.
C
C$ Detailed_Output
C
C     SCLKCH   is the character representation of the clock count.
C              The exact form that SCLKCH takes depends on the
C              spacecraft.
C
C              Nevertheless, SCLKCH will have the following general
C              format:
C
C                 'pp/sclk_string'
C
C              'pp' is an integer greater than or equal to one and
C              represents a "partition number".
C
C              Each mission is divided into some number of partitions.
C              A new partition starts when the spacecraft clock
C              resets, either to zero, or to some other
C              value. Thus, the first partition for any mission
C              starts with launch, and ends with the first clock
C              reset. The second partition starts immediately when
C              the first stopped, and so on.
C
C              In order to be completely unambiguous about a
C              particular time, you need to specify a partition number
C              along with the standard clock string.
C
C              Information about when partitions occur for different
C              missions is contained in a spacecraft clock kernel
C              file which needs to be loaded into the kernel pool
C              before calling SCDECD.
C
C              The routine SCPART may be used to read the partition
C              start and stop times, in encoded units of ticks, from
C              the kernel file.
C
C              Since the end time of one partition is coincident with
C              the begin time of the next, two different time strings
C              with different partition numbers can encode into the
C              same value.
C
C              For example, if partition 1 ends at time t1, and
C              partition 2 starts at time t2, then
C
C                 '1/t1' and '2/t2'
C
C              will be encoded into the same value, say X. SCDECD
C              always decodes such values into the latter of the
C              two partitions. In this example,
C
C                 CALL SCDECD ( X, SC, CLKSTR )
C
C              will result in
C
C                 CLKSTR = '2/t2'.
C
C              'sclk_string' is a spacecraft specific clock string,
C              typically consisting of a number of components
C              separated by delimiters.
C
C              Using Galileo as an example, the full format is
C
C                 wwwwwwww:xx:y:z
C
C              where z is a mod-8 counter (values 0-7) which
C              increments approximately once every 8 1/3 ms., y is a
C              mod-10 counter (values 0-9) which increments once
C              every time z turns over, i.e., approximately once every
C              66 2/3 ms., xx is a mod-91 (values 0-90) counter
C              which increments once every time y turns over, i.e.,
C              once every 2/3 seconds. wwwwwwww is the Real-Time Image
C              Count (RIM), which increments once every time xx turns
C              over, i.e., once every 60 2/3 seconds. The roll-over
C              expression for the RIM is 16777215, which corresponds
C              to approximately 32 years.
C
C              wwwwwwww, xx, y, and z are referred to interchangeably
C              as the fields or components of the spacecraft clock.
C              SCLK components may be separated by any of these five
C              characters: ' '  ':'  ','  '-'  '.'
C              The delimiter used is determined by a kernel pool
C              variable and can be adjusted by the user.
C
C              Some spacecraft clock components have offset, or
C              starting, values different from zero. For example,
C              with an offset value of 1, a mod 20 counter would
C              cycle from 1 to 20 instead of from 0 to 19.
C
C              See the SCLK required reading for a detailed
C              description of the Voyager and Mars Observer clock
C              formats.
C
C$ Parameters
C
C     MXPART   is the maximum number of spacecraft clock partitions
C              expected in the kernel file for any one spacecraft.
C              See the INCLUDE file sclk.inc for this parameter's
C              value.
C
C$ Exceptions
C
C     1)  If kernel variables required by this routine are unavailable,
C         an error is signaled by a routine in the call tree of this
C         routine. SCLKCH will be returned as a blank string in this
C         case.
C
C     2)  If the number of partitions in the kernel file for spacecraft
C         SC exceeds the parameter MXPART, the error
C         SPICE(TOOMANYPARTS) is signaled. SCLKCH will be returned
C         as a blank string in this case.
C
C     3)  If the encoded value does not fall in the boundaries of the
C         mission, the error SPICE(VALUEOUTOFRANGE) is signaled.
C         SCLKCH will be returned as a blank string in this case.
C
C     4)  If the declared length of SCLKCH is not large enough to
C         contain the output clock string, the error
C         SPICE(SCLKTRUNCATED) is signaled by either this routine or a
C         routine in the call tree of this routine. On output SCLKCH
C         will contain a portion of the truncated clock string.
C
C$ Files
C
C     A kernel file containing spacecraft clock partition information
C     for the desired spacecraft must be loaded, using the routine
C     FURNSH, before calling this routine.
C
C$ Particulars
C
C     In general, it is difficult to compare spacecraft clock counts
C     numerically since there are too many clock components for a
C     single comparison. The routine SCENCD provides a method of
C     assigning a single double precision number to a spacecraft's
C     clock count, given one of its character representations.
C
C     This routine performs the inverse operation to SCENCD, converting
C     an encoded double precision number to character format.
C
C     To convert the number of ticks since the start of the mission to
C     a clock format character string, SCDECD:
C
C        1) Determines the spacecraft clock partition that TICKS falls
C           in.
C
C        2) Subtracts off the number of ticks occurring in previous
C           partitions, to get the number of ticks since the beginning
C           of the current partition.
C
C        3) Converts the resulting ticks to clock format and forms the
C           string
C
C              'partition_number/clock_string'
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) Double precision encodings of spacecraft clock counts are used
C        to tag pointing data in the C-kernel.
C
C        In the following example, pointing for a sequence of images
C        from the CASSINI Imaging Science Subsystem (ISS) is requested
C        from the C-kernel using an array of character spacecraft clock
C        counts as input. The clock counts attached to the output are
C        then decoded to character and compared with the input strings.
C
C        Use the CK kernel below to load the CASSINI image navigated
C        spacecraft pointing and orientation data.
C
C           04153_04182ca_ISS.bc
C
C
C        Use the SCLK kernel below to load the CASSINI spacecraft clock
C        time correlation data required for the conversion between
C        spacecraft clock string representation and double precision
C        encoding of spacecraft clock counts.
C
C           cas00071.tsc
C
C
C        Example code begins here.
C
C
C              PROGRAM SCDECD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C        C     The instrument we want pointing for is the CASSINI
C        C     spacecraft. The reference frame we want is
C        C     J2000. The spacecraft is CASSINI.
C        C
C              INTEGER               SC
C              PARAMETER           ( SC     = -82 )
C
C              INTEGER               INST
C              PARAMETER           ( INST   = -82000 )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000' )
C
C              CHARACTER*(*)         CK
C              PARAMETER           ( CK     = '04153_04182ca_ISS.bc' )
C
C              CHARACTER*(*)         SCLK
C              PARAMETER           ( SCLK   = 'cas00071.tsc' )
C
C              INTEGER               NPICS
C              PARAMETER           ( NPICS  = 4 )
C
C              CHARACTER*(*)         CLKTOL
C              PARAMETER           ( CLKTOL = '1.0' )
C
C              INTEGER               MAXLEN
C              PARAMETER           ( MAXLEN = 30 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(25)        SCLKIN (4)
C              CHARACTER*(25)        SCLKOUT
C
C              DOUBLE PRECISION      CMAT   (3,3)
C              DOUBLE PRECISION      TIMEIN
C              DOUBLE PRECISION      TIMEOUT
C              DOUBLE PRECISION      TOL
C
C              INTEGER               I
C              INTEGER               J
C              INTEGER               K
C
C              LOGICAL               FOUND
C
C        C
C        C     Set the input SCLK strings.
C        C
C              DATA                  SCLKIN /  '1/1465644279.0',
C             .                                '1/1465644281.0',
C             .                                '1/1465644351.0',
C             .                                '1/1465644361.0'  /
C
C        C
C        C     Load the appropriate files. We need
C        C
C        C        1. CK file containing pointing data.
C        C        2. Spacecraft clock kernel file.
C        C
C              CALL FURNSH ( CK   )
C              CALL FURNSH ( SCLK )
C
C        C
C        C     Convert the tolerance string to ticks.
C        C
C              CALL SCTIKS ( SC, CLKTOL, TOL )
C
C              DO I= 1, NPICS
C
C                 CALL SCENCD ( SC, SCLKIN(I), TIMEIN )
C
C                 CALL CKGP ( INST, TIMEIN,  TOL,  REF,
C             .               CMAT, TIMEOUT, FOUND     )
C
C                 WRITE(*,*)
C                 WRITE(*,'(2A)') 'Input s/c clock count : ', SCLKIN(I)
C
C                 IF ( FOUND ) THEN
C
C                    CALL SCDECD ( SC, TIMEOUT, SCLKOUT )
C
C                    WRITE(*,'(2A)') 'Output s/c clock count: ',
C             .                                          SCLKOUT
C                    WRITE(*,'(A)') 'Output C-Matrix:'
C
C                    DO J = 1, 3
C
C                       WRITE(*,'(3F21.15)') ( CMAT(J,K), K = 1, 3 )
C
C                    END DO
C
C                 ELSE
C
C                    WRITE(*,'(A)') 'No pointing found.'
C
C                 END IF
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
C        Input s/c clock count : 1/1465644279.0
C        No pointing found.
C
C        Input s/c clock count : 1/1465644281.0
C        Output s/c clock count: 1/1465644281.171
C        Output C-Matrix:
C           -0.335351455948710    0.864374440205611    0.374694846658341
C           -0.937887426812980   -0.343851965210223   -0.046184419961653
C            0.088918927227039   -0.366909598048763    0.925997176691424
C
C        Input s/c clock count : 1/1465644351.0
C        Output s/c clock count: 1/1465644351.071
C        Output C-Matrix:
C           -0.335380929397586    0.864363638262230    0.374693385378623
C           -0.937874292008090   -0.343889838107825   -0.046169163264003
C            0.088946301703530   -0.366899550417080    0.925998528787713
C
C        Input s/c clock count : 1/1465644361.0
C        No pointing found.
C
C
C$ Restrictions
C
C     1)  Assumes that an SCLK kernel file appropriate for the clock
C         designated by SC is loaded in the kernel pool at the time
C         this routine is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     J.M. Lynch         (JPL)
C     W.L. Taber         (JPL)
C     R.E. Thurman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.2.0, 18-NOV-2021 (NJB) (JDR)
C
C        Now variables PSTART, PSTOP, and PTOTLS are saved. Made minor
C        changes to formatting of code.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example fragments using PDS
C        archived CASSINI data.
C
C        Added FAILED() call after SCFMT call.
C
C        Removed unnecessary entries in $Revisions section.
C
C-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
C
C        Values of parameter MXPART and PARTLN are now
C        provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references
C        to FURNSH.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT)
C
C        The routine was changed to signal an error when SCLKCH is
C        not long enough to contain the output spacecraft clock
C        string.
C
C        FAILED is now checked after calling SCPART.
C
C        References to CLPOOL were deleted.
C
C        Miscellaneous minor updates to the header were performed.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
C
C-&


C$ Index_Entries
C
C     decode spacecraft_clock
C
C-&


C$ Revisions
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT)
C
C        The routine was changed to signal an error when SCLKCH is
C        not long enough to contain the output spacecraft clock
C        string. Previously, the SCLK routines simply truncated
C        the clock string on the right. It was determined that
C        since this truncation could easily go undetected by the
C        user ( only the leftmost field of a clock string is
C        required when clock string is used as an input to a
C        SCLK routine ), it would be better to signal an error
C        when this happens.
C
C        FAILED is checked after calling SCPART in case an
C        error has occurred reading the kernel file and the
C        error action is not set to 'abort'.
C
C        References to CLPOOL were deleted.
C
C        Miscellaneous minor updates to the header were performed.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

      INTEGER               LSTLED
      INTEGER               LASTNB

C
C     Local variables
C
      CHARACTER*(PARTLN)    PRTSTR

      INTEGER               NPARTS
      INTEGER               PART
      INTEGER               PRELEN
      INTEGER               SUFLEN
      INTEGER               I

      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      PSTART   ( MXPART )
      DOUBLE PRECISION      PSTOP    ( MXPART )
      DOUBLE PRECISION      PTOTLS   ( MXPART )

C
C     Saved variables
C
      SAVE                  PSTART
      SAVE                  PSTOP
      SAVE                  PTOTLS


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCDECD' )

C
C     Use a working copy of the input.
C
      TICKS = ANINT ( SCLKDP )

      SCLKCH = ' '

C
C     Read the partition start and stop times (in ticks) for this
C     mission. Error if there are too many of them.  Also need to
C     check FAILED in case error handling is not in ABORT or
C     DEFAULT mode.
C
      CALL SCPART ( SC, NPARTS, PSTART, PSTOP )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCDECD' )
         RETURN
      END IF

      IF ( NPARTS .GT. MXPART ) THEN
C
C        This code should be unreachable. It is included for safety.
C
         CALL SETMSG ( 'The number of partitions, #, for spacecraft ' //
     .                 '# exceeds the value for parameter MXPART, #.' )
         CALL ERRINT ( '#', NPARTS           )
         CALL ERRINT ( '#', SC               )
         CALL ERRINT ( '#', MXPART           )
         CALL SIGERR ( 'SPICE(TOOMANYPARTS)' )
         CALL CHKOUT ( 'SCDECD'              )
         RETURN

      END IF

C
C     For each partition, compute the total number of ticks in that
C     partition plus all preceding partitions.
C
      PTOTLS( 1 ) = ANINT ( PSTOP( 1 ) - PSTART( 1 ) )

      DO I = 2, NPARTS
         PTOTLS( I ) = ANINT ( PTOTLS( I-1 ) + PSTOP( I ) - PSTART( I ))
      END DO

C
C     The partition corresponding to the input ticks is the first one
C     whose tick total is greater than the input value.  The one
C     exception is when the input ticks is equal to the total number
C     of ticks represented by all the partitions.  In this case the
C     partition number is the last one, i.e. NPARTS.
C
C     Error if TICKS comes before the first partition (that is, if it's
C     negative), or after the last one.
C
      IF ( TICKS .EQ. PTOTLS (NPARTS) )  THEN
         PART = NPARTS
      ELSE
         PART = LSTLED ( TICKS, NPARTS, PTOTLS ) + 1
      END IF


      IF ( ( TICKS .LT. 0.D0 ).OR.( PART .GT. NPARTS ) ) THEN

         CALL SETMSG ( 'Value for ticks, #, does not fall in any ' //
     .                 'partition for spacecraft #.'  )
         CALL ERRDP  ( '#', TICKS                     )
         CALL ERRINT ( '#', SC                        )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'SCDECD'                       )
         RETURN

      END IF

C
C     To get the count in this partition, subtract off the total of
C     the preceding partition counts and add the beginning count for
C     this partition.
C
      IF ( PART .EQ. 1 ) THEN
         TICKS = TICKS + PSTART( PART )
      ELSE
         TICKS = TICKS + PSTART( PART ) - PTOTLS( PART - 1 )
      END IF

C
C     Now create the output SCLK clock string.
C
C     First convert from ticks to clock string format.
C
      CALL SCFMT ( SC, TICKS, SCLKCH )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCDECD' )
         RETURN
      END IF

C
C     Now convert the partition number to a character string and prefix
C     it to the output string.
C
      CALL INTSTR ( PART, PRTSTR )

      CALL SUFFIX ( '/', 0, PRTSTR )

      PRELEN = LASTNB ( PRTSTR )
      SUFLEN = LASTNB ( SCLKCH )

      IF ( LEN ( SCLKCH ) - SUFLEN .LT. PRELEN ) THEN

         CALL SETMSG ( 'Output string too short to contain clock '    //
     .                 'string. Input tick value: #, requires string '//
     .                 'of length #, but declared length is #.'       )
         CALL ERRDP  ( '#', SCLKDP )
         CALL ERRINT ( '#', PRELEN + SUFLEN     )
         CALL ERRINT ( '#', LEN ( SCLKCH )      )
         CALL SIGERR ( 'SPICE(SCLKTRUNCATED)'   )
         CALL CHKOUT ( 'SCDECD'                 )
         RETURN

      END IF

      CALL PREFIX ( PRTSTR, 0, SCLKCH )

      CALL CHKOUT ( 'SCDECD' )
      RETURN
      END
